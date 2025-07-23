use crate::frontend::action::belt_placement::{BeltState, expected_belt_state};
use crate::item::Indexable;
use crate::rendering::Corner;
use crate::{
    TICKS_PER_SECOND_LOGIC,
    assembler::AssemblerOnclickInfo,
    belt::{BeltTileId, belt::BeltLenType, splitter::SPLITTER_BELT_LEN},
    data::{DataStore, ItemRecipeDir, factorio_1_1::get_raw_data_test},
    frontend::{
        action::{
            ActionType,
            action_state_machine::{
                ActionStateMachine, ActionStateMachineState, HeldObject, StatisticsPanel,
                WIDTH_PER_LEVEL,
            },
            set_recipe::SetRecipeInfo,
        },
        world::{
            Position,
            tile::{AssemblerInfo, BELT_LEN_PER_TILE, CHUNK_SIZE, CHUNK_SIZE_FLOAT, Dir, Entity},
        },
    },
    item::{IdxTrait, Item, Recipe, usize_from},
    power::{Joule, Watt, power_grid::MAX_POWER_MULT},
    rendering::map_view::{self, MapViewUpdate, create_map_textures_if_needed},
    statistics::{
        NUM_SAMPLES_AT_INTERVALS, NUM_X_AXIS_TICKS, RELATIVE_INTERVAL_MULTS, TIMESCALE_LEGEND,
    },
};
use eframe::egui::{
    self, Align2, Color32, ComboBox, Context, CornerRadius, Label, Layout, ProgressBar, Stroke, Ui,
    Window,
};
use egui::{Button, Modal, RichText, ScrollArea, Sense};
use egui_extras::{Column, TableBuilder};
use egui_plot::{AxisHints, GridMark, Line, Plot, PlotPoints};
use log::{info, trace};
use parking_lot::MutexGuard;
use petgraph::dot::Dot;
use rayon::iter::{IntoParallelIterator, ParallelIterator};
use std::cmp::max;
use std::fs::File;
use std::sync::LazyLock;
use std::{
    cmp::{Ordering, min},
    mem,
    time::Duration,
};
use tilelib::types::{DrawInstance, Layer, RendererTrait};

use super::{TextureAtlas, app_state::GameState};

const BELT_ANIM_SPEED: f32 = 1.0 / (BELT_LEN_PER_TILE as f32);

const ALT_MODE_ICON_SIZE: f32 = 0.5;

pub const SWITCH_TO_MAPVIEW_TILES: f32 = if cfg!(debug_assertions) { 200.0 } else { 500.0 };
pub const SWITCH_TO_MAPVIEW_ZOOM_LEVEL: LazyLock<f32> =
    LazyLock::new(|| ((SWITCH_TO_MAPVIEW_TILES - 1.0) / WIDTH_PER_LEVEL as f32).log(1.5));

// TODO: I found a weird performance cliff while zooming out, jumping from ~10ms to 20ms suddenly
//       Investigate!
//       => This seems to happen at 992 -> 993 width, i.e. when the for loop range jumps from 31 to 32, very suspicous

struct Layers {
    tile_layer: Layer,
    entity_layer: Layer,
    entity_overlay_layer: Layer,
    item_layer: Layer,
    warning_layer: Layer,
    range_layer: Layer,
}

impl Layers {
    pub fn extend(&mut self, other: Self) {
        let Self {
            tile_layer,
            entity_layer,
            entity_overlay_layer,
            item_layer,
            warning_layer,
            range_layer,
        } = self;
        let Self {
            tile_layer: other_tile_layer,
            entity_layer: other_entity_layer,
            entity_overlay_layer: other_entity_overlay_layer,
            item_layer: other_item_layer,
            warning_layer: other_warning_layer,
            range_layer: other_range_layer,
        } = other;
        tile_layer.extend(other_tile_layer);
        entity_layer.extend(other_entity_layer);
        entity_overlay_layer.extend(other_entity_overlay_layer);
        item_layer.extend(other_item_layer);
        warning_layer.extend(other_warning_layer);
        range_layer.extend(other_range_layer);
    }
}

fn layers_tile_grid(tilesize: f32, ar: f32) -> Layers {
    Layers {
        tile_layer: Layer::square_tile_grid(tilesize, ar),
        entity_layer: Layer::square_tile_grid(tilesize, ar),
        entity_overlay_layer: Layer::square_tile_grid(tilesize, ar),
        item_layer: Layer::square_tile_grid(tilesize, ar),
        warning_layer: Layer::square_tile_grid(tilesize, ar),
        range_layer: Layer::square_tile_grid(tilesize, ar),
    }
}

#[allow(clippy::too_many_lines)]
#[profiling::function]
pub fn render_world<ItemIdxType: IdxTrait, RecipeIdxType: IdxTrait>(
    renderer: &mut impl RendererTrait,
    mut game_state: MutexGuard<GameState<ItemIdxType, RecipeIdxType>>,
    texture_atlas: &TextureAtlas,
    state_machine: MutexGuard<ActionStateMachine<ItemIdxType, RecipeIdxType>>,
    data_store: &DataStore<ItemIdxType, RecipeIdxType>,
) {
    let mut updates = Some(vec![]);

    mem::swap(&mut updates, &mut game_state.world.map_updates);

    {
        profiling::scope!("map_view::apply_updates");
        map_view::apply_updates(
            updates
                .into_iter()
                .flat_map(|v| v.into_iter())
                .map(|pos| MapViewUpdate {
                    pos,
                    color: game_state.world.get_entity_color(pos, data_store),
                }),
            renderer,
        );
    }

    let ar = renderer.get_aspect_ratio();

    let num_tiles_across_screen_horizontal =
        WIDTH_PER_LEVEL as f32 * 1.5f32.powf(state_machine.zoom_level);
    let num_tiles_across_screen_vertical = num_tiles_across_screen_horizontal / ar;
    let tilesize: f32 = 1.0 / num_tiles_across_screen_horizontal;

    let mut state_machine_layer = Layer::square_tile_grid(tilesize, ar);
    let mut entity_overlay_layer = Layer::square_tile_grid(tilesize, ar);
    let mut player_layer = Layer::square_tile_grid(tilesize, ar);

    let camera_pos = match &state_machine.map_view_info {
        Some(map_view_pos) => *map_view_pos,
        None => state_machine.local_player_pos,
    };

    let player_chunk = (
        (camera_pos.0 / CHUNK_SIZE_FLOAT) as i32,
        (camera_pos.1 / CHUNK_SIZE_FLOAT) as i32,
    );

    if num_tiles_across_screen_horizontal > SWITCH_TO_MAPVIEW_TILES {
        if let ActionStateMachineState::Holding(HeldObject::Blueprint(bp)) = &state_machine.state {
            let Position { x, y } =
                ActionStateMachine::<ItemIdxType, RecipeIdxType>::player_mouse_to_tile(
                    state_machine.zoom_level,
                    camera_pos,
                    state_machine.current_mouse_pos,
                );

            bp.draw(
                (
                    x as f32 + num_tiles_across_screen_horizontal / 2.0,
                    y as f32 + num_tiles_across_screen_vertical / 2.0,
                ),
                camera_pos,
                &mut entity_overlay_layer,
                texture_atlas,
                data_store,
            );
        }

        mem::drop(state_machine);

        {
            profiling::scope!("Create Map Textures");
            create_map_textures_if_needed(
                &game_state.world,
                renderer,
                Position {
                    x: camera_pos.0 as i32,
                    y: camera_pos.1 as i32,
                },
                num_tiles_across_screen_horizontal as usize,
                num_tiles_across_screen_vertical as usize,
                data_store,
            );
        }
        mem::drop(game_state);

        {
            profiling::scope!("Render Map View");
            map_view::render_map_view(
                renderer,
                Position {
                    x: camera_pos.0 as i32,
                    y: camera_pos.1 as i32,
                },
                num_tiles_across_screen_horizontal,
                num_tiles_across_screen_vertical,
                num_tiles_across_screen_horizontal as usize,
                num_tiles_across_screen_vertical as usize,
                tilesize,
                ar,
                camera_pos,
            );
        }

        renderer.draw(&entity_overlay_layer);

        return;
    }

    // let mut storage_storage_inserter_batch = vec![];

    let x_range = (-((num_tiles_across_screen_horizontal / CHUNK_SIZE_FLOAT / 2.0).ceil() as i32)
        ..=((num_tiles_across_screen_horizontal / CHUNK_SIZE_FLOAT / 2.0).ceil() as i32))
        .into_par_iter();
    let y_range = (-((num_tiles_across_screen_vertical / CHUNK_SIZE_FLOAT / 2.0).ceil() as i32)
        ..=((num_tiles_across_screen_vertical / CHUNK_SIZE_FLOAT / 2.0).ceil() as i32));

    let pos_iter = x_range.flat_map_iter(|x| y_range.clone().map(move |y| (x, y)));

    let folded_layers = {
        profiling::scope!("Render Chunks");
        pos_iter
            .map(|(x_offs, y_offs)| {
                let chunk_draw_offs = (
                    x_offs as f32 * CHUNK_SIZE_FLOAT - camera_pos.0 % CHUNK_SIZE_FLOAT
                        + (0.5 * num_tiles_across_screen_horizontal),
                    y_offs as f32 * CHUNK_SIZE_FLOAT - camera_pos.1 % CHUNK_SIZE_FLOAT
                        + (0.5 * num_tiles_across_screen_vertical),
                );

                let (chunk_x, chunk_y) = (
                    player_chunk
                        .0
                        .checked_add(x_offs.try_into().unwrap())
                        .unwrap(),
                    player_chunk
                        .1
                        .checked_add(y_offs.try_into().unwrap())
                        .unwrap(),
                );

                ((chunk_x, chunk_y), chunk_draw_offs)
            })
            .fold(
                || layers_tile_grid(tilesize, ar),
                |mut layers: Layers, ((chunk_x, chunk_y), chunk_draw_offs)| {
                    let Layers {
                        tile_layer,
                        entity_layer,
                        entity_overlay_layer,
                        item_layer,
                        warning_layer,
                        range_layer,
                    } = &mut layers;

                    profiling::scope!("Rendering Chunk", format!("{:?}", (chunk_x, chunk_y)));

                    match game_state.world.get_chunk(chunk_x, chunk_y) {
                        Some(chunk) => {
                            for (x, row) in chunk
                                .floor_tiles
                                .as_ref()
                                .unwrap_or(&Box::new(Default::default()))
                                .iter()
                                .enumerate()
                            {
                                for (y, tile) in row.iter().enumerate() {
                                    match tile {
                                        crate::frontend::world::tile::FloorTile::Empty => {
                                            tile_layer.draw_sprite(
                                                &texture_atlas.blue,
                                                DrawInstance {
                                                    position: [
                                                        chunk_draw_offs.0 + x as f32,
                                                        chunk_draw_offs.1 + y as f32,
                                                    ],
                                                    size: [1.0, 1.0],
                                                    animation_frame: 0,
                                                },
                                            )
                                        },
                                        _ => {
                                            tile_layer.draw_sprite(
                                                &texture_atlas.default,
                                                DrawInstance {
                                                    position: [
                                                        chunk_draw_offs.0 + x as f32,
                                                        chunk_draw_offs.1 + y as f32,
                                                    ],
                                                    size: [1.0, 1.0],
                                                    animation_frame: 0,
                                                },
                                            );
                                        },
                                    }

                                    // TODO: Get current ore
                                    if game_state
                                        .world
                                        .get_original_ore_at_pos(Position {
                                            x: chunk_x * CHUNK_SIZE as i32 + x as i32,
                                            y: chunk_y * CHUNK_SIZE as i32 + y as i32,
                                        })
                                        .is_some()
                                    {
                                        tile_layer.draw_sprite(
                                            &texture_atlas.items[0],
                                            DrawInstance {
                                                position: [
                                                    chunk_draw_offs.0 + x as f32,
                                                    chunk_draw_offs.1 + y as f32,
                                                ],
                                                size: [1.0, 1.0],
                                                animation_frame: 0,
                                            },
                                        );
                                    }
                                }
                            }

                            {
                                profiling::scope!("Render Entities");
                                for entity in chunk.get_entities() {
                                    match entity {
                                        crate::frontend::world::tile::Entity::Assembler {
                                            ty,
                                            pos,
                                            info,
                                            ..
                                        } => {
                                            let size: [u16; 2] = [
                                                data_store.assembler_info[usize::from(*ty)].size.0,
                                                data_store.assembler_info[usize::from(*ty)].size.1,
                                            ];

                                            match info {
                                                AssemblerInfo::UnpoweredNoRecipe => {
                                                    texture_atlas.not_connected.draw_centered_on(
                                                        &texture_atlas.assembler,
                                                        [
                                                            chunk_draw_offs.0 + (pos.x % 16) as f32,
                                                            chunk_draw_offs.1 + (pos.y % 16) as f32,
                                                        ],
                                                        size,
                                                        0,
                                                        warning_layer,
                                                    );

                                                    texture_atlas.assembler.draw(
                                                        [
                                                            chunk_draw_offs.0 + (pos.x % 16) as f32,
                                                            chunk_draw_offs.1 + (pos.y % 16) as f32,
                                                        ],
                                                        size,
                                                        0,
                                                        entity_layer,
                                                    );
                                                },
                                                AssemblerInfo::Unpowered(recipe) => {
                                                    texture_atlas.not_connected.draw_centered_on(
                                                        &texture_atlas.assembler,
                                                        [
                                                            chunk_draw_offs.0 + (pos.x % 16) as f32,
                                                            chunk_draw_offs.1 + (pos.y % 16) as f32,
                                                        ],
                                                        size,
                                                        0,
                                                        warning_layer,
                                                    );

                                                    texture_atlas.assembler.draw(
                                                        [
                                                            chunk_draw_offs.0 + (pos.x % 16) as f32,
                                                            chunk_draw_offs.1 + (pos.y % 16) as f32,
                                                        ],
                                                        size,
                                                        0,
                                                        entity_layer,
                                                    );

                                                    let item_idx = data_store.recipe_to_items
                                                        [recipe]
                                                        .iter()
                                                        .find(|item| item.0 == ItemRecipeDir::Out)
                                                        .map(|item| item.1.into_usize())
                                                        .unwrap_or(0);

                                                    let icon_size: [f32; 2] = [
                                                        size[0] as f32 * ALT_MODE_ICON_SIZE,
                                                        size[1] as f32 * ALT_MODE_ICON_SIZE,
                                                    ];

                                                    entity_overlay_layer.draw_sprite(
                                                        &texture_atlas.items[item_idx],
                                                        DrawInstance {
                                                            position: [
                                                                chunk_draw_offs.0
                                                                    + (pos.x % 16) as f32
                                                                    + (size[0] as f32
                                                                        - icon_size[0])
                                                                        / 2.0,
                                                                chunk_draw_offs.1
                                                                    + (pos.y % 16) as f32
                                                                    + (size[1] as f32
                                                                        - icon_size[1])
                                                                        / 2.0,
                                                            ],
                                                            size: icon_size,
                                                            animation_frame: 0,
                                                        },
                                                    );
                                                },
                                                AssemblerInfo::PoweredNoRecipe(pole_position) => {
                                                    let grid = game_state
                                                        .simulation_state
                                                        .factory
                                                        .power_grids
                                                        .pole_pos_to_grid_id[pole_position];

                                                    let last_power = game_state
                                                        .simulation_state
                                                        .factory
                                                        .power_grids
                                                        .power_grids[usize::from(grid)]
                                                    .last_power_mult;

                                                    if last_power == 0 {
                                                        texture_atlas.no_power.draw_centered_on(
                                                            &texture_atlas.assembler,
                                                            [
                                                                chunk_draw_offs.0
                                                                    + (pos.x % 16) as f32,
                                                                chunk_draw_offs.1
                                                                    + (pos.y % 16) as f32,
                                                            ],
                                                            size,
                                                            0,
                                                            warning_layer,
                                                        );
                                                    }

                                                    texture_atlas.assembler.draw(
                                                        [
                                                            chunk_draw_offs.0 + (pos.x % 16) as f32,
                                                            chunk_draw_offs.1 + (pos.y % 16) as f32,
                                                        ],
                                                        size,
                                                        0,
                                                        entity_layer,
                                                    );
                                                },
                                                AssemblerInfo::Powered {
                                                    id,
                                                    pole_position,
                                                    ..
                                                } => {
                                                    let grid = game_state
                                                        .simulation_state
                                                        .factory
                                                        .power_grids
                                                        .pole_pos_to_grid_id[pole_position];

                                                    let last_power = game_state
                                                        .simulation_state
                                                        .factory
                                                        .power_grids
                                                        .power_grids[usize::from(grid)]
                                                    .last_power_mult;

                                                    if last_power == 0 {
                                                        texture_atlas.no_power.draw_centered_on(
                                                            &texture_atlas.assembler,
                                                            [
                                                                chunk_draw_offs.0
                                                                    + (pos.x % 16) as f32,
                                                                chunk_draw_offs.1
                                                                    + (pos.y % 16) as f32,
                                                            ],
                                                            size,
                                                            0,
                                                            warning_layer,
                                                        );
                                                    }

                                                    let AssemblerOnclickInfo {
                                                        inputs: _,
                                                        outputs: _,
                                                        timer_percentage,
                                                        prod_timer_percentage: _,
                                                        base_speed: _,
                                                        speed_mod: _,
                                                        prod_mod: _,
                                                        power_consumption_mod: _,
                                                        base_power_consumption: _,
                                                    } = game_state
                                                        .simulation_state
                                                        .factory
                                                        .power_grids
                                                        .get_assembler_info(*id, data_store);

                                                    texture_atlas.assembler.draw(
                                                        [
                                                            chunk_draw_offs.0 + (pos.x % 16) as f32,
                                                            chunk_draw_offs.1 + (pos.y % 16) as f32,
                                                        ],
                                                        size,
                                                        (timer_percentage
                                                            * (texture_atlas
                                                                .assembler
                                                                .sprite
                                                                .texture
                                                                .number_anim_frames
                                                                as f32))
                                                            .floor()
                                                            as u32
                                                            % texture_atlas
                                                                .assembler
                                                                .sprite
                                                                .texture
                                                                .number_anim_frames,
                                                        entity_layer,
                                                    );

                                                    let item_idx = data_store.recipe_to_items
                                                        [&id.recipe]
                                                        .iter()
                                                        .find(|item| item.0 == ItemRecipeDir::Out)
                                                        .map(|item| item.1.into_usize())
                                                        .unwrap_or(0);

                                                    let icon_size: [f32; 2] = [
                                                        size[0] as f32 * ALT_MODE_ICON_SIZE,
                                                        size[1] as f32 * ALT_MODE_ICON_SIZE,
                                                    ];

                                                    entity_overlay_layer.draw_sprite(
                                                        &texture_atlas.items[item_idx],
                                                        DrawInstance {
                                                            position: [
                                                                chunk_draw_offs.0
                                                                    + (pos.x % 16) as f32
                                                                    + (size[0] as f32
                                                                        - icon_size[0])
                                                                        / 2.0,
                                                                chunk_draw_offs.1
                                                                    + (pos.y % 16) as f32
                                                                    + (size[1] as f32
                                                                        - icon_size[1])
                                                                        / 2.0,
                                                            ],
                                                            size: icon_size,
                                                            animation_frame: 0,
                                                        },
                                                    );
                                                },
                                            }
                                        },

                                        crate::frontend::world::tile::Entity::Belt {
                                            pos,
                                            direction,
                                            ty,
                                            id,
                                            belt_pos,
                                        } => {
                                            let inputs = game_state
                                                .world
                                                .get_belt_possible_inputs_no_cache(*pos);
                                            let (sprite, corner) =
                                                match expected_belt_state(*direction, |dir| {
                                                    inputs[*dir]
                                                }) {
                                                    BeltState::Straight => {
                                                        (&texture_atlas.belt[*direction], None)
                                                    },
                                                    BeltState::Curved => {
                                                        if inputs[direction.turn_right()] {
                                                            (
                                                        &texture_atlas.belt_corners[Corner {
                                                            to_dir: *direction,
                                                            from_dir:
                                                                crate::rendering::BeltSide::Right,
                                                        }],
                                                        Some(crate::rendering::BeltSide::Right),
                                                    )
                                                        } else {
                                                            (
                                                        &texture_atlas.belt_corners[Corner {
                                                            to_dir: *direction,
                                                            from_dir:
                                                                crate::rendering::BeltSide::Left,
                                                        }],
                                                        Some(crate::rendering::BeltSide::Left),
                                                    )
                                                        }
                                                    },
                                                    BeltState::Sideloading => {
                                                        (&texture_atlas.belt[*direction], None)
                                                    },
                                                    BeltState::DoubleSideloading => {
                                                        (&texture_atlas.belt[*direction], None)
                                                    },
                                                };
                                            sprite.draw(
                                                [
                                                    chunk_draw_offs.0 + (pos.x % 16) as f32,
                                                    chunk_draw_offs.1 + (pos.y % 16) as f32,
                                                ],
                                                [1, 1],
                                                (game_state
                                                    .simulation_state
                                                    .factory
                                                    .belts
                                                    .inner
                                                    .belt_update_timers_cumulative
                                                    [*ty as usize]
                                                    as f32
                                                    / 120.0
                                                    * BELT_ANIM_SPEED
                                                    * (texture_atlas.belt[*direction]
                                                        .sprite
                                                        .texture
                                                        .number_anim_frames
                                                        as f32))
                                                    as u32
                                                    % texture_atlas.belt[*direction]
                                                        .sprite
                                                        .texture
                                                        .number_anim_frames,
                                                entity_layer,
                                            );

                                            // Draw Items
                                            // TODO: Draw items on corners not straight
                                            let items_iter = game_state
                                                .simulation_state
                                                .factory
                                                .belts
                                                .get_item_iter(*id)
                                                .into_iter()
                                                .enumerate()
                                                .skip(
                                                    (belt_pos
                                                        .checked_sub(BELT_LEN_PER_TILE)
                                                        .expect("Belt idx wrapped?!?"))
                                                    .into(),
                                                )
                                                .take(BELT_LEN_PER_TILE.into());

                                            let offs = direction.into_offset();
                                            let item_render_offs = (
                                                -f32::from(offs.0) / f32::from(BELT_LEN_PER_TILE),
                                                -f32::from(offs.1) / f32::from(BELT_LEN_PER_TILE),
                                            );

                                            let centered_on_tile = (
                                                chunk_draw_offs.0 + (pos.x % 16) as f32 + 0.5
                                                    - 0.5 * (1.0 / f32::from(BELT_LEN_PER_TILE)),
                                                chunk_draw_offs.1 + (pos.y % 16) as f32 + 0.5
                                                    - 0.5 * (1.0 / f32::from(BELT_LEN_PER_TILE)),
                                            );

                                            let last_moved: BeltLenType = game_state
                                                .simulation_state
                                                .factory
                                                .belts
                                                .get_last_moved_pos(*id);

                                            let belt_progress: u8 = game_state
                                                .simulation_state
                                                .factory
                                                .belts
                                                .get_belt_progress(*ty);

                                            let offset_perc = belt_progress as f32 / 120.0;

                                            let slow_offset = (
                                                item_render_offs.0 * (1.0 - offset_perc),
                                                item_render_offs.1 * (1.0 - offset_perc),
                                            );

                                            // TODO: This needs to be positions correctly and take rotation into account
                                            let mut item_render_base_pos: (f32, f32) = (
                                                centered_on_tile.0 + f32::from(offs.0) * 0.5
                                                    - 0.5
                                                        * (f32::from(offs.0)
                                                            / f32::from(BELT_LEN_PER_TILE)),
                                                centered_on_tile.1 + f32::from(offs.1) * 0.5
                                                    - 0.5
                                                        * (f32::from(offs.1)
                                                            / f32::from(BELT_LEN_PER_TILE)),
                                            );

                                            for (belt_pos, item) in items_iter {
                                                if let Some(item) = item {
                                                    let draw_pos = if belt_pos < last_moved.into() {
                                                        [
                                                            item_render_base_pos.0,
                                                            item_render_base_pos.1,
                                                        ]
                                                    } else {
                                                        [
                                                            item_render_base_pos.0 + slow_offset.0,
                                                            item_render_base_pos.1 + slow_offset.1,
                                                        ]
                                                    };

                                                    item_layer.draw_sprite(
                                                        &texture_atlas.items[item.id.into()],
                                                        // &texture_atlas.items[0],
                                                        DrawInstance {
                                                            position: draw_pos,
                                                            size: [
                                                                1.0 / f32::from(BELT_LEN_PER_TILE),
                                                                1.0 / f32::from(BELT_LEN_PER_TILE),
                                                            ],
                                                            animation_frame: 0,
                                                        },
                                                    );
                                                }

                                                item_render_base_pos = (
                                                    item_render_base_pos.0 + item_render_offs.0,
                                                    item_render_base_pos.1 + item_render_offs.1,
                                                );
                                            }
                                        },

                                        crate::frontend::world::tile::Entity::Underground {
                                            pos,
                                            direction,
                                            ty,
                                            id,
                                            underground_dir,
                                            belt_pos,
                                        } => {
                                            texture_atlas.belt[*direction].draw(
                                                [
                                                    chunk_draw_offs.0 + (pos.x % 16) as f32,
                                                    chunk_draw_offs.1 + (pos.y % 16) as f32,
                                                ],
                                                [1, 1],
                                                (game_state
                                                    .simulation_state
                                                    .factory
                                                    .belts
                                                    .inner
                                                    .belt_update_timers_cumulative
                                                    [*ty as usize]
                                                    as f32
                                                    / 120.0
                                                    * BELT_ANIM_SPEED
                                                    * (texture_atlas.belt[*direction]
                                                        .sprite
                                                        .texture
                                                        .number_anim_frames
                                                        as f32))
                                                    as u32
                                                    % texture_atlas.belt[*direction]
                                                        .sprite
                                                        .texture
                                                        .number_anim_frames,
                                                entity_layer,
                                            );

                                            texture_atlas.underground[*direction][*underground_dir]
                                                .draw(
                                                    [
                                                        chunk_draw_offs.0 + (pos.x % 16) as f32,
                                                        chunk_draw_offs.1 + (pos.y % 16) as f32,
                                                    ],
                                                    [1, 1],
                                                    0,
                                                    entity_overlay_layer,
                                                );

                                            // Draw Items
                                            let items_iter = game_state
                                                .simulation_state
                                                .factory
                                                .belts
                                                .get_item_iter(*id)
                                                .into_iter()
                                                .enumerate()
                                                .skip(
                                                    (belt_pos
                                                        .checked_sub(BELT_LEN_PER_TILE)
                                                        .expect("Belt idx wrapped?!?"))
                                                    .into(),
                                                )
                                                .take(BELT_LEN_PER_TILE.into());

                                            let offs = direction.into_offset();
                                            let item_render_offs = (
                                                -f32::from(offs.0) / f32::from(BELT_LEN_PER_TILE),
                                                -f32::from(offs.1) / f32::from(BELT_LEN_PER_TILE),
                                            );

                                            let centered_on_tile = (
                                                chunk_draw_offs.0 + (pos.x % 16) as f32 + 0.5
                                                    - 0.5 * (1.0 / f32::from(BELT_LEN_PER_TILE)),
                                                chunk_draw_offs.1 + (pos.y % 16) as f32 + 0.5
                                                    - 0.5 * (1.0 / f32::from(BELT_LEN_PER_TILE)),
                                            );

                                            let last_moved: BeltLenType = game_state
                                                .simulation_state
                                                .factory
                                                .belts
                                                .get_last_moved_pos(*id);

                                            let belt_progress: u8 = game_state
                                                .simulation_state
                                                .factory
                                                .belts
                                                .get_belt_progress(*ty);

                                            let offset_perc = belt_progress as f32 / 120.0;

                                            let slow_offset = (
                                                item_render_offs.0 * (1.0 - offset_perc),
                                                item_render_offs.1 * (1.0 - offset_perc),
                                            );

                                            // TODO: This needs to be positions correctly and take rotation into account
                                            let mut item_render_base_pos: (f32, f32) = (
                                                centered_on_tile.0 + f32::from(offs.0) * 0.5
                                                    - 0.5
                                                        * (f32::from(offs.0)
                                                            / f32::from(BELT_LEN_PER_TILE)),
                                                centered_on_tile.1 + f32::from(offs.1) * 0.5
                                                    - 0.5
                                                        * (f32::from(offs.1)
                                                            / f32::from(BELT_LEN_PER_TILE)),
                                            );

                                            for (belt_pos, item) in items_iter {
                                                if let Some(item) = item {
                                                    let draw_pos = if belt_pos < last_moved.into() {
                                                        [
                                                            item_render_base_pos.0,
                                                            item_render_base_pos.1,
                                                        ]
                                                    } else {
                                                        [
                                                            item_render_base_pos.0 + slow_offset.0,
                                                            item_render_base_pos.1 + slow_offset.1,
                                                        ]
                                                    };

                                                    item_layer.draw_sprite(
                                                        &texture_atlas.items[item.id.into()],
                                                        // &texture_atlas.items[0],
                                                        DrawInstance {
                                                            position: draw_pos,
                                                            size: [
                                                                1.0 / f32::from(BELT_LEN_PER_TILE),
                                                                1.0 / f32::from(BELT_LEN_PER_TILE),
                                                            ],
                                                            animation_frame: 0,
                                                        },
                                                    );
                                                }

                                                item_render_base_pos = (
                                                    item_render_base_pos.0 + item_render_offs.0,
                                                    item_render_base_pos.1 + item_render_offs.1,
                                                );
                                            }
                                        },

                                        Entity::Inserter {
                                            pos,
                                            direction,
                                            info,
                                            ..
                                        } => {
                                            entity_layer.draw_sprite(
                                                &texture_atlas.inserter[*direction],
                                                DrawInstance {
                                                    position: [
                                                        chunk_draw_offs.0 + (pos.x % 16) as f32,
                                                        chunk_draw_offs.1 + (pos.y % 16) as f32,
                                                    ],
                                                    size: [1.0, 1.0],
                                                    animation_frame: 0,
                                                },
                                            );

                                            match info {
                                    crate::frontend::world::tile::InserterInfo::NotAttached {
                                        start_pos,
                                        end_pos,
                                    } => {},
                                    crate::frontend::world::tile::InserterInfo::Attached {
                                        start_pos,
                                        end_pos,
                                        info,
                                    } => {
                                        // match info {
                                        //     crate::frontend::world::tile::AttachedInserter::BeltStorage { id, belt_pos } => todo!(),
                                        //     crate::frontend::world::tile::AttachedInserter::BeltBelt { item, inserter } => todo!(),
                                        //     crate::frontend::world::tile::AttachedInserter::StorageStorage { item, inserter } => {
                                        //         // let info = game_state.simulation_state.factory.storage_storage_inserters

                                        //     },
                                        // }
                                    },
                                }
                                        },

                                        Entity::PowerPole {
                                            ty,
                                            pos,
                                            connected_power_poles,
                                        } => {
                                            // TODO:
                                            // println!("Pole at {pos:?}, with grid: {grid_id}");
                                            let size =
                                                data_store.power_pole_data[usize::from(*ty)].size;
                                            let size = [size.0, size.1];
                                            texture_atlas.power_pole.draw(
                                                [
                                                    chunk_draw_offs.0 + (pos.x % 16) as f32,
                                                    chunk_draw_offs.1 + (pos.y % 16) as f32,
                                                ],
                                                size,
                                                0,
                                                entity_layer,
                                            );

                                            let power_range = data_store.power_pole_data
                                                [usize::from(*ty)]
                                            .power_range;

                                            if let ActionStateMachineState::Holding(
                                                HeldObject::Entity(e),
                                            ) = state_machine.state
                                            {
                                                if e.cares_about_power() {
                                                    range_layer.draw_sprite(
                                                        &texture_atlas.dark_square,
                                                        DrawInstance {
                                                            position: [
                                                                chunk_draw_offs.0
                                                                    + (pos.x % 16) as f32
                                                                    - power_range as f32,
                                                                chunk_draw_offs.1
                                                                    + (pos.y % 16) as f32
                                                                    - power_range as f32,
                                                            ],
                                                            size: [
                                                                power_range as f32 * 2.0
                                                                    + size[0] as f32,
                                                                power_range as f32 * 2.0
                                                                    + size[1] as f32,
                                                            ],
                                                            animation_frame: 0,
                                                        },
                                                    );
                                                }
                                            }
                                        },

                                        Entity::Splitter { pos, direction, id } => {
                                            let [inputs, outputs] = game_state
                                                .simulation_state
                                                .factory
                                                .belts
                                                .get_splitter_belt_ids(*id);

                                            let (left_pos, right_pos) = match direction {
                                                Dir::North => (*pos, *pos + Dir::East),
                                                Dir::East => (*pos, *pos + Dir::South),
                                                Dir::South => (*pos + Dir::East, *pos),
                                                Dir::West => (*pos + Dir::South, *pos),
                                            };

                                            // FIXME: We currently do not take partial movement (from slow belt speeds) into account, which leads to ugly jumping of the items on the belt
                                            for ((pos, input), output) in [left_pos, right_pos]
                                                .into_iter()
                                                .zip(inputs)
                                                .zip(outputs)
                                            {
                                                texture_atlas.belt[*direction].draw(
                                                    [
                                                        chunk_draw_offs.0 + (pos.x % 16) as f32,
                                                        chunk_draw_offs.1 + (pos.y % 16) as f32,
                                                    ],
                                                    [1, 1],
                                                    0,
                                                    entity_layer,
                                                );

                                                let centered_on_tile = (
                                                    chunk_draw_offs.0 + (pos.x % 16) as f32 + 0.5
                                                        - 0.5
                                                            * (1.0 / f32::from(BELT_LEN_PER_TILE)),
                                                    chunk_draw_offs.1 + (pos.y % 16) as f32 + 0.5
                                                        - 0.5
                                                            * (1.0 / f32::from(BELT_LEN_PER_TILE)),
                                                );
                                                let offs = direction.into_offset();
                                                render_items_straight::<ItemIdxType, RecipeIdxType>(
                                                    game_state
                                                        .simulation_state
                                                        .factory
                                                        .belts
                                                        .get_item_iter(input),
                                                    *direction,
                                                    SPLITTER_BELT_LEN,
                                                    SPLITTER_BELT_LEN,
                                                    (
                                                        centered_on_tile.0
                                                            - f32::from(offs.0)
                                                                * (1.0
                                                                    / f32::from(BELT_LEN_PER_TILE)),
                                                        centered_on_tile.1
                                                            - f32::from(offs.1)
                                                                * (0.5
                                                                    / f32::from(BELT_LEN_PER_TILE)),
                                                    ),
                                                    item_layer,
                                                    texture_atlas,
                                                );
                                                let out_belt_len = game_state
                                                    .simulation_state
                                                    .factory
                                                    .belts
                                                    .get_len(output);
                                                let out_belt_iter = game_state
                                                    .simulation_state
                                                    .factory
                                                    .belts
                                                    .get_item_iter(output);
                                                let offs = direction.into_offset();
                                                let item_render_base_pos: (f32, f32) = (
                                                    centered_on_tile.0 + f32::from(offs.0) * 0.5
                                                        - 0.5
                                                            * (f32::from(offs.0)
                                                                / f32::from(BELT_LEN_PER_TILE)),
                                                    centered_on_tile.1 + f32::from(offs.1) * 0.5
                                                        - 0.5
                                                            * (f32::from(offs.1)
                                                                / f32::from(BELT_LEN_PER_TILE)),
                                                );
                                                render_items_straight::<ItemIdxType, RecipeIdxType>(
                                                    out_belt_iter,
                                                    *direction,
                                                    out_belt_len,
                                                    SPLITTER_BELT_LEN,
                                                    item_render_base_pos,
                                                    item_layer,
                                                    texture_atlas,
                                                );
                                            }
                                            // todo!()
                                        },
                                        Entity::Chest {
                                            ty,
                                            pos,
                                            item,
                                            slot_limit: _,
                                        } => {
                                            let size =
                                                data_store.chest_tile_sizes[usize::from(*ty)];
                                            let size = [size.0, size.1];
                                            texture_atlas.chest.draw(
                                                [
                                                    chunk_draw_offs.0 + (pos.x % 16) as f32,
                                                    chunk_draw_offs.1 + (pos.y % 16) as f32,
                                                ],
                                                size,
                                                0,
                                                entity_layer,
                                            );
                                        },
                                        Entity::SolarPanel { ty, pos, .. } => {
                                            entity_layer.draw_sprite(
                                                &texture_atlas.default,
                                                DrawInstance {
                                                    position: [
                                                        chunk_draw_offs.0 + (pos.x % 16) as f32,
                                                        chunk_draw_offs.1 + (pos.y % 16) as f32,
                                                    ],
                                                    size: data_store.solar_panel_info
                                                        [usize::from(*ty)]
                                                    .size
                                                    .map(|v| v as f32),
                                                    animation_frame: 0,
                                                },
                                            );
                                        },
                                        // TODO: Render if a lab is working!
                                        Entity::Lab {
                                            ty,
                                            pos,
                                            pole_position,
                                            ..
                                        } => {
                                            let size = data_store.lab_info[usize::from(*ty)].size;
                                            let size = [size.0, size.1];

                                            texture_atlas.lab.draw(
                                                [
                                                    chunk_draw_offs.0 + (pos.x % 16) as f32,
                                                    chunk_draw_offs.1 + (pos.y % 16) as f32,
                                                ],
                                                size,
                                                0,
                                                entity_layer,
                                            );

                                            if let Some((pole_pos, _, _)) = pole_position {
                                                let grid = game_state
                                                    .simulation_state
                                                    .factory
                                                    .power_grids
                                                    .pole_pos_to_grid_id[pole_pos];

                                                let last_power = game_state
                                                    .simulation_state
                                                    .factory
                                                    .power_grids
                                                    .power_grids[usize::from(grid)]
                                                .last_power_mult;

                                                if last_power == 0 {
                                                    texture_atlas.no_power.draw_centered_on(
                                                        &texture_atlas.lab,
                                                        [
                                                            chunk_draw_offs.0 + (pos.x % 16) as f32,
                                                            chunk_draw_offs.1 + (pos.y % 16) as f32,
                                                        ],
                                                        size,
                                                        0,
                                                        warning_layer,
                                                    );
                                                }
                                            } else {
                                                texture_atlas.not_connected.draw_centered_on(
                                                    &texture_atlas.lab,
                                                    [
                                                        chunk_draw_offs.0 + (pos.x % 16) as f32,
                                                        chunk_draw_offs.1 + (pos.y % 16) as f32,
                                                    ],
                                                    size,
                                                    0,
                                                    warning_layer,
                                                );
                                            }
                                        },

                                        Entity::Beacon {
                                            ty,
                                            pos,
                                            modules,
                                            pole_position,
                                        } => {
                                            let size =
                                                data_store.beacon_info[usize::from(*ty)].size;
                                            let size = [size.0, size.1];

                                            texture_atlas.beacon.draw(
                                                [
                                                    chunk_draw_offs.0 + (pos.x % 16) as f32,
                                                    chunk_draw_offs.1 + (pos.y % 16) as f32,
                                                ],
                                                size,
                                                0,
                                                entity_layer,
                                            );

                                            if let Some((pole_pos, _)) = pole_position {
                                                let grid = game_state
                                                    .simulation_state
                                                    .factory
                                                    .power_grids
                                                    .pole_pos_to_grid_id[pole_pos];

                                                let last_power = game_state
                                                    .simulation_state
                                                    .factory
                                                    .power_grids
                                                    .power_grids[usize::from(grid)]
                                                .last_power_mult;

                                                if last_power == 0 {
                                                    texture_atlas.no_power.draw_centered_on(
                                                        &texture_atlas.beacon,
                                                        [
                                                            chunk_draw_offs.0 + (pos.x % 16) as f32,
                                                            chunk_draw_offs.1 + (pos.y % 16) as f32,
                                                        ],
                                                        size,
                                                        0,
                                                        warning_layer,
                                                    );
                                                }
                                            } else {
                                                texture_atlas.not_connected.draw_centered_on(
                                                    &texture_atlas.beacon,
                                                    [
                                                        chunk_draw_offs.0 + (pos.x % 16) as f32,
                                                        chunk_draw_offs.1 + (pos.y % 16) as f32,
                                                    ],
                                                    size,
                                                    0,
                                                    warning_layer,
                                                );
                                            }

                                            // TODO: Render modules
                                        },
                                        Entity::FluidTank { ty, pos, rotation } => {
                                            let size =
                                                data_store.fluid_tank_infos[usize::from(*ty)].size;

                                            texture_atlas.belt[*rotation].draw(
                                                [
                                                    chunk_draw_offs.0 + (pos.x % 16) as f32,
                                                    chunk_draw_offs.1 + (pos.y % 16) as f32,
                                                ],
                                                size,
                                                0,
                                                entity_layer,
                                            );
                                        },

                                        e => todo!("{:?}", e),
                                    }
                                }
                            }
                        },
                        None => {
                            for y in 0..16 {
                                for x in 0..16 {
                                    tile_layer.draw_sprite(
                                        &texture_atlas.outside_world,
                                        DrawInstance {
                                            position: [
                                                chunk_draw_offs.0 + x as f32,
                                                chunk_draw_offs.1 + y as f32,
                                            ],
                                            size: [1.0, 1.0],
                                            animation_frame: 0,
                                        },
                                    );
                                }
                            }
                        },
                    }

                    layers
                },
            )
            .reduce(
                || layers_tile_grid(tilesize, ar),
                |mut a, b| {
                    a.extend(b);
                    a
                },
            )
    };

    for (player_id, player) in game_state
        .world
        .players
        .iter()
        .enumerate()
        .filter(|(_, p)| p.visible)
        .filter(|(i, _)| {
            *i != state_machine.my_player_id as usize || state_machine.map_view_info.is_some()
        })
    {
        player_layer.draw_sprite(
            &texture_atlas.player,
            DrawInstance {
                position: [
                    player.pos.0 - camera_pos.0 + num_tiles_across_screen_horizontal / 2.0,
                    player.pos.1 - camera_pos.1 + num_tiles_across_screen_vertical / 2.0,
                ],
                size: [1.0, 2.0],
                animation_frame: 0,
            },
        );
        info!(
            "Rendering other player {} at {:?}",
            player_id,
            [player.pos.0 - camera_pos.0, player.pos.1 - camera_pos.1,]
        );
    }

    if state_machine.map_view_info.is_none() {
        player_layer.draw_sprite(
            &texture_atlas.player,
            DrawInstance {
                // Always in the middle
                position: [
                    num_tiles_across_screen_horizontal / 2.0,
                    num_tiles_across_screen_vertical / 2.0,
                ],
                size: [1.0, 2.0],
                animation_frame: 0,
            },
        );
        trace!("Rendering self at {:?}", state_machine.local_player_pos);
    }

    mem::drop(game_state);

    match &state_machine.state {
        ActionStateMachineState::CtrlCPressed => {},
        ActionStateMachineState::CopyDragInProgress { start_pos } => {
            let end_pos = ActionStateMachine::<ItemIdxType, RecipeIdxType>::player_mouse_to_tile(
                state_machine.zoom_level,
                camera_pos,
                state_machine.current_mouse_pos,
            );

            let bottom_right = Position {
                x: max(start_pos.x, end_pos.x) + 1,
                y: max(start_pos.y, end_pos.y) + 1,
            };

            let base_pos = Position {
                x: min(start_pos.x, end_pos.x),
                y: min(start_pos.y, end_pos.y),
            };

            entity_overlay_layer.draw_sprite(
                &texture_atlas.dark_square,
                DrawInstance {
                    position: [
                        base_pos.x as f32 - camera_pos.0 + num_tiles_across_screen_horizontal / 2.0,
                        base_pos.y as f32 - camera_pos.1 + num_tiles_across_screen_vertical / 2.0,
                    ],
                    size: [
                        (bottom_right.x - base_pos.x) as f32,
                        (bottom_right.y - base_pos.y) as f32,
                    ],
                    animation_frame: 0,
                },
            );
        },

        crate::frontend::action::action_state_machine::ActionStateMachineState::Idle => {},
        crate::frontend::action::action_state_machine::ActionStateMachineState::Holding(e) => {
            match e {
                crate::frontend::action::action_state_machine::HeldObject::Blueprint(bp) => {
                    let Position { x, y } =
                        ActionStateMachine::<ItemIdxType, RecipeIdxType>::player_mouse_to_tile(
                            state_machine.zoom_level,
                            camera_pos,
                            state_machine.current_mouse_pos,
                        );

                    bp.draw(
                        (
                            x as f32 + num_tiles_across_screen_horizontal / 2.0,
                            y as f32 + num_tiles_across_screen_vertical / 2.0,
                        ),
                        camera_pos,
                        &mut entity_overlay_layer,
                        texture_atlas,
                        data_store,
                    );
                },

                crate::frontend::action::action_state_machine::HeldObject::Tile(floor_tile) => {
                    // TODO
                },
                crate::frontend::action::action_state_machine::HeldObject::Entity(
                    place_entity_type,
                ) => match place_entity_type {
                    crate::frontend::world::tile::PlaceEntityType::Assembler { ty, pos } => {
                        let size: [u16; 2] = [
                            data_store.assembler_info[usize::from(*ty)].size.0,
                            data_store.assembler_info[usize::from(*ty)].size.1,
                        ];
                        texture_atlas.assembler.draw(
                            [
                                pos.x as f32 - camera_pos.0
                                    + num_tiles_across_screen_horizontal / 2.0,
                                pos.y as f32 - camera_pos.1
                                    + num_tiles_across_screen_vertical / 2.0,
                            ],
                            size,
                            0,
                            &mut state_machine_layer,
                        );
                    },
                    crate::frontend::world::tile::PlaceEntityType::Inserter {
                        pos,
                        dir,
                        filter,
                    } => {
                        let size: [u16; 2] = [1, 1];
                        state_machine_layer.draw_sprite(
                            &texture_atlas.inserter[*dir],
                            DrawInstance {
                                position: [
                                    pos.x as f32 - camera_pos.0
                                        + num_tiles_across_screen_horizontal / 2.0,
                                    pos.y as f32 - camera_pos.1
                                        + num_tiles_across_screen_vertical / 2.0,
                                ],
                                size: [size[0].into(), size[1].into()],
                                animation_frame: 0,
                            },
                        );
                    },
                    crate::frontend::world::tile::PlaceEntityType::Belt { pos, direction, ty } => {
                        let size: [u16; 2] = [1, 1];
                        texture_atlas.belt[*direction].draw(
                            [
                                pos.x as f32 - camera_pos.0
                                    + num_tiles_across_screen_horizontal / 2.0,
                                pos.y as f32 - camera_pos.1
                                    + num_tiles_across_screen_vertical / 2.0,
                            ],
                            size,
                            0,
                            &mut state_machine_layer,
                        );
                    },
                    crate::frontend::world::tile::PlaceEntityType::Underground {
                        pos,
                        direction,
                        ty,
                        underground_dir,
                    } => {
                        let size: [u16; 2] = [1, 1];
                        // TODO:
                        texture_atlas.belt[*direction].draw(
                            [
                                pos.x as f32 - camera_pos.0
                                    + num_tiles_across_screen_horizontal / 2.0,
                                pos.y as f32 - camera_pos.1
                                    + num_tiles_across_screen_vertical / 2.0,
                            ],
                            size,
                            0,
                            &mut state_machine_layer,
                        );

                        texture_atlas.underground[*direction][*underground_dir].draw(
                            [
                                pos.x as f32 - camera_pos.0
                                    + num_tiles_across_screen_horizontal / 2.0,
                                pos.y as f32 - camera_pos.1
                                    + num_tiles_across_screen_vertical / 2.0,
                            ],
                            [1, 1],
                            0,
                            &mut entity_overlay_layer,
                        );
                    },
                    crate::frontend::world::tile::PlaceEntityType::PowerPole { pos, ty } => {
                        let size: [u16; 2] = [
                            data_store.power_pole_data[usize::from(*ty)].size.0,
                            data_store.power_pole_data[usize::from(*ty)].size.1,
                        ];
                        texture_atlas.power_pole.draw(
                            [
                                pos.x as f32 - camera_pos.0
                                    + num_tiles_across_screen_horizontal / 2.0,
                                pos.y as f32 - camera_pos.1
                                    + num_tiles_across_screen_vertical / 2.0,
                            ],
                            size,
                            0,
                            &mut state_machine_layer,
                        );

                        let power_range = data_store.power_pole_data[usize::from(*ty)].power_range;

                        state_machine_layer.draw_sprite(
                            &texture_atlas.dark_square,
                            DrawInstance {
                                position: [
                                    (pos.x as f32 - power_range as f32) - camera_pos.0
                                        + num_tiles_across_screen_horizontal / 2.0,
                                    (pos.y as f32 - power_range as f32) - camera_pos.1
                                        + num_tiles_across_screen_vertical / 2.0,
                                ],
                                size: [
                                    power_range as f32 * 2.0 + size[0] as f32,
                                    power_range as f32 * 2.0 + size[1] as f32,
                                ],
                                animation_frame: 0,
                            },
                        );
                    },
                    crate::frontend::world::tile::PlaceEntityType::Splitter {
                        pos,
                        direction,
                        in_mode,
                        out_mode,

                        ty,
                    } => {},
                    crate::frontend::world::tile::PlaceEntityType::Chest { pos, ty } => {
                        let size = data_store.chest_tile_sizes[usize::from(*ty)];
                        let size = [size.0, size.1];
                        texture_atlas.chest.draw(
                            [
                                pos.x as f32 - camera_pos.0
                                    + num_tiles_across_screen_horizontal / 2.0,
                                pos.y as f32 - camera_pos.1
                                    + num_tiles_across_screen_vertical / 2.0,
                            ],
                            size,
                            0,
                            &mut state_machine_layer,
                        );
                    },
                    crate::frontend::world::tile::PlaceEntityType::SolarPanel { pos, ty } => {
                        let size = data_store.solar_panel_info[usize::from(*ty)].size;
                        state_machine_layer.draw_sprite(
                            &texture_atlas.default,
                            DrawInstance {
                                position: [
                                    pos.x as f32 - camera_pos.0
                                        + num_tiles_across_screen_horizontal / 2.0,
                                    pos.y as f32 - camera_pos.1
                                        + num_tiles_across_screen_vertical / 2.0,
                                ],
                                size: [size[0] as f32, size[1] as f32],
                                animation_frame: 0,
                            },
                        );
                    },
                    crate::frontend::world::tile::PlaceEntityType::Lab { pos, ty } => {
                        let size = data_store.lab_info[usize::from(*ty)].size;
                        let size = [size.0, size.1];

                        texture_atlas.lab.draw(
                            [
                                pos.x as f32 - camera_pos.0
                                    + num_tiles_across_screen_horizontal / 2.0,
                                pos.y as f32 - camera_pos.1
                                    + num_tiles_across_screen_vertical / 2.0,
                            ],
                            size,
                            0,
                            &mut state_machine_layer,
                        );
                    },
                    crate::frontend::world::tile::PlaceEntityType::Beacon { pos, ty } => {
                        let size = data_store.beacon_info[usize::from(*ty)].size;
                        let size = [size.0, size.1];

                        texture_atlas.beacon.draw(
                            [
                                pos.x as f32 - camera_pos.0
                                    + num_tiles_across_screen_horizontal / 2.0,
                                pos.y as f32 - camera_pos.1
                                    + num_tiles_across_screen_vertical / 2.0,
                            ],
                            size,
                            0,
                            &mut state_machine_layer,
                        );

                        let effect_range = data_store.beacon_info[usize::from(*ty)].effect_range;

                        state_machine_layer.draw_sprite(
                            &texture_atlas.dark_square,
                            DrawInstance {
                                position: [
                                    (pos.x as f32 - ((effect_range.0 - size[0]) / 2) as f32)
                                        - camera_pos.0
                                        + num_tiles_across_screen_horizontal / 2.0,
                                    (pos.y as f32 - ((effect_range.1 - size[1]) / 2) as f32)
                                        - camera_pos.1
                                        + num_tiles_across_screen_vertical / 2.0,
                                ],
                                size: [effect_range.0 as f32, effect_range.1 as f32],
                                animation_frame: 0,
                            },
                        );
                    },
                    crate::frontend::world::tile::PlaceEntityType::FluidTank {
                        ty,
                        pos,
                        rotation,
                    } => {
                        let size: [u16; 2] = [1, 1];
                        texture_atlas.belt[*rotation].draw(
                            [
                                pos.x as f32 - camera_pos.0
                                    + num_tiles_across_screen_horizontal / 2.0,
                                pos.y as f32 - camera_pos.1
                                    + num_tiles_across_screen_vertical / 2.0,
                            ],
                            size,
                            0,
                            &mut state_machine_layer,
                        );
                    },
                    crate::frontend::world::tile::PlaceEntityType::MiningDrill {
                        ty,
                        pos,
                        rotation,
                    } => {
                        // FIXME: Rotation
                        let size = data_store.mining_drill_info[usize::from(*ty)].size;

                        // TODO:
                        texture_atlas.chest.draw(
                            [
                                pos.x as f32 - camera_pos.0
                                    + num_tiles_across_screen_horizontal / 2.0,
                                pos.y as f32 - camera_pos.1
                                    + num_tiles_across_screen_vertical / 2.0,
                            ],
                            size,
                            0,
                            &mut state_machine_layer,
                        );

                        let mining_range =
                            data_store.mining_drill_info[usize::from(*ty)].mining_range;

                        state_machine_layer.draw_sprite(
                            &texture_atlas.dark_square,
                            DrawInstance {
                                position: [
                                    (pos.x as f32 - ((mining_range[0] - size[0]) / 2) as f32)
                                        - camera_pos.0
                                        + num_tiles_across_screen_horizontal / 2.0,
                                    (pos.y as f32 - ((mining_range[1] - size[1]) / 2) as f32)
                                        - camera_pos.1
                                        + num_tiles_across_screen_vertical / 2.0,
                                ],
                                size: [mining_range[0] as f32, mining_range[1] as f32],
                                animation_frame: 0,
                            },
                        );
                    },
                },
            }
        },
        crate::frontend::action::action_state_machine::ActionStateMachineState::Viewing(_) => {
            // TODO:
        },
        crate::frontend::action::action_state_machine::ActionStateMachineState::Deconstructing(
            _,
            _,
        ) => {
            // TODO:
        },
    }

    mem::drop(state_machine);

    {
        profiling::scope!("Draw layers");
        {
            let Layers {
                tile_layer,
                entity_layer,
                entity_overlay_layer,
                item_layer,
                warning_layer,
                range_layer,
            } = folded_layers;
            renderer.draw(&tile_layer);

            renderer.draw(&entity_layer);

            renderer.draw(&item_layer);

            renderer.draw(&entity_overlay_layer);

            renderer.draw(&range_layer);

            renderer.draw(&warning_layer);
            renderer.draw(&player_layer);
        }
        renderer.draw(&state_machine_layer);
        renderer.draw(&entity_overlay_layer);
        renderer.draw(&player_layer);
    }
}

pub(super) enum EscapeMenuOptions {
    BackToMainMenu,
}

pub fn render_ui<ItemIdxType: IdxTrait, RecipeIdxType: IdxTrait>(
    ctx: &Context,
    ui: &mut Ui,
    mut state_machine: MutexGuard<ActionStateMachine<ItemIdxType, RecipeIdxType>>,
    mut game_state: MutexGuard<GameState<ItemIdxType, RecipeIdxType>>,
    data_store: MutexGuard<DataStore<ItemIdxType, RecipeIdxType>>,
) -> Result<
    impl Iterator<Item = ActionType<ItemIdxType, RecipeIdxType>> + use<ItemIdxType, RecipeIdxType>,
    EscapeMenuOptions,
> {
    let state_machine_ref = &mut *state_machine;
    let game_state_ref = &mut *game_state;
    let data_store_ref = &*data_store;
    let mut actions = vec![];

    if state_machine_ref.escape_menu_open {
        if let Some(escape_action) = Modal::new("Pause Window".into())
            .show(ctx, |ui| {
                ui.heading("Paused");
                if ui.button("Main Menu").clicked() {
                    return Some(EscapeMenuOptions::BackToMainMenu);
                }

                None
            })
            .inner
        {
            match escape_action {
                EscapeMenuOptions::BackToMainMenu => return Err(escape_action),
            }
        }
    }

    Window::new("Mouse Pos").default_open(true).show(ctx, |ui| {
        ui.label(
            format!(
                "{:?}",
                ActionStateMachine::<u8, u8>::player_mouse_to_tile(
                    state_machine_ref.zoom_level,
                    state_machine_ref
                        .map_view_info
                        .unwrap_or(state_machine_ref.local_player_pos),
                    state_machine_ref.current_mouse_pos
                )
            )
            .as_str(),
        )
    });

    Window::new("Import BP")
        .default_open(false)
        .show(ctx, |ui| {
            if ui.button("Import").clicked() {
                if let Some(path) = rfd::FileDialog::new().pick_file() {
                    if let Ok(file) = File::open(path) {
                        if let Ok(bp) = ron::de::from_reader(file) {
                            state_machine_ref.state =
                                ActionStateMachineState::Holding(HeldObject::Blueprint(bp));
                        }
                    }
                }
            }
        });

    Window::new("DEBUG USE WITH CARE")
        .default_open(false)
        .show(ctx, |ui| {
            if ui.button("⚠️DEFRAGMENT GAMESTATE").clicked() {
                let mut new_state = game_state_ref.clone();

                mem::swap(&mut new_state, &mut *game_state_ref);

                mem::drop(new_state);
            }
            ui.checkbox(
                &mut state_machine_ref.show_graph_dot_output,
                "Generate Belt Graph",
            );
            if state_machine_ref.show_graph_dot_output {
                let mut graph = format!(
                    "{:?}",
                    Dot::new(&game_state_ref.simulation_state.factory.belts.belt_graph)
                );

                ui.text_edit_multiline(&mut graph);
            }
        });

    Window::new("UPS").default_open(false).show(ctx, |ui| {
        let points = &game_state_ref.update_times.get_data_points(0)[0..30];
        ui.label(format!(
            "{:.1} UPS",
            1.0 / (points.iter().map(|v| v.dur).sum::<Duration>() / points.len() as u32)
                .as_secs_f32()
        ));
    });

    Window::new("BP").default_open(false).show(ctx, |ui| {
        let bp = if let ActionStateMachineState::Holding(HeldObject::Blueprint(bp)) =
            &state_machine_ref.state
        {
            Some(bp)
        } else {
            None
        };

        if ui
            .add_enabled(bp.is_some(), Button::new("Copy Blueprint String"))
            .clicked()
        {
            let s: String =
                ron::ser::to_string_pretty(bp.unwrap(), ron::ser::PrettyConfig::default()).unwrap();
            ctx.copy_text(s);
        }
    });

    Window::new("RawData").default_open(false).show(ctx, |ui| {
        let raw = get_raw_data_test();

        let mut s: String =
            ron::ser::to_string_pretty(&raw, ron::ser::PrettyConfig::default()).unwrap();
        ui.text_edit_multiline(&mut s);
    });

    ctx.set_cursor_icon(egui::CursorIcon::Default);
    match &state_machine_ref.state {
        ActionStateMachineState::CtrlCPressed => {
            ctx.set_cursor_icon(egui::CursorIcon::Copy);
        },
        ActionStateMachineState::CopyDragInProgress { start_pos } => {
            ctx.set_cursor_icon(egui::CursorIcon::Copy);
        },

        crate::frontend::action::action_state_machine::ActionStateMachineState::Idle => {},
        crate::frontend::action::action_state_machine::ActionStateMachineState::Holding(
            held_object,
        ) => {},
        crate::frontend::action::action_state_machine::ActionStateMachineState::Viewing(
            position,
        ) => {
            let mut viewing = true;
            Window::new("Viewing").open(&mut viewing).show(ctx, |ui| {
                let chunk = game_state_ref
                .world
                .get_chunk_for_tile(*position)
                .expect("Cannot find chunk for viewing");
            let entity = chunk.get_entity_at(*position, data_store_ref);

            if let Some(entity) = entity {
                match entity {
                    crate::frontend::world::tile::Entity::Assembler { ty, pos, info, modules } => {
                        let mut goal_recipe: Option<Recipe<RecipeIdxType>> = match info {
                            AssemblerInfo::UnpoweredNoRecipe => None,
                            AssemblerInfo::Unpowered(recipe) => Some(*recipe),
                            AssemblerInfo::PoweredNoRecipe(_) => None,
                            AssemblerInfo::Powered { id, .. } => Some(id.recipe),
                        };

                        ComboBox::new("Recipe list", "Recipes").selected_text(goal_recipe.map(|recipe| data_store_ref.recipe_display_names[usize_from(recipe.id)].as_str()).unwrap_or("Choose a recipe!")).show_ui(ui, |ui| {
                            data_store_ref.recipe_display_names.iter().enumerate().filter(|(i, recipe_name)| {
                                    (game_state_ref.settings.show_unresearched_recipes || game_state_ref.simulation_state.tech_state.get_active_recipes()[*i]) && data_store_ref.recipe_allowed_assembling_machines[*i].contains(ty)
                                }).for_each(|(i, recipe_name)| {

                                ui.selectable_value(&mut goal_recipe, Some(Recipe {id: i.try_into().unwrap()}), recipe_name);
                            });
                        });


                        match info {
                            crate::frontend::world::tile::AssemblerInfo::UnpoweredNoRecipe => {
                                ui.label(&data_store_ref.assembler_info[usize::from(*ty)].display_name);
                                if let Some(goal_recipe) = goal_recipe {
                                    actions.push(ActionType::SetRecipe(SetRecipeInfo { pos: *pos, recipe: goal_recipe }));
                                }
                            },
                            crate::frontend::world::tile::AssemblerInfo::Unpowered(recipe) => {
                                ui.label(&data_store_ref.assembler_info[usize::from(*ty)].display_name);
                                if let Some(goal_recipe) = goal_recipe {
                                    if goal_recipe != *recipe {
                                        actions.push(ActionType::SetRecipe(SetRecipeInfo { pos: *pos, recipe: goal_recipe }));
                                    }
                                }
                            },
                            crate::frontend::world::tile::AssemblerInfo::PoweredNoRecipe(_) => {
                                ui.label(&data_store_ref.assembler_info[usize::from(*ty)].display_name);
                                if let Some(goal_recipe) = goal_recipe {
                                    actions.push(ActionType::SetRecipe(SetRecipeInfo { pos: *pos, recipe: goal_recipe }));
                                }
                            },
                            crate::frontend::world::tile::AssemblerInfo::Powered {
                                id, ..
                            } => {
                                ui.label(&data_store_ref.assembler_info[usize::from(*ty)].display_name);

                                ui.label(format!("{:?}", *id));

                                if let Some(goal_recipe) = goal_recipe {
                                    if goal_recipe != id.recipe {
                                        actions.push(ActionType::SetRecipe(SetRecipeInfo { pos: *pos, recipe: goal_recipe }));
                                    }
                                }

                                // TODO:
                                // ui.label(data_store.recipe_names[usize_from(assembler_id.recipe.id)]);

                                let time_per_recipe = data_store_ref.recipe_timers[usize_from(id.recipe.id)] as f32;

                                let AssemblerOnclickInfo {
                                    inputs,
                                    outputs,
                                    timer_percentage,
                                    prod_timer_percentage,
                                    base_speed,
                                    speed_mod,
                                    prod_mod,
                                    power_consumption_mod,
                                    base_power_consumption,
                                } = game_state_ref
                                    .simulation_state
                                    .factory
                                    .power_grids
                                    .get_assembler_info(*id, data_store_ref);

                                let main_pb = ProgressBar::new(timer_percentage).show_percentage().corner_radius(CornerRadius::ZERO);
                                ui.add(main_pb);
                                let prod_pb = ProgressBar::new(prod_timer_percentage).fill(Color32::ORANGE).show_percentage().corner_radius(CornerRadius::ZERO);
                                ui.add(prod_pb);

                                // Render module slots
                                TableBuilder::new(ui).id_salt("Module Slots").columns(Column::auto(), modules.len()).body(|mut body| {
                                    body.row(1.0, |mut row| {
                                        for module in modules {
                                            row.col(|ui| {
                                                if let Some(module_id) = module {
                                                    ui.label(&data_store_ref.module_info[*module_id].display_name);
                                                } else {
                                                    ui.label("Empty Module Slot");
                                                }
                                            });
                                        }
                                    });
                                });

                                let crafting_speed = base_speed * (1.0 + speed_mod);
                                let time_per_craft = time_per_recipe / crafting_speed;

                                TableBuilder::new(ui).columns(Column::auto().resizable(false), inputs.len() + outputs.len()).body(|mut body| {
                                    body.row(5.0, |mut row| {
                                        for (item, count) in inputs.iter() {
                                            let (_, _, count_in_recipe) = data_store_ref.recipe_to_items_and_amounts[&id.recipe].iter().find(|(dir, recipe_item, _)| *dir == ItemRecipeDir::Ing && *item == *recipe_item).unwrap();
                                            row.col(|ui| {
                                                ui.add(egui::Label::new(&data_store_ref.item_display_names[usize_from(item.id)]).wrap_mode(egui::TextWrapMode::Extend));
                                                ui.add(egui::Label::new(format!("{}", *count)).wrap_mode(egui::TextWrapMode::Extend));
                                                ui.add(egui::Label::new(format!("{}/s", (*count_in_recipe as f32) / (time_per_craft / 60.0))).wrap_mode(egui::TextWrapMode::Extend));
                                            });
                                        }

                                        for (item, count) in outputs.iter() {
                                            let (_, _, count_in_recipe) = data_store_ref.recipe_to_items_and_amounts[&id.recipe].iter().find(|(dir, recipe_item, _)| *dir == ItemRecipeDir::Out && *item == *recipe_item).unwrap();
                                            row.col(|ui| {
                                                ui.add(egui::Label::new(&data_store_ref.item_display_names[usize_from(item.id)]).wrap_mode(egui::TextWrapMode::Extend));
                                                ui.add(egui::Label::new(format!("{}", *count)).wrap_mode(egui::TextWrapMode::Extend));
                                                ui.add(egui::Label::new(format!("{:.2}/s", (*count_in_recipe as f32) / (time_per_craft / 60.0) * (1.0 + prod_mod))).wrap_mode(egui::TextWrapMode::Extend));
                                            });
                                        }
                                    });
                                });



                                ui.label(format!("Crafting Speed: {:.2}({:+.0}%)", crafting_speed, speed_mod * 100.0));
                                ui.label(format!("Productivity: {:.1}%", prod_mod * 100.0));
                                ui.label(format!("Max Consumption: {}({:+.0}%)", Watt((base_power_consumption.0 as f64 * (1.0 + power_consumption_mod as f64)) as u64), power_consumption_mod * 100.0));

                            }
                        }
                    },
                    crate::frontend::world::tile::Entity::PowerPole {
                        pos,
                        ..
                    } => {
                        // TODO:
                        // let power_range = data_store.power_pole_data[usize::from(*ty)].power_range;
                        // let size = data_store.power_pole_data[usize::from(*ty)].size;

                        let grid_id = game_state_ref
                        .simulation_state
                        .factory
                        .power_grids
                        .pole_pos_to_grid_id[pos] as usize;

                        let pg = &game_state_ref.simulation_state.factory.power_grids.power_grids
                            [grid_id];

                        ui.label(format!("Power Grid number: {}", grid_id));

                        ui.columns_const(|[ui_consumption, ui_production, ui_storage]| {
                            // Power Consumption
                            ui_consumption.add(Label::new(RichText::new("Satisfaction").heading()).wrap_mode(egui::TextWrapMode::Extend));
                            ui_consumption.add(ProgressBar::new(pg.last_produced_power.0 as f32 / pg.last_power_consumption.0 as f32).corner_radius(CornerRadius::ZERO).fill(if pg.last_power_mult == MAX_POWER_MULT {
                                Color32::GREEN
                            } else if pg.last_power_mult > MAX_POWER_MULT / 2 {
                                Color32::YELLOW
                            } else {
                                Color32::RED
                            }).text(RichText::new(format!("{}/{}", pg.last_produced_power, pg.last_power_consumption)).color(Color32::BLACK)));


                            // Power Production
                            ui_production.add(Label::new(RichText::new("Production").heading()).wrap_mode(egui::TextWrapMode::Extend));
                            ui_production.add(ProgressBar::new(pg.last_produced_power.0 as f32 / pg.last_ticks_max_power_production.0 as f32).corner_radius(CornerRadius::ZERO).text(RichText::new(format!("{}/{}", pg.last_produced_power, pg.last_ticks_max_power_production)).color(Color32::BLACK)));


                            // Power Storage
                            let max_charge: Joule = pg.main_accumulator_count.iter().copied().zip(data_store_ref.accumulator_info.iter().map(|info| info.max_charge)).map(|(count, charge)| charge * count).sum();
                            let current_charge: Joule = pg.main_accumulator_count.iter().copied().zip(pg.main_accumulator_charge.iter().copied()).map(|(count, charge)| charge * count).sum();

                            ui_storage.add(Label::new(RichText::new("Accumulator charge").heading()).wrap_mode(egui::TextWrapMode::Extend));
                            ui_storage.add(ProgressBar::new(current_charge.0 as f32 / max_charge.0 as f32).corner_radius(CornerRadius::ZERO).text(RichText::new(format!("{}/{}", current_charge, max_charge)).color(Color32::BLACK)));
                        });

                        let timescale = 1;
                        let max_value_at_timescale = (MAX_POWER_MULT as f64) * (RELATIVE_INTERVAL_MULTS[..=timescale].iter().copied().product::<usize>() as f64);
                        let num_samples = NUM_SAMPLES_AT_INTERVALS[timescale];

                        let points = pg.power_mult_history.get_series(timescale, data_store_ref, Some(|_| true)).into_iter().map(|(_, series)| (series.name, series.data.into_iter()
                        .enumerate()
                        .map(|(i, v)| [i as f64, v.into()])
                        .collect::<Vec<_>>()));
                        let lines = points.into_iter().map(|(name, points)| {
                            Line::new(name, points)
                                .stroke(Stroke::new(2.0, Color32::GREEN))
                        });

                        TableBuilder::new(ui).columns(Column::auto(), 2).body(|body| {
                            body.rows(1.0, pg.num_assemblers_of_type.len() + pg.num_solar_panels_of_type.len(), |mut row| {
                                let i = row.index();

                                if i < pg.num_assemblers_of_type.len() {
                                    row.col(|ui| {
                                        ui.add(Label::new(&data_store_ref.assembler_info[i].display_name).extend());

                                    });
                                    row.col(|ui| {ui.add(Label::new(format!("{}", pg.num_assemblers_of_type[i])).extend());});
                                } else {
                                    let i = i - pg.num_assemblers_of_type.len();
                                    row.col(|ui| {
                                        ui.add(Label::new(&data_store_ref.solar_panel_info[i].display_name).extend());

                                    });
                                    row.col(|ui| {ui.add(Label::new(format!("{}", pg.num_solar_panels_of_type[i])).extend());});
                                }
                            });
                        });

                        ui.label(format!("{}", pg.last_power_consumption));

                        Plot::new("power_history_graph").show_x(false).show_y(false)
                        // .auto_bounds([true, false])
                        .set_margin_fraction([0.0, 0.05].into())
                        .y_grid_spacer(|_grid_input| (0..=4).map(|v| GridMark {
                            value: v as f64 / 4.0 * max_value_at_timescale, step_size: 1.0 / 4.0 * max_value_at_timescale }).chain((0..=20).map(|v| GridMark {
                                value: v as f64 / 20.0 * max_value_at_timescale, step_size: 1.0 / 20.0 * max_value_at_timescale })).collect())
                        .x_grid_spacer(|_grid_input| (0..NUM_X_AXIS_TICKS[timescale]).map(|v| GridMark {
                            value: v as f64 / (NUM_X_AXIS_TICKS[timescale] as f64) * (num_samples as f64), step_size: 1.0 / (NUM_X_AXIS_TICKS[timescale] as f64) * (num_samples as f64) }).collect())
                        .custom_y_axes([AxisHints::new_y().formatter(|v, _| format!("{:.0}%", v.value/max_value_at_timescale*100.0))].to_vec())
                        .custom_x_axes([AxisHints::new_x().formatter(|v, _| TIMESCALE_LEGEND[timescale](v.value))].to_vec())
                            .include_y(0)
                            .include_y(1.0 * max_value_at_timescale)
                            .allow_zoom([false, false])
                            .allow_drag([false, false])
                            .allow_scroll([false, false])
                            .show(ui, |ui| {
                                for line in lines {
                                    ui.line(line);
                                }
                            });
                    },
                    crate::frontend::world::tile::Entity::Belt {
                        id,
                        belt_pos,
                        ..
                    } => {
                        match id {
                            BeltTileId::AnyBelt(index, _) => {
                                ui.label("Belt");
                                ui.label(format!("Any Belt {}", *index).as_str());
                            },
                        }
                        ui.label(format!("Item: {:?}", game_state_ref.simulation_state.factory.belts.get_pure_item(*id)).as_str());

                        let mut dedup = Default::default();
                        let mut done = Default::default();
                        game_state_ref.simulation_state.factory.belts.get_items_which_could_end_up_on_that_belt(*id, &mut dedup, &mut done);
                        ui.label(format!("Possible items: {:?}", done[id]).as_str());

                        ui.label(format!("Inner: {:?}", game_state_ref.simulation_state.factory.belts.inner.belt_belt_inserters).as_str());

                        ui.label(format!("Belt Pos: {:?}", *belt_pos));
                    },
                    crate::frontend::world::tile::Entity::Underground {
                        id,
                        underground_dir,
                        belt_pos,
                        ..
                    } => {
                        match id {
                            BeltTileId::AnyBelt(index, _) => {
                                ui.label("Underground Belt");
                                ui.label(format!("Any Belt {}", *index).as_str());
                            },
                        }
                        ui.label(format!("Item: {:?}", game_state_ref.simulation_state.factory.belts.get_pure_item(*id)).as_str());

                        let mut dedup = Default::default();
                        let mut done = Default::default();
                        game_state_ref.simulation_state.factory.belts.get_items_which_could_end_up_on_that_belt(*id, &mut dedup, &mut done);
                        ui.label(format!("Possible items: {:?}", done[id]).as_str());

                        ui.label(format!("Inner: {:?}", game_state_ref.simulation_state.factory.belts.inner.belt_belt_inserters).as_str());

                        ui.label(format!("UndergroundDir: {:?}", *underground_dir));

                        ui.label(format!("Belt Pos: {:?}", *belt_pos));

                    },
                    crate::frontend::world::tile::Entity::Inserter {
                        user_movetime,
                        type_movetime,

                        pos,
                        info,
                        ..
                    } => {
                        ui.label("Inserter");
                        match info {
                            crate::frontend::world::tile::InserterInfo::NotAttached { .. } => {
                                ui.label("NotAttached");
                            },
                            crate::frontend::world::tile::InserterInfo::Attached {info: ins, ..} => match ins {
                                crate::frontend::world::tile::AttachedInserter::BeltStorage {
                                    id,
                                    belt_pos,
                                } => {
                                    ui.label("BeltStorage");

                                    ui.label(format!("belt_id: {:?}", *id));
                                    ui.label(format!("belt_pos: {}", *belt_pos));

                                    ui.label(format!("storage: {:?}", game_state_ref.simulation_state.factory.belts.get_inserter_info_at(*id, *belt_pos).expect("No inserter at pos indicated in entity!")));

                                    // TODO:
                                },
                                crate::frontend::world::tile::AttachedInserter::BeltBelt {
                                    item,
                                    inserter,
                                } => {
                                    ui.label("BeltBelt");
                                    // TODO:
                                },
                                crate::frontend::world::tile::AttachedInserter::StorageStorage { item,  .. } => {
                                    ui.label("StorageStorage");
                                    ui.label(&data_store_ref.item_display_names[usize_from(item.id)]);

                                    // TODO:
                                },
                            },
                        }

                        let mut movetime_overridden = user_movetime.is_some();

                        ui.checkbox(&mut movetime_overridden, "Override Swing Time");

                        if movetime_overridden {
                            let mut movetime = user_movetime.map(|v| v.into()).unwrap_or(*type_movetime);

                            ui.add(egui::Slider::new(&mut movetime, (*type_movetime)..=u16::MAX).text("Ticks per half swing"));

                            if *user_movetime != Some(movetime.try_into().unwrap()) {
                                actions.push(ActionType::OverrideInserterMovetime { pos: *pos, new_movetime: Some(movetime.try_into().unwrap()) });
                            }
                        } else if movetime_overridden != user_movetime.is_some() {
                            actions.push(ActionType::OverrideInserterMovetime { pos: *pos, new_movetime: None });
                        }
                    },
                    Entity::Splitter { id, .. } => {
                        let [inputs, outputs] = game_state_ref
                                    .simulation_state
                                    .factory
                                    .belts
                                    .get_splitter_belt_ids(*id);

                        ui.label(format!("Inputs: {:?}", inputs));
                        ui.label(format!("Outputs: {:?}", outputs));
                    },
                    Entity::Chest {
                        ty,
                        item,
                        ..
                    } => {
                        let Some((item, index)) = item else {
                            todo!()
                        };
                        ui.label(&data_store_ref.item_display_names[usize_from(item.id)]);
                        ui.label(format!("{}", *index));

                        let stack_size: u16 = data_store_ref.item_stack_sizes[usize_from(item.id)] as u16;

                        let num_slots = data_store_ref.chest_num_slots[*ty as usize];
                        let (current_items, _max_items) = game_state_ref.simulation_state.factory.chests.stores[usize_from(item.id)].get_chest(*index);

                        TableBuilder::new(ui).columns(Column::auto().resizable(false), 10).body(|body| {
                            body.rows(5.0, (num_slots / 10) as usize + (num_slots % 10 > 0) as usize, |mut row| {
                                let idx = row.index();
                                for col_idx in 0..10 {
                                    let slot_id = idx * 10 + col_idx;
                                    if slot_id >= num_slots as usize {
                                        break;
                                    }
                                    row.col(|ui| {
                                        let this_slots_stack_count = min(current_items.saturating_sub(slot_id as u16 * stack_size), stack_size);

                                        let clicked = ui.add(Label::new(format!("{}", this_slots_stack_count)).extend()).clicked();
                                        let mut shift = false;
                                        ctx.input(|input| {shift = input.modifiers.shift; });

                                        if shift && clicked {
                                            todo!("Move the items into the players inventory if there is space");
                                        }
                                    });
                                }
                            });
                        });
                    },
                    Entity::Lab {   .. } => {
                        // TODO
                    },
                    Entity::SolarPanel {  .. } => {
                        // TODO
                    }
                    Entity::Beacon {   .. } => {
                        // TODO
                    },
                    Entity::FluidTank { ty, pos, rotation } => {
                        let id = game_state_ref.simulation_state.factory.fluid_store.fluid_box_pos_to_network_id[pos];

                        ui.label(format!("{:?}", id));
                    }
                    _ => todo!(),
                }
            }
            });

            if !viewing {
                state_machine_ref.state = ActionStateMachineState::Idle;
            }
        },
        crate::frontend::action::action_state_machine::ActionStateMachineState::Deconstructing(
            _,
            timer,
        ) => {
            Window::new("Deconstructing").show(ui.ctx(), |ui| {
                let pb = ProgressBar::new((*timer as f32) / 100.0);
                ui.add(pb);
            });
        },
    }

    egui::Area::new("Hotbar".into())
        .anchor(Align2::CENTER_BOTTOM, (0.0, 0.0))
        .show(ui.ctx(), |ui| {
            egui_extras::TableBuilder::new(ui)
                .columns(Column::auto().resizable(false), 10)
                .body(|mut body| {
                    body.row(30.0, |mut row| {
                        for i in 0..10 {
                            if row
                                .col(|ui| {
                                    let button_response = ui.button(format!("{i}"));

                                    if button_response.hovered() {
                                        dbg!(i);
                                    }
                                })
                                .1
                                .hovered()
                            {
                                dbg!(i);
                            };
                        }
                    });
                });
        });

    Window::new("Technology")
        .open(&mut state_machine_ref.technology_panel_open)
        .show(ctx, |ui| {
            let research_actions = game_state_ref
                .simulation_state
                .tech_state
                .render_tech_window(
                    ui,
                    state_machine_ref.tech_tree_render.get_or_insert(
                        game_state_ref
                            .simulation_state
                            .tech_state
                            .generate_render_graph(data_store_ref),
                    ),
                    data_store_ref,
                );

            actions.extend(research_actions);
        });

    Window::new("Statistics")
        .open(&mut state_machine_ref.statistics_panel_open)
        .show(ctx, |ui| {
            let time_scale = match &mut state_machine_ref.statistics_panel {
                StatisticsPanel::Items(timescale) => timescale,
                StatisticsPanel::Fluids(timescale) => timescale,
            };
            ui.with_layout(Layout::left_to_right(egui::Align::Min), |ui| {
                ui.radio_value(time_scale, 0, "10 Seconds");
                ui.radio_value(time_scale, 1, "1 Minute");
                ui.radio_value(time_scale, 2, "1 Hour");
            });

            let time_scale = *time_scale;

            match state_machine_ref.statistics_panel {
                StatisticsPanel::Items(scale) | StatisticsPanel::Fluids(scale) => {
                    let take_fluids = matches!(
                        state_machine_ref.statistics_panel,
                        StatisticsPanel::Fluids(_)
                    );

                    ui.columns_const(|[ui_production, ui_consumption]: &mut [Ui; 2]| {
                        ui_production.heading("Production");
                        ui_production.separator();
                        ui_consumption.heading("Consumption");
                        ui_consumption.separator();

                        let prod_points: Vec<(String, usize, f32, PlotPoints)> = game_state_ref
                            .statistics
                            .production
                            .get_series(
                                scale,
                                data_store_ref,
                                Some(|item: Item<ItemIdxType>| {
                                    data_store_ref.item_is_fluid[usize_from(item.id)] == take_fluids
                                }),
                            )
                            .into_iter()
                            .map(|(item_id, series)| (series.name, item_id, series.data))
                            .map(|(name, i, data)| {
                                (
                                    name,
                                    i,
                                    data.iter().copied().sum(),
                                    data.into_iter()
                                        .enumerate()
                                        .map(|(i, v)| [i as f64, v.into()])
                                        .collect(),
                                )
                            })
                            .filter(|(_, _, sum, _): &(_, _, f32, _)| *sum > 0.0)
                            .collect();

                        let max_prod = prod_points
                            .iter()
                            .map(|v| v.2)
                            .max_by(|a, b| {
                                if a < b {
                                    Ordering::Less
                                } else {
                                    Ordering::Greater
                                }
                            })
                            .unwrap_or(0.0);

                        let mut sum_list_prod: Vec<_> = prod_points
                            .iter()
                            .map(|v| (v.0.clone(), v.1, v.2))
                            .collect();

                        sum_list_prod.sort_by(|a, b| {
                            if a.2 < b.2 {
                                Ordering::Greater
                            } else {
                                Ordering::Less
                            }
                        });

                        let lines = prod_points
                            .into_iter()
                            .filter(|(_, id, _, _)| state_machine_ref.production_filters[*id])
                            .map(|(name, id, _sum, points)| {
                                Line::new(name, points)
                                    .stroke(Stroke::new(2.0, data_store_ref.item_to_colour[id]))
                            });

                        let ticks_per_value = RELATIVE_INTERVAL_MULTS[..=scale]
                            .iter()
                            .copied()
                            .product::<usize>()
                            as f64;

                        Plot::new("production_graph")
                            .set_margin_fraction([0.0, 0.05].into())
                            .x_grid_spacer(|_grid_input| {
                                (0..NUM_X_AXIS_TICKS[scale])
                                    .map(|v| GridMark {
                                        value: v as f64 / (NUM_X_AXIS_TICKS[scale] as f64)
                                            * (NUM_SAMPLES_AT_INTERVALS[scale] as f64),
                                        step_size: 1.0 / (NUM_X_AXIS_TICKS[scale] as f64)
                                            * (NUM_SAMPLES_AT_INTERVALS[scale] as f64),
                                    })
                                    .collect()
                            })
                            .y_grid_spacer(|grid_input| {
                                let max: f64 = grid_input.bounds.1;

                                let mut lower_dec = 10.0_f64
                                    .powf((max / ticks_per_value * 60.0 * 60.0).log10().floor());

                                if lower_dec < 1.0 {
                                    lower_dec = 1.0;
                                }

                                lower_dec = lower_dec * ticks_per_value / 60.0 / 60.0;

                                (0..40)
                                    .filter_map(|v| {
                                        ((v as f64) / 4.0 * lower_dec < max).then_some(GridMark {
                                            value: (v as f64) / 4.0 * lower_dec,
                                            step_size: lower_dec / 4.0,
                                        })
                                    })
                                    .chain((0..10).filter_map(|v| {
                                        ((v as f64) * lower_dec < max).then_some(GridMark {
                                            value: (v as f64) * lower_dec,
                                            step_size: 1.0 * lower_dec,
                                        })
                                    }))
                                    .collect()
                            })
                            .custom_y_axes(
                                [AxisHints::new_y().formatter(move |v, _| {
                                    format!("{:.1}/min", v.value / ticks_per_value * 60.0 * 60.0)
                                })]
                                .to_vec(),
                            )
                            .custom_x_axes(
                                [AxisHints::new_x()
                                    .formatter(|v, _| TIMESCALE_LEGEND[scale](v.value))]
                                .to_vec(),
                            )
                            .include_y(0)
                            .allow_zoom([false, false])
                            .allow_drag([false, false])
                            .allow_scroll([false, false])
                            .view_aspect(3.0)
                            .show(ui_production, |ui| {
                                for line in lines {
                                    ui.line(line);
                                }
                            });

                        let ticks_per_sample = RELATIVE_INTERVAL_MULTS[..=time_scale]
                            .iter()
                            .copied()
                            .product::<usize>();
                        let ticks_total = min(
                            ticks_per_sample * NUM_SAMPLES_AT_INTERVALS[time_scale],
                            game_state_ref
                                .statistics
                                .production
                                .num_samples_pushed
                                .next_multiple_of(ticks_per_sample)
                                - ticks_per_sample,
                        ) as f32;

                        let row_height = ui_production.spacing().interact_size.y;
                        ScrollArea::vertical().id_salt("Prod List Scroll").show(
                            ui_production,
                            |ui| {
                                TableBuilder::new(ui)
                                    .id_salt("Production List")
                                    .sense(Sense::click())
                                    .column(Column::auto())
                                    .column(Column::remainder())
                                    .column(Column::auto())
                                    .body(|body| {
                                        body.rows(row_height, sum_list_prod.len(), |mut row| {
                                            let idx = row.index();
                                            row.col(|ui| {
                                                if state_machine_ref.production_filters
                                                    [sum_list_prod[idx].1]
                                                {
                                                    ui.add(
                                                        Label::new(egui::RichText::new(
                                                            sum_list_prod[idx].0.as_str(),
                                                        ))
                                                        .extend(),
                                                    );
                                                } else {
                                                    ui.add(
                                                        Label::new(
                                                            egui::RichText::new(
                                                                sum_list_prod[idx].0.as_str(),
                                                            )
                                                            .strikethrough(),
                                                        )
                                                        .extend(),
                                                    );
                                                }
                                            });
                                            row.col(|ui| {
                                                ui.add(
                                                    ProgressBar::new(
                                                        sum_list_prod[idx].2 / max_prod,
                                                    )
                                                    .fill(
                                                        data_store_ref.item_to_colour
                                                            [sum_list_prod[idx].1],
                                                    )
                                                    .corner_radius(CornerRadius::ZERO),
                                                );
                                            });
                                            row.col(|ui| {
                                                ui.with_layout(
                                                    Layout::right_to_left(egui::Align::Center),
                                                    |ui| {
                                                        ui.add(
                                                            Label::new(format!(
                                                                "{:.0}/m",
                                                                sum_list_prod[idx].2 / ticks_total
                                                                    * TICKS_PER_SECOND_LOGIC as f32
                                                                    * 60.0
                                                            ))
                                                            .extend(),
                                                        );
                                                    },
                                                );
                                            });
                                            if row.response().clicked() {
                                                state_machine_ref.production_filters
                                                    [sum_list_prod[idx].1] = !state_machine_ref
                                                    .production_filters[sum_list_prod[idx].1];
                                            }
                                        });
                                    });
                            },
                        );

                        let cons_points: Vec<(String, usize, f32, PlotPoints)> = game_state_ref
                            .statistics
                            .consumption
                            .get_series(
                                scale,
                                data_store_ref,
                                Some(|item: Item<ItemIdxType>| {
                                    data_store_ref.item_is_fluid[usize_from(item.id)] == take_fluids
                                }),
                            )
                            .into_iter()
                            .map(|(item_id, series)| (series.name, item_id, series.data))
                            .map(|(name, i, data)| {
                                (
                                    name,
                                    i,
                                    data.iter().copied().sum(),
                                    data.into_iter()
                                        .enumerate()
                                        .map(|(i, v)| [i as f64, v.into()])
                                        .collect(),
                                )
                            })
                            .filter(|(_, _, sum, _): &(_, _, f32, _)| *sum > 0.0)
                            .collect();

                        let max_cons = cons_points
                            .iter()
                            .map(|v| v.2)
                            .max_by(|a, b| {
                                if a < b {
                                    Ordering::Less
                                } else {
                                    Ordering::Greater
                                }
                            })
                            .unwrap_or(0.0);

                        let mut sum_list_cons: Vec<_> = cons_points
                            .iter()
                            .map(|v| (v.0.clone(), v.1, v.2))
                            .collect();

                        sum_list_cons.sort_by(|a, b| {
                            if a.2 < b.2 {
                                Ordering::Greater
                            } else {
                                Ordering::Less
                            }
                        });

                        let lines = cons_points
                            .into_iter()
                            .filter(|(_, id, _, _)| state_machine_ref.consumption_filters[*id])
                            .map(|(name, id, _sum, points)| {
                                Line::new(name, points)
                                    .stroke(Stroke::new(2.0, data_store_ref.item_to_colour[id]))
                            });

                        let ticks_per_value = RELATIVE_INTERVAL_MULTS[..=scale]
                            .iter()
                            .copied()
                            .product::<usize>()
                            as f64;

                        Plot::new("consumption_graph")
                            .set_margin_fraction([0.0, 0.05].into())
                            .x_grid_spacer(|_grid_input| {
                                (0..NUM_X_AXIS_TICKS[scale])
                                    .map(|v| GridMark {
                                        value: v as f64 / (NUM_X_AXIS_TICKS[scale] as f64)
                                            * (NUM_SAMPLES_AT_INTERVALS[scale] as f64),
                                        step_size: 1.0 / (NUM_X_AXIS_TICKS[scale] as f64)
                                            * (NUM_SAMPLES_AT_INTERVALS[scale] as f64),
                                    })
                                    .collect()
                            })
                            .y_grid_spacer(|grid_input| {
                                let max: f64 = grid_input.bounds.1;

                                let mut lower_dec = 10.0_f64
                                    .powf((max / ticks_per_value * 60.0 * 60.0).log10().floor());

                                if lower_dec < 1.0 {
                                    lower_dec = 1.0;
                                }

                                lower_dec = lower_dec * ticks_per_value / 60.0 / 60.0;

                                (0..40)
                                    .filter_map(|v| {
                                        ((v as f64) / 4.0 * lower_dec < max).then_some(GridMark {
                                            value: (v as f64) / 4.0 * lower_dec,
                                            step_size: lower_dec / 4.0,
                                        })
                                    })
                                    .chain((0..10).filter_map(|v| {
                                        ((v as f64) * lower_dec < max).then_some(GridMark {
                                            value: (v as f64) * lower_dec,
                                            step_size: 1.0 * lower_dec,
                                        })
                                    }))
                                    .collect()
                            })
                            .custom_y_axes(
                                [AxisHints::new_y().formatter(move |v, _| {
                                    format!("{:.1}/min", v.value / ticks_per_value * 60.0 * 60.0)
                                })]
                                .to_vec(),
                            )
                            .custom_x_axes(
                                [AxisHints::new_x()
                                    .formatter(|v, _| TIMESCALE_LEGEND[scale](v.value))]
                                .to_vec(),
                            )
                            .include_y(0)
                            .allow_zoom([false, false])
                            .allow_drag([false, false])
                            .allow_scroll([false, false])
                            .view_aspect(3.0)
                            .show(ui_consumption, |ui| {
                                for line in lines {
                                    ui.line(line);
                                }
                            });

                        ScrollArea::vertical().id_salt("Cons List Scroll").show(
                            ui_consumption,
                            |ui| {
                                TableBuilder::new(ui)
                                    .id_salt("Consumption List")
                                    .sense(Sense::click())
                                    .column(Column::auto())
                                    .column(Column::remainder())
                                    .column(Column::auto())
                                    .body(|body| {
                                        body.rows(row_height, sum_list_cons.len(), |mut row| {
                                            let idx = row.index();
                                            row.col(|ui| {
                                                if state_machine_ref.consumption_filters
                                                    [sum_list_cons[idx].1]
                                                {
                                                    ui.add(
                                                        Label::new(egui::RichText::new(
                                                            sum_list_cons[idx].0.as_str(),
                                                        ))
                                                        .extend(),
                                                    );
                                                } else {
                                                    ui.add(
                                                        Label::new(
                                                            egui::RichText::new(
                                                                sum_list_cons[idx].0.as_str(),
                                                            )
                                                            .strikethrough(),
                                                        )
                                                        .extend(),
                                                    );
                                                }
                                            });
                                            row.col(|ui| {
                                                ui.add(
                                                    ProgressBar::new(
                                                        sum_list_cons[idx].2 / max_cons,
                                                    )
                                                    .fill(
                                                        data_store_ref.item_to_colour
                                                            [sum_list_cons[idx].1],
                                                    )
                                                    .corner_radius(CornerRadius::ZERO),
                                                );
                                            });
                                            row.col(|ui| {
                                                ui.with_layout(
                                                    Layout::right_to_left(egui::Align::Center),
                                                    |ui| {
                                                        ui.add(
                                                            Label::new(format!(
                                                                "{:.0}/m",
                                                                sum_list_cons[idx].2 / ticks_total
                                                                    * TICKS_PER_SECOND_LOGIC as f32
                                                                    * 60.0
                                                            ))
                                                            .extend(),
                                                        );
                                                    },
                                                );
                                            });
                                            if row.response().clicked() {
                                                state_machine_ref.consumption_filters
                                                    [sum_list_cons[idx].1] = !state_machine_ref
                                                    .consumption_filters[sum_list_cons[idx].1];
                                            }
                                        });
                                    });
                            },
                        );
                    });
                },
            }
        });

    mem::drop(game_state);
    mem::drop(state_machine);
    mem::drop(data_store);

    puffin_egui::profiler_window(ctx);

    Ok(actions.into_iter())
}

fn render_items_straight<ItemIdxType: IdxTrait, RecipeIdxType: IdxTrait>(
    full_items_iter: impl IntoIterator<Item = Option<Item<ItemIdxType>>>,
    dir: Dir,
    start_pos: u16,
    amount: u16,
    draw_pos_start_pos: (f32, f32),
    layer: &mut Layer,
    atlas: &TextureAtlas,
) {
    let items_iter = full_items_iter
        .into_iter()
        .skip((start_pos.checked_sub(amount).expect("Belt idx wrapped?!?")).into())
        .take(amount.into());

    let offs = dir.into_offset();
    let item_render_offs = (
        -f32::from(offs.0) / f32::from(BELT_LEN_PER_TILE),
        -f32::from(offs.1) / f32::from(BELT_LEN_PER_TILE),
    );

    let mut item_render_base_pos: (f32, f32) = draw_pos_start_pos;

    for item in items_iter {
        if let Some(item) = item {
            layer.draw_sprite(
                &atlas.items[item.id.into()],
                DrawInstance {
                    position: [item_render_base_pos.0, item_render_base_pos.1],
                    size: [
                        1.0 / f32::from(BELT_LEN_PER_TILE),
                        1.0 / f32::from(BELT_LEN_PER_TILE),
                    ],
                    animation_frame: 0,
                },
            );
        }

        item_render_base_pos = (
            item_render_base_pos.0 + item_render_offs.0,
            item_render_base_pos.1 + item_render_offs.1,
        );
    }
}
