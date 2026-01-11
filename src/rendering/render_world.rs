use crate::belt::belt::Belt;
#[cfg(feature = "debug-stat-gathering")]
use crate::belt::smart::{
    NUM_BELT_FREE_CACHE_HITS, NUM_BELT_INSERTER_UPDATES, NUM_BELT_LOCS_SEARCHED, NUM_BELT_UPDATES,
    NUM_INSERTER_LOADS_WAITING_FOR_ITEMS, NUM_INSERTER_LOADS_WAITING_FOR_SPACE,
    NUM_INSERTER_LOADS_WAITING_FOR_SPACE_IN_GUARANTEED_FULL, TIMES_ALL_INCOMING_EARLY_RETURN,
    TIMES_INSERTERS_EXTRACTED,
};

use crate::belt::smart::SmartBelt;
use crate::blueprint::blueprint_string::BlueprintString;
use crate::chest::ChestSize;
#[cfg(not(all(target_arch = "wasm32", target_os = "unknown")))]
use crate::frontend::action::action_state_machine::ForkSaveInfo;
use crate::frontend::action::place_entity::EntityPlaceOptions;
use crate::frontend::action::place_entity::PlaceEntityInfo;
use crate::frontend::world::tile::BeltState;
use crate::frontend::world::tile::PlaceEntityType;
use crate::frontend::world::tile::{InternalInserterInfo, World};
use crate::get_size::RAMExtractor;
use crate::get_size::RamUsage;
use crate::item::{ITEMCOUNTTYPE, Indexable};
use crate::lab::{LabViewInfo, TICKS_PER_SCIENCE};
use crate::liquid::FluidSystemState;
use crate::par_generation::{ParGenerateInfo, Timer};
use crate::rendering::{BeltSide, Corner};
use crate::saving::save_components;
#[cfg(not(all(target_arch = "wasm32", target_os = "unknown")))]
use crate::saving::save_with_fork;
use crate::statistics::{NUM_DIFFERENT_TIMESCALES, TIMESCALE_NAMES};
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
use crate::{data, mining_drill};
use eframe::egui::{
    self, Align2, Color32, ComboBox, Context, CornerRadius, Label, Layout, ProgressBar, Stroke, Ui,
    Window,
};
use egui::{Button, CollapsingHeader, Modal, RichText, ScrollArea, Sense, Slider};
use egui_extras::{Column, TableBuilder};
use egui_plot::{AxisHints, GridMark, Line, Plot, PlotPoints};
use egui_show_info::ShowInfo;
use flate2::Compression;
use flate2::write::ZlibEncoder;

#[cfg(not(all(target_arch = "wasm32", target_os = "unknown")))]
use interprocess::os::unix::unnamed_pipe::UnnamedPipeExt;
use itertools::Itertools;
use log::error;
use log::{info, trace, warn};
use parking_lot::MutexGuard;
use petgraph::dot::Dot;
use rayon::iter::{IntoParallelIterator, ParallelIterator};
use std::cmp::max;
use std::fs::File;
use std::io::{Read, Write};
use std::num::NonZero;
use std::sync::LazyLock;
use std::{
    cmp::{Ordering, min},
    mem,
    time::Duration,
};
use tilelib::types::{DrawInstance, Layer, RendererTrait};

use super::TextureAtlas;
use crate::app_state::{AuxillaryData, SimulationState};

const BELT_ANIM_SPEED: f32 = 1.0 / (BELT_LEN_PER_TILE as f32);

const ITEM_RENDER_SIZE: f32 = 1.0 / (BELT_LEN_PER_TILE as f32);

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
    inserter_item_layer: Layer,
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
            inserter_item_layer,
        } = self;
        let Self {
            tile_layer: other_tile_layer,
            entity_layer: other_entity_layer,
            entity_overlay_layer: other_entity_overlay_layer,
            item_layer: other_item_layer,
            warning_layer: other_warning_layer,
            range_layer: other_range_layer,
            inserter_item_layer: other_inserter_item_layer,
        } = other;
        tile_layer.extend(other_tile_layer);
        entity_layer.extend(other_entity_layer);
        entity_overlay_layer.extend(other_entity_overlay_layer);
        item_layer.extend(other_item_layer);
        warning_layer.extend(other_warning_layer);
        range_layer.extend(other_range_layer);
        inserter_item_layer.extend(other_inserter_item_layer);
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
        inserter_item_layer: Layer::square_tile_grid(tilesize, ar),
    }
}

struct FakeGameState<'a, ItemIdxType: IdxTrait, RecipeIdxType: IdxTrait> {
    simulation_state: MutexGuard<'a, SimulationState<ItemIdxType, RecipeIdxType>>,
    world: MutexGuard<'a, World<ItemIdxType, RecipeIdxType>>,
}

#[allow(clippy::too_many_lines)]
#[profiling::function]
pub fn render_world<ItemIdxType: IdxTrait, RecipeIdxType: IdxTrait>(
    renderer: &mut impl RendererTrait,
    simulation_state: MutexGuard<SimulationState<ItemIdxType, RecipeIdxType>>,
    world: MutexGuard<World<ItemIdxType, RecipeIdxType>>,
    aux_data: MutexGuard<AuxillaryData>,
    texture_atlas: &TextureAtlas,
    state_machine: MutexGuard<ActionStateMachine<ItemIdxType, RecipeIdxType>>,
    data_store: &DataStore<ItemIdxType, RecipeIdxType>,
) {
    let mut game_state = FakeGameState {
        simulation_state,
        world,
    };

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
        (camera_pos.0 / CHUNK_SIZE_FLOAT).floor() as i32,
        (camera_pos.1 / CHUNK_SIZE_FLOAT).floor() as i32,
    );

    if num_tiles_across_screen_horizontal > SWITCH_TO_MAPVIEW_TILES {
        let mut updates = Some(vec![]);
        mem::swap(&mut updates, &mut game_state.world.map_updates);
        mem::drop(aux_data);
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

        let FakeGameState {
            simulation_state,
            world,
        } = game_state;
        mem::drop(simulation_state);

        {
            profiling::scope!("map_view::apply_updates");
            map_view::apply_updates(
                updates
                    .into_iter()
                    .flat_map(|v| v.into_iter())
                    .map(|pos| MapViewUpdate {
                        pos,
                        color: world.get_entity_color(pos, data_store),
                    }),
                renderer,
            );
        }

        {
            profiling::scope!("Create Map Textures");
            create_map_textures_if_needed(
                &world,
                renderer,
                Position {
                    x: camera_pos.0 as i32,
                    y: camera_pos.1 as i32,
                },
                num_tiles_across_screen_horizontal as u32,
                num_tiles_across_screen_vertical as u32,
                // Only allow incremental map_view building for the last view level
                map_view::MIN_WIDTH
                    .iter()
                    .all(|&v| v < num_tiles_across_screen_horizontal as u32)
                    .then_some(Duration::from_millis(15)),
                // Some(Duration::from_millis(15)),
                // None,
                data_store,
            );
        }

        mem::drop(world);

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
                num_tiles_across_screen_horizontal as u32,
                num_tiles_across_screen_vertical as u32,
                tilesize,
                ar,
                camera_pos,
            );
        }

        renderer.draw(&entity_overlay_layer);

        return;
    }

    let x_range = -((num_tiles_across_screen_horizontal / CHUNK_SIZE_FLOAT / 2.0).ceil() as i32)
        ..=((num_tiles_across_screen_horizontal / CHUNK_SIZE_FLOAT / 2.0).ceil() as i32);
    // .into_par_iter();
    let y_range = -((num_tiles_across_screen_vertical / CHUNK_SIZE_FLOAT / 2.0).ceil() as i32)
        ..=((num_tiles_across_screen_vertical / CHUNK_SIZE_FLOAT / 2.0).ceil() as i32);

    let pos_iter = x_range
        .into_par_iter()
        .flat_map(|x| y_range.clone().into_par_iter().map(move |y| (x, y)));

    let draw_offset = (
        -camera_pos.0 + (0.5 * num_tiles_across_screen_horizontal),
        -camera_pos.1 + (0.5 * num_tiles_across_screen_vertical),
    );

    let current_tick = aux_data.current_tick as u32;

    let folded_layers = {
        profiling::scope!("Render Chunks");
        pos_iter
            .map(|(x_offs, y_offs)| {
                let chunk_draw_offs = (
                    x_offs as f32 * CHUNK_SIZE_FLOAT - camera_pos.0.rem_euclid(CHUNK_SIZE_FLOAT)
                        + (0.5 * num_tiles_across_screen_horizontal),
                    y_offs as f32 * CHUNK_SIZE_FLOAT - camera_pos.1.rem_euclid(CHUNK_SIZE_FLOAT)
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
                        inserter_item_layer
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
                                                &texture_atlas.tile_floor[(x + y) % 2],
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

                                    if let Some(ore) = game_state
                                        .world
                                        .get_ore_type_at_pos(Position {
                                            x: chunk_x * CHUNK_SIZE as i32 + x as i32,
                                            y: chunk_y * CHUNK_SIZE as i32 + y as i32,
                                        })
                                    {
                                        // TODO: Render different sprites for different rechnesses
                                        // TODO: Do not use item sprite for ore on the ground
                                        tile_layer.draw_sprite(
                                            &texture_atlas.items[ore.into_usize()],
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
                                    profiling::scope!("Single Entity", format!("{}", entity.get_type_name()));
                                    match entity {
                                        crate::frontend::world::tile::Entity::Assembler {
                                            ty,
                                            pos,
                                            info,
                                            rotation,
                                            ..
                                        } => {
                                            let size: [u16; 2] = data_store.assembler_info
                                                [usize::from(*ty)]
                                            .size(*rotation)
                                            .into();

                                            match info {
                                                AssemblerInfo::UnpoweredNoRecipe => {
                                                    texture_atlas.not_connected.draw_centered_on(
                                                        &texture_atlas.assembler[*ty as usize],
                                                        [
                                                            draw_offset.0 + pos.x as f32,
                                                            draw_offset.1 + pos.y as f32,
                                                        ],
                                                        size,
                                                        0,
                                                        warning_layer,
                                                    );

                                                    texture_atlas.assembler[*ty as usize].draw(
                                                        [
                                                            draw_offset.0 + pos.x as f32,
                                                            draw_offset.1 + pos.y as f32,
                                                        ],
                                                        size,
                                                        0,
                                                        entity_layer,
                                                    );
                                                },
                                                AssemblerInfo::Unpowered(recipe) => {
                                                    texture_atlas.not_connected.draw_centered_on(
                                                        &texture_atlas.assembler[*ty as usize],
                                                        [
                                                            draw_offset.0 + pos.x as f32,
                                                            draw_offset.1 + pos.y as f32,
                                                        ],
                                                        size,
                                                        0,
                                                        warning_layer,
                                                    );

                                                    texture_atlas.assembler[*ty as usize].draw(
                                                        [
                                                            draw_offset.0 + pos.x as f32,
                                                            draw_offset.1 + pos.y as f32,
                                                        ],
                                                        size,
                                                        0,
                                                        entity_layer,
                                                    );

                                                    let item_idx = data_store.recipe_to_items
                                                        [recipe.into_usize()]
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
                                                               draw_offset.0
                                                                    + pos.x as f32
                                                                    + (size[0] as f32
                                                                        - icon_size[0])
                                                                        / 2.0,
                                                               draw_offset.1
                                                                    + pos.y as f32
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
                                                            &texture_atlas.assembler[*ty as usize],
                                                            [
                                                               draw_offset.0
                                                                    + pos.x as f32,
                                                               draw_offset.1
                                                                    + pos.y as f32,
                                                            ],
                                                            size,
                                                            0,
                                                            warning_layer,
                                                        );
                                                    }

                                                    texture_atlas.assembler[*ty as usize].draw(
                                                        [
                                                            draw_offset.0 + pos.x as f32,
                                                            draw_offset.1 + pos.y as f32,
                                                        ],
                                                        size,
                                                        0,
                                                        entity_layer,
                                                    );
                                                },
                                                AssemblerInfo::Powered {
                                                    id, ..
                                                } => {
                                                    let grid = id.grid;

                                                    let last_power = game_state
                                                        .simulation_state
                                                        .factory
                                                        .power_grids
                                                        .power_grids[usize::from(grid)]
                                                    .last_power_mult;

                                                    if last_power == 0 {
                                                        texture_atlas.no_power.draw_centered_on(
                                                            &texture_atlas.assembler[*ty as usize],
                                                            [
                                                               draw_offset.0
                                                                    + pos.x as f32,
                                                               draw_offset.1
                                                                    + pos.y as f32,
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

                                                        ..
                                                    } = game_state
                                                        .simulation_state
                                                        .factory
                                                        .power_grids
                                                        .get_assembler_info(*id, data_store);

                                                    texture_atlas.assembler[*ty as usize].draw(
                                                        [
                                                            draw_offset.0 + pos.x as f32,
                                                            draw_offset.1 + pos.y as f32,
                                                        ],
                                                        size,
                                                        (timer_percentage
                                                            * (texture_atlas.assembler[*ty as usize]
                                                                .sprite
                                                                .texture
                                                                .number_anim_frames
                                                                as f32))
                                                            .floor()
                                                            as u32
                                                            % texture_atlas.assembler[*ty as usize]
                                                                .sprite
                                                                .texture
                                                                .number_anim_frames,
                                                        entity_layer,
                                                    );

                                                    let item_idx = data_store.recipe_to_items
                                                        [id.recipe.into_usize()]
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
                                                               draw_offset.0
                                                                    + pos.x as f32
                                                                    + (size[0] as f32
                                                                        - icon_size[0])
                                                                        / 2.0,
                                                               draw_offset.1
                                                                    + pos.y as f32
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
                                            state,
                                        } => {
                                            let mut source_dir = None;
                                            let (sprite, _corner) = {
                                                match state {
                                                    BeltState::Straight => {
                                                        (&texture_atlas.belt[*direction], None::<BeltSide>)
                                                    },
                                                    BeltState::Curved { source_dir: found } => {
                                                        source_dir = Some(*found);
                                                        if *found == direction.turn_right() {
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
                                                }
                                            };
                                            sprite.draw(
                                                [
                                                    draw_offset.0 + pos.x as f32,
                                                    draw_offset.1 + pos.y as f32,
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

                                            if state_machine.debug_view_options.highlight_sushi_belts {
                                                if game_state.simulation_state.factory.belts.get_pure_item(*id).is_none() {
                                                    texture_atlas.no_power.draw(
                                                        [
                                                            draw_offset.0 + pos.x as f32,
                                                            draw_offset.1 + pos.y as f32,
                                                        ],
                                                        [1, 1],
                                                        0,
                                                        warning_layer,
                                                    );
                                                }
                                            }

                                            let curved_source_dir = source_dir;
                                            let belt_progress: u8 = game_state
                                                .simulation_state
                                                .factory
                                                .belts
                                                .get_belt_progress(*ty);
                                            let offset_perc = belt_progress as f32 / 120.0;
                                            let last_moved: BeltLenType = game_state
                                                .simulation_state
                                                .factory
                                                .belts
                                                .get_last_moved_pos(*id);

                                            // Draw Items
                                            if let Some(curved_source_dir) = curved_source_dir {
                                                assert_ne!(curved_source_dir, *direction);
                                                let first_offs = curved_source_dir.reverse().into_offset();
                                                let second_offs = direction.into_offset();
                                                let centered_on_tile = (
                                                   draw_offset.0 + pos.x as f32 + 0.5
                                                        - 0.5 * (1.0 / f32::from(BELT_LEN_PER_TILE)),
                                                    draw_offset.1 + pos.y as f32 + 0.5
                                                        - 0.5 * (1.0 / f32::from(BELT_LEN_PER_TILE)),
                                                );
                                                render_items_straight::<ItemIdxType, RecipeIdxType>(game_state
                                                    .simulation_state
                                                    .factory
                                                    .belts
                                                    .get_item_iter_for_subrange(*id, (*belt_pos - BELT_LEN_PER_TILE / 2)..=(*belt_pos - 1)), curved_source_dir.reverse(), *belt_pos, BELT_LEN_PER_TILE / 2, (
                                                        centered_on_tile.0
                                                            - f32::from(first_offs.0)
                                                                * (0.5
                                                                    / f32::from(BELT_LEN_PER_TILE)),
                                                        centered_on_tile.1
                                                            - f32::from(first_offs.1)
                                                                * (0.5
                                                                    / f32::from(BELT_LEN_PER_TILE)),
                                                    ), offset_perc, last_moved, item_layer, texture_atlas);

                                                render_items_straight::<ItemIdxType, RecipeIdxType>(game_state
                                                        .simulation_state
                                                        .factory
                                                        .belts
                                                        .get_item_iter_for_subrange(*id,  ((*belt_pos - BELT_LEN_PER_TILE.div_ceil(2)) - BELT_LEN_PER_TILE.div_ceil(2))..=(*belt_pos - BELT_LEN_PER_TILE.div_ceil(2) - 1)), *direction, *belt_pos - BELT_LEN_PER_TILE.div_ceil(2), BELT_LEN_PER_TILE.div_ceil(2), (
                                                            centered_on_tile.0
                                                                + f32::from(second_offs.0) * 0.5 - f32::from(second_offs.0) * (0.5 / f32::from(BELT_LEN_PER_TILE)),
                                                            centered_on_tile.1
                                                                + f32::from(second_offs.1) * 0.5 - f32::from(second_offs.1) * (0.5 / f32::from(BELT_LEN_PER_TILE)),
                                                        ), offset_perc, last_moved, item_layer, texture_atlas);
                                            } else {
                                                let offs = direction.into_offset();
                                                let centered_on_tile = (
                                                   draw_offset.0 + pos.x as f32 + 0.5
                                                        - 0.5 * (1.0 / f32::from(BELT_LEN_PER_TILE)),
                                                    draw_offset.1 + pos.y as f32 + 0.5
                                                        - 0.5 * (1.0 / f32::from(BELT_LEN_PER_TILE)),
                                                );
                                                render_items_straight::<ItemIdxType, RecipeIdxType>(game_state
                                                    .simulation_state
                                                    .factory
                                                    .belts
                                                    .get_item_iter_for_subrange(*id, (*belt_pos - BELT_LEN_PER_TILE)..=(*belt_pos - 1)), *direction, *belt_pos, BELT_LEN_PER_TILE, (
                                                        centered_on_tile.0
                                                            + f32::from(offs.0)
                                                                * (0.5 - 0.5 / (f32::from(BELT_LEN_PER_TILE))),
                                                        centered_on_tile.1
                                                        + f32::from(offs.1)
                                                        * (0.5 - 0.5 / (f32::from(BELT_LEN_PER_TILE))),
                                                    ), offset_perc, last_moved, item_layer, texture_atlas);
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
                                                    draw_offset.0 + pos.x as f32,
                                                    draw_offset.1 + pos.y as f32,
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
                                                        draw_offset.0 + pos.x as f32,
                                                        draw_offset.1 + pos.y as f32,
                                                    ],
                                                    [1, 1],
                                                    0,
                                                    entity_overlay_layer,
                                                );

                                                if state_machine.debug_view_options.highlight_sushi_belts {
                                                    if game_state.simulation_state.factory.belts.get_pure_item(*id).is_none() {
                                                        texture_atlas.no_power.draw(
                                                            [
                                                                draw_offset.0 + pos.x as f32,
                                                                draw_offset.1 + pos.y as f32,
                                                            ],
                                                            [1, 1],
                                                            0,
                                                            warning_layer,
                                                        );
                                                    }
                                                }

                                            // Draw Items
                                            let belt_progress: u8 = game_state
                                                .simulation_state
                                                .factory
                                                .belts
                                                .get_belt_progress(*ty);
                                            let offset_perc = belt_progress as f32 / 120.0;
                                            let last_moved: BeltLenType = game_state
                                                .simulation_state
                                                .factory
                                                .belts
                                                .get_last_moved_pos(*id);

                                            let offs = direction.into_offset();
                                            let centered_on_tile = (
                                                draw_offset.0 + pos.x as f32 + 0.5
                                                    - 0.5 * (1.0 / f32::from(BELT_LEN_PER_TILE)),
                                                draw_offset.1 + pos.y as f32 + 0.5
                                                    - 0.5 * (1.0 / f32::from(BELT_LEN_PER_TILE)),
                                            );
                                            render_items_straight::<ItemIdxType, RecipeIdxType>(game_state
                                                .simulation_state
                                                .factory
                                                .belts
                                                .get_item_iter_for_subrange(*id, (*belt_pos - BELT_LEN_PER_TILE)..=(*belt_pos - 1)), *direction, *belt_pos, BELT_LEN_PER_TILE, (
                                                    centered_on_tile.0
                                                        + f32::from(offs.0)
                                                            * (0.5 - 0.5 / (f32::from(BELT_LEN_PER_TILE))),
                                                    centered_on_tile.1
                                                    + f32::from(offs.1)
                                                    * (0.5 - 0.5 / (f32::from(BELT_LEN_PER_TILE))),
                                                ), offset_perc, last_moved, item_layer, texture_atlas);
                                        },

                                        Entity::Inserter {
                                            pos,
                                            direction,
                                            info,
                                            user_movetime,
                                            ty,
                                            ..
                                        } => {
                                            entity_layer.draw_sprite(
                                                &texture_atlas.inserter[*direction],
                                                DrawInstance {
                                                    position: [
                                                        draw_offset.0 + pos.x as f32,
                                                        draw_offset.1 + pos.y as f32,
                                                    ],
                                                    size: [1.0, 1.0],
                                                    animation_frame: 0,
                                                },
                                            );

                                            match info {
                                    crate::frontend::world::tile::InserterInfo::NotAttached { .. } => {},
                                    crate::frontend::world::tile::InserterInfo::Attached {
                                        info,
                                    } => {
                                        let start_pos = data_store.inserter_start_pos(*ty, *pos, *direction);
                                        let end_pos = data_store.inserter_end_pos(*ty, *pos, *direction);

                                        let movetime = user_movetime.map(|v| v.into()).unwrap_or(data_store.inserter_infos[*ty as usize].swing_time_ticks).into();
                                        match info {
                                            crate::frontend::world::tile::AttachedInserter::BeltStorage { id, belt_pos } => {
                                                let Some(state) = game_state.simulation_state.factory.belts.get_inserter_info_at(*id, *belt_pos) else {
                                                    error!("Could not get rendering info for inserter!");
                                                    continue;
                                                };
                                                let hand_size = state.hand_size;

                                                // TODO: Due to clamping this does not currently hold:
                                                // assert_eq!(movetime, u16::from(state.movetime));
                                                let movetime = u16::from(state.movetime);

                                                let item =  game_state.simulation_state.factory.belts.get_inserter_item(*id, *belt_pos);

                                                let (mut position, items): (f32, ITEMCOUNTTYPE) = match state.state {
                                                    crate::inserter::InserterState::WaitingForSourceItems(count) => (0.0, count),
                                                    crate::inserter::InserterState::WaitingForSpaceInDestination(count) => (1.0, count),
                                                    crate::inserter::InserterState::FullAndMovingOut(timer) => (1.0 - (timer as f32 / movetime as f32), hand_size),
                                                    crate::inserter::InserterState::EmptyAndMovingBack(timer) => (timer as f32 / movetime as f32, 0),
                                                };

                                                // FIXME: This currently does not hold if the movetime is clamped to a u8 by the belt inserter implementation
                                                // assert!(position >= 0.0);
                                                assert!(position <= 1.0);
                                                // FIXME: This should not be needed
                                                position = position.clamp(0.0, 1.0);

                                                let start_pos = [
                                                    draw_offset.0 + start_pos.x as f32 + 0.5 - (0.5 * ITEM_RENDER_SIZE),
                                                    draw_offset.1 + start_pos.y as f32 + 0.5 - (0.5 * ITEM_RENDER_SIZE),
                                                ];

                                                let end_pos = [
                                                    draw_offset.0 + end_pos.x as f32 + 0.5 - (0.5 * ITEM_RENDER_SIZE),
                                                    draw_offset.1 + end_pos.y as f32 + 0.5 - (0.5 * ITEM_RENDER_SIZE),
                                                ];

                                                let mut item_pos = [
                                                    start_pos[0] + (end_pos[0] - start_pos[0]) * position,
                                                    start_pos[1] + (end_pos[1] - start_pos[1]) * position,
                                                ];

                                                let stack_offset = 1.0 / f32::from(BELT_LEN_PER_TILE) / 16.0;

                                                for _ in 0..items {
                                                    inserter_item_layer.draw_sprite(&texture_atlas.items[item.id.into()], DrawInstance { position: item_pos, size: [
                                                        ITEM_RENDER_SIZE,
                                                        ITEM_RENDER_SIZE,
                                                    ], animation_frame: 0 });

                                                    item_pos[0] += stack_offset;
                                                    item_pos[1] += stack_offset;
                                                }
                                            },
                                            crate::frontend::world::tile::AttachedInserter::BeltBelt { .. } => {
                                                // TODO:
                                            },
                                            crate::frontend::world::tile::AttachedInserter::StorageStorage { item, inserter } => {
                                                let hand_size = data_store.inserter_infos[*ty as usize].base_hand_size;
                                                let item =  *item;
                                                let state = game_state.simulation_state.factory.storage_storage_inserters.get_inserter(item, movetime, *inserter, current_tick);

                                                let (position, items): (f32, ITEMCOUNTTYPE) = match state.state {
                                                    crate::inserter::storage_storage_with_buckets::LargeInserterState::WaitingForSourceItems(count) => (0.0, count),
                                                    crate::inserter::storage_storage_with_buckets::LargeInserterState::WaitingForSpaceInDestination(count) => (1.0, count),
                                                    crate::inserter::storage_storage_with_buckets::LargeInserterState::FullAndMovingOut(timer) => (1.0 - (timer as f32 / u16::from(movetime) as f32), hand_size),
                                                    crate::inserter::storage_storage_with_buckets::LargeInserterState::EmptyAndMovingBack(timer) => (timer as f32 / u16::from(movetime) as f32, 0),
                                                };

                                                assert!(position >= 0.0);
                                                assert!(position <= 1.0);

                                                let start_pos = [
                                                    draw_offset.0 + start_pos.x as f32 + 0.5 - (0.5 * ITEM_RENDER_SIZE),
                                                    draw_offset.1 + start_pos.y as f32 + 0.5 - (0.5 * ITEM_RENDER_SIZE),
                                                ];

                                                let end_pos = [
                                                    draw_offset.0 + end_pos.x as f32 + 0.5 - (0.5 * ITEM_RENDER_SIZE),
                                                    draw_offset.1 + end_pos.y as f32 + 0.5 - (0.5 * ITEM_RENDER_SIZE),
                                                ];

                                                let mut item_pos = [
                                                    start_pos[0] + (end_pos[0] - start_pos[0]) * position,
                                                    start_pos[1] + (end_pos[1] - start_pos[1]) * position,
                                                ];

                                                let stack_offset = 1.0 / f32::from(BELT_LEN_PER_TILE) / 16.0;

                                                for _ in 0..items {
                                                    inserter_item_layer.draw_sprite(&texture_atlas.items[item.id.into()], DrawInstance { position: item_pos, size: [
                                                        ITEM_RENDER_SIZE,
                                                        ITEM_RENDER_SIZE,
                                                    ], animation_frame: 0 });

                                                    item_pos[0] += stack_offset;
                                                    item_pos[1] += stack_offset;
                                                }
                                            },
                                        }
                                    },
                                }
                                        },

                                        Entity::PowerPole {
                                            ty,
                                            pos,
                                        } => {
                                            // TODO:
                                            // println!("Pole at {pos:?}, with grid: {grid_id}");
                                            let size =
                                                data_store.power_pole_data[usize::from(*ty)].size;
                                            let size = [size.0, size.1];
                                            texture_atlas.power_pole.draw(
                                                [
                                                    draw_offset.0 + pos.x as f32,
                                                    draw_offset.1 + pos.y as f32,
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
                                                                draw_offset.0
                                                                    + pos.x as f32
                                                                    - power_range as f32,
                                                                draw_offset.1
                                                                    + pos.y as f32
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

                                            let belt_progress: u8 = game_state
                                                .simulation_state
                                                .factory
                                                .belts
                                                // FIXME: Splitter ty
                                                .get_belt_progress(0);
                                            let offset_perc = belt_progress as f32 / 120.0;

                                            // FIXME: We currently do not take partial movement (from slow belt speeds) into account, which leads to ugly jumping of the items on the belt
                                            for ((pos, input), output) in [left_pos, right_pos]
                                            .into_iter()
                                            .zip(inputs)
                                            .zip(outputs)
                                            {
                                                let last_moved: BeltLenType = game_state
                                                    .simulation_state
                                                    .factory
                                                    .belts
                                                    .get_last_moved_pos(input);
                                                texture_atlas.belt[*direction].draw(
                                                    [
                                                        draw_offset.0 + pos.x as f32,
                                                        draw_offset.1 + pos.y as f32,
                                                    ],
                                                    [1, 1],
                                                    0,
                                                    entity_layer,
                                                );

                                                let centered_on_tile = (
                                                    draw_offset.0 + pos.x as f32 + 0.5
                                                        - 0.5
                                                            * (1.0 / f32::from(BELT_LEN_PER_TILE)),
                                                    draw_offset.1 + pos.y as f32 + 0.5
                                                        - 0.5
                                                            * (1.0 / f32::from(BELT_LEN_PER_TILE)),
                                                );
                                                let offs = direction.into_offset();
                                                render_items_straight::<ItemIdxType, RecipeIdxType>(
                                                    game_state
                                                        .simulation_state
                                                        .factory
                                                        .belts
                                                        .get_item_iter_for_subrange(input, 0..=(SPLITTER_BELT_LEN - 1)),
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
                                                    offset_perc,
                                                    last_moved,

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
                                                    .get_item_iter_for_subrange(output, (out_belt_len - SPLITTER_BELT_LEN)..=(out_belt_len - 1));
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

                                                let last_moved: BeltLenType = game_state
                                                    .simulation_state
                                                    .factory
                                                    .belts
                                                    .get_last_moved_pos(output);
                                                render_items_straight::<ItemIdxType, RecipeIdxType>(
                                                    out_belt_iter,
                                                    *direction,
                                                    out_belt_len,
                                                    SPLITTER_BELT_LEN,
                                                    item_render_base_pos,
                                                    offset_perc,
                                                    last_moved,

                                                    item_layer,
                                                    texture_atlas,
                                                );
                                            }
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
                                                    draw_offset.0 + pos.x as f32,
                                                    draw_offset.1 + pos.y as f32,
                                                ],
                                                size,
                                                0,
                                                entity_layer,
                                            );

                                            // TODO: Implement alt mode
                                        },
                                        Entity::SolarPanel { ty, pos, .. } => {
                                            texture_atlas.solar_panel.draw([
                                                draw_offset.0 + pos.x as f32,
                                                draw_offset.1 + pos.y as f32,
                                            ], data_store.solar_panel_info
                                            [usize::from(*ty)].size, 0, entity_layer);
                                        },
                                        Entity::Accumulator { ty, pos, .. } => {
                                            texture_atlas.accumulator.draw([
                                                draw_offset.0 + pos.x as f32,
                                                draw_offset.1 + pos.y as f32,
                                            ], data_store.solar_panel_info
                                            [usize::from(*ty)].size, 0, entity_layer);
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
                                                    draw_offset.0 + pos.x as f32,
                                                    draw_offset.1 + pos.y as f32,
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
                                                            draw_offset.0 + pos.x as f32,
                                                            draw_offset.1 + pos.y as f32,
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
                                                        draw_offset.0 + pos.x as f32,
                                                        draw_offset.1 + pos.y as f32,
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
                                                    draw_offset.0 + pos.x as f32,
                                                    draw_offset.1 + pos.y as f32,
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
                                                            draw_offset.0 + pos.x as f32,
                                                            draw_offset.1 + pos.y as f32,
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
                                                        draw_offset.0 + pos.x as f32,
                                                        draw_offset.1 + pos.y as f32,
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
                                                    draw_offset.0 + pos.x as f32,
                                                    draw_offset.1 + pos.y as f32,
                                                ],
                                                size,
                                                0,
                                                entity_layer,
                                            );
                                        },

                                        Entity::MiningDrill { ty, pos, rotation, drill_id, internal_inserter } => {
                                            let size =
                                                data_store.mining_drill_info[usize::from(*ty)].size(*rotation);

                                            // TODO: Animation
                                            // TODO: Rotation
                                            texture_atlas.mining_drill.draw(
                                                [
                                                    draw_offset.0 + pos.x as f32,
                                                    draw_offset.1 + pos.y as f32,
                                                ],
                                                size,
                                                (current_tick / 2) % texture_atlas.mining_drill.sprite.texture.number_anim_frames,
                                                entity_layer,
                                            );
                                        },
                                        Entity::Roboport { ty, pos, power_grid, network, id } => todo!(),
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

    // TODO: Why is all of this hashmap based????
    // {
    //     profiling::scope!("Render Inserter States");
    //     for ((item, movetime), inserter_list) in storage_storage_inserter_render_list {
    //         let item_sprite = &texture_atlas.items[item.into_usize()];

    //         let infos = game_state
    //             .simulation_state
    //             .factory
    //             .storage_storage_inserters
    //             .get_info_batched(item, movetime, inserter_list.iter().map(|(_, _, ident)| *ident), aux_data.current_tick as u32);

    //         for (draw_pos, ty, ident) in inserter_list {
    //             let info = infos[&ident];

    //             let num_items = match info {
    //                 crate::inserter::storage_storage_with_buckets::LargeInserterState::WaitingForSourceItems(count) => count,
    //                 crate::inserter::storage_storage_with_buckets::LargeInserterState::WaitingForSpaceInDestination(count) => count,
    //                 crate::inserter::storage_storage_with_buckets::LargeInserterState::FullAndMovingOut(_) => 12,
    //                 crate::inserter::storage_storage_with_buckets::LargeInserterState::EmptyAndMovingBack(_) => 0,
    //             };

    //             if num_items > 0 {
    //                 inserter_item_layer.draw_sprite(item_sprite, DrawInstance { position: draw_pos, size: [
    //                     1.0 / f32::from(BELT_LEN_PER_TILE),
    //                     1.0 / f32::from(BELT_LEN_PER_TILE),
    //                 ], animation_frame: 0 });
    //             }
    //         }
    //     }
    // }

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
    mem::drop(aux_data);

    match &state_machine.state {
        ActionStateMachineState::CtrlCPressed | ActionStateMachineState::DelPressed => {},
        ActionStateMachineState::CopyDragInProgress { start_pos }
        | ActionStateMachineState::DeleteDragInProgress { start_pos } => {
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
                    crate::frontend::world::tile::PlaceEntityType::Assembler {
                        ty,
                        pos,
                        rotation,
                    } => {
                        let size: [u16; 2] = data_store.assembler_info[usize::from(*ty)]
                            .size(*rotation)
                            .into();
                        texture_atlas.assembler[*ty as usize].draw(
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
                        ty,
                        user_movetime,
                    } => {
                        // FIXME: Respect ty while rendering
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
                    } => {
                        // TODO:
                    },
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
                        texture_atlas.solar_panel.draw(
                            [draw_offset.0 + pos.x as f32, draw_offset.1 + pos.y as f32],
                            data_store.solar_panel_info[usize::from(*ty)].size,
                            0,
                            &mut state_machine_layer,
                        );
                    },
                    crate::frontend::world::tile::PlaceEntityType::Accumulator { pos, ty } => {
                        texture_atlas.accumulator.draw(
                            [draw_offset.0 + pos.x as f32, draw_offset.1 + pos.y as f32],
                            data_store.accumulator_info[usize::from(*ty)].size,
                            0,
                            &mut state_machine_layer,
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
                        texture_atlas.mining_drill.draw(
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
                crate::frontend::action::action_state_machine::HeldObject::OrePlacement {
                    ore,
                    amount,
                } => {
                    // TODO:
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
                inserter_item_layer,
            } = folded_layers;
            renderer.draw(&tile_layer);

            renderer.draw(&entity_layer);

            renderer.draw(&item_layer);

            renderer.draw(&entity_overlay_layer);

            renderer.draw(&range_layer);

            renderer.draw(&warning_layer);

            renderer.draw(&inserter_item_layer);
        }
        renderer.draw(&state_machine_layer);
        renderer.draw(&entity_overlay_layer);
        renderer.draw(&player_layer);
    }
}

pub enum EscapeMenuOptions {
    BackToMainMenu,
}

pub fn render_ui<
    ItemIdxType: IdxTrait + ShowInfo<RAMExtractor, RamUsage>,
    RecipeIdxType: IdxTrait + ShowInfo<RAMExtractor, RamUsage>,
>(
    ctx: &Context,
    ui: &mut Ui,
    mut state_machine: MutexGuard<ActionStateMachine<ItemIdxType, RecipeIdxType>>,
    simulation_state: MutexGuard<SimulationState<ItemIdxType, RecipeIdxType>>,
    world: MutexGuard<World<ItemIdxType, RecipeIdxType>>,
    aux_data: MutexGuard<AuxillaryData>,
    data_store: MutexGuard<DataStore<ItemIdxType, RecipeIdxType>>,
) -> Result<
    impl Iterator<Item = ActionType<ItemIdxType, RecipeIdxType>> + use<ItemIdxType, RecipeIdxType>,
    EscapeMenuOptions,
> {
    let state_machine_ref = &mut *state_machine;
    let data_store_ref = &*data_store;
    let mut actions = vec![];

    let current_tick = aux_data.current_tick;

    let tick = (current_tick % u64::from(state_machine_ref.autosave_interval)) as u32;

    #[cfg(not(all(target_arch = "wasm32", target_os = "unknown")))]
    if cfg!(target_os = "linux") {
        if tick < state_machine_ref.last_tick_seen_for_autosave {
            if state_machine_ref.current_fork_save_in_progress.is_none() {
                let recv = save_with_fork(
                    &aux_data.game_name,
                    None,
                    &*world,
                    &*simulation_state,
                    &*aux_data,
                    data_store_ref,
                );
                if let Some(recv) = recv {
                    recv.set_nonblocking(true)
                        .expect("Could not set pipe to nonblocking!");
                    state_machine_ref.current_fork_save_in_progress = Some(ForkSaveInfo {
                        recv,
                        current_state: 0,
                    });
                } else {
                    error!("Nonblocking save failed to start! Saving in blocking mode");
                    save_components(
                        &aux_data.game_name,
                        None,
                        &*world,
                        &*simulation_state,
                        &*aux_data,
                        data_store_ref,
                    );
                }
            } else {
                warn!(
                    "Save already in progress while trying to start autosave interval. If this was due to autosaves taking too long, consider increasing your autosave interval."
                );
            }
        }
    } else {
        // Ensure that the saving Window is on screen when the window freezes
        if tick >= state_machine_ref.autosave_interval - 10 || tick <= 5 {
            let progress = if tick > 1 && tick <= 5 { 1.0 } else { 0.0 };
            if tick < state_machine_ref.last_tick_seen_for_autosave {
                let _timer = Timer::new("Saving");
                save_components(
                    &aux_data.game_name,
                    None,
                    &*world,
                    &*simulation_state,
                    &*aux_data,
                    data_store_ref,
                );
            }
            Window::new("Saving...").default_open(true).show(ctx, |ui| {
                ui.add(ProgressBar::new(progress).corner_radius(0.0));
            });
        }
    }
    state_machine_ref.last_tick_seen_for_autosave = tick;

    #[cfg(all(target_arch = "wasm32", target_os = "unknown"))]
    ui.vertical_centered(|ui|{
        ui.label(
            egui::RichText::new("Detected running in a browser(WASM). Performance might be significantly degraded, and/or features might not work correctly. Support is on a best effort basis.")
                .heading()
                .color(egui::Color32::RED),
        );
        ui.label(
            egui::RichText::new("For the best experience run on native.")
                .heading()
                .color(egui::Color32::RED),
        );
    });

    #[cfg(not(all(target_arch = "wasm32", target_os = "unknown")))]
    if let Some(recv) = &mut state_machine_ref.current_fork_save_in_progress {
        const NUM_STATES: u8 = 12;

        let mut v = [0];
        if let Err(e) = recv.recv.read_exact(&mut v) {
            if e.kind() != std::io::ErrorKind::WouldBlock {
                error!("Failed to read from unnamed pipe");
                state_machine_ref.current_fork_save_in_progress = None;
            }
        } else {
            if let Some(current_state) = v.last() {
                recv.current_state = *current_state;
                if recv.current_state == NUM_STATES {
                    state_machine_ref.current_fork_save_in_progress = None;
                }
            }
        }
        if let Some(recv) = &state_machine_ref.current_fork_save_in_progress {
            Window::new("Saving...").default_open(true).show(ctx, |ui| {
                ui.add(
                    ProgressBar::new(recv.current_state as f32 / NUM_STATES as f32)
                        .corner_radius(0.0),
                );
            });
        }
    }

    if state_machine_ref.escape_menu_open {
        if let Some(escape_action) = Modal::new("Pause Window".into())
            .show(ctx, |ui| {
                ui.heading("Paused");
                if ui.button("Save").clicked() {
                    save_components(
                        &aux_data.game_name,
                        Some(&format!("{}.save", &aux_data.game_name)),
                        &*world,
                        &*simulation_state,
                        &*aux_data,
                        data_store_ref,
                    );
                }

                let enabled = {
                    #[cfg(all(target_arch = "wasm32", target_os = "unknown"))]
                    {
                        false
                    }

                    #[cfg(not(all(target_arch = "wasm32", target_os = "unknown")))]
                    {
                        cfg!(target_os = "linux")
                            && state_machine_ref.current_fork_save_in_progress.is_none()
                    }
                };

                if ui
                    .add_enabled(enabled, Button::new("Save with fork"))
                    .on_disabled_hover_text(if !cfg!(target_os = "linux") {
                        "Only available on Linux"
                    } else {
                        "Save already in progress"
                    })
                    .clicked()
                {
                    #[cfg(not(all(target_arch = "wasm32", target_os = "unknown")))]
                    {
                        let recv = save_with_fork(
                            &aux_data.game_name,
                            Some(&format!("{}.save", &aux_data.game_name)),
                            &*world,
                            &*simulation_state,
                            &*aux_data,
                            data_store_ref,
                        );
                        if let Some(recv) = recv {
                            recv.set_nonblocking(true)
                                .expect("Could not set pipe to nonblocking!");
                            state_machine_ref.current_fork_save_in_progress = Some(ForkSaveInfo {
                                recv,
                                current_state: 0,
                            });
                        }
                    }
                }
                if ui.button("Main Menu").clicked() {
                    return Some(EscapeMenuOptions::BackToMainMenu);
                }

                ui.add(
                    egui::Slider::new(&mut state_machine_ref.mouse_wheel_sensitivity, 0.01..=100.0)
                        .text("Mouse Wheel sensitivity")
                        .logarithmic(true),
                );

                let mut autosave_interval_minutes =
                    state_machine_ref.autosave_interval / 60 / (TICKS_PER_SECOND_LOGIC as u32);
                ui.add(
                    egui::Slider::new(&mut autosave_interval_minutes, 1..=100)
                        .integer()
                        .custom_formatter(|v, _range| {
                            let value: u32 = v as u32;

                            format!("{value} min")
                        })
                        .text("Autosave interval"),
                );
                state_machine_ref.autosave_interval =
                    autosave_interval_minutes * 60 * (TICKS_PER_SECOND_LOGIC as u32);

                None
            })
            .inner
        {
            match escape_action {
                EscapeMenuOptions::BackToMainMenu => return Err(escape_action),
            }
        }
    }

    let mut fake_game_state = FakeGameState {
        simulation_state: simulation_state,
        world: world,
    };
    let game_state_ref = &mut fake_game_state;

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

    // TODO: Make this conditional
    let mut open = state_machine_ref.hotbar_window_open;
    Window::new("Customize Hotbar")
        .open(&mut open)
        .show(ctx, |ui| {
            state_machine_ref.hotbar_window(ui, data_store_ref);
        });
    state_machine_ref.hotbar_window_open = open;

    Window::new("Size")
        .fixed_size(egui::vec2(1920f32, 1080f32))
        .default_open(false)
        .show(ctx, |ui| {
            // Workaround for fixed_size not working (https://users.rust-lang.org/t/egui-questions-regarding-window-size/88753/6)
            ui.set_width(ui.available_width());
            ui.set_height(ui.available_height());
            profiling::scope!("Show SimState Size");
            if ui.button("Reset Cache").clicked() {
                state_machine_ref.get_size_cache.clear();
            }
            ScrollArea::new([false, true]).show(ui, |ui| {
                ShowInfo::<RAMExtractor, RamUsage>::show_info(
                    &*game_state_ref.simulation_state,
                    &mut RAMExtractor,
                    ui,
                    "",
                    &mut state_machine_ref.get_size_cache,
                );
                ShowInfo::<RAMExtractor, RamUsage>::show_info(
                    &*game_state_ref.world,
                    &mut RAMExtractor,
                    ui,
                    "",
                    &mut state_machine_ref.get_size_cache,
                );
            });
        });

    #[cfg(not(all(target_arch = "wasm32", target_os = "unknown")))]
    Window::new("Import BP")
        .default_open(false)
        .show(ctx, |ui| {
            if ui.button("Import").clicked() {
                if let Some(path) = rfd::FileDialog::new().pick_file() {
                    if let Ok(mut file) = File::open(path) {
                        let mut bp_string = BlueprintString(String::new());
                        file.read_to_string(&mut bp_string.0)
                            .expect("Failed to read from file");

                        if let Ok(bp) = bp_string.try_into() {
                            state_machine_ref.state =
                                ActionStateMachineState::Holding(HeldObject::Blueprint(bp));
                        }
                    }
                }
            }
        });

    let tech_response = Window::new("Tech")
        .anchor(Align2::RIGHT_TOP, [0.0, 0.0])
        .title_bar(false)
        .collapsible(false)
        .resizable(false)
        .show(ctx, |ui| {
            if let Some(tech) = game_state_ref
                .simulation_state
                .tech_state
                .current_technology
            {
                let (tech_cost_units, _tech_cost_items) =
                    &data_store.technology_costs[tech.id as usize];
                let mut tech_cost_units = *tech_cost_units;

                let name: &str = if let Some(infinite) = &data_store
                    .technology_tree
                    .node_weight(tech.id.into())
                    .unwrap()
                    .repeatable
                {
                    let times_this_tech_was_finished = game_state_ref
                        .simulation_state
                        .tech_state
                        .finished_technologies
                        .get(&tech)
                        .copied()
                        .unwrap_or(0);

                    let tech_cost_increase = match infinite.scaling {
                        data::RepeatableCostScaling::Linear {
                            unit_increase_per_level,
                        } => unit_increase_per_level * u64::from(times_this_tech_was_finished),
                        data::RepeatableCostScaling::Exponential { .. } => todo!(),
                    };

                    tech_cost_units += tech_cost_increase;

                    &format!(
                        "{} {}",
                        &data_store
                            .technology_tree
                            .node_weight(tech.id.into())
                            .unwrap()
                            .name,
                        times_this_tech_was_finished + infinite.level_counter_offset
                    )
                } else {
                    &data_store
                        .technology_tree
                        .node_weight(tech.id.into())
                        .unwrap()
                        .name
                };

                ui.add(
                    Label::new(name)
                        .selectable(false)
                        .wrap_mode(egui::TextWrapMode::Truncate),
                );
                ui.add(
                    ProgressBar::new(
                        game_state_ref
                            .simulation_state
                            .tech_state
                            .in_progress_technologies
                            .get(&tech)
                            .copied()
                            .unwrap_or(0) as f32
                            / tech_cost_units as f32,
                    )
                    .corner_radius(0),
                );
            } else {
                ui.label("Press 'T' to start researching");
            }
        })
        .unwrap()
        .response;

    if tech_response.hovered() {
        const SCALE: usize = 1;

        let mouse_height = tech_response.rect.bottom();
        let width = tech_response.rect.width();
        let height = width / 2.0;

        let ticks_per_value = RELATIVE_INTERVAL_MULTS[..=SCALE]
            .iter()
            .copied()
            .product::<usize>() as f64;

        // Show the research speed graph
        Window::new("Research Graph")
            .fixed_size([width, height])
            .title_bar(false)
            .anchor(Align2::RIGHT_TOP, [0.0, mouse_height])
            .show(ctx, |ui| {
                Plot::new("research_graph")
                    .allow_zoom(false)
                    .allow_drag(false)
                    .allow_scroll(false)
                    .allow_boxed_zoom(false)
                    .include_y(0.0)
                    .set_margin_fraction([0.0, 0.05].into())
                    .x_grid_spacer(|grid_input| {
                        (0..NUM_X_AXIS_TICKS[SCALE])
                            .map(|v| GridMark {
                                value: v as f64 / (NUM_X_AXIS_TICKS[SCALE] as f64)
                                    * (NUM_SAMPLES_AT_INTERVALS[SCALE] as f64),
                                // step_size: 1.0 / (NUM_X_AXIS_TICKS[SCALE] as f64)
                                //     * (NUM_SAMPLES_AT_INTERVALS[SCALE] as f64),
                                step_size: NUM_SAMPLES_AT_INTERVALS[SCALE] as f64,
                            })
                            .chain((0..NUM_SAMPLES_AT_INTERVALS[SCALE]).map(|v| GridMark {
                                value: v as f64,
                                step_size: 1.0,
                            }))
                            .collect()
                    })
                    .y_grid_spacer(|grid_input| {
                        let max: f64 = grid_input.bounds.1;

                        let mut lower_dec =
                            10.0_f64.powf((max / ticks_per_value * 60.0 * 60.0).log10().floor());

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
                            .chain((1..8).filter_map(|v| {
                                ((v as f64) * lower_dec * 5.0 < max).then_some(GridMark {
                                    value: (v as f64) * 5.0 * lower_dec,
                                    step_size: 1.0 * lower_dec * 5.0,
                                })
                            }))
                            .chain((1..10).filter_map(|v| {
                                ((v as f64) * lower_dec / 2.0 < max).then_some(GridMark {
                                    value: (v as f64) / 2.0 * lower_dec,
                                    step_size: 1.0 * lower_dec / 2.0,
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
                        [AxisHints::new_x().formatter(|v, _| TIMESCALE_LEGEND[SCALE](v.value))]
                            .to_vec(),
                    )
                    .show(ui, |ui| {
                        let series = aux_data
                            .statistics
                            .research
                            .get_series(SCALE, data_store_ref, Some(|_| true))
                            .next()
                            .unwrap()
                            .1;
                        let pp = PlotPoints::from_ys_f32(&series.data);

                        ui.line(Line::new("Research", pp));
                    });
            });
    }

    if tech_response.clicked() {
        state_machine_ref.technology_panel_open = true;
    }

    Window::new("DEBUG USE WITH CARE")
        .default_open(false)
        .show(ctx, |ui| {
            // TODO:
            // if ui.button("⚠️DEFRAGMENT GAMESTATE").clicked() {
                // let mut new_state = game_state_ref.clone();

                // mem::swap(&mut new_state, &mut *game_state_ref);

                // mem::drop(new_state);
            // }

            if ui.button("Switch from generation assemblers to miners (inserter_transfer)").clicked() {
                for entity in game_state_ref.world.get_chunks().flat_map(|chunk| chunk.get_entities()) {
                    match entity {
                        Entity::Assembler { ty, pos: assembler_pos, modules, info, rotation } => {
                            match info {
                                AssemblerInfo::UnpoweredNoRecipe => {},
                                AssemblerInfo::Unpowered(recipe) => {
                                    if data_store.recipe_names[recipe.into_usize()].contains("generation") {
                                        todo!()
                                    }
                                },
                                AssemblerInfo::PoweredNoRecipe(position) => {},
                                AssemblerInfo::Powered { id, pole_position, weak_index } => {
                                    if data_store.recipe_names[id.recipe.into_usize()].contains("generation") {
                                        let Some((_, ore)) = data_store.recipe_to_items[id.recipe.into_usize()].iter().find(|(dir, item)| *dir == ItemRecipeDir::Out) else {
                                            unreachable!();
                                        };
                                        if data_store.item_is_fluid[ore.into_usize()] {
                                            continue;
                                        }

                                        // Find the inserter which gets items from this assembler
                                        let assembler_pos = entity.get_pos();
                                        let assembler_size = entity.get_entity_size(data_store_ref);

                                        actions.push(ActionType::Remove(assembler_pos));

                                        let area = data_store.mining_drill_info[0].size(*rotation);

                                        for x in assembler_pos.x..(assembler_pos.x + i32::from(area[0])) {
                                            for y in assembler_pos.y..(assembler_pos.y + i32::from(area[1])) {
                                                // TODO: Is this amount reasonable?
                                                actions.push(ActionType::PlaceOre{ pos: Position {
                                                    x,y
                                                }, ore: *ore,amount: 10_000_000});
                                            }
                                        }

                                        let inserter_rotation =  game_state_ref.world.get_entities_colliding_with(Position {
                                            x: assembler_pos.x - i32::from(data_store.max_inserter_search_range),
                                            y: assembler_pos.y - i32::from(data_store.max_inserter_search_range),
                                        }, (assembler_size.0 + 2 * u16::from(data_store.max_inserter_search_range), assembler_size.1 + 2 * u16::from(data_store.max_inserter_search_range)), data_store_ref).into_iter().find_map(|inserter_entity| {
                                            match inserter_entity {
                                                Entity::Inserter { ty, user_movetime, pos, direction, filter, info } => {
                                                    let start_pos = data_store.inserter_start_pos(*ty, *pos, *direction);

                                                    if start_pos.contained_in(assembler_pos, assembler_size) {
                                                        Some(*direction)
                                                    } else {
                                                        None
                                                    }
                                                },
                                                _ => { None }
                                            }
                                        }).unwrap();

                                        actions.push(ActionType::PlaceEntity(PlaceEntityInfo {
                                            entities: EntityPlaceOptions::Single(PlaceEntityType::MiningDrill {
                                                pos: assembler_pos,
                                                rotation: inserter_rotation,
                                                ty: 0,
                                            }),
                                            force: false,
                                        }));
                                    }
                                },
                            }
                        },

                        _ => {},
                    }
                }
            }

            if ui.button("Switch from generation assemblers to miners (correct)").clicked() {
                let mut drill_actions: Vec<ActionType<ItemIdxType, RecipeIdxType>> = vec![];
                for entity in game_state_ref.world.get_chunks().flat_map(|chunk| chunk.get_entities()) {
                    match entity {
                        Entity::Assembler { ty, pos, modules, info, rotation } => {
                            match info {
                                AssemblerInfo::UnpoweredNoRecipe => {},
                                AssemblerInfo::Unpowered(recipe) => {
                                    if data_store.recipe_names[recipe.into_usize()].contains("generation") {
                                        todo!()
                                    }
                                },
                                AssemblerInfo::PoweredNoRecipe(position) => {},
                                AssemblerInfo::Powered { id, pole_position, weak_index } => {
                                    if data_store.recipe_names[id.recipe.into_usize()].contains("generation") {
                                        let Some((_, ore)) = data_store.recipe_to_items[id.recipe.into_usize()].iter().find(|(dir, item)| *dir == ItemRecipeDir::Out) else {
                                            unreachable!();
                                        };
                                        if data_store.item_is_fluid[ore.into_usize()] {
                                            continue;
                                        }

                                        actions.push(ActionType::Remove(*pos));

                                        // Find the inserter which gets items from this assembler
                                        let assembler_pos = entity.get_pos();
                                        let assembler_size = entity.get_entity_size(data_store_ref);

                                        for inserter_entity in  game_state_ref.world.get_entities_colliding_with(Position {
                                            x: assembler_pos.x - i32::from(data_store.max_inserter_search_range),
                                            y: assembler_pos.y - i32::from(data_store.max_inserter_search_range),
                                        }, (assembler_size.0 + 2 * u16::from(data_store.max_inserter_search_range), assembler_size.1 + 2 * u16::from(data_store.max_inserter_search_range)), data_store_ref) {
                                            match inserter_entity {
                                                Entity::Inserter { ty, user_movetime, pos, direction, filter, info } => {
                                                    let start_pos = data_store.inserter_start_pos(*ty, *pos, *direction);

                                                    if start_pos.contained_in(assembler_pos, assembler_size) {
                                                        // This inserter is taking from the generating assembler
                                                        actions.push(ActionType::Remove(*pos));

                                                        let end_pos = data_store.inserter_end_pos(*ty, *pos, *direction);
                                                        // Find where to place a mining_drill to output into this same spot

                                                        // The mining drill will end up being rotated the same way as the inserter
                                                        let rotation = *direction;

                                                        // TODO: This is hardcoded to only work correctly with factorio base game miners
                                                        let pos = match rotation {
                                                            Dir::North => Position {
                                                                x: end_pos.x - 1,
                                                                y: end_pos.y + 1,
                                                            },
                                                            Dir::South => Position {
                                                                x: end_pos.x - 1,
                                                                y: end_pos.y - 3,
                                                            },
                                                            Dir::West => Position {
                                                                x: end_pos.x + 1,
                                                                y: end_pos.y - 1,
                                                            },
                                                            Dir::East => Position {
                                                                x: end_pos.x - 3,
                                                                y: end_pos.y - 1,
                                                            },
                                                        };

                                                        let area = data_store.mining_drill_info[0].size(rotation);

                                                        let Some((_, ore)) = data_store.recipe_to_items[id.recipe.into_usize()].iter().find(|(dir, item)| *dir == ItemRecipeDir::Out) else {
                                                            unreachable!();
                                                        };

                                                        for x in pos.x..(pos.x + i32::from(area[0])) {
                                                            for y in pos.y..(pos.y + i32::from(area[1])) {
                                                                // TODO: Is this amount reasonable?
                                                                // actions.push(ActionType::Remove(Position {
                                                                //     x,y
                                                                // }));
                                                                actions.push(ActionType::PlaceOre{ pos: Position {
                                                                    x,y
                                                                }, ore: *ore,amount: 1_000_000});
                                                            }
                                                        }
                                                        drill_actions.push(ActionType::PlaceEntity(PlaceEntityInfo {
                                                            entities: EntityPlaceOptions::Single(PlaceEntityType::MiningDrill {
                                                                pos,
                                                                rotation,
                                                                ty: 0,
                                                            }),
                                                            force: false,
                                                        }));
                                                    }
                                                },
                                                _ => {}
                                            }
                                        }
                                    }
                                },
                            }
                        },

                        _ => {},
                    }
                }

                actions.extend(drill_actions);
            }


            if ui.button("⚠️Auto Clock Inserters").clicked() {
                let inserters_without_values_set = game_state_ref.world.get_chunks().flat_map(|chunk| chunk.get_entities()).filter_map(|e| match e {
                    Entity::Inserter { ty, user_movetime, direction, pos, info, .. } => {
                        match info {
                            crate::frontend::world::tile::InserterInfo::NotAttached { .. } => None,
                            crate::frontend::world::tile::InserterInfo::Attached { info } => match info {
                                crate::frontend::world::tile::AttachedInserter::BeltStorage {id, belt_pos, .. } => {
                                    let item = game_state_ref.simulation_state.factory.belts.get_inserter_item(*id, *belt_pos);
                                    let start_pos =
                                        data_store.inserter_start_pos(*ty, *pos, *direction);
                                    let end_pos =
                                        data_store.inserter_end_pos(*ty, *pos, *direction);
                                    Some((ty, pos, start_pos, end_pos, item, true))
                                },
                                crate::frontend::world::tile::AttachedInserter::BeltBelt { .. } => None,
                                crate::frontend::world::tile::AttachedInserter::StorageStorage { item, .. } => {
                                    let start_pos =
                                    data_store.inserter_start_pos(*ty, *pos, *direction);
                                let end_pos =
                                    data_store.inserter_end_pos(*ty, *pos, *direction);
                                    Some((ty, pos, start_pos, end_pos, *item, false))
                                },
                            },
                        }
                    },
                    _ => None,
                });

                let inserter_pos_and_time = inserters_without_values_set.map(|(ty, pos, start_pos, end_pos, item, is_belt)| {
                    let mut goal_movetime = data_store.inserter_infos[*ty as usize].swing_time_ticks;

                    if let Some(e) = game_state_ref.world.get_entity_at(end_pos, data_store_ref) {
                        match e {
                            Entity::Assembler {pos: assembler_pos, ty: assembler_ty, rotation: assembler_rotation, info, .. } => {
                                match info {
                                    AssemblerInfo::UnpoweredNoRecipe => {},
                                    AssemblerInfo::Unpowered(_) => {},
                                    AssemblerInfo::PoweredNoRecipe(_) => {},
                                    AssemblerInfo::Powered { id, .. } => {
                                        let (_, _, count_in_recipe) = data_store_ref.recipe_to_items_and_amounts[id.recipe.into_usize()].iter().find(|(dir, recipe_item, _)| *dir == ItemRecipeDir::Ing && item == *recipe_item).unwrap();
                                        let time_per_recipe = data_store_ref.recipe_timers[usize_from(id.recipe.id)] as f32;

                                        let AssemblerOnclickInfo { base_speed, speed_mod, prod_mod, .. } = game_state_ref.simulation_state.factory.power_grids.get_assembler_info(*id, data_store_ref);
                                        let crafting_speed = base_speed * (1.0 + speed_mod);
                                        let mut crafts_per_tick = crafting_speed / time_per_recipe;

                                        let mut outputs = data_store.recipe_to_items_and_amounts[id.recipe.into_usize()]
                                            .iter()
                                            .filter(|(dir, _, _)| *dir == data::ItemRecipeDir::Out).map(|(_, item ,amount_in_recipe)| (*item, *amount_in_recipe, 0.0))
                                            .collect_vec();

                                        let inserters = game_state_ref.world.get_entities_colliding_with(Position {
                                            x: assembler_pos.x - i32::from(data_store_ref.max_inserter_search_range),
                                            y: assembler_pos.y - i32::from(data_store_ref.max_inserter_search_range),
                                        }, [
                                            u16::from(data_store_ref.max_inserter_search_range) * 2 + data_store_ref.assembler_info[*assembler_ty as usize].size(*assembler_rotation).0,
                                            u16::from(data_store_ref.max_inserter_search_range) * 2 + data_store_ref.assembler_info[*assembler_ty as usize].size(*assembler_rotation).1
                                        ].into(), data_store_ref).into_iter().filter(|e| matches!(e, Entity::Inserter { .. })).filter_map(|e| match e {
                                            Entity::Inserter {
                                                pos, direction, ty, user_movetime, info, ..
                                            } => {
                                                let start_pos = data_store_ref.inserter_start_pos(*ty, *pos, *direction);

                                                let item = match info {
                                                    crate::frontend::world::tile::InserterInfo::NotAttached {  } => return None,
                                                    crate::frontend::world::tile::InserterInfo::Attached { info } => {
                                                        match info {
                                                            crate::frontend::world::tile::AttachedInserter::BeltStorage { id, belt_pos } => {
                                                                let item = game_state_ref.simulation_state.factory.belts.get_inserter_item(*id, *belt_pos);

                                                                item
                                                            },
                                                            crate::frontend::world::tile::AttachedInserter::BeltBelt { item, .. } => *item,
                                                            crate::frontend::world::tile::AttachedInserter::StorageStorage { item, .. } => *item,
                                                        }
                                                    },
                                                };

                                                if start_pos.contained_in(*assembler_pos, data_store.assembler_info[*assembler_ty as usize].size(*assembler_rotation)) {
                                                    let movetime = user_movetime.map(|v| v.into()).unwrap_or(data_store_ref.inserter_infos[*ty as usize].swing_time_ticks);

                                                    let items_per_tick = f32::from(data_store_ref.inserter_infos[*ty as usize].base_hand_size) / (2.0 * f32::from(u16::from(movetime)) + 2.0);

                                                    Some((item, items_per_tick))
                                                } else {
                                                    None
                                                }
                                            }
                                            _ => unreachable!(),
                                        });

                                        for (item, amount) in inserters {
                                            outputs.iter_mut().find(|(list_item, _, _)| *list_item == item).unwrap().2 += amount;
                                        }

                                        for (item, amount_in_recipe, amount_removed_by_inserters_per_tick) in outputs {
                                            let recipes_per_tick = amount_removed_by_inserters_per_tick / (f32::from(amount_in_recipe) * (1.0 + prod_mod));


                                            if !data_store_ref.item_is_fluid[item.into_usize()] && amount_removed_by_inserters_per_tick > 0.0 {
                                                if recipes_per_tick < crafts_per_tick {
                                                    crafts_per_tick = recipes_per_tick;
                                                }

                                            }
                                        }


                                        let items_needed_per_tick = *count_in_recipe as f32 * crafts_per_tick;

                                        // FIXME: Take tech level into consideration
                                        let hand_size = data_store.inserter_infos[*ty as usize].base_hand_size as f32;

                                        let full_rotations_needed_per_tick = items_needed_per_tick / hand_size;

                                        let mut full_rotation_time_in_ticks = 1.0 / full_rotations_needed_per_tick;

                                        if is_belt {
                                            // Since an inserter can only pick up/drop off one item per tick
                                            // We want these inserters to be a bit faster to compensate
                                            full_rotation_time_in_ticks -= hand_size;
                                        }

                                        let swing_time_in_ticks = full_rotation_time_in_ticks / 2.0 - 1.0;

                                        goal_movetime = max(goal_movetime, (swing_time_in_ticks as u16 / 10 * 10).try_into().unwrap());
                                    },
                                }
                            },

                            Entity::Chest { ty: chest_ty, pos: chest_pos, item, slot_limit } => {
                                let inserters = game_state_ref.world.get_entities_colliding_with(Position {
                                    x: chest_pos.x - i32::from(data_store_ref.max_inserter_search_range),
                                    y: chest_pos.y - i32::from(data_store_ref.max_inserter_search_range),
                                }, [
                                    u16::from(data_store_ref.max_inserter_search_range) * 3, u16::from(data_store_ref.max_inserter_search_range) * 3
                                ].into(), data_store_ref).into_iter().filter(|e| matches!(e, Entity::Inserter { .. })).filter_map(|e| match e {
                                    Entity::Inserter {
                                        pos, direction, ty, user_movetime, info, ..
                                    } => {
                                        let start_pos = data_store_ref.inserter_start_pos(*ty, *pos, *direction);

                                        let item = match info {
                                            crate::frontend::world::tile::InserterInfo::NotAttached {  } => return None,
                                            crate::frontend::world::tile::InserterInfo::Attached { info } => {
                                                match info {
                                                    crate::frontend::world::tile::AttachedInserter::BeltStorage { id, belt_pos } => {
                                                        let item = game_state_ref.simulation_state.factory.belts.get_inserter_item(*id, *belt_pos);

                                                        item
                                                    },
                                                    crate::frontend::world::tile::AttachedInserter::BeltBelt { item, .. } => *item,
                                                    crate::frontend::world::tile::AttachedInserter::StorageStorage { item, .. } => *item,
                                                }
                                            },
                                        };

                                        if start_pos.contained_in(*chest_pos, data_store_ref.chest_tile_sizes[*chest_ty as usize]) {
                                            let movetime = user_movetime.map(|v| v.into()).unwrap_or(data_store_ref.inserter_infos[*ty as usize].swing_time_ticks);

                                            let items_per_tick = f32::from(data_store_ref.inserter_infos[*ty as usize].base_hand_size) / (2.0 * f32::from(u16::from(movetime)));

                                            Some((item, items_per_tick))
                                        } else {
                                            None
                                        }
                                    }
                                    _ => unreachable!(),
                                });

                                let outgoing_amount: f32 = inserters.map(|(_item, amount_per_tick)| amount_per_tick).sum();

                                let hand_size = data_store.inserter_infos[*ty as usize].base_hand_size as f32;

                                let full_rotations_needed_per_tick = outgoing_amount / hand_size;

                                let full_rotation_time_in_ticks = 1.0 / full_rotations_needed_per_tick;

                                let swing_time_in_ticks = full_rotation_time_in_ticks / 2.0 - 1.0;

                                goal_movetime = max(goal_movetime, (swing_time_in_ticks as u16 / 10 * 10).try_into().unwrap());
                            }

                            _ => {}
                        }
                    }

                    if let Some(e) = game_state_ref.world.get_entity_at(start_pos, data_store_ref) {
                        match e {
                            Entity::Assembler { pos: assembler_pos, ty: assembler_ty, rotation: assembler_rotation, info, .. } => {
                                match info {
                                    AssemblerInfo::UnpoweredNoRecipe => {},
                                    AssemblerInfo::Unpowered(_) => {},
                                    AssemblerInfo::PoweredNoRecipe(_) => {},
                                    AssemblerInfo::Powered { id, .. } => {
                                        let (_, _, count_in_recipe) = data_store_ref.recipe_to_items_and_amounts[id.recipe.into_usize()].iter().find(|(dir, recipe_item, _)| *dir == ItemRecipeDir::Out && item == *recipe_item).unwrap();
                                        let time_per_recipe = data_store_ref.recipe_timers[usize_from(id.recipe.id)] as f32;

                                        let AssemblerOnclickInfo { base_speed, speed_mod, prod_mod, .. } = game_state_ref.simulation_state.factory.power_grids.get_assembler_info(*id, data_store_ref);
                                        let crafting_speed = base_speed * (1.0 + speed_mod);
                                        let mut crafts_per_tick = crafting_speed / time_per_recipe;

                                        let mut outputs = data_store.recipe_to_items_and_amounts[id.recipe.into_usize()]
                                            .iter()
                                            .filter(|(dir, _, _)| *dir == data::ItemRecipeDir::Out).map(|(_, item ,amount_in_recipe)| (*item, *amount_in_recipe, 0.0))
                                            .collect_vec();

                                        let inserters = game_state_ref.world.get_entities_colliding_with(Position {
                                            x: assembler_pos.x - i32::from(data_store_ref.max_inserter_search_range),
                                            y: assembler_pos.y - i32::from(data_store_ref.max_inserter_search_range),
                                        }, [
                                            u16::from(data_store_ref.max_inserter_search_range) * 2 + data_store_ref.assembler_info[*assembler_ty as usize].size(*assembler_rotation).0,
                                            u16::from(data_store_ref.max_inserter_search_range) * 2 + data_store_ref.assembler_info[*assembler_ty as usize].size(*assembler_rotation).1
                                        ].into(), data_store_ref).into_iter().filter(|e| matches!(e, Entity::Inserter { .. })).filter_map(|e| match e {
                                            Entity::Inserter {
                                                pos, direction, ty, user_movetime, info, ..
                                            } => {
                                                let start_pos = data_store_ref.inserter_start_pos(*ty, *pos, *direction);

                                                let item = match info {
                                                    crate::frontend::world::tile::InserterInfo::NotAttached {  } => return None,
                                                    crate::frontend::world::tile::InserterInfo::Attached { info } => {
                                                        match info {
                                                            crate::frontend::world::tile::AttachedInserter::BeltStorage { id, belt_pos } => {
                                                                let item = game_state_ref.simulation_state.factory.belts.get_inserter_item(*id, *belt_pos);

                                                                item
                                                            },
                                                            crate::frontend::world::tile::AttachedInserter::BeltBelt { item, .. } => *item,
                                                            crate::frontend::world::tile::AttachedInserter::StorageStorage { item, .. } => *item,
                                                        }
                                                    },
                                                };

                                                if start_pos.contained_in(*assembler_pos, data_store.assembler_info[*assembler_ty as usize].size(*assembler_rotation)) {
                                                    let movetime = user_movetime.map(|v| v.into()).unwrap_or(data_store_ref.inserter_infos[*ty as usize].swing_time_ticks);

                                                    let items_per_tick = f32::from(data_store_ref.inserter_infos[*ty as usize].base_hand_size) / (2.0 * f32::from(u16::from(movetime)) + 2.0);

                                                    Some((item, items_per_tick))
                                                } else {
                                                    None
                                                }
                                            }
                                            _ => unreachable!(),
                                        });

                                        for (item, amount) in inserters {
                                            outputs.iter_mut().find(|(list_item, _, _)| *list_item == item).unwrap().2 += amount;
                                        }

                                        for (list_item, amount_in_recipe, amount_removed_by_inserters_per_tick) in outputs {
                                            let recipes_per_tick = amount_removed_by_inserters_per_tick / (f32::from(amount_in_recipe) * (1.0 + prod_mod));

                                            if list_item != item && !data_store_ref.item_is_fluid[item.into_usize()] && amount_removed_by_inserters_per_tick > 0.0 {
                                                if recipes_per_tick < crafts_per_tick {
                                                    crafts_per_tick = recipes_per_tick;
                                                }
                                            }
                                        }

                                        let items_produced_per_tick = (*count_in_recipe as f32 * (1.0 + prod_mod)) * crafts_per_tick;

                                        // FIXME: Take tech level into consideration
                                        let hand_size = data_store.inserter_infos[*ty as usize].base_hand_size as f32;

                                        let full_rotations_needed_per_tick = items_produced_per_tick / hand_size;

                                        let full_rotation_time_in_ticks = 1.0 / full_rotations_needed_per_tick;

                                        let swing_time_in_ticks = full_rotation_time_in_ticks / 2.0 - 1.0;

                                        goal_movetime = max(goal_movetime, (swing_time_in_ticks as u16 / 10 * 10).try_into().unwrap());
                                    },
                                }
                            },

                            _ => {}
                        }
                    }

                    (*pos, goal_movetime)
                });

                actions.extend(inserter_pos_and_time.map(|(pos, time)| {
                    ActionType::OverrideInserterMovetime { pos, new_movetime: Some(time.try_into().unwrap()) }
                }))
            }

            // if ui.button("⚠️Auto Clock Inserters (SLOW!)").clicked() {
            //     let mut clocking_state = crate::clocking::ClockingState::default();

            //     for assembler in game_state_ref.world.get_chunks().flat_map(|chunk| chunk.get_entities()).filter(|e| matches!(e, Entity::Assembler { ..})) {
            //         clocking_state.assume_enough_ingredients(assembler, &game_state_ref.world, &game_state_ref.simulation_state, data_store_ref);
            //     }

            //     for _ in 0..5 {
            //         for inserter in game_state_ref.world.get_chunks().flat_map(|chunk| chunk.get_entities()).filter(|e| matches!(e, Entity::Inserter { ..})) {
            //             clocking_state.get_clocking(inserter, &game_state_ref.world, &game_state_ref.simulation_state, data_store_ref);
            //         }

            //         for assembler in game_state_ref.world.get_chunks().flat_map(|chunk| chunk.get_entities()).filter(|e| matches!(e, Entity::Assembler { ..})) {
            //             clocking_state.calculate_machine_slowdown(assembler, &game_state_ref.world, &game_state_ref.simulation_state, data_store_ref);
            //         }
            //     }

            //     actions.extend(game_state_ref.world.get_chunks().flat_map(|chunk| chunk.get_entities()).filter(|e| matches!(e, Entity::Inserter { ..})).map(|inserter| {
            //         let pos = inserter.get_pos();

            //         let ticks = clocking_state.get_clocking(inserter, &game_state_ref.world, &game_state_ref.simulation_state, data_store_ref);


            //         ActionType::OverrideInserterMovetime { pos, new_movetime: ticks.map(|v| v.try_into().unwrap()) }
            //     }));
            // }

            if ui.button("Remove Clocking from all Inserters").clicked() {
                let inserters_without_values_set = game_state_ref.world.get_chunks().flat_map(|chunk| chunk.get_entities()).filter_map(|e| match e {
                    Entity::Inserter { pos, info, .. } => {
                        match info {
                            crate::frontend::world::tile::InserterInfo::NotAttached { .. } => None,
                            crate::frontend::world::tile::InserterInfo::Attached {  info, .. } => match info {
                                crate::frontend::world::tile::AttachedInserter::BeltStorage { .. } => {
                                    None
                                    // TODO: Currently BeltStorage Inserters do not support movetime changes
                                    // Some((ty, pos, start_pos, end_pos))
                                },
                                crate::frontend::world::tile::AttachedInserter::BeltBelt { .. } => None,
                                crate::frontend::world::tile::AttachedInserter::StorageStorage { .. } => {
                                    Some(pos)
                                },
                            },
                        }
                    },
                    _ => None,
                });
                actions.extend(inserters_without_values_set.copied().map(|pos| {
                    ActionType::OverrideInserterMovetime { pos, new_movetime: None }
                }))
            }
            ui.checkbox(
                &mut state_machine_ref.debug_view_options.highlight_sushi_belts,
                "Highlight Sushi Belts",
            );

            ui.label(&format!("Num Sushi Belts: {}", game_state_ref.simulation_state.factory.belts.inner.sushi_belts.len()));
            ui.label(&format!("Num Sushi Belts Holes: {}", game_state_ref.simulation_state.factory.belts.inner.sushi_belt_holes.len()));

            ui.label(&format!("Num PowerGrids: {}", game_state_ref.simulation_state.factory.power_grids.power_grids.len()));
            ui.label(&format!("Num PowerGrid Holes: {}", game_state_ref.simulation_state.factory.power_grids.power_grids.iter().filter(|pg| pg.is_placeholder).count()));
            ui.label(&String::from_iter(game_state_ref.simulation_state.factory.power_grids.power_grids.iter().map(|pg| if pg.is_placeholder {
                '_'
            } else {
                'P'
            })));


            #[cfg(feature = "debug-stat-gathering")]
            CollapsingHeader::new("Gathered Debug Stats").show(ui, |ui| {
                let num_locs_searched = NUM_BELT_LOCS_SEARCHED.load(std::sync::atomic::Ordering::Relaxed);
                let num_cache_hits = NUM_BELT_FREE_CACHE_HITS.load(std::sync::atomic::Ordering::Relaxed);
                let num_updates = NUM_BELT_UPDATES.load(std::sync::atomic::Ordering::Relaxed);
                ui.label(&format!("BeltUpdates: {}, BeltCacheHits: {}, Cache ratio: {:.2}%", num_updates, num_cache_hits,num_cache_hits as f64 / num_updates as f64 * 100.0 ));
                ui.label(&format!("BeltLocsSearched: {}, LocsPerUpdate: {:.2}", num_locs_searched, num_locs_searched as f64 / num_updates as f64 ));


                let inserter_update_calls = NUM_BELT_INSERTER_UPDATES.load(std::sync::atomic::Ordering::Relaxed);
                let inserter_update_skips = TIMES_ALL_INCOMING_EARLY_RETURN.load(std::sync::atomic::Ordering::Relaxed);
                let inserter_loads_waiting_for_item = NUM_INSERTER_LOADS_WAITING_FOR_ITEMS.load(std::sync::atomic::Ordering::Relaxed);
                let inserter_loads_waiting_for_space = NUM_INSERTER_LOADS_WAITING_FOR_SPACE.load(std::sync::atomic::Ordering::Relaxed);
                let inserter_loads_waiting_for_space_waster = NUM_INSERTER_LOADS_WAITING_FOR_SPACE_IN_GUARANTEED_FULL.load(std::sync::atomic::Ordering::Relaxed);
                let inserter_extractions = TIMES_INSERTERS_EXTRACTED.load(std::sync::atomic::Ordering::Relaxed);
                ui.label(&format!("Belts updated: {}, percentage skipped: {:.2}", inserter_update_calls, (inserter_update_skips) as f64 / inserter_update_calls as f64));
                ui.label(&format!("Total inserter loads: {}, Avg per belt: {:.2}", inserter_loads_waiting_for_item + inserter_loads_waiting_for_space, (inserter_loads_waiting_for_item + inserter_loads_waiting_for_space) as f64 / inserter_update_calls as f64));
                ui.label(&format!("Loads waiting for item: {}, {:.2}", inserter_loads_waiting_for_item, inserter_loads_waiting_for_item as f64 / (inserter_loads_waiting_for_item + inserter_loads_waiting_for_space) as f64 ));
                ui.label(&format!("Loads waiting for space: {}, {:.2}", inserter_loads_waiting_for_space, inserter_loads_waiting_for_space as f64 / (inserter_loads_waiting_for_item + inserter_loads_waiting_for_space) as f64 ));
                ui.label(&format!("Loads waiting for space, while space is guaranteed filled: {}, {:.2}", inserter_loads_waiting_for_space_waster, inserter_loads_waiting_for_space_waster as f64 / (inserter_loads_waiting_for_item + inserter_loads_waiting_for_space) as f64 ));
                ui.label(&format!("Extractions: {}, Avg ticks before extraction: {:.2}", inserter_extractions, (inserter_loads_waiting_for_item + inserter_loads_waiting_for_space) as f64 / inserter_extractions as f64 ));
            });


            if ui.button("Remove Infinity Batteries").clicked() {
                for entity in game_state_ref.world.get_chunks().flat_map(|chunk| chunk.get_entities()) {
                    match entity {
                        Entity::SolarPanel {ty, pos, ..} => {
                            if &*data_store.solar_panel_info[*ty as usize].name == "factory_game::infinity_battery" {
                                actions.push(ActionType::Remove(*pos));
                            }
                        }
                        _ => {}
                    }
                }
            }

            CollapsingHeader::new("Inserter Counts").show(ui, |ui| {
                let mut storage_storage = 0;
                let mut belt_storage = 0;
                let mut belt_belt = 0;


                for inserter_info in game_state_ref.world.get_chunks().flat_map(|chunk| chunk.get_entities()).filter_map(|e| match e {
                    Entity::Inserter {
                        info: crate::frontend::world::tile::InserterInfo::Attached {
                            info
                        },
                        ..
                    } => {
                        Some(info)
                    },

                    _ => None,
                }) {
                    match inserter_info {
                        crate::frontend::world::tile::AttachedInserter::StorageStorage { ..} => storage_storage += 1,
                        crate::frontend::world::tile::AttachedInserter::BeltStorage { ..} => belt_storage += 1,
                        crate::frontend::world::tile::AttachedInserter::BeltBelt { ..} => belt_belt += 1,
                    }
                }

                ui.label(&format!("StorageStorage: {}", storage_storage));
                ui.label(&format!("BeltStorage: {}", belt_storage));
                ui.label(&format!("BeltBelt: {}", belt_belt));

            });

            CollapsingHeader::new("Lab analysis").show(ui, |ui| {
                let mut items = vec![0u32; data_store.item_names.len()];


                for lab_info in game_state_ref.world.get_chunks().flat_map(|chunk| chunk.get_entities()).filter_map(|e| match e {
                    Entity::Lab {
                        pole_position: Some((pole_pos, _weak_idx, lab_index)),
                        ..
                    } => {
                        Some(game_state_ref.simulation_state.factory.power_grids.power_grids[game_state_ref.simulation_state.factory.power_grids.pole_pos_to_grid_id[pole_pos] as usize].lab_stores.get_lab_info(*lab_index, &data_store))
                    },

                    _ => None,
                }) {
                    if lab_info.items.iter().filter(|(_, count)| *count == 0).count() == 1 {
                        // This lab is waiting for exactly one item
                        for item in lab_info.items {
                            if item.1 == 0 {
                                items[item.0.into_usize()] += 1;
                            }
                        }
                    }
                }

                for (item, lab_count) in items.into_iter().enumerate().filter(|(_i, v)| *v > 0) {
                    ui.label(&format!("{lab_count} labs are waiting on {}", data_store.item_display_names[item]));
                }
            });

            let mut lock_view = state_machine_ref.debug_view_options.sushi_finder_view_lock.is_some();

            ui.checkbox(
                &mut lock_view,
                "Lock view on sushi belt finder",
            );

            if lock_view {
                let index = state_machine_ref.debug_view_options.sushi_finder_view_lock.get_or_insert_default();
                let mut mov = |index| {
                    let pos = game_state_ref.world.get_chunks().flat_map(|chunk| chunk.get_entities()).filter_map(|e| match e {
                        Entity::Belt {
                            pos,
                            id, ..
                        } => {
                            Some((*pos, *id))
                        },
                        Entity::Underground {
                            pos,
                            id, ..
                        } => {
                            Some((*pos, *id))
                        },

                        _ => None,
                    }).filter_map(|(pos, id)| game_state_ref.simulation_state.factory.belts.get_pure_item(id).is_none().then_some(pos)).nth(index);

                    if let Some(pos) = pos {
                        state_machine_ref.map_view_info = Some((pos.x as f32, pos.y as f32));
                    }
                };

                if ui.button("Prev").clicked() {
                    *index -= 1;

                    mov(*index);
                } else if ui.button("Next").clicked() {
                    *index += 1;

                    mov(*index);
                } else if ui.button("Skip 10").clicked() {
                    *index += 10;

                    mov(*index);
                }
            } else {
                state_machine_ref.debug_view_options.sushi_finder_view_lock = None;
            }

            ui.add(Slider::new(&mut state_machine_ref.debug_view_options.sushi_belt_len_threshhold, 0..=1_000).logarithmic(true));

            ui.label(&format!("Num Sushi Belts with length <= {}: {}", state_machine_ref.debug_view_options.sushi_belt_len_threshhold, game_state_ref.simulation_state.factory.belts.inner.sushi_belts.iter().map(|belt| belt.get_len()).filter(|len| *len as u32 <= state_machine_ref.debug_view_options.sushi_belt_len_threshhold).count()));

            ui.checkbox(
                &mut state_machine_ref.show_graph_dot_output,
                "Generate Belt Graph",
            );
            if state_machine_ref.show_graph_dot_output {
                let mut graph = format!(
                    "{:?}",
                    Dot::new(&*game_state_ref.simulation_state.factory.belts.belt_graph)
                );

                ui.text_edit_multiline(&mut graph);
            }

            if ui.button("Write out par_generate info").clicked() {
                let info = ParGenerateInfo::from_gamestate(&game_state_ref.world, &game_state_ref.simulation_state, &*aux_data, &data_store);
                let file = File::create("par_generation_info").unwrap();
                let mut encoder = ZlibEncoder::new(file, Compression::best());
                bincode::serde::encode_into_std_write(&info, &mut encoder, bincode::config::standard()).unwrap();
                encoder.finish().unwrap();
            }


            CollapsingHeader::new("Bucket cache line sizes")
                .default_open(false)
                .show(ui, |ui| {
                    let num_bytes = game_state_ref
                        .simulation_state
                        .factory
                        .storage_storage_inserters
                        .inserters
                        .iter()
                        .flat_map(|tree| tree.values())
                        .map(|(store, )| store.get_load_info())
                        .map(|(_, _, num_storage_cachelines, num_struct_cachelines)| {
                            num_storage_cachelines + num_struct_cachelines
                        })
                        .sum::<usize>()
                        * 64;
                    ui.label(&format!(
                        "Total RAM usage next tick: {}",
                        RamUsage(num_bytes)
                    ));
                    ui.label(&format!(
                        "Total RAM usage per second: {}",
                        RamUsage(num_bytes * TICKS_PER_SECOND_LOGIC as usize)
                    ));
                    TableBuilder::new(ui)
                        .columns(Column::auto(), 5)
                        .header(1.0, |mut header| {
                            header.col(|ui| {
                                ui.label("Item");
                            });
                            header.col(|ui| {
                                ui.label("num_loads");
                            });
                            header.col(|ui| {
                                ui.label("num_cacheline_reuses");
                            });
                            header.col(|ui| {
                                ui.label("num_cachelines");
                            });
                            header.col(|ui| {
                                ui.label("num_cachelines_for_inserter_struct");
                            });
                        })
                        .body(|body| {
                            body.rows(
                                1.0,
                                game_state_ref
                                    .simulation_state
                                    .factory
                                    .storage_storage_inserters
                                    .inserters
                                    .len(),
                                |mut row| {
                                    let item = row.index();

                                    row.col(|ui| {
                                        ui.label(&*data_store.item_names[item]);
                                    });

                                    let (
                                        num_loads,
                                        num_cacheline_reuses,
                                        num_cachelines,
                                        num_cachelines_for_inserter_struct,
                                    ) = game_state_ref
                                        .simulation_state
                                        .factory
                                        .storage_storage_inserters
                                        .inserters[item]
                                        .iter()
                                        .max_by_key(|v| v.1.0.get_num_inserters())
                                        .map(|store| store.1.0.get_load_info())
                                        .unwrap_or_default();

                                    row.col(|ui| {
                                        ui.label(&format!("{}", num_loads));
                                    });
                                    row.col(|ui| {
                                        ui.label(&format!("{}", num_cacheline_reuses));
                                    });
                                    row.col(|ui| {
                                        ui.label(&format!("{}", num_cachelines));
                                    });
                                    row.col(|ui| {
                                        ui.label(&format!(
                                            "{}",
                                            num_cachelines_for_inserter_struct
                                        ));
                                    });
                                },
                            )
                        })
                });

            CollapsingHeader::new("Pure Belt cache line sizes")
                .default_open(false)
                .show(ui, |ui| {
                    let [
                        num_bytes_belt_update,
                        num_bytes_inserter_struct,
                        num_bytes_inserter_update,
                    ] = game_state_ref
                        .simulation_state
                        .factory
                        .belts
                        .inner
                        .smart_belts
                        .iter()
                        .flat_map(|store| store.belts.iter())
                        .map(|belt| belt.get_update_size())
                        .map(
                            |(
                                cache_lines_from_free_index_search,
                                cache_lines_from_inserter_structs,
                                cache_lines_from_inserter_belt_lookup,
                                cache_lines_from_storage_lookup,
                                splitter_cache_lines,
                            )| {
                                [
                                    cache_lines_from_free_index_search + splitter_cache_lines,
                                    cache_lines_from_inserter_structs,
                                    cache_lines_from_inserter_structs
                                        + cache_lines_from_inserter_belt_lookup
                                        + cache_lines_from_storage_lookup,
                                ]
                            },
                        )
                        .reduce(|acc, v| [acc[0] + v[0], acc[1] + v[1], acc[2] + v[2]])
                        .unwrap_or([0, 0, 0])
                        .map(|v| v * 64);

                    let num_bytes_inner = num_bytes_belt_update + num_bytes_inserter_update;
                    let num_bytes_outer = game_state_ref
                        .simulation_state
                        .factory
                        .belts
                        .inner
                        .smart_belts
                        .iter()
                        .flat_map(|store| store.belts.iter())
                        .count()
                        * std::mem::size_of::<SmartBelt<ItemIdxType>>();

                    let num_bytes = num_bytes_inner + num_bytes_outer;
                    ui.label(&format!(
                        "Total RAM usage (belt update) next tick: {}",
                        RamUsage(num_bytes_belt_update)
                    ));
                    ui.label(&format!(
                        "Total RAM usage (belt update and Belt struct load) next tick: {}",
                        RamUsage(num_bytes_belt_update + num_bytes_outer)
                    ));
                    ui.label(&format!(
                        "Total RAM usage (inserter struct) next tick: {}",
                        RamUsage(num_bytes_inserter_struct)
                    ));
                    ui.label(&format!(
                        "Total RAM usage (full inserter update) next tick: {}",
                        RamUsage(num_bytes_inserter_update)
                    ));
                    ui.label(&format!(
                        "Total RAM usage next tick: {}",
                        RamUsage(num_bytes)
                    ));
                    ui.label(&format!(
                        "Total RAM usage per second: {}",
                        RamUsage(num_bytes * TICKS_PER_SECOND_LOGIC as usize)
                    ));
                });

            CollapsingHeader::new("BeltStore")
                .default_open(false)
                .show(ui, |ui| {
                    ui.code_editor(&mut format!(
                        "{:?}",
                        game_state_ref.simulation_state.factory.belts
                    ));
                });

                ui.label(&format!("Number of generated chunks: {}", game_state_ref.world.chunks.num_chunks));
        });

    Window::new("UPS").default_open(true).show(ctx, |ui| {
        let points = &aux_data.update_round_trip_times.get_data_points(0)[0..600];
        ui.label(format!(
            "{:.1} UPS",
            1.0 / (points.iter().map(|v| v.dur).sum::<Duration>() / points.len() as u32)
                .as_secs_f32()
        ));
        let points = &aux_data.update_times.get_data_points(0)[0..600];
        ui.label(format!(
            "{:.1} mspt",
            (points.iter().map(|v| v.dur).sum::<Duration>() / points.len() as u32).as_secs_f32()
                * 1000.0
        ));
    });

    Window::new("Editor").default_open(false).show(ctx, |ui| {
        ui.horizontal(|ui| {
            ui.label("Place Ore");
            let (mut ore, mut amount) = match state_machine_ref.state {
                ActionStateMachineState::Holding(HeldObject::OrePlacement { ore, amount }) => {
                    (Some(ore), Some(amount))
                },

                _ => (None, None),
            };
            ComboBox::new("OrePlacementDropdown", "Select ore to place")
                .selected_text(match ore {
                    Some(ore) => &data_store.item_display_names[ore.into_usize()],
                    None => "No Ore Selected",
                })
                .show_ui(ui, |ui| {
                    // TOOD: Filter only ores
                    for ore_opt in 0..data_store.item_names.len() {
                        ui.selectable_value(
                            &mut ore,
                            Some(Item {
                                id: ore_opt.try_into().unwrap(),
                            }),
                            &data_store.item_display_names[ore_opt],
                        );
                    }
                });

            if ore.is_some() && amount.is_none() {
                amount = Some(1_000_000);
            }

            if let Some(amount) = amount.as_mut() {
                ui.add(Slider::new(amount, 0..=100_000_000).logarithmic(true));
            }

            if let Some(ore) = ore {
                state_machine_ref.state =
                    ActionStateMachineState::Holding(HeldObject::OrePlacement {
                        ore,
                        amount: amount.unwrap(),
                    });
            }
        });
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
            let s: BlueprintString = bp.cloned().unwrap().into();
            ctx.copy_text(s.0);
        }

        if ui
            .add_enabled(bp.is_some(), Button::new("Write Blueprint String to file"))
            .clicked()
        {
            let s: BlueprintString = bp.cloned().unwrap().into();
            let mut file = File::create("saved.bp").unwrap();
            file.write(s.0.as_bytes()).unwrap();
        }

        if ui
            .add_enabled(
                bp.is_some(),
                Button::new("Write Blueprint binary data to file"),
            )
            .clicked()
        {
            let v: Vec<u8> = bitcode::serialize(bp.unwrap()).unwrap();
            let file = File::create("saved_binary.bp").unwrap();
            let mut encoder = ZlibEncoder::new(file, Compression::best());
            encoder.write_all(&v).unwrap();
            encoder.finish().unwrap();
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
        ActionStateMachineState::CopyDragInProgress { start_pos: _ } => {
            ctx.set_cursor_icon(egui::CursorIcon::Copy);
        },

        ActionStateMachineState::DelPressed => {
            ctx.set_cursor_icon(egui::CursorIcon::Copy);
        },
        ActionStateMachineState::DeleteDragInProgress { start_pos: _ } => {
            ctx.set_cursor_icon(egui::CursorIcon::NotAllowed);
        },

        crate::frontend::action::action_state_machine::ActionStateMachineState::Idle => {},
        crate::frontend::action::action_state_machine::ActionStateMachineState::Holding(_) => {},
        crate::frontend::action::action_state_machine::ActionStateMachineState::Viewing(
            position,
        ) => {
            let mut viewing = true;
            Window::new("Viewing").collapsible(false).open(&mut viewing).show(ctx, |ui| {
                let chunk = game_state_ref
                .world
                .get_chunk_for_tile(*position)
                .expect("Cannot find chunk for viewing");
            let entity = chunk.get_entity_at(*position, data_store_ref);

            if let Some(entity) = entity {
                match entity {
                    crate::frontend::world::tile::Entity::Assembler { ty, pos, info, modules, rotation } => {
                        // FIXME: Rotate sprite
                        let mut goal_recipe: Option<Recipe<RecipeIdxType>> = match info {
                            AssemblerInfo::UnpoweredNoRecipe => None,
                            AssemblerInfo::Unpowered(recipe) => Some(*recipe),
                            AssemblerInfo::PoweredNoRecipe(_) => None,
                            AssemblerInfo::Powered { id, .. } => Some(id.recipe),
                        };

                        // TODO: Once a dropdown with a low number of items is shown, all future dropdowns get cropped to that count :/
                        ComboBox::new(format!("Recipe list {}", *ty), "Recipes").selected_text(goal_recipe.map(|recipe| data_store_ref.recipe_display_names[usize_from(recipe.id)].as_str()).unwrap_or("Choose a recipe!")).show_ui(ui, |ui| {
                            data_store_ref.recipe_display_names.iter().enumerate().filter(|(i, recipe_name)| {
                                    (aux_data.settings.show_unresearched_recipes || game_state_ref.simulation_state.tech_state.get_active_recipes()[*i]) && data_store_ref.recipe_allowed_assembling_machines[*i].contains(ty)
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

                                    #[cfg(feature = "assembler-craft-tracking")]
                                    times_craft_finished
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
                                let modules = &game_state_ref.world.module_slot_dedup_table[*modules as usize];
                                TableBuilder::new(ui).id_salt("Module Slots").columns(Column::auto(), modules.len()).body(|mut body| {
                                    body.row(1.0, |mut row| {
                                        for module in modules.iter() {
                                            row.col(|ui| {
                                                if let Some(module_id) = module {
                                                    ui.label(&data_store_ref.module_info[*module_id as usize].display_name);
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
                                            let (_, _, count_in_recipe) = data_store_ref.recipe_to_items_and_amounts[id.recipe.into_usize()].iter().find(|(dir, recipe_item, _)| *dir == ItemRecipeDir::Ing && *item == *recipe_item).unwrap();
                                            row.col(|ui| {
                                                ui.add(egui::Label::new(&data_store_ref.item_display_names[usize_from(item.id)]).wrap_mode(egui::TextWrapMode::Extend));
                                                ui.add(egui::Label::new(format!("{}", *count)).wrap_mode(egui::TextWrapMode::Extend));
                                                ui.add(egui::Label::new(format!("{}/s", (*count_in_recipe as f32) / (time_per_craft / 60.0))).wrap_mode(egui::TextWrapMode::Extend));
                                            });
                                        }

                                        for (item, count) in outputs.iter() {
                                            let (_, _, count_in_recipe) = data_store_ref.recipe_to_items_and_amounts[id.recipe.into_usize()].iter().find(|(dir, recipe_item, _)| *dir == ItemRecipeDir::Out && *item == *recipe_item).unwrap();
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


                                #[cfg(feature = "assembler-craft-tracking")]
                                ui.label(format!("Crafts finished: {}", times_craft_finished));

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
                            let current_charge: Joule = pg.main_accumulator_charge.iter().copied().sum();

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
                            body.rows(1.0, pg.num_assemblers_of_type.len() + pg.num_solar_panels_of_type.len() + pg.num_beacons_of_type.len()+ pg.num_labs_of_type.len(), |mut row| {
                                let i = row.index();

                                if i < pg.num_assemblers_of_type.len() {
                                    row.col(|ui| {
                                        ui.add(Label::new(&data_store_ref.assembler_info[i].display_name).extend());

                                    });
                                    row.col(|ui| {ui.add(Label::new(format!("{}", pg.num_assemblers_of_type[i])).extend());});
                                } else if  i < pg.num_assemblers_of_type.len() +  pg.num_solar_panels_of_type.len()  {
                                    let i = i - pg.num_assemblers_of_type.len();
                                    row.col(|ui| {
                                        ui.add(Label::new(&data_store_ref.solar_panel_info[i].display_name).extend());

                                    });
                                    row.col(|ui| {ui.add(Label::new(format!("{}", pg.num_solar_panels_of_type[i])).extend());});
                                } else if i < pg.num_assemblers_of_type.len() +  pg.num_solar_panels_of_type.len() + pg.num_beacons_of_type.len() {
                                    let i = i - (pg.num_assemblers_of_type.len() +  pg.num_solar_panels_of_type.len());
                                    row.col(|ui| {
                                        ui.add(Label::new(&data_store_ref.beacon_info[i].display_name).extend());

                                    });
                                    row.col(|ui| {ui.add(Label::new(format!("{}", pg.num_beacons_of_type[i])).extend());});
                                } else {
                                    let i = i - (pg.num_assemblers_of_type.len() +  pg.num_solar_panels_of_type.len() + pg.num_beacons_of_type.len());
                                    row.col(|ui| {
                                        ui.add(Label::new(&data_store_ref.lab_info[i].display_name).extend());

                                    });
                                    row.col(|ui| {ui.add(Label::new(format!("{}", pg.num_labs_of_type[i])).extend());});

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
                        ty,
                        user_movetime,

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

                                    if let Some(info) = game_state_ref.simulation_state.factory.belts.get_inserter_info_at(*id, *belt_pos) {
                                        ui.label(format!("storage: {:?}", info));
                                    } else {
                                        error!("No inserter at pos indicated in entity!");

                                    }


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
                            let mut movetime = user_movetime.map(|v| v.into()).unwrap_or(data_store.inserter_infos[*ty as usize].swing_time_ticks);

                            ui.add(egui::Slider::new(&mut movetime, (data_store.inserter_infos[*ty as usize].swing_time_ticks)..=NonZero::<u16>::MAX).text("Ticks per half swing"));

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

                        let storage = game_state_ref.simulation_state.factory.belts.get_splitter_belt_content(*id);
                        let [inputs, outputs] = storage;

                        ui.label(format!("Inputs: {:?}", inputs));
                        ui.label(format!("Outputs: {:?}", outputs));
                    },
                    Entity::Chest {
                        ty,
                        item,
                        ..
                    } => {
                        let num_slots = data_store_ref.chest_num_slots[*ty as usize];
                        let (current_items, stack_size) = if let Some((item, index)) = item {
                            ui.label(&data_store_ref.item_display_names[usize_from(item.id)]);
                            ui.label(format!("{}", *index));

                            let stack_size: u16 = data_store_ref.item_stack_sizes[usize_from(item.id)] as u16;

                            let (current_items, _max_items) = game_state_ref.simulation_state.factory.chests.stores[usize_from(item.id)].get_chest(*index);

                            (current_items, stack_size)
                        } else {
                            (0, 0)
                        };
                        TableBuilder::new(ui).columns(Column::auto().resizable(false), 10).body(|body| {
                            body.rows(5.0, (num_slots / 10) as usize + (num_slots % 10 > 0) as usize, |mut row| {
                                let idx = row.index();
                                for col_idx in 0..10 {
                                    let slot_id = idx * 10 + col_idx;
                                    if slot_id >= num_slots as usize {
                                        break;
                                    }
                                    row.col(|ui| {
                                        let this_slots_stack_count = min(current_items.saturating_sub(slot_id as ChestSize * stack_size as ChestSize), stack_size as ChestSize);

                                        let clicked = ui.add(Label::new(format!("{}", this_slots_stack_count)).extend()).clicked();
                                        let mut shift = false;
                                        ctx.input(|input| {shift = input.modifiers.shift; });

                                        if shift && clicked {
                                            warn!("TODO: Move the items into the players inventory if there is space");
                                        }
                                    });
                                }
                            });
                        });
                    },
                    Entity::Lab { pole_position, ty, modules, .. } => {
                        ui.label(&data_store.lab_info[*ty as usize].display_name);
                        match pole_position {
                            Some((pole_pos, _weak_index, lab_index)) => {
                                let ticks_per_science: f32 = TICKS_PER_SCIENCE as f32;

                                let LabViewInfo {
                                    items,
                                    timer_percentage,
                                    prod_timer_percentage,
                                    base_speed,
                                    speed_mod,
                                    prod_mod,
                                    power_consumption_mod,
                                    base_power_consumption,
                                } = game_state_ref.simulation_state.factory.power_grids.power_grids[game_state_ref.simulation_state.factory.power_grids.pole_pos_to_grid_id[pole_pos] as usize].lab_stores.get_lab_info(*lab_index, &data_store);

                                let main_pb = ProgressBar::new(timer_percentage).show_percentage().corner_radius(CornerRadius::ZERO);
                                ui.add(main_pb);
                                let prod_pb = ProgressBar::new(prod_timer_percentage).fill(Color32::ORANGE).show_percentage().corner_radius(CornerRadius::ZERO);
                                ui.add(prod_pb);

                                // Render module slots
                                let modules = &game_state_ref.world.module_slot_dedup_table[*modules as usize];
                                TableBuilder::new(ui).id_salt("Module Slots").columns(Column::auto(), modules.len()).body(|mut body| {
                                    body.row(1.0, |mut row| {
                                        for module in modules.iter() {
                                            row.col(|ui| {
                                                if let Some(module_id) = module {
                                                    ui.label(&data_store_ref.module_info[*module_id as usize].display_name);
                                                } else {
                                                    ui.label("Empty Module Slot");
                                                }
                                            });
                                        }
                                    });
                                });

                                let crafting_speed = base_speed * (1.0 + speed_mod);

                                TableBuilder::new(ui).columns(Column::auto().resizable(false), items.len()).body(|mut body| {
                                    body.row(5.0, |mut row| {
                                        for (item, count) in items.iter() {
                                            row.col(|ui| {
                                                ui.add(egui::Label::new(&data_store_ref.item_display_names[usize_from(item.id)]).wrap_mode(egui::TextWrapMode::Extend));
                                                ui.add(egui::Label::new(format!("{}", *count)).wrap_mode(egui::TextWrapMode::Extend));
                                                ui.add(egui::Label::new(format!("{}/s", (ticks_per_science / 60.0))).wrap_mode(egui::TextWrapMode::Extend));
                                            });
                                        }

                                    });
                                });



                                ui.label(format!("Crafting Speed: {:.2}({:+.0}%)", crafting_speed, speed_mod * 100.0));
                                ui.label(format!("Productivity: {:.1}%", prod_mod * 100.0));
                                ui.label(format!("Max Consumption: {}({:+.0}%)", Watt((base_power_consumption.0 as f64 * (1.0 + power_consumption_mod as f64)) as u64), power_consumption_mod * 100.0));
                            },
                            None => {
                                // TODO:
                            },
                        }
                    },
                    Entity::SolarPanel {  ty, .. } => {
                        ui.label(format!("{}", &data_store.solar_panel_info[*ty as usize].display_name));

                        let current = data_store.solar_panel_info[*ty as usize].power_output.get_at_time(aux_data.current_tick as u32);
                        let max = data_store.solar_panel_info[*ty as usize].power_output.max();

                        let perc = current.0 as f32 / max.0 as f32;

                        ui.add(ProgressBar::new(perc).text(format!("{}/{}", current, max)).corner_radius(0.0));
                    },
                    Entity::Accumulator {  ty, pole_position, .. } => {
                        ui.label(format!("{}", &data_store.accumulator_info[*ty as usize].display_name));

                        let max_charge = data_store.accumulator_info[*ty as usize].max_charge;
                        let charge = if let Some(pole_pos) = pole_position {
                            let grid = game_state_ref.simulation_state.factory.power_grids.pole_pos_to_grid_id[&pole_pos.0];
                            let grid = &game_state_ref.simulation_state.factory.power_grids.power_grids[grid as usize];
                            let charge = grid.main_accumulator_charge[*ty as usize] / grid.main_accumulator_count[*ty as usize];

                            charge
                        } else {
                            // FIXME: A unconnected accumulator can still have charge
                            Joule(0)
                        };

                        let perc = charge.0 as f32 / max_charge.0 as f32;

                        ui.add(ProgressBar::new(perc).text(format!("{}/{}", charge, max_charge)).corner_radius(0.0));
                    },
                    Entity::Beacon { ty, modules, .. } => {
                        // TODO
                    },
                    Entity::FluidTank { ty, pos, rotation } => {
                        let id = game_state_ref.simulation_state.factory.fluid_store.fluid_box_pos_to_network_id[pos];

                        ui.label(format!("{:?}", id));
                        if let Some(fluid) = id.fluid {
                            ui.label(&data_store.item_display_names[fluid.into_usize()]);
                            let FluidSystemState::HasFluid { fluid } = game_state_ref.simulation_state.factory.fluid_store.fluid_systems_with_fluid[fluid.into_usize()][id.index].as_ref().unwrap().state else {
                                unreachable!();
                            };

                            // TODO:
                            // let (units, _max_units) = game_state_ref.simulation_state.factory.chests.stores[fluid.into_usize()].get_chest(chest_id);

                            // ui.label(format!("{} units", units));
                        } else {
                            ui.label("Empty");
                        }
                    }
                    Entity::MiningDrill { ty, pos, rotation, drill_id, internal_inserter } => {
                        // TODO:
                        ui.label(&*data_store.mining_drill_info[*ty as usize].display_name);

                        let mining_drill::MiningDrillInfo {
                            timer: timer_percentage,
                            prod_timer: prod_timer_percentage,
                            remaining_ore
                        } = game_state_ref.simulation_state.factory.ore_store.get_info(*drill_id);

                        let main_pb = ProgressBar::new(timer_percentage).show_percentage().corner_radius(CornerRadius::ZERO);
                                ui.add(main_pb);
                                let prod_pb = ProgressBar::new(prod_timer_percentage).fill(Color32::ORANGE).show_percentage().corner_radius(CornerRadius::ZERO);
                                ui.add(prod_pb);

                        for (ore, amount) in remaining_ore {
                            ui.label(&format!("{}: {}", data_store.item_display_names[ore.into_usize()], amount));
                        }

                        if let InternalInserterInfo::Attached {info} = internal_inserter {
                            ui.label(&format!("{:?}", info));
                        } else {
                            ui.label("Not attached");
                        }
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
            Window::new("Deconstructing")
                .collapsible(false)
                .show(ui.ctx(), |ui| {
                    let pb = ProgressBar::new((*timer as f32) / 100.0);
                    ui.add(pb);
                });
        },
    }

    // egui::Area::new("Hotbar".into())
    //     .anchor(Align2::CENTER_BOTTOM, (0.0, 0.0))
    //     .show(ui.ctx(), |ui| {
    //         egui_extras::TableBuilder::new(ui)
    //             .columns(Column::auto().resizable(false), 10)
    //             .body(|mut body| {
    //                 body.row(30.0, |mut row| {
    //                     for i in 0..10 {
    //                         if row
    //                             .col(|ui| {
    //                                 let button_response = ui.button(format!("{i}"));

    //                                 if button_response.hovered() {
    //                                     dbg!(i);
    //                                 }
    //                             })
    //                             .1
    //                             .hovered()
    //                         {
    //                             dbg!(i);
    //                         };
    //                     }
    //                 });
    //             });
    //     });

    Window::new("Technology")
        .collapsible(false)
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
        .collapsible(false)
        .open(&mut state_machine_ref.statistics_panel_open)
        .show(ctx, |ui| {
            let time_scale = match &mut state_machine_ref.statistics_panel {
                StatisticsPanel::Items(timescale) => timescale,
                StatisticsPanel::Fluids(timescale) => timescale,
            };
            ui.with_layout(Layout::left_to_right(egui::Align::Min), |ui| {
                for i in 0..NUM_DIFFERENT_TIMESCALES {
                    ui.radio_value(time_scale, i, TIMESCALE_NAMES[i]);
                }
                ui.radio_value(time_scale, usize::MAX, "Total");
            });
            let time_scale = *time_scale;
            ui.with_layout(Layout::left_to_right(egui::Align::Min), |ui| {
                ui.radio_value(
                    &mut state_machine_ref.statistics_panel,
                    StatisticsPanel::Items(time_scale),
                    "Items",
                );
                ui.radio_value(
                    &mut state_machine_ref.statistics_panel,
                    StatisticsPanel::Fluids(time_scale),
                    "Fluids",
                );
            });
            ui.with_layout(Layout::left_to_right(egui::Align::Min), |ui| {
                ui.checkbox(
                    &mut state_machine_ref.statistics_panel_locked_scale,
                    "Share graph scale",
                );
            });

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
                        let row_height = ui_production.spacing().interact_size.y;

                        let (ticks_total, sum_list_prod, sum_list_cons) = if scale == usize::MAX {
                            (
                                aux_data.statistics.consumption.num_samples_pushed,
                                aux_data
                                    .statistics
                                    .production
                                    .total
                                    .as_ref()
                                    .unwrap()
                                    .items_produced
                                    .iter()
                                    .copied()
                                    .map(|v| v as f32)
                                    .enumerate()
                                    .zip(data_store.item_display_names.iter())
                                    .filter(|((_item_id, count), _name)| *count > 0.0)
                                    .map(|((a, b), c)| (c.to_string(), a, b))
                                    .filter(|(_, id, _)| {
                                        take_fluids == data_store.item_is_fluid[*id]
                                    })
                                    .filter(|(_, id, _)| state_machine_ref.production_filters[*id])
                                    .sorted_unstable_by(|a, b| {
                                        if a.2 < b.2 {
                                            Ordering::Greater
                                        } else {
                                            Ordering::Less
                                        }
                                    })
                                    .collect(),
                                aux_data
                                    .statistics
                                    .consumption
                                    .total
                                    .as_ref()
                                    .unwrap()
                                    .items_consumed
                                    .iter()
                                    .copied()
                                    .map(|v| v as f32)
                                    .enumerate()
                                    .zip(data_store.item_display_names.iter())
                                    .filter(|((_item_id, count), _name)| *count > 0.0)
                                    .map(|((a, b), c)| (c.to_string(), a, b))
                                    .filter(|(_, id, _)| {
                                        take_fluids == data_store.item_is_fluid[*id]
                                    })
                                    .filter(|(_, id, _)| state_machine_ref.consumption_filters[*id])
                                    .sorted_unstable_by(|a, b| {
                                        if a.2 < b.2 {
                                            Ordering::Greater
                                        } else {
                                            Ordering::Less
                                        }
                                    })
                                    .collect(),
                            )
                        } else {
                            let prod_points: Vec<(String, usize, f32, PlotPoints)> = aux_data
                                .statistics
                                .production
                                .get_series(
                                    scale,
                                    data_store_ref,
                                    Some(|item: Item<ItemIdxType>| {
                                        data_store_ref.item_is_fluid[usize_from(item.id)]
                                            == take_fluids
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

                            let cons_points: Vec<(String, usize, f32, PlotPoints)> = aux_data
                                .statistics
                                .consumption
                                .get_series(
                                    scale,
                                    data_store_ref,
                                    Some(|item: Item<ItemIdxType>| {
                                        data_store_ref.item_is_fluid[usize_from(item.id)]
                                            == take_fluids
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

                            let max_prod_sample = prod_points
                                .iter()
                                .filter(|(_, id, _, _)| state_machine_ref.production_filters[*id])
                                .flat_map(|v| v.3.points())
                                .map(|point| point.y)
                                .max_by(|a, b| {
                                    if a < b {
                                        Ordering::Less
                                    } else {
                                        Ordering::Greater
                                    }
                                })
                                .unwrap_or(0.0);

                            let max_cons_sample = cons_points
                                .iter()
                                .filter(|(_, id, _, _)| state_machine_ref.consumption_filters[*id])
                                .flat_map(|v| v.3.points())
                                .map(|point| point.y)
                                .max_by(|a, b| {
                                    if a < b {
                                        Ordering::Less
                                    } else {
                                        Ordering::Greater
                                    }
                                })
                                .unwrap_or(0.0);

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

                                    let mut lower_dec = 10.0_f64.powf(
                                        (max / ticks_per_value * 60.0 * 60.0).log10().floor(),
                                    );

                                    if lower_dec < 1.0 {
                                        lower_dec = 1.0;
                                    }

                                    lower_dec = lower_dec * ticks_per_value / 60.0 / 60.0;

                                    (0..40)
                                        .filter_map(|v| {
                                            ((v as f64) / 4.0 * lower_dec < max).then_some(
                                                GridMark {
                                                    value: (v as f64) / 4.0 * lower_dec,
                                                    step_size: lower_dec / 4.0,
                                                },
                                            )
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
                                        format!(
                                            "{:.1}/min",
                                            v.value / ticks_per_value * 60.0 * 60.0
                                        )
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
                                .include_y(if state_machine_ref.statistics_panel_locked_scale {
                                    max_cons_sample
                                } else {
                                    0.0
                                })
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
                                (aux_data
                                    .statistics
                                    .production
                                    .num_samples_pushed
                                    .checked_sub(ticks_per_sample)
                                    .map(|v| v + 1)
                                    .unwrap_or(0))
                                .next_multiple_of(ticks_per_sample),
                            );

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

                                    let mut lower_dec = 10.0_f64.powf(
                                        (max / ticks_per_value * 60.0 * 60.0).log10().floor(),
                                    );

                                    if lower_dec < 1.0 {
                                        lower_dec = 1.0;
                                    }

                                    lower_dec = lower_dec * ticks_per_value / 60.0 / 60.0;

                                    (0..40)
                                        .filter_map(|v| {
                                            ((v as f64) / 4.0 * lower_dec < max).then_some(
                                                GridMark {
                                                    value: (v as f64) / 4.0 * lower_dec,
                                                    step_size: lower_dec / 4.0,
                                                },
                                            )
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
                                        format!(
                                            "{:.1}/min",
                                            v.value / ticks_per_value * 60.0 * 60.0
                                        )
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
                                .include_y(if state_machine_ref.statistics_panel_locked_scale {
                                    max_prod_sample
                                } else {
                                    0.0
                                })
                                .show(ui_consumption, |ui| {
                                    for line in lines {
                                        ui.line(line);
                                    }
                                });

                            (ticks_total, sum_list_prod, sum_list_cons)
                        };

                        let max_prod = sum_list_prod.get(0).map(|v| v.2).unwrap_or(0.0);
                        let max_cons = sum_list_cons.get(0).map(|v| v.2).unwrap_or(0.0);

                        ScrollArea::vertical().id_salt("Prod List Scroll").show(
                            ui_production,
                            |ui| {
                                TableBuilder::new(ui)
                                    .id_salt("Production List")
                                    .sense(Sense::click())
                                    .column(Column::auto())
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
                                                ui.with_layout(
                                                    Layout::right_to_left(egui::Align::Center),
                                                    |ui| {
                                                        ui.add(
                                                            Label::new(format!(
                                                                "{:.0}",
                                                                sum_list_prod[idx].2
                                                            ))
                                                            .extend(),
                                                        );
                                                    },
                                                );
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
                                                                "{:.0}/min",
                                                                sum_list_prod[idx].2
                                                                    / ticks_total as f32
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

                        ScrollArea::vertical().id_salt("Cons List Scroll").show(
                            ui_consumption,
                            |ui| {
                                TableBuilder::new(ui)
                                    .id_salt("Consumption List")
                                    .sense(Sense::click())
                                    .column(Column::auto())
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
                                                ui.with_layout(
                                                    Layout::right_to_left(egui::Align::Center),
                                                    |ui| {
                                                        ui.add(
                                                            Label::new(format!(
                                                                "{:.0}",
                                                                sum_list_cons[idx].2
                                                            ))
                                                            .extend(),
                                                        );
                                                    },
                                                );
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
                                                                "{:.0}/min",
                                                                sum_list_cons[idx].2
                                                                    / ticks_total as f32
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

    mem::drop(fake_game_state);
    mem::drop(aux_data);
    mem::drop(state_machine);
    mem::drop(data_store);

    Window::new("Profiler").default_open(false).show(ctx, |ui| {
        puffin_egui::profiler_ui(ui);
    });

    Ok(actions.into_iter())
}

fn render_items_straight<ItemIdxType: IdxTrait, RecipeIdxType: IdxTrait>(
    range_items_iter: impl IntoIterator<Item = Option<Item<ItemIdxType>>>,
    dir: Dir,
    start_pos: BeltLenType,
    amount: u16,
    draw_pos_start_pos: (f32, f32),
    partial_offs: f32,
    last_moved: BeltLenType,
    layer: &mut Layer,
    atlas: &TextureAtlas,
) {
    assert!(partial_offs >= 0.0);
    assert!(partial_offs <= 1.0);

    let offs = dir.into_offset();
    let item_render_offs = (
        -f32::from(offs.0) / f32::from(BELT_LEN_PER_TILE),
        -f32::from(offs.1) / f32::from(BELT_LEN_PER_TILE),
    );

    let mut item_render_base_pos: (f32, f32) = draw_pos_start_pos;

    for (i, item) in range_items_iter.into_iter().enumerate() {
        let belt_pos = start_pos - (amount - i as BeltLenType);
        let draw_pos = if belt_pos < last_moved {
            [item_render_base_pos.0, item_render_base_pos.1]
        } else {
            [
                item_render_base_pos.0 + item_render_offs.0 * (1.0 - partial_offs),
                item_render_base_pos.1 + item_render_offs.1 * (1.0 - partial_offs),
            ]
        };

        if let Some(item) = item {
            layer.draw_sprite(
                &atlas.items[item.id.into()],
                DrawInstance {
                    position: [draw_pos[0], draw_pos[1]],
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
