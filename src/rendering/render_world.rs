use std::{
    cmp::{max, min},
    iter::successors,
};

use crate::{
    belt::{belt::Belt, splitter::SPLITTER_BELT_LEN, BeltTileId},
    data::DataStore,
    frontend::{
        action::{
            action_state_machine::{
                ActionStateMachine, ActionStateMachineState, StatisticsPanel, WIDTH_PER_LEVEL,
            },
            set_recipe::SetRecipeInfo,
            ActionType,
        },
        world::tile::{
            AssemblerID, AssemblerInfo, Dir, Entity, BELT_LEN_PER_TILE, CHUNK_SIZE_FLOAT,
        },
    },
    item::{usize_from, IdxTrait, Item, Recipe},
    power::power_grid::MAX_POWER_MULT,
    statistics::{
        NUM_SAMPLES_AT_INTERVALS, NUM_X_AXIS_TICKS, RELATIVE_INTERVAL_MULTS, TIMESCALE_LEGEND,
    },
};
use eframe::egui::{
    self, Align2, Color32, ComboBox, Context, CornerRadius, ProgressBar, Stroke, Ui, Window,
};
use egui_extras::{Column, TableBuilder};
use egui_plot::{AxisHints, GridMark, Line, Plot, PlotPoints};
use log::{info, warn};
use tilelib::types::{DrawInstance, Layer, RendererTrait};

use super::{app_state::GameState, TextureAtlas};

// TODO: I found a weird performance cliff while zooming out, jumping from ~10ms to 20ms suddenly
//       Investigate!
//       => This seems to happen at 992 -> 993 width, i.e. when the for loop range jumps from 31 to 32, very suspicous

#[allow(clippy::too_many_lines)]
pub fn render_world<ItemIdxType: IdxTrait, RecipeIdxType: IdxTrait>(
    renderer: &mut impl RendererTrait,
    game_state: &GameState<ItemIdxType, RecipeIdxType>,
    texture_atlas: &TextureAtlas,
    state_machine: &ActionStateMachine<ItemIdxType, RecipeIdxType>,
    data_store: &DataStore<ItemIdxType, RecipeIdxType>,
) {
    let num_tiles_across_screen = WIDTH_PER_LEVEL as f32 * state_machine.zoom_level;
    let tilesize: f32 = 1.0 / num_tiles_across_screen;

    let mut tile_layer = Layer::square_tile_grid(tilesize);
    let mut entity_layer = Layer::square_tile_grid(tilesize);

    let mut item_layer = Layer::square_tile_grid(tilesize);

    let mut player_layer = Layer::square_tile_grid(tilesize);

    let mut warning_layer = Layer::square_tile_grid(tilesize);

    let range_layer = Layer::square_tile_grid(tilesize);

    let player_pos = state_machine.local_player_pos;

    let player_chunk = (
        (player_pos.0 / CHUNK_SIZE_FLOAT) as usize,
        (player_pos.1 / CHUNK_SIZE_FLOAT) as usize,
    );

    for x_offs in -((num_tiles_across_screen / CHUNK_SIZE_FLOAT / 2.0).ceil() as i32)
        ..=((num_tiles_across_screen / CHUNK_SIZE_FLOAT / 2.0).ceil() as i32)
    {
        // TODO: Use different height (aspect ratio!)
        for y_offs in -((num_tiles_across_screen / CHUNK_SIZE_FLOAT / 2.0).ceil() as i32)
            ..=((num_tiles_across_screen / CHUNK_SIZE_FLOAT / 2.0).ceil() as i32)
        {
            let chunk_draw_offs = (
                x_offs as f32 * CHUNK_SIZE_FLOAT - player_pos.0 % CHUNK_SIZE_FLOAT
                    + (0.5 * num_tiles_across_screen),
                y_offs as f32 * CHUNK_SIZE_FLOAT - player_pos.1 % CHUNK_SIZE_FLOAT
                    + (0.5 * num_tiles_across_screen),
            );

            match game_state.world.get_chunk(
                player_chunk
                    .0
                    .wrapping_add_signed(x_offs.try_into().unwrap()),
                player_chunk
                    .1
                    .wrapping_add_signed(y_offs.try_into().unwrap()),
            ) {
                Some(chunk) => {
                    for (x, row) in chunk.floor_tiles.iter().enumerate() {
                        for (y, tile) in row.iter().enumerate() {
                            match tile {
                                crate::frontend::world::tile::FloorTile::Empty => tile_layer
                                    .draw_sprite(
                                        &texture_atlas.blue,
                                        DrawInstance {
                                            position: [
                                                chunk_draw_offs.0 + x as f32,
                                                chunk_draw_offs.1 + y as f32,
                                            ],
                                            size: [1.0, 1.0],
                                            animation_frame: 0,
                                        },
                                    ),
                                _ => tile_layer.draw_sprite(
                                    &texture_atlas.default,
                                    DrawInstance {
                                        position: [
                                            chunk_draw_offs.0 + x as f32,
                                            chunk_draw_offs.1 + y as f32,
                                        ],
                                        size: [1.0, 1.0],
                                        animation_frame: 0,
                                    },
                                ),
                            }
                        }
                    }

                    for entity in chunk.get_entities() {
                        match entity {
                            crate::frontend::world::tile::Entity::Assembler {
                                pos, info, ..
                            } => {
                                entity_layer.draw_sprite(
                                    &texture_atlas.assembler,
                                    DrawInstance {
                                        position: [
                                            chunk_draw_offs.0 + (pos.x % 16) as f32,
                                            chunk_draw_offs.1 + (pos.y % 16) as f32,
                                        ],
                                        size: [3.0, 3.0],
                                        animation_frame: 0,
                                    },
                                );

                                match info {
                                    AssemblerInfo::UnpoweredNoRecipe
                                    | AssemblerInfo::Unpowered(_) => {
                                        warning_layer.draw_sprite(
                                            &texture_atlas.not_connected,
                                            DrawInstance {
                                                position: [
                                                    chunk_draw_offs.0 + (pos.x % 16) as f32,
                                                    chunk_draw_offs.1 + (pos.y % 16) as f32,
                                                ],
                                                size: [3.0, 3.0],
                                                animation_frame: 0,
                                            },
                                        );
                                    },
                                    AssemblerInfo::PoweredNoRecipe(pole_position)
                                    | AssemblerInfo::Powered {
                                        id:
                                            AssemblerID {
                                                recipe: _,
                                                grid: _,
                                                assembler_index: _,
                                            },
                                        pole_position,
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
                                            warning_layer.draw_sprite(
                                                &texture_atlas.no_power,
                                                DrawInstance {
                                                    position: [
                                                        chunk_draw_offs.0 + (pos.x % 16) as f32,
                                                        chunk_draw_offs.1 + (pos.y % 16) as f32,
                                                    ],
                                                    size: [3.0, 3.0],
                                                    animation_frame: 0,
                                                },
                                            );
                                        }
                                    },
                                }
                            },

                            crate::frontend::world::tile::Entity::Belt {
                                pos,
                                direction,
                                id,
                                belt_pos,
                            } => {
                                entity_layer.draw_sprite(
                                    &texture_atlas.belt[*direction],
                                    DrawInstance {
                                        position: [
                                            chunk_draw_offs.0 + (pos.x % 16) as f32,
                                            chunk_draw_offs.1 + (pos.y % 16) as f32,
                                        ],
                                        size: [1.0, 1.0],
                                        animation_frame: 0,
                                    },
                                );

                                // Draw Items
                                let items_iter = game_state
                                    .simulation_state
                                    .factory
                                    .belts
                                    .get_item_iter(*id)
                                    .into_iter()
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

                                // TODO: This needs to be positions correctly and take rotation into account
                                let mut item_render_base_pos: (f32, f32) = (
                                    centered_on_tile.0 + f32::from(offs.0) * 0.5
                                        - 0.5 * (f32::from(offs.0) / f32::from(BELT_LEN_PER_TILE)),
                                    centered_on_tile.1 + f32::from(offs.1) * 0.5
                                        - 0.5 * (f32::from(offs.1) / f32::from(BELT_LEN_PER_TILE)),
                                );

                                for item in items_iter {
                                    if let Some(item) = item {
                                        item_layer.draw_sprite(
                                            // &texture_atlas.items[item.id.into()],
                                            &texture_atlas.items[0],
                                            DrawInstance {
                                                position: [
                                                    item_render_base_pos.0,
                                                    item_render_base_pos.1,
                                                ],
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

                            Entity::Inserter { pos, direction, .. } => {
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
                            },

                            Entity::PowerPole {
                                ty,
                                pos,
                                connected_power_poles,
                            } => {
                                // TODO:
                                // println!("Pole at {pos:?}, with grid: {grid_id}");
                                entity_layer.draw_sprite(
                                    &texture_atlas.assembler,
                                    DrawInstance {
                                        position: [
                                            chunk_draw_offs.0 + (pos.x % 16) as f32,
                                            chunk_draw_offs.1 + (pos.y % 16) as f32,
                                        ],
                                        size: [1.0, 1.0],
                                        animation_frame: 0,
                                    },
                                );
                            },

                            Entity::Splitter { pos, direction, id } => {
                                let [inputs, outputs] = game_state
                                    .simulation_state
                                    .factory
                                    .belts
                                    .get_splitter_belt_ids(*id);

                                let right_dir = direction.turn_right();

                                for ((pos, input), output) in
                                    successors(Some(*pos), |pos| Some(*pos + right_dir))
                                        .zip(inputs)
                                        .zip(outputs)
                                {
                                    entity_layer.draw_sprite(
                                        &texture_atlas.belt[*direction],
                                        DrawInstance {
                                            position: [
                                                chunk_draw_offs.0 + (pos.x % 16) as f32,
                                                chunk_draw_offs.1 + (pos.y % 16) as f32,
                                            ],
                                            size: [1.0, 1.0],
                                            animation_frame: 0,
                                        },
                                    );

                                    let centered_on_tile = (
                                        chunk_draw_offs.0 + (pos.x % 16) as f32 + 0.5
                                            - 0.5 * (1.0 / f32::from(BELT_LEN_PER_TILE)),
                                        chunk_draw_offs.1 + (pos.y % 16) as f32 + 0.5
                                            - 0.5 * (1.0 / f32::from(BELT_LEN_PER_TILE)),
                                    );
                                    render_items_straight::<ItemIdxType, RecipeIdxType>(
                                        game_state
                                            .simulation_state
                                            .factory
                                            .belts
                                            .get_item_iter(input),
                                        *direction,
                                        SPLITTER_BELT_LEN,
                                        SPLITTER_BELT_LEN,
                                        centered_on_tile,
                                        &mut item_layer,
                                        texture_atlas,
                                    );
                                    let out_belt_len =
                                        game_state.simulation_state.factory.belts.get_len(output);
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
                                        &mut item_layer,
                                        texture_atlas,
                                    );
                                }
                                // todo!()
                            },
                            Entity::Chest {
                                ty,
                                pos,
                                item,
                                index,
                            } => {
                                entity_layer.draw_sprite(
                                    &texture_atlas.default,
                                    DrawInstance {
                                        position: [
                                            chunk_draw_offs.0 + (pos.x % 16) as f32,
                                            chunk_draw_offs.1 + (pos.y % 16) as f32,
                                        ],
                                        size: [1.0, 1.0],
                                        animation_frame: 0,
                                    },
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
                                        size: [3.0, 3.0],
                                        animation_frame: 0,
                                    },
                                );
                            },
                            // TODO: Render if a lab is working!
                            Entity::Lab { ty, pos, .. } => {
                                entity_layer.draw_sprite(
                                    &texture_atlas.belt[Dir::North],
                                    DrawInstance {
                                        position: [
                                            chunk_draw_offs.0 + (pos.x % 16) as f32,
                                            chunk_draw_offs.1 + (pos.y % 16) as f32,
                                        ],
                                        size: [3.0, 3.0],
                                        animation_frame: 0,
                                    },
                                );
                            },

                            e => todo!("{:?}", e),
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
        }
    }

    match &state_machine.state {
        crate::frontend::action::action_state_machine::ActionStateMachineState::Idle => {},
        crate::frontend::action::action_state_machine::ActionStateMachineState::Holding(e) => {
            match e {
                crate::frontend::action::action_state_machine::HeldObject::Tile(floor_tile) => {
                    // TODO
                },
                crate::frontend::action::action_state_machine::HeldObject::Entity(
                    place_entity_type,
                ) => match place_entity_type {
                    // TODO:
                    crate::frontend::world::tile::PlaceEntityType::Assembler(position) => {
                        entity_layer.draw_sprite(
                            &texture_atlas.assembler,
                            DrawInstance {
                                position: [
                                    position.x as f32 - state_machine.local_player_pos.0
                                        + num_tiles_across_screen / 2.0,
                                    position.y as f32 - state_machine.local_player_pos.1
                                        + num_tiles_across_screen / 2.0,
                                ],
                                size: [3.0, 3.0],
                                animation_frame: 0,
                            },
                        )
                    },
                    crate::frontend::world::tile::PlaceEntityType::Inserter {
                        pos,
                        dir,
                        filter,
                    } => {},
                    crate::frontend::world::tile::PlaceEntityType::Belt { pos, direction } => {},
                    crate::frontend::world::tile::PlaceEntityType::PowerPole { pos, ty } => {},
                    crate::frontend::world::tile::PlaceEntityType::Splitter {
                        pos,
                        direction,
                        in_mode,
                        out_mode,
                    } => {},
                    crate::frontend::world::tile::PlaceEntityType::Chest { pos } => {},
                    crate::frontend::world::tile::PlaceEntityType::SolarPanel { pos, ty } => {},
                    crate::frontend::world::tile::PlaceEntityType::Lab { pos, ty } => {},
                },
            }
        },
        crate::frontend::action::action_state_machine::ActionStateMachineState::Viewing(pos) => {
            // TODO:
        },
        crate::frontend::action::action_state_machine::ActionStateMachineState::Decontructing(
            position,
            _,
        ) => {
            // TODO:
        },
    }

    for (player_id, player) in game_state
        .world
        .players
        .iter()
        .enumerate()
        .filter(|(_, p)| p.visible)
        .filter(|(i, _)| *i != state_machine.my_player_id as usize)
    {
        player_layer.draw_sprite(
            &texture_atlas.player,
            DrawInstance {
                position: [
                    player.pos.0 - state_machine.local_player_pos.0 + num_tiles_across_screen / 2.0,
                    player.pos.1 - state_machine.local_player_pos.1 + num_tiles_across_screen / 2.0,
                ],
                size: [1.0, 2.0],
                animation_frame: 0,
            },
        );
        info!(
            "Rendering other player {} at {:?}",
            player_id,
            [
                player.pos.0 - state_machine.local_player_pos.0,
                player.pos.1 - state_machine.local_player_pos.1,
            ]
        );
    }

    player_layer.draw_sprite(
        &texture_atlas.player,
        DrawInstance {
            // Always in the middle
            position: [num_tiles_across_screen / 2.0, num_tiles_across_screen / 2.0],
            size: [1.0, 2.0],
            animation_frame: 0,
        },
    );

    info!("Rendering self at {:?}", state_machine.local_player_pos);

    renderer.draw(&tile_layer);

    renderer.draw(&range_layer);

    renderer.draw(&entity_layer);

    renderer.draw(&item_layer);

    renderer.draw(&warning_layer);

    renderer.draw(&player_layer);
}

pub fn render_ui<ItemIdxType: IdxTrait, RecipeIdxType: IdxTrait>(
    ctx: &Context,
    ui: &Ui,
    state_machine: &mut ActionStateMachine<ItemIdxType, RecipeIdxType>,
    game_state: &GameState<ItemIdxType, RecipeIdxType>,
    data_store: &DataStore<ItemIdxType, RecipeIdxType>,
) -> impl IntoIterator<Item = ActionType<ItemIdxType, RecipeIdxType>> {
    let mut actions = vec![];

    match &state_machine.state {
        crate::frontend::action::action_state_machine::ActionStateMachineState::Idle => {},
        crate::frontend::action::action_state_machine::ActionStateMachineState::Holding(
            held_object,
        ) => {},
        crate::frontend::action::action_state_machine::ActionStateMachineState::Viewing(
            position,
        ) => {
            let mut viewing = true;
            Window::new("Viewing").open(&mut viewing).show(ctx, |ui| {
                let chunk = game_state
                .world
                .get_chunk_for_tile(*position)
                .expect("Cannot find chunk for viewing");
            let entity = chunk.get_entity_at(*position, data_store);

            if let Some(entity) = entity {
                match entity {
                    crate::frontend::world::tile::Entity::Assembler { pos, info } => {
                        let mut goal_recipe: Option<Recipe<RecipeIdxType>> = match info {
                            AssemblerInfo::UnpoweredNoRecipe => None,
                            AssemblerInfo::Unpowered(recipe) => Some(*recipe),
                            AssemblerInfo::PoweredNoRecipe(position) => None,
                            AssemblerInfo::Powered { id, pole_position } => Some(id.recipe),
                        };

                        ComboBox::new("Recipe list", "Recipes").selected_text(goal_recipe.map(|recipe| data_store.recipe_names[usize_from(recipe.id)].as_str()).unwrap_or("Choose a recipe!")).show_ui(ui, |ui| {
                            data_store.recipe_names.iter().enumerate().for_each(|(i, recipe_name)| {
                                ui.selectable_value(&mut goal_recipe, Some(Recipe {id: i.try_into().unwrap()}), recipe_name);
                            });
                        });


                        match info {
                            crate::frontend::world::tile::AssemblerInfo::UnpoweredNoRecipe => {
                                ui.label("Assembler");
                                if let Some(goal_recipe) = goal_recipe {
                                    actions.push(ActionType::SetRecipe(SetRecipeInfo { pos: *pos, recipe: goal_recipe }));
                                }
                            },
                            crate::frontend::world::tile::AssemblerInfo::Unpowered(recipe) => {
                                ui.label("Assembler");
                                if let Some(goal_recipe) = goal_recipe {
                                    if goal_recipe != *recipe {
                                        actions.push(ActionType::SetRecipe(SetRecipeInfo { pos: *pos, recipe: goal_recipe }));
                                    }
                                }
                            },
                            crate::frontend::world::tile::AssemblerInfo::PoweredNoRecipe(grid) => {
                                ui.label("Assembler");
                                if let Some(goal_recipe) = goal_recipe {
                                    actions.push(ActionType::SetRecipe(SetRecipeInfo { pos: *pos, recipe: goal_recipe }));
                                }
                            },
                            crate::frontend::world::tile::AssemblerInfo::Powered {
                                id,
                                pole_position
                            } => {
                                ui.label("Assembler");

                                if let Some(goal_recipe) = goal_recipe {
                                    if goal_recipe != id.recipe {
                                        actions.push(ActionType::SetRecipe(SetRecipeInfo { pos: *pos, recipe: goal_recipe }));
                                    }
                                }

                                // TODO:
                                // ui.label(data_store.recipe_names[usize_from(assembler_id.recipe.id)]);

                                let assembler = game_state
                                    .simulation_state
                                    .factory
                                    .power_grids
                                    .get_assembler_info(*id, data_store);

                                let pb = ProgressBar::new(assembler.timer_percentage).show_percentage().corner_radius(CornerRadius::ZERO);
                                ui.add(pb);

                                TableBuilder::new(ui).columns(Column::auto().resizable(false), assembler.inputs.len() + assembler.outputs.len()).body(|mut body| {
                                    body.row(5.0, |mut row| {
                                        for (item, count) in assembler.inputs.iter().chain(assembler.outputs.iter()) {
                                            row.col(|ui| {
                                                ui.label(&data_store.item_names[usize_from(item.id)]);
                                                ui.label(format!("{}", *count));
                                            });
                                        }
                                    });
                                });
                            }
                        }
                    },
                    crate::frontend::world::tile::Entity::PowerPole {
                        ty,
                        pos,
                        connected_power_poles,
                    } => {
                        // TODO:
                        let power_range = data_store.power_pole_data[usize::from(*ty)].power_range;
                        let size = data_store.power_pole_data[usize::from(*ty)].size;

                        let pg = &game_state.simulation_state.factory.power_grids.power_grids
                            [game_state
                                .simulation_state
                                .factory
                                .power_grids
                                .pole_pos_to_grid_id[pos] as usize];

                        ui.label("Power Grid");

                        let pb = ProgressBar::new(pg.last_power_mult as f32 / 64.0);
                        ui.add(pb);

                        let timescale = 1;
                        let max_value_at_timescale = (MAX_POWER_MULT as f64) * (RELATIVE_INTERVAL_MULTS[..=timescale].iter().copied().product::<usize>() as f64);
                        let num_samples = NUM_SAMPLES_AT_INTERVALS[timescale];

                        let points = pg.power_history.get_series(timescale, data_store, Some(|_| true)).into_iter().map(|series| (series.name, series.data.into_iter()
                        .enumerate()
                        .map(|(i, v)| [i as f64, v.into()])
                        .collect::<Vec<_>>()));
                        let lines = points.into_iter().map(|(name, points)| {
                            Line::new(name, points)
                                .stroke(Stroke::new(2.0, Color32::GREEN))
                        });

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
                        pos,
                        direction,
                        id,
                        belt_pos,
                    } => match id {
                        BeltTileId::AnyBelt(index, phantom_dat_) => todo!(),
                    },
                    crate::frontend::world::tile::Entity::Inserter {
                        pos,
                        direction,
                        info,
                    } => match info {
                        crate::frontend::world::tile::InserterInfo::NotAttached {
                            start_pos,
                            end_pos,
                        } => println!("Unattached inserter at {pos:?}"),
                        crate::frontend::world::tile::InserterInfo::Attached(ins) => match ins {
                            crate::frontend::world::tile::AttachedInserter::BeltStorage {
                                id,
                                belt_pos,
                            } => {
                                todo!()
                            },
                            crate::frontend::world::tile::AttachedInserter::BeltBelt {
                                item,
                                inserter,
                            } => {
                                todo!()
                            },
                            crate::frontend::world::tile::AttachedInserter::StorageStorage(_) => {
                                todo!()
                            },
                        },
                    },
                    Entity::Splitter { .. } => {
                        warn!("Viewing Splitter. This currently does nothing!");
                    },
                    Entity::Chest {
                        ty,
                        pos,
                        item,
                        index,
                    } => {
                        let Some(item) = item else {
                            todo!()
                        };

                        let stack_size: u16 = data_store.item_stack_sizes[usize_from(item.id)] as u16;

                        let num_slots = data_store.chest_num_slots[*ty as usize];
                        let (current_items, _max_items) = game_state.simulation_state.factory.chests.stores[usize_from(item.id)].get_chest(*index);

                        TableBuilder::new(ui).columns(Column::auto().resizable(false), 10).body(|mut body| {
                            body.rows(5.0, (num_slots / 10) as usize + (num_slots % 10 > 0) as usize, |mut row| {
                                let idx = row.index();
                                for col_idx in 0..10 {
                                    let slot_id = idx * 10 + col_idx;
                                    if slot_id >= num_slots as usize {
                                        break;
                                    }
                                    row.col(|ui| {
                                        let this_slots_stack_count = min(current_items.saturating_sub(slot_id as u16 * stack_size), stack_size);

                                        let clicked = ui.label(format!("{}", this_slots_stack_count)).clicked();
                                        let mut shift = false;
                                        ctx.input(|input| {shift = input.modifiers.shift; });

                                        if shift && clicked {
                                            todo!("Move the items into the players inventory if there is space");
                                        }
                                    });
                                }
                            });
                        });
                    }
                    _ => todo!(),
                }
            }
            });

            if !viewing {
                state_machine.state = ActionStateMachineState::Idle;
            }
        },
        crate::frontend::action::action_state_machine::ActionStateMachineState::Decontructing(
            position,
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

    Window::new("Statistics")
        .open(&mut state_machine.statistics_panel_open)
        .show(ctx, |ui| match state_machine.statistics_panel {
            StatisticsPanel::Production(scale) => {
                let points: Vec<(String, usize, PlotPoints)> = game_state
                    .statistics
                    .production
                    .get_series(scale, data_store, Some(|_| true))
                    .into_iter()
                    .enumerate()
                    .map(|(i, series)| (series.name, i, series.data))
                    .map(|(name, i, data)| {
                        (
                            name,
                            i,
                            data.into_iter()
                                .enumerate()
                                .map(|(i, v)| [i as f64, v.into()])
                                .collect(),
                        )
                    })
                    .filter(|(_, _, points): &(_, _, PlotPoints)| {
                        points.points().iter().any(|p| p.y > 0.0)
                    })
                    .collect();
                let lines = points.into_iter().map(|(name, id, points)| {
                    Line::new(name, points).stroke(Stroke::new(2.0, data_store.item_to_colour[id]))
                });

                let ticks_per_value = RELATIVE_INTERVAL_MULTS[..=scale]
                    .iter()
                    .copied()
                    .product::<usize>() as f64;

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
                        let mut lower_dec = 10.0_f64.powf(
                            (grid_input.bounds.1 / ticks_per_value * 60.0 * 60.0)
                                .log10()
                                .floor(),
                        );

                        if lower_dec < 1.0 {
                            lower_dec = 1.0;
                        }

                        lower_dec = lower_dec * ticks_per_value / 60.0 / 60.0;

                        (0..40)
                            .map(|v| GridMark {
                                value: (v as f64) / 4.0 * lower_dec,
                                step_size: lower_dec / 4.0,
                            })
                            .chain((0..10).map(|v| GridMark {
                                value: (v as f64) * lower_dec,
                                step_size: 1.0 * lower_dec,
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
                        [AxisHints::new_x().formatter(|v, _| TIMESCALE_LEGEND[scale](v.value))]
                            .to_vec(),
                    )
                    .include_y(0)
                    .allow_zoom([false, false])
                    .allow_drag([false, false])
                    .allow_scroll([false, false])
                    .show(ui, |ui| {
                        for line in lines {
                            ui.line(line);
                        }
                    })
            },
        });

    actions
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
