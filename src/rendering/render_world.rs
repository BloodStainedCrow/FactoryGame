use crate::{
    data::DataStore,
    frontend::{
        action::action_state_machine::{ActionStateMachine, WIDTH_PER_LEVEL},
        world::{
            tile::{
                AssemblerID, AssemblerInfo, BeltId, BeltTileId, Entity, BELT_LEN_PER_TILE,
                CHUNK_SIZE_FLOAT,
            },
            Position,
        },
    },
    item::{IdxTrait, WeakIdxTrait},
};
use log::{info, warn};
use tilelib::types::{DrawInstance, Layer, Renderer};

use super::{app_state::GameState, TextureAtlas};

// TODO: I found a weird performance cliff while zooming out, jumping from ~10ms to 20ms suddenly
//       Investigate!
//       => This seems to happen at 992 -> 993 width, i.e. when the for loop range jumps from 31 to 32, very suspicous

#[allow(clippy::too_many_lines)]
pub fn render_world<ItemIdxType: IdxTrait, RecipeIdxType: IdxTrait>(
    renderer: &mut Renderer,
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

    let mut waring_layer = Layer::square_tile_grid(tilesize);

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
                                        waring_layer.draw_sprite(
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
                                    AssemblerInfo::PoweredNoRecipe(grid)
                                    | AssemblerInfo::Powered(AssemblerID {
                                        recipe: _,
                                        grid,
                                        assembler_index: _,
                                    }) => {
                                        let last_power = game_state
                                            .simulation_state
                                            .factory
                                            .power_grids
                                            .power_grids[usize::from(*grid)]
                                        .as_ref()
                                        .unwrap()
                                        .last_power_mult;

                                        if last_power == 0 {
                                            waring_layer.draw_sprite(
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
                                if let BeltTileId::BeltId(id) = id {
                                    // TODO: Do more than one item at once
                                    let item_id: usize = id.item.id.into();
                                    let items_iter =
                                        game_state.simulation_state.factory.belts.belts[item_id]
                                            .belts[id.index]
                                            .items()
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
                                            - 0.5
                                                * (f32::from(offs.0)
                                                    / f32::from(BELT_LEN_PER_TILE)),
                                        centered_on_tile.1 + f32::from(offs.1) * 0.5
                                            - 0.5
                                                * (f32::from(offs.1)
                                                    / f32::from(BELT_LEN_PER_TILE)),
                                    );

                                    for item in items_iter {
                                        if !*item {
                                            continue;
                                        }

                                        item_layer.draw_sprite(
                                            &texture_atlas.plate,
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

                                        item_render_base_pos = (
                                            item_render_base_pos.0 + item_render_offs.0,
                                            item_render_base_pos.1 + item_render_offs.1,
                                        );
                                    }
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
                                grid_id,
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

                            _ => todo!(),
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
        crate::frontend::action::action_state_machine::ActionStateMachineState::Holding(..) => {
            // TODO:
        },
        crate::frontend::action::action_state_machine::ActionStateMachineState::Viewing(pos) => {
            // TODO:
            let chunk = game_state
                .world
                .get_chunk_for_tile(*pos)
                .expect("Cannot find chunk for viewing");
            let entity = chunk.get_entity_at(*pos, data_store);

            if let Some(entity) = entity {
                dbg!(entity);
                match entity {
                    crate::frontend::world::tile::Entity::Assembler { pos, info } => match info {
                        crate::frontend::world::tile::AssemblerInfo::UnpoweredNoRecipe => {
                            println!("Viewing AssemblerWithoutRecipe {pos:?}");
                        },
                        crate::frontend::world::tile::AssemblerInfo::Unpowered(recipe) => {
                            println!("Viewing Unpowered Assembler {pos:?}, {recipe:?}");
                        },
                        crate::frontend::world::tile::AssemblerInfo::PoweredNoRecipe(grid) => {
                            println!("Viewing AssemblerWithoutRecipe {pos:?}, {grid:?}");
                        },
                        crate::frontend::world::tile::AssemblerInfo::Powered(assembler_id) => {
                            let assembler = game_state
                                .simulation_state
                                .factory
                                .power_grids
                                .get_assembler_info(*assembler_id, data_store);

                            dbg!(assembler);
                        },
                    },
                    crate::frontend::world::tile::Entity::PowerPole {
                        ty,
                        pos,
                        grid_id,
                        connected_power_poles,
                    } => {
                        // TODO:
                        let pg = &game_state.simulation_state.factory.power_grids.power_grids
                            [*grid_id as usize];

                        dbg!(pg);
                    },
                    crate::frontend::world::tile::Entity::Belt {
                        pos,
                        direction,
                        id,
                        belt_pos,
                    } => match id {
                        crate::frontend::world::tile::BeltTileId::EmptyBeltId(idx) => {
                            let belt = &game_state.simulation_state.factory.belts.empty_belts[*idx];

                            dbg!(belt);
                        },
                        crate::frontend::world::tile::BeltTileId::BeltId(BeltId {
                            item,
                            index,
                        }) => {
                            let item_id: usize = item.id.into();
                            let belt = &game_state.simulation_state.factory.belts.belts[item_id]
                                .belts[*index];

                            dbg!(belt);
                        },
                    },
                    crate::frontend::world::tile::Entity::Inserter {
                        pos,
                        direction,
                        info,
                    } => {
                        match info {
                            crate::frontend::world::tile::InserterInfo::NotAttached {
                                start_pos,
                                end_pos,
                            } => println!("Unattached inserter at {pos:?}"),
                            crate::frontend::world::tile::InserterInfo::Attached(ins) => match ins {
                                crate::frontend::world::tile::AttachedInserter::BeltStorage {
                                    id,
                                    belt_pos,
                                } => {
                                    let crate::frontend::world::tile::BeltTileId::BeltId(BeltId {
                                        item,
                                        index,
                                    }) = id
                                    else {
                                        unreachable!("Inserter attached to EmptyBelt, is currently impossible");
                                    };

                                    let item_id: usize = item.id.into();
                                    let belt = &game_state.simulation_state.factory.belts.belts
                                        [item_id]
                                        .belts[*index];

                                    let inserter_info = belt
                                        .get_inserter_info_at(*belt_pos)
                                        .expect("Did not find inserter where an entity points to!");

                                    dbg!(inserter_info);
                                },
                                crate::frontend::world::tile::AttachedInserter::BeltBelt {
                                    item,
                                    inserter,
                                } => {
                                    let item_id: usize = item.id.into();
                                    let inserter = &game_state
                                        .simulation_state
                                        .factory
                                        .belt_belt_inserters
                                        .inserters[item_id][*inserter];

                                    dbg!(&inserter.0);
                                },
                                crate::frontend::world::tile::AttachedInserter::StorageStorage(
                                    _,
                                ) => todo!(),
                            },
                        }
                    },
                }
            }
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
    renderer.draw(&entity_layer);

    renderer.draw(&item_layer);

    renderer.draw(&waring_layer);

    renderer.draw(&player_layer);
}
