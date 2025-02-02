use std::mem;

use crate::{
    belt::{
        belt::Belt,
        smart::{EmptyBelt, SmartBelt},
    },
    frontend::{
        action::{action_state_machine::ActionStateMachine, ActionType},
        world::{
            tile::{AssemblerID, BeltId, Entity, World, BELT_LEN_PER_TILE},
            Position,
        },
    },
    inserter::{StorageID, Storages, NUM_RECIPES},
    item::{CopperOre, IronOre, Item, ItemStorage},
    power::{PowerGrid, Watt},
    research::TechState,
};
use log::warn;
use rayon::iter::{IntoParallelRefMutIterator, ParallelIterator};
use tilelib::types::Renderer;

use super::{render_world::render_world, TextureAtlas};

use crate::frontend::action::place_tile::PositionInfo;

#[derive(Debug, serde::Deserialize, serde::Serialize)]
pub struct GameState {
    pub world: World,
    pub simulation_state: SimulationState,
}

#[derive(Debug, Default, serde::Deserialize, serde::Serialize)]
pub struct SimulationState {
    tech_state: TechState,
    pub factory: Factory,
    // TODO:
}

#[derive(Debug, serde::Deserialize, serde::Serialize)]
pub struct Factory {
    pub power_grids: Vec<PowerGrid>,
    pub belts: BeltStore,
}

impl Default for Factory {
    fn default() -> Self {
        Self {
            power_grids: vec![PowerGrid::default()],
            belts: BeltStore::default(),
        }
    }
}

#[derive(Debug, Default, serde::Deserialize, serde::Serialize)]
pub struct BeltStore {
    pub empty_belts: Vec<EmptyBelt>,

    pub iron_ore_belts: Vec<SmartBelt<IronOre>>,
    pub copper_ore_belt: Vec<SmartBelt<CopperOre>>,
}

struct StorageStore<'a, 'b> {
    iron_ore: Storages<'a, 'b, IronOre>,
    copper_ore: Storages<'a, 'b, CopperOre>,
}

pub enum AppState {
    Ingame(GameState),
}

impl AppState {
    pub fn render(
        &self,
        renderer: &mut Renderer,
        texture_atlas: &TextureAtlas,
        state_machine: &ActionStateMachine,
    ) {
        match self {
            Self::Ingame(game) => {
                render_world(
                    renderer,
                    game,
                    texture_atlas,
                    game.world.player_pos,
                    state_machine,
                );
            },
        }
    }
}

impl GameState {
    #[allow(clippy::too_many_lines)]
    pub fn apply_actions(&mut self, actions: impl IntoIterator<Item = ActionType>) {
        for action in actions {
            match action {
                ActionType::PlaceFloorTile(place_floor_tile_by_hand_info) => {
                    let num_items_needed = match place_floor_tile_by_hand_info.ghost_info.position {
                        PositionInfo::Rect { pos, width, height } => width * height,
                        PositionInfo::Single { pos } => 1,
                        PositionInfo::List { ref positions } => positions.len(),
                    };

                    // TODO: Check player inventory for enough resources

                    match place_floor_tile_by_hand_info.ghost_info.position {
                        PositionInfo::Rect { pos, width, height } => {
                            for x in pos.x..(pos.x + width) {
                                for y in pos.y..(pos.y + height) {
                                    let chunk = (x / 16, y / 16);
                                    let chunk = self.world.chunks.get_mut(chunk.0, chunk.1);

                                    if let Some(chunk) = chunk {
                                        chunk.floor_tiles[x % 16][y % 16] =
                                            place_floor_tile_by_hand_info.ghost_info.tile;
                                    } else {
                                        // This is not in a chunk that was already generated, ignored
                                    }
                                }
                            }
                        },
                        PositionInfo::Single { pos } => {
                            let chunk = (pos.x / 16, pos.y / 16);
                            dbg!(chunk);
                            let chunk = self.world.chunks.get_mut(chunk.0, chunk.1);

                            if let Some(chunk) = chunk {
                                chunk.floor_tiles[pos.x % 16][pos.y % 16] =
                                    place_floor_tile_by_hand_info.ghost_info.tile;
                            } else {
                                // This is not in a chunk that was already generated, ignored
                            }
                        },
                        PositionInfo::List { positions } => {
                            for pos in positions {
                                let chunk = (pos.x / 16, pos.y / 16);
                                let chunk = self.world.chunks.get_mut(chunk.0, chunk.1);

                                if let Some(chunk) = chunk {
                                    chunk.floor_tiles[pos.x % 16][pos.y % 16] =
                                        place_floor_tile_by_hand_info.ghost_info.tile;
                                } else {
                                    // This is not in a chunk that was already generated, ignored
                                }
                            }
                        },
                    }
                },
                ActionType::PlaceEntity(place_entity_info) => match place_entity_info.entities {
                    crate::frontend::action::place_entity::EntityPlaceOptions::Single(
                        place_entity_type,
                    ) => match place_entity_type {
                        crate::frontend::world::tile::PlaceEntityType::Assembler(position) => {
                            // TODO: Do collision checks!
                            let chunk = self.world.get_chunk_for_tile_mut(position);

                            if let Some(chunk) = chunk {
                                // TODO: Get correct power_grid for this position
                                let power_grid_idx = 0;

                                // TODO: For now we just default to producing gears
                                let assembler_id = self.simulation_state.factory.power_grids
                                    [power_grid_idx]
                                    .stores
                                    .gears
                                    .add_assembler();

                                chunk
                                    .entities
                                    .push(crate::frontend::world::tile::Entity::Assembler {
                                    pos: Position {
                                        x: position.x % 16,
                                        y: position.y % 16,
                                    },
                                    // TODO:
                                    id: AssemblerID {
                                        recipe: 0,
                                        grid: 0,
                                        assembler_index: assembler_id.try_into().expect(
                                            "Reached maximum of u16::MAX assemblers of this recipe",
                                        ),
                                    },
                                });
                            } else {
                                warn!(
                                    "Tried to place assember outside world/in ungenerated chunk!"
                                );
                            }
                        },
                        crate::frontend::world::tile::PlaceEntityType::Inserter {
                            start_pos,
                            end_pos,
                            filter,
                        } => {
                            enum InserterStartInfo {
                                Belt(BeltId, usize),
                                Assembler(AssemblerID),
                            }

                            let start_chunk = self.world.get_chunk_for_tile(start_pos);

                            #[allow(clippy::option_if_let_else)]
                            let start_info = if let Some(chunk) = start_chunk {
                                if let Some(start_entity) = chunk.get_entity_at(start_pos) {
                                    match start_entity {
                                        Entity::Assembler { pos, id } => {
                                            Some(InserterStartInfo::Assembler(*id))
                                        },
                                        Entity::PowerPole { .. } => {
                                            warn!("Tried to place assembler on PowerPole");
                                            None
                                        },
                                        Entity::Belt { id, belt_pos, .. } => {
                                            Some(InserterStartInfo::Belt(*id, *belt_pos))
                                        },
                                    }
                                } else {
                                    warn!("Tried to place inserter on nothing!");
                                    None
                                }
                            } else {
                                warn!(
                                    "Tried to place inserter outside world/in ungenerated chunk!"
                                );
                                None
                            };

                            if let Some(start_info) = start_info {
                                let end_chunk = self.world.get_chunk_for_tile(end_pos);

                                if let Some(chunk) = end_chunk {
                                    let end_entity = chunk.get_entity_at(end_pos);
                                    let end_entity = end_entity.copied();
                                    if let Some(end_entity) = end_entity {
                                        match end_entity {
                                            Entity::Assembler { id, .. } => {
                                                match start_info {
                                                    InserterStartInfo::Belt(belt_id, belt_pos) => {
                                                        // TODO: match on item type
                                                        // TODO: Instantiate EmptyBelt to SmartBelt, if necessary
                                                        match filter {
                                                            Item::IronOre => {
                                                                if belt_id.item == 0 {
                                                                    // This is an empty belt
                                                                    let mut dummy_belt =
                                                                        EmptyBelt::new(0);

                                                                    mem::swap(
                                                                        &mut dummy_belt,
                                                                        &mut self
                                                                            .simulation_state
                                                                            .factory
                                                                            .belts
                                                                            .empty_belts
                                                                            [belt_id.index],
                                                                    );
                                                                    let removed_belt = dummy_belt;

                                                                    let new_belt_id = self
                                                                        .simulation_state
                                                                        .factory
                                                                        .belts
                                                                        .iron_ore_belts
                                                                        .len();

                                                                    self.simulation_state
                                                                        .factory
                                                                        .belts
                                                                        .iron_ore_belts
                                                                        .push(
                                                                            removed_belt
                                                                                .instantiate(),
                                                                        );

                                                                    // TODO: update the belt tiles!!
                                                                    // start_chunk.change_belt_id(
                                                                    //     belt_id,
                                                                    //     BeltId {
                                                                    //         item: 1,
                                                                    //         index: belt_id.index,
                                                                    //     },
                                                                    // );

                                                                    self.simulation_state
                                                                        .factory
                                                                        .belts
                                                                        .iron_ore_belts[new_belt_id]
                                                                        .add_out_inserter(
                                                                            belt_pos.try_into().unwrap(),
                                                                            // FXIME: this conversion is likely not this simple!!!
                                                                            StorageID {
                                                                                recipe: id.recipe,
                                                                                grid: id.grid,
                                                                                storage: id.assembler_index,
                                                                            },
                                                                        )
                                                                        .expect("Invalid inserter position in action");
                                                                } else {
                                                                    self.simulation_state
                                                                        .factory
                                                                        .belts
                                                                        .iron_ore_belts[belt_id.index]
                                                                        .add_out_inserter(
                                                                            belt_pos.try_into().unwrap(),
                                                                            // FXIME: this conversion is likely not this simple!!!
                                                                            StorageID {
                                                                                recipe: id.recipe,
                                                                                grid: id.grid,
                                                                                storage: id.assembler_index,
                                                                            },
                                                                        )
                                                                        .expect("Invalid inserter position in action");
                                                                }
                                                            },

                                                            _ => todo!(),
                                                        }
                                                    },
                                                    InserterStartInfo::Assembler(assembler_id) => {
                                                        todo!()
                                                    },
                                                }
                                            },
                                            Entity::PowerPole { .. } => {
                                                warn!("Tried to place assembler on PowerPole");
                                            },
                                            Entity::Belt { id, .. } => match start_info {
                                                InserterStartInfo::Belt(belt_id, belt_pos) => {
                                                    todo!()
                                                },
                                                InserterStartInfo::Assembler(assembler_id) => {
                                                    todo!()
                                                },
                                            },
                                        }
                                    } else {
                                        warn!("Tried to place inserter on nothing!");
                                    }
                                } else {
                                    warn!(
                                        "Tried to place inserter outside world/in ungenerated chunk!"
                                    );
                                };
                            }
                        },
                        crate::frontend::world::tile::PlaceEntityType::Belt { pos, direction } => {
                            // TODO: Implement corners correctly

                            // Find neighboring belts if they exist
                            let front_pos = Position {
                                x: pos
                                    .x
                                    .checked_add_signed(direction.into_offset().0.into())
                                    .expect("out of bounds"),
                                y: pos
                                    .y
                                    .checked_add_signed(direction.into_offset().1.into())
                                    .expect("out of bounds"),
                            };
                            let front_internal_id = self
                                .world
                                .get_chunk_for_tile(front_pos)
                                .map(|chunk| chunk.get_entity_at(front_pos))
                                .and_then(|e| {
                                    if let Some(Entity::Belt {
                                        direction: dir, id, ..
                                    }) = e
                                    {
                                        if *dir == direction.reverse() {
                                            None
                                        } else {
                                            Some(*id)
                                        }
                                    } else {
                                        None
                                    }
                                });

                            let back_pos = Position {
                                x: pos
                                    .x
                                    .checked_add_signed(direction.reverse().into_offset().0.into())
                                    .expect("out of bounds"),
                                y: pos
                                    .y
                                    .checked_add_signed(direction.reverse().into_offset().1.into())
                                    .expect("out of bounds"),
                            };
                            let back_internal_id = self
                                .world
                                .get_chunk_for_tile(back_pos)
                                .map(|chunk| chunk.get_entity_at(back_pos))
                                .and_then(|e| {
                                    if let Some(Entity::Belt {
                                        direction: dir, id, ..
                                    }) = e
                                    {
                                        if *dir == direction {
                                            Some(*id)
                                        } else {
                                            None
                                        }
                                    } else {
                                        None
                                    }
                                });

                            let new_belt = EmptyBelt {
                                len: BELT_LEN_PER_TILE,
                            };

                            match (front_internal_id, back_internal_id) {
                                (None, None) => {
                                    let idx = self.simulation_state.factory.belts.empty_belts.len();
                                    self.simulation_state
                                        .factory
                                        .belts
                                        .empty_belts
                                        .push(new_belt);

                                    let chunk = self.world.get_chunk_for_tile_mut(pos).unwrap();
                                    chunk.entities.push(Entity::Belt {
                                        pos: Position {
                                            x: pos.x % 16,
                                            y: pos.y % 16,
                                        },
                                        direction,
                                        id: BeltId {
                                            item: 0,
                                            index: idx,
                                        },
                                        belt_pos: 0,
                                    });
                                },
                                (None, Some(back_id)) => {
                                    let chunk = self.world.get_chunk_for_tile_mut(pos).unwrap();

                                    // Merge back with this, it will get back_id
                                    // TODO: Match on recipe
                                    let back_belt = self
                                        .simulation_state
                                        .factory
                                        .belts
                                        .empty_belts
                                        .get_mut(back_id.index)
                                        .expect("id from world not in simulation!!");

                                    take_mut::take_or_recover(
                                        back_belt,
                                        || EmptyBelt::new(0),
                                        |back_belt| EmptyBelt::join(new_belt, back_belt),
                                    );

                                    chunk.entities.push(Entity::Belt {
                                        pos: Position {
                                            x: pos.x % 16,
                                            y: pos.y % 16,
                                        },
                                        direction,
                                        id: back_id,

                                        belt_pos: (back_belt.len - BELT_LEN_PER_TILE).into(),
                                    });
                                },
                                (Some(front_id), None) => {
                                    // Merge front with this, it will get front_id
                                    // TODO: Match on recipe
                                    let front_belt = self
                                        .simulation_state
                                        .factory
                                        .belts
                                        .empty_belts
                                        .get_mut(front_id.index)
                                        .expect("id from world not in simulation!!");

                                    take_mut::take_or_recover(
                                        front_belt,
                                        || EmptyBelt::new(0),
                                        |front_belt| EmptyBelt::join(front_belt, new_belt),
                                    );

                                    let chunk = self.world.get_chunk_for_tile_mut(pos).unwrap();

                                    // TODO: Also do this recursively!
                                    chunk.increase_all_belt_pos(front_id, BELT_LEN_PER_TILE.into());

                                    chunk.entities.push(Entity::Belt {
                                        pos: Position {
                                            x: pos.x % 16,
                                            y: pos.y % 16,
                                        },
                                        direction,
                                        id: front_id,

                                        belt_pos: 0,
                                    });
                                },
                                (Some(front_id), Some(back_id)) => {
                                    // TODO: We need a way to handle removing belts, just removing it will invalidate all indices!!!
                                    // Or we can (for now) just leak the freed up spot (by not storing the holes, just not keeping any references to it)
                                    // FIXME: This is a hack to make this work, even though we do not have support for holes in the list of belts yet
                                    self.simulation_state
                                        .factory
                                        .belts
                                        .empty_belts
                                        .push(EmptyBelt::new(0));
                                    let back_belt = self
                                        .simulation_state
                                        .factory
                                        .belts
                                        .empty_belts
                                        .swap_remove(back_id.index);

                                    let back_len = back_belt.len;

                                    // Merge front with this, it will get front_id
                                    // TODO: Match on recipe
                                    let front_belt = self
                                        .simulation_state
                                        .factory
                                        .belts
                                        .empty_belts
                                        .get_mut(front_id.index)
                                        .expect("id from world not in simulation!!");

                                    // TODO: join these three using a single call, instead of two
                                    //       Currently this will reallocate TWICE for no reason!
                                    // Join Front belt with the new empty spot created by placing the belt tile
                                    // FIXME: This will abort the program if it fails
                                    take_mut::take(front_belt, |front_belt| {
                                        EmptyBelt::join(front_belt, new_belt)
                                    });

                                    // Join the (now elongated) front belt with the back belt it is now connected to
                                    // FIXME: This will abort the program if it fails
                                    take_mut::take(front_belt, |front_belt| {
                                        EmptyBelt::join(front_belt, back_belt)
                                    });

                                    let chunk = self.world.get_chunk_for_tile_mut(pos).unwrap();

                                    // TODO: Also do this recursively!
                                    chunk.increase_all_belt_pos(
                                        front_id,
                                        (back_len + BELT_LEN_PER_TILE).into(),
                                    );

                                    chunk.entities.push(Entity::Belt {
                                        pos: Position {
                                            x: pos.x % 16,
                                            y: pos.y % 16,
                                        },
                                        direction,
                                        id: front_id,

                                        belt_pos: back_len.into(),
                                    });

                                    // TODO: Also do this recursively!
                                    chunk.change_belt_id(back_id, front_id);
                                },
                            }
                        },
                    },
                    crate::frontend::action::place_entity::EntityPlaceOptions::Multiple(vec) => {
                        todo!()
                    },
                },
                ActionType::Moving((x, y)) => {
                    self.world.player_move = (f32::from(x), f32::from(y));
                },
                ActionType::Ping((x, y)) => {
                    // Do nothing for now
                    println!("Ping at {:?}", (x, y));
                    // TODO:
                },
            }
        }
    }

    pub fn update(&mut self) {
        self.world.player_pos.0 += self.world.player_move.0 / 60.0;
        self.world.player_pos.1 += self.world.player_move.1 / 60.0;

        self.simulation_state.factory.belt_update();

        let tech_progress = self
            .simulation_state
            .factory
            .power_grids
            .par_iter_mut()
            .map(|grid| grid.update(Watt(1000), &self.simulation_state.tech_state))
            .sum();

        self.simulation_state
            .tech_state
            .apply_progress(tech_progress);
    }
}

impl Factory {
    fn belt_update(&mut self) {
        // let mut storage = Self::build_storage_store(&mut self.power_grids);

        // TODO: Maybe this should be a SmallVec, to avoid allocating in the common case of very few PowerGrids
        let mut stores: (
            Vec<[&mut [ItemStorage<IronOre>]; NUM_RECIPES]>,
            Vec<[&mut [ItemStorage<CopperOre>]; NUM_RECIPES]>,
        ) = self
            .power_grids
            .iter_mut()
            .map(|pg| {
                (
                    [pg.stores.iron_plates.get_inputs_1_mut()],
                    [pg.stores.copper_plates.store.get_inputs_1_mut()],
                )
            })
            .collect();

        let mut storage = StorageStore {
            iron_ore: stores.0.as_mut_slice(),
            copper_ore: stores.1.as_mut_slice(),
        };

        self.belts.update(&mut storage);
    }

    fn build_storage_store(power_grids: &mut Vec<PowerGrid>) -> StorageStore {
        todo!()
    }
}

impl BeltStore {
    fn update(&mut self, storages: &mut StorageStore) {
        rayon::join(
            || {
                self.iron_ore_belts.iter_mut().for_each(|b| {
                    b.update();
                    b.update_inserters(&mut storages.iron_ore);
                });
            },
            || {
                self.copper_ore_belt.iter_mut().for_each(|b| {
                    b.update();
                    b.update_inserters(&mut storages.copper_ore);
                    // Hm, at this point the storages will be in cache. By updating all belts first, we might flush it out of the cache :/
                });
            },
        );
    }
}
