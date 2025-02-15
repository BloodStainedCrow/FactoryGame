use std::{
    collections::{BTreeSet, HashSet},
    iter,
    mem::{self},
};

use crate::{
    belt::{
        belt::Belt,
        smart::{EmptyBelt, Side, SmartBelt},
    },
    data::DataStore,
    frontend::{
        action::{action_state_machine::ActionStateMachine, set_recipe::SetRecipeInfo, ActionType},
        world::{
            tile::{AssemblerID, BeltId, BeltTileId, Entity, World, BELT_LEN_PER_TILE},
            Position,
        },
    },
    item::IdxTrait,
    power::{PowerGrid, Watt},
    research::TechState,
};
use log::warn;
use rayon::iter::{IndexedParallelIterator, IntoParallelRefMutIterator, ParallelIterator};
use tilelib::types::Renderer;

use super::{render_world::render_world, TextureAtlas};

use crate::frontend::action::place_tile::PositionInfo;

#[derive(Debug, serde::Deserialize, serde::Serialize)]
pub struct GameState<ItemIdxType: IdxTrait, RecipeIdxType: IdxTrait> {
    pub world: World<ItemIdxType, RecipeIdxType>,
    pub simulation_state: SimulationState<RecipeIdxType>,
}

#[derive(Debug, serde::Deserialize, serde::Serialize)]
pub struct SimulationState<RecipeIdxType: IdxTrait> {
    tech_state: TechState,
    pub factory: Factory<RecipeIdxType>,
    // TODO:
}

impl<RecipeIdxType: IdxTrait> SimulationState<RecipeIdxType> {
    pub fn new<ItemIdxType: IdxTrait>(data_store: &DataStore<ItemIdxType, RecipeIdxType>) -> Self {
        Self {
            tech_state: TechState::default(),
            factory: Factory::new(data_store),
        }
    }
}

#[derive(Debug, serde::Deserialize, serde::Serialize)]
pub struct Factory<RecipeIdxType: IdxTrait> {
    pub power_grids: Vec<PowerGrid<RecipeIdxType>>,
    pub belts: BeltStore<RecipeIdxType>,
}

impl<RecipeIdxType: IdxTrait> Factory<RecipeIdxType> {
    fn new<ItemIdxType: IdxTrait>(data_store: &DataStore<ItemIdxType, RecipeIdxType>) -> Self {
        Self {
            power_grids: vec![PowerGrid::new(data_store)],
            belts: BeltStore {
                empty_belts: vec![],
                empty_belt_holes: vec![],
                belts: vec![MultiBeltStore::default(); data_store.recipe_timers.len()]
                    .into_boxed_slice(),
            },
        }
    }
}

#[derive(Debug, serde::Deserialize, serde::Serialize)]
pub struct BeltStore<RecipeIdxType: IdxTrait> {
    pub empty_belts: Vec<EmptyBelt>,
    pub empty_belt_holes: Vec<usize>,

    pub belts: Box<[MultiBeltStore<RecipeIdxType>]>,
}

#[derive(Debug, Clone, serde::Deserialize, serde::Serialize)]
pub struct MultiBeltStore<RecipeIdxType: IdxTrait> {
    pub belts: Vec<SmartBelt<RecipeIdxType>>,

    pub holes: Vec<usize>,
}

impl<RecipeIdxType: IdxTrait> Default for MultiBeltStore<RecipeIdxType> {
    fn default() -> Self {
        Self {
            belts: vec![],
            holes: vec![],
        }
    }
}

pub enum AppState<ItemIdxType: IdxTrait, RecipeIdxType: IdxTrait> {
    Ingame(GameState<ItemIdxType, RecipeIdxType>),
}

impl<ItemIdxType: IdxTrait, RecipeIdxType: IdxTrait> AppState<ItemIdxType, RecipeIdxType> {
    pub fn render(
        &self,
        renderer: &mut Renderer,
        texture_atlas: &TextureAtlas,
        state_machine: &ActionStateMachine<ItemIdxType>,
        data_store: &DataStore<ItemIdxType, RecipeIdxType>,
    ) {
        match self {
            Self::Ingame(game) => {
                render_world(
                    renderer,
                    game,
                    texture_atlas,
                    game.world.player_pos,
                    state_machine,
                    data_store,
                );
            },
        }
    }
}

impl<ItemIdxType: IdxTrait, RecipeIdxType: IdxTrait> GameState<ItemIdxType, RecipeIdxType> {
    #[allow(clippy::too_many_lines)]
    pub fn apply_actions(
        &mut self,
        actions: impl IntoIterator<Item = ActionType<ItemIdxType, RecipeIdxType>>,
        data_store: &DataStore<ItemIdxType, RecipeIdxType>,
    ) {
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
                            // TODO: get size dynamically
                            if !self.world.can_fit(position, (3, 3)) {
                                warn!("Tried to place assembler where it does not fit");
                                continue;
                            }

                            let chunk = self.world.get_chunk_for_tile_mut(position);

                            if let Some(chunk) = chunk {
                                let power_grid_idx = 0;

                                // We do NOT store assemblers without recipes in the simulation state!

                                chunk.add_entity(
                                    crate::frontend::world::tile::Entity::AssemblerWithoutRecipe {
                                        pos: position,
                                    },
                                );
                            } else {
                                warn!(
                                    "Tried to place assember outside world/in ungenerated chunk!"
                                );
                            }
                        },
                        crate::frontend::world::tile::PlaceEntityType::Inserter {
                            pos,
                            dir,
                            filter,
                        } => {
                            enum InserterStartInfo<ItemIdxType: IdxTrait, RecipeIdxType: IdxTrait> {
                                Belt(BeltTileId<ItemIdxType>, u16),
                                Assembler(AssemblerID<RecipeIdxType>),
                            }

                            if !self.world.can_fit(pos, (1, 1)) {
                                warn!("Tried to place inserter where it does not fit");
                                continue;
                            }

                            let start_pos = Position {
                                x: pos
                                    .x
                                    .checked_add_signed(dir.reverse().into_offset().0.into())
                                    .unwrap(),
                                y: pos
                                    .y
                                    .checked_add_signed(dir.reverse().into_offset().1.into())
                                    .unwrap(),
                            };
                            let end_pos = Position {
                                x: pos
                                    .x
                                    .checked_add_signed(dir.into_offset().0.into())
                                    .unwrap(),
                                y: pos
                                    .y
                                    .checked_add_signed(dir.into_offset().1.into())
                                    .unwrap(),
                            };

                            let start_chunk = self.world.get_chunk_for_tile(start_pos);

                            #[allow(clippy::option_if_let_else)]
                            let start_info = if let Some(chunk) = start_chunk {
                                if let Some(start_entity) = chunk.get_entity_at(start_pos) {
                                    match start_entity {
                                        Entity::Assembler { pos, id } => {
                                            Some(InserterStartInfo::Assembler(*id))
                                        },
                                        Entity::PowerPole { .. } => {
                                            warn!(
                                                "Tried to place inserter starting on on PowerPole"
                                            );
                                            None
                                        },
                                        Entity::Belt { id, belt_pos, .. } => {
                                            Some(InserterStartInfo::Belt(*id, *belt_pos))
                                        },
                                        Entity::AssemblerWithoutRecipe { pos } => todo!(),
                                        Entity::Inserter { .. }
                                        | Entity::UnconnectedInserter { .. } => {
                                            warn!("Tried to place inserter starting on another Inserter");
                                            None
                                        },
                                    }
                                } else {
                                    warn!("Tried to place inserter starting on nothing!");
                                    None
                                }
                            } else {
                                warn!(
                                    "Tried to place inserter outside world/in ungenerated chunk!"
                                );
                                None
                            };

                            if let Some(start_info) = start_info {
                                let end_chunk = self.world.get_chunk_for_tile_mut(end_pos);

                                if let Some(chunk) = end_chunk {
                                    let end_entity = chunk.get_entity_at(end_pos);
                                    let end_entity = end_entity.copied();
                                    if let Some(end_entity) = end_entity {
                                        match end_entity {
                                            Entity::Assembler { id, .. } => {
                                                match start_info {
                                                    InserterStartInfo::Belt(
                                                        belt_tile_id,
                                                        belt_pos,
                                                    ) => {
                                                        match belt_tile_id {
                                                            BeltTileId::EmptyBeltId(idx) => {
                                                                // We are inserting something on an empty belt!
                                                                // It should no longer be empty!

                                                                // Ensure that the assembler can take this item
                                                                let Ok(storage) = data_store
                                                                    .get_storage_id_for_assembler(
                                                                    crate::data::ItemRecipeDir::Ing,
                                                                    filter,
                                                                    id,
                                                                ) else {
                                                                    warn!("This assembler does not require {:?}", filter);
                                                                    continue;
                                                                };

                                                                // Remove the old empty belt
                                                                let mut empty_belt =
                                                                    EmptyBelt::new(0);

                                                                mem::swap(
                                                                    &mut empty_belt,
                                                                    &mut self
                                                                        .simulation_state
                                                                        .factory
                                                                        .belts
                                                                        .empty_belts[idx],
                                                                );

                                                                self.simulation_state
                                                                    .factory
                                                                    .belts
                                                                    .empty_belt_holes
                                                                    .push(idx);

                                                                let mut instantiated_belt =
                                                                    empty_belt.into_smart_belt();

                                                                // Add the inserter to the belt
                                                                instantiated_belt
                                                                    .add_out_inserter(
                                                                        belt_pos - 1,
                                                                        storage,
                                                                    )
                                                                    .unwrap();

                                                                let new_item_id: usize =
                                                                    filter.id.into();

                                                                let hole_to_use = self
                                                                    .simulation_state
                                                                    .factory
                                                                    .belts
                                                                    .belts[new_item_id]
                                                                    .holes
                                                                    .pop();

                                                                let new_id = if let Some(idx) =
                                                                    hole_to_use
                                                                {
                                                                    mem::swap(
                                                                        &mut instantiated_belt,
                                                                        &mut self
                                                                            .simulation_state
                                                                            .factory
                                                                            .belts
                                                                            .belts[new_item_id]
                                                                            .belts[idx],
                                                                    );
                                                                    mem::drop(instantiated_belt);

                                                                    BeltTileId::BeltId(BeltId {
                                                                        item: filter,
                                                                        index: idx,
                                                                    })
                                                                } else {
                                                                    self.simulation_state
                                                                        .factory
                                                                        .belts
                                                                        .belts[new_item_id]
                                                                        .belts
                                                                        .push(instantiated_belt);

                                                                    BeltTileId::BeltId(BeltId {
                                                                        item: filter,
                                                                        index: self
                                                                            .simulation_state
                                                                            .factory
                                                                            .belts
                                                                            .belts[new_item_id]
                                                                            .belts
                                                                            .len()
                                                                            - 1,
                                                                    })
                                                                };

                                                                // Add the inserter entity to the map
                                                                chunk.add_entity(
                                                                    Entity::Inserter {
                                                                        pos,
                                                                        direction: dir,
                                                                        id: new_id,
                                                                        belt_pos,
                                                                    },
                                                                );

                                                                let mut visited = BTreeSet::new();
                                                                let mut to_visit = BTreeSet::new();
                                                                to_visit.insert((
                                                                    pos.x / 16,
                                                                    pos.y / 16,
                                                                ));

                                                                // recursively update all chunks with this belt
                                                                while let Some(chunk_coord) =
                                                                    to_visit.pop_first()
                                                                {
                                                                    dbg!(chunk_coord);
                                                                    let chunk = self.world.get_chunk_mut(chunk_coord).expect("Ungenerated chunk pointed to by belt?!?");

                                                                    let new_chunks = chunk
                                                                        .change_belt_id(
                                                                            belt_tile_id,
                                                                            new_id,
                                                                        )
                                                                        .map(|i| i.into_iter())
                                                                        .into_iter()
                                                                        .flatten();

                                                                    visited.insert(chunk_coord);

                                                                    to_visit.extend(
                                                                        // Add all chunks, which were not previously visited, to the worklist
                                                                        new_chunks
                                                                            .into_iter()
                                                                            .filter(|c| {
                                                                                !visited.contains(c)
                                                                            }),
                                                                    );
                                                                }
                                                            },
                                                            BeltTileId::BeltId(belt_id) => {
                                                                assert_eq!(belt_id.item, filter);
                                                                let item_id: usize =
                                                                    filter.id.into();
                                                                let belt = &mut self
                                                                    .simulation_state
                                                                    .factory
                                                                    .belts
                                                                    .belts[item_id]
                                                                    .belts[belt_id.index];

                                                                let Ok(storage) = data_store
                                                                    .get_storage_id_for_assembler(
                                                                    crate::data::ItemRecipeDir::Ing,
                                                                    filter,
                                                                    id,
                                                                ) else {
                                                                    warn!("This assembler does not require {:?}", filter);
                                                                    continue;
                                                                };

                                                                // Add the inserter to the belt
                                                                belt.add_out_inserter(
                                                                    belt_pos - 1,
                                                                    storage,
                                                                )
                                                                .unwrap();

                                                                // Add the inserter entity to the map
                                                                chunk.add_entity(
                                                                    Entity::Inserter {
                                                                        pos,
                                                                        direction: dir,
                                                                        id: belt_tile_id,
                                                                        belt_pos,
                                                                    },
                                                                );
                                                            },
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
                                            Entity::Belt {
                                                id: belt_tile_id,
                                                belt_pos,
                                                ..
                                            } => match start_info {
                                                InserterStartInfo::Belt(belt_id, belt_pos) => {
                                                    todo!()
                                                },
                                                InserterStartInfo::Assembler(assembler_id) => {
                                                    match belt_tile_id {
                                                        BeltTileId::EmptyBeltId(idx) => {
                                                            // We are removing something from an empty belt!
                                                            // It should no longer be empty!

                                                            let Ok(storage) = data_store
                                                                .get_storage_id_for_assembler(
                                                                    crate::data::ItemRecipeDir::Out,
                                                                    filter,
                                                                    assembler_id,
                                                                )
                                                            else {
                                                                warn!("This assembler cannot output {:?}", filter);
                                                                continue;
                                                            };

                                                            // Remove the old empty belt
                                                            let mut empty_belt = EmptyBelt::new(0);

                                                            mem::swap(
                                                                &mut empty_belt,
                                                                &mut self
                                                                    .simulation_state
                                                                    .factory
                                                                    .belts
                                                                    .empty_belts[idx],
                                                            );

                                                            self.simulation_state
                                                                .factory
                                                                .belts
                                                                .empty_belt_holes
                                                                .push(idx);

                                                            let mut instantiated_belt =
                                                                empty_belt.into_smart_belt();

                                                            // Add the inserter to the belt
                                                            instantiated_belt
                                                                .add_in_inserter(
                                                                    belt_pos - 1,
                                                                    storage,
                                                                )
                                                                .unwrap();

                                                            let new_item_id: usize =
                                                                filter.id.into();

                                                            let hole_to_use = self
                                                                .simulation_state
                                                                .factory
                                                                .belts
                                                                .belts[new_item_id]
                                                                .holes
                                                                .pop();

                                                            let new_id =
                                                                if let Some(idx) = hole_to_use {
                                                                    mem::swap(
                                                                        &mut instantiated_belt,
                                                                        &mut self
                                                                            .simulation_state
                                                                            .factory
                                                                            .belts
                                                                            .belts[new_item_id]
                                                                            .belts[idx],
                                                                    );
                                                                    mem::drop(instantiated_belt);

                                                                    BeltTileId::BeltId(BeltId {
                                                                        item: filter,
                                                                        index: idx,
                                                                    })
                                                                } else {
                                                                    self.simulation_state
                                                                        .factory
                                                                        .belts
                                                                        .belts[new_item_id]
                                                                        .belts
                                                                        .push(instantiated_belt);

                                                                    BeltTileId::BeltId(BeltId {
                                                                        item: filter,
                                                                        index: self
                                                                            .simulation_state
                                                                            .factory
                                                                            .belts
                                                                            .belts[new_item_id]
                                                                            .belts
                                                                            .len()
                                                                            - 1,
                                                                    })
                                                                };

                                                            // Add the inserter entity to the map
                                                            chunk.add_entity(Entity::Inserter {
                                                                pos,
                                                                direction: dir,
                                                                id: new_id,
                                                                belt_pos: belt_pos - 1,
                                                            });

                                                            let mut visited = BTreeSet::new();
                                                            let mut to_visit = BTreeSet::new();
                                                            to_visit
                                                                .insert((pos.x / 16, pos.y / 16));

                                                            // recursively update all chunks with this belt
                                                            while let Some(chunk_coord) =
                                                                to_visit.pop_first()
                                                            {
                                                                dbg!(chunk_coord);
                                                                let chunk = self.world.get_chunk_mut(chunk_coord).expect("Ungenerated chunk pointed to by belt?!?");

                                                                let new_chunks = chunk
                                                                    .change_belt_id(
                                                                        belt_tile_id,
                                                                        new_id,
                                                                    )
                                                                    .map(|i| i.into_iter())
                                                                    .into_iter()
                                                                    .flatten();

                                                                visited.insert(chunk_coord);

                                                                to_visit.extend(
                                                                    // Add all chunks, which were not previously visited, to the worklist
                                                                    new_chunks.into_iter().filter(
                                                                        |c| !visited.contains(c),
                                                                    ),
                                                                );
                                                            }
                                                        },
                                                        BeltTileId::BeltId(belt_id) => {
                                                            if belt_id.item != filter {
                                                                // TODO: Try to downgrade the belt if it is empty
                                                                warn!("You cannot connect an inserter to a belt, which carries an item different from its filter");
                                                                continue;
                                                            }
                                                            let item_id: usize = filter.id.into();
                                                            let belt = &mut self
                                                                .simulation_state
                                                                .factory
                                                                .belts
                                                                .belts[item_id]
                                                                .belts[belt_id.index];

                                                            let Ok(storage) = data_store
                                                                .get_storage_id_for_assembler(
                                                                    crate::data::ItemRecipeDir::Out,
                                                                    filter,
                                                                    assembler_id,
                                                                )
                                                            else {
                                                                warn!("This assembler cannot output {:?}", filter);
                                                                continue;
                                                            };

                                                            // Add the inserter to the belt
                                                            belt.add_in_inserter(
                                                                belt_pos - 1,
                                                                storage,
                                                            )
                                                            .unwrap();

                                                            // Add the inserter entity to the map
                                                            chunk.add_entity(Entity::Inserter {
                                                                pos,
                                                                direction: dir,
                                                                id: belt_tile_id,
                                                                belt_pos: belt_pos - 1,
                                                            });
                                                        },
                                                    }
                                                },
                                            },
                                            Entity::AssemblerWithoutRecipe { pos } => todo!(),
                                            Entity::Inserter {
                                                pos,
                                                direction,
                                                id,
                                                belt_pos,
                                            } => todo!(),
                                            Entity::UnconnectedInserter { pos, direction } => {
                                                todo!()
                                            },
                                        }
                                    } else {
                                        warn!("Tried to place inserter ending on nothing!");
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

                            if !self.world.can_fit(pos, (1, 1)) {
                                warn!("Tried to place belt where it does not fit");
                                continue;
                            }

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
                                    let chunk = self.world.get_chunk_for_tile_mut(pos).unwrap();

                                    let idx = self.simulation_state.factory.belts.empty_belts.len();
                                    self.simulation_state
                                        .factory
                                        .belts
                                        .empty_belts
                                        .push(new_belt);

                                    chunk.add_entity(Entity::Belt {
                                        pos,
                                        direction,
                                        id: BeltTileId::EmptyBeltId(idx),
                                        belt_pos: BELT_LEN_PER_TILE,
                                    });
                                },
                                (None, Some(back_id)) => {
                                    let chunk = self.world.get_chunk_for_tile_mut(pos).unwrap();

                                    // Merge back with this, it will get back_id
                                    let new_len = match back_id {
                                        BeltTileId::EmptyBeltId(idx) => {
                                            take_mut::take(
                                                &mut self
                                                    .simulation_state
                                                    .factory
                                                    .belts
                                                    .empty_belts[idx],
                                                |back_belt| EmptyBelt::join(new_belt, back_belt),
                                            );

                                            self.simulation_state.factory.belts.empty_belts[idx].len
                                        },
                                        BeltTileId::BeltId(belt_id) => {
                                            let belt_item_id: usize = belt_id.item.id.into();
                                            let back_belt =
                                                self.simulation_state.factory.belts.belts
                                                    [belt_item_id]
                                                    .belts
                                                    .get_mut(belt_id.index)
                                                    .expect("id from world not in simulation!!");

                                            take_mut::take(back_belt, |back_belt| {
                                                back_belt.join_with_empty(
                                                    EmptyBelt::new(BELT_LEN_PER_TILE),
                                                    Side::FRONT,
                                                )
                                            });

                                            back_belt.get_len()
                                        },
                                    };

                                    // Add the belt. Its belt_pos will be increased to the correct value of BELT_LEN_PER_TILE in the next step
                                    chunk.add_entity(Entity::Belt {
                                        pos,
                                        direction,
                                        id: back_id,

                                        belt_pos: 0,
                                    });

                                    let mut visited = BTreeSet::new();
                                    let mut to_visit = BTreeSet::new();
                                    to_visit.insert((pos.x / 16, pos.y / 16));

                                    // recursively update all chunks with this belt
                                    while let Some(chunk_coord) = to_visit.pop_first() {
                                        dbg!(chunk_coord);

                                        let chunk = self
                                            .world
                                            .get_chunk_mut(chunk_coord)
                                            .expect("Ungenerated chunk pointed to by belt?!?");

                                        let new_chunks = chunk
                                            .increase_all_belt_pos(back_id, BELT_LEN_PER_TILE)
                                            .map(|i| i.into_iter())
                                            .into_iter()
                                            .flatten();

                                        visited.insert(chunk_coord);

                                        to_visit.extend(
                                            // Add all chunks, which were not previously visited, to the worklist
                                            new_chunks.into_iter().filter(|c| !visited.contains(c)),
                                        );
                                    }
                                },
                                (Some(front_id), None) => {
                                    let chunk = self.world.get_chunk_for_tile_mut(pos).unwrap();

                                    // Merge front with this, it will get front_id
                                    let new_len = match front_id {
                                        BeltTileId::EmptyBeltId(idx) => {
                                            take_mut::take(
                                                &mut self
                                                    .simulation_state
                                                    .factory
                                                    .belts
                                                    .empty_belts[idx],
                                                |front_belt| EmptyBelt::join(front_belt, new_belt),
                                            );

                                            self.simulation_state.factory.belts.empty_belts[idx].len
                                        },
                                        BeltTileId::BeltId(belt_id) => {
                                            let belt_item_id: usize = belt_id.item.id.into();
                                            let front_belt =
                                                self.simulation_state.factory.belts.belts
                                                    [belt_item_id]
                                                    .belts
                                                    .get_mut(belt_id.index)
                                                    .expect("id from world not in simulation!!");

                                            take_mut::take(front_belt, |front_belt| {
                                                front_belt.join_with_empty(
                                                    EmptyBelt::new(BELT_LEN_PER_TILE),
                                                    Side::BACK,
                                                )
                                            });

                                            front_belt.get_len()
                                        },
                                    };

                                    dbg!(new_len);

                                    chunk.add_entity(dbg!(Entity::Belt {
                                        pos,
                                        direction,
                                        id: front_id,

                                        belt_pos: (new_len),
                                    }));
                                },
                                (Some(front_tile_id), Some(back_tile_id)) => {
                                    let chunk = self.world.get_chunk_for_tile_mut(pos).unwrap();

                                    let (front_len, removed_id, new_id) = match (
                                        front_tile_id,
                                        back_tile_id,
                                    ) {
                                        (
                                            BeltTileId::EmptyBeltId(front_idx),
                                            BeltTileId::EmptyBeltId(back_idx),
                                        ) => {
                                            // Remove back belt from simulation
                                            let mut back_belt = EmptyBelt::new(0);

                                            mem::swap(
                                                &mut back_belt,
                                                &mut self
                                                    .simulation_state
                                                    .factory
                                                    .belts
                                                    .empty_belts[back_idx],
                                            );

                                            let back_len = back_belt.len;

                                            self.simulation_state
                                                .factory
                                                .belts
                                                .empty_belt_holes
                                                .push(back_idx);

                                            // Get ref to front belt
                                            let front_belt = self
                                                .simulation_state
                                                .factory
                                                .belts
                                                .empty_belts
                                                .get_mut(front_idx)
                                                .expect("id from world not in simulation!!");

                                            let front_len = front_belt.len;

                                            // Merge front and middle and back
                                            take_mut::take(front_belt, |front_belt| {
                                                EmptyBelt::join(
                                                    EmptyBelt::join(front_belt, new_belt),
                                                    back_belt,
                                                )
                                            });

                                            (front_len, back_tile_id, front_tile_id)
                                        },
                                        (
                                            BeltTileId::EmptyBeltId(front_idx),
                                            BeltTileId::BeltId(back_id),
                                        ) => {
                                            // Remove front belt from simulation
                                            let mut front_belt = EmptyBelt::new(0);

                                            mem::swap(
                                                &mut front_belt,
                                                &mut self
                                                    .simulation_state
                                                    .factory
                                                    .belts
                                                    .empty_belts[front_idx],
                                            );

                                            let back_item_id: usize = back_id.item.id.into();

                                            // Get ref to back belt
                                            let back_belt =
                                                self.simulation_state.factory.belts.belts
                                                    [back_item_id]
                                                    .belts
                                                    .get_mut(back_id.index)
                                                    .expect("id from world not in simulation!!");

                                            let back_len = back_belt.get_len();

                                            let front_len = front_belt.len;

                                            // Merge front and middle and back
                                            take_mut::take(back_belt, |back_belt| {
                                                back_belt
                                                    .join_with_empty(new_belt, Side::FRONT)
                                                    .join_with_empty(front_belt, Side::FRONT)
                                            });

                                            (front_len, front_tile_id, back_tile_id)
                                        },
                                        (
                                            BeltTileId::BeltId(front_id),
                                            BeltTileId::EmptyBeltId(back_idx),
                                        ) => {
                                            // Remove back belt from simulation
                                            let mut back_belt = EmptyBelt::new(0);

                                            mem::swap(
                                                &mut back_belt,
                                                &mut self
                                                    .simulation_state
                                                    .factory
                                                    .belts
                                                    .empty_belts[back_idx],
                                            );

                                            let back_len = back_belt.len;

                                            self.simulation_state
                                                .factory
                                                .belts
                                                .empty_belt_holes
                                                .push(back_idx);

                                            let front_item_id: usize = front_id.item.id.into();

                                            // Get ref to front belt
                                            let front_belt =
                                                self.simulation_state.factory.belts.belts
                                                    [front_item_id]
                                                    .belts
                                                    .get_mut(front_id.index)
                                                    .expect("id from world not in simulation!!");

                                            let front_len = front_belt.get_len();

                                            // Merge front and middle and back
                                            take_mut::take(front_belt, |front_belt| {
                                                front_belt
                                                    .join_with_empty(new_belt, Side::BACK)
                                                    .join_with_empty(back_belt, Side::BACK)
                                            });

                                            (front_len, back_tile_id, front_tile_id)
                                        },
                                        (
                                            BeltTileId::BeltId(front_id),
                                            BeltTileId::BeltId(back_id),
                                        ) => {
                                            // Remove back belt from simulation
                                            let mut back_belt = SmartBelt::new(0);
                                            let back_item_id: usize = back_id.item.id.into();
                                            let front_item_id: usize = front_id.item.id.into();

                                            if front_item_id != back_item_id {
                                                // TODO: Try to downgrade the belt if it is empty
                                                warn!("Two belts filled with different items cannot be connected");
                                                continue;
                                            }

                                            mem::swap(
                                                &mut back_belt,
                                                &mut self.simulation_state.factory.belts.belts
                                                    [back_item_id]
                                                    .belts[back_id.index],
                                            );

                                            self.simulation_state.factory.belts.belts[back_item_id]
                                                .holes
                                                .push(back_id.index);

                                            // Get ref to front belt
                                            let front_belt =
                                                self.simulation_state.factory.belts.belts
                                                    [front_item_id]
                                                    .belts
                                                    .get_mut(front_id.index)
                                                    .expect("id from world not in simulation!!");

                                            let front_len = front_belt.get_len();

                                            // Merge front and middle and back
                                            take_mut::take(front_belt, |front_belt| {
                                                SmartBelt::join(
                                                    front_belt
                                                        .join_with_empty(new_belt, Side::BACK),
                                                    back_belt,
                                                )
                                            });

                                            (front_len, back_tile_id, front_tile_id)
                                        },
                                    };

                                    // TODO: Also do this recursively!
                                    chunk.increase_all_belt_pos(
                                        back_tile_id,
                                        front_len + BELT_LEN_PER_TILE,
                                    );

                                    chunk.add_entity(Entity::Belt {
                                        pos,
                                        direction,
                                        id: new_id,

                                        belt_pos: front_len + BELT_LEN_PER_TILE,
                                    });

                                    // TODO: Also do this recursively!
                                    chunk.change_belt_id(removed_id, new_id);
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
                ActionType::SetRecipe(SetRecipeInfo { pos, recipe }) => {
                    let chunk = self.world.get_chunk_for_tile_mut(pos);

                    if let Some(chunk) = chunk {
                        let assembler = chunk.get_entity_at_mut(pos);

                        if let Some(entity) = assembler {
                            if let Entity::Assembler { pos: _pos, id } = entity {
                                let old_recipe_id = id.recipe.id.into();
                                let new_recipe_id = recipe.id.into();
                                match (
                                    data_store.recipe_num_ing_lookup[old_recipe_id],
                                    data_store.recipe_num_out_lookup[old_recipe_id],
                                ) {
                                    (0, 1) => {
                                        let old_assembler =
                                            self.simulation_state.factory.power_grids
                                                [id.grid as usize]
                                                .stores
                                                .assemblers_0_1[data_store
                                                .recipe_to_ing_out_combo_idx[old_recipe_id]]
                                                .remove_assembler(id.assembler_index as usize);

                                        // TODO: Add the old_assembler_items to a players inventory or something
                                    },

                                    _ => unreachable!(),
                                };

                                let new_id = match (
                                    data_store.recipe_num_ing_lookup[new_recipe_id],
                                    data_store.recipe_num_out_lookup[new_recipe_id],
                                ) {
                                    (0, 1) => {
                                        let new_idx = self.simulation_state.factory.power_grids
                                            [id.grid as usize]
                                            .stores
                                            .assemblers_0_1
                                            [data_store.recipe_to_ing_out_combo_idx[new_recipe_id]]
                                            .add_assembler();

                                        AssemblerID {
                                            recipe,
                                            grid: id.grid,
                                            assembler_index: new_idx
                                                .try_into()
                                                .expect("More than u16::MAX assemblers"),
                                        }
                                    },

                                    _ => unreachable!(),
                                };

                                *id = new_id;
                                // TODO: Update surrounding inserters!!!!
                            } else if let Entity::AssemblerWithoutRecipe { pos } = entity {
                                let new_recipe_id = recipe.id.into();

                                // TODO: Evaluate PowerGrid surroundings!!
                                let power_grid_id = 0;

                                let new_id = match (
                                    data_store.recipe_num_ing_lookup[new_recipe_id],
                                    data_store.recipe_num_out_lookup[new_recipe_id],
                                ) {
                                    (0, 1) => {
                                        let new_idx = self.simulation_state.factory.power_grids
                                            [power_grid_id as usize]
                                            .stores
                                            .assemblers_0_1
                                            [data_store.recipe_to_ing_out_combo_idx[new_recipe_id]]
                                            .add_assembler();

                                        AssemblerID {
                                            recipe,
                                            grid: power_grid_id,
                                            assembler_index: new_idx
                                                .try_into()
                                                .expect("More than u16::MAX assemblers"),
                                        }
                                    },

                                    _ => unreachable!(),
                                };

                                *entity = Entity::Assembler {
                                    pos: *pos,
                                    id: new_id,
                                }
                            } else {
                                warn!("Tried to change recipe where there was no assembler!");
                            }
                        }
                    } else {
                        warn!("Tried to change assembler recipe outside world!");
                    }
                },
            }
        }
    }

    pub fn update(&mut self, data_store: &DataStore<ItemIdxType, RecipeIdxType>) {
        self.world.player_pos.0 += self.world.player_move.0 / 60.0;
        self.world.player_pos.1 += self.world.player_move.1 / 60.0;

        self.simulation_state.factory.belts.belt_update(
            &mut self.simulation_state.factory.power_grids[0],
            data_store,
        );

        let tech_progress = self
            .simulation_state
            .factory
            .power_grids
            .par_iter_mut()
            .map(|grid| grid.update(Watt(1000), &self.simulation_state.tech_state, data_store))
            .sum();

        self.simulation_state
            .tech_state
            .apply_progress(tech_progress);
    }
}

impl<RecipeIdxType: IdxTrait> Factory<RecipeIdxType> {
    // fn build_storage_store(power_grids: &mut Vec<PowerGrid>) -> StorageStore {
    //     todo!()
    // }
}

impl<RecipeIdxType: IdxTrait> BeltStore<RecipeIdxType> {
    fn belt_update<ItemIdxType: IdxTrait>(
        &mut self,
        grid: &mut PowerGrid<RecipeIdxType>,
        data_store: &DataStore<ItemIdxType, RecipeIdxType>,
    ) {
        // NOTE: This has to be assembled in the same way the lookup is generated in DataStore
        //       Currently this means assemblers -> labs -> TODO
        // TODO: This is very fragile :/
        let all_storages = grid
            .stores
            .assemblers_0_1
            .iter_mut()
            .flat_map(|store| iter::once(store.get_outputs_mut(0)))
            // TODO: Chain the other storages here
            .chain(
                grid.lab_stores
                    .sciences
                    .iter_mut()
                    .map(std::vec::Vec::as_mut_slice),
            );

        // Sort the slices by item
        let storages_by_item = all_storages
            .zip(
                data_store
                    .recipe_item_store_to_item
                    .iter()
                    .chain(data_store.science_bottle_items.iter()),
            )
            .fold(Vec::new(), |mut acc, (storage, item)| {
                let old = acc.get_mut(item.id.into());

                match old {
                    None => {
                        acc.resize_with(item.id.into() + 1, Vec::new);
                        acc.get_mut(item.id.into()).unwrap().push(storage);
                    },
                    Some(v) => v.push(storage),
                }

                acc
            });

        self.belts.par_iter_mut().zip(storages_by_item).for_each(
            |(belt_store, mut assembler_item_storages)| {
                for belt in &mut belt_store.belts {
                    belt.update();
                    belt.update_inserters(&mut assembler_item_storages);
                }
            },
        );
    }
}
