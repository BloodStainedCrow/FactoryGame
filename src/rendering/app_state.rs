use std::{
    collections::BTreeSet, fs::File, io::Write, mem::{self}, ops::ControlFlow
};

use crate::{
    belt::{
        belt::Belt,
        smart::{EmptyBelt, Side, SmartBelt}, BeltStore, MultiBeltStore,
    },
    data::{DataStore, ItemRecipeDir},
    frontend::{
        action::{action_state_machine::ActionStateMachine, set_recipe::SetRecipeInfo, ActionType},
        world::{
            tile::{
                AssemblerID, AssemblerInfo, AttachedInserter, BeltId, BeltTileId, Dir, Entity,
                InserterInfo, World, BELT_LEN_PER_TILE,
            },
            Position,
        },
    },
    item::{IdxTrait, Item, Recipe},
    power::{power_grid::{all_storages, PowerGrid, PowerGridIdentifier}, PowerGridStorage, Watt},
    research::{ResearchProgress, TechState}, statistics::{production::ProductionInfo, recipe::RecipeTickInfo, research::ResearchInfo, Statistics},
};
use log::{error, info, warn};
use rayon::iter::{IndexedParallelIterator, IntoParallelRefMutIterator, ParallelIterator};
use tilelib::types::Renderer;

use super::{render_world::render_world, TextureAtlas};

use crate::frontend::action::place_tile::PositionInfo;

#[derive(Debug, Clone, serde::Deserialize, serde::Serialize)]
pub struct GameState<ItemIdxType: IdxTrait, RecipeIdxType: IdxTrait> {
    pub world: World<ItemIdxType, RecipeIdxType>,
    pub simulation_state: SimulationState<RecipeIdxType>,

    statistics: Statistics,
}

impl<ItemIdxType: IdxTrait, RecipeIdxType: IdxTrait> GameState<ItemIdxType, RecipeIdxType> {
    pub fn new(data_store: &DataStore<ItemIdxType, RecipeIdxType>) -> Self {
        Self {
            world: World::new(),
            simulation_state: SimulationState::new(data_store),
            statistics: Statistics::new(data_store),
        }
    }
}

#[derive(Debug, Clone, serde::Deserialize, serde::Serialize)]
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

#[derive(Debug, Clone, serde::Deserialize, serde::Serialize)]
pub struct Factory<RecipeIdxType: IdxTrait> {
    pub power_grids: PowerGridStorage<RecipeIdxType>,
    pub belts: BeltStore<RecipeIdxType>,
}

impl<RecipeIdxType: IdxTrait> Factory<RecipeIdxType> {
    fn new<ItemIdxType: IdxTrait>(data_store: &DataStore<ItemIdxType, RecipeIdxType>) -> Self {
        Self {
            power_grids: PowerGridStorage::new(),
            belts: BeltStore {
                empty_belts: vec![],
                empty_belt_holes: vec![],
                belts: vec![MultiBeltStore::default(); data_store.recipe_timers.len()]
                    .into_boxed_slice(),
            },
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

#[derive(Debug, Clone, Copy)]
enum InserterUpdateInfo {
    NewAssembler {
        pos: Position,
        size: (u8, u8),
    },
    NewBelt {
        pos: Position,
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
                                    self.world.set_floor_tile(Position { x, y }, place_floor_tile_by_hand_info.ghost_info.tile);
                                }
                            }
                        },
                        PositionInfo::Single { pos } => {
                            self.world.set_floor_tile(pos, place_floor_tile_by_hand_info.ghost_info.tile);
                        },
                        PositionInfo::List { positions } => {
                            for pos in positions {
                                self.world.set_floor_tile(pos, place_floor_tile_by_hand_info.ghost_info.tile);
                            }
                        },
                    }
                },
                ActionType::PlaceEntity(place_entity_info) => match place_entity_info.entities {
                    crate::frontend::action::place_entity::EntityPlaceOptions::Single(
                        place_entity_type,
                    ) => match place_entity_type {
                        crate::frontend::world::tile::PlaceEntityType::Assembler(position) => {
                            info!("Trying to place assembler at {position:?}");
                            // TODO: get size dynamically
                            if !self.world.can_fit(position, (3, 3), data_store) {
                                warn!("Tried to place assembler where it does not fit");
                                continue;
                            }

                            let powered_by = self.world.is_powered_by(position, (3, 3), data_store);

                            if let Some(grid_id) = powered_by {
                                self.world.add_entity(crate::frontend::world::tile::Entity::Assembler {
                                    pos: position,
                                    info: AssemblerInfo::PoweredNoRecipe(grid_id),
                                }, data_store);
                            } else {
                                self.world.add_entity(
                                    crate::frontend::world::tile::Entity::Assembler {
                                        pos: position,
                                        info: AssemblerInfo::UnpoweredNoRecipe,
                                    }, data_store
                                );
                            }
                        },
                        crate::frontend::world::tile::PlaceEntityType::Inserter {
                            pos,
                            dir,
                            filter,
                        } => {
                            let _ = self.try_adding_inserter(pos, dir, Some(filter), data_store);
                        },
                        crate::frontend::world::tile::PlaceEntityType::Belt { pos, direction } => {
                            // TODO: Implement corners correctly

                            if !self.world.can_fit(pos, (1, 1), data_store) {
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
                                .map(|chunk| chunk.get_entity_at(front_pos, data_store))
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
                                .map(|chunk| chunk.get_entity_at(back_pos, data_store))
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

                                    self.world.add_entity(Entity::Belt {
                                        pos,
                                        direction,
                                        id: BeltTileId::EmptyBeltId(idx),
                                        belt_pos: BELT_LEN_PER_TILE,
                                    }, data_store);

                                    self.update_inserters(InserterUpdateInfo::NewBelt { pos }, data_store);
                                },
                                (None, Some(back_id)) => {
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
                                    self.world.add_entity(Entity::Belt {
                                        pos,
                                        direction,
                                        id: back_id,

                                        belt_pos: 0,
                                    }, data_store);

                                    self.world
                                        .modify_belt_pos(
                                            back_id,
                                            BELT_LEN_PER_TILE.try_into().unwrap()
                                        );

                                        self.update_inserters(InserterUpdateInfo::NewBelt { pos }, data_store);
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

                                    self.world.add_entity(Entity::Belt {
                                        pos,
                                        direction,
                                        id: front_id,

                                        belt_pos: (new_len),
                                    }, data_store);

                                    self.update_inserters(InserterUpdateInfo::NewBelt { pos }, data_store);
                                },
                                (Some(front_tile_id), Some(back_tile_id)) => {
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

                                    self.world.add_entity(Entity::Belt {
                                        pos,
                                        direction,
                                        id: new_id,

                                        belt_pos: front_len + BELT_LEN_PER_TILE,
                                    }, data_store);

                                    self.world
                                        .modify_belt_pos(
                                            back_tile_id,
                                            (front_len + BELT_LEN_PER_TILE).try_into().expect("Belt too long!")
                                        );

                                    self.update_inserters(InserterUpdateInfo::NewBelt { pos }, data_store);
                                },
                            }
                        },
                        crate::frontend::world::tile::PlaceEntityType::PowerPole { pos, ty } => {
                            // Check if the powerpole fits
                            if !self.world.can_fit(
                                pos,
                                data_store.power_pole_data[usize::from(ty)].size,
                                data_store,
                            ) {
                                warn!("Tried to place power pole where it does not fit");
                                continue;
                            }

                            let new_pole_entity = {
                                // Check which poles are in range to connect to
                                let mut connection_candidates = self
                                    .world
                                    .get_power_poles_which_could_connect_to_pole_at(
                                        pos,
                                        data_store.power_pole_data[usize::from(ty)].size,
                                        data_store.power_pole_data[usize::from(ty)]
                                            .connection_range,
                                        data_store,
                                    )
                                    .into_iter()
                                    .peekable();

                                if let Some(pole) = connection_candidates.peek() {
                                    let final_id = match pole {
                                        Entity::PowerPole { grid_id, .. } => *grid_id,
                                        _ => unreachable!(
                                            "List of power poles returned non power pole?!"
                                        ),
                                    };

                                    // TODO: A rust magician would probably be able to write this without temporary vectors but this should work
                                    let grids_to_merge: BTreeSet<PowerGridIdentifier> =
                                        connection_candidates
                                            .clone()
                                            .map(|pole| match pole {
                                                Entity::PowerPole {
                                                    grid_id,
                                                    ..
                                                } => *grid_id,
                                                _ => unreachable!(
                                                    "List of power poles returned non power pole?!"
                                                ),
                                            })
                                            .collect();

                                    let future_connected_power_poles: Vec<Position> = connection_candidates.map(|e| e.get_pos()).collect();

                                    // Merge all power_grids in the candidates
                                    for grid_id in grids_to_merge {
                                        let updates = self
                                            .simulation_state
                                            .factory
                                            .power_grids
                                            .merge_power_grids(final_id, grid_id, data_store, pos, future_connected_power_poles.iter().copied())
                                            .unzip();

                                        for pole_info in updates.1.into_iter().flatten() {
                                            let e = self
                                                .world
                                                .get_chunk_for_tile_mut(pole_info.position)
                                                .unwrap()
                                                .get_entity_at_mut(pole_info.position, data_store)
                                                .unwrap();

                                            match e {
                                                Entity::PowerPole { grid_id, .. } => {
                                                    *grid_id = pole_info.new_grid_id
                                                },
                                                _ => unreachable!(
                                                    "List of power poles returned non power pole?!"
                                                ),
                                            }
                                        }

                                        for storage_update_info in updates.0.into_iter().flatten() {
                                            // FIXME: Here we loop through the whole factory (or all inserters at least) multiple times. That is awful
                                            //        Accelerate that
                                            // Update Inserters!
                                            for belt_store in
                                                &mut self.simulation_state.factory.belts.belts
                                            {
                                                for belt in belt_store.belts_mut() {
                                                    belt.change_inserter_storage_id(
                                                        storage_update_info.old,
                                                        storage_update_info.new,
                                                    );
                                                }
                                            }
                                        }

                                        self.world.update_power_grid_id(&mut self.simulation_state, grid_id, final_id);
                                    }

                                    // Add this new connection to the connection candidates

                                    for pole_pos in &future_connected_power_poles {
                                        let pole = self
                                            .world
                                            .get_chunk_for_tile_mut(*pole_pos)
                                            .unwrap()
                                            .get_entity_at_mut(*pole_pos, data_store)
                                            .unwrap();

                                        match pole {
                                            Entity::PowerPole {
                                                connected_power_poles,
                                                ..
                                            } => connected_power_poles.push(pos),
                                            _ => unreachable!(
                                                "List of power poles returned non power pole?!"
                                            ),
                                        }
                                    }

                                    // Add any new machine, who are (exclusively) in range of this new pole into the grid

                                    let power_range =
                                        data_store.power_pole_data[usize::from(ty)].power_range;
                                    let size = data_store.power_pole_data[usize::from(ty)].size;

                                    let mut to_update_assemblers = vec![];

                                    self.world.mutate_entities_colliding_with(
                                        Position {
                                            x: pos.x - usize::from(power_range),
                                            y: pos.y - usize::from(power_range),
                                        },
                                        (2 * power_range + size.0, 2 * power_range + size.1),
                                        data_store,
                                        |e| {
                                        match e {
                                            Entity::Assembler { info, pos, .. } => match info {
                                                AssemblerInfo::UnpoweredNoRecipe => *info = AssemblerInfo::PoweredNoRecipe(final_id),
                                                AssemblerInfo::Unpowered(recipe) => {
                                                    let new_id = Self::add_assembler_to_sim(&mut self.simulation_state, *recipe, final_id, data_store);
                                                
                                                    to_update_assemblers.push(*pos);

                                                    *info = AssemblerInfo::Powered(new_id);
                                                },
                                                AssemblerInfo::PoweredNoRecipe(_) | AssemblerInfo::Powered(..) => {}
                                            },
                                            Entity::Inserter { .. } => {
                                                warn!("Could be powering Inserter, ignore for now!")
                                            },
                                            _ => {},
                                        }
                                        ControlFlow::Continue(())
                                    });
                                    
                                    // We might have instantiated an assembler, update surrounding inserters if necessary
                                    for pos in to_update_assemblers {
                                        self.update_inserters(InserterUpdateInfo::NewAssembler { pos, size: (3, 3) }, data_store);
                                    }

                                    // The new Power Pole entity
                                    Entity::PowerPole {
                                        ty,
                                        pos,
                                        grid_id: final_id,
                                        connected_power_poles: future_connected_power_poles,
                                    }
                                } else {
                                    // We cannot connect to anything.
                                    mem::drop(connection_candidates);
                                    // Create a new PowerGrid.

                                    let new_grid_id = self
                                        .simulation_state
                                        .factory
                                        .power_grids
                                        .add_power_grid(data_store);

                                    self.simulation_state.factory.power_grids.power_grids
                                        [usize::from(new_grid_id)]
                                    .as_mut()
                                    .unwrap()
                                    .add_pole(pos, vec![]);

                                    // Add all machines in range into the grid
                                    let power_range =
                                        data_store.power_pole_data[usize::from(ty)].power_range;
                                    let size = data_store.power_pole_data[usize::from(ty)].size;

                                    let mut to_update_assemblers = vec![];

                                    self.world.mutate_entities_colliding_with(
                                        Position {
                                            x: pos.x - usize::from(power_range),
                                            y: pos.y - usize::from(power_range),
                                        },
                                        (2 * power_range + size.0, 2 * power_range + size.1),
                                        data_store,
                                        |e| {
                                        match e {
                                            Entity::Assembler { info, pos, .. } => match info {
                                                AssemblerInfo::UnpoweredNoRecipe => *info = AssemblerInfo::PoweredNoRecipe(new_grid_id),
                                                AssemblerInfo::Unpowered(recipe) => {
                                                    let new_id = Self::add_assembler_to_sim(&mut self.simulation_state, *recipe, new_grid_id, data_store);
                                                
                                                    to_update_assemblers.push(*pos);

                                                    *info = AssemblerInfo::Powered(new_id);
                                                },
                                                AssemblerInfo::PoweredNoRecipe(_) | AssemblerInfo::Powered(..) => {}
                                            },
                                            Entity::Inserter { .. } => {
                                                warn!("Could be powering Inserter, ignore for now!")
                                            },
                                            _ => {},
                                        }
                                        ControlFlow::Continue(())
                                    });

                                    // We might have instantiated an assembler, update surrounding inserters if necessary
                                    for pos in to_update_assemblers {
                                        self.update_inserters(InserterUpdateInfo::NewAssembler { pos, size: (3, 3) }, data_store);
                                    }

                                    Entity::PowerPole {
                                        ty,
                                        pos,
                                        grid_id: new_grid_id,
                                        connected_power_poles: vec![],
                                    }
                                }
                            };

                            // Add the powerpole entity to the correct chunk

                            self.world
                                .add_entity(new_pole_entity, data_store);
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
                    info!("Ping at {:?}", (x, y));
                    // TODO:
                },
                ActionType::SetRecipe(SetRecipeInfo { pos, recipe }) => {
                    let mut needs_update = false;
                    let chunk = self.world.get_chunk_for_tile_mut(pos);

                    if let Some(chunk) = chunk {
                        let assembler = chunk.get_entity_at_mut(pos, data_store);

                        if let Some(mut entity) = assembler {
                            match &mut entity {
                                Entity::Assembler { pos, ref mut info } => match info {
                                    AssemblerInfo::UnpoweredNoRecipe => {
                                        *info = AssemblerInfo::Unpowered(recipe)
                                    },
                                    AssemblerInfo::Unpowered(_old_recipe) => {
                                        *info = AssemblerInfo::Unpowered(recipe)
                                    },
                                    AssemblerInfo::PoweredNoRecipe(power_grid_id) => {
                                        let new_id = Self::add_assembler_to_sim(&mut self.simulation_state, recipe, *power_grid_id, data_store);

                                        *info = AssemblerInfo::Powered(new_id);

                                        needs_update = true;
                                    },
                                    AssemblerInfo::Powered(assembler_id) => {
                                        let old_recipe_id = assembler_id.recipe.id.into();

                                        match (
                                            data_store.recipe_num_ing_lookup[old_recipe_id],
                                            data_store.recipe_num_out_lookup[old_recipe_id],
                                        ) {
                                            (0, 1) => {
                                                let old_assembler = self
                                                    .simulation_state
                                                    .factory
                                                    .power_grids
                                                    .power_grids
                                                    [assembler_id.grid as usize]
                                                    .as_mut()
                                                    .unwrap()
                                                    .remove_assembler(*assembler_id, data_store);

                                                // TODO: Add the old_assembler_items to a players inventory or something
                                            },

                                            _ => unreachable!(),
                                        };

                                        let new_id = Self::add_assembler_to_sim(&mut self.simulation_state, recipe, assembler_id.grid, data_store);

                                        *assembler_id = new_id;
                                        
                                        needs_update = true;
                                    },
                                },
                                Entity::PowerPole { .. }
                                | Entity::Belt { .. }
                                | Entity::Inserter { .. } => {
                                    warn!("Tried to change assembler recipe on non assembler!");
                                },
                            }
                        } else {
                            warn!("Tried to change assembler recipe where there was no entity!");
                        }
                    } else {
                        warn!("Tried to change assembler recipe outside world!");
                    }

                    if needs_update {
                        // FIXME: Size hardcoded!
                        self.update_inserters(InserterUpdateInfo::NewAssembler { pos, size: (3,3) }, data_store);
                    }
                },
            }
        }
    }

    pub fn update(&mut self, data_store: &DataStore<ItemIdxType, RecipeIdxType>) {
        self.world.player_pos.0 += self.world.player_move.0 / 60.0;
        self.world.player_pos.1 += self.world.player_move.1 / 60.0;

        self.simulation_state.factory.belts.belt_update(
            self.simulation_state
                .factory
                .power_grids
                .power_grids
                .iter_mut()
                .flatten(),
            data_store,
        );

        let (tech_progress, recipe_tick_info): (ResearchProgress, RecipeTickInfo) = self
            .simulation_state
            .factory
            .power_grids
            .power_grids
            .par_iter_mut()
            .flatten()
            .map(|grid| grid.update(Watt(1000), &self.simulation_state.tech_state, data_store))
            .reduce(|| (0, RecipeTickInfo::new(data_store)), |(acc_progress, infos), (rhs_progress, info)| {
                (acc_progress + rhs_progress, infos + &info)
            });

        self.simulation_state
            .tech_state
            .apply_progress(tech_progress);

        self.statistics.append_single_set_of_samples((ProductionInfo::from_recipe_info(&recipe_tick_info, data_store), ResearchInfo {}));

        if self.statistics.num_samples_pushed % 60 == 0 {
            File::create("./stats.svg").unwrap().write(self.statistics.get_chart(2, data_store, Some(|_| true)).svg().unwrap().as_bytes()).unwrap();
        }
    }

    fn update_inserters(&mut self, info: InserterUpdateInfo, data_store: &DataStore<ItemIdxType, RecipeIdxType>) {
        let inserter_range = data_store.max_inserter_search_range;

        let mut inserter_positions = vec![];

        match info {
            InserterUpdateInfo::NewAssembler { pos: assembler_pos, size } => {
                self.world.mutate_entities_colliding_with(Position {
                    x: assembler_pos.x - usize::from(inserter_range),
                    y: assembler_pos.y - usize::from(inserter_range),
                },
                (2 * inserter_range + size.0, 2 * inserter_range + size.1),
                data_store, |e| {
                    match e {
                        Entity::Inserter { pos, direction, info } => {
                            match info {
                                InserterInfo::NotAttached { start_pos, end_pos } => {
                                    if start_pos.contained_in(assembler_pos, size) || end_pos.contained_in(assembler_pos, size) {
                                        inserter_positions.push(*pos);
                                    }
                                },
                                InserterInfo::Attached(..) => {},
                            }
                        },
                        _ => {}
                    }
                    ControlFlow::Continue(())
                });
            },
            InserterUpdateInfo::NewBelt { pos: belt_pos } => {
                self.world.mutate_entities_colliding_with(Position {
                    x: belt_pos.x - usize::from(inserter_range),
                    y: belt_pos.y - usize::from(inserter_range),
                },
                (2 * inserter_range + 1, 2 * inserter_range + 1),
                data_store, |e| {
                    match e {
                        Entity::Inserter { pos, direction, info } => {
                            match info {
                                InserterInfo::NotAttached { start_pos, end_pos } => {
                                    if start_pos.contained_in(belt_pos, (1, 1)) || end_pos.contained_in(belt_pos, (1, 1)) {
                                        inserter_positions.push(*pos);
                                    }
                                },
                                InserterInfo::Attached(..) => {},
                            }
                        },
                        _ => {}
                    }
                    ControlFlow::Continue(())
                });
            },
        }

        for pos in inserter_positions {
            dbg!(pos);
            self.try_instantiate_inserter(pos, None, data_store);
        }
    }

    fn add_assembler_to_sim(
        sim_state: &mut SimulationState<RecipeIdxType>,
        recipe: Recipe<RecipeIdxType>,
        power_grid_id: PowerGridIdentifier,
        data_store: &DataStore<ItemIdxType, RecipeIdxType>,
    ) -> AssemblerID<RecipeIdxType> {
        sim_state.factory.power_grids.power_grids[power_grid_id as usize].as_mut().unwrap().add_assembler(power_grid_id, recipe, data_store)
    }

    fn try_instantiate_inserter(&mut self, pos: Position, filter: Option<Item<ItemIdxType>>, data_store: &DataStore<ItemIdxType, RecipeIdxType>) -> Result<(), ()> {
        enum InserterStartInfo<ItemIdxType: IdxTrait, RecipeIdxType: IdxTrait> {
            Belt(BeltTileId<ItemIdxType>, u16),
            Assembler(AssemblerInfo<RecipeIdxType>),
        }
        
        let Some(Entity::Inserter { info: InserterInfo::NotAttached { start_pos, end_pos }, .. }) = self.world.get_chunk_for_tile(pos).unwrap().get_entity_at(pos, data_store) else {
            return Err(())
        };

        let start_pos = *start_pos;
        let end_pos = *end_pos;

        let start_chunk = self.world.get_chunk_for_tile(start_pos);

        #[allow(clippy::option_if_let_else)]
        let start_info = if let Some(chunk) = start_chunk {
            if let Some(start_entity) = chunk.get_entity_at(start_pos, data_store) {
                match start_entity {
                    Entity::Assembler { pos, info } => Some(InserterStartInfo::Assembler(*info)),
                    Entity::PowerPole { .. } => {
                        warn!("Tried to place inserter starting on on PowerPole");
                        None
                    },
                    Entity::Belt { id, belt_pos, .. } => {
                        Some(InserterStartInfo::Belt(*id, *belt_pos))
                    },
                    Entity::Inserter { .. } => {
                        warn!("Tried to place inserter starting on another Inserter");
                        None
                    },
                }
            } else {
                info!("Tried to place inserter starting on nothing!");
                None
            }
        } else {
            warn!("Tried to place inserter starting outside world/in ungenerated chunk!");
            None
        };

        if let Some(start_info) = start_info {
            let end_chunk = self.world.get_chunk_for_tile_mut(end_pos);

            if let Some(end_chunk) = end_chunk {
                let end_entity = end_chunk.get_entity_at(end_pos, data_store);
                // I don´t like cloning here, but it should be fine. No entity should be big enough for this to matter much
                let end_entity = end_entity.cloned();
                if let Some(end_entity) = end_entity {
                    match end_entity {
                        Entity::Assembler { info, .. } => {
                            let id = match info {
                                AssemblerInfo::UnpoweredNoRecipe => {
                                    return Ok(());
                                },
                                AssemblerInfo::Unpowered(_) => {
                                    return Ok(());
                                },
                                AssemblerInfo::PoweredNoRecipe(_) => {
                                    return Ok(());
                                },
                                AssemblerInfo::Powered(assembler_id) => assembler_id,
                            };

                            match start_info {
                                InserterStartInfo::Belt(belt_tile_id, belt_pos) => {
                                    let Some(filter) = filter.or_else(|| match belt_tile_id {
                                        BeltTileId::EmptyBeltId(_) => {
                                            let mut inputs = data_store
                                                .recipe_to_items
                                                .get(&id.recipe)
                                                .unwrap()
                                                .iter()
                                                .filter(|i| i.0 == ItemRecipeDir::Ing);
                                            let first_input = inputs.next();

                                            if inputs.next().is_none() {
                                                first_input.map(|i| i.1)
                                            } else {
                                                None
                                            }
                                        },
                                        BeltTileId::BeltId(belt_id) => Some(belt_id.item),
                                    }) else {
                                        warn!("Could not determine filter for inserter, please specify a filter!");
                                        return Err(());
                                    };

                                    match belt_tile_id {
                                        BeltTileId::EmptyBeltId(idx) => {
                                            // We are inserting something on an empty belt!
                                            // It should no longer be empty!

                                            // Ensure that the assembler can take this item
                                            let Ok(storage) = data_store
                                                .get_storage_id_for_assembler(
                                                    crate::data::ItemRecipeDir::Out,
                                                    filter,
                                                    id,
                                                )
                                            else {
                                                warn!(
                                                    "This assembler does not require ing {:?}",
                                                    filter
                                                );
                                                return Err(());
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
                                                .add_out_inserter(belt_pos - 1, storage)
                                                .unwrap();

                                            let new_item_id: usize = filter.id.into();

                                            let hole_to_use =
                                                self.simulation_state.factory.belts.belts
                                                    [new_item_id]
                                                    .holes
                                                    .pop();

                                            let new_id = if let Some(idx) = hole_to_use {
                                                mem::swap(
                                                    &mut instantiated_belt,
                                                    &mut self.simulation_state.factory.belts.belts
                                                        [new_item_id]
                                                        .belts[idx],
                                                );
                                                mem::drop(instantiated_belt);

                                                BeltTileId::BeltId(BeltId {
                                                    item: filter,
                                                    index: idx,
                                                })
                                            } else {
                                                self.simulation_state.factory.belts.belts
                                                    [new_item_id]
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

                                            let Some(place_chunk) =
                                                self.world.get_chunk_for_tile_mut(pos)
                                            else {
                                                warn!(
                                                    "Tried to place inserter outside world/in ungenerated chunk!"
                                                );
                                                return Err(());
                                            };

                                            // Modify the inserter entity
                                            let Some(Entity::Inserter { pos, direction, info }) = place_chunk.get_entity_at_mut(pos, data_store) else {
                                                unreachable!()
                                            };
                                            *info = InserterInfo::Attached(AttachedInserter::BeltStorage { id: new_id, belt_pos: belt_pos - 1 });

                                            self.world
                                                .update_belt_id(
                                                    belt_tile_id,
                                                        new_id
                                                );
                                        },
                                        BeltTileId::BeltId(belt_id) => {
                                            if belt_id.item != filter {
                                                warn!("Can not output {filter:?} on belt already carrying {:?}", belt_id.item);
                                                return Err(());
                                            }

                                            let item_id: usize = filter.id.into();
                                            let belt =
                                                &mut self.simulation_state.factory.belts.belts
                                                    [item_id]
                                                    .belts[belt_id.index];

                                            let Ok(storage) = data_store
                                                .get_storage_id_for_assembler(
                                                    crate::data::ItemRecipeDir::Ing,
                                                    filter,
                                                    id,
                                                )
                                            else {
                                                warn!(
                                                    "This assembler does not require ing {:?}",
                                                    filter
                                                );
                                                return Err(());
                                            };

                                            // Add the inserter to the belt
                                            belt.add_out_inserter(belt_pos - 1, storage).unwrap();

                                            let Some(place_chunk) =
                                                self.world.get_chunk_for_tile_mut(pos)
                                            else {
                                                warn!(
                                                    "Tried to place inserter outside world/in ungenerated chunk!"
                                                );
                                                return Err(());
                                            };

                                            // Modify the inserter entity
                                            let Some(Entity::Inserter { pos, direction, info }) = place_chunk.get_entity_at_mut(pos, data_store) else {
                                                unreachable!()
                                            };
                                            *info = InserterInfo::Attached(AttachedInserter::BeltStorage { id: belt_tile_id, belt_pos: belt_pos - 1 });
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
                            return Err(());
                        },
                        Entity::Belt {
                            id: belt_tile_id,
                            belt_pos,
                            ..
                        } => match start_info {
                            InserterStartInfo::Belt(belt_id, belt_pos) => {
                                todo!()
                            },
                            InserterStartInfo::Assembler(info) => {
                                let assembler_id = match info {
                                    AssemblerInfo::UnpoweredNoRecipe => {
                                        return Ok(());
                                    },
                                    AssemblerInfo::Unpowered(recipe) => {
                                        return Ok(());
                                    },
                                    AssemblerInfo::PoweredNoRecipe(_) => {
                                        return Ok(());
                                    },
                                    AssemblerInfo::Powered(assembler_id) => assembler_id,
                                };

                                let Some(filter) = filter.or_else(|| match belt_tile_id {
                                    BeltTileId::EmptyBeltId(_) => {
                                        let filter = match info {
                                            AssemblerInfo::Unpowered(recipe)
                                            | AssemblerInfo::Powered(AssemblerID {
                                                recipe, ..
                                            }) => {
                                                let mut inputs = data_store
                                                    .recipe_to_items
                                                    .get(&recipe)
                                                    .unwrap()
                                                    .iter()
                                                    .filter(|i| i.0 == ItemRecipeDir::Out);
                                                let first_input = inputs.next();

                                                if inputs.next().is_none() {
                                                    first_input.map(|i| i.1)
                                                } else {
                                                    None
                                                }
                                            },

                                            AssemblerInfo::UnpoweredNoRecipe
                                            | AssemblerInfo::PoweredNoRecipe(_) => None,
                                        };

                                        filter
                                    },
                                    BeltTileId::BeltId(belt_id) => Some(belt_id.item),
                                }) else {
                                    warn!("Could not determine filter for inserter, please specify a filter!");
                                    return Err(());
                                };

                                match belt_tile_id {
                                    BeltTileId::EmptyBeltId(idx) => {
                                        // We are removing something from an empty belt!
                                        // It should no longer be empty!

                                        let Ok(storage) = data_store.get_storage_id_for_assembler(
                                            crate::data::ItemRecipeDir::Out,
                                            filter,
                                            assembler_id,
                                        ) else {
                                            warn!("This assembler cannot output {:?}", filter);
                                            return Err(());
                                        };

                                        // Remove the old empty belt
                                        let mut empty_belt = EmptyBelt::new(0);

                                        mem::swap(
                                            &mut empty_belt,
                                            &mut self.simulation_state.factory.belts.empty_belts
                                                [idx],
                                        );

                                        self.simulation_state
                                            .factory
                                            .belts
                                            .empty_belt_holes
                                            .push(idx);

                                        let mut instantiated_belt = empty_belt.into_smart_belt();

                                        // Add the inserter to the belt
                                        instantiated_belt
                                            .add_in_inserter(belt_pos - 1, storage)
                                            .unwrap();

                                        let new_item_id: usize = filter.id.into();

                                        let hole_to_use = self.simulation_state.factory.belts.belts
                                            [new_item_id]
                                            .holes
                                            .pop();

                                        let new_id = if let Some(idx) = hole_to_use {
                                            mem::swap(
                                                &mut instantiated_belt,
                                                &mut self.simulation_state.factory.belts.belts
                                                    [new_item_id]
                                                    .belts[idx],
                                            );
                                            mem::drop(instantiated_belt);

                                            BeltTileId::BeltId(BeltId {
                                                item: filter,
                                                index: idx,
                                            })
                                        } else {
                                            self.simulation_state.factory.belts.belts[new_item_id]
                                                .belts
                                                .push(instantiated_belt);

                                            BeltTileId::BeltId(BeltId {
                                                item: filter,
                                                index: self.simulation_state.factory.belts.belts
                                                    [new_item_id]
                                                    .belts
                                                    .len()
                                                    - 1,
                                            })
                                        };

                                        let Some(place_chunk) =
                                            self.world.get_chunk_for_tile_mut(pos)
                                        else {
                                            warn!(
                                                    "Tried to place inserter outside world/in ungenerated chunk!"
                                                );
                                            return Ok(());
                                        };

                                        // Modify the inserter entity
                                        let Some(Entity::Inserter { pos, direction, info }) = place_chunk.get_entity_at_mut(pos, data_store) else {
                                            unreachable!()
                                        };
                                        *info = InserterInfo::Attached(AttachedInserter::BeltStorage { id: new_id, belt_pos: belt_pos - 1 });

                                        self.world
                                            .update_belt_id(
                                                belt_tile_id,
                                                    new_id
                                            );
                                    },
                                    BeltTileId::BeltId(belt_id) => {
                                        if belt_id.item != filter {
                                            // TODO: Try to downgrade the belt if it is empty
                                            warn!("You cannot connect an inserter to a belt, which carries an item different from its filter");
                                            return Err(());
                                        }
                                        let item_id: usize = filter.id.into();
                                        let belt = &mut self.simulation_state.factory.belts.belts
                                            [item_id]
                                            .belts[belt_id.index];

                                        let Ok(storage) = data_store.get_storage_id_for_assembler(
                                            crate::data::ItemRecipeDir::Out,
                                            filter,
                                            assembler_id,
                                        ) else {
                                            warn!("This assembler cannot output {:?}", filter);
                                            return Ok(());
                                        };

                                        // Add the inserter to the belt
                                        belt.add_in_inserter(belt_pos - 1, storage).unwrap();

                                        let Some(place_chunk) =
                                            self.world.get_chunk_for_tile_mut(pos)
                                        else {
                                            warn!(
                                                "Tried to place inserter outside world/in ungenerated chunk!"
                                            );
                                            return Ok(());
                                        };

                                        // Modify the inserter entity
                                        let Some(Entity::Inserter { pos, direction, info }) = place_chunk.get_entity_at_mut(pos, data_store) else {
                                            unreachable!()
                                        };
                                        // TODO: Why -1 here?
                                        *info = InserterInfo::Attached(AttachedInserter::BeltStorage { id: belt_tile_id, belt_pos: belt_pos - 1 });
                                    },
                                }
                            },
                        },
                        Entity::Inserter {
                            pos,
                            direction,
                            info,
                        } => todo!(),
                    }
                } else {
                    // We do not have a end entity
                }
            } else {
                warn!("Tried to place inserter outside world/in ungenerated chunk!");
            };
        } else {
            // We do not have a start entity
        }

        Ok(())
    }

    fn try_adding_inserter(
        &mut self,
        pos: Position,
        dir: Dir,
        filter: Option<Item<ItemIdxType>>,
        data_store: &DataStore<ItemIdxType, RecipeIdxType>,
    ) -> Result<(), ()> {
        self.add_unattached_inserter(pos, dir, data_store)?;

        self.try_instantiate_inserter(pos, filter, data_store)
    }

    fn add_unattached_inserter(&mut self, pos: Position, dir: Dir, data_store: &DataStore<ItemIdxType, RecipeIdxType>,) -> Result<(), ()> {
        if !self.world.can_fit(pos, (1, 1), data_store) {
            warn!("Tried to place inserter where it does not fit");
            return Err(());
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

        self.world
            .add_entity(Entity::Inserter {
                pos,
                direction: dir,
                info: InserterInfo::NotAttached { start_pos, end_pos },
            }, data_store);

        Ok(())
    }
}

impl<RecipeIdxType: IdxTrait> Factory<RecipeIdxType> {
    // fn build_storage_store(power_grids: &mut Vec<PowerGrid>) -> StorageStore {
    //     todo!()
    // }
}

impl<RecipeIdxType: IdxTrait> BeltStore<RecipeIdxType> {
    fn belt_update<'a, ItemIdxType: IdxTrait>(
        &mut self,
        grids: impl IntoIterator<Item = &'a mut PowerGrid<RecipeIdxType>>,
        data_store: &DataStore<ItemIdxType, RecipeIdxType>,
    ) {
        let all_storages = all_storages(grids);

        // Sort the slices by item
        //                    item grid recipe idx
        let storages_by_item: Vec<Vec<Vec<&mut [u8]>>> = all_storages.into_iter()
            .zip(
                data_store
                    .recipe_item_store_to_item
                    .iter()
                    .chain(data_store.science_bottle_items.iter())
                    .cycle(),
            )
            .fold(Vec::new(), |mut acc, ((grid_id, storages), item)| {
                let old_grid = acc.get_mut(grid_id);

                match old_grid {
                    Some(_) => {},
                    None => {
                        acc.resize_with(grid_id + 1, Vec::new);
                    },
                }

                let grid_vec = acc.get_mut(grid_id).unwrap();

                for storage in storages {
                    let old = grid_vec.get_mut(item.id.into());

                    match old {
                        None => {
                            grid_vec.resize_with(item.id.into() + 1, Vec::new);
                            grid_vec.get_mut(item.id.into()).unwrap().push(storage);
                        },
                        Some(v) => v.push(storage),
                    }
                }
                acc
            });

        self.belts.par_iter_mut().zip(storages_by_item).for_each(
            |(belt_store, mut assembler_item_storages)| {
                for belt in &mut belt_store.belts {
                    belt.update();
                    belt.update_inserters(assembler_item_storages.as_mut_slice());
                }
            },
        );
    }
}