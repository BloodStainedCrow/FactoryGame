use std::{borrow::Borrow, marker::PhantomData, ops::ControlFlow};

use crate::{
    belt::{
        belt::Belt, splitter::Splitter, BeltBeltInserterInfo, BeltStore, BeltTileId, MultiBeltStore,
    },
    chest::{FullChestStore, MultiChestStore},
    data::{DataStore, ItemRecipeDir},
    frontend::{
        action::{
            belt_placement::{handle_belt_placement, handle_splitter_placement},
            set_recipe::SetRecipeInfo,
            ActionType,
        },
        world::{
            tile::{
                AssemblerID, AssemblerInfo, AttachedInserter, BeltId, Dir, Entity, InserterInfo,
                World,
            },
            Position,
        },
    },
    inserter::{belt_belt_inserter::BeltBeltInserter, StaticID, Storage},
    item::{usize_from, IdxTrait, Item, Recipe, WeakIdxTrait},
    power::{power_grid::PowerGridIdentifier, PowerGridStorage, Watt},
    research::{ResearchProgress, TechState},
    statistics::{production::ProductionInfo, recipe::RecipeTickInfo, GenStatistics},
    storage_list::{full_to_by_item, grid_size, num_recipes, sizes, storages_by_item},
};
use itertools::Itertools;
use log::{error, info, warn};
use rayon::iter::{IndexedParallelIterator, IntoParallelRefMutIterator, ParallelIterator};

use crate::frontend::action::place_tile::PositionInfo;

#[derive(Debug, Clone, serde::Deserialize, serde::Serialize)]
pub struct GameState<ItemIdxType: WeakIdxTrait, RecipeIdxType: WeakIdxTrait> {
    pub current_tick: u64,

    pub world: World<ItemIdxType, RecipeIdxType>,
    pub simulation_state: SimulationState<ItemIdxType, RecipeIdxType>,

    statistics: GenStatistics,
}

impl<ItemIdxType: IdxTrait, RecipeIdxType: IdxTrait> GameState<ItemIdxType, RecipeIdxType> {
    pub fn new(data_store: &DataStore<ItemIdxType, RecipeIdxType>) -> Self {
        Self {
            current_tick: 0,
            world: World::new(),
            simulation_state: SimulationState::new(data_store),
            statistics: GenStatistics::new(data_store),
        }
    }
}

#[derive(Debug, Clone, serde::Deserialize, serde::Serialize)]
pub struct SimulationState<ItemIdxType: WeakIdxTrait, RecipeIdxType: WeakIdxTrait> {
    tech_state: TechState,
    pub factory: Factory<ItemIdxType, RecipeIdxType>,
    // TODO:
}

impl<ItemIdxType: IdxTrait, RecipeIdxType: IdxTrait> SimulationState<ItemIdxType, RecipeIdxType> {
    pub fn new(data_store: &DataStore<ItemIdxType, RecipeIdxType>) -> Self {
        Self {
            tech_state: TechState::default(),
            factory: Factory::new(data_store),
        }
    }
}

#[derive(Debug, Clone, serde::Deserialize, serde::Serialize)]
pub struct Factory<ItemIdxType: WeakIdxTrait, RecipeIdxType: WeakIdxTrait> {
    pub power_grids: PowerGridStorage<ItemIdxType, RecipeIdxType>,
    pub belts: BeltStore<ItemIdxType, RecipeIdxType>,
    pub chests: FullChestStore<ItemIdxType>,
}

#[derive(Debug, Clone, serde::Deserialize, serde::Serialize)]
pub struct BeltBeltInserterStore<ItemIdxType: WeakIdxTrait> {
    // TODO: Holes
    pub inserters: Box<[Vec<(BeltBeltInserter, BeltBeltInserterInfo<ItemIdxType>)>]>,
}

impl<ItemIdxType: IdxTrait, RecipeIdxType: IdxTrait> Factory<ItemIdxType, RecipeIdxType> {
    fn new(data_store: &DataStore<ItemIdxType, RecipeIdxType>) -> Self {
        Self {
            power_grids: PowerGridStorage::new(),
            belts: BeltStore::new(data_store),
            chests: FullChestStore {
                stores: (0..data_store.item_names.len())
                    .map(|id| Item {
                        id: id.try_into().unwrap(),
                    })
                    .map(|item| MultiChestStore::new(item, data_store))
                    .collect(),
            },
        }
    }

    fn belt_update<'a>(&mut self, data_store: &DataStore<ItemIdxType, RecipeIdxType>) {
        let num_grids_total = self.power_grids.power_grids.iter().flatten().count();
        let mut all_storages =
            storages_by_item(&mut self.power_grids, &mut self.chests, data_store);
        let sizes: Vec<_> = sizes(data_store, num_grids_total).into_iter().collect();
        // dbg!(&all_storages);
        assert_eq!(sizes.len(), data_store.item_names.len());
        let storages_by_item = full_to_by_item(&mut all_storages, &sizes);

        self.belts
            .update(num_grids_total, storages_by_item, data_store);
    }
}

pub enum AppState {
    Ingame,
    Loading,
}

#[derive(Debug, Clone, Copy)]
enum InserterUpdateInfo {
    NewAssembler { pos: Position, size: (u8, u8) },
    NewBelt { pos: Position },
}

#[derive(Debug)]
enum InstantiateInserterError {
    NotUnattachedInserter,
    SourceMissing,
    DestMissing,
    PleaseSpecifyFilter,
    ItemConflict,
}

impl<ItemIdxType: IdxTrait, RecipeIdxType: IdxTrait> GameState<ItemIdxType, RecipeIdxType> {
    #[allow(clippy::too_many_lines)]
    pub fn apply_actions(
        &mut self,
        actions: impl IntoIterator<Item = impl Borrow<ActionType<ItemIdxType, RecipeIdxType>>>,
        data_store: &DataStore<ItemIdxType, RecipeIdxType>,
    ) {
        for action in actions {
            // FIXME: I just clone for now
            match action.borrow().clone() {
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
                                    self.world.set_floor_tile(
                                        Position { x, y },
                                        place_floor_tile_by_hand_info.ghost_info.tile,
                                    );
                                }
                            }
                        },
                        PositionInfo::Single { pos } => {
                            self.world
                                .set_floor_tile(pos, place_floor_tile_by_hand_info.ghost_info.tile);
                        },
                        PositionInfo::List { positions } => {
                            for pos in positions {
                                self.world.set_floor_tile(
                                    pos,
                                    place_floor_tile_by_hand_info.ghost_info.tile,
                                );
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

                            let powered_by = self.world.is_powered_by(
                                &self.simulation_state,
                                position,
                                (3, 3),
                                data_store,
                            );

                            if let Some(pole_position) = powered_by {
                                self.world.add_entity(
                                    crate::frontend::world::tile::Entity::Assembler {
                                        pos: position,
                                        info: AssemblerInfo::PoweredNoRecipe(pole_position),
                                    },
                                    &self.simulation_state,
                                    data_store,
                                );
                            } else {
                                self.world.add_entity(
                                    crate::frontend::world::tile::Entity::Assembler {
                                        pos: position,
                                        info: AssemblerInfo::UnpoweredNoRecipe,
                                    },
                                    &self.simulation_state,
                                    data_store,
                                );
                            }
                        },
                        crate::frontend::world::tile::PlaceEntityType::Inserter {
                            pos,
                            dir,
                            filter,
                        } => {
                            let ret = self.try_adding_inserter(pos, dir, Some(filter), data_store);
                            dbg!(ret);
                        },
                        crate::frontend::world::tile::PlaceEntityType::Belt { pos, direction } => {
                            if !self.world.can_fit(pos, (1, 1), data_store) {
                                warn!("Tried to place belt where it does not fit");
                                continue;
                            }

                            handle_belt_placement(self, pos, direction, data_store);

                            self.update_inserters(InserterUpdateInfo::NewBelt { pos }, data_store);
                        },
                        crate::frontend::world::tile::PlaceEntityType::PowerPole {
                            pos: pole_pos,
                            ty,
                        } => {
                            // Check if the powerpole fits
                            if !self.world.can_fit(
                                pole_pos,
                                data_store.power_pole_data[usize::from(ty)].size,
                                data_store,
                            ) {
                                warn!("Tried to place power pole where it does not fit");
                                continue;
                            }

                            // Check which poles are in range to connect to
                            let connection_candidates: Vec<_> = self
                                .world
                                .get_power_poles_which_could_connect_to_pole_at(
                                    pole_pos,
                                    data_store.power_pole_data[usize::from(ty)].size,
                                    data_store.power_pole_data[usize::from(ty)].connection_range,
                                    data_store,
                                )
                                .into_iter()
                                .map(|e| e.get_pos())
                                .collect();

                            if let Some((pole_updates, storage_updates)) =
                                self.simulation_state.factory.power_grids.add_pole(
                                    pole_pos,
                                    connection_candidates.iter().copied(),
                                    data_store,
                                )
                            {
                                // Handle Entities that are now part of another power_grid
                                for pole_position in pole_updates {
                                    let grid = self
                                        .simulation_state
                                        .factory
                                        .power_grids
                                        .pole_pos_to_grid_id[&pole_position];

                                    assert!(self.simulation_state.factory.power_grids.power_grids
                                        [grid as usize]
                                        .is_some());

                                    self.world
                                        .update_pole_power(pole_position, grid, data_store);
                                }

                                // Handle storage updates
                                for storage_update in storage_updates {
                                    todo!("Handle {:?}", storage_update);
                                }
                            } else {
                                // No updates needed
                            }

                            let grid = self
                                .simulation_state
                                .factory
                                .power_grids
                                .pole_pos_to_grid_id[&pole_pos];

                            // Handle Entities that are newly powered
                            let power_range = data_store.power_pole_data[ty as usize].power_range;
                            self.world.mutate_entities_colliding_with(
                                Position {
                                    x: pole_pos.x - power_range as usize,
                                    y: pole_pos.y - power_range as usize,
                                },
                                (2 * power_range + 1, 2 * power_range + 1),
                                data_store,
                                |e| {
                                    match e {
                                        Entity::Assembler { pos, info } => match info {
                                            AssemblerInfo::UnpoweredNoRecipe => {
                                                *info = AssemblerInfo::PoweredNoRecipe(pole_pos);
                                            },
                                            AssemblerInfo::Unpowered(recipe) => {
                                                let assembler_id = self
                                                    .simulation_state
                                                    .factory
                                                    .power_grids
                                                    .power_grids
                                                    [grid as usize]
                                                    .as_mut()
                                                    .unwrap() // TODO: Assembler ty
                                                    .add_assembler(
                                                        0, grid, *recipe, pole_pos, *pos,
                                                        data_store,
                                                    );
                                                *info = AssemblerInfo::Powered {
                                                    id: assembler_id,
                                                    pole_position: pole_pos,
                                                };
                                            },
                                            _ => {},
                                        },
                                        Entity::Roboport {
                                            ty,
                                            pos,
                                            power_grid,
                                            network,
                                            id,
                                        } => {
                                            if power_grid.is_none() {
                                                *power_grid = Some(grid)
                                            }
                                            todo!("Add Roboport to power grid")
                                        },

                                        _ => {},
                                    }
                                    ControlFlow::Continue(())
                                },
                            );

                            // Add the powerpole entity to the correct chunk
                            self.world.add_entity(
                                Entity::PowerPole {
                                    ty,
                                    pos: pole_pos,
                                    connected_power_poles: connection_candidates,
                                },
                                &self.simulation_state,
                                data_store,
                            );
                        },
                        crate::frontend::world::tile::PlaceEntityType::Splitter {
                            pos,
                            direction,
                            in_mode,
                            out_mode,
                        } => {
                            let splitter = handle_splitter_placement(
                                self, pos, direction, in_mode, out_mode, data_store,
                            );
                        },
                        crate::frontend::world::tile::PlaceEntityType::Chest { pos } => {
                            // FIXME: Chest item hardcoded
                            let item = Item {
                                id: ItemIdxType::from(0),
                            };

                            let index = self.simulation_state.factory.chests.stores
                                [usize_from(item.id)]
                            .add_chest(0, data_store);

                            self.world.add_entity(
                                // FIXME: Chest type hardcoded
                                Entity::Chest {
                                    ty: 0,
                                    pos,
                                    item: Some(item),
                                    index,
                                },
                                &self.simulation_state,
                                data_store,
                            );
                        },
                    },
                    crate::frontend::action::place_entity::EntityPlaceOptions::Multiple(vec) => {
                        todo!()
                    },
                },
                ActionType::Position(id, pos) => {
                    self.world.players[usize::from(id)].visible = true;
                    self.world.players[usize::from(id)].pos = pos;
                },
                ActionType::Ping(Position { x, y }) => {
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
                                    AssemblerInfo::PoweredNoRecipe(pole_position) => {
                                        let grid_id = self
                                            .simulation_state
                                            .factory
                                            .power_grids
                                            .pole_pos_to_grid_id[&pole_position];

                                        let new_id = Self::add_assembler_to_sim(
                                            &mut self.simulation_state,
                                            recipe,
                                            grid_id,
                                            *pole_position,
                                            *pos,
                                            data_store,
                                        );

                                        *info = AssemblerInfo::Powered {
                                            id: new_id,
                                            pole_position: *pole_position,
                                        };

                                        needs_update = true;
                                    },
                                    AssemblerInfo::Powered {
                                        id: assembler_id,
                                        pole_position,
                                    } => {
                                        let old_recipe_id = assembler_id.recipe;

                                        if old_recipe_id == recipe {
                                            continue;
                                        }

                                        match (
                                            data_store.recipe_num_ing_lookup
                                                [old_recipe_id.id.into()],
                                            data_store.recipe_num_out_lookup
                                                [old_recipe_id.id.into()],
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

                                        let new_id = Self::add_assembler_to_sim(
                                            &mut self.simulation_state,
                                            recipe,
                                            assembler_id.grid,
                                            *pole_position,
                                            *pos,
                                            data_store,
                                        );

                                        *assembler_id = new_id;

                                        needs_update = true;
                                    },
                                },
                                _ => {
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
                        self.update_inserters(
                            InserterUpdateInfo::NewAssembler { pos, size: (3, 3) },
                            data_store,
                        );
                    }
                },
                ActionType::Remove(pos) => {
                    self.world
                        .remove_entity_at(pos, &mut self.simulation_state, data_store);
                },
            }
        }
    }

    pub fn update(&mut self, data_store: &DataStore<ItemIdxType, RecipeIdxType>) {
        self.simulation_state.factory.chests.update();

        self.simulation_state.factory.belt_update(data_store);

        // TODO: Do I want this, or just do it in the belt_update
        //self.simulation_state
        //    .factory
        //    .belt_belt_inserters
        //    .update(&mut self.simulation_state.factory.belts, data_store);

        let (tech_progress, recipe_tick_info): (ResearchProgress, RecipeTickInfo) = self
            .simulation_state
            .factory
            .power_grids
            .power_grids
            .par_iter_mut()
            .flatten()
            .map(|grid| grid.update(Watt(1000), &self.simulation_state.tech_state, data_store))
            .reduce(
                || (0, RecipeTickInfo::new(data_store)),
                |(acc_progress, infos), (rhs_progress, info)| {
                    (acc_progress + rhs_progress, infos + &info)
                },
            );

        self.simulation_state
            .tech_state
            .apply_progress(tech_progress);

        self.statistics.append_single_set_of_samples((
            ProductionInfo::from_recipe_info(&recipe_tick_info, data_store),
            tech_progress,
        ));

        // if self.statistics.production.num_samples_pushed % 60 == 0 {
        //     File::create("./stats.svg").unwrap().write(self.statistics.get_chart(1, data_store, Some(|_| true)).svg().unwrap().as_bytes()).unwrap();
        // }
    }

    fn update_inserters(
        &mut self,
        info: InserterUpdateInfo,
        data_store: &DataStore<ItemIdxType, RecipeIdxType>,
    ) {
        let inserter_range = data_store.max_inserter_search_range;

        let mut inserter_positions = vec![];

        match info {
            InserterUpdateInfo::NewAssembler {
                pos: assembler_pos,
                size,
            } => {
                self.world.mutate_entities_colliding_with(
                    Position {
                        x: assembler_pos.x - usize::from(inserter_range),
                        y: assembler_pos.y - usize::from(inserter_range),
                    },
                    (2 * inserter_range + size.0, 2 * inserter_range + size.1),
                    data_store,
                    |e| {
                        match e {
                            Entity::Inserter {
                                pos,
                                direction,
                                info,
                            } => match info {
                                InserterInfo::NotAttached { start_pos, end_pos } => {
                                    if start_pos.contained_in(assembler_pos, size)
                                        || end_pos.contained_in(assembler_pos, size)
                                    {
                                        inserter_positions.push(*pos);
                                    }
                                },
                                InserterInfo::Attached(..) => {},
                            },
                            _ => {},
                        }
                        ControlFlow::Continue(())
                    },
                );
            },
            InserterUpdateInfo::NewBelt { pos: belt_pos } => {
                self.world.mutate_entities_colliding_with(
                    Position {
                        x: belt_pos.x - usize::from(inserter_range),
                        y: belt_pos.y - usize::from(inserter_range),
                    },
                    (2 * inserter_range + 1, 2 * inserter_range + 1),
                    data_store,
                    |e| {
                        match e {
                            Entity::Inserter {
                                pos,
                                direction,
                                info,
                            } => match info {
                                InserterInfo::NotAttached { start_pos, end_pos } => {
                                    if start_pos.contained_in(belt_pos, (1, 1))
                                        || end_pos.contained_in(belt_pos, (1, 1))
                                    {
                                        inserter_positions.push(*pos);
                                    }
                                },
                                InserterInfo::Attached(..) => {},
                            },
                            _ => {},
                        }
                        ControlFlow::Continue(())
                    },
                );
            },
        }

        for pos in inserter_positions {
            dbg!(pos);
            self.try_instantiate_inserter(pos, None, data_store);
        }
    }

    fn add_assembler_to_sim(
        sim_state: &mut SimulationState<ItemIdxType, RecipeIdxType>,
        recipe: Recipe<RecipeIdxType>,
        power_grid_id: PowerGridIdentifier,
        pole_position: Position,
        assembler_position: Position,
        data_store: &DataStore<ItemIdxType, RecipeIdxType>,
    ) -> AssemblerID<RecipeIdxType> {
        sim_state.factory.power_grids.power_grids[power_grid_id as usize]
            .as_mut()
            .unwrap()
            // Assembler type
            .add_assembler(
                0,
                power_grid_id,
                recipe,
                pole_position,
                assembler_position,
                data_store,
            )
    }

    fn try_instantiate_inserter(
        &mut self,
        pos: Position,
        filter: Option<Item<ItemIdxType>>,
        data_store: &DataStore<ItemIdxType, RecipeIdxType>,
    ) -> Result<(), InstantiateInserterError> {
        enum InserterConnection<ItemIdxType: IdxTrait, RecipeIdxType: IdxTrait> {
            Belt(BeltTileId<ItemIdxType>, u16),
            Storage(Storage<RecipeIdxType>),
        }

        let Some(Entity::Inserter {
            pos: _pos,
            direction,
            info: InserterInfo::NotAttached { start_pos, end_pos },
        }) = self
            .world
            .get_entities_colliding_with(pos, (1, 1), data_store)
            .into_iter()
            .next()
        else {
            return Err(InstantiateInserterError::NotUnattachedInserter);
        };

        let start_conn: Option<(
            InserterConnection<ItemIdxType, RecipeIdxType>,
            Option<Vec<Item<ItemIdxType>>>,
        )> = self
            .world
            .get_entities_colliding_with(*start_pos, (1, 1), data_store)
            .into_iter()
            .next()
            .map(|e| match e {
                Entity::Inserter { .. } | Entity::PowerPole { .. } => None,

                Entity::Roboport { ty, pos, power_grid, network, id } => {
                    // TODO:
                    warn!("It is currently not possible to add or remove bots from roboports using inserters");
                    None
                }

                Entity::Assembler {
                    pos,
                    info: AssemblerInfo::Powered {
                        id,
                        pole_position
                    },
                    // FIXME: Translate the recipe_idx to
                } => Some((
                    InserterConnection::Storage(Storage::Assembler {
                        grid: id.grid,
                        recipe_idx_with_this_item: id.recipe.id,
                        index: id.assembler_index,
                    }),
                    Some(
                        data_store.recipe_to_items[&id.recipe]
                            .iter()
                            .filter_map(|(dir, item)| (*dir == ItemRecipeDir::Out).then_some(*item))
                            .collect(),
                    ),
                )),
                Entity::Assembler { .. } => None,
                Entity::Belt {
                    id: BeltTileId::AnyBelt(id, _),
                    belt_pos,
                    ..
                }
                | Entity::Underground {
                    id: BeltTileId::AnyBelt(id, _),
                    belt_pos,
                    ..
                } => Some((
                    InserterConnection::Belt(BeltTileId::AnyBelt(*id, PhantomData), *belt_pos),
                    match self
                        .simulation_state
                        .factory
                        .belts
                        .get_pure_item(BeltTileId::AnyBelt(*id, PhantomData))
                    {
                        Some(item) => Some(vec![item]),
                        None => None,
                    },
                )),
                Entity::Splitter { pos, direction, id } => todo!("Inserters on splitters"),
                Entity::Chest {
                    ty,
                    pos,
                    item,
                    index,
                } => Some((
                    InserterConnection::Storage(Storage::Static {
                        static_id: StaticID::Chest,
                        index: *index,
                    }),
                    Some(item.into_iter().copied().collect()),
                )),
            })
            .flatten();

        let Some(start_conn) = start_conn else {
            return Err(InstantiateInserterError::SourceMissing);
        };

        let dest_conn: Option<(
            InserterConnection<ItemIdxType, RecipeIdxType>,
            Option<Vec<Item<ItemIdxType>>>,
        )> = self
            .world
            .get_entities_colliding_with(*end_pos, (1, 1), data_store)
            .into_iter()
            .next()
            .map(|e| match e {
                Entity::Inserter { .. } | Entity::PowerPole { .. } => None,

                Entity::Roboport { ty, pos, power_grid, network, id } => {
                    // TODO:
                    warn!("It is currently not possible to add or remove bots from roboports using inserters");
                    None
                }

                Entity::Assembler {
                    pos,
                    info: AssemblerInfo::Powered {
                        id,
                        pole_position,
                    },
                    // FIXME: Translate the recipe_idx to
                } => Some((
                    InserterConnection::Storage(Storage::Assembler {
                        grid: id.grid,
                        recipe_idx_with_this_item: id.recipe.id,
                        index: id.assembler_index,
                    }),
                    Some(
                        data_store.recipe_to_items[&id.recipe]
                            .iter()
                            .filter_map(|(dir, item)| (*dir == ItemRecipeDir::Ing).then_some(*item))
                            .collect(),
                    ),
                )),
                Entity::Assembler { .. } => None,
                Entity::Belt {
                    id: BeltTileId::AnyBelt(id, _),
                    belt_pos,
                    ..
                }
                | Entity::Underground {
                    id: BeltTileId::AnyBelt(id, _),
                    belt_pos,
                    ..
                } => Some((
                    InserterConnection::Belt(BeltTileId::AnyBelt(*id, PhantomData), *belt_pos),
                    match self
                        .simulation_state
                        .factory
                        .belts
                        .get_pure_item(BeltTileId::AnyBelt(*id, PhantomData))
                    {
                        Some(item) => Some(vec![item]),
                        None => None,
                    },
                )),
                Entity::Splitter { pos, direction, id } => todo!(),
                Entity::Chest {
                    ty,
                    pos,
                    item,
                    index,
                } => Some((
                    InserterConnection::Storage(Storage::Static {
                        static_id: StaticID::Chest,
                        index: *index,
                    }),
                    Some(item.into_iter().copied().collect()),
                )),
            })
            .flatten();

        let Some(dest_conn) = dest_conn else {
            return Err(InstantiateInserterError::DestMissing);
        };

        let filter = filter
            .into_iter()
            .chain(start_conn.1.into_iter().flatten())
            .chain(dest_conn.1.into_iter().flatten())
            .all_equal_value();

        let filter = match filter {
            Ok(filter) => filter,
            Err(None) => return Err(InstantiateInserterError::PleaseSpecifyFilter),
            Err(Some(wrong)) => return Err(InstantiateInserterError::ItemConflict),
        };

        let Entity::Inserter { info, .. } = self
            .world
            .get_chunk_for_tile_mut(pos)
            .unwrap()
            .get_entity_at_mut(pos, data_store)
            .unwrap()
        else {
            unreachable!("We already checked it was an unattached inserter before")
        };

        match (start_conn.0, dest_conn.0) {
            (
                InserterConnection::Belt(start_belt_id, start_belt_pos),
                InserterConnection::Belt(dest_belt_id, dest_belt_pos),
            ) => {
                // TODO:
                //debug_assert_eq!(filter, start_belt_id.item);
                //debug_assert_eq!(start_belt_id.item, dest_belt_id.item);
                //// FIXME: The movetime should be dependent on the inserter type!
                //let index = self.simulation_state.factory.add_belt_belt_inserter(
                //    (start_belt_id, start_belt_pos),
                //    (dest_belt_id, dest_belt_pos),
                //    BeltBeltInserterAdditionInfo { cooldown: MOVETIME },
                //);
                //*info = InserterInfo::Attached(AttachedInserter::BeltBelt {
                //    item: filter,
                //    inserter: index,
                //})
            },
            (
                InserterConnection::Belt(start_belt_id, start_belt_pos),
                InserterConnection::Storage(dest_storage_untranslated),
            ) => {
                let dest_storage = dest_storage_untranslated.translate(filter, data_store);

                match self
                    .simulation_state
                    .factory
                    .belts
                    .add_belt_storage_inserter(
                        filter,
                        start_belt_id,
                        start_belt_pos - 1,
                        dest_storage,
                    ) {
                    Ok(()) => {},
                    Err(_) => todo!(),
                };

                *info = InserterInfo::Attached(AttachedInserter::BeltStorage {
                    id: start_belt_id,
                    belt_pos: start_belt_pos - 1,
                });
            },
            (
                InserterConnection::Storage(start_storage_untranslated),
                InserterConnection::Belt(dest_belt_id, dest_belt_pos),
            ) => {
                let start_storage = start_storage_untranslated.translate(filter, data_store);

                match self
                    .simulation_state
                    .factory
                    .belts
                    .add_belt_storage_inserter(
                        filter,
                        dest_belt_id,
                        dest_belt_pos - 1,
                        start_storage,
                    ) {
                    Ok(()) => {},
                    Err(_) => todo!(),
                };
                *info = InserterInfo::Attached(AttachedInserter::BeltStorage {
                    id: dest_belt_id,
                    belt_pos: dest_belt_pos - 1,
                });
            },
            (
                InserterConnection::Storage(start_storage_untranslated),
                InserterConnection::Storage(dest_storage_untranslated),
            ) => todo!(),
        }

        Ok(())
    }

    fn try_adding_inserter(
        &mut self,
        pos: Position,
        dir: Dir,
        filter: Option<Item<ItemIdxType>>,
        data_store: &DataStore<ItemIdxType, RecipeIdxType>,
    ) -> Result<(), InstantiateInserterError> {
        match self.add_unattached_inserter(pos, dir, data_store) {
            Ok(_) => {},
            Err(_) => return Err(InstantiateInserterError::NotUnattachedInserter),
        };

        self.try_instantiate_inserter(pos, filter, data_store)
    }

    fn add_unattached_inserter(
        &mut self,
        pos: Position,
        dir: Dir,
        data_store: &DataStore<ItemIdxType, RecipeIdxType>,
    ) -> Result<(), ()> {
        if !self.world.can_fit(pos, (1, 1), data_store) {
            warn!("Tried to place inserter where it does not fit");
            return Err(());
        }

        let (start_pos, end_pos) = calculate_inserter_positions(pos, dir);

        self.world.add_entity(
            Entity::Inserter {
                pos,
                direction: dir,
                info: InserterInfo::NotAttached { start_pos, end_pos },
            },
            &self.simulation_state,
            data_store,
        );

        Ok(())
    }
}

// TODO: Different types of inserters
pub fn calculate_inserter_positions(pos: Position, dir: Dir) -> (Position, Position) {
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

    (start_pos, end_pos)
}

// #[cfg(test)]
mod tests {
    use std::sync::LazyLock;

    use crate::{
        blueprint::{random_blueprint_strategy, random_position},
        data::{get_raw_data_test, DataStore},
        frontend::world::Position,
        rendering::app_state::GameState,
    };
    use proptest::{prelude::ProptestConfig, proptest};
    static DATA_STORE: LazyLock<DataStore<u8, u8>> =
        LazyLock::new(|| get_raw_data_test().turn::<u8, u8>());

    proptest! {
        #[test]
        fn test_random_blueprint_does_not_crash(base_pos in random_position(), blueprint in random_blueprint_strategy::<u8, u8>(0..100, &DATA_STORE)) {

            let mut game_state = GameState::new(&DATA_STORE);

            blueprint.apply(base_pos, &mut game_state, &DATA_STORE);

        }
    }
}
