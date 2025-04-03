use std::{
    collections::BTreeSet,
    marker::PhantomData,
    mem::{self},
    ops::ControlFlow,
};

use crate::{
    belt::{belt::Belt, splitter::Splitter, BeltStore, BeltTileId},
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
                AssemblerID, AssemblerInfo, AttachedInserter, Dir, Entity, InserterInfo, World,
            },
            Position,
        },
    },
    inserter::{StaticID, Storage},
    item::{usize_from, IdxTrait, Item, Recipe, WeakIdxTrait},
    power::{power_grid::PowerGridIdentifier, PowerGridStorage, Watt},
    research::{ResearchProgress, TechState},
    statistics::{
        production::ProductionInfo, recipe::RecipeTickInfo, research::ResearchInfo, GenStatistics,
    },
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
    pub power_grids: PowerGridStorage<RecipeIdxType>,
    pub belts: BeltStore<ItemIdxType, RecipeIdxType>,
    pub chests: FullChestStore<ItemIdxType>,
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
        if self.power_grids.power_grids.len() > 1 {
            warn!("Bug here");
        }

        let num_grids_total = self.power_grids.power_grids.iter().flatten().count();
        let mut all_storages =
            storages_by_item(&mut self.power_grids, &mut self.chests, data_store);
        let sizes: Vec<_> = sizes(data_store, num_grids_total).into_iter().collect();
        // dbg!(&all_storages);
        assert_eq!(sizes.len(), data_store.item_names.len());
        let storages_by_item = full_to_by_item(&mut all_storages, &sizes);

        self.belts.update(storages_by_item);
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

                            let powered_by = self.world.is_powered_by(position, (3, 3), data_store);

                            if let Some(grid_id) = powered_by {
                                self.world.add_entity(
                                    crate::frontend::world::tile::Entity::Assembler {
                                        pos: position,
                                        info: AssemblerInfo::PoweredNoRecipe(grid_id),
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
                                                Entity::PowerPole { grid_id, .. } => *grid_id,
                                                _ => unreachable!(
                                                    "List of power poles returned non power pole?!"
                                                ),
                                            })
                                            .collect();

                                    let future_connected_power_poles: Vec<Position> =
                                        connection_candidates.map(|e| e.get_pos()).collect();

                                    // Merge all power_grids in the candidates
                                    for grid_id in grids_to_merge {
                                        let updates = self
                                            .simulation_state
                                            .factory
                                            .power_grids
                                            .merge_power_grids(
                                                final_id,
                                                grid_id,
                                                data_store,
                                                pos,
                                                future_connected_power_poles.iter().copied(),
                                            )
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
                                                        storage_update_info.old.1,
                                                        storage_update_info.new.1,
                                                    );
                                                }
                                            }
                                        }

                                        self.world.update_power_grid_id(
                                            &mut self.simulation_state,
                                            grid_id,
                                            final_id,
                                        );
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
                                        self.update_inserters(
                                            InserterUpdateInfo::NewAssembler { pos, size: (3, 3) },
                                            data_store,
                                        );
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
                                        self.update_inserters(
                                            InserterUpdateInfo::NewAssembler { pos, size: (3, 3) },
                                            data_store,
                                        );
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

                            self.world.add_entity(
                                new_pole_entity,
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
                                        let new_id = Self::add_assembler_to_sim(
                                            &mut self.simulation_state,
                                            recipe,
                                            *power_grid_id,
                                            data_store,
                                        );

                                        *info = AssemblerInfo::Powered(new_id);

                                        needs_update = true;
                                    },
                                    AssemblerInfo::Powered(assembler_id) => {
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
        data_store: &DataStore<ItemIdxType, RecipeIdxType>,
    ) -> AssemblerID<RecipeIdxType> {
        sim_state.factory.power_grids.power_grids[power_grid_id as usize]
            .as_mut()
            .unwrap()
            .add_assembler(power_grid_id, recipe, data_store)
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

                Entity::Assembler {
                    pos,
                    info: AssemblerInfo::Powered(id),
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

                Entity::Assembler {
                    pos,
                    info: AssemblerInfo::Powered(id),
                    // FXIME: Translate the recipe_idx to
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
