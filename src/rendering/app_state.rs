use std::{borrow::Borrow, marker::PhantomData, ops::ControlFlow};

use crate::{
    belt::{belt::Belt, splitter::Splitter, BeltStore, MultiBeltStore},
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
                AssemblerID, AssemblerInfo, AttachedInserter, BeltId, BeltTileId, Dir, Entity,
                InserterInfo, World,
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
use log::{info, warn};
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
    pub belts: BeltStore<RecipeIdxType>,
    pub belt_belt_inserters: BeltBeltInserterStore<ItemIdxType>,
    pub splitters: SplitterStore<ItemIdxType>,
    pub chests: FullChestStore<ItemIdxType>,
}

#[derive(Debug, Clone, serde::Deserialize, serde::Serialize)]
pub struct SplitterStore<ItemIdxType: WeakIdxTrait> {
    pub empty_splitters: Vec<Splitter>,
    // TODO: Holes
    pub splitters: Box<[Vec<Splitter>]>,
    item: PhantomData<ItemIdxType>,
}

impl<ItemIdxType: IdxTrait> SplitterStore<ItemIdxType> {
    pub fn get_splitter_belt_ids<'a>(
        &'a self,
        item: Option<Item<ItemIdxType>>,
        id: usize,
    ) -> [impl IntoIterator<Item = BeltTileId<ItemIdxType>> + use<'a, ItemIdxType>; 2] {
        let index_to_id = move |index| match item {
            Some(item) => BeltTileId::BeltId(BeltId { item, index }),
            None => BeltTileId::EmptyBeltId(index),
        };

        match item {
            Some(item) => [
                self.splitters[Into::<usize>::into(item.id)][id]
                    .input_belts
                    .iter()
                    .copied()
                    .map(index_to_id),
                self.splitters[Into::<usize>::into(item.id)][id]
                    .output_belts
                    .iter()
                    .copied()
                    .map(index_to_id),
            ],
            None => [
                self.empty_splitters[id]
                    .input_belts
                    .iter()
                    .copied()
                    .map(index_to_id),
                self.empty_splitters[id]
                    .output_belts
                    .iter()
                    .copied()
                    .map(index_to_id),
            ],
        }
    }
}

#[derive(Debug, Clone, serde::Deserialize, serde::Serialize)]
pub struct BeltBeltInserterStore<ItemIdxType: WeakIdxTrait> {
    // TODO: Holes
    pub inserters: Box<[Vec<(BeltBeltInserter, BeltBeltInserterInfo<ItemIdxType>)>]>,
}

impl<ItemIdxType: IdxTrait> BeltBeltInserterStore<ItemIdxType> {
    pub fn update<RecipeIdxType: IdxTrait>(
        &mut self,
        belts: &mut BeltStore<RecipeIdxType>,
        data_store: &DataStore<ItemIdxType, RecipeIdxType>,
    ) {
        self.inserters
            .par_iter_mut()
            .zip(belts.belts.par_iter_mut())
            .for_each(|(inserters, belts)| {
                for ins in inserters {
                    let [source, dest] = if ins.1.source.0 == ins.1.dest.0 {
                        // We are taking and inserting from the same belt
                        debug_assert!(ins.1.source.1 != ins.1.dest.1);
                        belts.belts[ins.1.source.0]
                            .get_two([ins.1.source.1 as usize, ins.1.dest.1 as usize])
                    } else {
                        let [source_belt, dest_belt] = belts
                            .belts
                            .get_many_mut([ins.1.source.0, ins.1.dest.0])
                            .expect("Index out of bounds");
                        [
                            source_belt.get_mut(ins.1.source.1),
                            dest_belt.get_mut(ins.1.dest.1),
                        ]
                    };
                    ins.0.update(source, dest, todo!());
                }
            });
    }
}

#[derive(Debug, Clone, serde::Deserialize, serde::Serialize)]
pub struct BeltBeltInserterInfo<ItemIdxType: WeakIdxTrait> {
    source: (usize, u16),
    dest: (usize, u16),
    cooldown: u8,
    item: PhantomData<ItemIdxType>,
}

pub struct BeltBeltInserterAdditionInfo {
    pub cooldown: u8,
}

impl<ItemIdxType: IdxTrait, RecipeIdxType: IdxTrait> Factory<ItemIdxType, RecipeIdxType> {
    fn new(data_store: &DataStore<ItemIdxType, RecipeIdxType>) -> Self {
        Self {
            power_grids: PowerGridStorage::new(),
            belts: BeltStore {
                empty_belts: vec![],
                empty_belt_holes: vec![],
                belts: vec![MultiBeltStore::default(); data_store.recipe_timers.len()]
                    .into_boxed_slice(),
            },
            belt_belt_inserters: BeltBeltInserterStore {
                inserters: vec![Vec::new(); data_store.item_names.len()].into_boxed_slice(),
            },
            splitters: SplitterStore {
                empty_splitters: vec![],
                splitters: vec![Vec::new(); data_store.item_names.len()].into_boxed_slice(),
                item: PhantomData,
            },
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
            .belts
            .par_iter_mut()
            .zip(storages_by_item)
            .zip(self.belt_belt_inserters.inserters.par_iter_mut())
            .zip(self.splitters.splitters.par_iter_mut())
            .enumerate()
            .for_each(
                |(item_id, (((belt_store, item_storages), belt_belt_inserters), splitters))| {
                    let grid_size = grid_size(
                        Item {
                            id: item_id.try_into().unwrap(),
                        },
                        data_store,
                    );
                    let num_recipes = num_recipes(
                        Item {
                            id: item_id.try_into().unwrap(),
                        },
                        data_store,
                    );

                    for belt in &mut belt_store.belts {
                        belt.update();
                        belt.update_inserters(
                            item_storages,
                            num_grids_total,
                            num_recipes,
                            grid_size,
                        );
                    }

                    for (ins, info) in belt_belt_inserters {
                        let [source, dest] = if info.source.0 == info.dest.0 {
                            assert_ne!(
                                info.source.1, info.dest.1,
                                "An inserter cannot take and drop off on the same tile"
                            );
                            // We are taking and placing onto the same belt
                            let belt = &mut belt_store.belts[info.source.0];

                            belt.get_two([info.source.1.into(), info.dest.1.into()])
                        } else {
                            let [inp, out] = belt_store
                                .belts
                                .get_many_mut([info.source.0, info.dest.0])
                                .unwrap();

                            [inp.get_mut(info.source.1), out.get_mut(info.dest.1)]
                        };
                        ins.update(source, dest, info.cooldown);
                    }

                    for splitter in splitters {
                        splitter.update(belt_store);
                    }
                },
            );
    }

    pub fn add_belt_belt_inserter(
        &mut self,
        from: (BeltId<ItemIdxType>, u16),
        to: (BeltId<ItemIdxType>, u16),
        info: BeltBeltInserterAdditionInfo,
    ) -> usize {
        assert_eq!(from.0.item, to.0.item);
        self.belt_belt_inserters.inserters[Into::<usize>::into(from.0.item.id)].push((
            BeltBeltInserter::new(),
            BeltBeltInserterInfo {
                source: (from.0.index, from.1),
                dest: (to.0.index, to.1),
                cooldown: info.cooldown,
                item: PhantomData,
            },
        ));
        self.belt_belt_inserters.inserters[Into::<usize>::into(from.0.item.id)].len() - 1
    }

    pub fn add_splitter(&mut self, splitter: Splitter, item: Option<Item<ItemIdxType>>) -> usize {
        match item {
            Some(item) => {
                self.splitters.splitters[Into::<usize>::into(item.id)].push(splitter);
                self.splitters.splitters[Into::<usize>::into(item.id)].len() - 1
            },
            None => {
                self.splitters.empty_splitters.push(splitter);
                self.splitters.empty_splitters.len() - 1
            },
        }
    }

    pub fn remove_splitter(&mut self, item: Option<Item<ItemIdxType>>, index: usize) {
        todo!()
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

                            let powered_by = self.world.is_powered_by(position, (3, 3), data_store);

                            if let Some(pole_position) = powered_by {
                                self.world.add_entity(
                                    crate::frontend::world::tile::Entity::Assembler {
                                        pos: position,
                                        info: AssemblerInfo::PoweredNoRecipe(
                                            self.simulation_state
                                                .factory
                                                .power_grids
                                                .pole_pos_to_grid_id[&pole_position],
                                        ),
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

                            // Check which poles are in range to connect to
                            let connection_candidates: Vec<_> = self
                                .world
                                .get_power_poles_which_could_connect_to_pole_at(
                                    pos,
                                    data_store.power_pole_data[usize::from(ty)].size,
                                    data_store.power_pole_data[usize::from(ty)].connection_range,
                                    data_store,
                                )
                                .into_iter()
                                .map(|e| e.get_pos())
                                .collect();

                            if let Some((pole_updates, storage_updates)) = self
                                .simulation_state
                                .factory
                                .power_grids
                                .add_pole(pos, connection_candidates.iter().copied(), data_store)
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
                                .pole_pos_to_grid_id[&pos];

                            // Handle Entities that are newly powered
                            let power_range = data_store.power_pole_data[ty as usize].power_range;
                            self.world.mutate_entities_colliding_with(
                                Position {
                                    x: pos.x - power_range as usize,
                                    y: pos.y - power_range as usize,
                                },
                                (2 * power_range + 1, 2 * power_range + 1),
                                data_store,
                                |e| {
                                    match e {
                                        Entity::Assembler { pos, info } => match info {
                                            AssemblerInfo::UnpoweredNoRecipe => {
                                                *info = AssemblerInfo::PoweredNoRecipe(grid);
                                            },
                                            AssemblerInfo::Unpowered(recipe) => {
                                                let assembler_id = self
                                                    .simulation_state
                                                    .factory
                                                    .power_grids
                                                    .power_grids
                                                    [grid as usize]
                                                    .as_mut()
                                                    .unwrap()
                                                    .add_assembler(grid, *recipe, data_store);
                                                *info = AssemblerInfo::Powered(assembler_id);
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
                                    pos,
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
                                    AssemblerInfo::PoweredNoRecipe(grid) => {
                                        let new_id = Self::add_assembler_to_sim(
                                            &mut self.simulation_state,
                                            recipe,
                                            *grid,
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
                ActionType::Remove(pos) => {
                    self.world
                        .remove_entity_at(pos, &mut self.simulation_state, data_store);
                },
            }
        }
    }

    pub fn update(&mut self, data_store: &DataStore<ItemIdxType, RecipeIdxType>) {
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

                Entity::Roboport { ty, pos, power_grid, network, id } => {
                    // TODO:
                    warn!("It is currently not possible to add or remove bots from roboports using inserters");
                    None
                }

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
                            .filter_map(|(dir, item)| (*dir == ItemRecipeDir::Out).then_some(*item))
                            .collect(),
                    ),
                )),
                Entity::Assembler { .. } => None,
                Entity::Belt {
                    id: BeltTileId::BeltId(id),
                    belt_pos,
                    ..
                }
                | Entity::Underground {
                    id: BeltTileId::BeltId(id),
                    belt_pos,
                    ..
                } => Some((
                    InserterConnection::Belt(BeltTileId::BeltId(*id), *belt_pos),
                    Some(vec![id.item]),
                )),
                Entity::Belt {
                    id: BeltTileId::EmptyBeltId(idx),
                    belt_pos,
                    ..
                }
                | Entity::Underground {
                    id: BeltTileId::EmptyBeltId(idx),
                    belt_pos,
                    ..
                } => Some((
                    InserterConnection::Belt(BeltTileId::EmptyBeltId(*idx), *belt_pos),
                    None,
                )),
                Entity::Splitter {
                    pos,
                    direction,
                    item,
                    id,
                } => todo!("Inserters on splitters"),
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
                    id: BeltTileId::BeltId(id),
                    belt_pos,
                    ..
                }
                | Entity::Underground {
                    id: BeltTileId::BeltId(id),
                    belt_pos,
                    ..
                } => Some((
                    InserterConnection::Belt(BeltTileId::BeltId(*id), *belt_pos),
                    Some(vec![id.item]),
                )),
                Entity::Belt {
                    id: BeltTileId::EmptyBeltId(idx),
                    belt_pos,
                    ..
                }
                | Entity::Underground {
                    id: BeltTileId::EmptyBeltId(idx),
                    belt_pos,
                    ..
                } => Some((
                    InserterConnection::Belt(BeltTileId::EmptyBeltId(*idx), *belt_pos),
                    None,
                )),
                Entity::Splitter {
                    pos,
                    direction,
                    item,
                    id,
                } => todo!(),
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
                let (belt, id) = self
                    .simulation_state
                    .factory
                    .belts
                    .get_mut_and_instantiate(filter, start_belt_id);

                debug_assert_eq!(filter, id.item);
                let index = belt.add_out_inserter(start_belt_pos - 1, dest_storage);

                *info = InserterInfo::Attached(AttachedInserter::BeltStorage {
                    id: BeltTileId::BeltId(id),
                    belt_pos: start_belt_pos - 1,
                });
                if BeltTileId::BeltId(id) != start_belt_id {
                    self.world
                        .update_belt_id(start_belt_id, BeltTileId::BeltId(id));
                }
            },
            (
                InserterConnection::Storage(start_storage_untranslated),
                InserterConnection::Belt(dest_belt_id, dest_belt_pos),
            ) => {
                let start_storage = start_storage_untranslated.translate(filter, data_store);
                let (belt, id) = self
                    .simulation_state
                    .factory
                    .belts
                    .get_mut_and_instantiate(filter, dest_belt_id);
                debug_assert_eq!(filter, id.item);

                let index = belt.add_in_inserter(dest_belt_pos - 1, start_storage);
                *info = InserterInfo::Attached(AttachedInserter::BeltStorage {
                    id: BeltTileId::BeltId(id),
                    belt_pos: dest_belt_pos - 1,
                });
                if BeltTileId::BeltId(id) != dest_belt_id {
                    self.world
                        .update_belt_id(dest_belt_id, BeltTileId::BeltId(id));
                }
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
    use proptest::proptest;
    static DATA_STORE: LazyLock<DataStore<u8, u8>> =
        LazyLock::new(|| get_raw_data_test().turn::<u8, u8>());

    proptest! {
        #[test]
        fn test_random_blueprint(base_pos in random_position(), blueprint in random_blueprint_strategy::<u8, u8>(0..100, &DATA_STORE)) {

            let mut game_state = GameState::new(&DATA_STORE);

            blueprint.apply(base_pos, &mut game_state, &DATA_STORE);

        }
    }
}
