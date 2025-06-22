use crate::data::AllowedFluidDirection;
use crate::liquid::connection_logic::can_fluid_tanks_connect_to_single_connection;
use crate::liquid::FluidConnectionDir;
use crate::{
    belt::{BeltBeltInserterInfo, BeltStore},
    blueprint::Blueprint,
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
    inserter::{
        belt_belt_inserter::BeltBeltInserter, storage_storage_inserter::StorageStorageInserter,
        FakeUnionStorage, Storage, MOVETIME,
    },
    item::{usize_from, IdxTrait, Item, Recipe, WeakIdxTrait},
    liquid::connection_logic::can_fluid_tanks_connect,
    network_graph::WeakIndex,
    power::{power_grid::PowerGridIdentifier, PowerGridStorage},
    research::{ResearchProgress, TechState, Technology},
    statistics::{
        consumption::ConsumptionInfo, production::ProductionInfo, recipe::RecipeTickInfo,
        GenStatistics, Timeline,
    },
    storage_list::{
        full_to_by_item, grid_size, num_recipes, sizes, storages_by_item, SingleItemStorages,
    },
};
use crate::{
    item::Indexable,
    liquid::{CannotMixFluidsError, FluidSystemStore},
};
use itertools::Itertools;
use log::{info, trace, warn};
use rayon::iter::{IndexedParallelIterator, IntoParallelRefMutIterator, ParallelIterator};
use std::iter;
use std::{
    borrow::Borrow,
    fs::File,
    ops::ControlFlow,
    time::{Duration, Instant},
};

use crate::frontend::action::place_tile::PositionInfo;

use std::ops::AddAssign;

#[derive(Debug, Clone, serde::Deserialize, serde::Serialize)]
pub struct GameState<ItemIdxType: WeakIdxTrait, RecipeIdxType: WeakIdxTrait> {
    pub current_tick: u64,

    pub world: World<ItemIdxType, RecipeIdxType>,
    pub simulation_state: SimulationState<ItemIdxType, RecipeIdxType>,

    pub statistics: GenStatistics,

    pub update_times: Timeline<UpdateTime>,
    #[serde(skip)]
    last_update_time: Option<Instant>,
}

#[derive(Debug, Clone, Default, serde::Deserialize, serde::Serialize)]
pub struct UpdateTime {
    pub dur: Duration,
}

impl<'a> AddAssign<&'a UpdateTime> for UpdateTime {
    fn add_assign(&mut self, rhs: &'a UpdateTime) {
        self.dur += rhs.dur;
    }
}

impl<ItemIdxType: IdxTrait, RecipeIdxType: IdxTrait> GameState<ItemIdxType, RecipeIdxType> {
    #[must_use]
    pub fn new(data_store: &DataStore<ItemIdxType, RecipeIdxType>) -> Self {
        Self {
            current_tick: 0,
            world: World::new(),
            simulation_state: SimulationState::new(data_store),
            statistics: GenStatistics::new(data_store),
            update_times: Timeline::new(false, data_store),
            last_update_time: None,
        }
    }

    #[must_use]
    pub fn new_with_production(data_store: &DataStore<ItemIdxType, RecipeIdxType>) -> Self {
        let mut ret = Self {
            current_tick: 0,
            world: World::new(),
            simulation_state: SimulationState::new(data_store),
            statistics: GenStatistics::new(data_store),
            update_times: Timeline::new(false, data_store),
            last_update_time: None,
        };

        let file = File::open("test_blueprints/red_sci.bp").unwrap();
        let bp: Blueprint<ItemIdxType, RecipeIdxType> = ron::de::from_reader(file).unwrap();

        puffin::set_scopes_on(false);
        for y_pos in (1590..30000).step_by(7) {
            for x_pos in (1590..3000).step_by(20) {
                if rand::random::<u16>() < u16::MAX / 100 {
                    ret.update(data_store);
                }

                bp.apply(Position { x: x_pos, y: y_pos }, &mut ret, data_store);
            }
        }
        puffin::set_scopes_on(true);

        ret
    }

    #[must_use]
    pub fn new_with_beacon_production(data_store: &DataStore<ItemIdxType, RecipeIdxType>) -> Self {
        Self::new_with_beacon_red_green_production_many_grids(data_store)
    }

    #[must_use]
    pub fn new_with_beacon_red_green_production_many_grids(
        data_store: &DataStore<ItemIdxType, RecipeIdxType>,
    ) -> Self {
        let mut ret = Self {
            current_tick: 0,
            world: World::new(),
            simulation_state: SimulationState::new(data_store),
            statistics: GenStatistics::new(data_store),
            update_times: Timeline::new(false, data_store),
            last_update_time: None,
        };

        let file = File::open("test_blueprints/red_and_green.bp").unwrap();
        let bp: Blueprint<ItemIdxType, RecipeIdxType> = ron::de::from_reader(file).unwrap();

        puffin::set_scopes_on(false);
        for y_start in (0..24_000).step_by(4_000) {
            for y_pos in (1590..4000).step_by(40) {
                for x_start in (0..24_000).step_by(4_000) {
                    for x_pos in (1590..4000).step_by(50) {
                        while rand::random::<u16>() < u16::MAX / 200 {
                            ret.update(data_store);
                        }
                        bp.apply(
                            Position {
                                x: x_start + x_pos,
                                y: y_start + y_pos,
                            },
                            &mut ret,
                            data_store,
                        );
                    }
                }
            }
        }
        puffin::set_scopes_on(true);

        ret
    }

    #[must_use]
    pub fn new_with_beacon_belt_production(
        data_store: &DataStore<ItemIdxType, RecipeIdxType>,
    ) -> Self {
        let mut ret = Self {
            current_tick: 0,
            world: World::new(),
            simulation_state: SimulationState::new(data_store),
            statistics: GenStatistics::new(data_store),
            update_times: Timeline::new(false, data_store),
            last_update_time: None,
        };

        let file = File::open("test_blueprints/red_sci_with_beacons_and_belts.bp").unwrap();
        let bp: Blueprint<ItemIdxType, RecipeIdxType> = ron::de::from_reader(file).unwrap();

        puffin::set_scopes_on(false);
        for y_start in (0..40_000).step_by(6_000) {
            for y_pos in (1590..6000).step_by(10) {
                for x_pos in (1590..3000).step_by(60) {
                    if rand::random::<u16>() == 0 {
                        ret.update(data_store);
                    }

                    bp.apply(
                        Position {
                            x: x_pos,
                            y: y_start + y_pos,
                        },
                        &mut ret,
                        data_store,
                    );
                }
            }
        }
        puffin::set_scopes_on(true);

        ret
    }

    #[must_use]
    pub fn new_with_lots_of_belts(data_store: &DataStore<ItemIdxType, RecipeIdxType>) -> Self {
        let mut ret = Self {
            current_tick: 0,
            world: World::new(),
            simulation_state: SimulationState::new(data_store),
            statistics: GenStatistics::new(data_store),
            update_times: Timeline::new(false, data_store),
            last_update_time: None,
        };

        let file = File::open("test_blueprints/lots_of_belts.bp").unwrap();
        let bp: Blueprint<ItemIdxType, RecipeIdxType> = ron::de::from_reader(file).unwrap();

        puffin::set_scopes_on(false);
        for y_pos in (1600..60_000).step_by(3) {
            ret.update(data_store);
            bp.apply(Position { x: 1600, y: y_pos }, &mut ret, data_store);
        }
        puffin::set_scopes_on(true);

        ret
    }

    #[must_use]
    pub fn new_with_tons_of_solar(data_store: &DataStore<ItemIdxType, RecipeIdxType>) -> Self {
        let mut ret = Self {
            current_tick: 0,
            world: World::new(),
            simulation_state: SimulationState::new(data_store),
            statistics: GenStatistics::new(data_store),
            update_times: Timeline::new(false, data_store),
            last_update_time: None,
        };

        let file = File::open("test_blueprints/solar_farm.bp").unwrap();
        let bp: Blueprint<ItemIdxType, RecipeIdxType> = ron::de::from_reader(file).unwrap();

        puffin::set_scopes_on(false);
        for y_pos in (1600..30_000).step_by(18) {
            for x_pos in (1600..30_000).step_by(18) {
                bp.apply(Position { x: x_pos, y: y_pos }, &mut ret, data_store);
            }
        }
        puffin::set_scopes_on(true);

        ret
    }

    #[must_use]
    pub fn new_eight_beacon_factory(data_store: &DataStore<ItemIdxType, RecipeIdxType>) -> Self {
        let mut ret = Self {
            current_tick: 0,
            world: World::new(),
            simulation_state: SimulationState::new(data_store),
            statistics: GenStatistics::new(data_store),
            update_times: Timeline::new(false, data_store),
            last_update_time: None,
        };

        let iron_ore = File::open("test_blueprints/iron_ore.bp").unwrap();
        let iron_ore: Blueprint<ItemIdxType, RecipeIdxType> =
            ron::de::from_reader(iron_ore).unwrap();

        let iron_plate = File::open("test_blueprints/iron_plate.bp").unwrap();
        let iron_plate: Blueprint<ItemIdxType, RecipeIdxType> =
            ron::de::from_reader(iron_plate).unwrap();

        let gears = File::open("test_blueprints/gears.bp").unwrap();
        let gears: Blueprint<ItemIdxType, RecipeIdxType> = ron::de::from_reader(gears).unwrap();

        let copper_ore = File::open("test_blueprints/copper_ore.bp").unwrap();
        let copper_ore: Blueprint<ItemIdxType, RecipeIdxType> =
            ron::de::from_reader(copper_ore).unwrap();

        let copper_plate = File::open("test_blueprints/copper_plate.bp").unwrap();
        let copper_plate: Blueprint<ItemIdxType, RecipeIdxType> =
            ron::de::from_reader(copper_plate).unwrap();

        puffin::set_scopes_on(false);
        for y_pos in (1600..=1600).step_by(18) {
            iron_ore.apply(Position { x: 1600, y: y_pos }, &mut ret, data_store);
            iron_plate.apply(
                Position {
                    x: 1644,
                    y: y_pos + 1,
                },
                &mut ret,
                data_store,
            );
            gears.apply(
                Position {
                    x: 1699,
                    y: y_pos - 3,
                },
                &mut ret,
                data_store,
            );

            copper_ore.apply(
                Position {
                    x: 1600,
                    y: y_pos + 10,
                },
                &mut ret,
                data_store,
            );
            copper_plate.apply(
                Position {
                    x: 1644,
                    y: y_pos + 11,
                },
                &mut ret,
                data_store,
            );
        }
        puffin::set_scopes_on(true);

        ret
    }

    pub fn new_with_bp(data_store: &DataStore<ItemIdxType, RecipeIdxType>, bp_path: &str) -> Self {
        let mut ret = Self {
            current_tick: 0,
            world: World::new(),
            simulation_state: SimulationState::new(data_store),
            statistics: GenStatistics::new(data_store),
            update_times: Timeline::new(false, data_store),
            last_update_time: None,
        };

        let file = File::open(bp_path).unwrap();
        let bp: Blueprint<ItemIdxType, RecipeIdxType> = ron::de::from_reader(file).unwrap();

        bp.apply(Position { x: 1590, y: 1590 }, &mut ret, data_store);
        // bp.apply(Position { x: 1600, y: 1590 }, &mut ret, data_store);
        // bp.apply(Position { x: 1610, y: 1590 }, &mut ret, data_store);
        // bp.apply(Position { x: 1620, y: 1590 }, &mut ret, data_store);
        // bp.apply(Position { x: 1630, y: 1590 }, &mut ret, data_store);

        // bp.apply(Position { x: 1590, y: 1600 }, &mut ret, data_store);
        // bp.apply(Position { x: 1600, y: 1600 }, &mut ret, data_store);
        // bp.apply(Position { x: 1610, y: 1600 }, &mut ret, data_store);
        // bp.apply(Position { x: 1620, y: 1600 }, &mut ret, data_store);
        // bp.apply(Position { x: 1630, y: 1600 }, &mut ret, data_store);

        // bp.apply(Position { x: 1590, y: 1610 }, &mut ret, data_store);
        // bp.apply(Position { x: 1600, y: 1610 }, &mut ret, data_store);
        // bp.apply(Position { x: 1610, y: 1610 }, &mut ret, data_store);
        // bp.apply(Position { x: 1620, y: 1610 }, &mut ret, data_store);
        // bp.apply(Position { x: 1630, y: 1610 }, &mut ret, data_store);

        // bp.apply(Position { x: 1590, y: 1620 }, &mut ret, data_store);
        // bp.apply(Position { x: 1600, y: 1620 }, &mut ret, data_store);
        // bp.apply(Position { x: 1610, y: 1620 }, &mut ret, data_store);
        // bp.apply(Position { x: 1620, y: 1620 }, &mut ret, data_store);
        // bp.apply(Position { x: 1630, y: 1620 }, &mut ret, data_store);

        ret
    }
}

#[derive(Debug, Clone, serde::Deserialize, serde::Serialize)]
pub struct SimulationState<ItemIdxType: WeakIdxTrait, RecipeIdxType: WeakIdxTrait> {
    tech_state: TechState,
    pub factory: Factory<ItemIdxType, RecipeIdxType>,
    // TODO:
}

impl<ItemIdxType: IdxTrait, RecipeIdxType: IdxTrait> SimulationState<ItemIdxType, RecipeIdxType> {
    #[must_use]
    pub fn new(data_store: &DataStore<ItemIdxType, RecipeIdxType>) -> Self {
        Self {
            tech_state: TechState {
                current_technology: Some(Technology { id: 0 }),
            },
            factory: Factory::new(data_store),
        }
    }
}

#[derive(Debug, Clone, serde::Deserialize, serde::Serialize)]
pub struct Factory<ItemIdxType: WeakIdxTrait, RecipeIdxType: WeakIdxTrait> {
    pub power_grids: PowerGridStorage<ItemIdxType, RecipeIdxType>,
    pub belts: BeltStore<ItemIdxType>,
    pub storage_storage_inserters: StorageStorageInserterStore,
    pub chests: FullChestStore<ItemIdxType>,

    pub fluid_store: FluidSystemStore<ItemIdxType>,
}

#[derive(Debug, Clone, serde::Deserialize, serde::Serialize)]
pub struct StorageStorageInserterStore {
    pub inserters: Box<[Vec<StorageStorageInserter>]>,
    holes: Box<[Vec<usize>]>,
}

impl StorageStorageInserterStore {
    fn new<ItemIdxType: IdxTrait, RecipeIdxType: IdxTrait>(
        data_store: &DataStore<ItemIdxType, RecipeIdxType>,
    ) -> Self {
        Self {
            inserters: vec![vec![]; data_store.item_names.len()].into_boxed_slice(),
            holes: vec![vec![]; data_store.item_names.len()].into_boxed_slice(),
        }
    }

    #[profiling::function]
    fn update<'a, 'b, ItemIdxType: IdxTrait, RecipeIdxType: IdxTrait>(
        &mut self,
        full_storages: impl IndexedParallelIterator<Item = SingleItemStorages<'a, 'b>>,
        num_grids_total: usize,
        data_store: &DataStore<ItemIdxType, RecipeIdxType>,
    ) where
        'b: 'a,
    {
        self.inserters
            .par_iter_mut()
            .zip(self.holes.par_iter_mut())
            .zip(full_storages)
            .enumerate()
            .for_each(|(item_id, ((ins, holes), storages))| {
                profiling::scope!(
                    "StorageStorage Inserter Update",
                    format!("Item: {}", data_store.item_names[item_id]).as_str()
                );

                let item = Item {
                    id: item_id.try_into().unwrap(),
                };

                let grid_size = grid_size(item, data_store);
                let num_recipes = num_recipes(item, data_store);

                ins.iter_mut()
                    .enumerate()
                    // FIXME: This is awful!
                    // Ideally we could replace inserter holes with placeholder that do not do anything, but I don't quite know how those would work.
                    .filter_map(|(i, v)| (!holes.contains(&i)).then_some(v))
                    .for_each(|ins| {
                        ins.update(storages, MOVETIME, num_grids_total, num_recipes, grid_size);
                    });
            });
    }

    pub fn add_ins<ItemIdxType: IdxTrait, RecipeIdxType: IdxTrait>(
        &mut self,
        item: Item<ItemIdxType>,
        start: Storage<RecipeIdxType>,
        dest: Storage<RecipeIdxType>,
        data_store: &DataStore<ItemIdxType, RecipeIdxType>,
    ) -> usize {
        let idx = if let Some(hole_idx) = self.holes[usize_from(item.id)].pop() {
            self.inserters[usize_from(item.id)][hole_idx] = StorageStorageInserter::new(
                FakeUnionStorage::from_storage_with_statics_at_zero(item, start, data_store),
                FakeUnionStorage::from_storage_with_statics_at_zero(item, dest, data_store),
            );

            hole_idx
        } else {
            self.inserters[usize_from(item.id)].push(StorageStorageInserter::new(
                FakeUnionStorage::from_storage_with_statics_at_zero(item, start, data_store),
                FakeUnionStorage::from_storage_with_statics_at_zero(item, dest, data_store),
            ));

            self.inserters[usize_from(item.id)].len() - 1
        };

        idx
    }

    pub fn remove_ins<ItemIdxType: IdxTrait>(&mut self, item: Item<ItemIdxType>, index: usize) {
        assert!(!self.holes[usize_from(item.id)].contains(&index));
        self.holes[usize_from(item.id)].push(index);
    }

    pub fn update_inserter_src<ItemIdxType: IdxTrait, RecipeIdxType: IdxTrait>(
        &mut self,
        item: Item<ItemIdxType>,
        index: usize,
        new_src: Storage<RecipeIdxType>,
        data_store: &DataStore<ItemIdxType, RecipeIdxType>,
    ) {
        self.inserters[item.into_usize()][index].storage_id_in =
            FakeUnionStorage::from_storage_with_statics_at_zero(item, new_src, data_store);
    }
    pub fn update_inserter_dest<ItemIdxType: IdxTrait, RecipeIdxType: IdxTrait>(
        &mut self,
        item: Item<ItemIdxType>,
        index: usize,
        new_dest: Storage<RecipeIdxType>,
        data_store: &DataStore<ItemIdxType, RecipeIdxType>,
    ) {
        self.inserters[item.into_usize()][index].storage_id_out =
            FakeUnionStorage::from_storage_with_statics_at_zero(item, new_dest, data_store);
    }
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
            storage_storage_inserters: StorageStorageInserterStore::new(data_store),
            chests: FullChestStore {
                stores: (0..data_store.item_names.len())
                    .map(|id| Item {
                        id: id.try_into().unwrap(),
                    })
                    .map(|item| MultiChestStore::new(item, data_store))
                    .collect(),
            },

            fluid_store: FluidSystemStore::new(data_store),
        }
    }

    #[profiling::function]
    fn belt_update<'a>(&mut self, data_store: &DataStore<ItemIdxType, RecipeIdxType>) {
        let num_grids_total = self.power_grids.power_grids.len();
        let mut all_storages = {
            profiling::scope!("Generate all_storages list");
            storages_by_item(&mut self.power_grids, &mut self.chests, data_store)
        };
        let sizes: Vec<_> = sizes(data_store, num_grids_total).into_iter().collect();
        // dbg!(&all_storages);
        assert_eq!(sizes.len(), data_store.item_names.len());
        let mut storages_by_item: Box<[_]> = {
            profiling::scope!("Sort storages by item");
            let storages_by_item = full_to_by_item(&mut all_storages, &sizes);
            storages_by_item.into_iter().collect()
        };

        self.storage_storage_inserters.update(
            storages_by_item.par_iter_mut().map(|v| &mut **v),
            num_grids_total,
            data_store,
        );

        self.belts.update(
            num_grids_total,
            storages_by_item.par_iter_mut().map(|v| &mut **v),
            data_store,
        );
    }
}

pub enum AppState {
    Ingame,
    Loading,
}

#[derive(Debug, Clone, Copy)]
enum InserterUpdateInfo {
    AssemblerRecipeChanged { pos: Position, size: (u8, u8) },
    NewAssembler { pos: Position, size: (u8, u8) },
    NewBelt { pos: Position },
}

#[derive(Debug)]
pub enum InstantiateInserterError {
    NotUnattachedInserter,
    SourceMissing,
    DestMissing,
    PleaseSpecifyFilter,
    ItemConflict,
}

impl<ItemIdxType: IdxTrait, RecipeIdxType: IdxTrait> GameState<ItemIdxType, RecipeIdxType> {
    #[allow(clippy::too_many_lines)]
    #[profiling::function]
    pub fn apply_actions(
        &mut self,
        actions: impl IntoIterator<Item = impl Borrow<ActionType<ItemIdxType, RecipeIdxType>>>,
        data_store: &DataStore<ItemIdxType, RecipeIdxType>,
    ) {
        // let num_assemblers = self
        //     .world
        //     .get_chunks()
        //     .into_iter()
        //     .map(|c| {
        //         c.get_entities()
        //             .into_iter()
        //             .filter(|e| matches!(e, Entity::Assembler { .. }))
        //     })
        //     .count();
        // panic!("{}", num_assemblers);

        for action in actions {
            // FIXME: I just clone for now
            match action.borrow().clone() {
                ActionType::PlaceFloorTile(place_floor_tile_by_hand_info) => {
                    let num_items_needed = match place_floor_tile_by_hand_info.ghost_info.position {
                        PositionInfo::Rect { pos, width, height } => width * height,
                        PositionInfo::Single { pos } => 1,
                        PositionInfo::List { ref positions } => positions.len().try_into().unwrap(),
                    };

                    // TODO: Check player inventory for enough resources

                    match place_floor_tile_by_hand_info.ghost_info.position {
                        PositionInfo::Rect { pos, width, height } => {
                            for x in pos.x..(pos.x + i32::try_from(width).unwrap()) {
                                for y in pos.y..(pos.y + i32::try_from(height).unwrap()) {
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
                ActionType::SetChestSlotLimit { pos, num_slots } => self
                    .world
                    .mutate_entities_colliding_with(pos, (1, 1), data_store, |e| {
                        match e {
                            Entity::Chest {
                                ty,
                                pos,
                                item,
                                slot_limit,
                            } => {
                                if let Some((item, index)) = item {
                                    let removed_items = self.simulation_state.factory.chests.stores
                                        [usize_from(item.id)]
                                    .change_chest_size(
                                        *index,
                                        data_store.item_stack_sizes[usize_from(item.id)] as u16
                                            * num_slots as u16,
                                    );
                                }
                                *slot_limit = num_slots;
                            },
                            _ => {
                                warn!("Tried to set slot limit on non chest");
                            },
                        }
                        ControlFlow::Break(())
                    }),
                ActionType::PlaceEntity(place_entity_info) => match place_entity_info.entities {
                    crate::frontend::action::place_entity::EntityPlaceOptions::Single(
                        place_entity_type,
                    ) => match place_entity_type {
                        crate::frontend::world::tile::PlaceEntityType::Assembler { pos, ty } => {
                            info!("Trying to place assembler at {pos:?}");
                            if !self.world.can_fit(
                                pos,
                                data_store.assembler_info[ty as usize].size,
                                data_store,
                            ) {
                                warn!("Tried to place assembler where it does not fit");
                                continue;
                            }

                            let powered_by = self.world.is_powered_by(
                                &self.simulation_state,
                                pos,
                                data_store.assembler_info[ty as usize].size,
                                data_store,
                            );

                            let modules = vec![
                                None;
                                data_store.assembler_info[usize::from(ty)]
                                    .num_module_slots
                                    .into()
                            ]
                            .into_boxed_slice();

                            if let Some(pole_position) = powered_by {
                                self.world.add_entity(
                                    crate::frontend::world::tile::Entity::Assembler {
                                        ty,
                                        pos,
                                        info: AssemblerInfo::PoweredNoRecipe(pole_position),
                                        modules,
                                    },
                                    &mut self.simulation_state,
                                    data_store,
                                );
                            } else {
                                self.world.add_entity(
                                    crate::frontend::world::tile::Entity::Assembler {
                                        ty,
                                        pos,
                                        info: AssemblerInfo::UnpoweredNoRecipe,
                                        modules,
                                    },
                                    &mut self.simulation_state,
                                    data_store,
                                );
                            }
                        },
                        crate::frontend::world::tile::PlaceEntityType::Inserter {
                            pos,
                            dir,
                            filter,
                        } => {
                            let ret = self.add_inserter(pos, dir, filter, data_store);
                            trace!("{:?}", ret);
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

                                    assert!(
                                        !self.simulation_state.factory.power_grids.power_grids
                                            [grid as usize]
                                            .is_placeholder
                                    );

                                    self.world
                                        .update_pole_power(pole_position, grid, data_store);
                                }

                                // Handle storage updates
                                for storage_update in storage_updates {
                                    dbg!(&storage_update);
                                    let mut entity_size = None;
                                    self.world.mutate_entities_colliding_with(storage_update.position, (1,1), data_store, |e| {
                                        match (e, storage_update.new_pg_entity.clone()) {
                                            (Entity::Assembler { ty, pos: _, info: AssemblerInfo::Powered { id, pole_position: _, weak_index }, modules: _ }, crate::power::power_grid::PowerGridEntity::Assembler { ty: _, recipe, index }) => {
                                                entity_size = Some(data_store.assembler_info[usize::from(*ty)].size);

                                                assert_eq!(id.recipe, recipe);
                                                id.grid = storage_update.new_grid;
                                                id.assembler_index = index;
                                                // FIXME: Store and update the weak_index
                                            },
                                            (Entity::Lab { pos: _, ty, modules: _, pole_position: Some((_pole_pos, weak_idx, lab_store_index)) }, crate::power::power_grid::PowerGridEntity::Lab { ty: _, index: new_idx  }) => {
                                                entity_size = Some(data_store.lab_info[usize::from(*ty)].size);


                                                *lab_store_index = new_idx;
                                                // The weak index stays the same since it it still connected to the same power pole
                                            }

                                            (_, _) => todo!("Handler storage_update {storage_update:?}")
                                        }
                                        ControlFlow::Break(())
                                    });

                                    // FIXME: Rotation
                                    let e_size = entity_size.unwrap();

                                    let inserter_range = data_store.max_inserter_search_range;

                                    self.world.mutate_entities_colliding_with(
                                        Position {
                                            x: storage_update.position.x
                                                - i32::from(inserter_range),
                                            y: storage_update.position.y
                                                - i32::from(inserter_range),
                                        },
                                        (
                                            u16::from(inserter_range) * 2 + e_size.0,
                                            u16::from(inserter_range) * 2 + e_size.1,
                                        ),
                                        data_store,
                                        |e| {
                                            match e {
                                                Entity::Inserter {
                                                    pos,
                                                    direction,
                                                    filter,
                                                    info,
                                                } => match info {
                                                    InserterInfo::NotAttached { .. } => {},
                                                    InserterInfo::Attached {
                                                        start_pos,
                                                        end_pos,
                                                        info,
                                                    } => {
                                                        if start_pos.contained_in(
                                                            storage_update.position,
                                                            e_size,
                                                        ) {
                                                            match info {
                                                                AttachedInserter::BeltStorage { id, belt_pos } => {
                                                                    let new_storage = match storage_update.new_pg_entity {
                                                                        crate::power::power_grid::PowerGridEntity::Assembler { ty, recipe, index } => Storage::Assembler { grid: storage_update.new_grid, recipe_idx_with_this_item: recipe.id, index },
                                                                        crate::power::power_grid::PowerGridEntity::Lab { ty, index } => Storage::Lab { grid: storage_update.new_grid, index },
                                                                        crate::power::power_grid::PowerGridEntity::LazyPowerProducer { item, index } => todo!(),
                                                                        crate::power::power_grid::PowerGridEntity::SolarPanel { .. } => unreachable!(),
                                                                        crate::power::power_grid::PowerGridEntity::Accumulator { .. } => unreachable!(),
                                                                        crate::power::power_grid::PowerGridEntity::Beacon { .. } => unreachable!(),
                                                                    }.translate(self.simulation_state.factory.belts.get_inserter_item(*id, *belt_pos, data_store), data_store);
                                                                    self.simulation_state.factory.belts.update_belt_storage_inserter_src(*id, *belt_pos, self.simulation_state.factory.belts.get_inserter_item(*id, *belt_pos, data_store), new_storage, data_store);
                                                                },
                                                                AttachedInserter::BeltBelt { .. } => {
                                                                    unreachable!("A BeltBelt inserter should not be pointing at a machine")
                                                                },
                                                                AttachedInserter::StorageStorage { item, inserter } => {
                                                                    let new_storage = match storage_update.new_pg_entity {
                                                                        crate::power::power_grid::PowerGridEntity::Assembler { ty, recipe, index } => Storage::Assembler { grid: storage_update.new_grid, recipe_idx_with_this_item: recipe.id, index },
                                                                        crate::power::power_grid::PowerGridEntity::Lab { ty, index } => Storage::Lab { grid: storage_update.new_grid, index },
                                                                        crate::power::power_grid::PowerGridEntity::LazyPowerProducer { item, index } => todo!(),
                                                                        crate::power::power_grid::PowerGridEntity::SolarPanel { .. } => unreachable!(),
                                                                        crate::power::power_grid::PowerGridEntity::Accumulator { .. } => unreachable!(),
                                                                        crate::power::power_grid::PowerGridEntity::Beacon { .. } => unreachable!(),
                                                                    }.translate(*item, data_store);
                                                                    self.simulation_state.factory.storage_storage_inserters.update_inserter_src(*item, *inserter, new_storage, data_store);
                                                                },
                                                            }
                                                        }

                                                        if end_pos.contained_in(
                                                            storage_update.position,
                                                            e_size,
                                                        ) {
                                                            match info {
                                                                AttachedInserter::BeltStorage { id, belt_pos } => {
                                                                    let new_storage = match storage_update.new_pg_entity {
                                                                        crate::power::power_grid::PowerGridEntity::Assembler { ty, recipe, index } => Storage::Assembler { grid: storage_update.new_grid, recipe_idx_with_this_item: recipe.id, index },
                                                                        crate::power::power_grid::PowerGridEntity::Lab { ty, index } => Storage::Lab { grid: storage_update.new_grid, index },
                                                                        crate::power::power_grid::PowerGridEntity::LazyPowerProducer { item, index } => todo!(),
                                                                        crate::power::power_grid::PowerGridEntity::SolarPanel { .. } => unreachable!(),
                                                                        crate::power::power_grid::PowerGridEntity::Accumulator { .. } => unreachable!(),
                                                                        crate::power::power_grid::PowerGridEntity::Beacon { .. } => unreachable!(),
                                                                    }.translate(self.simulation_state.factory.belts.get_inserter_item(*id, *belt_pos, data_store), data_store);
                                                                    self.simulation_state.factory.belts.update_belt_storage_inserter_dest(*id, *belt_pos, self.simulation_state.factory.belts.get_inserter_item(*id, *belt_pos, data_store), new_storage, data_store);
                                                                },
                                                                AttachedInserter::BeltBelt { item, inserter } => {
                                                                    unreachable!("A BeltBelt inserter should not be pointing at a machine")
                                                                },
                                                                AttachedInserter::StorageStorage { item, inserter } => {
                                                                    let new_storage = match storage_update.new_pg_entity {
                                                                        crate::power::power_grid::PowerGridEntity::Assembler { ty, recipe, index } => Storage::Assembler { grid: storage_update.new_grid, recipe_idx_with_this_item: recipe.id, index },
                                                                        crate::power::power_grid::PowerGridEntity::Lab { ty, index } => Storage::Lab { grid: storage_update.new_grid, index },
                                                                        crate::power::power_grid::PowerGridEntity::LazyPowerProducer { item, index } => todo!(),
                                                                        crate::power::power_grid::PowerGridEntity::SolarPanel { .. } => unreachable!(),
                                                                        crate::power::power_grid::PowerGridEntity::Accumulator { .. } => unreachable!(),
                                                                        crate::power::power_grid::PowerGridEntity::Beacon { .. } => unreachable!(),
                                                                    }.translate(*item, data_store);
                                                                    self.simulation_state.factory.storage_storage_inserters.update_inserter_dest(*item, *inserter, new_storage, data_store);
                                                                },
                                                            }
                                                        }
                                                    },
                                                },

                                                _ => {},
                                            }
                                            ControlFlow::Continue(())
                                        },
                                    );
                                }
                            } else {
                                // No updates needed
                            }

                            // Add the powerpole entity to the correct chunk
                            self.world.add_entity(
                                Entity::PowerPole {
                                    ty,
                                    pos: pole_pos,
                                    connected_power_poles: connection_candidates,
                                },
                                &mut self.simulation_state,
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
                        crate::frontend::world::tile::PlaceEntityType::Chest { pos, ty } => {
                            info!("Trying to place chest at {pos:?}");
                            if !self.world.can_fit(
                                pos,
                                data_store.chest_tile_sizes[usize::from(ty)],
                                data_store,
                            ) {
                                warn!("Tried to place chest where it does not fit");
                                continue;
                            }

                            self.world.add_entity(
                                Entity::Chest {
                                    ty,
                                    pos,
                                    item: None,
                                    slot_limit: data_store.chest_num_slots[usize::from(ty)],
                                },
                                &mut self.simulation_state,
                                data_store,
                            );
                        },
                        crate::frontend::world::tile::PlaceEntityType::SolarPanel { pos, ty } => {
                            info!("Trying to place solar_panel at {pos:?}");
                            let size = data_store.solar_panel_info[usize::from(ty)].size;
                            let size = size.into();

                            if !self.world.can_fit(pos, size, data_store) {
                                warn!("Tried to place solar_panel where it does not fit");
                                continue;
                            }

                            let powered_by = self.world.is_powered_by(
                                &self.simulation_state,
                                pos,
                                size,
                                data_store,
                            );

                            let powered_by = if let Some(pole_pos) = powered_by {
                                let grid = self
                                    .simulation_state
                                    .factory
                                    .power_grids
                                    .pole_pos_to_grid_id[&pole_pos];

                                let grid =
                                    &mut self.simulation_state.factory.power_grids.power_grids
                                        [usize::from(grid)];

                                let weak_idx = grid.add_solar_panel(pos, ty, pole_pos, data_store);

                                Some((pole_pos, weak_idx))
                            } else {
                                None
                            };

                            self.world.add_entity(
                                Entity::SolarPanel {
                                    pos,
                                    ty,
                                    pole_position: powered_by,
                                },
                                &mut self.simulation_state,
                                data_store,
                            );
                        },
                        crate::frontend::world::tile::PlaceEntityType::Lab { pos, ty } => {
                            info!("Trying to place lab at {pos:?}");
                            if !self.world.can_fit(
                                pos,
                                data_store.lab_info[usize::from(ty)].size,
                                data_store,
                            ) {
                                warn!("Tried to place lab where it does not fit");
                                continue;
                            }

                            let modules =
                                vec![
                                    None;
                                    data_store.lab_info[usize::from(ty)].num_module_slots.into()
                                ]
                                .into_boxed_slice();

                            let powered_by = self.world.is_powered_by(
                                &self.simulation_state,
                                pos,
                                data_store.lab_info[usize::from(ty)].size,
                                data_store,
                            );

                            let powered_by = if let Some(pole_pos) = powered_by {
                                let grid = self
                                    .simulation_state
                                    .factory
                                    .power_grids
                                    .pole_pos_to_grid_id[&pole_pos];

                                let grid =
                                    &mut self.simulation_state.factory.power_grids.power_grids
                                        [usize::from(grid)];

                                let weak_idx =
                                    grid.add_lab(pos, ty, &modules, pole_pos, data_store);

                                Some((pole_pos, weak_idx.0, weak_idx.1))
                            } else {
                                None
                            };

                            self.world.add_entity(
                                Entity::Lab {
                                    pos,
                                    ty,
                                    modules,
                                    pole_position: powered_by,
                                },
                                &mut self.simulation_state,
                                data_store,
                            );
                        },
                        crate::frontend::world::tile::PlaceEntityType::Beacon { pos, ty } => {
                            info!("Trying to place beacon at {pos:?}");
                            let size = data_store.beacon_info[usize::from(ty)].size;

                            if !self.world.can_fit(pos, size, data_store) {
                                warn!("Tried to place beacon where it does not fit");
                                continue;
                            }

                            let modules = vec![
                                // TODO: Do not add modules immediately
                                Some(0);
                                data_store.beacon_info[usize::from(ty)]
                                    .num_module_slots
                                    .into()
                            ]
                            .into_boxed_slice();

                            let powered_by = self.world.is_powered_by(
                                &self.simulation_state,
                                pos,
                                data_store.beacon_info[usize::from(ty)].size,
                                data_store,
                            );

                            let powered_by = if let Some(pole_pos) = powered_by {
                                let weak_idx = self
                                    .simulation_state
                                    .factory
                                    .power_grids
                                    .add_beacon(ty, pos, pole_pos, modules.clone(), [], data_store);

                                Some((pole_pos, weak_idx))
                            } else {
                                None
                            };

                            self.world.add_entity(
                                Entity::Beacon {
                                    pos,
                                    ty,
                                    modules,
                                    pole_position: powered_by,
                                },
                                &mut self.simulation_state,
                                data_store,
                            );
                        },
                        crate::frontend::world::tile::PlaceEntityType::FluidTank {
                            ty,
                            pos,
                            rotation,
                        } => {
                            let size = data_store.fluid_tank_infos[usize::from(ty)].size;
                            // FIXME: Stop ignoring rotation
                            if !self.world.can_fit(pos, size.into(), data_store) {
                                warn!("Tried to place storage tank where it does not fit");
                                continue;
                            }

                            let search_range =
                                data_store.fluid_tank_infos[usize::from(ty)].max_search_range;

                            // Get connecting entities:
                            let connecting_fluid_box_positions: Vec<_> = self
                                .world
                                .get_entities_colliding_with(
                                    Position {
                                        x: pos.x - i32::from(search_range),
                                        y: pos.y - i32::from(search_range),
                                    },
                                    (size[0] + 2 * search_range, size[1] + 2 * search_range),
                                    data_store,
                                )
                                .into_iter()
                                .filter_map(|e| match e {
                                    Entity::Assembler {
                                        ty,
                                        pos,
                                        modules,
                                        info,
                                    } => {
                                        // FIXME: Implement assembler flowthough
                                        None
                                    },
                                    Entity::Lab {
                                        pos,
                                        ty,
                                        modules,
                                        pole_position,
                                    } => {
                                        // TODO: Do I want to support fluid science? Would be really easy
                                        None
                                    },

                                    Entity::FluidTank {
                                        ty: other_ty,
                                        pos: other_pos,
                                        rotation: other_rotation,
                                    } => {
                                        let we_can_connect = can_fluid_tanks_connect(
                                            pos,
                                            ty,
                                            rotation,
                                            *other_pos,
                                            *other_ty,
                                            *other_rotation,
                                            data_store,
                                        );

                                        we_can_connect
                                    },

                                    // TODO: There are some future entities which might need connections like mining drills
                                    _ => None,
                                })
                                .collect();

                            let in_out_connections = self
                                .world
                                .get_entities_colliding_with(
                                    Position {
                                        x: pos.x - 1,
                                        y: pos.y - 1,
                                    },
                                    (size[0] + 2, size[1] + 2),
                                    data_store,
                                )
                                .into_iter()
                                .filter_map(|e| match e {
                                    Entity::Assembler {
                                        ty: assembler_ty,
                                        pos: assembler_pos,
                                        info:
                                            AssemblerInfo::Powered {
                                                id,
                                                pole_position,
                                                weak_index,
                                            },
                                        ..
                                    } => {
                                        let assembler_size = data_store.assembler_info
                                            [usize::from(*assembler_ty)]
                                        .size;
                                        let assembler_size = [assembler_size.0, assembler_size.1];

                                        let recipe_fluid_inputs: Vec<_> = data_store
                                            .recipe_to_items[&id.recipe]
                                            .iter()
                                            .filter_map(|(dir, item)| {
                                                (*dir == ItemRecipeDir::Ing
                                                    && data_store.item_is_fluid[item.into_usize()])
                                                .then_some(*item)
                                            })
                                            .collect();
                                        let recipe_fluid_outputs: Vec<_> = data_store
                                            .recipe_to_items[&id.recipe]
                                            .iter()
                                            .filter_map(|(dir, item)| {
                                                (*dir == ItemRecipeDir::Out
                                                    && data_store.item_is_fluid[item.into_usize()])
                                                .then_some(*item)
                                            })
                                            .collect();

                                        let fluid_pure_outputs: Vec<_> = data_store.assembler_info
                                            [usize::from(*assembler_ty)]
                                        .fluid_connections
                                        .iter()
                                        .filter(|(_conn, allowed)| {
                                            *allowed
                                                == AllowedFluidDirection::Single(ItemRecipeDir::Out)
                                                || matches!(
                                                    *allowed,
                                                    AllowedFluidDirection::Both { .. }
                                                )
                                        })
                                        .collect();

                                        let fluid_pure_inputs: Vec<_> = data_store.assembler_info
                                            [usize::from(*assembler_ty)]
                                        .fluid_connections
                                        .iter()
                                        .filter(|(_conn, allowed)| {
                                            *allowed
                                                == AllowedFluidDirection::Single(ItemRecipeDir::Ing)
                                                || matches!(
                                                    *allowed,
                                                    AllowedFluidDirection::Both { .. }
                                                )
                                        })
                                        .collect();

                                        // FIXME: FINISH IMPLEMENTING THIS

                                        let all_connections_with_items = recipe_fluid_inputs
                                            .into_iter()
                                            .cycle()
                                            .zip(fluid_pure_inputs)
                                            .zip(iter::repeat(FluidConnectionDir::Output))
                                            .chain(
                                                recipe_fluid_outputs
                                                    .into_iter()
                                                    .cycle()
                                                    .zip(fluid_pure_outputs)
                                                    .zip(iter::repeat(FluidConnectionDir::Input)),
                                            );

                                        Some(all_connections_with_items.filter_map(
                                            move |((item, (fluid_conn, _allowed)), fluid_dir)| {
                                                can_fluid_tanks_connect_to_single_connection(
                                                    pos,
                                                    ty,
                                                    rotation,
                                                    *assembler_pos,
                                                    *fluid_conn,
                                                    // FIXME: Pass in the assemblers rotation
                                                    Dir::North,
                                                    assembler_size,
                                                    data_store,
                                                )
                                                .map(|(dest_conn, dest_conn_dir)| {
                                                    (
                                                        fluid_dir,
                                                        item,
                                                        Storage::Assembler {
                                                            grid: id.grid,
                                                            index: id.assembler_index,
                                                            recipe_idx_with_this_item: data_store
                                                                .recipe_to_translated_index
                                                                [&(id.recipe, item)],
                                                        },
                                                        Box::new(|_weak_index: WeakIndex| {})
                                                            as Box<dyn FnOnce(WeakIndex) -> ()>,
                                                    )
                                                })
                                            },
                                        ))
                                    },
                                    Entity::Lab {
                                        pos,
                                        ty,
                                        modules,
                                        pole_position,
                                    } => {
                                        // TODO: Do I want to support fluid science? Would be really easy
                                        None
                                    },

                                    // TODO: There are some future entities which might need connections like mining drills
                                    _ => None,
                                })
                                .flatten();

                            // TODO: Only keep the closest connection for each connection
                            // connecting_fluid_box_positions.retain(
                            //     |(dest_pos, conn_dir_of_destination)| {
                            //         let mut current_pos = *dest_pos;

                            //         loop {
                            //             if current_pos.contained_in(pos, size.into()) {
                            //                 return true;
                            //             }

                            //             if let Some(e) = self
                            //                 .world
                            //                 .get_entities_colliding_with(
                            //                     current_pos,
                            //                     (1, 1),
                            //                     data_store,
                            //                 )
                            //                 .into_iter()
                            //                 .next()
                            //             {
                            //                 match e {
                            //                     Entity::FluidTank {
                            //                         ty: found_ty,
                            //                         pos: found_pos,
                            //                         rotation: found_rotation,
                            //                     } => {
                            //                         if can_fluid_tanks_connect(
                            //                             pos,
                            //                             ty,
                            //                             rotation,
                            //                             *found_pos,
                            //                             *found_ty,
                            //                             *found_rotation,
                            //                             data_store,
                            //                         )
                            //                         .is_some()
                            //                         {
                            //                             // The underground should connect with the found fluid tank instead
                            //                             return false;
                            //                         }
                            //                     },
                            //                     Entity::Assembler { ty: found_ty, pos: found_pos, modules: found_modules, info: found_info } => {
                            //                         for conn in data_store.assembler_info[usize::from(*found_ty)].fluid_connections {
                            //                             if can_fluid_tanks_connect_to_single_connection(pos, ty, rotation, *found_pos, conn.0, Dir::North, data_store.assembler_info[usize::from(*found_ty)].size.into(), data_store).is_some() {
                            //                                 // The underground should connect with the found machine instead
                            //                                 return false;
                            //                             }
                            //                         }
                            //                     }
                            //                     _ => {},
                            //                 }
                            //             }

                            //             current_pos = current_pos + *conn_dir_of_destination;
                            //         }
                            //     },
                            // );

                            // TODO: Check if us connecting might break any already existing connections

                            let ret = self.simulation_state.factory.fluid_store.try_add_fluid_box(
                                pos,
                                data_store.fluid_tank_infos[usize::from(ty)].capacity,
                                connecting_fluid_box_positions.iter().map(|v| v.0),
                                in_out_connections,
                                &mut self.simulation_state.factory.chests,
                                &mut self.simulation_state.factory.storage_storage_inserters,
                                data_store,
                            );
                            match ret {
                                Ok(id) => {
                                    self.world.add_entity(
                                        Entity::FluidTank { pos, ty, rotation },
                                        &mut self.simulation_state,
                                        data_store,
                                    );
                                },
                                Err(CannotMixFluidsError { items: [a, b] }) => {
                                    warn!(
                                        "Cannot connect systems containing {} and {}",
                                        data_store.item_names[a.into_usize()],
                                        data_store.item_names[b.into_usize()]
                                    )
                                },
                            }
                        },
                        crate::frontend::world::tile::PlaceEntityType::MiningDrill {
                            ty,
                            pos,
                            rotation,
                        } => todo!("Place Mining Drill"),
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
                ActionType::SetRecipe(SetRecipeInfo {
                    pos: assembler_pos,
                    recipe,
                }) => {
                    let Some(Entity::Assembler { .. }) = self
                        .world
                        .get_entities_colliding_with(assembler_pos, (1, 1), data_store)
                        .into_iter()
                        .next()
                    else {
                        warn!("Tried to set recipe on non assembler");
                        continue;
                    };

                    self.world.change_assembler_recipe(
                        &mut self.simulation_state,
                        assembler_pos,
                        recipe,
                        data_store,
                    );
                },
                ActionType::Remove(pos) => {
                    self.world
                        .remove_entity_at(pos, &mut self.simulation_state, data_store);
                },
                ActionType::AddModules {
                    pos,
                    modules: new_modules,
                } => {
                    self.world
                        .mutate_entities_colliding_with(pos, (1, 1), data_store, |e| {
                            match e {
                                Entity::Assembler { modules, info, .. } => {
                                    let num_free_module_slots =
                                        modules.iter().filter(|slot| slot.is_none()).count();

                                    if new_modules.len() > num_free_module_slots {
                                        // Not enough space in the module slots
                                        warn!(
                                            "Tried to insert more modules than space is available"
                                        );
                                    } else {
                                        // We are okay!

                                        modules
                                            .iter_mut()
                                            .filter(|slot| slot.is_none())
                                            .zip(new_modules.iter().copied())
                                            .for_each(|(slot, new_module)| {
                                                assert!(slot.is_none());
                                                *slot = Some(new_module);
                                            });

                                        match info {
                                            AssemblerInfo::UnpoweredNoRecipe
                                            | AssemblerInfo::Unpowered(_)
                                            | AssemblerInfo::PoweredNoRecipe(_) => {},
                                            AssemblerInfo::Powered {
                                                id,
                                                pole_position,
                                                weak_index,
                                            } => {
                                                for module in &new_modules {
                                                    self.simulation_state
                                                        .factory
                                                        .power_grids
                                                        .power_grids[usize::from(id.grid)]
                                                    .add_module_to_assembler(
                                                        *id, *module, data_store,
                                                    );
                                                }
                                            },
                                        }
                                    }
                                },
                                Entity::Lab {
                                    pos,
                                    ty,
                                    modules,
                                    pole_position,
                                } => {
                                    let num_free_module_slots =
                                        modules.iter().filter(|slot| slot.is_none()).count();

                                    if new_modules.len() > num_free_module_slots {
                                        // Not enough space in the module slots
                                        warn!(
                                            "Tried to insert more modules than space is available"
                                        );
                                    } else {
                                        // We are okay!

                                        modules
                                            .iter_mut()
                                            .filter(|slot| slot.is_none())
                                            .zip(new_modules.iter().copied())
                                            .for_each(|(slot, new_module)| {
                                                assert!(slot.is_none());
                                                *slot = Some(new_module);
                                            });

                                        match pole_position {
                                            None => {},
                                            Some((pole_pos, weak_index, index)) => {
                                                for module in &new_modules {
                                                    self.simulation_state
                                                        .factory
                                                        .power_grids
                                                        .power_grids[usize::from(
                                                        self.simulation_state
                                                            .factory
                                                            .power_grids
                                                            .pole_pos_to_grid_id[pole_pos],
                                                    )]
                                                    .add_module_to_lab(*index, *module, data_store);
                                                }
                                            },
                                        }
                                    }
                                },
                                Entity::Beacon {
                                    ty,
                                    pos,
                                    modules,
                                    pole_position,
                                } => {
                                    // TODO:
                                    // todo!();
                                },
                                _ => {
                                    warn!(
                                        "Tried to insert modules into entity without module slots"
                                    );
                                },
                            }
                            ControlFlow::Break(())
                        });
                },
                ActionType::RemoveModules { pos, indices } => {
                    self.world
                        .mutate_entities_colliding_with(pos, (1, 1), data_store, |e| {
                            match e {
                                Entity::Assembler { modules, info, .. } => {
                                    let num_used_module_slots =
                                        modules.iter().filter(|slot| slot.is_some()).count();

                                    if indices.len() > num_used_module_slots {
                                        // Not enough space in the module slots
                                        warn!("Tried to remove more modules than exist in machine");
                                    } else {
                                        // We are okay!

                                        assert!(indices.iter().all_unique());

                                        assert!(indices.iter().all(|v| *v < modules.len()));

                                        let modules_to_remove = modules
                                            .iter_mut()
                                            .enumerate()
                                            .filter(|(i, _)| indices.contains(i))
                                            .map(|(_, slot)| {
                                                let Some(module) = slot else {
                                                    todo!("How do I want to handle this");
                                                };

                                                let module = *module;

                                                *slot = None;

                                                module
                                            });

                                        match info {
                                            AssemblerInfo::UnpoweredNoRecipe
                                            | AssemblerInfo::Unpowered(_)
                                            | AssemblerInfo::PoweredNoRecipe(_) => {},
                                            AssemblerInfo::Powered {
                                                id,
                                                pole_position,
                                                weak_index,
                                            } => {
                                                for removed_module in modules_to_remove {
                                                    self.simulation_state
                                                        .factory
                                                        .power_grids
                                                        .power_grids[usize::from(id.grid)]
                                                    .remove_module_from_assembler(
                                                        *id,
                                                        removed_module,
                                                        data_store,
                                                    );
                                                }
                                            },
                                        }
                                    }
                                },
                                _ => {
                                    warn!("Tried to insert modules into non assembler");
                                },
                            }
                            ControlFlow::Break(())
                        });
                },
            }
        }

        #[cfg(debug_assertions)]
        {
            assert!(self
                .world
                .get_chunks()
                .into_iter()
                .flat_map(|chunk| chunk.get_entities())
                .all(|e| match e {
                    Entity::Assembler {
                        ty,
                        pos,
                        modules,
                        info,
                    } => {
                        match info {
                            AssemblerInfo::UnpoweredNoRecipe => true,
                            AssemblerInfo::Unpowered(recipe) => true,
                            AssemblerInfo::PoweredNoRecipe(position) => true,
                            AssemblerInfo::Powered {
                                id,
                                pole_position,
                                weak_index,
                            } => {
                                !self.simulation_state.factory.power_grids.power_grids
                                    [usize::from(id.grid)]
                                .is_placeholder
                            },
                        }
                    },
                    Entity::Beacon {
                        ty,
                        pos,
                        modules,
                        pole_position: Some((pole_pos, _)),
                    } => {
                        self.simulation_state
                            .factory
                            .power_grids
                            .pole_pos_to_grid_id
                            .get(pole_pos)
                            .is_some()
                    },
                    _ => true,
                }));
        }
    }

    #[profiling::function]
    pub fn update(&mut self, data_store: &DataStore<ItemIdxType, RecipeIdxType>) {
        self.simulation_state.factory.chests.update(data_store);

        self.simulation_state.factory.belt_update(data_store);

        // TODO: Do I want this, or just do it in the belt_update
        //self.simulation_state
        //    .factory
        //    .belt_belt_inserters
        //    .update(&mut self.simulation_state.factory.belts, data_store);

        // #[cfg(debug_assertions)]
        // {
        //     let num_placeholders = self
        //         .simulation_state
        //         .factory
        //         .power_grids
        //         .power_grids
        //         .iter()
        //         .filter(|grid| grid.is_placeholder)
        //         .count();
        //     dbg!(
        //         num_placeholders,
        //         self.simulation_state.factory.power_grids.power_grids.len()
        //     );

        //     // let num_assemblers = self
        //     //     .world
        //     //     .get_chunks()
        //     //     .into_iter()
        //     //     .flat_map(|c| c.get_entities())
        //     //     .filter(|e| matches!(e, Entity::Assembler { .. }))
        //     //     .count();
        //     // dbg!(num_assemblers);
        // }

        let (tech_progress, recipe_tick_info): (ResearchProgress, RecipeTickInfo) = self
            .simulation_state
            .factory
            .power_grids
            .update(&self.simulation_state.tech_state, 0, data_store);

        self.simulation_state
            .tech_state
            .apply_progress(tech_progress);

        self.statistics.append_single_set_of_samples((
            ProductionInfo::from_recipe_info(&recipe_tick_info, data_store),
            ConsumptionInfo::from_recipe_info(&recipe_tick_info, data_store),
            tech_progress,
        ));

        trace!(
            "{}",
            self.statistics
                .production
                .total
                .as_ref()
                .unwrap()
                .items_produced[0]
        );

        self.current_tick += 1;

        let done_updating = Instant::now();

        if let Some(last_update_time) = self.last_update_time {
            self.update_times.append_single_set_of_samples(UpdateTime {
                dur: done_updating - last_update_time,
            });
        }
        self.last_update_time = Some(done_updating);
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
                        x: assembler_pos.x - i32::from(inserter_range),
                        y: assembler_pos.y - i32::from(inserter_range),
                    },
                    (
                        (2 * inserter_range + size.0).into(),
                        (2 * inserter_range + size.1).into(),
                    ),
                    data_store,
                    |e| {
                        match e {
                            Entity::Inserter {
                                pos,
                                direction,
                                info,
                                filter,
                            } => match info {
                                InserterInfo::NotAttached { start_pos, end_pos } => {
                                    if start_pos
                                        .contained_in(assembler_pos, (size.0.into(), size.1.into()))
                                        || end_pos.contained_in(
                                            assembler_pos,
                                            (size.0.into(), size.1.into()),
                                        )
                                    {
                                        inserter_positions.push(*pos);
                                    }
                                },
                                InserterInfo::Attached { .. } => {},
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
                        x: belt_pos.x - i32::from(inserter_range),
                        y: belt_pos.y - i32::from(inserter_range),
                    },
                    (
                        (2 * inserter_range + 1).into(),
                        (2 * inserter_range + 1).into(),
                    ),
                    data_store,
                    |e| {
                        match e {
                            Entity::Inserter {
                                pos,
                                direction,
                                info,
                                filter,
                            } => match info {
                                InserterInfo::NotAttached { start_pos, end_pos } => {
                                    if start_pos.contained_in(belt_pos, (1, 1))
                                        || end_pos.contained_in(belt_pos, (1, 1))
                                    {
                                        inserter_positions.push(*pos);
                                    }
                                },
                                InserterInfo::Attached { .. } => {},
                            },
                            _ => {},
                        }
                        ControlFlow::Continue(())
                    },
                );
            },
            InserterUpdateInfo::AssemblerRecipeChanged {
                pos: assembler_pos,
                size,
            } => {
                self.world.mutate_entities_colliding_with(
                    Position {
                        x: assembler_pos.x - i32::from(inserter_range),
                        y: assembler_pos.y - i32::from(inserter_range),
                    },
                    (
                        (2 * inserter_range + size.0).into(),
                        (2 * inserter_range + size.1).into(),
                    ),
                    data_store,
                    |e| {
                        match e {
                            Entity::Inserter {
                                pos,
                                direction,
                                info,
                                filter,
                            } => match info {
                                InserterInfo::NotAttached { start_pos, end_pos } => {
                                    if start_pos
                                        .contained_in(assembler_pos, (size.0.into(), size.1.into()))
                                        || end_pos.contained_in(
                                            assembler_pos,
                                            (size.0.into(), size.1.into()),
                                        )
                                    {
                                        inserter_positions.push(*pos);
                                    }
                                },
                                InserterInfo::Attached {
                                    info: AttachedInserter::BeltStorage { id, belt_pos },
                                    ..
                                } => todo!(),
                                InserterInfo::Attached {
                                    info: AttachedInserter::StorageStorage { item, inserter },
                                    ..
                                } => todo!(),
                                InserterInfo::Attached {
                                    info: AttachedInserter::BeltBelt { item, inserter },
                                    ..
                                } => {},
                            },
                            _ => {},
                        }
                        ControlFlow::Continue(())
                    },
                );
            },
        }

        for pos in inserter_positions {
            self.world
                .try_instantiate_inserter(&mut self.simulation_state, pos, data_store);
        }
    }

    fn add_assembler_to_sim(
        sim_state: &mut SimulationState<ItemIdxType, RecipeIdxType>,
        ty: u8,
        recipe: Recipe<RecipeIdxType>,
        modules: &[Option<usize>],
        power_grid_id: PowerGridIdentifier,
        pole_position: Position,
        assembler_position: Position,
        data_store: &DataStore<ItemIdxType, RecipeIdxType>,
    ) -> (AssemblerID<RecipeIdxType>, WeakIndex) {
        sim_state.factory.power_grids.power_grids[power_grid_id as usize].add_assembler(
            ty,
            power_grid_id,
            recipe,
            modules,
            pole_position,
            assembler_position,
            data_store,
        )
    }

    fn add_inserter(
        &mut self,
        pos: Position,
        dir: Dir,
        filter: Option<Item<ItemIdxType>>,
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
                filter,
                info: InserterInfo::NotAttached { start_pos, end_pos },
            },
            &mut self.simulation_state,
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
            .checked_add(dir.reverse().into_offset().0.into())
            .unwrap(),
        y: pos
            .y
            .checked_add(dir.reverse().into_offset().1.into())
            .unwrap(),
    };
    let end_pos = Position {
        x: pos.x.checked_add(dir.into_offset().0.into()).unwrap(),
        y: pos.y.checked_add(dir.into_offset().1.into()).unwrap(),
    };

    (start_pos, end_pos)
}

#[cfg(test)]
mod tests {
    use std::fs::File;

    use crate::{
        blueprint::{random_blueprint_strategy, random_position, Blueprint},
        data::{factorio_1_1::get_raw_data_test, DataStore},
        frontend::{
            action::{
                place_entity::{EntityPlaceOptions, PlaceEntityInfo},
                set_recipe::SetRecipeInfo,
                ActionType,
            },
            world::{
                tile::{AssemblerInfo, Entity, PlaceEntityType},
                Position,
            },
        },
        item::Recipe,
        power::{power_grid::MAX_POWER_MULT, Watt},
        rendering::app_state::GameState,
        replays::Replay,
        DATA_STORE,
    };
    use proptest::{
        prelude::{Just, Strategy},
        prop_assert, prop_assert_eq, prop_assume, proptest,
    };
    use test::Bencher;

    fn beacon_test_val() -> impl Strategy<Value = Vec<ActionType<u8, u8>>> {
        Just(vec![
            ActionType::PlaceEntity(PlaceEntityInfo {
                entities: EntityPlaceOptions::Single(PlaceEntityType::Assembler {
                    pos: Position { x: 1600, y: 1600 },
                    ty: 2,
                }),
            }),
            ActionType::SetRecipe(SetRecipeInfo {
                pos: Position { x: 1600, y: 1600 },
                recipe: Recipe { id: 0 },
            }),
            ActionType::PlaceEntity(PlaceEntityInfo {
                entities: EntityPlaceOptions::Single(PlaceEntityType::Beacon {
                    pos: Position { x: 1603, y: 1600 },
                    ty: 0,
                }),
            }),
            ActionType::AddModules {
                pos: Position { x: 1603, y: 1600 },
                modules: vec![0, 0],
            },
            ActionType::PlaceEntity(PlaceEntityInfo {
                entities: EntityPlaceOptions::Single(PlaceEntityType::PowerPole {
                    pos: Position { x: 1603, y: 1599 },
                    ty: 0,
                }),
            }),
            ActionType::PlaceEntity(PlaceEntityInfo {
                entities: EntityPlaceOptions::Single(PlaceEntityType::PowerPole {
                    pos: Position { x: 1608, y: 1599 },
                    ty: 0,
                }),
            }),
            ActionType::PlaceEntity(PlaceEntityInfo {
                entities: EntityPlaceOptions::Single(PlaceEntityType::PowerPole {
                    pos: Position { x: 1613, y: 1599 },
                    ty: 0,
                }),
            }),
            ActionType::PlaceEntity(PlaceEntityInfo {
                entities: EntityPlaceOptions::Single(PlaceEntityType::SolarPanel {
                    pos: Position { x: 1600, y: 1597 },
                    ty: 0,
                }),
            }),
        ])
    }

    fn full_beacon() -> impl Strategy<Value = Vec<ActionType<u8, u8>>> {
        Just(ron::de::from_reader(File::open("test_blueprints/full_beacons.bp").unwrap()).unwrap())
            .prop_map(|bp: Blueprint<u8, u8>| bp.actions)
    }

    proptest! {
        #[test]
        fn test_random_blueprint_does_not_crash(base_pos in random_position(), blueprint in random_blueprint_strategy::<u8, u8>(0..1_000, &DATA_STORE)) {

            let mut game_state = GameState::new(&DATA_STORE);

            blueprint.apply(base_pos, &mut game_state, &DATA_STORE);

        }

        #[test]
        fn test_random_blueprint_does_not_crash_after(base_pos in random_position(), blueprint in random_blueprint_strategy::<u8, u8>(0..100, &DATA_STORE), time in 0usize..600) {

            let mut game_state = GameState::new(&DATA_STORE);

            blueprint.apply(base_pos, &mut game_state, &DATA_STORE);

            for _ in 0usize..time {
                game_state.update(&DATA_STORE)
            }
        }

        #[test]
        fn test_beacons_always_effect(actions in beacon_test_val().prop_shuffle()) {
            prop_assume!(actions.iter().position(|a| matches!(a, ActionType::PlaceEntity(PlaceEntityInfo {
                entities: EntityPlaceOptions::Single(PlaceEntityType::Assembler {
                    ..
                }),
            }))) < actions.iter().position(|a| matches!(a, ActionType::SetRecipe(_))));
            // prop_assume!(actions.iter().position(|a| matches!(a, ActionType::PlaceEntity(PlaceEntityInfo {
            //     entities: EntityPlaceOptions::Single(PlaceEntityType::Assembler {
            //         ..
            //     }),
            // }))) < actions.iter().position(|a| matches!(a, ActionType::AddModules {
            //     pos: Position { x: 1600, y: 1600 },
            //     ..
            // })));

            let mut game_state = GameState::new(&DATA_STORE);

            Blueprint { actions }.apply(Position { x: 0, y: 0 }, &mut game_state, &DATA_STORE);

            for _ in 0usize..10 {
                game_state.update(&DATA_STORE);
            }

            let Some(Entity::Assembler { ty, pos, modules, info: AssemblerInfo::Powered { id, pole_position, weak_index } }) = game_state.world.get_entities_colliding_with(Position { x: 1600, y: 1600 }, (1,1), &DATA_STORE).into_iter().next() else {
                unreachable!("{:?}", game_state.world.get_entities_colliding_with(Position { x: 1600, y: 1600 }, (1,1), &DATA_STORE).into_iter().next());
            };

            prop_assume!(game_state.simulation_state.factory.power_grids.power_grids[usize::from(id.grid)].last_power_mult == MAX_POWER_MULT);

            let info = game_state.simulation_state.factory.power_grids.power_grids[usize::from(id.grid)].get_assembler_info(*id, &DATA_STORE);

            prop_assert!((info.power_consumption_mod - 0.7).abs() < 1.0e-6, "power_consumption_mod: {:?}", info.power_consumption_mod);
            prop_assert!((info.base_speed - 1.25).abs() < 1.0e-6, "base_speed: {:?}", info.base_speed);
            prop_assert!((info.prod_mod - 0.0).abs() < 1.0e-6, "prod_mod: {:?}", info.prod_mod);
            prop_assert!((info.speed_mod - (0.5)).abs() < 1.0e-6, "speed_mod: {:?}", info.speed_mod);
            prop_assert_eq!(info.base_power_consumption, Watt(375_000), "base_power_consumption: {:?}", info.base_power_consumption);

        }

        #[test]
        fn test_full_beacons_always_effect(actions in full_beacon().prop_shuffle()) {
            prop_assume!(actions.iter().position(|a| matches!(a, ActionType::PlaceEntity(PlaceEntityInfo {
                entities: EntityPlaceOptions::Single(PlaceEntityType::Assembler {
                    ..
                }),
            }))) < actions.iter().position(|a| matches!(a, ActionType::SetRecipe(_))));
            prop_assume!(actions.iter().position(|a| matches!(a, ActionType::PlaceEntity(PlaceEntityInfo {
                entities: EntityPlaceOptions::Single(PlaceEntityType::Assembler {
                    ..
                }),
            }))) < actions.iter().position(|a| matches!(a, ActionType::AddModules { pos: Position { x: 24, y: 21 }, ..})));

            let mut game_state = GameState::new(&DATA_STORE);

            Blueprint { actions }.apply(Position { x: 1600, y: 1600 }, &mut game_state, &DATA_STORE);

            for _ in 0usize..10 {
                game_state.update(&DATA_STORE);
            }

            let Some(Entity::Assembler { ty, pos, modules, info: AssemblerInfo::Powered { id, pole_position, weak_index } }) = game_state.world.get_entities_colliding_with(Position { x: 1624, y: 1621 }, (1,1), &DATA_STORE).into_iter().next() else {
                unreachable!("{:?}", game_state.world.get_entities_colliding_with(Position { x: 1624, y: 1621 }, (1,1), &DATA_STORE).into_iter().next());
            };

            prop_assume!(game_state.simulation_state.factory.power_grids.power_grids[usize::from(id.grid)].last_power_mult == MAX_POWER_MULT);

            let info = game_state.simulation_state.factory.power_grids.power_grids[usize::from(id.grid)].get_assembler_info(*id, &DATA_STORE);

            prop_assert_eq!(info.base_power_consumption, Watt(375_000), "base_power_consumption: {:?}", info.base_power_consumption);
            prop_assert!((info.base_speed - 1.25).abs() < 1.0e-6, "base_speed: {:?}", info.base_speed);
            prop_assert!((info.prod_mod - 0.4).abs() < 1.0e-6, "prod_mod: {:?}", info.prod_mod);
            prop_assert!((info.speed_mod - (5.4)).abs() < 1.0e-6, "speed_mod: {:?}", info.speed_mod);
            prop_assert!((info.power_consumption_mod - 11.60).abs() < 1.0e-6, "power_consumption_mod: {:?}", info.power_consumption_mod);

        }
    }

    #[bench]
    fn bench_single_inserter(b: &mut Bencher) {
        let mut game_state = GameState::new(&DATA_STORE);

        let mut rep = Replay::new(&game_state, None, (*DATA_STORE).clone());

        rep.append_actions(vec![ActionType::PlaceEntity(PlaceEntityInfo {
            entities: crate::frontend::action::place_entity::EntityPlaceOptions::Single(
                crate::frontend::world::tile::PlaceEntityType::PowerPole {
                    pos: Position { x: 0, y: 5 },
                    ty: 0,
                },
            ),
        })]);

        rep.append_actions(vec![ActionType::PlaceEntity(PlaceEntityInfo {
            entities: crate::frontend::action::place_entity::EntityPlaceOptions::Single(
                crate::frontend::world::tile::PlaceEntityType::SolarPanel {
                    pos: Position { x: 0, y: 2 },
                    ty: 0,
                },
            ),
        })]);

        rep.append_actions(vec![ActionType::PlaceEntity(PlaceEntityInfo {
            entities: crate::frontend::action::place_entity::EntityPlaceOptions::Single(
                crate::frontend::world::tile::PlaceEntityType::Assembler {
                    pos: Position { x: 0, y: 6 },
                    ty: 0,
                },
            ),
        })]);

        rep.append_actions(vec![ActionType::SetRecipe(
            crate::frontend::action::set_recipe::SetRecipeInfo {
                pos: Position { x: 0, y: 6 },
                recipe: Recipe { id: 0 },
            },
        )]);

        rep.append_actions(vec![ActionType::PlaceEntity(PlaceEntityInfo {
            entities: crate::frontend::action::place_entity::EntityPlaceOptions::Single(
                crate::frontend::world::tile::PlaceEntityType::Belt {
                    pos: Position { x: 1, y: 4 },
                    direction: crate::frontend::world::tile::Dir::East,
                },
            ),
        })]);

        rep.append_actions(vec![ActionType::PlaceEntity(PlaceEntityInfo {
            entities: crate::frontend::action::place_entity::EntityPlaceOptions::Single(
                crate::frontend::world::tile::PlaceEntityType::Belt {
                    pos: Position { x: 2, y: 4 },
                    direction: crate::frontend::world::tile::Dir::East,
                },
            ),
        })]);

        rep.append_actions(vec![ActionType::PlaceEntity(PlaceEntityInfo {
            entities: crate::frontend::action::place_entity::EntityPlaceOptions::Single(
                crate::frontend::world::tile::PlaceEntityType::Inserter {
                    pos: Position { x: 1, y: 5 },
                    dir: crate::frontend::world::tile::Dir::North,
                    filter: None,
                },
            ),
        })]);

        let blueprint = Blueprint::from_replay(&rep);

        blueprint.apply(Position { x: 1600, y: 1600 }, &mut game_state, &DATA_STORE);

        dbg!(&game_state
            .world
            .get_chunk_for_tile(Position { x: 1600, y: 1600 }));

        dbg!(game_state.current_tick);

        for _ in 0..60 {
            game_state.update(&DATA_STORE);
        }

        b.iter(|| {
            game_state.update(&DATA_STORE);
        });

        dbg!(game_state.current_tick);

        assert!(
            game_state
                .statistics
                .production
                .total
                .unwrap()
                .items_produced[0]
                > 0
        );
    }
}
