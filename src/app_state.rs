use crate::belt::BeltTileId;
use crate::belt::belt::Belt;
use crate::blueprint::blueprint_string::BlueprintString;
use crate::chest::ChestSize;
use crate::data::AllowedFluidDirection;
use crate::frontend::action::belt_placement::FakeGameState;
use crate::frontend::action::place_entity::PlaceEntityInfo;
use crate::frontend::world::tile::ModuleSlots;
use crate::frontend::world::tile::ModuleTy;
use crate::inserter::InserterStateInfo;
use crate::inserter::storage_storage_with_buckets_indirect::BucketedStorageStorageInserterStore;
use crate::inserter::storage_storage_with_buckets_indirect::InserterIdentifier;
use crate::item::ITEMCOUNTTYPE;
use crate::join_many::join;
use crate::liquid::FluidConnectionDir;
use crate::liquid::FluidSystemId;
use crate::liquid::connection_logic::can_fluid_tanks_connect_to_single_connection;
use crate::liquid::update_fluid_system;
use crate::mining_drill::FullOreStore;
use crate::par_generation::BoundingBox;
use crate::par_generation::ParGenerateInfo;
use crate::par_generation::par_generate;
use crate::power::Watt;
#[cfg(feature = "client")]
use crate::{Input, LoadedGame};
use crate::{
    belt::{BeltBeltInserterInfo, BeltStore},
    blueprint::Blueprint,
    chest::{FullChestStore, MultiChestStore},
    data::{DataStore, ItemRecipeDir},
    frontend::{
        action::{
            ActionType,
            belt_placement::{
                handle_belt_placement, handle_splitter_placement, handle_underground_belt_placement,
            },
            set_recipe::SetRecipeInfo,
        },
        world::{
            Position,
            tile::{
                AssemblerID, AssemblerInfo, AttachedInserter, Dir, Entity, InserterInfo, World,
            },
        },
    },
    inserter::{FakeUnionStorage, Storage, belt_belt_inserter::BeltBeltInserter},
    item::{IdxTrait, Item, Recipe, WeakIdxTrait, usize_from},
    liquid::connection_logic::can_fluid_tanks_connect,
    network_graph::WeakIndex,
    power::{PowerGridStorage, power_grid::PowerGridIdentifier},
    research::TechState,
    statistics::{
        GenStatistics, Timeline, consumption::ConsumptionInfo, production::ProductionInfo,
    },
    storage_list::{
        SingleItemStorages, full_to_by_item, grid_size, num_recipes, sizes, storages_by_item,
    },
};
use crate::{
    item::Indexable,
    liquid::{CannotMixFluidsError, FluidSystemStore},
};
#[cfg(feature = "client")]
use egui_show_info_derive::ShowInfo;
use flate2::bufread::ZlibDecoder;
#[cfg(feature = "client")]
use get_size2::GetSize;
use itertools::Itertools;
use log::error;
use log::{info, trace, warn};
use petgraph::graph::NodeIndex;
use rayon::iter::{IndexedParallelIterator, IntoParallelRefMutIterator, ParallelIterator};
use std::collections::BTreeMap;
use std::io::BufReader;
use std::io::Read;
use std::iter;
use std::num::NonZero;
use std::path::Path;
use std::sync::Arc;
use std::sync::atomic::AtomicU64;
use std::sync::atomic::Ordering;
use std::sync::mpsc::Receiver;
use std::sync::mpsc::Sender;
use std::{borrow::Borrow, fs::File, ops::ControlFlow, time::Duration};

use wasm_timer::Instant;

use crate::frontend::action::place_tile::PositionInfo;

use std::ops::AddAssign;

use crate::get_size::Mutex;

#[cfg_attr(feature = "client", derive(ShowInfo), derive(GetSize))]
#[derive(Debug, Clone, serde::Deserialize, serde::Serialize)]
pub struct AuxillaryData {
    pub current_tick: u64,

    pub statistics: GenStatistics,

    pub update_round_trip_times: Timeline<UpdateTime>,
    pub update_times: Timeline<UpdateTime>,
    #[serde(skip)]
    last_update_time: Option<Instant>,

    pub settings: GameSettings,
}
impl AuxillaryData {
    pub fn new<ItemIdxType: IdxTrait, RecipeIdxType: IdxTrait>(
        data_store: &DataStore<ItemIdxType, RecipeIdxType>,
    ) -> Self {
        AuxillaryData {
            current_tick: 0,
            statistics: GenStatistics::new(data_store),
            update_times: Timeline::new(false, data_store),
            update_round_trip_times: Timeline::new(false, data_store),
            last_update_time: None,
            settings: GameSettings {
                show_unresearched_recipes: true,
            },
        }
    }
}

#[cfg_attr(feature = "client", derive(ShowInfo), derive(GetSize))]
#[derive(Debug, serde::Deserialize, serde::Serialize)]
pub struct GameState<ItemIdxType: WeakIdxTrait, RecipeIdxType: WeakIdxTrait> {
    pub world: Mutex<World<ItemIdxType, RecipeIdxType>>,
    pub simulation_state: Mutex<SimulationState<ItemIdxType, RecipeIdxType>>,

    pub aux_data: Mutex<AuxillaryData>,
}

#[cfg_attr(feature = "client", derive(ShowInfo), derive(GetSize))]
#[derive(Debug, Clone, serde::Deserialize, serde::Serialize)]
pub struct GameSettings {
    pub show_unresearched_recipes: bool,
}

#[cfg_attr(feature = "client", derive(ShowInfo), derive(GetSize))]
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
            world: Mutex::new(World::default()),
            simulation_state: Mutex::new(SimulationState::new(data_store)),
            aux_data: Mutex::new(AuxillaryData::new(data_store)),
        }
    }

    fn new_with_world_area(
        top_left: Position,
        bottom_right: Position,
        data_store: &DataStore<ItemIdxType, RecipeIdxType>,
    ) -> Self {
        Self {
            world: Mutex::new(World::new_with_area(top_left, bottom_right)),
            simulation_state: Mutex::new(SimulationState::new(data_store)),
            aux_data: Mutex::new(AuxillaryData::new(data_store)),
        }
    }

    #[must_use]
    pub fn new_with_production(
        progress: Arc<AtomicU64>,
        data_store: &DataStore<ItemIdxType, RecipeIdxType>,
    ) -> Self {
        let mut ret = GameState::new_with_world_area(
            Position { x: 0, y: 0 },
            Position { x: 3500, y: 31000 },
            data_store,
        );

        const BP_STRING: &'static str = include_str!("../test_blueprints/red_sci.bp");
        let bp: Blueprint = ron::de::from_str(BP_STRING).unwrap();
        // let file = File::open("test_blueprints/red_sci.bp").unwrap();
        // let bp: Blueprint = ron::de::from_reader(file).unwrap();
        let bp = bp.get_reusable(false, data_store);

        puffin::set_scopes_on(false);
        let y_range = (1590..3000).step_by(7);
        let x_range = (1590..3000).step_by(20);

        let total = y_range.size_hint().0 * x_range.size_hint().0;

        let mut current = 0;

        for y_pos in y_range {
            for x_pos in x_range.clone() {
                progress.store((current as f64 / total as f64).to_bits(), Ordering::Relaxed);
                current += 1;

                bp.apply(Position { x: x_pos, y: y_pos }, &mut ret, data_store);
            }
        }
        puffin::set_scopes_on(true);

        ret
    }

    #[must_use]
    pub fn new_with_megabase(
        use_solar_field: bool,
        progress: Arc<AtomicU64>,
        data_store: &DataStore<ItemIdxType, RecipeIdxType>,
    ) -> Self {
        // TODO: Increase size to fit solar field
        let mut ret = GameState::new_with_world_area(
            Position { x: 0, y: 0 },
            Position {
                x: 16_000,
                y: 12_000,
            },
            data_store,
        );
        let mut file =
            File::open("test_blueprints/murphy/megabase_new_blueprint_format.bp").unwrap();
        let mut s = String::new();
        file.read_to_string(&mut s).unwrap();
        let bp: BlueprintString = BlueprintString(s);
        let mut bp: Blueprint = bp.try_into().expect("Blueprint String Invalid");
        bp.optimize();

        let num_calls = bp.action_count();

        let mut current = 0;

        puffin::set_scopes_on(false);
        if use_solar_field {
            ret.add_solar_field(
                Position { x: 4503, y: 2454 },
                Watt(325_000_000_000),
                Some(9_000),
                progress.clone(),
                data_store,
            );
        } else {
            GameState::apply_actions(
                &mut *ret.simulation_state.lock(),
                &mut *ret.world.lock(),
                vec![ActionType::PlaceEntity(PlaceEntityInfo {
                    entities: crate::frontend::action::place_entity::EntityPlaceOptions::Single(
                        crate::frontend::world::tile::PlaceEntityType::SolarPanel {
                            pos: Position { x: 4502, y: 2454 },
                            ty: data_store
                                .solar_panel_info
                                .iter()
                                .position(|info| &*info.name == "factory_game::infinity_battery")
                                .expect("Mod set does not include factory_game::infinity_battery")
                                .try_into()
                                .unwrap(),
                        },
                    ),
                    force: false,
                })],
                data_store,
            );
        }

        bp.apply_at_positions(
            iter::once(Position { x: 1600, y: 1600 }),
            false,
            &mut ret,
            || {
                progress.store(
                    (current as f64 / num_calls as f64).to_bits(),
                    Ordering::Relaxed,
                );
                current += 1;
            },
            data_store,
        );

        puffin::set_scopes_on(true);

        ret
    }

    #[must_use]
    pub fn new_with_gigabase(
        count: u16,
        progress: Arc<AtomicU64>,
        data_store: &DataStore<ItemIdxType, RecipeIdxType>,
    ) -> Self {
        let width = count.isqrt();
        // Make the stack of factories as tall as possible
        // let width = 1;

        let main_height = count / width;

        let remainder = count - (width * main_height);

        let full_height = if remainder == 0 {
            main_height
        } else {
            main_height + 1
        };

        const MEGABASE_WIDTH: i32 = 4_000;
        const MEGABASE_HEIGHT: i32 = 10_000;

        let width_in_tiles = i32::from(width) * 4_000;
        let height_in_tiles = i32::from(full_height) * 10_000;

        let start = 0;
        // let start = -(height_in_tiles / 2);

        // let mut ret = GameState::new_with_world_area(
        //     Position { x: 0, y: 0 },
        //     Position {
        //         x: width_in_tiles + 2000,
        //         y: height_in_tiles + 2000,
        //     },
        //     data_store,
        // );

        // TODO: Calulated needed solar field size (and pos)
        // ret.add_solar_field(Position { x: 1590, y: 70000 }, Watt(3_000_000_000_000), progress.clone(), data_store);

        puffin::set_scopes_on(false);
        let x_range = (0..width).map(|x| i32::from(x) * MEGABASE_WIDTH);
        let y_range = (0..full_height).map(|y| i32::from(y) * MEGABASE_HEIGHT);

        error!("Loading Generation Info...");
        let file = {
            File::open("./test_blueprints/par_generation_info")
                .expect(&format!("could not open file"))
        };
        let par_data: ParGenerateInfo<ItemIdxType, RecipeIdxType> = {
            profiling::scope!("Decompressing and deserializing");
            let mut e = ZlibDecoder::new(BufReader::new(file));
            bincode::serde::decode_from_std_read(&mut e, bincode::config::standard())
                .expect("Deserialization failed")
        };
        error!("Done!");

        let ret = par_generate(
            BoundingBox {
                top_left: Position { x: 0, y: start },
                bottom_right: Position {
                    x: width_in_tiles + 4000,
                    y: start + height_in_tiles + 4000,
                },
            },
            par_data,
            y_range
                .cartesian_product(x_range)
                .map(|(y, x)| Position { x, y: y + start })
                .take(count as usize)
                .collect(),
            data_store,
        );

        puffin::set_scopes_on(true);

        ret
    }

    #[must_use]
    pub fn new_with_beacon_production(
        progress: Arc<AtomicU64>,
        data_store: &DataStore<ItemIdxType, RecipeIdxType>,
    ) -> Self {
        Self::new_with_beacon_red_green_production_many_grids(progress, data_store)
    }

    #[must_use]
    pub fn new_with_beacon_red_green_production_many_grids(
        progress: Arc<AtomicU64>,
        data_store: &DataStore<ItemIdxType, RecipeIdxType>,
    ) -> Self {
        let mut ret = GameState::new_with_world_area(
            Position { x: 0, y: 0 },
            Position { x: 44000, y: 44000 },
            data_store,
        );

        const BP_STRING: &'static str =
            include_str!("../test_blueprints/red_and_green_with_clocking.bp");
        let bp: Blueprint = ron::de::from_str(BP_STRING).unwrap();
        // let file = File::open("test_blueprints/red_and_green_with_clocking.bp").unwrap();
        // let bp: Blueprint = ron::de::from_reader(file).unwrap();
        let bp = bp.get_reusable(false, data_store);

        puffin::set_scopes_on(false);
        let y_range = (0..40_000).step_by(4_000);
        let x_range = (0..40_000).step_by(4_000);

        let total = y_range.size_hint().0 * x_range.size_hint().0;

        let mut current = 0;

        for y_start in y_range {
            for x_start in x_range.clone() {
                progress.store((current as f64 / total as f64).to_bits(), Ordering::Relaxed);
                current += 1;
                for y_pos in (1590..4000).step_by(40) {
                    for x_pos in (1590..4000).step_by(50) {
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
        progress: Arc<AtomicU64>,
        data_store: &DataStore<ItemIdxType, RecipeIdxType>,
    ) -> Self {
        let mut ret = GameState::new_with_world_area(
            Position { x: 0, y: 0 },
            Position { x: 10000, y: 20000 },
            data_store,
        );

        let file = File::open("test_blueprints/red_sci_with_beacons_and_belts.bp").unwrap();
        let bp: Blueprint = ron::de::from_reader(file).unwrap();
        let bp = bp.get_reusable(false, data_store);

        let y_range = (0..20_000).step_by(6_000);

        let total = y_range.size_hint().0;

        let mut current = 0;

        puffin::set_scopes_on(false);
        for y_start in y_range {
            progress.store((current as f64 / total as f64).to_bits(), Ordering::Relaxed);
            current += 1;
            for y_pos in (1590..6000).step_by(10) {
                for x_pos in (1590..4000).step_by(60) {
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
    pub fn new_with_lots_of_belts(
        progress: Arc<AtomicU64>,
        data_store: &DataStore<ItemIdxType, RecipeIdxType>,
    ) -> Self {
        let mut ret = GameState::new_with_world_area(
            Position { x: 0, y: 0 },
            Position {
                x: 5_000,
                y: 250_000,
            },
            data_store,
        );

        let mut file = File::open("test_blueprints/lots_of_belts.bp").unwrap();
        let mut s = String::new();
        file.read_to_string(&mut s).unwrap();
        let bp: BlueprintString = BlueprintString(s);
        let mut bp: Blueprint = Blueprint::try_from(bp).unwrap();
        bp.optimize();

        let total = bp.action_count() as f64;
        let mut current = 0;

        puffin::set_scopes_on(false);
        let positions = (1600..250_000)
            .step_by(3)
            .map(|y_pos| Position { x: 1600, y: y_pos });
        bp.apply_at_positions(
            positions,
            false,
            &mut ret,
            || {
                current += 1;
                progress.store((current as f64 / total).to_bits(), Ordering::Relaxed);
            },
            data_store,
        );
        puffin::set_scopes_on(true);

        ret
    }

    #[must_use]
    pub fn new_with_tons_of_solar(
        wattage: Watt,
        base_pos: Position,
        height: Option<u64>,
        progress: Arc<AtomicU64>,
        data_store: &DataStore<ItemIdxType, RecipeIdxType>,
    ) -> Self {
        // TODO: Correct size
        let mut ret = GameState::new_with_world_area(
            Position { x: 0, y: 0 },
            Position { x: 60000, y: 60000 },
            data_store,
        );

        ret.add_solar_field(base_pos, wattage, height, progress, data_store);

        ret
    }

    pub fn add_solar_field(
        &mut self,
        pos: Position,
        amount: Watt,
        height_in_tiles: Option<u64>,
        progress: Arc<AtomicU64>,
        data_store: &DataStore<ItemIdxType, RecipeIdxType>,
    ) {
        let mut file = File::open("test_blueprints/solar_tile.bp").unwrap();
        let mut s = String::new();
        file.read_to_string(&mut s).unwrap();
        let bp: BlueprintString = BlueprintString(s);
        let mut bp: Blueprint = Blueprint::try_from(bp).unwrap();
        bp.optimize();
        let bp = bp.get_reusable(false, data_store);
        let bp = bp.optimize();

        let bp_count = amount.0.div_ceil((Watt(42_000) * 104).0);

        let height = height_in_tiles.map(|v| v / 36).unwrap_or(bp_count.isqrt());
        let width = bp_count.div_ceil(height);

        let x_positions = (0..width).map(|v| pos.x + 36 * v as i32);
        let y_positions = (0..height).map(|v| pos.y + 36 * v as i32);

        // let total = bp.action_count();
        let total = y_positions.clone().count() * x_positions.clone().count();

        let mut current = 0;

        puffin::set_scopes_on(false);
        for pos in x_positions
            .cartesian_product(y_positions)
            .map(|(x_pos, y_pos)| Position { x: x_pos, y: y_pos })
        {
            progress.store((current as f64 / total as f64).to_bits(), Ordering::Relaxed);
            current += 1;
            bp.apply(pos, self, data_store);
        }
        // bp.apply_at_positions(
        //     x_positions
        //         .cartesian_product(y_positions)
        //         .map(|(x, y)| Position { x, y }),
        //     false,
        //     self,
        //     || {
        //         progress.store((current as f64 / total as f64).to_bits(), Ordering::Relaxed);
        //         current += 1;
        //     },
        //     data_store,
        // );
        puffin::set_scopes_on(true);
    }

    #[must_use]
    pub fn new_eight_beacon_factory(
        _progress: Arc<AtomicU64>,
        data_store: &DataStore<ItemIdxType, RecipeIdxType>,
    ) -> Self {
        let mut ret = GameState::new(data_store);

        let red = File::open("test_blueprints/eight_beacon_red_sci_with_storage.bp").unwrap();
        let red: Blueprint = ron::de::from_reader(red).unwrap();
        let red = red.get_reusable(false, data_store);

        puffin::set_scopes_on(false);
        for y_pos in (1600..=30_000).step_by(20) {
            red.apply(Position { x: 1600, y: y_pos }, &mut ret, data_store);
        }
        puffin::set_scopes_on(true);

        ret
    }

    pub fn new_with_bp(
        data_store: &DataStore<ItemIdxType, RecipeIdxType>,
        bp_path: impl AsRef<Path>,
    ) -> Self {
        let mut ret = GameState::new_with_world_area(
            Position { x: 0, y: 0 },
            Position {
                x: 32000,
                y: 105000,
            },
            data_store,
        );

        let mut file = File::open(bp_path).unwrap();
        let mut bp_string = BlueprintString(String::new());
        file.read_to_string(&mut bp_string.0).unwrap();
        let mut bp: Blueprint = bp_string.try_into().expect("Invalid Blueprint");
        bp.optimize();

        bp.apply(false, Position { x: 2000, y: 2000 }, &mut ret, data_store);

        ret
    }
}

#[cfg_attr(feature = "client", derive(ShowInfo), derive(GetSize))]
#[derive(Debug, Clone, serde::Deserialize, serde::Serialize)]
pub struct SimulationState<ItemIdxType: WeakIdxTrait, RecipeIdxType: WeakIdxTrait> {
    pub tech_state: TechState,
    pub factory: Factory<ItemIdxType, RecipeIdxType>,
    // TODO:
}

impl<ItemIdxType: IdxTrait, RecipeIdxType: IdxTrait> SimulationState<ItemIdxType, RecipeIdxType> {
    #[must_use]
    pub fn new(data_store: &DataStore<ItemIdxType, RecipeIdxType>) -> Self {
        Self {
            tech_state: TechState::new(data_store),
            factory: Factory::new(data_store),
        }
    }
}

#[cfg_attr(feature = "client", derive(ShowInfo), derive(GetSize))]
#[derive(Debug, Clone, serde::Deserialize, serde::Serialize)]
pub struct Factory<ItemIdxType: WeakIdxTrait, RecipeIdxType: WeakIdxTrait> {
    pub power_grids: PowerGridStorage<ItemIdxType, RecipeIdxType>,
    pub belts: BeltStore<ItemIdxType>,
    pub storage_storage_inserters: StorageStorageInserterStore,
    pub belt_storage_inserters: Box<[BTreeMap<
        u16,
        (
            crate::inserter::belt_storage_pure_buckets::BucketedStorageStorageInserterStoreFrontend,
            crate::inserter::belt_storage_pure_buckets::BucketedStorageStorageInserterStore,
        ),
    >]>,
    pub chests: FullChestStore<ItemIdxType>,

    pub fluid_store: FluidSystemStore<ItemIdxType>,

    pub ore_store: FullOreStore<ItemIdxType>,

    pub item_times: Box<[f32]>,
}

#[cfg_attr(feature = "client", derive(ShowInfo), derive(GetSize))]
#[derive(Debug, Clone, serde::Deserialize, serde::Serialize)]
pub struct StorageStorageInserterStore {
    pub inserters: Box<[BTreeMap<u16, (BucketedStorageStorageInserterStore,)>]>,
}

impl StorageStorageInserterStore {
    pub fn new<ItemIdxType: IdxTrait, RecipeIdxType: IdxTrait>(
        data_store: &DataStore<ItemIdxType, RecipeIdxType>,
    ) -> Self {
        Self {
            inserters: vec![BTreeMap::new(); data_store.item_display_names.len()]
                .into_boxed_slice(),
        }
    }

    // #[profiling::function]
    // pub fn get_info_batched<ItemIdxType: IdxTrait>(
    //     &mut self,
    //     item: Item<ItemIdxType>,
    //     movetime: u16,
    //     ids: impl IntoIterator<Item = InserterIdentifier>,
    //     current_tick: u32,
    // ) -> HashMap<InserterIdentifier, LargeInserterState> {
    //     let (front, back) = self.inserters[item.into_usize()]
    //         .get_mut(&movetime)
    //         .unwrap();

    //     let info = front.get_info_batched(ids, &back, true, current_tick);
    //     info
    // }

    #[profiling::function]
    fn update<'a, 'b, ItemIdxType: IdxTrait, RecipeIdxType: IdxTrait>(
        &mut self,
        full_storages: impl IndexedParallelIterator<Item = SingleItemStorages<'a, 'b>>,
        _num_grids_total: usize,
        current_tick: u32,
        data_store: &DataStore<ItemIdxType, RecipeIdxType>,
    ) where
        'b: 'a,
    {
        self.inserters
            .par_iter_mut()
            .zip(full_storages)
            .enumerate()
            .for_each(|(item_id, (map, storages))| {
                profiling::scope!(
                    "StorageStorage Inserter Update",
                    format!("Item: {}", data_store.item_display_names[item_id]).as_str()
                );

                let item = Item {
                    id: item_id.try_into().unwrap(),
                };

                let grid_size = grid_size(item, data_store);

                for (ins_store,) in map.values_mut() {
                    profiling::scope!(
                        "StorageStorage Inserter Update",
                        format!("Movetime: {}", ins_store.movetime).as_str()
                    );
                    ins_store.update(item_id, storages, grid_size, current_tick);
                }
            });
    }

    pub fn add_ins<ItemIdxType: IdxTrait, RecipeIdxType: IdxTrait>(
        &mut self,
        item: Item<ItemIdxType>,
        movetime: u16,
        start: Storage<RecipeIdxType>,
        dest: Storage<RecipeIdxType>,
        hand_size: ITEMCOUNTTYPE,
        data_store: &DataStore<ItemIdxType, RecipeIdxType>,
    ) -> InserterIdentifier {
        let source = FakeUnionStorage::from_storage_with_statics_at_zero(item, start, data_store);
        let dest = FakeUnionStorage::from_storage_with_statics_at_zero(item, dest, data_store);

        let id: InserterIdentifier = self.inserters[item.into_usize()]
            .entry(movetime)
            .or_insert_with(|| (BucketedStorageStorageInserterStore::new(movetime),))
            .0
            .add_inserter(source, dest, hand_size);

        id
    }

    pub fn get_inserter<ItemIdxType: IdxTrait>(
        &self,
        item: Item<ItemIdxType>,
        movetime: u16,
        id: InserterIdentifier,
        current_tick: u32,
    ) -> InserterStateInfo {
        self.inserters[item.into_usize()]
            .get(&movetime)
            .unwrap()
            .0
            .get_inserter(id, current_tick)
    }

    pub fn remove_ins<ItemIdxType: IdxTrait>(
        &mut self,
        item: Item<ItemIdxType>,
        movetime: u16,
        id: InserterIdentifier,
    ) {
        let inserter = self.inserters[item.into_usize()]
            .get_mut(&movetime)
            .unwrap()
            .0
            .remove_inserter(id);
        // TODO: Handle what happens with the items
    }

    #[must_use]
    pub fn change_movetime<ItemIdxType: IdxTrait>(
        &mut self,
        item: Item<ItemIdxType>,
        old_movetime: u16,
        new_movetime: u16,
        id: InserterIdentifier,
    ) -> InserterIdentifier {
        // FIXME: This does not preserve the inserter state at all!
        let inserter = self.inserters[item.into_usize()]
            .get_mut(&old_movetime)
            .unwrap()
            .0
            .remove_inserter(id);

        let inner_id: InserterIdentifier = self.inserters[item.into_usize()]
            .entry(new_movetime)
            .or_insert_with(|| (BucketedStorageStorageInserterStore::new(new_movetime),))
            .0
            .add_inserter(
                inserter.storage_id_in,
                inserter.storage_id_out,
                inserter.max_hand_size,
            );

        inner_id
    }

    #[profiling::function]
    pub fn update_inserter_src<ItemIdxType: IdxTrait, RecipeIdxType: IdxTrait>(
        &mut self,
        item: Item<ItemIdxType>,
        movetime: u16,
        id: InserterIdentifier,
        new_src: Storage<RecipeIdxType>,
        data_store: &DataStore<ItemIdxType, RecipeIdxType>,
    ) -> InserterIdentifier {
        self.inserters[item.into_usize()]
            .get_mut(&movetime)
            .unwrap()
            .0
            .update_inserter_src(
                id,
                FakeUnionStorage::from_storage_with_statics_at_zero(item, new_src, data_store),
            )
    }

    #[profiling::function]
    pub fn update_inserter_dest<ItemIdxType: IdxTrait, RecipeIdxType: IdxTrait>(
        &mut self,
        item: Item<ItemIdxType>,
        movetime: u16,
        id: InserterIdentifier,
        new_dest: Storage<RecipeIdxType>,
        data_store: &DataStore<ItemIdxType, RecipeIdxType>,
    ) -> InserterIdentifier {
        self.inserters[item.into_usize()]
            .get_mut(&movetime)
            .unwrap()
            .0
            .update_inserter_dest(
                id,
                FakeUnionStorage::from_storage_with_statics_at_zero(item, new_dest, data_store),
            )
    }

    #[profiling::function]
    pub fn update_inserter_src_if_equal<ItemIdxType: IdxTrait, RecipeIdxType: IdxTrait>(
        &mut self,
        item: Item<ItemIdxType>,
        movetime: u16,
        id: InserterIdentifier,
        old_src: FakeUnionStorage,
        new_src: Storage<RecipeIdxType>,
        data_store: &DataStore<ItemIdxType, RecipeIdxType>,
    ) -> InserterIdentifier {
        self.inserters[item.into_usize()]
            .get_mut(&movetime)
            .unwrap()
            .0
            .update_inserter_src_if_equal(
                id,
                old_src,
                FakeUnionStorage::from_storage_with_statics_at_zero(item, new_src, data_store),
            )
    }

    #[profiling::function]
    pub fn update_inserter_dest_if_equal<ItemIdxType: IdxTrait, RecipeIdxType: IdxTrait>(
        &mut self,
        item: Item<ItemIdxType>,
        movetime: u16,
        id: InserterIdentifier,
        old_dest: FakeUnionStorage,
        new_dest: Storage<RecipeIdxType>,
        data_store: &DataStore<ItemIdxType, RecipeIdxType>,
    ) -> InserterIdentifier {
        self.inserters[item.into_usize()]
            .get_mut(&movetime)
            .unwrap()
            .0
            .update_inserter_dest_if_equal(
                id,
                old_dest,
                FakeUnionStorage::from_storage_with_statics_at_zero(item, new_dest, data_store),
            )
    }
}

#[derive(Debug, Clone, serde::Deserialize, serde::Serialize)]
pub struct BeltBeltInserterStore<ItemIdxType: WeakIdxTrait> {
    // TODO: Holes
    pub inserters: Box<[Vec<(BeltBeltInserter, BeltBeltInserterInfo<ItemIdxType>)>]>,
}

impl<ItemIdxType: IdxTrait, RecipeIdxType: IdxTrait> Factory<ItemIdxType, RecipeIdxType> {
    pub fn new(data_store: &DataStore<ItemIdxType, RecipeIdxType>) -> Self {
        Self {
            power_grids: PowerGridStorage::new(),
            belts: BeltStore::new(data_store),
            storage_storage_inserters: StorageStorageInserterStore::new(data_store),
            belt_storage_inserters: vec![BTreeMap::new(); data_store.item_names.len()]
                .into_boxed_slice(),
            chests: FullChestStore {
                stores: (0..data_store.item_display_names.len())
                    .map(|id| Item {
                        id: id.try_into().unwrap(),
                    })
                    .map(|item| MultiChestStore::new(item))
                    .collect(),
            },

            fluid_store: FluidSystemStore::new(data_store),

            item_times: (0..data_store.item_display_names.len())
                .map(|_| 0.0)
                .collect(),

            ore_store: FullOreStore::new(data_store),
        }
    }

    // #[profiling::function]
    // fn belt_update(
    //     &mut self,
    //     current_tick: u32,
    //     data_store: &DataStore<ItemIdxType, RecipeIdxType>,
    // ) {
    //     self.storage_storage_inserters.update(
    //         storages_by_item.par_iter_mut().map(|v| &mut **v),
    //         num_grids_total,
    //         current_tick,
    //         data_store,
    //     );

    //     self.belts.update(
    //         storages_by_item.par_iter_mut().map(|v| &mut **v),
    //         data_store,
    //     );
    // }

    fn belt_and_inserter_update(
        &mut self,
        current_tick: u32,
        data_store: &DataStore<ItemIdxType, RecipeIdxType>,
    ) {
        let num_grids_total = self.power_grids.power_grids.len();
        let mut all_storages = {
            profiling::scope!("Generate all_storages list");
            storages_by_item(
                &mut self.power_grids,
                &mut self.chests,
                &mut self.ore_store.drills,
                data_store,
            )
        };
        let sizes: Vec<_> = sizes(data_store, num_grids_total).into_iter().collect();
        // dbg!(&all_storages);
        assert_eq!(sizes.len(), data_store.item_display_names.len());
        let mut storages_by_item: Box<[_]> = {
            profiling::scope!("Sort storages by item");
            let storages_by_item = full_to_by_item(&mut all_storages, &sizes);
            storages_by_item.collect()
        };

        self.belts.pre_pure_update(data_store);

        // let total_belt_len: usize = self.belts.inner.smart_belts.iter().flat_map(|store| store.belts.iter()).map(|belt| usize::from(belt.get_len())).sum();
        // let total_num_belt_inserters: usize = self.belts.inner.smart_belts.iter().flat_map(|store| store.belts.iter()).map(|belt| belt.get_num_inserters()).sum();
        // let num_belts: usize = self.belts.inner.smart_belts.iter().flat_map(|store| store.belts.iter()).count();
        // let avg_num_inserters_per_belt = total_num_belt_inserters as f64 / num_belts as f64;
        // let count_which_would_be_on_stack = self.belts.inner.smart_belts.iter().flat_map(|store| store.belts.iter()).map(|belt| belt.get_num_inserters() * (mem::size_of::<crate::inserter::belt_storage_inserter_non_const_gen::BeltStorageInserterDyn>())).filter(|&size| size > 0).filter(|&size| size <= 15).count();
        // let avg_size_inserters = avg_num_inserters_per_belt * mem::size_of::<crate::inserter::belt_storage_inserter_non_const_gen::BeltStorageInserterDyn>() as f64;

        // let perc_inline = count_which_would_be_on_stack as f64 / num_belts as f64;

        // let avg_len = total_belt_len as f64 / num_belts as f64;

        // let total_num_storage_inserters: usize = self.storage_storage_inserters.inserters.iter().flat_map(|ins| ins.values()).map(|(a, b)| b.get_num_inserters()).sum();

        // dbg!(total_num_belt_inserters);
        // dbg!(avg_num_inserters_per_belt);
        // dbg!(avg_size_inserters);
        // dbg!(perc_inline);
        // dbg!(avg_len);
        // dbg!(total_num_storage_inserters);

        let update_timers = &self.belts.inner.belt_update_timers;
        let sushi_splitters = &self.belts.inner.sushi_splitters;
        rayon::scope(|scope| {
            {
                // Update all the "Pure Belts"
                self.belts
                    .inner
                    .smart_belts
                    .iter_mut()
                    .zip(storages_by_item.iter_mut())
                    .zip(
                        self.belts
                            .inner
                            .belt_belt_inserters
                            .pure_to_pure_inserters
                            .iter_mut(),
                    )
                    .zip(self.storage_storage_inserters.inserters.iter_mut().zip(self.fluid_store.fluid_systems_with_fluid.iter_mut()))
                    .zip(self.item_times.iter_mut())
                    .enumerate()
                    // Queue long running items first, so we do not end up waiting for a long running item at the end, which we could have started early on
                    // This alone is ~10% faster on a test game I have
                    .sorted_unstable_by_key(|&(_, (_, &mut avg_time))| -(avg_time * 1000.0) as i32)
                    .for_each(
                        |(
                            item_id,
                            (
                                (
                                    ((belt_store, item_storages), pure_to_pure_inserters),
                                    (storage_storage_inserter_stores, fluid_store),
                                ),
                                avg_time,
                            ),
                        )| {
                            scope.spawn(move |_| {
                                let item = Item {
                                    id: item_id.try_into().unwrap(),
                                };
                                profiling::scope!(
                                    "Pure Update",
                                    format!("Item: {}", data_store.item_display_names[item_id]).as_str()
                                );
                                let pure_update_start = Instant::now();
                                {
                                    profiling::scope!(
                                        "Pure Belt Update",
                                        format!("Item: {}", data_store.item_display_names[item_id]).as_str()
                                    );

                                    let grid_size = grid_size(item, data_store);

                                    {
                                        profiling::scope!(
                                            "Update Belts",
                                            format!("Count: {}", belt_store.belts.len())
                                        );
                                        for (belt, ty) in
                                            belt_store.belts.iter_mut().zip(&belt_store.belt_ty)
                                        {
                                            // TODO: Avoid last minute decision making
                                            if update_timers[usize::from(*ty)] >= 120 {
                                                belt.update(sushi_splitters);
                                            }
                                            belt.update_inserters(item_storages, grid_size);
                                        }
                                    }

                                    {
                                        profiling::scope!("Update PurePure Inserters");
                                        for (
                                            ins,
                                            ((source, source_pos), (dest, dest_pos), cooldown, filter),
                                        ) in pure_to_pure_inserters.iter_mut().flatten()
                                        {
                                            let [mut source_loc, mut dest_loc] = if *source == *dest {
                                                assert_ne!(
                                                    source_pos, dest_pos,
                                                    "An inserter cannot take and drop off on the same tile"
                                                );
                                                // We are taking and placing onto the same belt
                                                let belt = &mut belt_store.belts[*source];

                                                belt.get_two([(*source_pos).into(), (*dest_pos).into()]).map(|v| *v)
                                            } else {
                                                let [inp, out] = belt_store
                                                    .belts
                                                    .get_disjoint_mut([*source, *dest])
                                                    .unwrap();

                                                [*inp.get(*source_pos), *out.get(*dest_pos)]
                                            };

                                            if *cooldown == 0 {
                                                ins.update_instant(&mut source_loc,&mut  dest_loc);
                                            } else {
                                                ins.update(
                                                    &mut source_loc,
                                                    &mut dest_loc,
                                                    *cooldown,
                                                    // FIXME:
                                                    1,
                                                    (),
                                                    |_| {
                                                        filter
                                                            .map(|filter_item| filter_item == item)
                                                            .unwrap_or(true)
                                                    },
                                                );
                                            }

                                            {
                                                profiling::scope!("Update update_first_free_pos");
                                                if !source_loc {
                                                    let _: Option<_> = belt_store.belts[*source]
                                                        .remove_item(*source_pos);
                                                }

                                                if dest_loc {
                                                    let _ = belt_store.belts[*dest]
                                                        .try_insert_item(*dest_pos, item);
                                                }
                                            }
                                        }
                                    }
                                }

                                {

                                    let item = Item {
                                        id: item_id.try_into().unwrap(),
                                    };

                                    let grid_size = grid_size(item, data_store);
                                    let num_recipes = num_recipes(item, data_store);

                                    if data_store.item_is_fluid[item_id] {
                                        profiling::scope!(
                                            "FluidSystem Update",
                                            format!("Item: {}", data_store.item_display_names[item_id])
                                                .as_str()
                                        );
                                        for fluid_system in fluid_store {
                                            // FIXME: Switch to holes
                                            if let Some(fluid_system) = fluid_system {
                                                update_fluid_system(&mut fluid_system.hot_data, item_storages, grid_size);

                                            }
                                        }
                                    } else {
                                        profiling::scope!(
                                            "StorageStorage Inserter Update",
                                            format!("Item: {}", data_store.item_display_names[item_id])
                                                .as_str()
                                        );
                                        for (ins_store,) in storage_storage_inserter_stores.values_mut()
                                        {
                                            profiling::scope!(
                                                "StorageStorage Inserter Update",
                                                format!("Movetime: {}", ins_store.movetime).as_str()
                                            );
                                            ins_store.update(
                                                item_id,
                                                item_storages,
                                                grid_size,
                                                current_tick,
                                            );
                                        }
                                    }
                                }

                                let pure_update_time = pure_update_start.elapsed();

                                *avg_time = *avg_time
                                    + (pure_update_time.as_millis() as f32 - *avg_time) / 20.0;
                            })
                        },
                    );
            }
        });

        {
            profiling::scope!("Sushi Belt Update");
            self.belts
                .sushi_belt_update(storages_by_item.par_iter_mut(), data_store);
        }
    }
}

#[cfg(feature = "client")]
pub enum AppState {
    MainMenu {
        in_ip_box: Option<(String, bool)>,
        gigabase_size: u16,
    },
    Ingame,
    Loading {
        start_time: Instant,
        /// WARNING: This is a f64!
        progress: Arc<AtomicU64>,
        game_state_receiver: Receiver<(LoadedGame, Arc<AtomicU64>, Sender<Input>)>,
    },
}

#[derive(Debug, Clone, Copy)]
enum InserterUpdateInfo {
    AssemblerRecipeChanged { pos: Position, size: (u8, u8) },
    NewAssembler { pos: Position, size: (u8, u8) },
    NewBelt { pos: Position },
}

#[derive(Debug)]
pub enum InstantiateInserterError<ItemIdxType: WeakIdxTrait> {
    NotUnattachedInserter,
    SourceMissing,
    DestMissing,
    PleaseSpecifyFilter {
        belts_which_could_help: Vec<BeltTileId<ItemIdxType>>,
    },
    ItemConflict {
        belts_which_could_help: Vec<BeltTileId<ItemIdxType>>,
    },
}

impl<ItemIdxType: IdxTrait, RecipeIdxType: IdxTrait> GameState<ItemIdxType, RecipeIdxType> {
    #[allow(clippy::too_many_lines)]
    #[profiling::function]
    pub fn apply_actions(
        sim_state: &mut SimulationState<ItemIdxType, RecipeIdxType>,
        world: &mut World<ItemIdxType, RecipeIdxType>,
        actions: impl IntoIterator<Item = impl Borrow<ActionType<ItemIdxType, RecipeIdxType>>>,
        data_store: &DataStore<ItemIdxType, RecipeIdxType>,
    ) {
        let mut fake_game_state = FakeGameState {
            simulation_state: sim_state,
            world: world,
        };
        let game_state = &mut fake_game_state;

        // let num_assemblers = game_state
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
            match action.borrow() {
                ActionType::SetActiveResearch { tech } => {
                    game_state.simulation_state.tech_state.current_technology = *tech;
                },

                ActionType::CheatUnlockTechnology { tech } => {
                    if game_state.simulation_state.tech_state.current_technology == Some(*tech) {
                        game_state.simulation_state.tech_state.current_technology = None;
                    }
                    game_state
                        .simulation_state
                        .tech_state
                        .in_progress_technologies
                        .remove(&tech);
                    *game_state
                        .simulation_state
                        .tech_state
                        .finished_technologies
                        .entry(*tech)
                        .or_default() += 1;
                    for recipe in &data_store
                        .technology_tree
                        .node_weight(NodeIndex::from(tech.id))
                        .unwrap()
                        .effect
                        .unlocked_recipes
                    {
                        game_state.simulation_state.tech_state.recipe_active[recipe.into_usize()] =
                            true;
                    }
                },

                ActionType::CheatRelockTechnology { tech } => {
                    game_state
                        .simulation_state
                        .tech_state
                        .finished_technologies
                        .remove(&tech);

                    for recipe in &mut game_state.simulation_state.tech_state.recipe_active {
                        *recipe = false;
                    }
                    for recipe in game_state
                        .simulation_state
                        .tech_state
                        .finished_technologies
                        .iter()
                        .flat_map(|(tech, _)| {
                            data_store
                                .technology_tree
                                .node_weight(NodeIndex::from(tech.id))
                                .unwrap()
                                .effect
                                .unlocked_recipes
                                .iter()
                        })
                    {
                        game_state.simulation_state.tech_state.recipe_active[recipe.into_usize()] =
                            true;
                    }
                },

                ActionType::PlaceFloorTile(place_floor_tile_by_hand_info) => {
                    match &place_floor_tile_by_hand_info.ghost_info.position {
                        &PositionInfo::Rect { pos, width, height } => {
                            for x in pos.x..(pos.x + i32::try_from(width).unwrap()) {
                                for y in pos.y..(pos.y + i32::try_from(height).unwrap()) {
                                    let _ = game_state.world.set_floor_tile(
                                        Position { x, y },
                                        place_floor_tile_by_hand_info.ghost_info.tile,
                                    );
                                }
                            }
                        },
                        &PositionInfo::Single { pos } => {
                            let _ = game_state
                                .world
                                .set_floor_tile(pos, place_floor_tile_by_hand_info.ghost_info.tile);
                        },
                        PositionInfo::List { positions } => {
                            for pos in positions {
                                let _ = game_state.world.set_floor_tile(
                                    *pos,
                                    place_floor_tile_by_hand_info.ghost_info.tile,
                                );
                            }
                        },
                    }
                },
                ActionType::SetChestSlotLimit { pos, num_slots } => {
                    game_state
                        .world
                        .get_entity_at_mut(*pos, data_store)
                        .map(|e| match e {
                            Entity::Chest {
                                ty,
                                pos,
                                item,
                                slot_limit,
                            } => {
                                if let Some((item, index)) = item {
                                    let removed_items =
                                        game_state.simulation_state.factory.chests.stores
                                            [usize_from(item.id)]
                                        .change_chest_size(
                                            *index,
                                            data_store.item_stack_sizes[usize_from(item.id)]
                                                as ChestSize
                                                * ChestSize::from(*num_slots),
                                        );
                                }
                                *slot_limit = *num_slots;
                            },
                            _ => {
                                warn!("Tried to set slot limit on non chest");
                            },
                        });
                },
                ActionType::OverrideInserterMovetime { pos, new_movetime } => {
                    game_state
                        .world
                        .get_entity_at_mut(*pos, data_store)
                        .map(|e| match e {
                            Entity::Inserter {
                                ty,
                                user_movetime,
                                info,
                                ..
                            } => {
                                match info {
                                    InserterInfo::NotAttached {} => {},
                                    InserterInfo::Attached { info } => match info {
                                        AttachedInserter::BeltStorage { id, belt_pos } => {
                                            game_state
                                                .simulation_state
                                                .factory
                                                .belts
                                                .change_inserter_movetime(
                                                    *id,
                                                    *belt_pos,
                                                    new_movetime.map(|v| v.into()).unwrap_or(
                                                        data_store.inserter_infos[*ty as usize]
                                                            .swing_time_ticks,
                                                    ),
                                                );
                                        },
                                        AttachedInserter::BeltBelt { item, inserter } => todo!(),
                                        AttachedInserter::StorageStorage { item, inserter } => {
                                            let old_movetime =
                                                user_movetime.map(|v| v.into()).unwrap_or(
                                                    data_store.inserter_infos[*ty as usize]
                                                        .swing_time_ticks,
                                                );

                                            let new_movetime =
                                                new_movetime.map(|v| v.into()).unwrap_or(
                                                    data_store.inserter_infos[*ty as usize]
                                                        .swing_time_ticks,
                                                );

                                            if old_movetime != new_movetime {
                                                let new_id = game_state
                                                    .simulation_state
                                                    .factory
                                                    .storage_storage_inserters
                                                    .change_movetime(
                                                        *item,
                                                        old_movetime.into(),
                                                        new_movetime.into(),
                                                        *inserter,
                                                    );

                                                *inserter = new_id;
                                            }
                                        },
                                    },
                                }
                                *user_movetime = *new_movetime;
                            },
                            _ => {
                                warn!("Tried to set Inserter Settings on non inserter");
                            },
                        });
                },
                ActionType::PlaceEntity(place_entity_info) => {
                    let force = place_entity_info.force;
                    if force {
                        let pos = action.borrow().get_pos().unwrap();
                        let size = action.borrow().get_building_size(data_store).unwrap();

                        for x in 0..size[0] {
                            for y in 0..size[1] {
                                let position = Position {
                                    x: pos.x + x as i32,
                                    y: pos.y + y as i32,
                                };

                                game_state.world.remove_entity_at(
                                    position,
                                    &mut game_state.simulation_state,
                                    data_store,
                                );
                            }
                        }
                    }
                    match place_entity_info.entities {
                        crate::frontend::action::place_entity::EntityPlaceOptions::Single(
                            place_entity_type,
                        ) => match place_entity_type {
                            crate::frontend::world::tile::PlaceEntityType::Assembler {
                                pos,
                                ty,
                                rotation,
                            } => {
                                let size = data_store.assembler_info[ty as usize].size(rotation);
                                if !game_state.world.can_fit(pos, size, data_store) {
                                    warn!("Tried to place assembler where it does not fit");
                                    continue;
                                }

                                let powered_by =
                                    game_state.world.is_powered_by(pos, size, data_store);

                                let modules = if let Some(found_idx) =
                                    game_state.world.module_slot_dedup_table.iter().position(
                                        |slots| {
                                            slots.len()
                                                == data_store.assembler_info[usize::from(ty)]
                                                    .num_module_slots
                                                    as usize
                                                && slots.iter().all(|v| v.is_none())
                                        },
                                    ) {
                                    found_idx as u32
                                } else {
                                    game_state.world.module_slot_dedup_table.push(
                                        vec![
                                            None;
                                            data_store.assembler_info[usize::from(ty)]
                                                .num_module_slots
                                                as usize
                                        ]
                                        .into_boxed_slice()
                                        .into(),
                                    );
                                    (game_state.world.module_slot_dedup_table.len() - 1) as u32
                                };

                                if let Some(pole_position) = powered_by {
                                    if let Err(e) = game_state.world.add_entity(
                                        crate::frontend::world::tile::Entity::Assembler {
                                            ty,
                                            pos,
                                            info: AssemblerInfo::PoweredNoRecipe(pole_position),
                                            modules,
                                            rotation,
                                        },
                                        &mut game_state.simulation_state,
                                        data_store,
                                    ) {
                                        warn!("Placing Entity failed: {:?}", e);
                                    }
                                } else {
                                    if let Err(e) = game_state.world.add_entity(
                                        crate::frontend::world::tile::Entity::Assembler {
                                            ty,
                                            pos,
                                            info: AssemblerInfo::UnpoweredNoRecipe,
                                            modules,
                                            rotation,
                                        },
                                        &mut game_state.simulation_state,
                                        data_store,
                                    ) {
                                        warn!("Placing Entity failed: {:?}", e);
                                    }
                                }
                            },
                            crate::frontend::world::tile::PlaceEntityType::Inserter {
                                pos,
                                dir,
                                filter,
                                ty,
                                user_movetime,
                            } => {
                                let ret = Self::add_inserter(
                                    &mut game_state.world,
                                    &mut game_state.simulation_state,
                                    ty,
                                    user_movetime,
                                    pos,
                                    dir,
                                    filter,
                                    data_store,
                                );
                            },
                            crate::frontend::world::tile::PlaceEntityType::Belt {
                                pos,
                                direction,
                                ty,
                            } => {
                                if !game_state.world.can_fit(pos, (1, 1), data_store) {
                                    warn!("Tried to place belt where it does not fit");
                                    continue;
                                }

                                handle_belt_placement(
                                    game_state.world,
                                    game_state.simulation_state,
                                    pos,
                                    direction,
                                    ty,
                                    data_store,
                                );

                                Self::update_inserters(
                                    game_state.world,
                                    game_state.simulation_state,
                                    InserterUpdateInfo::NewBelt { pos },
                                    data_store,
                                );
                            },
                            crate::frontend::world::tile::PlaceEntityType::Underground {
                                pos,
                                direction,
                                ty,
                                underground_dir,
                            } => {
                                if !game_state.world.can_fit(pos, (1, 1), data_store) {
                                    warn!("Tried to place underground_belt where it does not fit");
                                    continue;
                                }

                                handle_underground_belt_placement(
                                    game_state.world,
                                    game_state.simulation_state,
                                    pos,
                                    direction,
                                    ty,
                                    underground_dir,
                                    data_store,
                                );

                                GameState::update_inserters(
                                    game_state.world,
                                    game_state.simulation_state,
                                    InserterUpdateInfo::NewBelt { pos },
                                    data_store,
                                );
                            },
                            crate::frontend::world::tile::PlaceEntityType::PowerPole {
                                pos: pole_pos,
                                ty,
                            } => {
                                // Check if the powerpole fits
                                if !game_state.world.can_fit(
                                    pole_pos,
                                    data_store.power_pole_data[usize::from(ty)].size,
                                    data_store,
                                ) {
                                    warn!("Tried to place power pole where it does not fit");
                                    continue;
                                }

                                // Check which poles are in range to connect to
                                let connection_candidates: Vec<_> = game_state
                                    .world
                                    .get_power_poles_which_could_connect_to_pole_at(
                                        pole_pos,
                                        data_store.power_pole_data[usize::from(ty)].size,
                                        data_store.power_pole_data[usize::from(ty)]
                                            .connection_range,
                                        data_store,
                                    )
                                    .into_iter()
                                    .map(|e| e.get_pos())
                                    .collect();

                                match game_state.simulation_state.factory.power_grids.add_pole(
                                    pole_pos,
                                    connection_candidates.iter().copied(),
                                    data_store,
                                ) {
                                    Some(storage_updates) => {
                                        // Handle storage updates
                                        for storage_update in storage_updates {
                                            let mut entity_size = None;
                                            game_state.world.get_entity_at_mut(storage_update.position, data_store).map(|e| {
                                        match (e, storage_update.new_pg_entity.clone()) {
                                            (Entity::Assembler { ty, pos: _, info: AssemblerInfo::Powered { id, pole_position: _, weak_index: _ }, modules: _, rotation }, crate::power::power_grid::PowerGridEntity::Assembler { ty: _, recipe, index }) => {
                                                entity_size = Some(data_store.assembler_info[usize::from(*ty)].size(*rotation));

                                                assert_eq!(id.recipe, recipe);
                                                id.grid = storage_update.new_grid;
                                                id.assembler_index = index;
                                                // FIXME: Store and update the weak_index
                                            },
                                            (Entity::Lab { pos: _, ty, modules: _, pole_position: Some((_pole_pos, _weak_idx, lab_store_index)) }, crate::power::power_grid::PowerGridEntity::Lab { ty: _, index: new_idx  }) => {
                                                entity_size = Some(data_store.lab_info[usize::from(*ty)].size);


                                                *lab_store_index = new_idx;
                                                // The weak index stays the same since it it still connected to the same power pole
                                            }

                                            (_, _) => todo!("Handler storage_update {storage_update:?}")
                                        }
                                    });

                                            // FIXME: Rotation
                                            let e_size = entity_size.unwrap();

                                            let inserter_range =
                                                data_store.max_inserter_search_range;

                                            game_state.world.mutate_entities_colliding_with(
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
                                        |e, _| {
                                            match e {
                                                Entity::Inserter {
                                                    ty,
                                                    user_movetime,
                                                    pos,
                                                    direction,

                                                    info, ..
                                                } => match info {
                                                    InserterInfo::NotAttached { .. } => {},
                                                    InserterInfo::Attached {
                                                        info,
                                                    } => {
                                                        let start_pos = data_store.inserter_start_pos(*ty, *pos, *direction);
                                                        let end_pos = data_store.inserter_end_pos(*ty, *pos, *direction);

                                                        if start_pos.contained_in(
                                                            storage_update.position,
                                                            e_size,
                                                        ) {
                                                            match info {
                                                                AttachedInserter::BeltStorage { id, belt_pos } => {
                                                                    let new_storage = match storage_update.new_pg_entity {
                                                                        crate::power::power_grid::PowerGridEntity::Assembler { recipe, index, .. } => Storage::Assembler { grid: storage_update.new_grid, recipe_idx_with_this_item: recipe.id, index },
                                                                        crate::power::power_grid::PowerGridEntity::Lab { index, .. } => Storage::Lab { grid: storage_update.new_grid, index },
                                                                        crate::power::power_grid::PowerGridEntity::LazyPowerProducer { item, index } => todo!(),
                                                                        crate::power::power_grid::PowerGridEntity::SolarPanel { .. } => unreachable!(),
                                                                        crate::power::power_grid::PowerGridEntity::Accumulator { .. } => unreachable!(),
                                                                        crate::power::power_grid::PowerGridEntity::Beacon { .. } => unreachable!(),
                                                                    }.translate(game_state.simulation_state.factory.belts.get_inserter_item(*id, *belt_pos), data_store).unwrap();
                                                                    game_state.simulation_state.factory.belts.update_belt_storage_inserter_src(*id, *belt_pos, game_state.simulation_state.factory.belts.get_inserter_item(*id, *belt_pos), new_storage, data_store);
                                                                },
                                                                AttachedInserter::BeltBelt { .. } => {
                                                                    unreachable!("A BeltBelt inserter should not be pointing at a machine")
                                                                },
                                                                AttachedInserter::StorageStorage { item, inserter } => {
                                                                    let new_storage = match storage_update.new_pg_entity {
                                                                        crate::power::power_grid::PowerGridEntity::Assembler { recipe, index, .. } => Storage::Assembler { grid: storage_update.new_grid, recipe_idx_with_this_item: recipe.id, index },
                                                                        crate::power::power_grid::PowerGridEntity::Lab { index, .. } => Storage::Lab { grid: storage_update.new_grid, index },
                                                                        crate::power::power_grid::PowerGridEntity::LazyPowerProducer { item, index } => todo!(),
                                                                        crate::power::power_grid::PowerGridEntity::SolarPanel { .. } => unreachable!(),
                                                                        crate::power::power_grid::PowerGridEntity::Accumulator { .. } => unreachable!(),
                                                                        crate::power::power_grid::PowerGridEntity::Beacon { .. } => unreachable!(),
                                                                    }.translate(*item, data_store).unwrap();

                                                                    let movetime = user_movetime.map(|v| v.into()).unwrap_or(data_store.inserter_infos[*ty as usize].swing_time_ticks);

                                                                    let new_id = game_state.simulation_state.factory.storage_storage_inserters.update_inserter_src(*item, movetime.into(), *inserter, new_storage, data_store);

                                                                    *inserter = new_id;
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
                                                                        crate::power::power_grid::PowerGridEntity::Assembler { recipe, index, .. } => Storage::Assembler { grid: storage_update.new_grid, recipe_idx_with_this_item: recipe.id, index },
                                                                        crate::power::power_grid::PowerGridEntity::Lab { index, .. } => Storage::Lab { grid: storage_update.new_grid, index },
                                                                        crate::power::power_grid::PowerGridEntity::LazyPowerProducer { item, index } => todo!(),
                                                                        crate::power::power_grid::PowerGridEntity::SolarPanel { .. } => unreachable!(),
                                                                        crate::power::power_grid::PowerGridEntity::Accumulator { .. } => unreachable!(),
                                                                        crate::power::power_grid::PowerGridEntity::Beacon { .. } => unreachable!(),
                                                                    }.translate(game_state.simulation_state.factory.belts.get_inserter_item(*id, *belt_pos), data_store).unwrap();
                                                                    game_state.simulation_state.factory.belts.update_belt_storage_inserter_dest(*id, *belt_pos, game_state.simulation_state.factory.belts.get_inserter_item(*id, *belt_pos), new_storage, data_store);
                                                                },
                                                                AttachedInserter::BeltBelt { .. } => {
                                                                    unreachable!("A BeltBelt inserter should not be pointing at a machine")
                                                                },
                                                                AttachedInserter::StorageStorage { item, inserter } => {
                                                                    let new_storage = match storage_update.new_pg_entity {
                                                                        crate::power::power_grid::PowerGridEntity::Assembler { recipe, index, .. } => Storage::Assembler { grid: storage_update.new_grid, recipe_idx_with_this_item: recipe.id, index },
                                                                        crate::power::power_grid::PowerGridEntity::Lab { index, .. } => Storage::Lab { grid: storage_update.new_grid, index },
                                                                        crate::power::power_grid::PowerGridEntity::LazyPowerProducer { item, index } => todo!(),
                                                                        crate::power::power_grid::PowerGridEntity::SolarPanel { .. } => unreachable!(),
                                                                        crate::power::power_grid::PowerGridEntity::Accumulator { .. } => unreachable!(),
                                                                        crate::power::power_grid::PowerGridEntity::Beacon { .. } => unreachable!(),
                                                                    }.translate(*item, data_store).unwrap();
                                                                    let movetime = user_movetime.map(|v| v.into()).unwrap_or(data_store.inserter_infos[*ty as usize].swing_time_ticks);

                                                                    let new_id = game_state.simulation_state.factory.storage_storage_inserters.update_inserter_dest(*item, movetime.into(), *inserter, new_storage, data_store);

                                                                    *inserter = new_id;
                                                                },
                                                            }
                                                        }
                                                    },
                                                },
                                                Entity::FluidTank { ty, pos, rotation } => {
                                                    let id: FluidSystemId<_> = game_state.simulation_state.factory.fluid_store.fluid_box_pos_to_network_id[pos];
                                                    if let Some(fluid) = id.fluid {
                                                        let storage = match storage_update.new_pg_entity {
                                                            crate::power::power_grid::PowerGridEntity::Assembler { ty, recipe, index } => Storage::Assembler { grid: storage_update.old_grid, recipe_idx_with_this_item: recipe.id, index },
                                                            crate::power::power_grid::PowerGridEntity::Lab { ty, index } => Storage::Lab { grid: storage_update.old_grid, index },
                                                            crate::power::power_grid::PowerGridEntity::LazyPowerProducer { item, index } => todo!(),
                                                            crate::power::power_grid::PowerGridEntity::SolarPanel { .. } => unreachable!(),
                                                            crate::power::power_grid::PowerGridEntity::Accumulator { .. } => unreachable!(),
                                                            crate::power::power_grid::PowerGridEntity::Beacon { .. } => unreachable!(),
                                                        };
                                                        dbg!(storage);
                                                        let Some(translated_storage) = storage.translate(fluid, data_store) else {
                                                            return ControlFlow::Continue(());
                                                        };
                                                        dbg!(translated_storage);
                                                        let old_storage = FakeUnionStorage::from_storage_with_statics_at_zero(fluid, translated_storage, data_store);
                                                        dbg!(old_storage);

                                                        let new_storage = match storage_update.new_pg_entity {
                                                            crate::power::power_grid::PowerGridEntity::Assembler { ty, recipe, index } => Storage::Assembler { grid: storage_update.new_grid, recipe_idx_with_this_item: recipe.id, index },
                                                            crate::power::power_grid::PowerGridEntity::Lab { ty, index } => Storage::Lab { grid: storage_update.new_grid, index },
                                                            crate::power::power_grid::PowerGridEntity::LazyPowerProducer { item, index } => todo!(),
                                                            crate::power::power_grid::PowerGridEntity::SolarPanel { .. } => unreachable!(),
                                                            crate::power::power_grid::PowerGridEntity::Accumulator { .. } => unreachable!(),
                                                            crate::power::power_grid::PowerGridEntity::Beacon { .. } => unreachable!(),
                                                        }.translate(fluid, data_store).unwrap();
                                                        game_state.simulation_state.factory.fluid_store.update_fluid_conn_if_needed(*pos, old_storage, FakeUnionStorage::from_storage_with_statics_at_zero(fluid, new_storage, data_store));
                                                    }
                                                },

                                                _ => {},
                                            }
                                            ControlFlow::Continue(())
                                        },
                                    );
                                        }
                                    },
                                    _ => {
                                        // No updates needed
                                    },
                                }

                                #[cfg(debug_assertions)]
                                {
                                    let affected_grids_and_potential_match = game_state
                                        .simulation_state
                                        .factory
                                        .power_grids
                                        .power_grids
                                        .iter()
                                        .filter(|grid| !grid.is_placeholder)
                                        .all(|pg| {
                                            pg.beacon_affected_entities
                                                .keys()
                                                .map(|e| e.get_power_grid())
                                                .all(|affected_grid| {
                                                    pg.potential_beacon_affected_powergrids
                                                        .contains(&affected_grid)
                                                })
                                        });
                                    if !affected_grids_and_potential_match {
                                        dbg!(action.borrow());
                                    }
                                    assert!(affected_grids_and_potential_match);
                                }

                                // Add the powerpole entity to the correct chunk
                                if let Err(e) = game_state.world.add_entity(
                                    Entity::PowerPole { ty, pos: pole_pos },
                                    &mut game_state.simulation_state,
                                    data_store,
                                ) {
                                    warn!("Unable to place entity {:?}", e);
                                }

                                #[cfg(debug_assertions)]
                                {
                                    let affected_grids_and_potential_match = game_state
                                        .simulation_state
                                        .factory
                                        .power_grids
                                        .power_grids
                                        .iter()
                                        .filter(|grid| !grid.is_placeholder)
                                        .all(|pg| {
                                            pg.beacon_affected_entities
                                                .keys()
                                                .map(|e| e.get_power_grid())
                                                .all(|affected_grid| {
                                                    pg.potential_beacon_affected_powergrids
                                                        .contains(&affected_grid)
                                                })
                                        });
                                    if !affected_grids_and_potential_match {
                                        dbg!(action.borrow());
                                    }
                                    assert!(affected_grids_and_potential_match);
                                }
                            },
                            crate::frontend::world::tile::PlaceEntityType::Splitter {
                                pos: splitter_pos,
                                direction: splitter_direction,
                                in_mode,
                                out_mode,

                                ty,
                            } => {
                                let (left_pos, right_pos) = match splitter_direction {
                                    Dir::North => (splitter_pos, splitter_pos + Dir::East),
                                    Dir::East => (splitter_pos, splitter_pos + Dir::South),
                                    Dir::South => (splitter_pos + Dir::East, splitter_pos),
                                    Dir::West => (splitter_pos + Dir::South, splitter_pos),
                                };
                                let game_state_positions = [left_pos, right_pos];
                                if game_state_positions
                                    .into_iter()
                                    .any(|pos| !game_state.world.can_fit(pos, (1, 1), data_store))
                                {
                                    warn!("Tried to place splitter where it does not fit");
                                    continue;
                                }
                                handle_splitter_placement(
                                    game_state.world,
                                    game_state.simulation_state,
                                    splitter_pos,
                                    splitter_direction,
                                    ty,
                                    in_mode,
                                    out_mode,
                                    data_store,
                                );
                            },
                            crate::frontend::world::tile::PlaceEntityType::Chest { pos, ty } => {
                                if !game_state.world.can_fit(
                                    pos,
                                    data_store.chest_tile_sizes[usize::from(ty)],
                                    data_store,
                                ) {
                                    warn!("Tried to place chest where it does not fit");
                                    continue;
                                }

                                if let Err(e) = game_state.world.add_entity(
                                    Entity::Chest {
                                        ty,
                                        pos,
                                        item: None,
                                        // slot_limit: data_store.chest_num_slots[usize::from(ty)],
                                        // FIXME: This is a hack to limit the size of buffers in the gigabase
                                        slot_limit: 1,
                                    },
                                    &mut game_state.simulation_state,
                                    data_store,
                                ) {
                                    warn!("Unable to place entity {:?}", e);
                                }
                            },
                            crate::frontend::world::tile::PlaceEntityType::SolarPanel {
                                pos,
                                ty,
                            } => {
                                let size = data_store.solar_panel_info[usize::from(ty)].size;
                                let size = size.into();

                                if !game_state.world.can_fit(pos, size, data_store) {
                                    warn!("Tried to place solar_panel where it does not fit");
                                    continue;
                                }

                                let powered_by =
                                    game_state.world.is_powered_by(pos, size, data_store);

                                let powered_by = if let Some(pole_pos) = powered_by {
                                    let grid = game_state
                                        .simulation_state
                                        .factory
                                        .power_grids
                                        .pole_pos_to_grid_id[&pole_pos];

                                    let grid = &mut game_state
                                        .simulation_state
                                        .factory
                                        .power_grids
                                        .power_grids[usize::from(grid)];

                                    let weak_idx =
                                        grid.add_solar_panel(pos, ty, pole_pos, data_store);

                                    Some((pole_pos, weak_idx))
                                } else {
                                    None
                                };

                                if let Err(e) = game_state.world.add_entity(
                                    Entity::SolarPanel {
                                        pos,
                                        ty,
                                        pole_position: powered_by,
                                    },
                                    &mut game_state.simulation_state,
                                    data_store,
                                ) {
                                    warn!("Unable to place entity {:?}", e);
                                }
                            },
                            crate::frontend::world::tile::PlaceEntityType::Accumulator {
                                pos,
                                ty,
                            } => {
                                let size = data_store.accumulator_info[usize::from(ty)].size;
                                let size = size.into();

                                if !game_state.world.can_fit(pos, size, data_store) {
                                    warn!("Tried to place accumulator where it does not fit");
                                    continue;
                                }

                                let powered_by =
                                    game_state.world.is_powered_by(pos, size, data_store);

                                let powered_by = if let Some(pole_pos) = powered_by {
                                    let grid = game_state
                                        .simulation_state
                                        .factory
                                        .power_grids
                                        .pole_pos_to_grid_id[&pole_pos];

                                    let grid = &mut game_state
                                        .simulation_state
                                        .factory
                                        .power_grids
                                        .power_grids[usize::from(grid)];

                                    let weak_idx =
                                        grid.add_accumulator(pos, ty, pole_pos, data_store);

                                    Some((pole_pos, weak_idx))
                                } else {
                                    None
                                };

                                if let Err(e) = game_state.world.add_entity(
                                    Entity::Accumulator {
                                        pos,
                                        ty,
                                        pole_position: powered_by,
                                    },
                                    &mut game_state.simulation_state,
                                    data_store,
                                ) {
                                    warn!("Unable to place entity {:?}", e);
                                }
                            },
                            crate::frontend::world::tile::PlaceEntityType::Lab { pos, ty } => {
                                if !game_state.world.can_fit(
                                    pos,
                                    data_store.lab_info[usize::from(ty)].size,
                                    data_store,
                                ) {
                                    warn!("Tried to place lab where it does not fit");
                                    continue;
                                }

                                // let modules: ModuleSlots = vec![
                                //     None;
                                //     data_store.lab_info[usize::from(ty)].num_module_slots
                                //         as usize
                                // ]
                                // .into_boxed_slice()
                                // .into();

                                // FIXME: Remove cheated modules
                                let modules: ModuleSlots = vec![
                                    Some(1);
                                    data_store.lab_info[usize::from(ty)].num_module_slots
                                        as usize
                                ]
                                .into_boxed_slice()
                                .into();

                                let powered_by = game_state.world.is_powered_by(
                                    pos,
                                    data_store.lab_info[usize::from(ty)].size,
                                    data_store,
                                );

                                let powered_by = if let Some(pole_pos) = powered_by {
                                    let grid = game_state
                                        .simulation_state
                                        .factory
                                        .power_grids
                                        .pole_pos_to_grid_id[&pole_pos];

                                    let grid = &mut game_state
                                        .simulation_state
                                        .factory
                                        .power_grids
                                        .power_grids[usize::from(grid)];

                                    let weak_idx =
                                        grid.add_lab(pos, ty, &*modules, pole_pos, data_store);

                                    Some((pole_pos, weak_idx.0, weak_idx.1))
                                } else {
                                    None
                                };

                                let module_idx = if let Some(idx) = game_state
                                    .world
                                    .module_slot_dedup_table
                                    .iter()
                                    .position(|slots| *slots == modules)
                                {
                                    idx as u32
                                } else {
                                    game_state.world.module_slot_dedup_table.push(modules);
                                    (game_state.world.module_slot_dedup_table.len() - 1) as u32
                                };

                                if let Err(e) = game_state.world.add_entity(
                                    Entity::Lab {
                                        pos,
                                        ty,
                                        modules: module_idx,
                                        pole_position: powered_by,
                                    },
                                    &mut game_state.simulation_state,
                                    data_store,
                                ) {
                                    warn!("Unable to place entity {:?}", e);
                                }
                            },
                            crate::frontend::world::tile::PlaceEntityType::Beacon { pos, ty } => {
                                let size = data_store.beacon_info[usize::from(ty)].size;

                                if !game_state.world.can_fit(pos, size, data_store) {
                                    warn!("Tried to place beacon where it does not fit");
                                    continue;
                                }

                                let modules: Box<[_]> = Box::new(
                                    // TODO: Do not add modules immediately
                                    [Some(0); 2],
                                );
                                let modules = modules.into();

                                let powered_by = game_state.world.is_powered_by(
                                    pos,
                                    data_store.beacon_info[usize::from(ty)].size,
                                    data_store,
                                );

                                let powered_by = if let Some(pole_pos) = powered_by {
                                    let weak_idx = game_state
                                        .simulation_state
                                        .factory
                                        .power_grids
                                        .add_beacon(ty, pos, pole_pos, &modules, [], data_store);

                                    Some((pole_pos, weak_idx))
                                } else {
                                    None
                                };

                                let modules_idx = if let Some(idx) = game_state
                                    .world
                                    .module_slot_dedup_table
                                    .iter()
                                    .position(|slots| *slots == modules)
                                {
                                    idx as u32
                                } else {
                                    game_state.world.module_slot_dedup_table.push(modules);
                                    (game_state.world.module_slot_dedup_table.len() - 1) as u32
                                };

                                if let Err(e) = game_state.world.add_entity(
                                    Entity::Beacon {
                                        pos,
                                        ty,
                                        modules: modules_idx,
                                        pole_position: powered_by,
                                    },
                                    &mut game_state.simulation_state,
                                    data_store,
                                ) {
                                    warn!("Unable to place entity {:?}", e);
                                }
                            },
                            crate::frontend::world::tile::PlaceEntityType::FluidTank {
                                ty,
                                pos,
                                rotation,
                            } => {
                                let size = data_store.fluid_tank_infos[usize::from(ty)].size;
                                // FIXME: Stop ignoring rotation
                                if !game_state.world.can_fit(pos, size.into(), data_store) {
                                    warn!("Tried to place storage tank where it does not fit");
                                    continue;
                                }

                                let search_range =
                                    data_store.fluid_tank_infos[usize::from(ty)].max_search_range;

                                // Get connecting entities:
                                let connecting_fluid_box_positions = game_state
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
                                        Entity::Assembler { .. } => {
                                            // FIXME: Implement assembler flowthough
                                            None
                                        },
                                        Entity::Lab { .. } => {
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
                                    });

                                let in_out_connections = game_state
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
                                            info: AssemblerInfo::Powered { id, .. },
                                            rotation: assembler_rotation,
                                            ..
                                        } => {
                                            let assembler_size = data_store.assembler_info
                                                [usize::from(*assembler_ty)]
                                            .size(*assembler_rotation);
                                            let assembler_size =
                                                [assembler_size.0, assembler_size.1];

                                            let recipe_fluid_inputs: Vec<_> = data_store
                                                .recipe_to_items[id.recipe.into_usize()]
                                            .iter()
                                            .filter_map(|(dir, item)| {
                                                (*dir == ItemRecipeDir::Ing
                                                    && data_store.item_is_fluid[item.into_usize()])
                                                .then_some(*item)
                                            })
                                            .collect();
                                            let recipe_fluid_outputs: Vec<_> = data_store
                                                .recipe_to_items[id.recipe.into_usize()]
                                            .iter()
                                            .filter_map(|(dir, item)| {
                                                (*dir == ItemRecipeDir::Out
                                                    && data_store.item_is_fluid[item.into_usize()])
                                                .then_some(*item)
                                            })
                                            .collect();

                                            let fluid_pure_outputs: Vec<_> = data_store
                                                .assembler_info[usize::from(*assembler_ty)]
                                            .fluid_connections
                                            .iter()
                                            .filter(|(_conn, allowed)| {
                                                *allowed
                                                    == AllowedFluidDirection::Single(
                                                        ItemRecipeDir::Out,
                                                    )
                                                    || matches!(
                                                        *allowed,
                                                        AllowedFluidDirection::Both { .. }
                                                    )
                                            })
                                            .collect();

                                            let fluid_pure_inputs: Vec<_> = data_store
                                                .assembler_info[usize::from(*assembler_ty)]
                                            .fluid_connections
                                            .iter()
                                            .filter(|(_conn, allowed)| {
                                                *allowed
                                                    == AllowedFluidDirection::Single(
                                                        ItemRecipeDir::Ing,
                                                    )
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
                                                        .zip(iter::repeat(
                                                            FluidConnectionDir::Input,
                                                        )),
                                                );

                                            Some(all_connections_with_items.filter_map(
                                                move |(
                                                    (item, (fluid_conn, _allowed)),
                                                    fluid_dir,
                                                )| {
                                                    can_fluid_tanks_connect_to_single_connection(
                                                        pos,
                                                        ty,
                                                        rotation,
                                                        *assembler_pos,
                                                        *fluid_conn,
                                                        *assembler_rotation,
                                                        assembler_size,
                                                        data_store,
                                                    )
                                                    .map(|(dest_conn, _dest_conn_dir)| {
                                                        (
                                                            fluid_dir,
                                                            item,
                                                            FakeUnionStorage::from_storage_with_statics_at_zero(item, Storage::Assembler {
                                                                grid: id.grid,
                                                                index: id.assembler_index,
                                                                recipe_idx_with_this_item:
                                                                    data_store
                                                                        .recipe_to_translated_index
                                                                        [&(id.recipe, item)],
                                                            }, data_store) ,
                                                            dest_conn,
                                                            Box::new(|_weak_index: WeakIndex| {})
                                                                as Box<dyn FnOnce(WeakIndex) -> ()>,
                                                        )
                                                    })
                                                },
                                            ))
                                        },
                                        Entity::Lab { .. } => {
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

                                //             if let Some(e) = game_state
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

                                let ret = game_state
                                    .simulation_state
                                    .factory
                                    .fluid_store
                                    .try_add_fluid_box(
                                        pos,
                                        data_store.fluid_tank_infos[usize::from(ty)].capacity,
                                        connecting_fluid_box_positions.map(|v| v.0),
                                        in_out_connections,
                                    );
                                match ret {
                                    Ok(_id) => {
                                        if let Err(e) = game_state.world.add_entity(
                                            Entity::FluidTank { pos, ty, rotation },
                                            &mut game_state.simulation_state,
                                            data_store,
                                        ) {
                                            warn!("Unable to place entity {:?}", e);
                                        }
                                    },
                                    Err(CannotMixFluidsError { items: [a, b] }) => {
                                        warn!(
                                            "Cannot connect systems containing {} and {}",
                                            data_store.item_display_names[a.into_usize()],
                                            data_store.item_display_names[b.into_usize()]
                                        )
                                    },
                                }
                            },
                            crate::frontend::world::tile::PlaceEntityType::MiningDrill {
                                ty,
                                pos,
                                rotation,
                            } => {
                                match game_state.world.add_mining_drill(
                                    pos,
                                    ty,
                                    rotation,
                                    &mut game_state.simulation_state,
                                    data_store,
                                ) {
                                    Ok(_) => {},
                                    Err(e) => error!(
                                        "Failed to place mining drill because of error {e:?}"
                                    ),
                                }
                            },
                        },
                    }
                },
                ActionType::Position(id, pos) => {
                    game_state.world.players[usize::from(*id)].visible = true;
                    game_state.world.players[usize::from(*id)].pos = *pos;
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
                    let Some(Entity::Assembler { .. }) =
                        game_state.world.get_entity_at(*assembler_pos, data_store)
                    else {
                        warn!("Tried to set recipe on non assembler");
                        continue;
                    };

                    game_state.world.change_assembler_recipe(
                        &mut game_state.simulation_state,
                        *assembler_pos,
                        *recipe,
                        data_store,
                    );
                },
                ActionType::Remove(pos) => {
                    game_state.world.remove_entity_at(
                        *pos,
                        &mut game_state.simulation_state,
                        data_store,
                    );
                },
                ActionType::AddModules {
                    pos,
                    modules: new_modules,
                } => {
                    game_state.world.mutate_entities_colliding_with(
                        *pos,
                        (1, 1),
                        data_store,
                        |e, dedup| {
                            match e {
                                Entity::Assembler { modules, info, .. } => {
                                    let num_free_module_slots = dedup[*modules as usize]
                                        .iter()
                                        .filter(|slot| slot.is_none())
                                        .count();

                                    if new_modules.len() > num_free_module_slots {
                                        // Not enough space in the module slots
                                        info!(
                                            "Tried to insert more modules than space is available"
                                        );
                                    } else {
                                        // We are okay!

                                        let mut modules_cloned = dedup[*modules as usize].clone();

                                        modules_cloned
                                            .iter_mut()
                                            .filter(|slot| slot.is_none())
                                            .zip(new_modules.iter().copied())
                                            .for_each(|(slot, new_module)| {
                                                assert!(slot.is_none());
                                                *slot = Some(new_module);
                                            });

                                        if let Some(idx) =
                                            dedup.iter().position(|slots| *slots == modules_cloned)
                                        {
                                            *modules = idx as u32;
                                        } else {
                                            dedup.push(modules_cloned);
                                            *modules = (dedup.len() - 1) as u32;
                                        }

                                        match info {
                                            AssemblerInfo::UnpoweredNoRecipe
                                            | AssemblerInfo::Unpowered(_)
                                            | AssemblerInfo::PoweredNoRecipe(_) => {},
                                            AssemblerInfo::Powered { id, .. } => {
                                                for module in new_modules {
                                                    game_state
                                                        .simulation_state
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
                                    modules,
                                    pole_position,
                                    ..
                                } => {
                                    let num_free_module_slots = dedup[*modules as usize]
                                        .iter()
                                        .filter(|slot| slot.is_none())
                                        .count();

                                    if new_modules.len() > num_free_module_slots {
                                        // Not enough space in the module slots
                                        info!(
                                            "Tried to insert more modules than space is available"
                                        );
                                    } else {
                                        // We are okay!

                                        let mut modules_cloned = dedup[*modules as usize].clone();

                                        modules_cloned
                                            .iter_mut()
                                            .filter(|slot| slot.is_none())
                                            .zip(new_modules.iter().copied())
                                            .for_each(|(slot, new_module)| {
                                                assert!(slot.is_none());
                                                *slot = Some(new_module);
                                            });

                                        if let Some(idx) =
                                            dedup.iter().position(|slots| *slots == modules_cloned)
                                        {
                                            *modules = idx as u32;
                                        } else {
                                            dedup.push(modules_cloned);
                                            *modules = (dedup.len() - 1) as u32;
                                        }

                                        match pole_position {
                                            None => {},
                                            Some((pole_pos, _weak_index, index)) => {
                                                for module in new_modules {
                                                    game_state
                                                        .simulation_state
                                                        .factory
                                                        .power_grids
                                                        .power_grids[usize::from(
                                                        game_state
                                                            .simulation_state
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
                        },
                    );
                },
                ActionType::RemoveModules { pos, indices } => {
                    game_state.world.mutate_entities_colliding_with(
                        *pos,
                        (1, 1),
                        data_store,
                        |e, dedup| {
                            match e {
                                Entity::Assembler { modules, info, .. } => {
                                    let num_used_module_slots = dedup[*modules as usize]
                                        .iter()
                                        .filter(|slot| slot.is_some())
                                        .count();

                                    if indices.len() > num_used_module_slots {
                                        // Not enough space in the module slots
                                        warn!("Tried to remove more modules than exist in machine");
                                    } else {
                                        // We are okay!

                                        let mut modules_clone = dedup[*modules as usize].clone();

                                        assert!(indices.iter().all_unique());

                                        assert!(indices.iter().all(|v| *v < modules_clone.len()));

                                        let modules_to_remove = modules_clone
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
                                            | AssemblerInfo::PoweredNoRecipe(_) => {
                                                // Make sure to consume the iterator
                                                let _ = modules_to_remove.count();
                                            },
                                            AssemblerInfo::Powered { id, .. } => {
                                                for removed_module in modules_to_remove {
                                                    game_state
                                                        .simulation_state
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

                                        if let Some(idx) =
                                            dedup.iter().position(|slots| *slots == modules_clone)
                                        {
                                            *modules = idx as u32;
                                        } else {
                                            dedup.push(modules_clone);
                                            *modules = (dedup.len() - 1) as u32;
                                        }
                                    }
                                },
                                _ => {
                                    warn!("Tried to insert modules into non assembler");
                                },
                            }
                            ControlFlow::Break(())
                        },
                    );
                },

                ActionType::PlaceOre { pos, ore, amount } => {
                    game_state.world.ore_lookup.add_ore(*pos, *ore, *amount);
                },
            }

            #[cfg(debug_assertions)]
            {
                let affected_grids_and_potential_match = game_state
                    .simulation_state
                    .factory
                    .power_grids
                    .power_grids
                    .iter()
                    .filter(|grid| !grid.is_placeholder)
                    .all(|pg| {
                        pg.beacon_affected_entities
                            .keys()
                            .map(|e| e.get_power_grid())
                            .all(|affected_grid| {
                                pg.potential_beacon_affected_powergrids
                                    .contains(&affected_grid)
                            })
                    });
                if !affected_grids_and_potential_match {
                    dbg!(action.borrow());
                }
                assert!(affected_grids_and_potential_match);
            }
        }

        #[cfg(debug_assertions)]
        {
            assert!(
                game_state
                    .world
                    .get_chunks()
                    .into_iter()
                    .flat_map(|chunk| chunk.get_entities())
                    .all(|e| match e {
                        Entity::Assembler { info, .. } => {
                            match info {
                                AssemblerInfo::UnpoweredNoRecipe => true,
                                AssemblerInfo::Unpowered(_) => true,
                                AssemblerInfo::PoweredNoRecipe(_) => true,
                                AssemblerInfo::Powered { id, .. } => {
                                    !game_state.simulation_state.factory.power_grids.power_grids
                                        [usize::from(id.grid)]
                                    .is_placeholder
                                },
                            }
                        },
                        Entity::Beacon {
                            pole_position: Some((pole_pos, _)),
                            ..
                        } => {
                            game_state
                                .simulation_state
                                .factory
                                .power_grids
                                .pole_pos_to_grid_id
                                .get(pole_pos)
                                .is_some()
                        },
                        _ => true,
                    })
            );
        }
    }

    #[profiling::function]
    pub fn update(
        simulation_state: &mut SimulationState<ItemIdxType, RecipeIdxType>,
        aux_data: &mut AuxillaryData,
        data_store: &DataStore<ItemIdxType, RecipeIdxType>,
    ) {
        let start_updating = Instant::now();
        aux_data.current_tick += 1;
        simulation_state
            .factory
            // We can downcast here, since this could only cause graphical weirdness for a couple frame every ~2 years of playtime
            .belt_and_inserter_update(aux_data.current_tick as u32, data_store);

        let ((), mining_production_by_item, (tech_progress, recipe_tick_info, lab_info)) = join!(
            || simulation_state.factory.chests.update(data_store),
            || {
                simulation_state.factory.ore_store.update(
                    &simulation_state.tech_state.mining_productivity_by_item,
                    data_store,
                )
            },
            || {
                simulation_state.factory.power_grids.update(
                    &simulation_state.tech_state,
                    aux_data.current_tick as u32,
                    data_store,
                )
            }
        );

        aux_data.statistics.append_single_set_of_samples((
            ProductionInfo::from_recipe_info_and_per_item(
                &recipe_tick_info,
                mining_production_by_item,
                data_store,
            ),
            ConsumptionInfo::from_infos(&recipe_tick_info, &lab_info, data_store),
            tech_progress,
        ));

        simulation_state
            .tech_state
            .apply_progress(tech_progress, data_store);

        let done_updating = Instant::now();

        if let Some(last_update_time) = aux_data.last_update_time {
            aux_data
                .update_round_trip_times
                .append_single_set_of_samples(UpdateTime {
                    dur: done_updating - last_update_time,
                });
        }
        aux_data
            .update_times
            .append_single_set_of_samples(UpdateTime {
                dur: done_updating - start_updating,
            });
        aux_data.last_update_time = Some(done_updating);
    }

    fn update_inserters(
        world: &mut World<ItemIdxType, RecipeIdxType>,
        mut simulation_state: &mut SimulationState<ItemIdxType, RecipeIdxType>,
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
                world.mutate_entities_colliding_with(
                    Position {
                        x: assembler_pos.x - i32::from(inserter_range),
                        y: assembler_pos.y - i32::from(inserter_range),
                    },
                    (
                        (2 * inserter_range + size.0).into(),
                        (2 * inserter_range + size.1).into(),
                    ),
                    data_store,
                    |e, _| {
                        match e {
                            Entity::Inserter {
                                pos,
                                ty,
                                direction,
                                info,
                                ..
                            } => match info {
                                InserterInfo::NotAttached {} => {
                                    let start_pos =
                                        data_store.inserter_start_pos(*ty, *pos, *direction);
                                    let end_pos =
                                        data_store.inserter_end_pos(*ty, *pos, *direction);

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
                world.mutate_entities_colliding_with(
                    Position {
                        x: belt_pos.x - i32::from(inserter_range),
                        y: belt_pos.y - i32::from(inserter_range),
                    },
                    (
                        (2 * inserter_range + 1).into(),
                        (2 * inserter_range + 1).into(),
                    ),
                    data_store,
                    |e, _| {
                        match e {
                            Entity::Inserter {
                                pos,
                                direction,
                                ty,
                                info,
                                ..
                            } => match info {
                                InserterInfo::NotAttached {} => {
                                    let start_pos =
                                        data_store.inserter_start_pos(*ty, *pos, *direction);
                                    let end_pos =
                                        data_store.inserter_end_pos(*ty, *pos, *direction);
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
                world.mutate_entities_colliding_with(
                    Position {
                        x: assembler_pos.x - i32::from(inserter_range),
                        y: assembler_pos.y - i32::from(inserter_range),
                    },
                    (
                        (2 * inserter_range + size.0).into(),
                        (2 * inserter_range + size.1).into(),
                    ),
                    data_store,
                    |e, _| {
                        match e {
                            Entity::Inserter {
                                pos,
                                direction,
                                ty,
                                info,
                                ..
                            } => match info {
                                InserterInfo::NotAttached {} => {
                                    let start_pos =
                                        data_store.inserter_start_pos(*ty, *pos, *direction);
                                    let end_pos =
                                        data_store.inserter_end_pos(*ty, *pos, *direction);

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
                                    info: AttachedInserter::BeltBelt { .. },
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
            let _ = world.try_instantiate_inserter_entity(&mut simulation_state, pos, data_store);
        }
    }

    fn add_assembler_to_sim(
        sim_state: &mut SimulationState<ItemIdxType, RecipeIdxType>,
        ty: u8,
        recipe: Recipe<RecipeIdxType>,
        modules: &[Option<ModuleTy>],
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
        world: &mut World<ItemIdxType, RecipeIdxType>,
        sim_state: &mut SimulationState<ItemIdxType, RecipeIdxType>,
        ty: u8,
        user_movetime: Option<NonZero<u16>>,
        pos: Position,
        dir: Dir,
        filter: Option<Item<ItemIdxType>>,
        data_store: &DataStore<ItemIdxType, RecipeIdxType>,
    ) -> Result<(), ()> {
        if !world.can_fit(pos, (1, 1), data_store) {
            warn!("Tried to place inserter where it does not fit");
            return Err(());
        }

        let (start_pos, end_pos) = calculate_inserter_positions(ty, pos, dir, data_store);

        if let Err(e) = world.add_entity(
            Entity::Inserter {
                ty,
                user_movetime,

                pos,
                direction: dir,
                filter,
                info: InserterInfo::NotAttached {},
            },
            sim_state,
            data_store,
        ) {
            warn!("Unable to place entity {:?}", e);
        }

        Ok(())
    }
}

// TODO: Different types of inserters
pub fn calculate_inserter_positions<ItemIdxType: IdxTrait, RecipeIdxType: IdxTrait>(
    ty: u8,
    pos: Position,
    dir: Dir,
    data_store: &DataStore<ItemIdxType, RecipeIdxType>,
) -> (Position, Position) {
    let input_offs = data_store.inserter_infos[ty as usize].input_offset;
    let output_offs = data_store.inserter_infos[ty as usize].output_offset;

    let input_offs = match dir {
        Dir::North => input_offs,
        Dir::South => [-input_offs[0], -input_offs[1]],
        Dir::East => [-input_offs[1], input_offs[0]],
        Dir::West => [input_offs[1], -input_offs[0]],
    };

    let output_offs = match dir {
        Dir::North => output_offs,
        Dir::South => [-output_offs[0], -output_offs[1]],
        Dir::East => [-output_offs[1], output_offs[0]],
        Dir::West => [output_offs[1], -output_offs[0]],
    };

    let start_pos = Position {
        x: pos.x.checked_add(input_offs[0].into()).unwrap(),
        y: pos.y.checked_add(input_offs[1].into()).unwrap(),
    };
    let end_pos = Position {
        x: pos.x.checked_add(output_offs[0].into()).unwrap(),
        y: pos.y.checked_add(output_offs[1].into()).unwrap(),
    };

    (start_pos, end_pos)
}

#[cfg(test)]
mod tests {
    use std::fs::File;

    use crate::blueprint::BlueprintAction;

    use crate::{
        DATA_STORE,
        app_state::GameState,
        blueprint::{
            Blueprint,
            test::{random_blueprint_strategy, random_position},
        },
        frontend::world::tile::Dir,
        frontend::{
            action::{
                ActionType,
                place_entity::{EntityPlaceOptions, PlaceEntityInfo},
                set_recipe::SetRecipeInfo,
            },
            world::{
                Position,
                tile::{AssemblerInfo, Entity, PlaceEntityType},
            },
        },
        item::Recipe,
        power::{Watt, power_grid::MAX_POWER_MULT},
        replays::Replay,
    };
    use proptest::{
        prelude::{Just, Strategy},
        prop_assert, prop_assert_eq, prop_assume, proptest,
    };

    use test::Bencher;

    fn beacon_test_val() -> impl Strategy<Value = Vec<ActionType<u8, u8>>> {
        Just(vec![
            ActionType::PlaceEntity(PlaceEntityInfo {
                force: false,
                entities: EntityPlaceOptions::Single(PlaceEntityType::Assembler {
                    pos: Position { x: 1600, y: 1600 },
                    ty: 2,
                    rotation: Dir::North,
                }),
            }),
            ActionType::SetRecipe(SetRecipeInfo {
                pos: Position { x: 1600, y: 1600 },
                recipe: Recipe { id: 0 },
            }),
            ActionType::PlaceEntity(PlaceEntityInfo {
                force: false,
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
                force: false,
                entities: EntityPlaceOptions::Single(PlaceEntityType::PowerPole {
                    pos: Position { x: 1603, y: 1599 },
                    ty: 0,
                }),
            }),
            ActionType::PlaceEntity(PlaceEntityInfo {
                force: false,
                entities: EntityPlaceOptions::Single(PlaceEntityType::PowerPole {
                    pos: Position { x: 1608, y: 1599 },
                    ty: 0,
                }),
            }),
            ActionType::PlaceEntity(PlaceEntityInfo {
                force: false,
                entities: EntityPlaceOptions::Single(PlaceEntityType::PowerPole {
                    pos: Position { x: 1613, y: 1599 },
                    ty: 0,
                }),
            }),
            ActionType::PlaceEntity(PlaceEntityInfo {
                force: false,
                entities: EntityPlaceOptions::Single(PlaceEntityType::SolarPanel {
                    pos: Position { x: 1600, y: 1597 },
                    ty: 0,
                }),
            }),
        ])
    }

    fn full_beacon() -> impl Strategy<Value = Vec<BlueprintAction>> {
        Just(ron::de::from_reader(File::open("test_blueprints/full_beacons.bp").unwrap()).unwrap())
            .prop_map(|bp: Blueprint| bp.actions)
    }

    proptest! {
        #[test]
        fn test_random_blueprint_does_not_crash(base_pos in random_position(), blueprint in random_blueprint_strategy::<u8, u8>(0..1_000, &DATA_STORE)) {

            let mut game_state = GameState::new(&DATA_STORE);

            blueprint.apply(false, base_pos, &mut game_state, &DATA_STORE);

        }

        #[test]
        fn test_random_blueprint_does_not_crash_after(base_pos in random_position(), blueprint in random_blueprint_strategy::<u8, u8>(0..100, &DATA_STORE), time in 0usize..10) {

            let mut game_state = GameState::new(&DATA_STORE);

            blueprint.apply(false, base_pos, &mut game_state, &DATA_STORE);

            for _ in 0usize..time {
                GameState::update(
                    &mut *game_state.simulation_state.lock(),
                    &mut *game_state.aux_data.lock(),
                    &DATA_STORE,
                );
            }
        }

        #[test]
        fn test_beacons_always_effect(actions in beacon_test_val().prop_shuffle()) {
            prop_assume!(actions.iter().position(|a| matches!(a, ActionType::PlaceEntity(PlaceEntityInfo {
                force: false,
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

            Blueprint { actions: actions.into_iter().map(|a| BlueprintAction::from_with_datastore(&a, &*DATA_STORE)).collect() }.apply(false, Position { x: 0, y: 0 }, &mut game_state, &DATA_STORE);

            for _ in 0usize..10 {
                GameState::update(
                    &mut *game_state.simulation_state.lock(),
                    &mut *game_state.aux_data.lock(),
                    &DATA_STORE,
                );
            }

            let world = game_state.world.lock();
            let Some(Entity::Assembler { info: AssemblerInfo::Powered { id, .. }, .. }) = world.get_entity_at(Position { x: 1600, y: 1600 }, &DATA_STORE) else {
                unreachable!("{:?}", game_state.world.lock().get_entity_at(Position { x: 1600, y: 1600 }, &DATA_STORE));
            };
            let id = *id;
            std::mem::drop(world);

            prop_assume!(game_state.simulation_state.lock().factory.power_grids.power_grids[usize::from(id.grid)].last_power_mult == MAX_POWER_MULT);

            let info = game_state.simulation_state.lock().factory.power_grids.power_grids[usize::from(id.grid)].get_assembler_info(id, &DATA_STORE);

            prop_assert!((info.power_consumption_mod - 0.7).abs() < 1.0e-6, "power_consumption_mod: {:?}", info.power_consumption_mod);
            prop_assert!((info.base_speed - 1.25).abs() < 1.0e-6, "base_speed: {:?}", info.base_speed);
            prop_assert!((info.prod_mod - 0.0).abs() < 1.0e-6, "prod_mod: {:?}", info.prod_mod);
            prop_assert!((info.speed_mod - (0.5)).abs() < 1.0e-6, "speed_mod: {:?}", info.speed_mod);
            prop_assert_eq!(info.base_power_consumption, Watt(375_000), "base_power_consumption: {:?}", info.base_power_consumption);

        }

        // #[test]
        // fn test_full_beacons_always_effect(actions in full_beacon().prop_shuffle()) {
        //     prop_assume!(actions.iter().position(|a| matches!(a.try_into_real_action(false, &DATA_STORE).unwrap(), ActionType::PlaceEntity(PlaceEntityInfo {
        //         force: false,
        //         entities: EntityPlaceOptions::Single(PlaceEntityType::Assembler {
        //             ..
        //         }),
        //     }))) < actions.iter().position(|a| matches!(a.try_into_real_action(false, &DATA_STORE).unwrap(), ActionType::SetRecipe(_))));
        //     prop_assume!(actions.iter().position(|a| matches!(a.try_into_real_action(false, &DATA_STORE).unwrap(), ActionType::PlaceEntity(PlaceEntityInfo {
        //         force: false,
        //         entities: EntityPlaceOptions::Single(PlaceEntityType::Assembler {
        //             ..
        //         }),
        //     }))) < actions.iter().position(|a| matches!(a.try_into_real_action(false, &DATA_STORE).unwrap(), ActionType::AddModules { pos: Position { x: 24, y: 21 }, ..})));

        //     let mut game_state = GameState::new(&DATA_STORE);

        //     Blueprint { actions }.apply(false, Position { x: 1600, y: 1600 }, &mut game_state, &DATA_STORE);

        //     for _ in 0usize..10 {
        //         GameState::update(
        //             &mut *game_state.simulation_state.lock(),
        //             &mut *game_state.aux_data.lock(),
        //             &DATA_STORE,
        //         );
        //     }

        //     let world = game_state.world.lock();
        //     let Some(Entity::Assembler { info: AssemblerInfo::Powered { id, .. }, .. }) = world.get_entity_at(Position { x: 1624, y: 1621 }, &DATA_STORE) else {
        //         unreachable!("{:?}", game_state.world.lock().get_entity_at(Position { x: 1624, y: 1621 }, &DATA_STORE));
        //     };
        //     let id = *id;
        //     std::mem::drop(world);

        //     prop_assume!(game_state.simulation_state.lock().factory.power_grids.power_grids[usize::from(id.grid)].last_power_mult == MAX_POWER_MULT);

        //     let info = game_state.simulation_state.lock().factory.power_grids.power_grids[usize::from(id.grid)].get_assembler_info(id, &DATA_STORE);

        //     prop_assert_eq!(info.base_power_consumption, Watt(375_000), "base_power_consumption: {:?}", info.base_power_consumption);
        //     prop_assert!((info.base_speed - 1.25).abs() < 1.0e-6, "base_speed: {:?}", info.base_speed);
        //     prop_assert!((info.prod_mod - 0.4).abs() < 1.0e-6, "prod_mod: {:?}", info.prod_mod);
        //     prop_assert!((info.speed_mod - (5.4)).abs() < 1.0e-6, "speed_mod: {:?}", info.speed_mod);
        //     prop_assert!((info.power_consumption_mod - 11.60).abs() < 1.0e-6, "power_consumption_mod: {:?}", info.power_consumption_mod);

        // }
    }

    #[bench]
    fn bench_single_inserter(b: &mut Bencher) {
        let mut game_state = GameState::new(&DATA_STORE);

        let mut rep = Replay::new(&game_state, None, (*DATA_STORE).clone());

        rep.append_actions(vec![ActionType::PlaceEntity(PlaceEntityInfo {
            force: false,
            entities: crate::frontend::action::place_entity::EntityPlaceOptions::Single(
                crate::frontend::world::tile::PlaceEntityType::PowerPole {
                    pos: Position { x: 0, y: 5 },
                    ty: 0,
                },
            ),
        })]);

        rep.append_actions(vec![ActionType::PlaceEntity(PlaceEntityInfo {
            force: false,
            entities: crate::frontend::action::place_entity::EntityPlaceOptions::Single(
                crate::frontend::world::tile::PlaceEntityType::SolarPanel {
                    pos: Position { x: 0, y: 2 },
                    ty: 0,
                },
            ),
        })]);

        rep.append_actions(vec![ActionType::PlaceEntity(PlaceEntityInfo {
            force: false,
            entities: crate::frontend::action::place_entity::EntityPlaceOptions::Single(
                crate::frontend::world::tile::PlaceEntityType::Assembler {
                    pos: Position { x: 0, y: 6 },
                    ty: 0,
                    rotation: Dir::North,
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
            force: false,
            entities: crate::frontend::action::place_entity::EntityPlaceOptions::Single(
                crate::frontend::world::tile::PlaceEntityType::Belt {
                    pos: Position { x: 1, y: 4 },
                    direction: crate::frontend::world::tile::Dir::East,
                    ty: 0,
                },
            ),
        })]);

        rep.append_actions(vec![ActionType::PlaceEntity(PlaceEntityInfo {
            force: false,
            entities: crate::frontend::action::place_entity::EntityPlaceOptions::Single(
                crate::frontend::world::tile::PlaceEntityType::Belt {
                    pos: Position { x: 2, y: 4 },
                    direction: crate::frontend::world::tile::Dir::East,
                    ty: 0,
                },
            ),
        })]);

        rep.append_actions(vec![ActionType::PlaceEntity(PlaceEntityInfo {
            force: false,
            entities: crate::frontend::action::place_entity::EntityPlaceOptions::Single(
                crate::frontend::world::tile::PlaceEntityType::Inserter {
                    ty: 0,
                    pos: Position { x: 1, y: 5 },
                    dir: crate::frontend::world::tile::Dir::North,
                    filter: None,
                    user_movetime: None,
                },
            ),
        })]);

        let blueprint = Blueprint::from_replay(&rep);

        blueprint.apply(
            false,
            Position { x: 1600, y: 1600 },
            &mut game_state,
            &DATA_STORE,
        );

        dbg!(
            &game_state
                .world
                .lock()
                .get_chunk_for_tile(Position { x: 1600, y: 1600 })
        );

        dbg!(game_state.aux_data.lock().current_tick);

        for _ in 0..600 {
            GameState::update(
                &mut *game_state.simulation_state.lock(),
                &mut *game_state.aux_data.lock(),
                &DATA_STORE,
            );
        }

        b.iter(|| {
            GameState::update(
                &mut *game_state.simulation_state.lock(),
                &mut *game_state.aux_data.lock(),
                &DATA_STORE,
            );
        });

        dbg!(game_state.aux_data.lock().current_tick);

        assert!(
            game_state
                .aux_data
                .lock()
                .statistics
                .production
                .total
                .as_ref()
                .unwrap()
                .items_produced[0]
                > 0
        );
    }
}
