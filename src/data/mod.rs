use std::{
    array,
    cmp::max,
    collections::{BTreeSet, HashMap},
    iter,
};

use eframe::egui::Color32;
use itertools::Itertools;
use log::warn;
use rand::random;
use sha2::{Digest, Sha256};
use strum::IntoEnumIterator;

pub mod factorio_1_1;

use crate::{
    assembler::TIMERTYPE,
    frontend::world::tile::Dir,
    inserter::StaticID,
    item::{IdxTrait, Item, Recipe, WeakIdxTrait, ITEMCOUNTTYPE},
    power::{Joule, Watt},
};

type ItemString = String;
type AssemblingMachineString = String;
type InserterString = String;
type EngineString = String;

#[derive(Debug, Clone, serde::Deserialize, serde::Serialize)]
pub struct RawRecipeData {
    /// The fully qualified name of the recipe
    name: String,
    display_name: String,
    possible_machines: Box<[AssemblingMachineString]>,
    ings: Box<[RawItemStack]>,
    output: Box<[RawItemStack]>,
    time_to_craft: TIMERTYPE,
    is_intermediate: bool,
}

#[derive(Debug, Clone, serde::Deserialize, serde::Serialize)]
struct RawItemStack {
    item: ItemString,
    amount: ITEMCOUNTTYPE,
}

#[derive(Debug, Clone, serde::Deserialize, serde::Serialize)]
struct RawChest {
    name: String,
    display_name: String,
    tile_size: (u8, u8),
    number_of_slots: u8,
}

#[derive(Debug, Clone, serde::Deserialize, serde::Serialize)]
struct RawAssemblingMachine {
    name: String,
    display_name: String,
    tile_size: (u8, u8),
    working_power_draw: Watt,
    fluid_connection_offsets: Vec<RawFluidConnection>,
    fluid_connection_flowthrough: Vec<RawFluidFlowthrough>,

    num_module_slots: u8,

    /// Base bonus productivity in %
    base_bonus_prod: u8,
    /// Speed multiplier compared to "baseline" in 5%
    base_speed: u8,
}

#[derive(Debug, Clone, serde::Deserialize, serde::Serialize)]
struct RawLab {
    name: String,
    display_name: String,
    tile_size: (u8, u8),
    working_power_draw: Watt,
    fluid_connection_offsets: Vec<RawFluidConnection>,
    fluid_connection_flowthrough: Vec<RawFluidFlowthrough>,

    num_module_slots: u8,

    /// Base bonus productivity in %
    base_bonus_prod: u8,
    /// Speed multiplier compared to "baseline" in 5%
    base_speed: u8,
}

#[derive(Debug, Clone, serde::Deserialize, serde::Serialize)]
struct RawBeacon {
    name: String,
    display_name: String,
    tile_size: (u8, u8),
    working_power_draw: Watt,

    num_module_slots: u8,
    /// Numerator and denominator
    effectiveness: (u8, u8),
    effect_size: (u8, u8),
}

#[derive(Debug, Clone, serde::Deserialize, serde::Serialize)]
struct RawSolarPanel {
    name: String,
    display_name: String,
    tile_size: (u8, u8),

    // TODO:
    output: Watt,
}

#[derive(Debug, Clone, serde::Deserialize, serde::Serialize)]
struct RawAccumulator {
    name: String,
    display_name: String,
    tile_size: (u8, u8),

    charge: Joule,
    max_charge_rate: Watt,
    max_discharge_rate: Watt,
}

#[derive(Debug, Clone, Copy, PartialEq, serde::Deserialize, serde::Serialize)]
pub enum AllowedFluidDirection {
    Single(ItemRecipeDir),
    Both { preferred: ItemRecipeDir },
}

#[derive(Debug, Clone, serde::Deserialize, serde::Serialize)]
struct RawFluidConnection {
    allowed_fluid_directions: AllowedFluidDirection,
    offset: (u8, u8),
    pipe_connection_direction: Dir,
    connection_type: RawPipeConnectionType,
}

#[derive(Debug, Clone, serde::Deserialize, serde::Serialize)]
pub enum RawPipeConnectionType {
    Direct,
    Underground {
        max_distance: u8,
        underground_allowed_kinds: Vec<String>,
    },
}

#[derive(Debug, Clone, serde::Deserialize, serde::Serialize)]
struct RawFluidFlowthrough {
    fluid_dir: AllowedFluidDirection,
    connections: Vec<((u8, u8), Dir, RawPipeConnectionType)>,
}

#[derive(Debug, Clone, serde::Deserialize, serde::Serialize)]
struct RawInserter {
    name: String,
    display_name: String,
    time_per_trip: TIMERTYPE,
    handsize: ITEMCOUNTTYPE,
    pickup_offs: (i8, i8),
    dropoff_offs: (i8, i8),
}

#[derive(Debug, Clone, serde::Deserialize, serde::Serialize)]
pub struct RawDataStore {
    recipes: Vec<RawRecipeData>,
    items: Vec<RawItem>,
    machines: Vec<RawAssemblingMachine>,
    labs: Vec<RawLab>,
    beacons: Vec<RawBeacon>,
    miners: Vec<RawMiner>,
    power_poles: Vec<RawPowerPole>,
    modules: Vec<RawModule>,
    chests: Vec<RawChest>,
    technologies: Vec<RawTechnology>,
    solar_panels: Vec<RawSolarPanel>,
    accumulators: Vec<RawAccumulator>,
    fluid_tanks: Vec<RawFluidTank>,
}

#[derive(Debug, Clone, serde::Deserialize, serde::Serialize)]
struct RawFluidTank {
    name: String,
    display_name: String,
    capacity: u32,
    tile_size: (u8, u8),
    connections: Vec<((u8, u8), Dir, RawPipeConnectionType)>,
}

#[derive(Debug, Clone, serde::Deserialize, serde::Serialize)]
struct RawTechnology {
    name: String,
    display_name: String,
    cost_of_single_research_unit: Vec<RawItemStack>,
    num_units: u64,
    precursors: Vec<String>,
}

#[derive(Debug, Clone, serde::Deserialize, serde::Serialize)]
struct RawModule {
    name: String,
    display_name: String,
    item: String,
    // TODO: Document the units of these modifiers
    // TODO: Maybe in percent?
    productivity_effect: i8,
    speed_effect: i8,
    power_effect: i8,
    allowed_in: AllowedIn,
}

#[derive(Debug, Clone, serde::Deserialize, serde::Serialize)]
enum AllowedIn {
    AllIncludingBeacons,
    AllNoBeacons,
    OnlyIntermediate,
}

#[derive(Debug, Clone, serde::Deserialize, serde::Serialize)]
struct RawPowerPole {
    name: String,
    display_name: String,
    tile_size: (u8, u8),
    power_range: u8,
    connection_range: u8,
}

#[derive(Debug, Clone, serde::Deserialize, serde::Serialize)]
struct RawMiner {
    name: String,
    display_name: String,
    timer: TIMERTYPE,
}

#[derive(Debug, Clone, serde::Deserialize, serde::Serialize)]
struct RawItem {
    name: String,
    display_name: String,
    stack_size: ITEMCOUNTTYPE,
    placed_as: Option<RawEntity>,
    burnable_in: Box<[EngineString]>,
    science_data: Option<()>,
    is_fluid: bool,
}

#[derive(Debug, Clone, serde::Deserialize, serde::Serialize)]
enum RawEntity {
    AssemblingMachine(AssemblingMachineString),
    Inserter(InserterString),
    Belt(()),
}

#[derive(Debug, Clone, serde::Serialize, serde:: Deserialize)]
pub struct AssemblerInfo {
    pub display_name: String,

    pub size: (u16, u16),
    pub num_module_slots: u8,

    pub fluid_connections: Vec<(FluidConnection, AllowedFluidDirection)>,

    /// Base Speed. All module modifiers apply multiplicatively
    pub base_speed: u8,
    pub base_prod: u8,
    pub base_power_consumption: Watt,
}

#[derive(Debug, Clone, serde::Serialize, serde:: Deserialize)]
pub struct LabInfo {
    pub display_name: String,

    pub size: (u16, u16),
    pub num_module_slots: u8,

    /// Base Speed as a numerator and divisor. All module modifiers apply multiplicatively
    pub base_speed: u8,
    pub base_prod: u8,
    pub base_power_consumption: Watt,
}

#[derive(Debug, Clone, serde::Serialize, serde:: Deserialize)]
pub struct BeaconInfo {
    pub display_name: String,

    pub size: (u16, u16),
    pub num_module_slots: u8,
    pub effectiveness: (u8, u8),
    // TODO: Currently mining range must be centered on the mining drill
    pub effect_range: (u16, u16),
    pub power_consumption: Watt,
}

#[derive(Debug, Clone, serde::Serialize, serde:: Deserialize)]
pub struct ModuleInfo {
    pub name: String,

    pub speed_mod: i8,
    pub prod_mod: i8,
    pub power_mod: i8,
}

#[derive(Debug, Clone, serde::Serialize, serde:: Deserialize)]
pub struct SolarPanelInfo {
    pub name: String,
    pub size: [u16; 2],

    pub power_output: SolarPanelOutputFunction,
}

#[derive(Debug, Clone, serde::Serialize, serde:: Deserialize)]
pub enum SolarPanelOutputFunction {
    Constant(Watt),
    Lookup(Vec<Watt>),
}

#[derive(Debug, Clone, serde::Serialize, serde:: Deserialize)]
pub struct AccumulatorInfo {
    pub name: String,
    pub size: [u16; 2],

    pub max_charge: Joule,
    pub max_charge_rate: Watt,
    pub max_discharge_rate: Watt,
}

type UndergroundGroupMask = u64;

#[derive(Debug, Clone, Copy, serde::Deserialize, serde::Serialize)]
pub enum PipeConnectionType {
    Direct,
    Underground {
        max_distance: u8,
        underground_group_mask: UndergroundGroupMask,
    },
}

#[derive(Debug, Clone, serde::Serialize, serde:: Deserialize)]
pub struct DataStore<ItemIdxType: WeakIdxTrait, RecipeIdxType: WeakIdxTrait> {
    pub checksum: String,

    pub max_entity_size: (u16, u16),

    pub assembler_info: Vec<AssemblerInfo>,
    pub lab_info: Vec<LabInfo>,
    pub beacon_info: Vec<BeaconInfo>,
    pub max_beacon_range: (u16, u16),
    pub module_info: Vec<ModuleInfo>,
    pub solar_panel_info: Vec<SolarPanelInfo>,
    pub accumulator_info: Vec<AccumulatorInfo>,

    /// In 5% steps
    pub min_power_mod: u8,

    pub recipe_names: Vec<String>,
    pub recipe_allowed_assembling_machines: Vec<Vec<u8>>,

    pub recipe_is_intermediate: Vec<bool>,

    pub recipe_num_ing_lookup: Vec<usize>,
    pub recipe_num_out_lookup: Vec<usize>,
    pub recipe_to_ing_out_combo_idx: Vec<usize>,
    pub ing_out_num_to_recipe: HashMap<(usize, usize), Vec<Recipe<RecipeIdxType>>>,

    pub recipe_item_store_to_item: Vec<Item<ItemIdxType>>,

    recipe_item_to_storage_list_idx:
        HashMap<(Recipe<RecipeIdxType>, Item<ItemIdxType>, ItemRecipeDir), u16>,

    pub recipe_to_items_and_amounts:
        HashMap<Recipe<RecipeIdxType>, Vec<(ItemRecipeDir, Item<ItemIdxType>, ITEMCOUNTTYPE)>>,

    /// A lookup from recipe to its ing and out idxs
    pub recipe_index_lookups: Vec<(usize, usize)>,
    /// A lookup from recipe_ing_idx to its ingredient counts
    pub recipe_ings: RecipeIngLookups,
    /// A lookup from recipe_out_idx to its output counts
    pub recipe_outputs: RecipeOutputLookups,

    pub recipe_timers: Box<[TIMERTYPE]>,

    pub recipe_to_items: HashMap<Recipe<RecipeIdxType>, Vec<(ItemRecipeDir, Item<ItemIdxType>)>>,

    pub science_bottle_items: Vec<Item<ItemIdxType>>,

    pub item_to_recipe_count_where_its_ingredient: Vec<Vec<(Recipe<RecipeIdxType>, ITEMCOUNTTYPE)>>,
    pub item_to_recipe_where_its_output: Vec<Vec<(Recipe<RecipeIdxType>, ITEMCOUNTTYPE)>>,

    pub lazy_power_machine_infos: Vec<LazyPowerMachineInfo<ItemIdxType>>,

    pub item_names: Vec<String>,

    pub item_is_fluid: Vec<bool>,

    pub power_pole_data: Vec<PowerPoleData>,

    pub max_power_search_range: u8,

    pub max_inserter_search_range: u8,

    pub num_different_static_containers: usize,

    /// use Item to index, gives how many recipes have this item as an ingredient
    pub num_recipes_with_item: Vec<usize>,

    pub item_is_science: Vec<bool>,

    pub item_stack_sizes: Vec<ITEMCOUNTTYPE>,
    pub chest_num_slots: Vec<u8>,
    pub chest_tile_sizes: Vec<(u16, u16)>,

    pub fluid_tank_infos: Vec<FluidTankData>,

    pub recipe_to_translated_index:
        HashMap<(Recipe<RecipeIdxType>, Item<ItemIdxType>), RecipeIdxType>,

    pub item_to_colour: Vec<Color32>,

    pub technology_costs: Vec<(u64, Box<[ITEMCOUNTTYPE]>)>,
    pub belt_infos: Vec<BeltInfo>,
    pub mining_drill_info: Vec<MiningDrillInfo>,
}

#[derive(Debug, Clone, serde::Serialize, serde:: Deserialize)]
pub struct MiningDrillInfo {
    pub name: String,
    pub size: [u16; 2],
    // TODO: Currently mining range must be centered on the mining drill
    pub mining_range: [u16; 2],
    pub base_speed: u16,
    // Fraction
    pub resource_drain: (u8, u8),
    // TODO: Allowed ore types
}

#[derive(Debug, Clone, serde::Serialize, serde:: Deserialize)]
pub struct BeltInfo {
    pub name: String,
    pub has_underground: bool,
    pub has_splitter: Option<BeltSplitterInfo>,
    /// Setting how often this kind of belt moves
    pub timer_increase: u8,
}

#[derive(Debug, Clone, serde::Serialize, serde:: Deserialize)]
struct BeltSplitterInfo {
    can_filter_item: bool,
    can_prioritise_side: bool,
}

#[derive(Debug, Clone, serde::Serialize, serde:: Deserialize)]
pub struct LazyPowerMachineInfo<ItemIdxType: WeakIdxTrait> {
    pub ingredient: Item<ItemIdxType>,
    pub power_per_item: Joule,
    pub max_power_per_tick: Joule,
}

#[derive(Debug, Clone, serde::Serialize, serde:: Deserialize)]
pub struct FluidTankData {
    pub size: [u16; 2],
    /// Capacity in fluid units
    pub capacity: u32,

    pub fluid_connections: Vec<FluidConnection>,
    pub max_search_range: u16,
}

/// These offset and directions are based on the entity facing north
#[derive(Debug, Clone, Copy, serde::Serialize, serde:: Deserialize)]
pub struct FluidConnection {
    pub offset: [u16; 2],
    pub dir: Dir,
    pub kind: PipeConnectionType,
}

#[derive(Debug, Clone, serde::Serialize, serde:: Deserialize)]
pub struct PowerPoleData {
    pub size: (u16, u16),
    pub power_range: u8,
    pub connection_range: u8,
}

#[derive(Debug, PartialEq, Eq, Hash, Clone, Copy, serde::Deserialize, serde::Serialize)]
pub enum ItemRecipeDir {
    Ing,
    Out,
}

#[derive(Debug)]
pub enum DataStoreOptions {
    ItemU8RecipeU8(DataStore<u8, u8>),
    ItemU8RecipeU16(DataStore<u8, u16>),
    ItemU16RecipeU8(DataStore<u16, u8>),
    ItemU16RecipeU16(DataStore<u16, u16>),
}

impl DataStoreOptions {
    pub fn assume_simple(self) -> DataStore<u8, u8> {
        match self {
            DataStoreOptions::ItemU8RecipeU8(data_store) => data_store,
            _ => unreachable!(),
        }
    }
}

struct RecipeIndexLookup {
    ing: usize,
    out: usize,
}

#[derive(Debug, Clone, serde::Serialize, serde:: Deserialize)]
pub struct RecipeIngLookups {
    pub ing0: Vec<[ITEMCOUNTTYPE; 0]>,
    pub ing1: Vec<[ITEMCOUNTTYPE; 1]>,
    pub ing2: Vec<[ITEMCOUNTTYPE; 2]>,
    pub ing3: Vec<[ITEMCOUNTTYPE; 3]>,
    pub ing4: Vec<[ITEMCOUNTTYPE; 4]>,
}

#[derive(Debug, Clone, serde::Serialize, serde:: Deserialize)]
pub struct RecipeOutputLookups {
    pub out1: Vec<[ITEMCOUNTTYPE; 1]>,
    pub out2: Vec<[ITEMCOUNTTYPE; 2]>,
    pub out3: Vec<[ITEMCOUNTTYPE; 3]>,
    pub out4: Vec<[ITEMCOUNTTYPE; 4]>,
}

impl RawDataStore {
    #[must_use]
    pub fn process(self) -> DataStoreOptions {
        match (self.items.len(), self.recipes.len()) {
            (items, recipes) if items <= u8::MAX.into() && recipes <= u8::MAX.into() => {
                DataStoreOptions::ItemU8RecipeU8(self.turn::<u8, u8>())
            },
            (items, recipes) if items <= u8::MAX.into() && recipes <= u16::MAX.into() => {
                DataStoreOptions::ItemU8RecipeU16(self.turn::<u8, u16>())
            },
            (items, recipes) if items <= u16::MAX.into() && recipes <= u8::MAX.into() => {
                DataStoreOptions::ItemU16RecipeU8(self.turn::<u16, u8>())
            },
            (items, recipes) if items <= u16::MAX.into() && recipes <= u16::MAX.into() => {
                DataStoreOptions::ItemU16RecipeU16(self.turn::<u16, u16>())
            },

            _ => unimplemented!("Too many items or recipes, u16::MAX is the max allowed amount (currently) (Btw, are you joking? Who are you trying to torture with this many options? xD)"),
        }
    }

    #[allow(clippy::too_many_lines)]
    pub fn turn<ItemIdxType: IdxTrait, RecipeIdxType: IdxTrait>(
        self,
    ) -> DataStore<ItemIdxType, RecipeIdxType> {
        let checksum = self.get_checksum();
        warn!("Parsing game data with checksum {}", checksum);

        let pipe_connection_groups_map: Vec<_> = self
            .machines
            .iter()
            .flat_map(|machine| {
                machine
                    .fluid_connection_flowthrough
                    .iter()
                    .flat_map(|flowthrough| {
                        flowthrough
                            .connections
                            .iter()
                            .flat_map(|v| match &v.2 {
                                RawPipeConnectionType::Direct => None,
                                RawPipeConnectionType::Underground {
                                    max_distance,
                                    underground_allowed_kinds,
                                } => Some(underground_allowed_kinds.iter().cloned()),
                            })
                            .flatten()
                    })
                    .chain(
                        machine
                            .fluid_connection_offsets
                            .iter()
                            .flat_map(|connection| match &connection.connection_type {
                                RawPipeConnectionType::Direct => None,
                                RawPipeConnectionType::Underground {
                                    max_distance,
                                    underground_allowed_kinds,
                                } => Some(underground_allowed_kinds.iter().cloned()),
                            })
                            .flatten(),
                    )
            })
            .chain(self.labs.iter().flat_map(|labs| {
                labs.fluid_connection_flowthrough
                    .iter()
                    .flat_map(|flowthrough| {
                        flowthrough.connections.iter().flat_map(|v| match &v.2 {
                            RawPipeConnectionType::Direct => None,
                            RawPipeConnectionType::Underground {
                                max_distance,
                                underground_allowed_kinds,
                            } => Some(underground_allowed_kinds.iter().cloned()),
                        })
                    })
                    .chain(labs.fluid_connection_offsets.iter().flat_map(|connection| {
                        match &connection.connection_type {
                            RawPipeConnectionType::Direct => None,
                            RawPipeConnectionType::Underground {
                                max_distance,
                                underground_allowed_kinds,
                            } => Some(underground_allowed_kinds.iter().cloned()),
                        }
                    }))
                    .flatten()
            }))
            .chain(self.fluid_tanks.iter().flat_map(|tank| {
                tank.connections
                    .iter()
                    .flat_map(|v| match &v.2 {
                        RawPipeConnectionType::Direct => None,
                        RawPipeConnectionType::Underground {
                            max_distance,
                            underground_allowed_kinds,
                        } => Some(underground_allowed_kinds.iter().cloned()),
                    })
                    .flatten()
            }))
            // We collect into a BTreeSet first, to ensure the order is deterministic (as opposed to a HashSet)
            .collect::<BTreeSet<_>>()
            .into_iter()
            .collect();

        assert!(
            pipe_connection_groups_map.iter().all(|v| !v.is_empty()),
            "Empty Underground groups are not allowed"
        );

        assert!(
            pipe_connection_groups_map.len() as u32 <= UndergroundGroupMask::BITS,
            "Currently a maximum of {} is supported",
            UndergroundGroupMask::BITS
        );

        // TODO: Stop cloning the item names
        let item_names = self.items.iter().map(|i| i.display_name.clone()).collect();

        let mut ing_out_num_to_recipe: HashMap<(usize, usize), Vec<Recipe<RecipeIdxType>>> = (0
            ..10)
            .cartesian_product(1..5)
            .map(|(ings, outputs)| ((ings, outputs), vec![]))
            .collect();
        let mut recipe_to_ing_out_combo_idx = vec![];

        let item_lookup: HashMap<&str, ItemIdxType> = self
            .items
            .iter()
            .map(|i| i.name.as_str())
            .enumerate()
            .map(|(i, v)| {
                (
                    v,
                    i.try_into()
                        .unwrap_or_else(|_| panic!("item idx did not fit!!!")),
                )
            })
            .collect();

        let reverse_item_lookup: HashMap<ItemIdxType, &str> =
            item_lookup.iter().map(|(k, v)| (*v, *k)).collect();

        let science_bottle_items: Vec<Item<ItemIdxType>> = self
            .items
            .iter()
            .filter(|i| i.science_data.is_some())
            .map(|r| {
                item_lookup
                    .get(r.name.as_str())
                    .expect("Science Item not in lookup?!?")
            })
            .map(|idx| Item { id: *idx })
            .collect();

        let item_to_recipe_where_its_ingredient: Vec<Vec<_>> = self
            .items
            .iter()
            .map(|item| {
                self.recipes
                    .iter()
                    .enumerate()
                    .filter_map(|(recipe_idx, r)| {
                        r.ings
                            .iter()
                            .find(|stack| stack.item == item.name)
                            .map(|found_stack| {
                                (
                                    Recipe {
                                        id: recipe_idx.try_into().unwrap_or_else(|_| todo!()),
                                    },
                                    found_stack.amount,
                                )
                            })
                    })
                    .collect()
            })
            .collect();

        let item_to_recipe_where_its_output: Vec<Vec<_>> =
            self.items
                .iter()
                .map(|item| {
                    self.recipes
                        .iter()
                        .enumerate()
                        .filter_map(|(recipe_idx, r)| {
                            r.output.iter().find(|stack| stack.item == item.name).map(
                                |found_stack| {
                                    (
                                        Recipe {
                                            id: recipe_idx.try_into().unwrap_or_else(|_| todo!()),
                                        },
                                        found_stack.amount,
                                    )
                                },
                            )
                        })
                        .collect()
                })
                .collect();

        for (i, raw_recipe) in self.recipes.iter().enumerate() {
            let recipe = Recipe {
                id: RecipeIdxType::try_from(i)
                    .unwrap_or_else(|_| panic!("recipe idx did not fit!!!")),
            };

            let prev_list =
                ing_out_num_to_recipe.get_mut(&(raw_recipe.ings.len(), raw_recipe.output.len()));

            if let Some(prev) = prev_list {
                recipe_to_ing_out_combo_idx.push(prev.len());
                prev.push(recipe);
            } else {
                recipe_to_ing_out_combo_idx.push(0);
                ing_out_num_to_recipe.insert(
                    (raw_recipe.ings.len(), raw_recipe.output.len()),
                    vec![recipe],
                );
            }
        }

        let mut recipe_item_store_to_item = vec![];

        let mut recipe_item_to_storage_list_idx = HashMap::new();

        for num_ings in 0..10 {
            for num_out in 1..10 {
                if let Some(recipes) = ing_out_num_to_recipe.get(&(num_ings, num_out)) {
                    for recipe in recipes {
                        let recipe_id: usize = recipe.id.into();
                        for ing in &self.recipes[recipe_id].ings {
                            let item = *item_lookup.get(ing.item.as_str()).unwrap_or_else(|| {
                                panic!("Item does not exist: {}", ing.item.as_str())
                            });

                            recipe_item_to_storage_list_idx.insert(
                                (*recipe, Item { id: item }, ItemRecipeDir::Ing),
                                recipe_item_store_to_item
                                    .iter()
                                    .filter(|i| **i == Item { id: item })
                                    .count()
                                    .try_into()
                                    .unwrap_or_else(|_| panic!("Too many storage type with item")),
                            );
                            recipe_item_store_to_item.push(Item { id: item });
                        }
                        for output in &self.recipes[recipe_id].output {
                            let item =
                                *item_lookup.get(output.item.as_str()).unwrap_or_else(|| {
                                    panic!("Item does not exist: {}", output.item.as_str())
                                });

                            recipe_item_to_storage_list_idx.insert(
                                (*recipe, Item { id: item }, ItemRecipeDir::Out),
                                recipe_item_store_to_item
                                    .iter()
                                    .filter(|i| **i == Item { id: item })
                                    .count()
                                    .try_into()
                                    .unwrap_or_else(|_| panic!("Too many storage type with item")),
                            );
                            recipe_item_store_to_item.push(Item { id: item });
                        }
                    }
                }
            }
        }

        let recipe_num_ing_lookup = self.recipes.iter().map(|r| r.ings.len()).collect();
        let recipe_num_out_lookup = self.recipes.iter().map(|r| r.output.len()).collect();

        let mut recipe_index_lookups = vec![];

        let mut recipe_ings = RecipeIngLookups {
            ing0: vec![],
            ing1: vec![],
            ing2: vec![],
            ing3: vec![],
            ing4: vec![],
        };

        let mut recipe_outputs = RecipeOutputLookups {
            out1: vec![],
            out2: vec![],
            out3: vec![],
            out4: vec![],
        };

        for (i, recipe) in self.recipes.iter().enumerate() {
            let ing_idx = match recipe.ings.len() {
                0 => {
                    recipe_ings
                        .ing0
                        .push(array::from_fn(|i| recipe.ings[i].amount));
                    recipe_ings.ing0.len()
                },
                1 => {
                    recipe_ings
                        .ing1
                        .push(array::from_fn(|i| recipe.ings[i].amount));
                    recipe_ings.ing1.len()
                },
                2 => {
                    recipe_ings
                        .ing2
                        .push(array::from_fn(|i| recipe.ings[i].amount));
                    recipe_ings.ing2.len()
                },
                3 => {
                    recipe_ings
                        .ing3
                        .push(array::from_fn(|i| recipe.ings[i].amount));
                    recipe_ings.ing3.len()
                },
                4 => {
                    recipe_ings
                        .ing4
                        .push(array::from_fn(|i| recipe.ings[i].amount));
                    recipe_ings.ing4.len()
                },
                n => {
                    unimplemented!("{n} ingredients in a single recipe are currently unsupported!!")
                },
            } - 1;

            let out_idx = match recipe.output.len() {
                0 => unimplemented!("Recipes without outputs are currently not supported!"),
                1 => {
                    recipe_outputs
                        .out1
                        .push(array::from_fn(|i| recipe.output[i].amount));
                    recipe_outputs.out1.len()
                },
                2 => {
                    recipe_outputs
                        .out2
                        .push(array::from_fn(|i| recipe.output[i].amount));
                    recipe_outputs.out2.len()
                },
                3 => {
                    recipe_outputs
                        .out3
                        .push(array::from_fn(|i| recipe.output[i].amount));
                    recipe_outputs.out3.len()
                },
                4 => {
                    recipe_outputs
                        .out4
                        .push(array::from_fn(|i| recipe.output[i].amount));
                    recipe_outputs.out4.len()
                },
                n => {
                    unimplemented!("{n} outputs in a single recipe are currently unsupported!!")
                },
            } - 1;

            recipe_index_lookups.push((ing_idx, out_idx));
        }

        let recipe_timers = self.recipes.iter().map(|r| r.time_to_craft).collect();

        let recipe_to_items = self
            .recipes
            .iter()
            .enumerate()
            .map(|(i, r)| {
                let mut v = vec![];

                for ing in &r.ings {
                    v.push((
                        ItemRecipeDir::Ing,
                        Item::from(*item_lookup.get(ing.item.as_str()).unwrap()),
                    ));
                }

                for out in &r.output {
                    v.push((
                        ItemRecipeDir::Out,
                        Item::from(*item_lookup.get(out.item.as_str()).unwrap()),
                    ));
                }

                (
                    Recipe {
                        id: RecipeIdxType::try_from(i).unwrap_or_else(|_| panic!()),
                    },
                    v,
                )
            })
            .collect();

        let recipe_to_items_and_amounts = self
            .recipes
            .iter()
            .enumerate()
            .map(|(i, r)| {
                let mut v = vec![];

                for ing in &r.ings {
                    v.push((
                        ItemRecipeDir::Ing,
                        Item::from(*item_lookup.get(ing.item.as_str()).unwrap()),
                        ing.amount,
                    ));
                }

                for out in &r.output {
                    v.push((
                        ItemRecipeDir::Out,
                        Item::from(*item_lookup.get(out.item.as_str()).unwrap()),
                        out.amount,
                    ));
                }

                (
                    Recipe {
                        id: RecipeIdxType::try_from(i).unwrap_or_else(|_| panic!()),
                    },
                    v,
                )
            })
            .collect();

        let power_pole_data = self
            .power_poles
            .iter()
            .map(|p| PowerPoleData {
                size: (u16::from(p.tile_size.0), u16::from(p.tile_size.1)),
                power_range: p.power_range,
                connection_range: p.connection_range,
            })
            .collect();

        let num_recipes_with_item = item_to_recipe_where_its_ingredient
            .iter()
            .map(Vec::len)
            .zip(item_to_recipe_where_its_output.iter().map(Vec::len))
            .map(|(ing, out)| ing + out)
            .collect();

        assert!(
            self.technologies
                .iter()
                .flat_map(|tech| tech.cost_of_single_research_unit.iter())
                .all(|stack| science_bottle_items.contains(&Item {
                    id: *item_lookup
                        .get(stack.item.as_str())
                        .expect("Could not find item")
                })),
            "Some research item in a technology is not designated as science item"
        );

        let technology_costs = self
            .technologies
            .iter()
            .map(|tech| {
                (
                    tech.num_units,
                    science_bottle_items
                        .iter()
                        .map(|item| reverse_item_lookup[&item.id])
                        .map(|raw_item_name| {
                            tech.cost_of_single_research_unit
                                .iter()
                                .find_map(|stack| {
                                    (stack.item == raw_item_name).then_some(stack.amount)
                                })
                                .unwrap_or(0)
                        })
                        .collect(),
                )
            })
            .collect();

        DataStore {
            checksum,

            belt_infos: vec![BeltInfo {
                name: "Transport Belt".to_string(),
                has_underground: true,
                has_splitter: None,
                // 7.5 items per second (per side, same speed as yellow belt)
                timer_increase: 45,
            }],

            // FIXME:
            mining_drill_info: vec![MiningDrillInfo {
                name: "Electric Mining Drill".to_string(),
                size: [3, 3],
                mining_range: [5, 5],
                base_speed: 20,
                resource_drain: (1, 1),
            }],

            recipe_allowed_assembling_machines: self
                .recipes
                .iter()
                .map(|raw| {
                    raw.possible_machines
                        .iter()
                        .map(|machine_string| {
                            self.machines
                                .iter()
                                .position(|machine| machine.name == *machine_string)
                                .expect(&format!(
                                    "Could not find machine {} for recipe {}!",
                                    machine_string, raw.name
                                ))
                                .try_into()
                                .unwrap()
                        })
                        .collect()
                })
                .collect(),

            recipe_is_intermediate: self.recipes.iter().map(|raw| raw.is_intermediate).collect(),

            assembler_info: self
                .machines
                .iter()
                .map(|m| AssemblerInfo {
                    display_name: m.display_name.clone(),

                    size: (m.tile_size.0.into(), m.tile_size.1.into()),
                    num_module_slots: m.num_module_slots,
                    base_speed: m.base_speed,
                    base_prod: m.base_bonus_prod,
                    base_power_consumption: m.working_power_draw,

                    fluid_connections: m
                        .fluid_connection_offsets
                        .iter()
                        .map(|raw_conn| {
                            (
                                FluidConnection {
                                    offset: [raw_conn.offset.0.into(), raw_conn.offset.1.into()],
                                    dir: raw_conn.pipe_connection_direction,
                                    kind: match &raw_conn.connection_type {
                                        RawPipeConnectionType::Direct => PipeConnectionType::Direct,
                                        RawPipeConnectionType::Underground {
                                            max_distance,
                                            underground_allowed_kinds,
                                        } => PipeConnectionType::Underground {
                                            max_distance: *max_distance,
                                            underground_group_mask: pipe_connection_groups_map
                                                .iter()
                                                .chain(iter::repeat(&String::from("")))
                                                .take(UndergroundGroupMask::BITS as usize)
                                                .enumerate()
                                                .map(|(i, group)| {
                                                    if underground_allowed_kinds.contains(group) {
                                                        1 << i
                                                    } else {
                                                        0
                                                    }
                                                })
                                                .sum(),
                                        },
                                    },
                                },
                                raw_conn.allowed_fluid_directions,
                            )
                        })
                        .collect(),
                })
                .collect(),

            lab_info: self
                .labs
                .iter()
                .map(|m| LabInfo {
                    display_name: m.display_name.clone(),

                    size: (m.tile_size.0.into(), m.tile_size.1.into()),
                    num_module_slots: m.num_module_slots,
                    base_speed: m.base_speed,
                    base_prod: m.base_bonus_prod,
                    base_power_consumption: m.working_power_draw,
                })
                .collect(),

            solar_panel_info: self
                .solar_panels
                .iter()
                .map(|raw| {
                    SolarPanelInfo {
                        name: raw.display_name.clone(),
                        size: [raw.tile_size.0.into(), raw.tile_size.1.into()],
                        // FIXME:
                        power_output: SolarPanelOutputFunction::Constant(raw.output),
                    }
                })
                .collect(),

            accumulator_info: self
                .accumulators
                .iter()
                .map(|raw| AccumulatorInfo {
                    name: raw.display_name.clone(),
                    size: [raw.tile_size.0.into(), raw.tile_size.1.into()],
                    max_charge: raw.charge,
                    max_charge_rate: raw.max_charge_rate,
                    max_discharge_rate: raw.max_discharge_rate,
                })
                .collect(),

            beacon_info: self
                .beacons
                .iter()
                .map(|m| BeaconInfo {
                    display_name: m.display_name.clone(),

                    size: (m.tile_size.0.into(), m.tile_size.1.into()),
                    num_module_slots: m.num_module_slots,
                    power_consumption: m.working_power_draw,

                    effectiveness: m.effectiveness,
                    effect_range: (m.effect_size.0.into(), m.effect_size.1.into()),
                })
                .collect(),

            max_beacon_range: self
                .beacons
                .iter()
                .map(|b| (b.effect_size.0.into(), b.effect_size.1.into()))
                .reduce(|a, b| (max(a.0, b.0), max(b.1, b.1)))
                .unwrap_or((0, 0)),

            module_info: self
                .modules
                .iter()
                .map(|module| ModuleInfo {
                    name: module.display_name.clone(),
                    speed_mod: module.speed_effect,
                    prod_mod: module.productivity_effect,
                    power_mod: module.power_effect,
                })
                .collect(),

            min_power_mod: 4,

            // TODO:
            max_entity_size: (5, 5),

            recipe_names: self
                .recipes
                .iter()
                .map(|r| r.display_name.clone())
                .collect(),

            recipe_num_ing_lookup,
            recipe_num_out_lookup,
            recipe_to_ing_out_combo_idx,
            ing_out_num_to_recipe,
            recipe_item_store_to_item,
            recipe_item_to_storage_list_idx,
            recipe_index_lookups,
            recipe_ings,

            recipe_outputs,

            recipe_timers,

            recipe_to_items,
            recipe_to_items_and_amounts,

            science_bottle_items,

            item_to_recipe_count_where_its_ingredient: item_to_recipe_where_its_ingredient,
            item_to_recipe_where_its_output,

            item_names,

            power_pole_data,

            item_is_fluid: self.items.iter().map(|i| i.is_fluid).collect(),

            lazy_power_machine_infos: vec![],

            max_power_search_range: self
                .power_poles
                .iter()
                .map(|p| p.power_range)
                .max()
                .expect("At least one type of power pole must exist"),

            // TODO:
            max_inserter_search_range: 2,

            item_is_science: self
                .items
                .iter()
                .map(|i| i.science_data.is_some())
                .collect(),
            num_different_static_containers: StaticID::iter().count(),
            num_recipes_with_item,

            item_stack_sizes: self.items.iter().map(|item| item.stack_size).collect(),
            chest_num_slots: self
                .chests
                .iter()
                .map(|chest| chest.number_of_slots)
                .collect(),
            chest_tile_sizes: self
                .chests
                .iter()
                .map(|chest| (u16::from(chest.tile_size.0), u16::from(chest.tile_size.1)))
                .collect(),

            recipe_to_translated_index: (0..self.recipes.len())
                .cartesian_product(
                    self.items
                        .iter()
                        .enumerate()
                        .map(|(item_id, item_name)| (item_id, &item_name.name)),
                )
                .map(|(recipe_id, (item_id, item_name))| {
                    (
                        (
                            Recipe {
                                id: recipe_id.try_into().unwrap(),
                            },
                            Item {
                                id: item_id.try_into().unwrap(),
                            },
                        ),
                        self.recipes
                            .iter()
                            .take(recipe_id)
                            .filter(|recipe| {
                                recipe
                                    .ings
                                    .iter()
                                    .map(|stack| &stack.item)
                                    .any(|item| item == item_name)
                                    | recipe
                                        .output
                                        .iter()
                                        .map(|stack| &stack.item)
                                        .any(|item| item == item_name)
                            })
                            .count()
                            .try_into()
                            .unwrap(),
                    )
                })
                .collect(),

            item_to_colour: self
                .items
                .iter()
                .map(|item| Color32::from_rgb(random(), random(), random()))
                .collect(),

            fluid_tank_infos: self
                .fluid_tanks
                .into_iter()
                .map(|tank| {
                    let max_search_range = tank
                        .connections
                        .iter()
                        .map(|conn| match &conn.2 {
                            RawPipeConnectionType::Direct => 1,
                            RawPipeConnectionType::Underground {
                                max_distance,
                                underground_allowed_kinds,
                            } => *max_distance,
                        })
                        .max()
                        .expect("A fluid tank without connections is useless")
                        .into();

                    FluidTankData {
                        // TODO: Sanity check the fluid connections
                        size: [tank.tile_size.0.into(), tank.tile_size.1.into()],
                        capacity: tank.capacity,
                        fluid_connections: tank
                            .connections
                            .into_iter()
                            .map(|conn| FluidConnection {
                                dir: conn.1,
                                offset: [conn.0 .0.into(), conn.0 .1.into()],
                                kind: match conn.2 {
                                    RawPipeConnectionType::Direct => PipeConnectionType::Direct,
                                    RawPipeConnectionType::Underground {
                                        max_distance,
                                        underground_allowed_kinds,
                                    } => PipeConnectionType::Underground {
                                        max_distance,
                                        underground_group_mask: pipe_connection_groups_map
                                            .iter()
                                            .chain(iter::repeat(&String::from("")))
                                            .take(UndergroundGroupMask::BITS as usize)
                                            .enumerate()
                                            .map(|(i, group)| {
                                                if underground_allowed_kinds.contains(group) {
                                                    1 << i
                                                } else {
                                                    0
                                                }
                                            })
                                            .sum(),
                                    },
                                },
                            })
                            .collect(),
                        max_search_range,
                    }
                })
                .collect(),

            technology_costs,
        }
    }

    pub fn get_checksum(&self) -> String {
        let mut hasher = Sha256::new();

        hasher.update(postcard::to_allocvec(self).unwrap());

        hex::encode_upper(hasher.finalize())
    }
}
