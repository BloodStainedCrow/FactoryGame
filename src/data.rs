use std::{array, collections::HashMap};

use eframe::egui::Color32;
use itertools::Itertools;
use log::warn;
use rand::random;
use sha2::{Digest, Sha256};
use strum::IntoEnumIterator;

use crate::{
    assembler::TIMERTYPE,
    frontend::world::tile::{AssemblerID, Dir},
    inserter::{StaticID, Storage},
    item::{IdxTrait, Item, Recipe, WeakIdxTrait, ITEMCOUNTTYPE},
    power::{power_grid::PowerGridIdentifier, Joule, Watt},
};

type ItemString = String;
type AssemblingMachineString = String;
type InserterString = String;
type EngineString = String;

#[must_use]
pub fn get_raw_data_test() -> RawDataStore {
    RawDataStore {
        recipes: vec![
            RawRecipeData {
                name: "factory_game::iron_ore_generation".to_string(),
                display_name: "Generate Iron Ore from nothing".to_string(),
                possible_machines: vec!["factory_game::assembler".to_string()].into_boxed_slice(),
                ings: vec![].into_boxed_slice(),
                output: vec![RawItemStack {
                    item: "factory_game::iron_ore".to_string(),
                    amount: 1,
                }]
                .into_boxed_slice(),
                time_to_craft: 60,
                is_intermediate: false,
            },
            RawRecipeData {
                name: "factory_game::copper_ore_generation".to_string(),
                display_name: "Generate Copper Ore from nothing".to_string(),
                possible_machines: vec!["factory_game::assembler".to_string()].into_boxed_slice(),
                ings: vec![].into_boxed_slice(),
                output: vec![RawItemStack {
                    item: "factory_game::copper_ore".to_string(),
                    amount: 1,
                }]
                .into_boxed_slice(),
                time_to_craft: 60,
                is_intermediate: false,
            },
            RawRecipeData {
                name: "factory_game::iron_smelting".to_string(),
                display_name: "Smelt Iron Ore into Iron Plates".to_string(),
                possible_machines: vec!["factory_game::assembler".to_string()].into_boxed_slice(),
                ings: vec![RawItemStack {
                    item: "factory_game::iron_ore".to_string(),
                    amount: 1,
                }]
                .into_boxed_slice(),
                output: vec![RawItemStack {
                    item: "factory_game::iron_plate".to_string(),
                    amount: 1,
                }]
                .into_boxed_slice(),
                time_to_craft: 300,
                is_intermediate: true,
            },
            RawRecipeData {
                name: "factory_game::copper_smelting".to_string(),
                display_name: "Smelt Copper Ore into Copper Plates".to_string(),
                possible_machines: vec!["factory_game::assembler".to_string()].into_boxed_slice(),
                ings: vec![RawItemStack {
                    item: "factory_game::copper_ore".to_string(),
                    amount: 1,
                }]
                .into_boxed_slice(),
                output: vec![RawItemStack {
                    item: "factory_game::copper_plate".to_string(),
                    amount: 1,
                }]
                .into_boxed_slice(),
                time_to_craft: 300,
                is_intermediate: true,
            },
            RawRecipeData {
                name: "factory_game::gears".to_string(),
                display_name: "Gears".to_string(),
                possible_machines: vec!["factory_game::assembler".to_string()].into_boxed_slice(),
                ings: vec![RawItemStack {
                    item: "factory_game::iron_plate".to_string(),
                    amount: 2,
                }]
                .into_boxed_slice(),
                output: vec![RawItemStack {
                    item: "factory_game::gear".to_string(),
                    amount: 1,
                }]
                .into_boxed_slice(),
                time_to_craft: 600,

                is_intermediate: true,
            },
            RawRecipeData {
                name: "factory_game::red_science".to_string(),
                display_name: "Automation Science".to_string(),
                possible_machines: vec!["factory_game::assembler".to_string()].into_boxed_slice(),
                ings: vec![
                    RawItemStack {
                        item: "factory_game::gear".to_string(),
                        amount: 1,
                    },
                    RawItemStack {
                        item: "factory_game::copper_plate".to_string(),
                        amount: 1,
                    },
                ]
                .into_boxed_slice(),
                output: vec![RawItemStack {
                    item: "factory_game::red_science".to_string(),
                    amount: 1,
                }]
                .into_boxed_slice(),
                time_to_craft: 300,

                is_intermediate: true,
            },
        ],
        items: vec![
            RawItem {
                name: "factory_game::iron_ore".to_string(),
                display_name: "Iron Ore".to_string(),
                stack_size: 100,
                placed_as: None,
                burnable_in: vec![].into_boxed_slice(),
                science_data: None,
                is_fluid: false,
            },
            RawItem {
                name: "factory_game::copper_ore".to_string(),
                display_name: "Copper Ore".to_string(),
                stack_size: 100,
                placed_as: None,
                burnable_in: vec![].into_boxed_slice(),
                science_data: None,
                is_fluid: false,
            },
            RawItem {
                name: "factory_game::iron_plate".to_string(),
                display_name: "Iron Plate".to_string(),
                stack_size: 100,
                placed_as: None,
                burnable_in: vec![].into_boxed_slice(),
                science_data: None,
                is_fluid: false,
            },
            RawItem {
                name: "factory_game::copper_plate".to_string(),
                display_name: "Copper Plate".to_string(),
                stack_size: 100,
                placed_as: None,
                burnable_in: vec![].into_boxed_slice(),
                science_data: None,
                is_fluid: false,
            },
            RawItem {
                name: "factory_game::gear".to_string(),
                display_name: "Gear".to_string(),
                stack_size: 50,
                placed_as: None,
                burnable_in: vec![].into_boxed_slice(),
                science_data: None,
                is_fluid: false,
            },
            RawItem {
                name: "factory_game::red_science".to_string(),
                display_name: "Automation Science Pack".to_string(),
                stack_size: 200,
                placed_as: None,
                burnable_in: vec![].into_boxed_slice(),
                science_data: Some(()),
                is_fluid: false,
            },
        ],
        machines: vec![RawAssemblingMachine {
            name: "factory_game::assembler2".to_string(),
            display_name: "Assembling Machine 2".to_string(),
            tile_size: (3, 3),
            working_power_draw: Watt(150_000),
            fluid_connection_offsets: vec![],
            base_bonus_prod: 0,
            base_speed: 15,
            num_module_slots: 2,
        }],
        miners: vec![],
        power_poles: vec![
            RawPowerPole {
                name: "factory_game::small_power_pole".to_string(),
                display_name: "Small Power Pole".to_string(),
                tile_size: (1, 1),
                power_range: 2,
                // TODO:
                connection_range: 7,
            },
            RawPowerPole {
                name: "factory_game::medium_power_pole".to_string(),
                display_name: "Medium Power Pole".to_string(),
                tile_size: (1, 1),
                power_range: 3,
                // TODO:
                connection_range: 9,
            },
            RawPowerPole {
                name: "factory_game::large_power_pole".to_string(),
                display_name: "Large Power Pole".to_string(),
                tile_size: (2, 2),
                power_range: 1,
                // TODO:
                connection_range: 32,
            },
            RawPowerPole {
                name: "factory_game::substation".to_string(),
                display_name: "Substation".to_string(),
                tile_size: (2, 2),
                power_range: 8,
                // TODO:
                connection_range: 16,
            },
        ],
        modules: vec![
            RawModule {
                name: "factory_game::prod_mod".to_string(),
                display_name: "Productivity Module".to_string(),
                item: "factory_game::prod_mod".to_string(),
                productivity_effect: 10,
                speed_effect: -2,
                power_effect: 8,
                allowed_in: AllowedIn::OnlyIntermediate,
            },
            RawModule {
                name: "factory_game::speed_mod".to_string(),
                display_name: "Speed Module".to_string(),
                item: "factory_game::speed_mod".to_string(),
                productivity_effect: 0,
                speed_effect: 5,
                power_effect: 7,
                allowed_in: AllowedIn::AllIncludingBeacons,
            },
        ],

        chests: vec![
            RawChest {
                name: "factory_game::wooden_chest".to_string(),
                display_name: "Wooden Chest".to_string(),
                tile_size: (1, 1),
                number_of_slots: 16,
            },
            RawChest {
                name: "factory_game::iron_chest".to_string(),
                display_name: "Iron Chest".to_string(),
                tile_size: (1, 1),
                number_of_slots: 32,
            },
            RawChest {
                name: "factory_game::steel_chest".to_string(),
                display_name: "Steel Chest".to_string(),
                tile_size: (1, 1),
                number_of_slots: 48,
            },
        ],
        technologies: vec![RawTechnology {
            name: "factory_game::automation".to_string(),
            display_name: "Automation".to_string(),
            cost_of_single_research_unit: vec![RawItemStack {
                item: "factory_game::red_science".to_string(),
                amount: 1,
            }],
            num_units: 10,
            precursors: vec![],
        }],
    }
}

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

    num_module_slots: u8,

    /// Base bonus productivity in %
    base_bonus_prod: u8,
    /// Speed multiplier compared to "baseline" in 5%
    base_speed: u8,
}

#[derive(Debug, Clone, serde::Deserialize, serde::Serialize)]
struct RawFluidConnection {
    fluid_dir: ItemRecipeDir,
    offs: (u8, u8),
    pipe_connection_direction: Dir,
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
    miners: Vec<RawMiner>,
    power_poles: Vec<RawPowerPole>,
    modules: Vec<RawModule>,
    chests: Vec<RawChest>,
    technologies: Vec<RawTechnology>,
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

#[derive(Debug, Clone)]
pub struct AssemblerInfo {
    pub size: (u8, u8),
    pub num_module_slots: u8,

    /// Base Speed as a numerator and divisor. All module modifiers apply multiplicatively
    pub base_speed: u8,
    pub base_prod: u8,
    pub base_power_consumption: Watt,
}

#[derive(Debug, Clone)]
pub struct ModuleInfo {
    pub name: String,

    pub speed_mod: i8,
    pub prod_mod: i8,
    pub power_mod: i8,
}

#[derive(Debug, Clone)]
pub struct DataStore<ItemIdxType: WeakIdxTrait, RecipeIdxType: WeakIdxTrait> {
    pub checksum: String,

    pub max_entity_size: (usize, usize),

    pub assembler_info: Vec<AssemblerInfo>,
    pub module_info: Vec<ModuleInfo>,

    /// In 5% steps
    pub min_power_mod: u8,

    pub recipe_names: Vec<String>,

    pub recipe_num_ing_lookup: Vec<usize>,
    pub recipe_num_out_lookup: Vec<usize>,
    pub recipe_to_ing_out_combo_idx: Vec<usize>,
    pub ing_out_num_to_recipe: HashMap<(usize, usize), Vec<Recipe<RecipeIdxType>>>,

    pub recipe_item_store_to_item: Vec<Item<ItemIdxType>>,

    recipe_item_to_storage_list_idx:
        HashMap<(Recipe<RecipeIdxType>, Item<ItemIdxType>, ItemRecipeDir), u16>,

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

    pub lab_infos: Vec<()>,
    pub item_is_science: Vec<bool>,

    pub item_stack_sizes: Vec<ITEMCOUNTTYPE>,
    pub chest_num_slots: Vec<u8>,
    pub chest_tile_sizes: Vec<(u8, u8)>,

    pub recipe_to_translated_index:
        HashMap<(Recipe<RecipeIdxType>, Item<ItemIdxType>), RecipeIdxType>,

    pub item_to_colour: Vec<Color32>,

    pub technology_costs: Vec<(u64, Box<[ITEMCOUNTTYPE]>)>,
}

#[derive(Debug, Clone)]
pub struct LazyPowerMachineInfo<ItemIdxType: WeakIdxTrait> {
    pub ingredient: Item<ItemIdxType>,
    pub power_per_item: Joule,
    pub max_power_per_tick: Joule,
}

#[derive(Debug, Clone)]
pub struct PowerPoleData {
    pub size: (u8, u8),
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

#[derive(Debug, Clone)]
pub struct RecipeIngLookups {
    pub ing0: Vec<[ITEMCOUNTTYPE; 0]>,
    pub ing1: Vec<[ITEMCOUNTTYPE; 1]>,
    pub ing2: Vec<[ITEMCOUNTTYPE; 2]>,
    pub ing3: Vec<[ITEMCOUNTTYPE; 3]>,
    pub ing4: Vec<[ITEMCOUNTTYPE; 4]>,
}

#[derive(Debug, Clone)]
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

        // TODO: Stop cloning the item names
        let item_names = self.items.iter().map(|i| i.display_name.clone()).collect();

        let mut ing_out_num_to_recipe: HashMap<(usize, usize), Vec<Recipe<RecipeIdxType>>> =
            HashMap::new();
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

        let power_pole_data = self
            .power_poles
            .iter()
            .map(|p| PowerPoleData {
                size: p.tile_size,
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

            assembler_info: self
                .machines
                .iter()
                .map(|m| AssemblerInfo {
                    size: m.tile_size,
                    num_module_slots: m.num_module_slots,
                    // TODO: Hm, this seems rather silly
                    base_speed: m.base_speed,
                    base_prod: m.base_bonus_prod,
                    base_power_consumption: m.working_power_draw,
                })
                .collect(),

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
            max_entity_size: (4, 4),

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

            max_inserter_search_range: 2,

            item_is_science: self
                .items
                .iter()
                .map(|i| i.science_data.is_some())
                .collect(),
            lab_infos: vec![],
            num_different_static_containers: StaticID::iter().count(),
            num_recipes_with_item,

            item_stack_sizes: self.items.iter().map(|item| item.stack_size).collect(),
            chest_num_slots: self
                .chests
                .iter()
                .map(|chest| chest.number_of_slots)
                .collect(),
            chest_tile_sizes: self.chests.iter().map(|chest| chest.tile_size).collect(),

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
                .map(|item_name| Color32::from_rgb(random(), random(), random()))
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

impl<ItemIdxType: IdxTrait, RecipeIdxType: IdxTrait> DataStore<ItemIdxType, RecipeIdxType> {
    pub fn get_storage_id_for_assembler(
        &self,
        dir: ItemRecipeDir,
        item: Item<ItemIdxType>,
        assembler_id: AssemblerID<RecipeIdxType>,
    ) -> Result<Storage<RecipeIdxType>, ()> {
        let Some(storage_list_idx) =
            self.recipe_item_to_storage_list_idx
                .get(&(assembler_id.recipe, item, dir))
        else {
            return Err(());
        };

        // Ok(StorageID {
        //     grid: assembler_id.grid,
        //     storage_list_idx: *storage_list_idx,
        //     machine_idx: assembler_id.assembler_index,

        //     phantom: PhantomData,
        // })
        Ok(Storage::Assembler {
            grid: assembler_id.grid,
            recipe_idx_with_this_item: assembler_id.recipe.id,
            index: assembler_id.assembler_index,
        })
    }

    pub fn get_storage_id_for_lab_science(
        &self,
        grid: PowerGridIdentifier,
        lab_idx: u16,
    ) -> Storage<RecipeIdxType> {
        // let num_entries_for_assemblers = self.recipe_item_to_storage_list_idx.len();
        // let science_idx = self
        //     .science_bottle_items
        //     .iter()
        //     .position(|i| *i == item)
        //     .expect("Science item for lab is not in science list");

        Storage::Lab {
            grid,
            index: lab_idx,
        }
        // StorageID {
        //     grid,
        //     storage_list_idx: (num_entries_for_assemblers + science_idx)
        //         .try_into()
        //         .expect("More than u16::MAX assemblers"),
        //     machine_idx: lab_idx,

        //     phantom: PhantomData,
        // }
    }
}
