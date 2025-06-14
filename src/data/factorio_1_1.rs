use crate::power::Watt;

use super::{
    AllowedIn, RawAssemblingMachine, RawBeacon, RawChest, RawDataStore, RawEntity, RawItem,
    RawItemStack, RawLab, RawModule, RawPowerPole, RawRecipeData, RawSolarPanel, RawTechnology,
};

const RAW_DATA_STR: &'static str = include_str!("factorio_1_1.fgmod");

#[must_use]
pub fn get_raw_data_test() -> RawDataStore {
    ron::from_str(RAW_DATA_STR).expect("RAW_DATA_STR invalid")
    // get_raw_data_fn()
}

#[must_use]
pub fn get_raw_data_fn() -> RawDataStore {
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
                time_to_craft: 192,
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
                time_to_craft: 192,
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
                time_to_craft: 30,

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
            RawRecipeData {
                name: "factory_game::copper_wire".to_string(),
                display_name: "Copper Wire".to_string(),
                possible_machines: vec!["factory_game::assembler".to_string()].into_boxed_slice(),
                ings: vec![RawItemStack {
                    item: "factory_game::copper_plate".to_string(),
                    amount: 1,
                }]
                .into_boxed_slice(),
                output: vec![RawItemStack {
                    item: "factory_game::copper_wire".to_string(),
                    amount: 2,
                }]
                .into_boxed_slice(),
                time_to_craft: 30,

                is_intermediate: true,
            },
            RawRecipeData {
                name: "factory_game::green_chip".to_string(),
                display_name: "Green Chip".to_string(),
                possible_machines: vec!["factory_game::assembler".to_string()].into_boxed_slice(),
                ings: vec![
                    RawItemStack {
                        item: "factory_game::copper_wire".to_string(),
                        amount: 3,
                    },
                    RawItemStack {
                        item: "factory_game::iron_plate".to_string(),
                        amount: 1,
                    },
                ]
                .into_boxed_slice(),
                output: vec![RawItemStack {
                    item: "factory_game::green_chip".to_string(),
                    amount: 1,
                }]
                .into_boxed_slice(),
                time_to_craft: 30,

                is_intermediate: true,
            },
            RawRecipeData {
                name: "factory_game::yellow_belt".to_string(),
                display_name: "Yellow Belt".to_string(),
                possible_machines: vec!["factory_game::assembler".to_string()].into_boxed_slice(),
                ings: vec![
                    RawItemStack {
                        item: "factory_game::gear".to_string(),
                        amount: 1,
                    },
                    RawItemStack {
                        item: "factory_game::iron_plate".to_string(),
                        amount: 1,
                    },
                ]
                .into_boxed_slice(),
                output: vec![RawItemStack {
                    item: "factory_game::yellow_belt".to_string(),
                    amount: 2,
                }]
                .into_boxed_slice(),
                time_to_craft: 30,

                is_intermediate: false,
            },
            RawRecipeData {
                name: "factory_game::inserter".to_string(),
                display_name: "Inserter".to_string(),
                possible_machines: vec!["factory_game::assembler".to_string()].into_boxed_slice(),
                ings: vec![
                    RawItemStack {
                        item: "factory_game::green_chip".to_string(),
                        amount: 1,
                    },
                    RawItemStack {
                        item: "factory_game::gear".to_string(),
                        amount: 1,
                    },
                    RawItemStack {
                        item: "factory_game::iron_plate".to_string(),
                        amount: 1,
                    },
                ]
                .into_boxed_slice(),
                output: vec![RawItemStack {
                    item: "factory_game::inserter".to_string(),
                    amount: 1,
                }]
                .into_boxed_slice(),
                time_to_craft: 30,

                is_intermediate: false,
            },
            RawRecipeData {
                name: "factory_game::green_science".to_string(),
                display_name: "Green Science".to_string(),
                possible_machines: vec!["factory_game::assembler".to_string()].into_boxed_slice(),
                ings: vec![
                    RawItemStack {
                        item: "factory_game::yellow_belt".to_string(),
                        amount: 1,
                    },
                    RawItemStack {
                        item: "factory_game::inserter".to_string(),
                        amount: 1,
                    },
                ]
                .into_boxed_slice(),
                output: vec![RawItemStack {
                    item: "factory_game::green_science".to_string(),
                    amount: 1,
                }]
                .into_boxed_slice(),
                time_to_craft: 360,

                is_intermediate: true,
            },
            RawRecipeData {
                name: "factory_game::water_generation".to_string(),
                display_name: "Generate Water from nothing".to_string(),
                possible_machines: vec!["factory_game::assembler".to_string()].into_boxed_slice(),
                ings: vec![].into_boxed_slice(),
                output: vec![RawItemStack {
                    item: "factory_game::water".to_string(),
                    amount: 1,
                }]
                .into_boxed_slice(),
                time_to_craft: 5,
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
            RawItem {
                name: "factory_game::copper_wire".to_string(),
                display_name: "Copper Wire".to_string(),
                stack_size: 200,
                placed_as: None,
                burnable_in: vec![].into_boxed_slice(),
                science_data: None,
                is_fluid: false,
            },
            RawItem {
                name: "factory_game::green_chip".to_string(),
                display_name: "Green Chip".to_string(),
                stack_size: 200,
                placed_as: None,
                burnable_in: vec![].into_boxed_slice(),
                science_data: None,
                is_fluid: false,
            },
            RawItem {
                name: "factory_game::yellow_belt".to_string(),
                display_name: "Yellow Belt".to_string(),
                stack_size: 100,
                placed_as: Some(RawEntity::Belt(())),
                burnable_in: vec![].into_boxed_slice(),
                science_data: None,
                is_fluid: false,
            },
            RawItem {
                name: "factory_game::inserter".to_string(),
                display_name: "Inserter".to_string(),
                stack_size: 50,
                placed_as: Some(RawEntity::Inserter("inserter".to_string())),
                burnable_in: vec![].into_boxed_slice(),
                science_data: None,
                is_fluid: false,
            },
            RawItem {
                name: "factory_game::green_science".to_string(),
                display_name: "Green Science".to_string(),
                stack_size: 200,
                placed_as: None,
                burnable_in: vec![].into_boxed_slice(),
                science_data: Some(()),
                is_fluid: false,
            },
            RawItem {
                name: "factory_game::water".to_string(),
                display_name: "Water".to_string(),
                stack_size: 1,
                placed_as: None,
                burnable_in: vec![].into_boxed_slice(),
                science_data: None,
                is_fluid: true,
            },
        ],
        machines: vec![
            RawAssemblingMachine {
                name: "factory_game::assembler1".to_string(),
                display_name: "Assembling Machine".to_string(),
                tile_size: (3, 3),
                working_power_draw: Watt(150_000),
                fluid_connection_offsets: vec![],
                fluid_connection_flowthrough: vec![],
                base_bonus_prod: 0,
                base_speed: 10,
                num_module_slots: 0,
            },
            RawAssemblingMachine {
                name: "factory_game::assembler2".to_string(),
                display_name: "Assembling Machine 2".to_string(),
                tile_size: (3, 3),
                working_power_draw: Watt(75_000),
                fluid_connection_offsets: vec![],
                fluid_connection_flowthrough: vec![],
                base_bonus_prod: 0,
                base_speed: 15,
                num_module_slots: 2,
            },
            RawAssemblingMachine {
                name: "factory_game::assembler3".to_string(),
                display_name: "Assembling Machine 3".to_string(),
                tile_size: (3, 3),
                working_power_draw: Watt(375_000),
                fluid_connection_offsets: vec![],
                fluid_connection_flowthrough: vec![],
                base_bonus_prod: 0,
                base_speed: 25,
                num_module_slots: 4,
            },
            RawAssemblingMachine {
                name: "factory_game::electric_furnace".to_string(),
                display_name: "Electric Furnace".to_string(),
                tile_size: (3, 3),
                working_power_draw: Watt(180000),
                fluid_connection_offsets: vec![],
                fluid_connection_flowthrough: vec![],
                num_module_slots: 2,
                base_bonus_prod: 0,
                base_speed: 40,
            },
        ],
        labs: vec![RawLab {
            name: "factory_game::lab".to_string(),
            display_name: "Lab".to_string(),
            tile_size: (3, 3),
            working_power_draw: Watt(60_000),
            fluid_connection_offsets: vec![],
            num_module_slots: 2,
            base_bonus_prod: 0,
            base_speed: 1,
        }],
        beacons: vec![RawBeacon {
            name: "factory_game::beacon".to_string(),
            display_name: "Beacon".to_string(),
            tile_size: (3, 3),
            working_power_draw: Watt(480_000),
            num_module_slots: 2,
            effectiveness: (1, 2),
            effect_size: (9, 9),
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
                name: "factory_game::speed_mod".to_string(),
                display_name: "Speed Module".to_string(),
                item: "factory_game::speed_mod".to_string(),
                productivity_effect: 0,
                speed_effect: 10,
                power_effect: 14,
                allowed_in: AllowedIn::AllIncludingBeacons,
            },
            RawModule {
                name: "factory_game::prod_mod".to_string(),
                display_name: "Productivity Module".to_string(),
                item: "factory_game::prod_mod".to_string(),
                productivity_effect: 10,
                speed_effect: -3,
                power_effect: 16,
                allowed_in: AllowedIn::OnlyIntermediate,
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
        solar_panels: vec![
            RawSolarPanel {
                name: "factory_game::infinity_battery".to_string(),
                display_name: "Infinity Battery".to_string(),
                tile_size: (2, 2),
                // 1 Terrawatt should be enough for now
                output: Watt(1_000_000_000_000),
            },
            RawSolarPanel {
                name: "factory_game::solar_panel".to_string(),
                display_name: "Solar Panel".to_string(),
                tile_size: (3, 3),
                // TODO: Non constant output
                output: Watt(60_000),
            },
        ],
        accumulators: vec![],
        fluid_tanks: vec![],
    }
}
