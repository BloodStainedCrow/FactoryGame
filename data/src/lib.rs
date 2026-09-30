#![feature(int_roundings)]

use std::{
    cmp::max,
    collections::HashMap,
    sync::{Arc, LazyLock},
};

use crate::{
    api::{
        ModData,
        entity::{
            belt::BeltInfo,
            chest::ChestInfo,
            inserter::{InserterInfo, InserterMovementTime},
            power_pole::PowerPoleInfo,
            solar_panel::SolarPanelInfo,
        },
        item::{ItemIdentifier, ItemName},
        recipe::RecipeName,
    },
    energy::{EnergySource, Watt},
    entity::{GlobalTy, PlacementRules, assember::AssemblerInfo, power_pole::PowerPoleData},
    item::ItemInfo,
    recipe::RecipeInfo,
    spacial::Extent,
};

pub const TICKS_PER_SECOND_LOGIC: usize = 60;
#[expect(clippy::cast_precision_loss)]
pub const TICKS_PER_SECOND_LOGIC_F32: f32 = TICKS_PER_SECOND_LOGIC as f32;

pub mod api;
pub mod energy;
pub mod entity;
pub mod item;
pub mod recipe;
pub mod spacial;

#[derive(Debug, Clone, serde::Deserialize)]
struct ModIdentifier(String);

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct EntityIdentifier(Arc<str>);

#[derive(Debug, Clone, serde::Deserialize)]
struct EntityName(String);

impl EntityIdentifier {
    fn new(mod_: &ModIdentifier, name: &EntityName) -> Self {
        Self(format!("{}::{}", mod_.0, name.0).into())
    }

    // TODO
    #[must_use]
    pub fn new_raw(full_name: String) -> Self {
        Self(full_name.into())
    }
}

#[derive(Debug)]
struct EntityInfo {
    size: Extent,

    can_be_rotated: bool,
    can_be_flipped: bool,

    kind: EntityPrototypeKind,

    name: EntityIdentifier,
    // FIXME(BSC): localisation support!
    display_name: String,
    // TODO: Icon, Collision, Sound, Placement (i.e. which item places it), mapcolor
    placement_rules: PlacementRules,
}

#[derive(Debug)]
struct DataStore {
    entities: Vec<EntityInfo>,
    power_poles: Vec<PowerPoleData>,
    inserters: Vec<InserterInfo>,
    assemblers: Vec<AssemblerInfo>,

    items: Vec<ItemInfo>,
    recipes: Vec<RecipeInfo>,
}

/// The parsed data of the currently loaded mod set
static DATA_STORE: LazyLock<DataStore> = LazyLock::new(|| {
    let mod_ident = ModIdentifier("factory_game".to_string());
    DataStore::from_mods(&[ModData {
        mod_name: mod_ident.clone(),

        solar_panels: vec![
            SolarPanelInfo {
                entity_info: api::entity::EntityInfo {
                    size: Extent {
                        width: 3,
                        height: 3,
                    },
                    can_be_rotated: false,
                    can_be_flipped: false,
                    name: EntityName("solar_panel".to_string()),
                    display_name: "Solar Panel".to_string(),
                    placement_rules: PlacementRules::no_restriction(),
                },
            },
            SolarPanelInfo {
                entity_info: api::entity::EntityInfo {
                    size: Extent {
                        width: 2,
                        height: 2,
                    },
                    can_be_rotated: false,
                    can_be_flipped: false,
                    name: EntityName("infinity_battery".to_string()),
                    display_name: "Infinity Battery".to_string(),
                    placement_rules: PlacementRules::no_restriction(),
                },
            },
        ],

        inserters: vec![InserterInfo {
            entity_info: api::entity::EntityInfo {
                size: Extent::single_tile(),
                can_be_rotated: true,
                can_be_flipped: false,
                name: EntityName("bulk_inserter".to_string()),
                display_name: "Bulk Inserter".to_string(),
                placement_rules: PlacementRules::no_restriction(),
            },
            source_offset: spacial::Offset {
                x_offs: 0,
                y_offs: 1,
            },
            dest_offset: spacial::Offset {
                x_offs: 0,
                y_offs: -1,
            },
            movetime: InserterMovementTime::RotationPerSecond { degrees: 864.0 },
            energy_source: EnergySource::ElectricEnergy {
                drain: Watt(1000),
                active_energy: Watt(169_000),
            },
            filter_count: 5,
            hand_size_bonus: 1,
        }],
        power_poles: vec![
            PowerPoleInfo {
                entity_info: api::entity::EntityInfo {
                    size: Extent::single_tile(),
                    can_be_rotated: false,
                    can_be_flipped: false,
                    name: EntityName("small_power_pole".to_string()),
                    display_name: "Small Power Pole".to_string(),
                    placement_rules: PlacementRules::no_restriction(),
                },
                range: 5,
                wire_reach: 15,
            },
            PowerPoleInfo {
                entity_info: api::entity::EntityInfo {
                    size: Extent::single_tile(),
                    can_be_rotated: false,
                    can_be_flipped: false,
                    name: EntityName("medium_power_pole".to_string()),
                    display_name: "Medium Power Pole".to_string(),
                    placement_rules: PlacementRules::no_restriction(),
                },
                range: 7,
                wire_reach: 18,
            },
            PowerPoleInfo {
                entity_info: api::entity::EntityInfo {
                    size: Extent {
                        width: 2,
                        height: 2,
                    },
                    can_be_rotated: false,
                    can_be_flipped: false,
                    name: EntityName("large_power_pole".to_string()),
                    display_name: "Large Power Pole".to_string(),
                    placement_rules: PlacementRules::no_restriction(),
                },
                range: 4,
                wire_reach: 64,
            },
            PowerPoleInfo {
                entity_info: api::entity::EntityInfo {
                    size: Extent {
                        width: 2,
                        height: 2,
                    },
                    can_be_rotated: false,
                    can_be_flipped: false,
                    name: EntityName("substation".to_string()),
                    display_name: "Substation".to_string(),
                    placement_rules: PlacementRules::no_restriction(),
                },
                range: 18,
                wire_reach: 38,
            },
        ],
        assemblers: vec![
            api::entity::assembler::AssemblerInfo {
                entity_info: api::entity::EntityInfo {
                    size: Extent {
                        width: 3,
                        height: 3,
                    },
                    can_be_rotated: true,
                    can_be_flipped: true,
                    name: EntityName("assembler1".to_string()),
                    display_name: "Assembler 1".to_string(),
                    placement_rules: PlacementRules::no_restriction(),
                },

                default_recipe: None,
            },
            api::entity::assembler::AssemblerInfo {
                entity_info: api::entity::EntityInfo {
                    size: Extent {
                        width: 3,
                        height: 3,
                    },
                    can_be_rotated: true,
                    can_be_flipped: true,
                    name: EntityName("assembler2".to_string()),
                    display_name: "Assembler 2".to_string(),
                    placement_rules: PlacementRules::no_restriction(),
                },

                default_recipe: None,
            },
            api::entity::assembler::AssemblerInfo {
                entity_info: api::entity::EntityInfo {
                    size: Extent {
                        width: 3,
                        height: 3,
                    },
                    can_be_rotated: true,
                    can_be_flipped: true,
                    name: EntityName("assembler3".to_string()),
                    display_name: "Assembler 3".to_string(),
                    placement_rules: PlacementRules::no_restriction(),
                },

                default_recipe: None,
            },
            api::entity::assembler::AssemblerInfo {
                entity_info: api::entity::EntityInfo {
                    size: Extent {
                        width: 3,
                        height: 3,
                    },
                    can_be_rotated: true,
                    can_be_flipped: true,
                    name: EntityName("chemical_plant".to_string()),
                    display_name: "Chemical Plant".to_string(),
                    placement_rules: PlacementRules::no_restriction(),
                },

                default_recipe: None,
            },
            api::entity::assembler::AssemblerInfo {
                entity_info: api::entity::EntityInfo {
                    size: Extent {
                        width: 5,
                        height: 5,
                    },
                    can_be_rotated: true,
                    can_be_flipped: true,
                    name: EntityName("refinery".to_string()),
                    display_name: "Refinery".to_string(),
                    placement_rules: PlacementRules::no_restriction(),
                },

                default_recipe: None,
            },
            api::entity::assembler::AssemblerInfo {
                entity_info: api::entity::EntityInfo {
                    size: Extent {
                        width: 3,
                        height: 3,
                    },
                    can_be_rotated: true,
                    can_be_flipped: true,
                    name: EntityName("electric_furnace".to_string()),
                    display_name: "Electric Furnace".to_string(),
                    placement_rules: PlacementRules::no_restriction(),
                },

                default_recipe: None,
            },
            api::entity::assembler::AssemblerInfo {
                entity_info: api::entity::EntityInfo {
                    size: Extent {
                        width: 7,
                        height: 7,
                    },
                    can_be_rotated: true,
                    can_be_flipped: true,
                    name: EntityName("rocket_silo".to_string()),
                    display_name: "Rocket Silo".to_string(),
                    placement_rules: PlacementRules::no_restriction(),
                },

                default_recipe: None,
            },
        ],
        chests: vec![ChestInfo {
            entity_info: api::entity::EntityInfo {
                size: Extent::single_tile(),
                can_be_rotated: false,
                can_be_flipped: false,
                name: EntityName("wooden_chest".to_string()),
                display_name: "Wooden Chest".to_string(),
                placement_rules: PlacementRules::no_restriction(),
            },
        }],
        belts: vec![BeltInfo {
            entity_info: api::entity::EntityInfo {
                size: Extent::single_tile(),
                can_be_rotated: true,
                can_be_flipped: false,
                name: EntityName("fast_transport_belt".to_string()),
                display_name: "Fast Transport Belt".to_string(),
                placement_rules: PlacementRules::no_restriction(),
            },
        }],

        items: vec![
            api::item::ItemInfo {
                name: ItemName("iron_ore".into()),
                stack_size: 50,
            },
            api::item::ItemInfo {
                name: ItemName("iron_plate".into()),
                stack_size: 100,
            },
            api::item::ItemInfo {
                name: ItemName("copper_ore".into()),
                stack_size: 50,
            },
            api::item::ItemInfo {
                name: ItemName("copper_plate".into()),
                stack_size: 100,
            },
            api::item::ItemInfo {
                name: ItemName("copper_wire".into()),
                stack_size: 200,
            },
            api::item::ItemInfo {
                name: ItemName("green_chip".into()),
                stack_size: 200,
            },
        ],
        recipes: vec![
            api::recipe::RecipeInfo {
                name: RecipeName("generate_iron".into()),
                ingredients: HashMap::new(),
                results: HashMap::from_iter([(
                    ItemIdentifier::new(&mod_ident, &ItemName("iron_ore".into())),
                    1,
                )]),
            },
            api::recipe::RecipeInfo {
                name: RecipeName("smelt_iron".into()),
                ingredients: HashMap::from_iter([(
                    ItemIdentifier::new(&mod_ident, &ItemName("iron_ore".into())),
                    1,
                )]),
                results: HashMap::from_iter([(
                    ItemIdentifier::new(&mod_ident, &ItemName("iron_plate".into())),
                    1,
                )]),
            },
            api::recipe::RecipeInfo {
                name: RecipeName("generate_copper".into()),
                ingredients: HashMap::new(),
                results: HashMap::from_iter([(
                    ItemIdentifier::new(&mod_ident, &ItemName("copper_ore".into())),
                    1,
                )]),
            },
            api::recipe::RecipeInfo {
                name: RecipeName("smelt_copper".into()),
                ingredients: HashMap::from_iter([(
                    ItemIdentifier::new(&mod_ident, &ItemName("copper_ore".into())),
                    1,
                )]),
                results: HashMap::from_iter([(
                    ItemIdentifier::new(&mod_ident, &ItemName("copper_plate".into())),
                    1,
                )]),
            },
            api::recipe::RecipeInfo {
                name: RecipeName("copper_wire".into()),
                ingredients: HashMap::from_iter([(
                    ItemIdentifier::new(&mod_ident, &ItemName("copper_plate".into())),
                    1,
                )]),
                results: HashMap::from_iter([(
                    ItemIdentifier::new(&mod_ident, &ItemName("copper_wire".into())),
                    2,
                )]),
            },
            api::recipe::RecipeInfo {
                name: RecipeName("green_chip".into()),
                ingredients: HashMap::from_iter([
                    (
                        ItemIdentifier::new(&mod_ident, &ItemName("copper_wire".into())),
                        3,
                    ),
                    (
                        ItemIdentifier::new(&mod_ident, &ItemName("iron_plate".into())),
                        1,
                    ),
                ]),
                results: HashMap::from_iter([(
                    ItemIdentifier::new(&mod_ident, &ItemName("green_chip".into())),
                    1,
                )]),
            },
        ],
    }])
});

/// # Safety
/// The caller is responsible that no reads are currently happening
/// and that no references to the `DATA_STORE` are currently live.
/// This is easiest to ensure by stopping any active update loops (by stopping simulations or the running game)
// NOTE(BSC): This function may never be called in unit tests, since those are inherently parallel and WILL race!
#[cfg(not(test))]
pub unsafe fn set_data(_data_store: DataStore) {
    todo!()
}

#[must_use]
pub fn max_entity_size() -> u32 {
    DATA_STORE
        .entities
        .iter()
        .map(|entity| max(entity.size.width, entity.size.height))
        .max()
        .expect("At least one entity must exist")
}

#[must_use]
pub fn get_kind(entity_ty: GlobalTy) -> EntityPrototypeKind {
    DATA_STORE.entities[usize::from(entity_ty)].kind
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, serde::Deserialize)]
pub enum EntityPrototypeKind {
    Assembler,
    Inserter,
    Belt,
    UndergroundBelt,
    Splitter,
    Chest,
    PowerPole,
    Pipe, // Pipe, Underground pipes and fluid tanks are the same thing
    SolarPanel,
    Accumulator,
    Beacon,
}

impl EntityPrototypeKind {
    fn global_index_for_kind_index(self, kind_index: usize) -> Option<usize> {
        DATA_STORE
            .entities
            .iter()
            .enumerate()
            .filter(|(_, e)| e.kind == self)
            .nth(kind_index)
            .map(|v| v.0)
    }

    fn kind_index_from_global_index(self, global_index: usize) -> Option<usize> {
        if DATA_STORE.entities[global_index].kind == self {
            Some(
                DATA_STORE.entities[0..global_index]
                    .iter()
                    .filter(|e| e.kind == self)
                    .count(),
            )
        } else {
            None
        }
    }
}

#[cfg(test)]
mod tests {}
