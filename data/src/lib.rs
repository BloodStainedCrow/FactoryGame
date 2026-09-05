#![feature(int_roundings)]

use std::{
    cmp::max,
    sync::{Arc, LazyLock},
};

use crate::{
    api::{
        ModData,
        entity::{
            assembler::AssemblerInfo, belt::BeltInfo, chest::ChestInfo, power_pole::PowerPoleInfo,
        },
    },
    entity::{GlobalTy, PlacementRules, power_pole::PowerPoleData},
    spacial::Extent,
};

pub const TICKS_PER_SECOND_LOGIC: usize = 60;
#[expect(clippy::cast_precision_loss)]
pub const TICKS_PER_SECOND_LOGIC_F32: f32 = TICKS_PER_SECOND_LOGIC as f32;

pub mod api;
pub mod energy;
pub mod entity;
pub mod item;
pub mod spacial;

#[derive(Debug, serde::Deserialize)]
struct ModIdentifier(String);

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct EntityIdentifier(Arc<str>);

#[derive(Debug, serde::Deserialize)]
struct EntityName(String);

impl EntityIdentifier {
    fn new(mod_: &ModIdentifier, name: &EntityName) -> Self {
        Self(dbg!(format!("{}::{}", mod_.0, name.0)).into())
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
}

/// The parsed data of the currently loaded mod set
static DATA_STORE: LazyLock<DataStore> = LazyLock::new(|| {
    DataStore::from_mods(&[ModData {
        mod_name: ModIdentifier("factory_game".to_string()),
        inserters: vec![],
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
            AssemblerInfo {
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
            },
            AssemblerInfo {
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
            },
            AssemblerInfo {
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
            },
            AssemblerInfo {
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
            },
            AssemblerInfo {
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
            },
            AssemblerInfo {
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
            },
            AssemblerInfo {
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
    }])
});

/// # Safety
/// The caller is responsible that no reads are currently happening
/// and that no references to the `DATA_STORE` are currently live.
/// This is easiest to ensure by stopping any active update loops (by stopping simulations or the running game)
#[expect(clippy::needless_pass_by_value)]
// NOTE(BSC): This function may never be called in unit tests, since those are inherently parallel and WILL race!
#[cfg(not(test))]
pub unsafe fn set_data(data_store: DataStore) {
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
