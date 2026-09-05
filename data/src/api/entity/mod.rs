use crate::{EntityName, entity::PlacementRules, spacial::Extent};

pub mod assembler;
pub mod belt;
pub mod chest;
pub mod inserter;
pub mod power_pole;

#[derive(Debug, serde::Deserialize)]
pub struct EntityInfo {
    // TODO: Add bounding box types
    pub size: Extent,

    #[serde(default)]
    pub can_be_rotated: bool,
    #[serde(default)]
    pub can_be_flipped: bool,

    pub name: EntityName,
    // FIXME(BSC): localisation support!
    pub display_name: String,
    pub placement_rules: PlacementRules,
}
