#![feature(never_type)]

use std::{cmp::max, sync::LazyLock};

use crate::{
    entity::{GlobalTy, PlacementRules},
    spacial::{Extent, Rotation},
};

pub const TICKS_PER_SECOND_LOGIC: usize = 60;
#[expect(clippy::cast_precision_loss)]
pub const TICKS_PER_SECOND_LOGIC_F32: f32 = TICKS_PER_SECOND_LOGIC as f32;

pub mod api;
pub mod energy;
pub mod entity;
pub mod spacial;

#[derive(Debug, serde::Deserialize)]
struct EntityInfo {
    // TODO: Add bounding box types
    size: Extent,

    #[serde(default)]
    can_be_rotated: bool,
    #[serde(default)]
    can_be_flipped: bool,

    kind: EntityPrototypeKind,

    name: String,
    // FIXME(BSC): localisation support!
    display_name: String,
    // TODO: Icon, Collision, Sound, Placement (i.e. which item places it), mapcolor
    placement_rules: PlacementRules,
}

#[derive(Debug)]
pub struct DataStore {
    entities: Vec<EntityInfo>,
}

/// The parsed data of the currently loaded mod set
static DATA_STORE: LazyLock<DataStore> = LazyLock::new(|| DataStore {
    entities: vec![EntityInfo {
        size: Extent {
            width: 3,
            height: 3,
        },
        can_be_rotated: true,
        can_be_flipped: true,
        kind: EntityPrototypeKind::Assembler,
        name: "factory_game::assembling_machine_1".to_string(),
        display_name: "Assembling Machine 1".to_string(),
        placement_rules: PlacementRules::no_restriction(),
    }],
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

#[derive(Debug, Clone, Copy, serde::Deserialize)]
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
}

#[cfg(test)]
mod tests {}
