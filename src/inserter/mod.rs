use crate::{
    item::{ItemStorage, ItemTrait},
    power::PowerGridIdentifier,
};

use static_assertions::const_assert;

pub mod belt_belt_inserter;
pub mod belt_storage_inserter;
pub mod storage_storage_inserter;

/// Time for an inserter to move in ticks
const MOVETIME: u8 = 60;
const_assert!(MOVETIME < 64);

// TODO: This could be minified using a union or similar,
// But since Inserters are the same size, whether this is 2 or 1 byte (atleast in a Vec of Structs)
// I will leave this be for now.
#[derive(Debug, Clone, Copy, serde::Deserialize, serde::Serialize)]
enum InserterState {
    Empty,
    FullAndWaitingForSlot,
    FullAndMovingOut(u8),
    EmptyAndMovingBack(u8),
}

// TODO: We still need to store the timer and hand fullness. For the timer, 5 bits should suffice
//       I don't think we need to store the recipe_id in 8 bits, since 32 recipes should be max for any resource (so 5 bits)
// Current Plan: htttttrrrrrsssssssssssssssssssss
//               grid_id recipe_id storage_id
// IDEA: Maybe switch the bits dynamically from storage_id to grid_id when the number of grids increases.
// That way we support both large single grid bases and bases with many grids (though maybe not megabases with independent outposts and zero beacons)
// TODO: This is less efficient, but should be fine for now
#[derive(Debug, Clone, PartialEq, Eq, serde::Deserialize, serde::Serialize)]
pub struct StorageID {
    pub recipe: u8,
    pub grid: PowerGridIdentifier,
    pub storage: u16,
}

pub const NUM_RECIPES: usize = 1;
// TODO: Use ItemTrait::NUM_RECIPES once #![feature(generic_const_exprs)] is usable
// FIXME: I swapped power_grid and recipe here, change that when indexing!!!
//                                        power_grid recipe     assembler
pub type Storages<'a, 'b, T: ItemTrait> = &'a mut [[&'b mut [ItemStorage<T>]; NUM_RECIPES]];
