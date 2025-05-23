use std::marker::PhantomData;

use crate::{
    data::DataStore,
    item::{IdxTrait, Item, Recipe, WeakIdxTrait},
    power::power_grid::PowerGridIdentifier,
};

use static_assertions::const_assert;
use strum::EnumIter;

pub mod belt_belt_inserter;
pub mod belt_storage_inserter;
pub mod storage_storage_inserter;

/// Time for a normal inserter to move in ticks
pub(super) const MOVETIME: u8 = 60;
const_assert!(MOVETIME < 64);

// TODO: This could be minified using a union or similar,
// But since Inserters are the same size, whether this is 2 or 1 byte (atleast in a Vec of Structs)
// I will leave this be for now.
#[derive(Debug, Clone, Copy, serde::Deserialize, serde::Serialize)]
pub enum InserterState {
    Empty,
    FullAndWaitingForSlot,
    FullAndMovingOut(u8),
    EmptyAndMovingBack(u8),
}

#[derive(Debug, Clone, serde::Deserialize, serde::Serialize)]
enum SushiInserterState<ItemIdxType: WeakIdxTrait> {
    Empty,
    FullAndWaitingForSlot(Item<ItemIdxType>),
    FullAndMovingOut(u8, Item<ItemIdxType>),
    EmptyAndMovingBack(u8),
}

// TODO: We still need to store the timer and hand fullness. For the timer, 5 bits should suffice
//       I don't think we need to store the recipe_id in 8 bits, since 32 recipes should be max for any resource (so 5 bits)
// Current Plan: htttttrrrrrsssssssssssssssssssss
//               grid_id recipe_id storage_id
// IDEA: Maybe switch the bits dynamically from storage_id to grid_id when the number of grids increases.
// That way we support both large single grid bases and bases with many grids (though maybe not megabases with independent outposts and zero beacons)
// TODO: This is less efficient, but should be fine for now
// TODO: maybe do this differently

// TODO: Since I collect the "all storage" list anyway I should be able to flatten it?
#[derive(Debug, Clone, Copy, PartialEq, Eq, serde::Deserialize, serde::Serialize)]
pub struct StorageID<RecipeIdxType: WeakIdxTrait> {
    pub grid: PowerGridIdentifier,
    pub storage_list_idx: u16,
    pub machine_idx: u16,

    // TODO: Do i want to make this generic?
    pub phantom: PhantomData<RecipeIdxType>,
}

#[derive(
    Debug, Clone, Copy, serde::Deserialize, serde::Serialize, PartialEq, Eq, Hash, PartialOrd, Ord,
)]
pub enum Storage<RecipeIdxType: WeakIdxTrait> {
    Assembler {
        grid: PowerGridIdentifier,
        recipe_idx_with_this_item: RecipeIdxType,
        // TODO:
        index: u16,
    },
    Lab {
        grid: PowerGridIdentifier,
        index: u16,
    },
    Static {
        index: usize,
        static_id: StaticID,
    },
}

impl<RecipeIdxType: IdxTrait> Storage<RecipeIdxType> {
    pub fn translate<ItemIdxType: IdxTrait>(
        self,
        item: Item<ItemIdxType>,
        data_store: &DataStore<ItemIdxType, RecipeIdxType>,
    ) -> Self {
        match self {
            Storage::Assembler {
                grid,
                recipe_idx_with_this_item,
                index,
            } => Storage::Assembler {
                grid,
                recipe_idx_with_this_item: data_store.recipe_to_translated_index[&(
                    Recipe {
                        id: recipe_idx_with_this_item,
                    },
                    item,
                )],
                index,
            },
            storage => storage,
        }
    }

    pub fn change_grid(self, new_id: PowerGridIdentifier) -> Self {
        match self {
            Storage::Assembler {
                grid: _,
                recipe_idx_with_this_item,
                index,
            } => Storage::Assembler {
                grid: new_id,
                recipe_idx_with_this_item,
                index,
            },
            Storage::Lab { grid: _, index } => Storage::Lab {
                grid: new_id,
                index,
            },
            Storage::Static { static_id, index } => Storage::Static { static_id, index },
        }
    }
}

#[derive(
    Debug,
    Clone,
    Copy,
    serde::Deserialize,
    serde::Serialize,
    PartialEq,
    Eq,
    Hash,
    PartialOrd,
    Ord,
    EnumIter,
)]
#[repr(u8)]
pub enum StaticID {
    Chest = 0,
}
