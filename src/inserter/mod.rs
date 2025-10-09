use std::{marker::PhantomData, u16};

use crate::item::{ITEMCOUNTTYPE, Indexable};
use crate::{
    data::DataStore,
    item::{IdxTrait, Item, Recipe, WeakIdxTrait},
    power::power_grid::PowerGridIdentifier,
    storage_list::{grid_size, static_size},
};

use static_assertions::const_assert;
use storage_storage_with_buckets::LargeInserterState;
use strum::EnumIter;

use std::cmp::min;

use crate::item::usize_from;

#[cfg(feature = "client")]
use egui_show_info::{EguiDisplayable, InfoExtractor, ShowInfo};
#[cfg(feature = "client")]
use egui_show_info_derive::ShowInfo;
#[cfg(feature = "client")]
use get_size::GetSize;

pub mod belt_belt_inserter;
pub mod belt_storage_inserter;
pub mod belt_storage_inserter_non_const_gen;
pub mod belt_storage_pure_buckets;
pub mod storage_storage_inserter;
pub mod storage_storage_with_buckets;
// pub mod storage_storage_with_buckets_compressed;
pub mod storage_storage_with_buckets_indirect;

mod bucket_bit_compressed;

// TODO: This could be minified using a union or similar,
// But since Inserters are the same size, whether this is 2 or 1 byte (atleast in a Vec of Structs)
// I will leave this be for now.
#[cfg_attr(feature = "client", derive(ShowInfo), derive(GetSize))]
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, serde::Deserialize, serde::Serialize)]
pub enum InserterState {
    WaitingForSourceItems(ITEMCOUNTTYPE),
    WaitingForSpaceInDestination(ITEMCOUNTTYPE),
    // FIXME: Not storing the amount of items in hand, and instead relying on the handsize will result in spawning items from the void, if a new tech increases hand size limit!
    FullAndMovingOut(u8),
    EmptyAndMovingBack(u8),
}

#[cfg_attr(feature = "client", derive(ShowInfo), derive(GetSize))]
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
#[cfg_attr(feature = "client", derive(ShowInfo), derive(GetSize))]
#[derive(Debug, Clone, Copy, PartialEq, Eq, serde::Deserialize, serde::Serialize)]
pub struct StorageID<RecipeIdxType: WeakIdxTrait> {
    pub grid: PowerGridIdentifier,
    pub storage_list_idx: u16,
    pub machine_idx: u16,

    // TODO: Do i want to make this generic?
    pub phantom: PhantomData<RecipeIdxType>,
}

// The power grid id u16::MAX is reserved for static inventories
pub const MAX_GRID_COUNT: usize = u16::MAX as usize - 1;
pub const MAX_TIMES_AN_ITEM_CAN_APPEAR_IN_RECIPES: usize = u16::MAX as usize;
pub const MAX_RECIPE_COUNT: usize = u16::MAX as usize;

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, serde::Deserialize, serde::Serialize)]
#[repr(packed(1))]
pub struct FakeUnionStorage {
    pub index: u32,
    pub grid_or_static_flag: u16,
    pub recipe_idx_with_this_item: u16,
}

pub struct InserterStateInfo {
    pub source: FakeUnionStorage,
    pub dest: FakeUnionStorage,
    pub state: LargeInserterState,
}

#[cfg(feature = "client")]
impl<E: InfoExtractor<Self, Info>, Info: EguiDisplayable> ShowInfo<E, Info> for FakeUnionStorage {
    fn show_fields<C: egui_show_info::Cache<String, Info>>(
        &self,
        _: &mut E,
        _: &mut egui::Ui,
        _: std::string::String,
        _: &mut C,
    ) {
        // TODO:
    }
}

#[cfg(feature = "client")]
impl GetSize for FakeUnionStorage {}

impl FakeUnionStorage {
    #[inline(always)]
    pub fn into_inner_and_outer_indices(
        self,
        num_grids_total: usize,
        grid_size: usize,
    ) -> (usize, usize) {
        debug_assert!(
            num_grids_total < u16::MAX as usize,
            "If we have u16::MAX power grids, we can no longer differentiate grids in that grid and static inventories independent of power grids"
        );

        let grid_offs = min(num_grids_total, usize::from(self.grid_or_static_flag));
        let recipe_idx_with_this_item_or_single_kind_power_grid_kind =
            usize::from(self.recipe_idx_with_this_item);

        (
            grid_offs * grid_size + recipe_idx_with_this_item_or_single_kind_power_grid_kind,
            self.index as usize,
        )
    }

    #[inline(always)]
    pub fn into_inner_and_outer_indices_with_statics_at_zero(
        self,
        grid_size: usize,
    ) -> (usize, usize) {
        let grid_offs = usize::from(self.grid_or_static_flag);
        let recipe_idx_with_this_item_or_single_kind_power_grid_kind =
            usize::from(self.recipe_idx_with_this_item);

        (
            grid_offs * grid_size + recipe_idx_with_this_item_or_single_kind_power_grid_kind,
            self.index as usize,
        )
    }

    pub fn from_storage<ItemIdxType: IdxTrait, RecipeIdxType: IdxTrait>(
        item: Item<ItemIdxType>,
        storage: Storage<RecipeIdxType>,
        data_store: &DataStore<ItemIdxType, RecipeIdxType>,
    ) -> Self {
        match storage {
            Storage::Assembler {
                grid,
                recipe_idx_with_this_item,
                index,
            } => Self {
                index: u32::from(index),
                grid_or_static_flag: u16::from(grid),
                recipe_idx_with_this_item: u16::try_from(Into::<usize>::into(
                    recipe_idx_with_this_item,
                ))
                .unwrap(),
            },
            Storage::Lab { grid, index } => Self {
                index: u32::from(index),
                grid_or_static_flag: u16::from(grid),
                recipe_idx_with_this_item: data_store.num_recipes_with_item[usize_from(item.id)]
                    as u16,
            },
            Storage::Static { index, static_id } => Self {
                index: u32::try_from(index).unwrap(),
                grid_or_static_flag: u16::MAX,
                recipe_idx_with_this_item: static_id as u16,
            },
        }
    }

    pub fn from_storage_with_statics_at_zero<ItemIdxType: IdxTrait, RecipeIdxType: IdxTrait>(
        item: Item<ItemIdxType>,
        storage: Storage<RecipeIdxType>,
        data_store: &DataStore<ItemIdxType, RecipeIdxType>,
    ) -> Self {
        let grid_size: usize = grid_size(item, data_store);
        let static_size: usize = static_size(item, data_store);

        let grid_offset = static_size.div_ceil(grid_size);

        match storage {
            Storage::Assembler {
                grid,
                recipe_idx_with_this_item,
                index,
            } => {
                assert!(
                    recipe_idx_with_this_item.into_usize()
                        < data_store.num_recipes_with_item[item.into_usize()]
                );
                Self {
                    index: u32::from(index),
                    grid_or_static_flag: u16::from(grid)
                        .checked_add(u16::try_from(grid_offset).unwrap())
                        .expect("Grid ID too high (would overflow the grid_or_static)"),
                    recipe_idx_with_this_item: u16::try_from(Into::<usize>::into(
                        recipe_idx_with_this_item,
                    ))
                    .unwrap(),
                }
            },
            Storage::Lab { grid, index } => Self {
                index: u32::from(index),
                grid_or_static_flag: u16::from(grid)
                    .checked_add(u16::try_from(grid_offset).unwrap())
                    .expect("Grid ID too high (would overflow the grid_or_static)"),
                recipe_idx_with_this_item: data_store.num_recipes_with_item[usize_from(item.id)]
                    as u16,
            },
            Storage::Static { index, static_id } => Self {
                index: u32::try_from(index).unwrap(),
                grid_or_static_flag: 0,
                recipe_idx_with_this_item: static_id,
            },
        }
    }
}

#[cfg_attr(feature = "client", derive(ShowInfo), derive(GetSize))]
#[derive(
    Debug, Clone, Copy, serde::Deserialize, serde::Serialize, PartialEq, Eq, Hash, PartialOrd, Ord,
)]
pub enum Storage<RecipeIdxType: WeakIdxTrait> {
    Assembler {
        grid: PowerGridIdentifier,
        recipe_idx_with_this_item: RecipeIdxType,
        index: u32,
    },
    Lab {
        grid: PowerGridIdentifier,
        index: u32,
    },
    Static {
        static_id: u16,
        index: u32,
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

    fn into_inner_and_outer_indices(
        self,
        num_grids_total: usize,
        num_recipes: usize,
        grid_size: usize,
    ) -> (usize, usize) {
        match self {
            Storage::Assembler {
                grid,
                recipe_idx_with_this_item,
                index,
            } => {
                debug_assert!(
                    usize_from(recipe_idx_with_this_item) < num_recipes,
                    "The recipe stored in an inserter needs to be translated!"
                );
                let outer = Into::<usize>::into(grid) * grid_size
                    + Into::<usize>::into(recipe_idx_with_this_item);
                (outer, index.try_into().unwrap())
            },
            Storage::Lab { grid, index } => {
                let outer = Into::<usize>::into(grid) * grid_size + num_recipes;
                (outer, index.try_into().unwrap())
            },
            Storage::Static { static_id, index } => {
                // debug_assert!(usize::from(static_id) < data_store.num_different_static_containers);
                let outer = num_grids_total * grid_size + Into::<usize>::into(static_id as u8);
                (outer, index.try_into().unwrap())
            },
        }
    }

    fn into_inner_and_outer_indices_with_statics_at_zero(
        self,
        num_recipes: usize,
        grid_size: usize,
        static_size: usize,
    ) -> (usize, usize) {
        let grid_offset = static_size.div_ceil(grid_size);

        match self {
            Storage::Assembler {
                grid,
                recipe_idx_with_this_item,
                index,
            } => {
                debug_assert!(
                    usize_from(recipe_idx_with_this_item) < num_recipes,
                    "The recipe stored in an inserter needs to be translated!"
                );
                let outer = (Into::<usize>::into(grid) + grid_offset) * grid_size
                    + Into::<usize>::into(recipe_idx_with_this_item);
                (outer, index.try_into().unwrap())
            },
            Storage::Lab { grid, index } => {
                let outer = (Into::<usize>::into(grid) + grid_offset) * grid_size + num_recipes;
                (outer, index.try_into().unwrap())
            },
            Storage::Static { static_id, index } => {
                // debug_assert!(usize::from(static_id) < data_store.num_different_static_containers);
                let outer = Into::<usize>::into(static_id as u8);
                (outer, index.try_into().unwrap())
            },
        }
    }
}

#[cfg_attr(feature = "client", derive(ShowInfo), derive(GetSize))]
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

#[cfg(test)]
mod test {
    use crate::DATA_STORE;
    use crate::blueprint::test::random_item;
    use crate::inserter::{FakeUnionStorage, Storage};
    use crate::item::{Item, usize_from};
    use crate::storage_list::{grid_size, static_size};
    use proptest::prelude::{Just, Strategy};
    use proptest::prop_oneof;
    use proptest::{prop_assert_eq, proptest};

    use crate::power::power_grid::PowerGridIdentifier;

    use super::StaticID;

    fn random_storage(num_grids: u16, num_recipes: u8) -> impl Strategy<Value = Storage<u8>> {
        prop_oneof![
            (0..num_grids, 0..num_recipes, 0..u32::MAX).prop_map(
                |(grid, recipe_idx_with_this_item, index)| {
                    Storage::Assembler {
                        grid: grid.try_into().unwrap_or(PowerGridIdentifier::MAX),
                        recipe_idx_with_this_item,
                        index,
                    }
                }
            ),
            (0..num_grids, 0..u32::MAX).prop_map(|(grid, index)| {
                Storage::Lab {
                    grid: grid.try_into().unwrap_or(PowerGridIdentifier::MAX),
                    index,
                }
            }),
            (random_static(), 0..u16::MAX).prop_map(|(static_id, index)| {
                Storage::Static {
                    static_id: static_id as u16,
                    index: index.into(),
                }
            })
        ]
    }

    fn random_static() -> impl Strategy<Value = StaticID> {
        Just(StaticID::Chest)
    }

    fn random_grid_count() -> impl Strategy<Value = u16> {
        1..u16::MAX
    }

    fn union_test_input() -> impl Strategy<Value = (Item<u8>, u16, Storage<u8>)> {
        (random_item(&DATA_STORE), random_grid_count()).prop_flat_map(|(item, grid_count)| {
            (
                Just(item),
                Just(grid_count),
                random_storage(
                    grid_count,
                    DATA_STORE.num_recipes_with_item[usize_from(item.id)] as u8,
                ),
            )
        })
    }

    proptest! {


        #[test]
        fn storage_and_fake_union_result_in_same_indices((item, num_grids, storage) in union_test_input()) {
            let grid_size = grid_size(item, &DATA_STORE);

            let storage_union = FakeUnionStorage::from_storage(item, storage, &DATA_STORE);

            let union_indices = storage_union.into_inner_and_outer_indices(num_grids.into(), grid_size);

            let storage_indices = storage.into_inner_and_outer_indices(num_grids.into(), DATA_STORE.num_recipes_with_item[usize_from(item.id)], grid_size);

            prop_assert_eq!(union_indices, storage_indices);
        }

        #[test]
        fn storage_and_fake_union_result_in_same_indices_with_statics_at_zero((item, _num_grids, storage) in union_test_input()) {
            let grid_size = grid_size(item, &DATA_STORE);

            let storage_union = FakeUnionStorage::from_storage_with_statics_at_zero(item, storage, &DATA_STORE);

            let union_indices = storage_union.into_inner_and_outer_indices_with_statics_at_zero(grid_size);

            let storage_indices = storage.into_inner_and_outer_indices_with_statics_at_zero(DATA_STORE.num_recipes_with_item[usize_from(item.id)], grid_size, static_size(item, &DATA_STORE));

            prop_assert_eq!(union_indices, storage_indices);
        }
    }
}
