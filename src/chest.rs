use std::cmp::max;
use std::{cmp::min, u8};

use rayon::iter::IndexedParallelIterator;
use rayon::iter::{IntoParallelRefMutIterator, ParallelIterator};

use crate::{
    data::DataStore,
    item::{ITEMCOUNTTYPE, IdxTrait, Item, WeakIdxTrait, usize_from},
};

const CHEST_GOAL_AMOUNT: ITEMCOUNTTYPE = ITEMCOUNTTYPE::MAX / 2;

// TODO: Add specilised chests for different sizes
pub type ChestSize = u32;
pub type SignedChestSize = i32;

#[derive(Debug, Clone, serde::Deserialize, serde::Serialize)]
pub struct FullChestStore<ItemIdxType: WeakIdxTrait> {
    pub stores: Box<[MultiChestStore<ItemIdxType>]>,
}

impl<ItemIdxType: IdxTrait> FullChestStore<ItemIdxType> {
    #[profiling::function]
    pub fn update<RecipeIdxType: IdxTrait>(
        &mut self,
        data_store: &DataStore<ItemIdxType, RecipeIdxType>,
    ) {
        self.stores
            .par_iter_mut()
            .enumerate()
            .for_each(|(item_id, store)| {
                profiling::scope!(
                    "Chest Update",
                    format!("Item: {}", data_store.item_display_names[item_id]).as_str()
                );
                store.update_simd()
            });
    }
}

#[derive(Debug, Clone, serde::Deserialize, serde::Serialize)]
pub struct MultiChestStore<ItemIdxType: WeakIdxTrait> {
    item: Item<ItemIdxType>,
    max_insert: Vec<ITEMCOUNTTYPE>,
    pub inout: Vec<ITEMCOUNTTYPE>,
    storage: Vec<ChestSize>,
    // TODO: Any way to not have to store this a billion times?
    max_items: Vec<ChestSize>,
    holes: Vec<usize>,
}

impl<ItemIdxType: IdxTrait> MultiChestStore<ItemIdxType> {
    #[must_use]
    pub fn new(item: Item<ItemIdxType>) -> Self {
        Self {
            item,
            inout: vec![],
            storage: vec![],
            max_insert: vec![],
            max_items: vec![],
            holes: vec![],
        }
    }

    pub fn add_chest<RecipeIdxType: IdxTrait>(
        &mut self,
        ty: u8,
        slot_limit: u8,
        data_store: &DataStore<ItemIdxType, RecipeIdxType>,
    ) -> u32 {
        let stack_size = data_store.item_stack_sizes[usize_from(self.item.id)];
        assert!(slot_limit <= data_store.chest_num_slots[usize::from(ty)]);
        let num_stacks = slot_limit;
        let max_items = ChestSize::from(stack_size) * ChestSize::from(num_stacks);

        if let Some(hole) = self.holes.pop() {
            self.inout[hole] = 0;
            self.storage[hole] = 0;
            self.max_insert[hole] = max_items.try_into().unwrap_or(ITEMCOUNTTYPE::MAX);

            self.max_items[hole] = max_items.saturating_sub(ChestSize::from(ITEMCOUNTTYPE::MAX));
            hole.try_into().unwrap()
        } else {
            self.inout.push(0);
            self.storage.push(0);
            self.max_insert
                .push(max_items.try_into().unwrap_or(ITEMCOUNTTYPE::MAX));

            self.max_items
                .push(max_items.saturating_sub(ChestSize::from(ITEMCOUNTTYPE::MAX)));
            (self.inout.len() - 1).try_into().unwrap()
        }
    }

    pub fn add_custom_chest(&mut self, max_items: ChestSize) -> u32 {
        if let Some(hole) = self.holes.pop() {
            self.inout[hole] = 0;
            self.storage[hole] = 0;
            self.max_insert[hole] = max_items.try_into().unwrap_or(ITEMCOUNTTYPE::MAX);

            self.max_items[hole] = max_items.saturating_sub(ChestSize::from(ITEMCOUNTTYPE::MAX));
            hole.try_into().unwrap()
        } else {
            self.inout.push(0);
            self.storage.push(0);
            self.max_insert
                .push(max_items.try_into().unwrap_or(ITEMCOUNTTYPE::MAX));

            self.max_items
                .push(max_items.saturating_sub(ChestSize::from(ITEMCOUNTTYPE::MAX)));
            (self.inout.len() - 1).try_into().unwrap()
        }
    }

    pub fn remove_chest(&mut self, index: u32) -> ChestSize {
        let index = index as usize;
        self.holes.push(index);

        let items = ChestSize::from(self.inout[index]) + self.storage[index];
        self.inout[index] = 0;
        self.storage[index] = 0;
        self.max_items[index] = 0;
        items
    }

    pub fn get_chest(&self, index: u32) -> (ChestSize, ChestSize) {
        (
            self.storage[index as usize] + ChestSize::from(self.inout[index as usize]),
            self.max_items[index as usize] + ChestSize::from(self.max_insert[index as usize]),
        )
    }

    pub fn update_naive(&mut self) {
        for (inout, (storage, max_items)) in self
            .inout
            .iter_mut()
            .zip(self.storage.iter_mut().zip(self.max_items.iter().copied()))
        {
            let to_move = inout.abs_diff(CHEST_GOAL_AMOUNT);

            if *inout >= CHEST_GOAL_AMOUNT {
                let moved: ITEMCOUNTTYPE = min(ChestSize::from(to_move), max_items - *storage)
                    .try_into()
                    .expect("since to_move was a ITEMCOUNTTYPE, this always fits");
                *inout -= moved;
                *storage += ChestSize::from(to_move);

                debug_assert!(*storage <= max_items);
            } else {
                let moved: ITEMCOUNTTYPE = min(ChestSize::from(to_move), *storage)
                    .try_into()
                    .expect("since to_move was a ITEMCOUNTTYPE, this always fits");
                *inout += moved;
                *storage -= ChestSize::from(to_move);
            }
        }
    }

    pub fn update_simd(&mut self) {
        for (inout, (storage, max_items)) in self
            .inout
            .iter_mut()
            .zip(self.storage.iter_mut().zip(self.max_items.iter().copied()))
        {
            let to_move = inout.abs_diff(CHEST_GOAL_AMOUNT);

            let switch = ChestSize::from(*inout >= CHEST_GOAL_AMOUNT);

            let moved: SignedChestSize = (switch as SignedChestSize
                + (1 - switch as SignedChestSize) * -1)
                * (min(
                    ChestSize::from(to_move),
                    (max_items - *storage) * switch + (1 - switch) * *storage,
                ) as SignedChestSize);

            *inout = (ChestSize::from(*inout)).wrapping_sub_signed(moved) as u8;
            *storage = (*storage).wrapping_add_signed(moved) as ChestSize;

            debug_assert!(*storage <= max_items);
        }
    }

    pub fn add_items_to_chest(
        &mut self,
        index: u32,
        new_items: ChestSize,
    ) -> Result<(), ChestSize> {
        let index = index as usize;

        let storage_size = self.max_items[index] + ChestSize::from(self.max_insert[index]);
        let current_items = self.inout[index] as ChestSize + self.storage[index];

        if current_items + new_items > storage_size {
            let not_inserted = (current_items + new_items) - storage_size;
            self.storage[index] = self.max_items[index];
            self.inout[index] = self.max_insert[index];

            Err(not_inserted)
        } else {
            let free_in_storage = self.max_items[index] - self.storage[index];

            if new_items > free_in_storage {
                self.storage[index] = self.max_items[index];
                self.inout[index] = self.inout[index]
                    .checked_add(u8::try_from(new_items - free_in_storage).unwrap())
                    .unwrap();
                assert!(self.inout[index] <= self.max_insert[index]);
            } else {
                self.storage[index] += new_items;
            }

            Ok(())
        }
    }

    pub fn remove_items_from_chest(
        &mut self,
        index: u32,
        to_remove: ChestSize,
    ) -> Result<(), ChestSize> {
        let index = index as usize;

        let current_items = self.inout[index] as ChestSize + self.storage[index];

        if current_items >= to_remove {
            if self.storage[index] >= to_remove {
                self.storage[index] -= to_remove;
            } else {
                self.inout[index] -= u8::try_from(to_remove - self.storage[index]).unwrap();
                self.storage[index] = 0;
            }

            Ok(())
        } else {
            let missing = to_remove - current_items;
            self.storage[index] = 0;
            self.inout[index] = 0;

            Err(missing)
        }
    }

    /// Returns the number of items no longer part of the box
    pub fn change_chest_size(&mut self, index: u32, new_size: ChestSize) -> ChestSize {
        let index = index as usize;

        let removed_items =
            if new_size < max(self.max_items[index], self.max_insert[index] as ChestSize) {
                let current_items = self.inout[index] as ChestSize + self.storage[index];

                if current_items > new_size {
                    let items_to_remove = current_items - new_size;

                    if self.storage[index] >= items_to_remove {
                        self.storage[index] -= items_to_remove;
                    } else {
                        self.inout[index] = self.inout[index]
                            .checked_sub(
                                items_to_remove
                                    .checked_sub(self.storage[index])
                                    .unwrap()
                                    .try_into()
                                    .unwrap(),
                            )
                            .unwrap();
                        self.storage[index] = 0;
                    }

                    items_to_remove
                } else {
                    0
                }
            } else {
                0
            };

        self.max_items[index] = new_size.saturating_sub(ChestSize::from(ITEMCOUNTTYPE::MAX));
        self.max_insert[index] = new_size.try_into().unwrap_or(ITEMCOUNTTYPE::MAX);

        removed_items
    }

    pub fn storage_list_slices(&mut self) -> (&[ITEMCOUNTTYPE], &mut [ITEMCOUNTTYPE]) {
        (self.max_insert.as_slice(), self.inout.as_mut_slice())
    }
}

#[cfg(test)]
mod test {
    use std::cmp::min;

    use proptest::prelude::Just;
    use proptest::prelude::Strategy;
    use proptest::prop_assert_eq;
    use proptest::proptest;

    use crate::chest::CHEST_GOAL_AMOUNT;
    use crate::chest::ChestSize;
    use crate::chest::ITEMCOUNTTYPE;
    use crate::chest::SignedChestSize;

    fn max_items_and_storage() -> impl Strategy<Value = (ChestSize, ChestSize)> {
        (0..ChestSize::MAX).prop_flat_map(|max_items| (Just(max_items), 0..max_items))
    }

    proptest! {


        #[test]
        fn simd_always_same_as_naive(inout in 0..ITEMCOUNTTYPE::MAX, (max_items, storage) in max_items_and_storage()) {
            let to_move = inout.abs_diff(CHEST_GOAL_AMOUNT);

            let mut storage_naive = storage;
            let mut inout_naive = inout;

            if inout_naive >= CHEST_GOAL_AMOUNT {
                let moved: ITEMCOUNTTYPE = min(ChestSize::from(to_move), max_items - storage)
                    .try_into()
                    .expect("since to_move was a ITEMCOUNTTYPE, this always fits");
                inout_naive -= moved;
                storage_naive += ChestSize::from(moved);

                debug_assert!(storage_naive <= max_items);
            } else {
                let moved: ITEMCOUNTTYPE = min(ChestSize::from(to_move), storage)
                    .try_into()
                    .expect("since to_move was a ITEMCOUNTTYPE, this always fits");
                inout_naive += moved;
                storage_naive -= ChestSize::from(moved);
            }

            let mut storage_simd = storage;
            let mut inout_simd = inout;

            let switch = ChestSize::from(inout_simd >= CHEST_GOAL_AMOUNT);

            let moved: SignedChestSize = (switch as SignedChestSize + (1 - switch as SignedChestSize) * -1)
                * (min(
                    to_move as ChestSize,
                    (max_items - storage_simd) * switch + (1 - switch) * storage_simd,
                ) as SignedChestSize);

            inout_simd = (inout_simd as ChestSize).checked_sub_signed(moved).unwrap() as u8;
            storage_simd = (storage_simd as ChestSize).checked_add_signed(moved).unwrap();

            debug_assert!(storage_simd <= max_items);

            prop_assert_eq!(inout_naive, inout_simd, "inout");
            prop_assert_eq!(storage_naive, storage_simd, "storage");
        }

    }
}
