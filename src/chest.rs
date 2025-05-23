use std::{cmp::min, u8};

use rayon::iter::{IntoParallelRefMutIterator, ParallelIterator};

use crate::{
    data::DataStore,
    item::{usize_from, IdxTrait, Item, WeakIdxTrait, ITEMCOUNTTYPE},
};

const CHEST_GOAL_AMOUNT: ITEMCOUNTTYPE = ITEMCOUNTTYPE::MAX / 2;

const MAX_INSERT_AMOUNT: &'static [ITEMCOUNTTYPE] = &[ITEMCOUNTTYPE::MAX; 10_000_000];

#[derive(Debug, Clone, serde::Deserialize, serde::Serialize)]
pub struct FullChestStore<ItemIdxType: WeakIdxTrait> {
    pub stores: Box<[MultiChestStore<ItemIdxType>]>,
}

impl<ItemIdxType: IdxTrait> FullChestStore<ItemIdxType> {
    pub fn update(&mut self) {
        self.stores.par_iter_mut().for_each(|store| store.update());
    }
}

#[derive(Debug, Clone, serde::Deserialize, serde::Serialize)]
pub struct MultiChestStore<ItemIdxType: WeakIdxTrait> {
    item: Item<ItemIdxType>,
    max_insert: Option<Vec<ITEMCOUNTTYPE>>,
    pub inout: Vec<ITEMCOUNTTYPE>,
    storage: Vec<u16>,
    // TODO: Any way to not have to store this a billion times?
    max_items: Vec<u16>,
    holes: Vec<usize>,
}

impl<ItemIdxType: IdxTrait> MultiChestStore<ItemIdxType> {
    pub fn new<RecipeIdxType: IdxTrait>(
        item: Item<ItemIdxType>,
        data_store: &DataStore<ItemIdxType, RecipeIdxType>,
    ) -> Self {
        Self {
            item,
            inout: vec![],
            storage: vec![],
            max_insert: None,
            max_items: vec![],
            holes: vec![],
        }
    }

    pub fn add_chest<RecipeIdxType: IdxTrait>(
        &mut self,
        ty: u8,
        data_store: &DataStore<ItemIdxType, RecipeIdxType>,
    ) -> usize {
        let stack_size = data_store.item_stack_sizes[usize_from(self.item.id)];
        let num_stacks = data_store.chest_num_slots[usize::from(ty)];
        let max_items = u16::from(stack_size) * u16::from(num_stacks);

        if max_items < u16::from(ITEMCOUNTTYPE::MAX) {
            if self.max_insert.is_none() {
                self.max_insert = Some(vec![ITEMCOUNTTYPE::MAX; self.max_items.len()]);
            }
        }

        if let Some(hole) = self.holes.pop() {
            self.inout[hole] = 0;
            self.storage[hole] = 0;
            if let Ok(max_insert) = max_items.try_into() {
                self.max_insert.as_mut().unwrap()[hole] = max_insert;
            }

            self.max_items[hole] = max_items.saturating_sub(u16::from(ITEMCOUNTTYPE::MAX));
            hole
        } else {
            self.inout.push(0);
            self.storage.push(0);
            if let Ok(max_insert) = max_items.try_into() {
                self.max_insert.as_mut().unwrap().push(max_insert);
            }

            self.max_items
                .push(max_items.saturating_sub(u16::from(ITEMCOUNTTYPE::MAX)));
            self.inout.len() - 1
        }
    }

    pub fn remove_chest<RecipeIdxType: IdxTrait>(
        &mut self,
        index: usize,
        data_store: &DataStore<ItemIdxType, RecipeIdxType>,
    ) -> u16 {
        self.holes.push(index);

        let items = self.inout[index] as u16 + self.storage[index];
        self.inout[index] = 0;
        self.storage[index] = 0;
        self.storage[index] = 0;
        items
    }

    pub fn get_chest(&self, index: usize) -> (u16, u16) {
        (
            self.storage[index] + u16::from(self.inout[index]),
            self.max_items[index],
        )
    }

    pub fn update(&mut self) {
        for (inout, (storage, max_items)) in self
            .inout
            .iter_mut()
            .zip(self.storage.iter_mut().zip(self.max_items.iter().copied()))
        {
            let to_move = inout.abs_diff(CHEST_GOAL_AMOUNT);

            if *inout >= CHEST_GOAL_AMOUNT {
                let moved: ITEMCOUNTTYPE = min(to_move as u16, max_items - *storage)
                    .try_into()
                    .expect("since to_move was a ITEMCOUNTTYPE, this always fits");
                *inout -= moved;
                *storage += moved as u16;

                debug_assert!(*storage <= max_items);
            } else {
                let moved: ITEMCOUNTTYPE = min(to_move as u16, *storage)
                    .try_into()
                    .expect("since to_move was a ITEMCOUNTTYPE, this always fits");
                *inout += moved;
                *storage -= moved as u16;
            }
        }
    }

    pub fn storage_list_slices(&mut self) -> (&[ITEMCOUNTTYPE], &mut [ITEMCOUNTTYPE]) {
        (
            self.max_insert
                .as_ref()
                .map(|v| v.as_slice())
                .unwrap_or(MAX_INSERT_AMOUNT),
            self.inout.as_mut_slice(),
        )
    }
}
