use std::{cmp::min, u8};

use crate::{
    data::DataStore,
    item::{usize_from, IdxTrait, Item, WeakIdxTrait, ITEMCOUNTTYPE},
};

#[derive(Debug, Clone, serde::Deserialize, serde::Serialize)]
pub struct FullChestStore<ItemIdxType: WeakIdxTrait> {
    pub stores: Box<[MultiChestStore<ItemIdxType>]>,
}

#[derive(Debug, Clone, serde::Deserialize, serde::Serialize)]
pub struct MultiChestStore<ItemIdxType: WeakIdxTrait> {
    item: Item<ItemIdxType>,
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
        if let Some(hole) = self.holes.pop() {
            self.inout[hole] = 0;
            self.storage[hole] = 0;
            self.max_items[hole] = u16::from(stack_size) * u16::from(num_stacks);
            hole
        } else {
            self.inout.push(0);
            self.storage.push(0);
            self.max_items
                .push(u16::from(stack_size) * u16::from(num_stacks));
            self.inout.len() - 1
        }
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
            let to_move = inout.abs_diff(128);

            if *inout >= 128 {
                let moved: ITEMCOUNTTYPE = min(to_move as u16, *storage)
                    .try_into()
                    .expect("since to_move was a ITEMCOUNTTYPE, this always fits");
                *inout -= moved;
                *storage += moved as u16;

                debug_assert!(*storage <= max_items);
            } else {
                let moved: ITEMCOUNTTYPE = min(to_move as u16, max_items - *storage)
                    .try_into()
                    .expect("since to_move was a ITEMCOUNTTYPE, this always fits");
                *inout += moved;
                *storage -= moved as u16;
            }
        }
    }
}
