use std::{
    marker::PhantomData,
    sync::{atomic::Ordering, Weak},
};

use crate::item::{ItemStorageStrict, ItemTrait};

use super::{
    belt::StrictBeltStorage,
    optimized::{OptimizedBelt, OptimizedBeltStorage},
};

#[derive(Debug)]
pub struct OutInserterStrict<T: ItemTrait> {
    marker: PhantomData<T>,
    pub connected_count: Weak<ItemStorageStrict<T>>,
    pub belt_pos: u32,
}

impl<T: ItemTrait> OutInserterStrict<T> {
    pub fn create_and_add(
        storage: Weak<ItemStorageStrict<T>>,
        belt: &mut OptimizedBelt<T>,
        pos: u32,
    ) {
        let ins = Self {
            marker: PhantomData,
            connected_count: storage,
            belt_pos: pos,
        };

        belt.add_out_inserter(ins);
    }

    pub(super) fn update_belt(&mut self, belt_storage: &mut StrictBeltStorage<T>) {
        let connected_storage_option = self.connected_count.upgrade();

        if let Some(storage) = connected_storage_option {
            if storage.count.load(Ordering::Relaxed) > 0
                && belt_storage.try_put_item_in_pos(self.belt_pos)
            {
                storage.count.fetch_sub(1, Ordering::Relaxed);
            }
        }
    }

    pub(super) fn update_optimized_belt(&mut self, belt_storage: &mut OptimizedBeltStorage<T>) {
        let connected_storage_option = self.connected_count.upgrade();

        if let Some(storage) = connected_storage_option {
            if storage.count.load(Ordering::Relaxed) > 0
                && belt_storage.try_put_item_in_pos(self.belt_pos)
            {
                storage.count.fetch_sub(1, Ordering::Relaxed);
            }
        }
    }
}
