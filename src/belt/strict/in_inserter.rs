use std::sync::{atomic::Ordering, Weak};

use crate::item::{get_max_stack_size, ItemStorageStrict, ItemTrait};

use super::{belt::StrictBeltStorage, optimized::OptimizedBeltStorage};

#[derive(Debug)]
pub struct InInserterStrict<T: ItemTrait> {
    pub connected_storage: Weak<ItemStorageStrict<T>>,
    pub belt_pos: u32,
}

impl<T: ItemTrait> InInserterStrict<T> {
    pub(super) fn update_belt(&mut self, belt_storage: &mut StrictBeltStorage<T>) {
        let connected_storage = self.connected_storage.upgrade();

        if let Some(storage) = connected_storage {
            if storage.count.load(Ordering::Relaxed) < get_max_stack_size(T::get_item())
                && belt_storage.try_take_item_from_pos(self.belt_pos)
            {
                storage.count.fetch_add(1, Ordering::Relaxed);
            }
        }
    }

    pub(super) fn update_optimized_belt(&mut self, belt_storage: &mut OptimizedBeltStorage<T>) {
        let connected_storage = self.connected_storage.upgrade();

        if let Some(storage) = connected_storage {
            if storage.count.load(Ordering::Relaxed) < get_max_stack_size(T::get_item())
                && belt_storage.try_take_item_from_pos(self.belt_pos)
            {
                storage.count.fetch_add(1, Ordering::Relaxed);
            }
        }
    }
}
