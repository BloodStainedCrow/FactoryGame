use std::sync::{
    atomic::{AtomicU64, Ordering},
    Weak,
};

use crate::{
    belt::{optimized::OptimizedBeltStorage, simple::BeltStorage},
    item::Item,
};

#[derive(Debug)]
pub struct InInserter {
    pub connected_count: Weak<AtomicU64>,
    pub belt_pos: u32,
}

impl InInserter {
    pub(super) fn update_belt(&mut self, belt_storage: &mut BeltStorage) {
        let connected_count = self.connected_count.upgrade();

        if let Some(count) = connected_count {
            // TODO: get Item type from producer for stack size
            if count.load(Ordering::Relaxed) < 10
                && belt_storage.try_take_item_from_pos(Item::Iron, self.belt_pos)
            {
                count.fetch_add(1, Ordering::Relaxed);
            }
        }
    }

    pub(super) fn update_optimized_belt(&mut self, belt_storage: &mut OptimizedBeltStorage) {
        let connected_count = self.connected_count.upgrade();

        if let Some(count) = connected_count {
            // TODO: get Item type from producer for stack size
            if count.load(Ordering::Relaxed) < 10
                && belt_storage.try_take_item_from_pos(self.belt_pos)
            {
                count.fetch_add(1, Ordering::Relaxed);
            }
        }
    }
}
