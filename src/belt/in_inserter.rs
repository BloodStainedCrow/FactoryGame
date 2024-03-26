use std::sync::atomic::{AtomicU64, Ordering};

use crate::{
    belt::{optimized::OptimizedBeltStorage, simple::BeltStorage},
    item::Item,
};

#[derive(Debug)]
pub struct InInserter<'a> {
    pub connected_count: &'a AtomicU64,
    pub belt_pos: u32,
}

impl InInserter<'_> {
    pub(super) fn update_belt(&mut self, belt_storage: &mut BeltStorage) {
        // TODO: get Item type from producer for stack size
        if self.connected_count.load(Ordering::Relaxed) < 10
            && belt_storage.try_take_item_from_pos(Item::Iron, self.belt_pos)
        {
            self.connected_count.fetch_add(1, Ordering::Relaxed);
        }
    }

    pub(super) fn update_optimized_belt(&mut self, belt_storage: &mut OptimizedBeltStorage) {
        // TODO: get Item type from producer for stack size
        if self.connected_count.load(Ordering::Relaxed) < 10
            && belt_storage.try_take_item_from_pos(self.belt_pos)
        {
            self.connected_count.fetch_add(1, Ordering::Relaxed);
        }
    }
}
