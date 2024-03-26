use std::sync::atomic::{AtomicU64, Ordering};

use crate::{
    belt::{optimized::OptimizedBeltStorage, simple::BeltStorage},
    item::Item,
};

#[derive(Debug)]
pub struct OutInserter<'a> {
    pub connected_count: &'a AtomicU64,
    pub belt_pos: u32,
}

impl OutInserter<'_> {
    pub(super) fn update_belt(&mut self, belt_storage: &mut BeltStorage) {
        // TODO: get Item type from producer
        if self.connected_count.load(Ordering::Acquire) > 0
            && belt_storage.try_put_item_in_pos(Item::Iron, self.belt_pos)
        {
            self.connected_count.fetch_sub(1, Ordering::Release);
        }
    }

    pub(super) fn update_optimized_belt(&mut self, belt_storage: &mut OptimizedBeltStorage) {
        // TODO: get Item type from producer
        if self.connected_count.load(Ordering::Acquire) > 0
            && belt_storage.try_put_item_in_pos(self.belt_pos)
        {
            self.connected_count.fetch_sub(1, Ordering::Release);
        }
    }
}
