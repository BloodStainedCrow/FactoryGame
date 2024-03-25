use std::sync::atomic::{AtomicU64, Ordering};

use crate::item::Item;

use super::{optimized::OptimizedBeltStorage, simple::BeltStorage};

#[derive(Debug)]
pub struct Inserter<'a> {
    pub connected_producer_count: &'a AtomicU64,
    pub belt_pos: u32,
}

impl Inserter<'_> {
    pub(super) fn update_belt(&mut self, belt_storage: &mut BeltStorage) {
        // TODO: get Item type from producer
        if self.connected_producer_count.load(Ordering::Acquire) > 0
            && belt_storage.try_put_item_in_pos(Item::Iron, self.belt_pos)
        {
            self.connected_producer_count
                .fetch_sub(1, Ordering::Release);
        }
    }

    pub(super) fn update_optimized_belt(&mut self, belt_storage: &mut OptimizedBeltStorage) {
        // TODO: get Item type from producer
        if self.connected_producer_count.load(Ordering::Acquire) > 0
            && belt_storage.try_put_item_in_pos(self.belt_pos)
        {
            self.connected_producer_count
                .fetch_sub(1, Ordering::Release);
        }
    }
}
