use std::sync::{
    atomic::{AtomicU64, Ordering},
    Weak,
};

use crate::{
    belt::{optimized::OptimizedBeltStorage, simple::BeltStorage},
    item::Item,
};

#[derive(Debug)]
pub struct OutInserter {
    pub connected_count: Weak<AtomicU64>,
    pub belt_pos: u32,
}

impl OutInserter {
    pub(super) fn update_belt(&mut self, belt_storage: &mut BeltStorage) {
        let connected_count = self.connected_count.upgrade();

        if let Some(count) = connected_count {
            // TODO: get Item for
            if count.load(Ordering::Relaxed) > 0
                && belt_storage.try_put_item_in_pos(Item::Iron, self.belt_pos)
            {
                count.fetch_sub(1, Ordering::Relaxed);
            }
        }
    }

    pub(super) fn update_optimized_belt(&mut self, belt_storage: &mut OptimizedBeltStorage) {
        let connected_count = self.connected_count.upgrade();

        if let Some(count) = connected_count {
            // TODO: get Item type from producer
            if count.load(Ordering::Relaxed) > 0 && belt_storage.try_put_item_in_pos(self.belt_pos)
            {
                count.fetch_sub(1, Ordering::Relaxed);
            }
        }
    }
}
