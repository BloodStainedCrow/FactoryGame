use std::sync::atomic::AtomicU64;
use std::sync::atomic::Ordering;

use crate::item::Item;

#[must_use]
pub const fn get_time_to_produce(item: Item) -> u16 {
    match item {
        Item::Iron => 2,
    }
}

#[must_use]
pub const fn get_max_stack_size(item: Item) -> u64 {
    match item {
        Item::Iron => 2_000_000_000_000,
    }
}

#[derive(Debug)]
pub struct Producer<'a> {
    pub item: Item,
    pub timer: u16,
    // TODO: Should this be pub? prob not
    pub count: &'a AtomicU64,
}

impl Producer<'_> {
    #[must_use]
    pub fn new(item_to_produce: Item) -> Self {
        Self {
            item: item_to_produce,
            timer: get_time_to_produce(item_to_produce),
            count: Box::leak(Box::new(AtomicU64::new(0))),
            // count: AtomicU64::new(0),
        }
    }

    pub fn update(&mut self) {
        if self.timer == 0 {
            if self.count.load(Ordering::Acquire) < get_max_stack_size(self.item) {
                self.count.fetch_add(1, Ordering::Release);

                self.timer = get_time_to_produce(self.item) - 1;
            }
        } else {
            self.timer -= 1;
        }
    }
}

#[cfg(test)]
mod tests {
    use crate::item::my_enum_strategy;

    use super::*;
    extern crate test;
    use proptest::proptest;
    use test::Bencher;

    proptest! {
        #[test]
        fn test_constructing_producer_does_not_panic(item in my_enum_strategy()) {
            let _ = Producer::new(item);
        }
    }

    #[bench]
    fn bench_producer_update(b: &mut Bencher) {
        let mut producer = Producer::new(Item::Iron);

        b.iter(|| {
            let bb = test::black_box(&mut producer);

            for _ in 0..5_000 {
                bb.update();
            }
            // bb.count.store(0, Ordering::Relaxed);
        });

        println!("{producer:?}");
    }
}
