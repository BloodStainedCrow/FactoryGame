use std::sync::atomic::AtomicU64;
use std::sync::atomic::Ordering;
use std::sync::Arc;

use crate::item::get_max_stack_size;
use crate::item::get_time_to_generate;
use crate::item::Item;

#[derive(Debug)]
pub struct Producer {
    pub item: Item,
    pub timer: u16,
    // TODO: Should this be pub? prob not
    pub count: Arc<AtomicU64>,
}

impl Producer {
    #[must_use]
    pub fn new(item_to_produce: Item) -> Self {
        Self {
            item: item_to_produce,
            timer: get_time_to_generate(item_to_produce),
            count: Arc::new(AtomicU64::new(0)),
        }
    }

    pub fn update(&mut self) {
        if self.timer == 0 {
            if self.count.load(Ordering::Relaxed) < get_max_stack_size(self.item) {
                self.count.fetch_add(1, Ordering::Relaxed);

                self.timer = get_time_to_generate(self.item) - 1;
            }
        } else {
            self.timer -= 1;
        }
    }
}

#[cfg(test)]
mod tests {
    use crate::item::all_items;

    use super::*;
    extern crate test;
    use proptest::proptest;
    use test::Bencher;

    proptest! {
        #[test]
        fn test_constructing_producer_does_not_panic(item in all_items()) {
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
