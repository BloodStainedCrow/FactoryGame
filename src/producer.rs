use std::marker::PhantomData;
use std::sync::atomic::Ordering;
use std::sync::Arc;

use crate::item::get_max_stack_size;
use crate::item::get_time_to_generate;
use crate::item::ItemStorageStrict;
use crate::item::ItemTrait;

#[derive(Debug)]
pub struct Producer<T: ItemTrait> {
    marker: PhantomData<T>,
    pub timer: i32,
    // TODO: Should this be pub? prob not
    pub storage: Arc<ItemStorageStrict<T>>,
}

impl<T: ItemTrait> Default for Producer<T> {
    fn default() -> Self {
        Self::new()
    }
}

impl<T: ItemTrait> Producer<T> {
    #[must_use]
    pub fn new() -> Self {
        Self {
            marker: PhantomData,
            timer: i32::from(get_time_to_generate(T::get_item())),
            storage: Arc::new(ItemStorageStrict::default()),
        }
    }

    #[inline(never)]
    pub fn update(&mut self) {
        if self.timer <= 0
            && self.storage.count.load(Ordering::Relaxed) < get_max_stack_size(T::get_item())
        {
            self.storage.count.fetch_add(1, Ordering::Relaxed);

            self.timer = i32::from(get_time_to_generate(T::get_item()));
        }
        self.timer -= 1;
    }
}

#[cfg(test)]
mod tests {
    use crate::item::Iron;

    use super::*;
    extern crate test;
    use proptest::proptest;
    use test::Bencher;

    proptest! {}

    #[bench]
    fn bench_producer_update(b: &mut Bencher) {
        let mut producer = Producer::<Iron>::new();

        b.iter(|| {
            let bb = test::black_box(&mut producer);

            for _ in 0..1_000 {
                bb.update();
            }
            // bb.count.store(0, Ordering::Relaxed);
        });

        println!("{producer:?}");
    }
}
