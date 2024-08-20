use std::simd::cmp::SimdPartialEq;
use std::simd::cmp::SimdPartialOrd;
use std::simd::Simd;

use bytemuck::TransparentWrapper;
use soa_rs::Soars;

use crate::item::GeneratableItem;
use crate::item::ItemStorage;

pub type Simdtype = Simd<u16, 16>;

#[derive(Debug, Default)]
pub struct MultiProducerStore<T: GeneratableItem> {
    timers: Vec<u16>,
    outputs: Vec<ItemStorage<T>>,
    len: usize,
}

// #[derive(Debug, Soars)]
// pub struct Test {
//     a: i32,
// }

#[derive(Debug)]
pub struct Producer<T: GeneratableItem> {
    timer: u16,
    output: ItemStorage<T>,
}

impl<T: GeneratableItem> MultiProducerStore<T> {
    #[must_use]
    pub const fn new() -> Self {
        Self {
            timers: vec![],
            outputs: vec![],
            len: 0,
        }
    }

    /// # Panics
    /// If `power_mult` > 64
    pub fn update(&mut self, power_mult: u8) {
        const TICKS_PER_CREATE: u16 = 64;
        if power_mult == 0 {
            return;
        }

        assert_eq!(self.outputs.len(), self.timers.len());
        assert!(self.outputs.len() % Simdtype::LEN == 0);

        let timer_increase: Simdtype =
            Simdtype::splat((u16::from(power_mult) * 512) / TICKS_PER_CREATE * 2);

        for i in (0..self
            .outputs
            .len()
            .min(self.len - (self.len % Simdtype::LEN)))
            .step_by(Simdtype::LEN)
        {
            const ZERO: Simdtype = Simdtype::from_array([0; Simdtype::LEN]);
            const ONE: Simdtype = Simdtype::from_array([1; Simdtype::LEN]);

            let output =
                Simdtype::from_slice(ItemStorage::<T>::peel_slice(self.outputs.split_at(i).1));

            let timer = Simdtype::from_slice(self.timers.split_at(i).1);
            let timer_new = timer + timer_increase;

            let timer_mask = timer_new.simd_lt(timer);

            let new_outputs = output + ONE;

            Simdtype::copy_to_slice(
                timer_mask.select(new_outputs, output),
                ItemStorage::<T>::peel_slice_mut(self.outputs.split_at_mut(i).1),
            );

            Simdtype::copy_to_slice(
                timer_mask.select(ZERO, timer + ONE),
                self.timers.split_at_mut(i).1,
            );
        }
    }

    /// # Panics
    /// If `power_mult` > 64
    pub fn update_branchless(&mut self, power_mult: u8) {
        const TICKS_PER_CREATE: u16 = 64;
        if power_mult == 0 {
            return;
        }

        assert_eq!(self.outputs.len(), self.timers.len());
        assert!(self.outputs.len() % Simdtype::LEN == 0);

        let increase = (u16::from(power_mult) * 512) / TICKS_PER_CREATE * 2;

        for (output, timer) in self.outputs.iter_mut().zip(self.timers.iter_mut()) {
            let new_timer = timer.wrapping_add(increase);
            let timer_mul = u16::from(*timer < new_timer);
            let space_mul = u16::from(*ItemStorage::<T>::peel_mut(output) < T::max_stack_size());

            *ItemStorage::<T>::peel_mut(output) += timer_mul * space_mul;
        }
    }

    pub fn get_output(&mut self, index: usize) -> Option<&mut ItemStorage<T>> {
        if index < self.len {
            Some(&mut self.outputs[index])
        } else {
            None
        }
    }

    fn get_outputs(&mut self) -> &mut [ItemStorage<T>] {
        self.outputs.as_mut_slice().split_at_mut(self.len).0
    }

    pub fn remove_producer(&mut self, index: usize) {
        self.outputs.swap(index, self.len - 1);
        self.timers.swap(index, self.len - 1);

        self.len -= 1;
    }

    pub fn add_producer(&mut self) {
        debug_assert_eq!(self.outputs.len(), self.timers.len());
        debug_assert!(self.outputs.len() % Simdtype::LEN == 0);

        if self.len == self.outputs.len() {
            // We need to grow
            self.outputs
                .resize_with(self.len + Simdtype::LEN, || ItemStorage::<T>::new(0));
            self.timers
                .resize(self.len + Simdtype::LEN, T::get_time_to_generate());
        } else {
            self.outputs[self.len] = ItemStorage::<T>::new(0);
            self.timers[self.len] = 0;
        }
        self.len += 1;
    }
}

#[cfg(test)]
mod tests {
    use std::hint::black_box;

    use crate::item::{Iron, IronOre};

    use super::*;
    extern crate test;
    use test::Bencher;

    #[bench]
    fn bench_1_000_000_producer_update_hard_simd(b: &mut Bencher) {
        let mut multi_store = MultiProducerStore::<IronOre>::new();

        for _ in 0..1_000_000 {
            multi_store.add_producer();
        }

        b.iter(|| {
            black_box(&mut multi_store).update(64);
        });
    }

    #[bench]
    fn bench_1_000_000_producer_update(b: &mut Bencher) {
        let mut multi_store = MultiProducerStore::<IronOre>::new();

        for _ in 0..1_000_000 {
            multi_store.add_producer();
        }

        b.iter(|| {
            black_box(&mut multi_store).update_branchless(64);
        });
    }

    #[bench]
    fn bench_250_000_producer_update(b: &mut Bencher) {
        let mut multi_store = MultiProducerStore::<IronOre>::new();

        for _ in 0..250_000 {
            multi_store.add_producer();
        }

        b.iter(|| {
            black_box(&mut multi_store).update_branchless(64);
        });
    }

    #[bench]
    fn bench_8_000_producer_update(b: &mut Bencher) {
        let mut multi_store = MultiProducerStore::<IronOre>::new();

        for _ in 0..8_000 {
            multi_store.add_producer();
        }

        b.iter(|| {
            black_box(&mut multi_store).update_branchless(64);
        });
    }
}
