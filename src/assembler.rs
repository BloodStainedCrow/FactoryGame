use std::simd::cmp::SimdPartialOrd;

use bytemuck::TransparentWrapper;

use crate::{
    item::{Copper, Iron, ItemStorage, ItemTrait},
    producer::Simdtype,
};

struct NoItem;

struct MultiItemStorage<T: ItemTrait, R>(Vec<ItemStorage<T>>, R);

const NUM_RECIPED_WITH_1: usize = 1;
const NUM_RECIPED_WITH_2: usize = 0;
const NUM_RECIPED_WITH_3: usize = 1;
const NUM_RECIPED_WITH_4: usize = 1;
const NUM_RECIPED_WITH_5: usize = 0;

const NUM_RECIPES: usize = NUM_RECIPED_WITH_1
    + NUM_RECIPED_WITH_2
    + NUM_RECIPED_WITH_3
    + NUM_RECIPED_WITH_4
    + NUM_RECIPED_WITH_5;

const INPUT_COUNTS_1: [u8; NUM_RECIPES] = [1, 3, 10];
const INPUT_COUNTS_2: [u8; NUM_RECIPES - NUM_RECIPED_WITH_1] = [2, 1];
const INPUT_COUNTS_3: [u8; NUM_RECIPES - NUM_RECIPED_WITH_2 - NUM_RECIPED_WITH_1] = [10, 10];
const INPUT_COUNTS_4: [u8; NUM_RECIPES
    - NUM_RECIPED_WITH_3
    - NUM_RECIPED_WITH_2
    - NUM_RECIPED_WITH_1] = [1];
const INPUT_COUNTS_5: [u8; NUM_RECIPES
    - NUM_RECIPED_WITH_4
    - NUM_RECIPED_WITH_3
    - NUM_RECIPED_WITH_2
    - NUM_RECIPED_WITH_1] = [];

#[derive(Debug, Default)]
pub struct MultiAssemblerStoreOne<Ing1: ItemTrait, Res: ItemTrait> {
    timers: Vec<u16>,
    input1: Vec<ItemStorage<Ing1>>,
    outputs: Vec<ItemStorage<Res>>,
    len: usize,
}

impl<Ing1: ItemTrait, Res: ItemTrait> MultiAssemblerStoreOne<Ing1, Res> {
    #[must_use]
    pub fn new() -> Self {
        Self {
            timers: vec![],
            input1: vec![],
            outputs: vec![],
            len: 0,
        }
    }

    pub fn update(&mut self, power_mult: u8) {
        const TICKS_PER_CREATE: u16 = 64;
        if power_mult == 0 {
            return;
        }

        debug_assert!(power_mult <= 64);

        assert_eq!(self.outputs.len(), self.timers.len());
        assert_eq!(self.input1.len(), self.timers.len());
        assert!(self.outputs.len() % Simdtype::LEN == 0);

        // TODO: Make these dependent on the recipe
        let ing1_amount: Simdtype = Simdtype::splat(2);
        let res_amount: Simdtype = Simdtype::splat(1);
        let timer_increase: Simdtype =
            Simdtype::splat(u16::from(power_mult) * 512 / TICKS_PER_CREATE * 2);
        let max_stack_size: Simdtype = Simdtype::splat(10);

        for i in (0..self
            .outputs
            .len()
            .min(self.len - (self.len % Simdtype::LEN)))
            .step_by(Simdtype::LEN)
        {
            let input =
                Simdtype::from_slice(ItemStorage::<Ing1>::peel_slice(self.input1.split_at(i).1));

            let output =
                Simdtype::from_slice(ItemStorage::<Res>::peel_slice(self.outputs.split_at(i).1));

            let timer = Simdtype::from_slice(self.timers.split_at(i).1);

            let enough_items_mask = input.simd_ge(ing1_amount);

            let new_timer = enough_items_mask.select(timer + timer_increase, timer);

            // We had a wrap, so we produce
            // Since the timer only changes whenever we have enough items this masks is both for enough time and input items
            let produce_mask = new_timer.simd_lt(timer);

            let space_mask = output.simd_lt(max_stack_size);

            let space_time_mask = produce_mask & space_mask;

            // This can never wrap, since we check we have enough items before
            let new_ing1_amount = space_time_mask.select(input - ing1_amount, input);
            let new_output = space_time_mask.select(output + res_amount, output);

            Simdtype::copy_to_slice(
                new_output,
                ItemStorage::<Res>::peel_slice_mut(self.outputs.split_at_mut(i).1),
            );

            Simdtype::copy_to_slice(
                new_ing1_amount,
                ItemStorage::<Ing1>::peel_slice_mut(self.input1.split_at_mut(i).1),
            );

            Simdtype::copy_to_slice(new_timer, self.timers.split_at_mut(i).1);
        }
    }

    pub fn get_output(&mut self, index: usize) -> Option<&mut ItemStorage<Res>> {
        if index < self.len {
            Some(&mut self.outputs[index])
        } else {
            None
        }
    }

    pub fn get_input_1(&mut self, index: usize) -> Option<&mut ItemStorage<Ing1>> {
        if index < self.len {
            Some(&mut self.input1[index])
        } else {
            None
        }
    }

    pub fn remove_assembler(&mut self, index: usize) {
        // TODO: This would change all indices. Is that what I want?

        self.input1.swap(index, self.len - 1);
        self.outputs.swap(index, self.len - 1);
        self.timers.swap(index, self.len - 1);

        self.len -= 1;
    }

    pub fn add_assembler(&mut self) {
        assert_eq!(self.outputs.len(), self.timers.len());
        assert_eq!(self.input1.len(), self.timers.len());
        assert!(self.outputs.len() % Simdtype::LEN == 0);

        if self.len == self.outputs.len() {
            // We need to grow
            self.outputs
                .resize_with(self.len + Simdtype::LEN, || ItemStorage::<Res>::new(0));
            self.input1
                .resize_with(self.len + Simdtype::LEN, || ItemStorage::<Ing1>::new(0));
            self.timers
                .resize(self.len + Simdtype::LEN, Res::get_time_to_generate());
        } else {
            self.outputs[self.len] = ItemStorage::<Res>::new(0);
            self.timers[self.len] = 0;
        }

        self.len += 1;
    }
}

pub struct MultiAssemblerStoreLenient {
    timers: Vec<u16>,
    crafting_speed: Vec<u16>,
    inputs: [Vec<ItemStorage<Iron>>; 5],
    input_counts: [Vec<u16>; 5],
    outputs: Vec<ItemStorage<Copper>>,
    output_counts: Vec<u16>,
    len: usize,
}

#[cfg(test)]
mod tests {
    use std::hint::black_box;

    use crate::item::Iron;

    use super::*;
    extern crate test;
    use test::Bencher;

    #[bench]
    fn bench_1_000_000_assembler_update(b: &mut Bencher) {
        let mut multi_store = MultiAssemblerStoreOne::<Iron, Copper>::new();

        for _ in 0..1_000_000 {
            multi_store.add_assembler();
        }

        b.iter(|| {
            for _ in 0..Simdtype::LEN {
                black_box(&mut multi_store).update(64);
            }
        });
    }

    #[bench]
    fn bench_250_000_assembler_update(b: &mut Bencher) {
        let mut multi_store = MultiAssemblerStoreOne::<Iron, Copper>::new();

        for _ in 0..250_000 {
            multi_store.add_assembler();
        }

        b.iter(|| {
            for _ in 0..Simdtype::LEN {
                black_box(&mut multi_store).update(64);
            }
        });
    }

    #[bench]
    fn bench_8_000_assembler_update(b: &mut Bencher) {
        let mut multi_store = MultiAssemblerStoreOne::<Iron, Copper>::new();

        for _ in 0..8_000 {
            multi_store.add_assembler();
        }

        b.iter(|| {
            for _ in 0..Simdtype::LEN {
                black_box(&mut multi_store).update(64);
            }
        });
    }
}
