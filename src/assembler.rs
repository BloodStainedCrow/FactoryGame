use std::{hint::black_box, simd::cmp::SimdPartialOrd};

use bytemuck::TransparentWrapper;

use crate::{
    item::{ItemStorage, SingleIngCraft},
    power::PowerGridIdentifier,
    producer::Simdtype,
};

#[derive(Debug, Default)]
pub struct MultiAssemblerStoreOne<Res: SingleIngCraft> {
    timers: Vec<u16>,
    connected_grids: Vec<PowerGridIdentifier>,
    input1: Vec<ItemStorage<Res::ING1>>,
    outputs: Vec<ItemStorage<Res>>,
    len: usize,
}

// TODO: Add ability to remove assemblers (probably by leaving holes and storing their indices)
// Maybe also add a defragmentation routine to mend the ineffeciencies left by deconstruction large amounts of assemblers
impl<Res: SingleIngCraft> MultiAssemblerStoreOne<Res> {
    #[must_use]
    pub const fn new() -> Self {
        Self {
            timers: vec![],
            connected_grids: vec![],
            input1: vec![],
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

        assert!(power_mult <= 64);

        assert_eq!(self.outputs.len(), self.timers.len());
        assert_eq!(self.input1.len(), self.timers.len());
        assert!(self.outputs.len() % Simdtype::LEN == 0);

        // TODO: Make these dependent on the recipe
        let ing1_amount: Simdtype = Simdtype::splat(2);
        let res_amount: Simdtype = Simdtype::splat(1);
        let timer_increase: Simdtype =
            Simdtype::splat((u16::from(power_mult) * 512) / TICKS_PER_CREATE * 2);
        let max_stack_size: Simdtype = Simdtype::splat(Res::MAX_STACK_SIZE);

        for i in (0..self
            .outputs
            .len()
            .min(self.len - (self.len % Simdtype::LEN)))
            .step_by(Simdtype::LEN)
        {
            let input = Simdtype::from_slice(ItemStorage::<Res::ING1>::peel_slice(
                self.input1.split_at(i).1,
            ));

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
                ItemStorage::<Res::ING1>::peel_slice_mut(self.input1.split_at_mut(i).1),
            );

            Simdtype::copy_to_slice(new_timer, self.timers.split_at_mut(i).1);
        }
    }

    // TODO: Currently this is the only routine with any kind of power calculations.
    /// # Panics
    /// If `power_mult` > 64
    pub fn update_simple(&mut self, power_mult: u8) {
        if power_mult == 0 {
            return;
        }

        assert!(power_mult <= 64);

        assert_eq!(self.outputs.len(), self.timers.len());
        assert_eq!(self.input1.len(), self.timers.len());
        assert!(self.outputs.len() % Simdtype::LEN == 0);

        let increase = (u16::from(power_mult) * 512) / Res::TIME_TO_CRAFT * 2;

        for (output, (input1, timer)) in self
            .outputs
            .iter_mut()
            .zip(self.input1.iter_mut().zip(self.timers.iter_mut()))
        {
            if *ItemStorage::<Res::ING1>::peel_mut(input1) >= Res::AMOUNT1 {
                let new_timer = timer.wrapping_add(increase);
                if *timer < new_timer {
                    // We should produce
                    if *ItemStorage::<Res>::peel_mut(output) + Res::OUTPUT_AMOUNT
                        <= Res::MAX_STACK_SIZE
                    {
                        *ItemStorage::<Res>::peel_mut(output) += Res::OUTPUT_AMOUNT;
                        *ItemStorage::<Res::ING1>::peel_mut(input1) -= Res::AMOUNT1;
                    }
                }
            }
        }
    }

    #[inline(never)]
    // TODO: Do i want this to also do the power calculation, or will that be done in another step?
    // TODO: Currently power demand and supply are offset by a single tick. Is this acceptable?
    // TODO: Write tests to ensure this works as expected.
    /// # Panics
    /// If `power_mult` > `MAX_POWER_MULT` = 64
    pub fn update_branchless(&mut self, power_mult: u8, grids: &mut [u32]) {
        const POWER_DRAIN: u16 = 25;
        const POWER_CONSUMPTION: u16 = 750;

        const MAX_POWER_MULT: u8 = 64;

        let mut power_used = u32::from(POWER_DRAIN)
            * u32::try_from(self.len).expect("more than u32::MAX assemblers");

        // TODO: With power calculations being done on the fly, we cannot return early, since we then do not know the power demands of the base :(
        // It might be fine, since it only applies if the power is so low, NOTHING happens and as soon as any power is connected it will start running again.
        // My guess is that returning 0 (or just the drain power) would potentially lead to a lot of flickering.
        if power_mult == 0 {
            return;
        }

        debug_assert!(power_mult <= MAX_POWER_MULT);

        // TODO: The goal of this line is to make sure that accesses will not panic (which works)
        //  and still allow vectorization (which does not)
        assert_eq!(grids.len(), PowerGridIdentifier::MAX as usize + 1);

        assert_eq!(self.outputs.len(), self.timers.len());
        assert_eq!(self.input1.len(), self.timers.len());
        assert!(self.outputs.len() % Simdtype::LEN == 0);

        let increase =
            (u16::from(power_mult) * (u16::MAX / u16::from(MAX_POWER_MULT))) / Res::TIME_TO_CRAFT;

        debug_assert!(increase > 0);

        for (output, (input1, (timer, grid))) in self.outputs.iter_mut().zip(
            self.input1
                .iter_mut()
                .zip(self.timers.iter_mut().zip(self.connected_grids.iter())),
        ) {
            let ing1_mul = u16::from(*ItemStorage::<Res::ING1>::peel_mut(input1) >= Res::AMOUNT1);
            let new_timer_output_space = timer.wrapping_add(increase * ing1_mul);
            let new_timer_output_full = timer.saturating_add(increase * ing1_mul);

            let space_mul = u16::from(
                *ItemStorage::<Res>::peel_mut(output) + Res::OUTPUT_AMOUNT <= Res::MAX_STACK_SIZE,
            );

            let new_timer =
                new_timer_output_space * space_mul + new_timer_output_full * (1 - space_mul);

            let timer_mul = u16::from(*timer < new_timer);

            let work_done_mul = u16::from(*timer != new_timer);

            // Power calculation
            // TODO: Make sure this cannot panic and does not inhibit vectorization
            // Without scatter this cannot SIMD, (probably even with it it seems hard tbh)
            // grids[*grid as usize] +=
            //     u32::from(POWER_DRAIN + timer_mul * space_mul * POWER_CONSUMPTION);
            // We use power if any work was done
            power_used += u32::from(work_done_mul * POWER_CONSUMPTION);

            *timer = new_timer;
            *ItemStorage::<Res>::peel_mut(output) += timer_mul * space_mul * Res::OUTPUT_AMOUNT;
            *ItemStorage::<Res::ING1>::peel_mut(input1) -= Res::AMOUNT1 * timer_mul * space_mul;
        }

        black_box(power_used);
    }

    pub fn get_outputs_mut(&mut self) -> &mut [ItemStorage<Res>] {
        &mut self.outputs
    }

    pub fn get_output_mut(&mut self, index: usize) -> Option<&mut ItemStorage<Res>> {
        if index < self.len {
            Some(&mut self.outputs[index])
        } else {
            None
        }
    }

    pub fn get_input_1_mut(&mut self, index: usize) -> Option<&mut ItemStorage<Res::ING1>> {
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
        debug_assert_eq!(self.connected_grids.len(), self.timers.len());
        debug_assert_eq!(self.outputs.len(), self.timers.len());
        debug_assert_eq!(self.input1.len(), self.timers.len());
        debug_assert!(self.outputs.len() % Simdtype::LEN == 0);

        if self.len == self.outputs.len() {
            // We need to grow
            self.outputs
                .resize_with(self.len + Simdtype::LEN, || ItemStorage::<Res>::new(0));
            self.connected_grids
                .resize_with(self.len + Simdtype::LEN, || 0);
            self.input1.resize_with(self.len + Simdtype::LEN, || {
                ItemStorage::<Res::ING1>::new(0)
            });
            self.timers
                .resize(self.len + Simdtype::LEN, Res::TIME_TO_CRAFT);
        } else {
            self.outputs[self.len] = ItemStorage::<Res>::new(0);
            // TODO: This should not be hardcoded like this!
            self.connected_grids[self.len] = 0;
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
    use rand::random;
    use test::Bencher;

    #[bench]
    fn bench_1_000_000_assembler_update_hard_simd(b: &mut Bencher) {
        let mut multi_store = MultiAssemblerStoreOne::<Iron>::new();

        for i in 0..1_000_000 {
            multi_store.add_assembler();
            *multi_store.get_input_1_mut(i).expect("Hardcoded") =
                ItemStorage::<IronOre>::new(random());
        }

        b.iter(|| {
            black_box(&mut multi_store).update(64);
        });
    }

    #[bench]
    fn bench_1_000_000_assembler_update_simple(b: &mut Bencher) {
        let mut multi_store = MultiAssemblerStoreOne::<Iron>::new();

        for i in 0..1_000_000 {
            multi_store.add_assembler();
            *multi_store.get_input_1_mut(i).expect("Hardcoded") =
                ItemStorage::<IronOre>::new(random());
        }

        b.iter(|| {
            black_box(&mut multi_store).update_simple(64);
        });
    }

    #[bench]
    fn bench_1_000_000_assembler_update_branchless(b: &mut Bencher) {
        let mut multi_store = MultiAssemblerStoreOne::<Iron>::new();
        let mut power = [0; PowerGridIdentifier::MAX as usize + 1];

        for i in 0..1_000_000 {
            multi_store.add_assembler();
            *multi_store.get_input_1_mut(i).expect("Hardcoded") =
                ItemStorage::<IronOre>::new(random());
        }

        b.iter(|| {
            black_box(&mut multi_store).update_branchless(64, &mut power);
        });
    }

    #[bench]
    fn bench_250_000_assembler_update(b: &mut Bencher) {
        let mut multi_store = MultiAssemblerStoreOne::<Iron>::new();
        let mut power = [0; PowerGridIdentifier::MAX as usize + 1];

        for _ in 0..250_000 {
            multi_store.add_assembler();
        }

        b.iter(|| {
            black_box(&mut multi_store).update_branchless(64, &mut power);
        });
    }

    #[bench]
    fn bench_8_000_assembler_update(b: &mut Bencher) {
        let mut multi_store = MultiAssemblerStoreOne::<Iron>::new();
        let mut power = [0; PowerGridIdentifier::MAX as usize + 1];

        for _ in 0..8_000 {
            multi_store.add_assembler();
        }

        b.iter(|| {
            black_box(&mut multi_store).update_branchless(64, &mut power);
        });
    }

    #[bench]
    fn bench_800_multi_assembler_with_10_each(b: &mut Bencher) {
        let mut multi_stores = vec![];
        let mut power = [0; PowerGridIdentifier::MAX as usize + 1];

        for _ in 0..800 {
            multi_stores.push(MultiAssemblerStoreOne::<Iron>::new());
        }

        for store in &mut multi_stores {
            for i in 0..10 {
                store.add_assembler();
                *store.get_input_1_mut(i).expect("Hardcoded") =
                    ItemStorage::<IronOre>::new(random());
            }
        }

        b.iter(|| {
            for store in &mut multi_stores {
                black_box(store).update_branchless(64, &mut power);
            }
        });
    }

    #[bench]
    fn bench_1000_multi_assembler_with_1000_each(b: &mut Bencher) {
        let mut multi_stores = vec![];
        let mut power = [0; PowerGridIdentifier::MAX as usize + 1];

        for _ in 0..1000 {
            multi_stores.push(MultiAssemblerStoreOne::<Iron>::new());
        }

        for store in &mut multi_stores {
            for i in 0..1000 {
                store.add_assembler();
                *store.get_input_1_mut(i).expect("Hardcoded") =
                    ItemStorage::<IronOre>::new(random());
            }
        }

        b.iter(|| {
            for store in &mut multi_stores {
                black_box(store).update_branchless(64, &mut power);
            }
        });
    }

    #[bench]
    fn bench_250_multi_assembler_with_100_000_each(b: &mut Bencher) {
        let mut multi_stores = vec![];
        let mut power = [0; PowerGridIdentifier::MAX as usize + 1];

        for _ in 0..250 {
            multi_stores.push(MultiAssemblerStoreOne::<Iron>::new());
        }

        for store in &mut multi_stores {
            for i in 0..100_000 {
                store.add_assembler();
                *store.get_input_1_mut(i).expect("Hardcoded") =
                    ItemStorage::<IronOre>::new(random());
            }
        }

        b.iter(|| {
            for store in &mut multi_stores {
                black_box(store).update_branchless(64, &mut power);
            }
        });
    }
}
