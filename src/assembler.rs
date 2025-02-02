use std::simd::cmp::SimdPartialOrd;

use bytemuck::TransparentWrapper;

use crate::{
    item::{BurnableItem, ItemStorage, SingleIngCraft, SmeltableItem},
    power::{Joule, Watt, MAX_POWER_MULT},
    producer::Simdtype,
};

#[derive(Debug, Default, Clone, serde::Deserialize, serde::Serialize)]
pub struct MultiAssemblerStoreOne<Res: SingleIngCraft> {
    timers: Vec<u16>,
    input1: Vec<ItemStorage<Res::ING1>>,
    outputs: Vec<ItemStorage<Res>>,

    // The indices in this vec are not actually used. This is to avoid changing the ids stored in the inserters
    holes: Vec<usize>,
    len: usize,
}

#[derive(Debug, Default, Clone, serde::Deserialize, serde::Serialize)]
pub struct MultiElectricFurnaceStore<Res: SmeltableItem> {
    pub store: MultiAssemblerStoreOne<Res>,
}

#[derive(Debug, Default, serde::Deserialize, serde::Serialize)]
pub struct MultiFurnaceStore<Res: SmeltableItem, Fuel: BurnableItem> {
    timers: Vec<u16>,
    fuel: Vec<ItemStorage<Fuel>>,
    input: Vec<ItemStorage<Res::ING1>>,
    outputs: Vec<ItemStorage<Res>>,

    // The indices in this vec are not actually used. This is to avoid changing the ids stored in the inserters etc
    holes: Vec<usize>,
    len: usize,
}

// TODO: Maybe also add a defragmentation routine to mend the ineffeciencies left by deconstruction large amounts of assemblers
impl<Res: SingleIngCraft> MultiAssemblerStoreOne<Res> {
    #[must_use]
    pub const fn new() -> Self {
        Self {
            timers: vec![],
            input1: vec![],
            outputs: vec![],

            holes: vec![],
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
    pub fn update_branchless(&mut self, power_mult: u8) -> Joule {
        const POWER_DRAIN: Watt = Watt(2_500);
        const POWER_CONSUMPTION: Watt = Watt(75_000);

        // TODO: For SOME reason, this is actually faster if this is a u32.
        // It is also better, since it allows possibly having more than u16::max assembers of a single recipe
        let mut running: u32 = 0;

        // TODO: With power calculations being done on the fly, we cannot return early, since we then do not know the power demands of the base :(
        // It might be fine, since it only applies if the power is so low, NOTHING happens and as soon as any power is connected it will start running again.
        // My guess is that returning 0 (or just the drain power) could lead to a lot of flickering.
        // if power_mult == 0 {
        //     return;
        // }

        debug_assert!(power_mult <= MAX_POWER_MULT);

        assert_eq!(self.outputs.len(), self.timers.len());
        assert_eq!(self.input1.len(), self.timers.len());
        assert!(self.outputs.len() % Simdtype::LEN == 0);

        // TODO: This does not round correctly
        let increase =
            (u16::from(power_mult) * (u16::MAX / u16::from(MAX_POWER_MULT))) / Res::TIME_TO_CRAFT;

        // TODO: I don't think this holds anymore, now that we cannot bail early at 0 power_mult
        // debug_assert!(increase > 0);

        for (output, (input1, timer)) in self
            .outputs
            .iter_mut()
            .zip(self.input1.iter_mut().zip(self.timers.iter_mut()))
        {
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
            // We use power if any work was done
            running += u32::from(work_done_mul);

            *timer = new_timer;
            *ItemStorage::<Res>::peel_mut(output) += timer_mul * space_mul * Res::OUTPUT_AMOUNT;
            *ItemStorage::<Res::ING1>::peel_mut(input1) -= Res::AMOUNT1 * timer_mul * space_mul;
        }

        POWER_DRAIN.joules_per_tick()
            * u64::try_from(self.len - self.holes.len()).expect("more than u64::MAX assemblers")
            + POWER_CONSUMPTION.joules_per_tick() * running.into()
    }

    pub fn get_outputs_mut(&mut self) -> &mut [ItemStorage<Res>] {
        &mut self.outputs
    }

    pub fn get_inputs_1_mut(&mut self) -> &mut [ItemStorage<Res::ING1>] {
        &mut self.input1
    }

    pub fn get_output_mut(&mut self, index: usize) -> Option<&mut ItemStorage<Res>> {
        if index < self.len {
            Some(&mut self.outputs[index])
        } else {
            None
        }
    }

    pub fn get_output(&self, index: usize) -> Option<&ItemStorage<Res>> {
        if index < self.len {
            Some(&self.outputs[index])
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

    /// The caller must make sure, that this index is not used in any other machine, since it will either crash/work on a nonexistant Assembler or be reused for another machine!
    pub fn remove_assembler(&mut self, index: usize) {
        debug_assert!(!self.holes.contains(&index));
        self.holes.push(index);
    }

    pub fn add_assembler(&mut self) -> usize {
        debug_assert_eq!(self.outputs.len(), self.timers.len());
        debug_assert_eq!(self.input1.len(), self.timers.len());
        debug_assert!(self.outputs.len() % Simdtype::LEN == 0);

        if let Some(hole_index) = self.holes.pop() {
            // TODO: This is scatter assemblers which are close around the vec which is bad for cache locality
            self.outputs[hole_index] = ItemStorage::<Res>::new(0);
            self.input1[hole_index] = ItemStorage::<Res::ING1>::new(0);
            self.timers[hole_index] = 0;
            return hole_index;
        }

        if self.len == self.outputs.len() {
            // We need to grow
            self.outputs
                .resize_with(self.len + Simdtype::LEN, || ItemStorage::<Res>::new(0));
            self.input1.resize_with(self.len + Simdtype::LEN, || {
                ItemStorage::<Res::ING1>::new(0)
            });
            self.timers
                .resize(self.len + Simdtype::LEN, Res::TIME_TO_CRAFT);
        } else {
            self.outputs[self.len] = ItemStorage::<Res>::new(0);
            self.input1[self.len] = ItemStorage::<Res::ING1>::new(0);
            self.timers[self.len] = 0;
        }

        self.len += 1;
        self.len - 1
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

        for i in 0..1_000_000 {
            multi_store.add_assembler();
            *multi_store.get_input_1_mut(i).expect("Hardcoded") =
                ItemStorage::<IronOre>::new(random());
        }

        b.iter(|| {
            black_box(&mut multi_store).update_branchless(64);
        });
    }

    #[bench]
    fn bench_250_000_assembler_update(b: &mut Bencher) {
        let mut multi_store = MultiAssemblerStoreOne::<Iron>::new();

        for _ in 0..250_000 {
            multi_store.add_assembler();
        }

        b.iter(|| {
            black_box(&mut multi_store).update_branchless(64);
        });
    }

    #[bench]
    fn bench_8_000_assembler_update(b: &mut Bencher) {
        let mut multi_store = MultiAssemblerStoreOne::<Iron>::new();

        for _ in 0..8_000 {
            multi_store.add_assembler();
        }

        b.iter(|| {
            black_box(&mut multi_store).update_branchless(64);
        });
    }

    #[bench]
    fn bench_800_multi_assembler_with_10_each(b: &mut Bencher) {
        let mut multi_stores = vec![];

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
                black_box(store).update_branchless(64);
            }
        });
    }

    #[bench]
    fn bench_1000_multi_assembler_with_1000_each(b: &mut Bencher) {
        let mut multi_stores = vec![];

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
                black_box(store).update_branchless(64);
            }
        });
    }

    #[bench]
    fn bench_250_multi_assembler_with_100_000_each(b: &mut Bencher) {
        let mut multi_stores = vec![];

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
                black_box(store).update_branchless(64);
            }
        });
    }
}
