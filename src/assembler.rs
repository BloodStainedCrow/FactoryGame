use std::{array, simd::Simd};

use itertools::Itertools;

use crate::{
    data::{DataStore, ItemRecipeDir},
    frontend::world::tile::AssemblerID,
    item::{IdxTrait, Item, Recipe, WeakIdxTrait, ITEMCOUNTTYPE},
    power::{
        power_grid::{IndexUpdateInfo, PowerGridIdentifier, MAX_POWER_MULT},
        Joule, Watt,
    },
};

pub type Simdtype = Simd<u8, 32>;

// TODO: Is u8 bit enough?
pub type TIMERTYPE = u16;

// TODO: Do I want these generics or just get it at runtime?
// FIXME: We store the same slice length n times!
// TODO: Do I want to use SimdTypes for this?
// TODO: Don´t clump update data and data for adding/removing assemblers together!
#[derive(Debug, Clone, serde::Serialize, serde::Deserialize)]
pub struct MultiAssemblerStore<
    RecipeIdxType: WeakIdxTrait,
    const NUM_INGS: usize,
    const NUM_OUTPUTS: usize,
> {
    pub recipe: Recipe<RecipeIdxType>,

    /// Crafting Speed in 10% increments
    /// i.e. 18 => 180% Crafting speed
    /// Maximum is 2550% Crafting Speed
    speed: Box<[u8]>,
    /// Bonus Productivity in %
    bonus_productivity: Box<[u8]>,
    /// Power Consumption in 10% increments
    /// i.e. 18 => 180% Power Consumption
    /// Maximum is 2550% x Base Power Consumption
    power_consumption_modifier: Box<[u8]>,
    // TODO: This can likely be smaller than full u64
    base_power_consumption: Box<[Watt]>,
    #[serde(with = "arrays")]
    ings: [Box<[ITEMCOUNTTYPE]>; NUM_INGS],
    #[serde(with = "arrays")]
    outputs: [Box<[ITEMCOUNTTYPE]>; NUM_OUTPUTS],
    timers: Box<[TIMERTYPE]>,

    holes: Vec<usize>,
    len: usize,
}

#[derive(Debug, Clone, Default, serde::Serialize, serde::Deserialize)]
pub struct FullAssemblerStore<RecipeIdxType: WeakIdxTrait> {
    pub assemblers_0_1: Box<[MultiAssemblerStore<RecipeIdxType, 0, 1>]>,
    pub assemblers_1_1: Box<[MultiAssemblerStore<RecipeIdxType, 1, 1>]>,
}

#[derive(Debug, Clone)]
pub struct AssemblerOnclickInfo<ItemIdxType: WeakIdxTrait> {
    inputs: Vec<(Item<ItemIdxType>, ITEMCOUNTTYPE)>,
    outputs: Vec<(Item<ItemIdxType>, ITEMCOUNTTYPE)>,
    timer_percentage: f32,
}

impl<RecipeIdxType: IdxTrait> FullAssemblerStore<RecipeIdxType> {
    #[must_use]
    pub fn new<ItemIdxType: IdxTrait>(data_store: &DataStore<ItemIdxType, RecipeIdxType>) -> Self {
        let assemblers_0_1 = data_store
            .ing_out_num_to_recipe
            .get(&(0, 1))
            .unwrap()
            .iter()
            .map(|r| MultiAssemblerStore::new(*r))
            .collect();
        // let assemblers_1_1 = data_store
        //     .ing_out_num_to_recipe
        //     .get(&(1, 1))
        //     .unwrap()
        //     .iter()
        //     .map(|r| MultiAssemblerStore::new(*r))
        //     .collect();

        Self {
            assemblers_0_1,
            assemblers_1_1: vec![].into_boxed_slice(),
        }
    }

    #[must_use]
    pub fn join<ItemIdxType: IdxTrait>(
        self,
        other: Self,
        data_store: &DataStore<ItemIdxType, RecipeIdxType>,
        self_grid: PowerGridIdentifier,
        other_grid: PowerGridIdentifier,
    ) -> (
        Self,
        impl IntoIterator<Item = IndexUpdateInfo<ItemIdxType, RecipeIdxType>>,
    ) {
        let mut idx_update = vec![];

        let ret = Self {
            // TODO: This just works with box::into_iter in edition 2024
            assemblers_0_1: self
                .assemblers_0_1
                .into_vec()
                .into_iter()
                .zip(other.assemblers_0_1.into_vec())
                .map(|(a, b)| a.join(b, data_store, self_grid, other_grid))
                .map(|(store, updates)| {
                    idx_update.extend(updates);
                    store
                })
                .collect(),
            assemblers_1_1: self
                .assemblers_1_1
                .into_vec()
                .into_iter()
                .zip(other.assemblers_1_1.into_vec())
                .map(|(a, b)| a.join(b, data_store, self_grid, other_grid))
                .map(|(store, updates)| {
                    idx_update.extend(updates);
                    store
                })
                .collect(),
        };

        (ret, idx_update)
    }

    pub fn get_info<ItemIdxType: IdxTrait>(
        &self,
        assembler_id: AssemblerID<RecipeIdxType>,
        data_store: &DataStore<ItemIdxType, RecipeIdxType>,
    ) -> AssemblerOnclickInfo<ItemIdxType> {
        let recipe_id = assembler_id.recipe.id.into();

        match (
            data_store.recipe_num_ing_lookup[recipe_id],
            data_store.recipe_num_out_lookup[recipe_id],
        ) {
            (0, 1) => {
                assert_eq!(
                    assembler_id.recipe,
                    self.assemblers_0_1[data_store.recipe_to_ing_out_combo_idx[recipe_id]].recipe
                );

                self.assemblers_0_1[data_store.recipe_to_ing_out_combo_idx[recipe_id]]
                    .get_info(assembler_id.assembler_index, data_store)
            },

            _ => unreachable!(),
        }
    }
}

// FIXME:
// fn get_slice_for_item<'a, ItemIdxType: IdxTrait, RecipeIdxType: IdxTrait>(
//     item: Item<ITEMCOUNTTYPE>,
//     assemblers: &'a mut FullAssemblerStore<RecipeIdxType>,
//     data_store: &DataStore<ItemIdxType, RecipeIdxType>,
// ) -> SingleItemSlice<'a, 'a> {
//     let res: Vec<&mut Vec<u8>> = assemblers
//         .iter_mut()
//         .flat_map(|store| {
//             let idx = recipe_item_idx(recipe, item);
//             idx.map(|idx| &mut store.ings[idx])
//         })
//         .collect();

//     // lifetime!
//     // We can´t return this out of a function
//     &mut res
// }

// ALTERNATIVE
// This has the alternative of having the lookup for inserters ready
// struct AssemblerItemStores {
//     stores: [(
//         [Vec<ITEMCOUNTTYPE>; NUM_RECIPES_ITEM_INGS],
//         [Vec<ITEMCOUNTTYPE>; NUM_RECIPES_ITEM_OUTPUT],
//     ); NUM_ITEMS],
// }

// struct MultiAssemblerTest {
//     recipe: (),
//     timers: Vec<TIMERTYPE>,
// }

pub struct AssemblerRemovalInfo {
    ings: Vec<ITEMCOUNTTYPE>,
    outputs: Vec<ITEMCOUNTTYPE>,
}

// TODO: Maybe also add a defragmentation routine to mend the ineffeciencies left by deconstruction large amounts of assemblers
impl<RecipeIdxType: IdxTrait, const NUM_INGS: usize, const NUM_OUTPUTS: usize>
    MultiAssemblerStore<RecipeIdxType, NUM_INGS, NUM_OUTPUTS>
{
    #[must_use]
    pub fn new(recipe: Recipe<RecipeIdxType>) -> Self {
        Self {
            recipe,

            ings: array::from_fn(|_| vec![].into_boxed_slice()),
            outputs: array::from_fn(|_| vec![].into_boxed_slice()),
            timers: vec![].into_boxed_slice(),

            bonus_productivity: vec![].into_boxed_slice(),
            speed: vec![].into_boxed_slice(),
            power_consumption_modifier: vec![].into_boxed_slice(),

            base_power_consumption: vec![].into_boxed_slice(),

            holes: vec![],
            len: 0,
        }
    }

    // TODO: Properly test this!
    pub fn join<ItemIdxType: IdxTrait>(
        self,
        other: Self,
        data_store: &DataStore<ItemIdxType, RecipeIdxType>,
        self_grid: PowerGridIdentifier,
        other_grid: PowerGridIdentifier,
    ) -> (
        Self,
        impl IntoIterator<Item = IndexUpdateInfo<ItemIdxType, RecipeIdxType>>,
    ) {
        let mut update_vec = vec![];

        for (new_offs, (idx, _)) in other
            .timers
            .iter()
            .enumerate()
            .filter(|(i, _)| !other.holes.contains(i))
            .enumerate()
        {
            for (dir, item) in data_store.recipe_to_items.get(&self.recipe).unwrap() {
                let old_id = data_store
                    .get_storage_id_for_assembler(
                        *dir,
                        *item,
                        AssemblerID {
                            recipe: other.recipe,
                            grid: other_grid,
                            assembler_index: idx.try_into().unwrap(),
                        },
                    )
                    .unwrap();

                let new_id = data_store
                    .get_storage_id_for_assembler(
                        *dir,
                        *item,
                        AssemblerID {
                            recipe: other.recipe,
                            grid: self_grid,
                            assembler_index: (self.timers.len() + new_offs).try_into().unwrap(),
                        },
                    )
                    .unwrap();

                update_vec.push(IndexUpdateInfo {
                    old: (*item, old_id),
                    new: (*item, new_id),
                });
            }
        }

        let new_ings: [Box<[u8]>; NUM_INGS] = self
            .ings
            .into_iter()
            .zip(other.ings)
            .map(|(s, o)| {
                let mut s = s.into_vec();
                s.extend(
                    o.into_vec()
                        .into_iter()
                        .enumerate()
                        .filter(|(i, _)| !other.holes.contains(i))
                        .map(|(_, v)| v),
                );
                s.into_boxed_slice()
            })
            .collect_array()
            .unwrap();
        let new_outputs: [Box<[u8]>; NUM_OUTPUTS] = self
            .outputs
            .into_iter()
            .zip(other.outputs)
            .map(|(s, o)| {
                let mut s = s.into_vec();
                s.extend(
                    o.into_vec()
                        .into_iter()
                        .enumerate()
                        .filter(|(i, _)| !other.holes.contains(i))
                        .map(|(_, v)| v),
                );
                s.into_boxed_slice()
            })
            .collect_array()
            .unwrap();
        let mut new_timers = self.timers.into_vec();
        new_timers.extend(
            other
                .timers
                .into_vec()
                .into_iter()
                .enumerate()
                .filter(|(i, _)| !other.holes.contains(i))
                .map(|(_, v)| v),
        );

        let ret = todo!();
        //Self {
        //    recipe: self.recipe,
        //    ings: new_ings,
        //    outputs: new_outputs,
        //    timers: new_timers.into(),
        //    holes: self.holes,
        //    len: 0,
        //};

        (ret, update_vec)
    }

    fn get_info<ItemIdxType: IdxTrait>(
        &self,
        index: u16,
        data_store: &DataStore<ItemIdxType, RecipeIdxType>,
    ) -> AssemblerOnclickInfo<ItemIdxType> {
        let items = data_store.recipe_to_items.get(&self.recipe).unwrap();

        AssemblerOnclickInfo {
            inputs: self
                .ings
                .iter()
                .map(|ings| ings[index as usize])
                .enumerate()
                .map(|(item_idx, num_ings)| {
                    (
                        items
                            .iter()
                            .filter(|(dir, _)| *dir == ItemRecipeDir::Ing)
                            .nth(item_idx)
                            .unwrap()
                            .1,
                        num_ings,
                    )
                })
                .collect(),
            outputs: self
                .outputs
                .iter()
                .map(|outputs| outputs[index as usize])
                .enumerate()
                .map(|(item_idx, num_outputs)| {
                    (
                        items
                            .iter()
                            .filter(|(dir, _)| *dir == ItemRecipeDir::Out)
                            .nth(item_idx)
                            .unwrap()
                            .1,
                        num_outputs,
                    )
                })
                .collect(),
            timer_percentage: f32::from(self.timers[index as usize]) / f32::from(TIMERTYPE::MAX),
        }
    }

    /// # Panics
    /// If `power_mult` > 64
    // pub fn update(&mut self, power_mult: u8) {
    //     const TICKS_PER_CREATE: u16 = 64;
    //     if power_mult == 0 {
    //         return;
    //     }

    //     assert!(power_mult <= 64);

    //     assert_eq!(self.outputs.len(), self.timers.len());
    //     assert_eq!(self.input1.len(), self.timers.len());
    //     assert!(self.outputs.len() % Simdtype::LEN == 0);

    //     // TODO: Make these dependent on the recipe
    //     let ing1_amount: Simdtype = Simdtype::splat(2);
    //     let res_amount: Simdtype = Simdtype::splat(1);
    //     let timer_increase: Simdtype =
    //         Simdtype::splat((u16::from(power_mult) * 512) / TICKS_PER_CREATE * 2);
    //     let max_stack_size: Simdtype = Simdtype::splat(Res::MAX_STACK_SIZE);

    //     for i in (0..self
    //         .outputs
    //         .len()
    //         .min(self.len - (self.len % Simdtype::LEN)))
    //         .step_by(Simdtype::LEN)
    //     {
    //         let input = Simdtype::from_slice(ItemStorage::<Res::ING1>::peel_slice(
    //             self.input1.split_at(i).1,
    //         ));

    //         let output =
    //             Simdtype::from_slice(ItemStorage::<Res>::peel_slice(self.outputs.split_at(i).1));

    //         let timer = Simdtype::from_slice(self.timers.split_at(i).1);

    //         let enough_items_mask = input.simd_ge(ing1_amount);

    //         let new_timer = enough_items_mask.select(timer + timer_increase, timer);

    //         // We had a wrap, so we produce
    //         // Since the timer only changes whenever we have enough items this masks is both for enough time and input items
    //         let produce_mask = new_timer.simd_lt(timer);

    //         let space_mask = output.simd_lt(max_stack_size);

    //         let space_time_mask = produce_mask & space_mask;

    //         // This can never wrap, since we check we have enough items before
    //         let new_ing1_amount = space_time_mask.select(input - ing1_amount, input);
    //         let new_output = space_time_mask.select(output + res_amount, output);

    //         Simdtype::copy_to_slice(
    //             new_output,
    //             ItemStorage::<Res>::peel_slice_mut(self.outputs.split_at_mut(i).1),
    //         );

    //         Simdtype::copy_to_slice(
    //             new_ing1_amount,
    //             ItemStorage::<Res::ING1>::peel_slice_mut(self.input1.split_at_mut(i).1),
    //         );

    //         Simdtype::copy_to_slice(new_timer, self.timers.split_at_mut(i).1);
    //     }
    // }

    // // TODO: Currently this is the only routine with any kind of power calculations.
    // /// # Panics
    // /// If `power_mult` > 64
    // pub fn update_simple(&mut self, power_mult: u8) {
    //     if power_mult == 0 {
    //         return;
    //     }

    //     assert!(power_mult <= 64);

    //     assert_eq!(self.outputs.len(), self.timers.len());
    //     assert_eq!(self.input1.len(), self.timers.len());
    //     assert!(self.outputs.len() % Simdtype::LEN == 0);

    //     let increase = (u16::from(power_mult) * 512) / Res::TIME_TO_CRAFT * 2;

    //     for (output, (input1, timer)) in self
    //         .outputs
    //         .iter_mut()
    //         .zip(self.input1.iter_mut().zip(self.timers.iter_mut()))
    //     {
    //         if *ItemStorage::<Res::ING1>::peel_mut(input1) >= Res::AMOUNT1 {
    //             let new_timer = timer.wrapping_add(increase);
    //             if *timer < new_timer {
    //                 // We should produce
    //                 if *ItemStorage::<Res>::peel_mut(output) + Res::OUTPUT_AMOUNT
    //                     <= Res::MAX_STACK_SIZE
    //                 {
    //                     *ItemStorage::<Res>::peel_mut(output) += Res::OUTPUT_AMOUNT;
    //                     *ItemStorage::<Res::ING1>::peel_mut(input1) -= Res::AMOUNT1;
    //                 }
    //             }
    //         }
    //     }
    // }

    #[inline(never)]
    // TODO: Do i want this to also do the power calculation, or will that be done in another step?
    // TODO: Currently power demand and supply are offset by a single tick. Is this acceptable?
    // TODO: Write tests to ensure this works as expected.
    /// # Panics
    /// If `power_mult` > `MAX_POWER_MULT` = 64
    pub fn update_branchless<ItemIdxType: IdxTrait>(
        &mut self,
        power_mult: u8,
        recipe_lookup: &[(usize, usize)],
        recipe_ings: &[[ITEMCOUNTTYPE; NUM_INGS]],
        recipe_outputs: &[[ITEMCOUNTTYPE; NUM_OUTPUTS]],
        times: &[TIMERTYPE],
    ) -> (Joule, u32, u32) {
        // FIXME: These depend on which machine we are.
        const POWER_DRAIN: Watt = Watt(2_500);
        const POWER_CONSUMPTION: Watt = Watt(75_000);

        let (ing_idx, out_idx) = recipe_lookup[self.recipe.id.into()];

        let our_ings: &[ITEMCOUNTTYPE; NUM_INGS] = &recipe_ings[ing_idx];
        let our_outputs: &[ITEMCOUNTTYPE; NUM_OUTPUTS] = &recipe_outputs[out_idx];

        // TODO: For SOME reason, this is actually faster if this is a u32.
        // It is also better, since it allows possibly having more than u16::max assembers of a single recipe
        let mut running: u32 = 0;

        let mut times_ings_used = 0;
        let mut num_finished_crafts = 0;

        // TODO: With power calculations being done on the fly, we cannot return early, since we then do not know the power demands of the base :(
        // It might be fine, since it only applies if the power is so low, NOTHING happens and as soon as any power is connected it will start running again.
        // My guess is that returning 0 (or just the drain power) would lead to flickering.
        // if power_mult == 0 {
        //     return;
        // }

        debug_assert!(power_mult <= MAX_POWER_MULT);

        // FIXME:
        // assert_eq!(self.outputs.len(), self.timers.len());
        // assert_eq!(self.input1.len(), self.timers.len());
        // assert!(self.outputs.len() % Simdtype::LEN == 0);

        // TODO: This does not round correctly
        let increase: TIMERTYPE = (TIMERTYPE::from(power_mult)
            * (TIMERTYPE::MAX / TIMERTYPE::from(MAX_POWER_MULT)))
            / times[self.recipe.id.into()];

        // TODO: I don't think this holds anymore, now that we cannot bail early at 0 power_mult
        // debug_assert!(increase > 0);

        let ings_arr = ZipArray {
            array: self.ings.each_mut().map(|r| r.iter_mut()),
        };

        let outputs_arr = ZipArray {
            array: self.outputs.each_mut().map(|r| r.iter_mut()),
        };

        for (mut outputs, (mut ings, timer)) in
            outputs_arr.zip(ings_arr.zip(self.timers.iter_mut()))
        {
            let ing_mul = ings
                .iter()
                .zip(our_ings.iter())
                .fold(1, |acc, (have, want)| acc * u16::from(**have >= *want));
            let new_timer_output_space = timer.wrapping_add(increase * ing_mul);
            let new_timer_output_full = timer.saturating_add(increase * ing_mul);

            let space_mul: u8 =
                outputs
                    .iter()
                    .zip(our_outputs.iter())
                    .fold(1, |acc, (have, new_from_recipe)| {
                        // TODO: 100 output amount hardcoded!!!!
                        acc * u8::from((have.saturating_add(*new_from_recipe)) <= 100)
                    });

            let new_timer = new_timer_output_space * u16::from(space_mul)
                + new_timer_output_full * (1 - u16::from(space_mul));

            let timer_mul: u8 = (new_timer < *timer).into();

            // let work_done_mul = (*timer != new_timer).into();

            // Power calculation
            // We use power if any work was done
            running += u32::from(ing_mul * u16::from(space_mul));

            *timer = new_timer;
            outputs
                .iter_mut()
                .zip(our_outputs.iter())
                .for_each(|(output, new)| **output += timer_mul * space_mul * new);
            ings.iter_mut()
                .zip(our_ings.iter())
                .for_each(|(ing, used)| **ing -= timer_mul * space_mul * used);
            times_ings_used += u32::from(timer_mul * space_mul);
            num_finished_crafts += u32::from(timer_mul * space_mul);
        }

        (
            POWER_DRAIN.joules_per_tick()
                * u64::try_from(self.len - self.holes.len())
                    .expect("more than u64::MAX assemblers")
                + POWER_CONSUMPTION.joules_per_tick() * u64::from(running),
            times_ings_used,
            num_finished_crafts,
        )
    }

    pub fn get_outputs_mut(&mut self, idx: usize) -> &mut [ITEMCOUNTTYPE] {
        &mut self.outputs[idx]
    }

    pub fn get_ings_mut(&mut self, idx: usize) -> &mut [ITEMCOUNTTYPE] {
        &mut self.ings[idx]
    }

    pub fn get_output_mut(
        &mut self,
        output_idx: usize,
        index: usize,
    ) -> Option<&mut ITEMCOUNTTYPE> {
        if index < self.len {
            Some(&mut self.outputs[output_idx][index])
        } else {
            None
        }
    }

    pub fn get_output(&self, output_idx: usize, index: usize) -> Option<&ITEMCOUNTTYPE> {
        if index < self.len {
            Some(&self.outputs[output_idx][index])
        } else {
            None
        }
    }

    pub fn get_ing_mut(&mut self, input_idx: usize, index: usize) -> Option<&mut ITEMCOUNTTYPE> {
        if index < self.len {
            Some(&mut self.ings[input_idx][index])
        } else {
            None
        }
    }

    /// The caller must make sure, that this index is not used in any other machine, since it will either crash/work on a nonexistant Assembler or be reused for another machine!
    pub fn remove_assembler(&mut self, index: usize) -> AssemblerRemovalInfo {
        debug_assert!(!self.holes.contains(&index));
        self.holes.push(index);

        let ret = AssemblerRemovalInfo {
            ings: (0..NUM_INGS).map(|i| self.ings[i][index]).collect(),
            outputs: (0..NUM_OUTPUTS).map(|i| self.outputs[i][index]).collect(),
        };
        for ing in &mut self.ings {
            ing[index] = 0;
        }
        for out in &mut self.outputs {
            out[index] = ITEMCOUNTTYPE::MAX;
        }

        ret
    }

    pub fn add_assembler(&mut self) -> usize {
        // TODO: Is 0 the correct initial timer value?
        self.add_assembler_with_data(array::from_fn(|i| 0), array::from_fn(|i| 0), 0)
    }

    fn add_assembler_with_data(
        &mut self,
        ings: [ITEMCOUNTTYPE; NUM_INGS],
        out: [ITEMCOUNTTYPE; NUM_OUTPUTS],
        timer: TIMERTYPE,
    ) -> usize {
        let len = self.timers.len();
        debug_assert!(len % Simdtype::LEN == 0);

        for output in &self.outputs {
            debug_assert_eq!(output.len(), len);
        }

        for ing in &self.ings {
            debug_assert_eq!(ing.len(), len);
        }

        if let Some(hole_index) = self.holes.pop() {
            // TODO: This is scatter assemblers which are close around the vec which is bad for cache locality
            for (output, new_val) in self.outputs.iter_mut().zip(out) {
                output[hole_index] = new_val;
            }
            for (ing, new_val) in self.ings.iter_mut().zip(ings) {
                ing[hole_index] = new_val;
            }
            self.timers[hole_index] = 0;
            return hole_index;
        }

        if self.len == self.outputs[0].len() {
            // We need to grow
            // TODO: This works on the assumption that Vec::into_boxed_slice does not reallocate,
            //       If it does, this code is still correct but horribly slow
            let mut new_len = None;

            take_mut::take(&mut self.outputs[0], |output| {
                let mut output = output.into_vec();
                output.reserve(Simdtype::LEN);
                new_len = Some(output.capacity());
                output.into_boxed_slice()
            });

            let new_len = new_len.expect("closure did not run?!?!");

            // Resize all to that size
            // FIXME: take_mut means that if we panic inside, we will ABORT instead of unrolling!
            for output in &mut self.outputs {
                take_mut::take(output, |output| {
                    let mut output = output.into_vec();
                    output.resize(new_len, ITEMCOUNTTYPE::MAX);
                    output.into_boxed_slice()
                });
            }

            for ing in &mut self.ings {
                take_mut::take(ing, |ing| {
                    let mut ing = ing.into_vec();
                    ing.resize(new_len, 0);
                    ing.into_boxed_slice()
                });
            }

            take_mut::take(&mut self.timers, |timers| {
                let mut timers = timers.into_vec();
                timers.resize(new_len, 0);
                timers.into_boxed_slice()
            });
        }

        for (output, new_val) in self.outputs.iter_mut().zip(out) {
            output[self.len] = new_val;
        }
        for (ing, new_val) in self.ings.iter_mut().zip(ings) {
            ing[self.len] = new_val;
        }
        self.timers[self.len] = timer;

        self.len += 1;
        self.len - 1
    }
}

pub struct ZipArray<T, const N: usize> {
    pub(crate) array: [T; N],
}

pub fn zip_array<T: Iterator, const N: usize>(array: [T; N]) -> ZipArray<T, N> {
    ZipArray { array }
}

impl<T: Iterator, const N: usize> Iterator for ZipArray<T, N> {
    type Item = [T::Item; N];

    fn next(&mut self) -> Option<Self::Item> {
        self.array.each_mut().try_map(std::iter::Iterator::next)
    }
}

mod arrays {
    use std::{convert::TryInto, marker::PhantomData};

    use serde::{
        de::{SeqAccess, Visitor},
        ser::SerializeTuple,
        Deserialize, Deserializer, Serialize, Serializer,
    };
    pub fn serialize<S: Serializer, T: Serialize, const N: usize>(
        data: &[T; N],
        ser: S,
    ) -> Result<S::Ok, S::Error> {
        let mut s = ser.serialize_tuple(N)?;
        for item in data {
            s.serialize_element(item)?;
        }
        s.end()
    }

    struct ArrayVisitor<T, const N: usize>(PhantomData<T>);

    impl<'de, T, const N: usize> Visitor<'de> for ArrayVisitor<T, N>
    where
        T: Deserialize<'de>,
    {
        type Value = [T; N];

        fn expecting(&self, formatter: &mut std::fmt::Formatter) -> std::fmt::Result {
            formatter.write_str(&format!("an array of length {N}"))
        }

        #[inline]
        fn visit_seq<A>(self, mut seq: A) -> Result<Self::Value, A::Error>
        where
            A: SeqAccess<'de>,
        {
            // can be optimized using MaybeUninit
            let mut data = Vec::with_capacity(N);
            for _ in 0..N {
                match (seq.next_element())? {
                    Some(val) => data.push(val),
                    None => return Err(serde::de::Error::invalid_length(N, &self)),
                }
            }
            data.try_into().map_or_else(|_| unreachable!(), Ok)
        }
    }
    pub fn deserialize<'de, D, T, const N: usize>(deserializer: D) -> Result<[T; N], D::Error>
    where
        D: Deserializer<'de>,
        T: Deserialize<'de>,
    {
        deserializer.deserialize_tuple(N, ArrayVisitor::<T, N>(PhantomData))
    }
}
