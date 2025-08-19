use std::{array, i32, u8};

use crate::{
    assembler::MultiAssemblerStore as MultiAssemblerStoreTrait,
    data::{DataStore, ItemRecipeDir},
    frontend::world::Position,
    inserter::HAND_SIZE,
    item::{ITEMCOUNTTYPE, IdxTrait, Recipe, WeakIdxTrait},
    power::{
        Watt,
        power_grid::{IndexUpdateInfo, MAX_POWER_MULT, PowerGridEntity, PowerGridIdentifier},
    },
};
use itertools::Itertools;
use std::cmp::max;

use super::{
    AssemblerOnclickInfo, AssemblerRemovalInfo, PowerUsageInfo, Simdtype, TIMERTYPE, arrays,
};

#[cfg(feature = "client")]
use egui_show_info_derive::ShowInfo;
#[cfg(feature = "client")]
use get_size::GetSize;

// FIXME: We store the same slice length n times!
// TODO: Don´t clump update data and data for adding/removing assemblers together!

// FIXME: Using Boxed slices here is probably the main contributor to the time usage for building large power grids, since this means reallocation whenever we add assemblers!
#[cfg_attr(feature = "client", derive(ShowInfo), derive(GetSize))]
#[derive(Debug, Clone, serde::Serialize, serde::Deserialize)]
pub struct MultiAssemblerStore<
    RecipeIdxType: WeakIdxTrait,
    const NUM_INGS: usize,
    const NUM_OUTPUTS: usize,
> {
    pub recipe: Recipe<RecipeIdxType>,

    /// Base Crafting Speed in 5% increments
    /// i.e. 28 => 140% Crafting speed
    /// Maximum is 1275% Crafting Speed
    base_speed: Box<[u8]>,

    /// Crafting Speed in 5% increments
    /// i.e. 28 => 140% Crafting speed
    /// Maximum is 1275% Crafting Speed
    combined_speed_mod: Box<[u8]>,
    /// Bonus Productivity in %
    bonus_productivity: Box<[u8]>,
    /// Power Consumption in 5% increments
    /// i.e. 28 => 140% Crafting speed
    /// Maximum is 1275% x Base Power Consumption
    // TODO: We could just store base_power_consumption * power_consumption_modifier instead of doing this calculation every tick
    power_consumption_modifier: Box<[u8]>,

    raw_speed_mod: Box<[i16]>,
    raw_bonus_productivity: Box<[i16]>,
    raw_power_consumption_modifier: Box<[i16]>,

    // TODO: This can likely be smaller than full u64
    base_power_consumption: Box<[Watt]>,
    #[serde(with = "arrays")]
    ings: [Box<[ITEMCOUNTTYPE]>; NUM_INGS],
    #[serde(with = "arrays")]
    ings_max_insert: [Box<[ITEMCOUNTTYPE]>; NUM_INGS],
    #[serde(with = "arrays")]
    outputs: [Box<[ITEMCOUNTTYPE]>; NUM_OUTPUTS],
    timers: Box<[TIMERTYPE]>,
    prod_timers: Box<[TIMERTYPE]>,

    holes: Vec<usize>,
    positions: Box<[Position]>,
    types: Box<[u8]>,
    len: usize,
}

// TODO: Maybe also add a defragmentation routine to mend the ineffeciencies left by deconstruction large amounts of assemblers
impl<RecipeIdxType: IdxTrait, const NUM_INGS: usize, const NUM_OUTPUTS: usize>
    MultiAssemblerStore<RecipeIdxType, NUM_INGS, NUM_OUTPUTS>
{
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

    // TODO: Currently power demand and supply are offset by a single tick. Is this acceptable?
    /// # Panics
    /// If `power_mult` > `MAX_POWER_MULT` = 64
    pub fn update_branchless(
        &mut self,
        power_mult: u8,
        recipe_lookup: &[(usize, usize)],
        recipe_ings: &[[ITEMCOUNTTYPE; NUM_INGS]],
        recipe_outputs: &[[ITEMCOUNTTYPE; NUM_OUTPUTS]],
        times: &[TIMERTYPE],
    ) -> (Watt, u32, u32) {
        let (ing_idx, out_idx) = recipe_lookup[self.recipe.id.into()];

        let our_ings: &[ITEMCOUNTTYPE; NUM_INGS] = &recipe_ings[ing_idx];
        let our_outputs: &[ITEMCOUNTTYPE; NUM_OUTPUTS] = &recipe_outputs[out_idx];

        let mut times_ings_used = 0;
        let mut num_finished_crafts = 0;

        assert!(power_mult <= MAX_POWER_MULT);

        // TODO: Is this amount of accuracy enough?
        let increase: TIMERTYPE = (u32::from(power_mult) * u32::from(TIMERTYPE::MAX)
            / u32::from(MAX_POWER_MULT)
            / u32::from(times[self.recipe.id.into()]))
        .try_into()
        .unwrap_or(TIMERTYPE::MAX);

        let mut power = Watt(0);

        for (index, (timer, (prod_timer, (speed_mod, (bonus_prod, (base_power, power_mod)))))) in
            self.timers
                .iter_mut()
                .zip(
                    self.prod_timers.iter_mut().zip(
                        self.combined_speed_mod.iter().copied().zip(
                            self.bonus_productivity.iter().copied().zip(
                                self.base_power_consumption
                                    .iter()
                                    .copied()
                                    .zip(self.power_consumption_modifier.iter().copied()),
                            ),
                        ),
                    ),
                )
                .enumerate()
        {
            // ~~Remove the items from the ings at the start of the crafting process~~
            // We will do this as part of the frontend ui!

            let increase = (u32::from(increase) * u32::from(speed_mod) / 20) as u16;

            let mut ing_mul: u8 = 1;
            for i in 0..NUM_INGS {
                ing_mul *= u8::from(self.ings[i][index] >= our_ings[i]);
            }

            let mut ing_mul_for_two_crafts: u16 = 1;
            for i in 0..NUM_INGS {
                ing_mul_for_two_crafts *= u16::from(self.ings[i][index] >= our_ings[i] * 2);
            }

            let new_timer_output_space = timer.wrapping_add(increase * u16::from(ing_mul));
            let new_timer_output_full = timer.saturating_add(increase * u16::from(ing_mul));

            let mut space_mul: u8 = 1;
            for i in 0..NUM_OUTPUTS {
                space_mul *=
                    u8::from((self.outputs[i][index].saturating_add(our_outputs[i])) <= 100);
            }

            let new_timer = new_timer_output_space * u16::from(space_mul)
                + new_timer_output_full * (1 - u16::from(space_mul));

            let timer_mul: u8 = u8::from(new_timer < *timer);

            // if we have enough items for another craft keep the wrapped value, else clamp it to 0
            let new_timer = u16::from(timer_mul) * (ing_mul_for_two_crafts * new_timer)
                + (1 - u16::from(timer_mul)) * new_timer;

            let new_prod_timer = prod_timer.wrapping_add(
                (u32::from(new_timer.wrapping_sub(*timer)) * (bonus_prod as u32) / 100) as u16,
            );

            let prod_timer_mul: u8 = (new_prod_timer < *prod_timer).into();

            // Power calculation
            // We use power if any work was done
            power = power + base_power * u64::from(ing_mul * space_mul) * u64::from(power_mod) / 20;

            *timer = new_timer;
            *prod_timer = new_prod_timer;
            for i in 0..NUM_OUTPUTS {
                self.outputs[i][index] += (timer_mul + prod_timer_mul) * our_outputs[i];
            }
            for i in 0..NUM_INGS {
                self.ings[i][index] -= timer_mul * our_ings[i];
            }
            times_ings_used += u32::from(timer_mul);
            num_finished_crafts += u32::from(timer_mul + prod_timer_mul);
        }

        (power, times_ings_used, num_finished_crafts)
    }

    pub fn get_all_outputs_mut(&mut self) -> [&mut [ITEMCOUNTTYPE]; NUM_OUTPUTS] {
        self.outputs.each_mut().map(|b| &mut **b)
    }

    pub fn get_all_ings_mut(&mut self) -> [&mut [ITEMCOUNTTYPE]; NUM_INGS] {
        self.ings.each_mut().map(|b| &mut **b)
    }

    pub fn get_outputs_mut(&mut self, idx: u32) -> &mut [ITEMCOUNTTYPE] {
        &mut self.outputs[idx as usize]
    }

    pub fn get_ings_mut(&mut self, idx: u32) -> &mut [ITEMCOUNTTYPE] {
        &mut self.ings[idx as usize]
    }

    pub fn get_output_mut(&mut self, output_idx: usize, index: u32) -> Option<&mut ITEMCOUNTTYPE> {
        if (index as usize) < self.len {
            Some(&mut self.outputs[output_idx][index as usize])
        } else {
            None
        }
    }

    pub fn get_output(&self, output_idx: usize, index: u32) -> Option<&ITEMCOUNTTYPE> {
        if (index as usize) < self.len {
            Some(&self.outputs[output_idx][index as usize])
        } else {
            None
        }
    }

    pub fn get_ing_mut(&mut self, input_idx: usize, index: u32) -> Option<&mut ITEMCOUNTTYPE> {
        if (index as usize) < self.len {
            Some(&mut self.ings[input_idx][index as usize])
        } else {
            None
        }
    }
}

impl<RecipeIdxType: WeakIdxTrait, const NUM_INGS: usize, const NUM_OUTPUTS: usize>
    super::MultiAssemblerStore<RecipeIdxType, NUM_INGS, NUM_OUTPUTS>
    for MultiAssemblerStore<RecipeIdxType, NUM_INGS, NUM_OUTPUTS>
{
    fn new<ItemIdxType: IdxTrait>(
        recipe: Recipe<RecipeIdxType>,
        _data_store: &DataStore<ItemIdxType, RecipeIdxType>,
    ) -> Self {
        Self {
            recipe,

            ings_max_insert: array::from_fn(|_| vec![].into_boxed_slice()),
            ings: array::from_fn(|_| vec![].into_boxed_slice()),
            outputs: array::from_fn(|_| vec![].into_boxed_slice()),
            timers: vec![].into_boxed_slice(),
            prod_timers: vec![].into_boxed_slice(),

            base_speed: vec![].into_boxed_slice(),
            bonus_productivity: vec![].into_boxed_slice(),
            combined_speed_mod: vec![].into_boxed_slice(),
            power_consumption_modifier: vec![].into_boxed_slice(),

            raw_speed_mod: vec![].into_boxed_slice(),
            raw_bonus_productivity: vec![].into_boxed_slice(),
            raw_power_consumption_modifier: vec![].into_boxed_slice(),

            base_power_consumption: vec![].into_boxed_slice(),

            holes: vec![],
            positions: vec![].into_boxed_slice(),
            types: vec![].into_boxed_slice(),
            len: 0,
        }
    }

    fn modify_modifiers<ItemIdxType: IdxTrait>(
        &mut self,
        index: u32,
        speed: i16,
        prod: i16,
        power: i16,
        data_store: &DataStore<ItemIdxType, RecipeIdxType>,
    ) {
        let index = index as usize;
        self.raw_speed_mod[usize::from(index)] = self.raw_speed_mod[usize::from(index)]
            .checked_add(speed)
            .expect("Over/Underflowed");
        self.raw_bonus_productivity[usize::from(index)] = self.raw_bonus_productivity
            [usize::from(index)]
        .checked_add(prod)
        .expect("Over/Underflowed");
        self.raw_power_consumption_modifier[usize::from(index)] = self
            .raw_power_consumption_modifier[usize::from(index)]
        .checked_add(power)
        .expect("Over/Underflowed");

        self.power_consumption_modifier[usize::from(index)] =
            (self.raw_power_consumption_modifier[usize::from(index)] + 20)
                .clamp(data_store.min_power_mod.into(), u8::MAX.into())
                .try_into()
                .expect("Values already clamped");
        self.bonus_productivity[usize::from(index)] = self.raw_bonus_productivity
            [usize::from(index)]
        .clamp(0, u8::MAX.into())
        .try_into()
        .expect("Values already clamped");
        self.combined_speed_mod[usize::from(index)] = ((self.raw_speed_mod[usize::from(index)]
            + 20)
            * i16::from(self.base_speed[usize::from(index)])
            / 20)
            .clamp(0, u8::MAX.into())
            .try_into()
            .expect("Values already clamped");
    }

    fn get_recipe(&self) -> Recipe<RecipeIdxType> {
        self.recipe
    }

    fn get_info<ItemIdxType: IdxTrait>(
        &self,
        index: u32,
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
            prod_timer_percentage: f32::from(self.prod_timers[index as usize])
                / f32::from(TIMERTYPE::MAX),
            base_speed: f32::from(self.base_speed[index as usize]) * 0.05,
            speed_mod: f32::from(self.raw_speed_mod[index as usize]) * 0.05,
            prod_mod: f32::from(self.bonus_productivity[index as usize]) * 0.01,
            power_consumption_mod: f32::from(self.power_consumption_modifier[index as usize])
                * 0.05
                - 1.0,
            base_power_consumption: self.base_power_consumption[index as usize],
        }
    }

    fn join<ItemIdxType: IdxTrait>(
        mut self,
        other: Self,
        new_grid_id: PowerGridIdentifier,
        data_store: &DataStore<ItemIdxType, RecipeIdxType>,
    ) -> (
        Self,
        impl IntoIterator<Item = IndexUpdateInfo<ItemIdxType, RecipeIdxType>>,
    ) {
        #[cfg(debug_assertions)]
        {
            for (i, pos) in self.positions.iter().enumerate() {
                assert_eq!(
                    (self.holes.contains(&i) || i >= self.len),
                    (pos.x == i32::MAX)
                );
            }

            for (i, pos) in other.positions.iter().enumerate() {
                assert_eq!(
                    (other.holes.contains(&i) || i >= other.len),
                    (pos.x == i32::MAX)
                );
            }
        }

        let old_len_stored = self.positions.len();

        for i in self.len..old_len_stored {
            self.holes.push(i);
        }
        self.len = old_len_stored;

        let new_ings_max: [Box<[u8]>; NUM_INGS] = self
            .ings_max_insert
            .into_iter()
            .zip(other.ings_max_insert)
            .map(|(s, o)| {
                let mut s = s.into_vec();
                s.extend(
                    o.into_vec()
                        .into_iter()
                        .enumerate()
                        .take(other.len)
                        .filter(|(i, _)| !other.holes.contains(i))
                        .map(|(_, v)| v),
                );
                s.into_boxed_slice()
            })
            .collect_array()
            .unwrap();

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
                        .take(other.len)
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
                        .take(other.len)
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
                .take(other.len)
                .filter(|(i, _)| !other.holes.contains(i))
                .map(|(_, v)| v),
        );

        let mut new_prod_timers = self.prod_timers.into_vec();
        new_prod_timers.extend(
            other
                .prod_timers
                .into_vec()
                .into_iter()
                .enumerate()
                .take(other.len)
                .filter(|(i, _)| !other.holes.contains(i))
                .map(|(_, v)| v),
        );

        let mut new_base_speed = self.base_speed.into_vec();
        new_base_speed.extend(
            other
                .base_speed
                .into_vec()
                .into_iter()
                .enumerate()
                .take(other.len)
                .filter(|(i, _)| !other.holes.contains(i))
                .map(|(_, v)| v),
        );

        let mut new_speed = self.combined_speed_mod.into_vec();
        new_speed.extend(
            other
                .combined_speed_mod
                .into_vec()
                .into_iter()
                .enumerate()
                .take(other.len)
                .filter(|(i, _)| !other.holes.contains(i))
                .map(|(_, v)| v),
        );

        let mut new_prod = self.bonus_productivity.into_vec();
        new_prod.extend(
            other
                .bonus_productivity
                .into_vec()
                .into_iter()
                .enumerate()
                .take(other.len)
                .filter(|(i, _)| !other.holes.contains(i))
                .map(|(_, v)| v),
        );

        let mut new_base_power_consumption = self.base_power_consumption.into_vec();
        new_base_power_consumption.extend(
            other
                .base_power_consumption
                .into_vec()
                .into_iter()
                .enumerate()
                .take(other.len)
                .filter(|(i, _)| !other.holes.contains(i))
                .map(|(_, v)| v),
        );

        let mut new_power_consumption_modifier = self.power_consumption_modifier.into_vec();
        new_power_consumption_modifier.extend(
            other
                .power_consumption_modifier
                .into_vec()
                .into_iter()
                .enumerate()
                .take(other.len)
                .filter(|(i, _)| !other.holes.contains(i))
                .map(|(_, v)| v),
        );

        let mut new_raw_speed_mod = self.raw_speed_mod.into_vec();
        new_raw_speed_mod.extend(
            other
                .raw_speed_mod
                .into_vec()
                .into_iter()
                .enumerate()
                .take(other.len)
                .filter(|(i, _)| !other.holes.contains(i))
                .map(|(_, v)| v),
        );

        let mut new_raw_bonus_productivity = self.raw_bonus_productivity.into_vec();
        new_raw_bonus_productivity.extend(
            other
                .raw_bonus_productivity
                .into_vec()
                .into_iter()
                .enumerate()
                .take(other.len)
                .filter(|(i, _)| !other.holes.contains(i))
                .map(|(_, v)| v),
        );

        let mut new_raw_power_consumption_modifier = self.raw_power_consumption_modifier.into_vec();
        new_raw_power_consumption_modifier.extend(
            other
                .raw_power_consumption_modifier
                .into_vec()
                .into_iter()
                .enumerate()
                .take(other.len)
                .filter(|(i, _)| !other.holes.contains(i))
                .map(|(_, v)| v),
        );

        let mut new_positions = self.positions.into_vec();
        new_positions.extend(
            other
                .positions
                .iter()
                .copied()
                .enumerate()
                .take(other.len)
                .filter(|(i, _)| !other.holes.contains(i))
                .map(|(_, v)| v),
        );

        let mut new_types = self.types.into_vec();
        new_types.extend(
            other
                .types
                .iter()
                .copied()
                .enumerate()
                .take(other.len)
                .filter(|(i, _)| !other.holes.contains(i))
                .map(|(_, v)| v),
        );

        let updates = IntoIterator::into_iter(other.positions)
            .take(other.len)
            .zip(other.types)
            .enumerate()
            .take(other.len)
            .filter(move |(i, _)| !other.holes.contains(i))
            .enumerate()
            .map(move |(new_index_offs, (old_index, (pos, ty)))| {
                assert!(new_index_offs <= old_index);
                IndexUpdateInfo {
                    position: pos,
                    old_pg_entity: PowerGridEntity::Assembler {
                        ty,
                        recipe: self.recipe,
                        index: old_index.try_into().unwrap(),
                    },
                    new_pg_entity: PowerGridEntity::Assembler {
                        ty,
                        recipe: self.recipe,
                        index: (old_len_stored + new_index_offs).try_into().unwrap(),
                    },
                    new_grid: new_grid_id,
                }
            });

        // #[cfg(debug_assertions)]
        // let updates = {
        //     let updates = updates.collect::<Vec<_>>();

        //     assert_eq!(updates.len(), other.len - other.holes.len());

        //     assert!(updates.iter().all(|update| {
        //         let IndexUpdateInfo {
        //             new_pg_entity: PowerGridEntity::Assembler { index, .. },
        //             ..
        //         } = update
        //         else {
        //             unreachable!()
        //         };

        //         !self.holes.contains(&(*index as usize))
        //     }));

        //     assert_eq!(
        //         new_positions.len(),
        //         old_len_stored + (other.len - other.holes.len())
        //     );

        //     assert!(updates.iter().all(|update| {
        //         let IndexUpdateInfo {
        //             new_pg_entity: PowerGridEntity::Assembler { index, .. },
        //             ..
        //         } = update
        //         else {
        //             unreachable!()
        //         };

        //         new_positions[*index as usize].x < i32::MAX
        //     }));

        //     updates
        // };

        let ret = Self {
            recipe: self.recipe,
            ings_max_insert: new_ings_max,
            ings: new_ings,
            outputs: new_outputs,
            timers: new_timers.into(),
            prod_timers: new_prod_timers.into(),
            holes: self.holes,
            len: new_positions.len(),
            base_speed: new_base_speed.into_boxed_slice(),
            combined_speed_mod: new_speed.into_boxed_slice(),
            bonus_productivity: new_prod.into_boxed_slice(),
            power_consumption_modifier: new_power_consumption_modifier.into_boxed_slice(),

            raw_speed_mod: new_raw_speed_mod.into_boxed_slice(),
            raw_bonus_productivity: new_raw_bonus_productivity.into_boxed_slice(),
            raw_power_consumption_modifier: new_raw_power_consumption_modifier.into_boxed_slice(),

            base_power_consumption: new_base_power_consumption.into_boxed_slice(),
            positions: new_positions.into_boxed_slice(),
            types: new_types.into_boxed_slice(),
        };

        #[cfg(debug_assertions)]
        {
            for (i, pos) in ret.positions.iter().enumerate() {
                assert_eq!(
                    (ret.holes.contains(&i) || i >= ret.len),
                    (pos.x == i32::MAX)
                );
            }
        }

        (ret, updates)
    }

    fn do_single_tick_update<ItemIdxType: IdxTrait>(
        &mut self,
        power_mult: u8,
        recipe_lookup: &[(usize, usize)],
        recipe_ings: &[[ITEMCOUNTTYPE; NUM_INGS]],
        recipe_outputs: &[[ITEMCOUNTTYPE; NUM_OUTPUTS]],
        times: &[TIMERTYPE],
        _data_store: &DataStore<ItemIdxType, RecipeIdxType>,
    ) -> (PowerUsageInfo, u32, u32)
    where
        RecipeIdxType: IdxTrait,
    {
        let (power, ings_used, produced) = self.update_branchless(
            power_mult,
            recipe_lookup,
            recipe_ings,
            recipe_outputs,
            times,
        );

        (PowerUsageInfo::Combined(power), ings_used, produced)
    }

    fn get_all_mut(
        &mut self,
    ) -> (
        (
            [&[ITEMCOUNTTYPE]; NUM_INGS],
            [&mut [ITEMCOUNTTYPE]; NUM_INGS],
        ),
        [&mut [ITEMCOUNTTYPE]; NUM_OUTPUTS],
    ) {
        (
            (
                self.ings_max_insert.each_mut().map(|b| &**b),
                self.ings.each_mut().map(|b| &mut **b),
            ),
            self.outputs.each_mut().map(|b| &mut **b),
        )
    }

    fn add_assembler_with_data<ItemIdxType: IdxTrait>(
        &mut self,
        ings_max_insert: [ITEMCOUNTTYPE; NUM_INGS],
        ings: [ITEMCOUNTTYPE; NUM_INGS],
        out: [ITEMCOUNTTYPE; NUM_OUTPUTS],
        timer: TIMERTYPE,
        prod_timer: TIMERTYPE,
        power: Watt,
        power_consumption_modifier: i16,
        bonus_productiviy: i16,
        base_speed: u8,
        speed_mod: i16,
        ty: u8,
        position: Position,
        data_store: &DataStore<ItemIdxType, RecipeIdxType>,
    ) -> u32 {
        let len = self.timers.len();
        // debug_assert!(len % Simdtype::LEN == 0);

        for output in &self.outputs {
            debug_assert_eq!(output.len(), len);
        }

        for ing in &self.ings {
            debug_assert_eq!(ing.len(), len);
        }

        if let Some(hole_index) = self.holes.pop() {
            // TODO: This could scatter assemblers which are close around the vec which is bad for cache locality
            for (output, new_val) in self.outputs.iter_mut().zip(out) {
                output[hole_index] = new_val;
            }
            for (ing, new_val) in self.ings.iter_mut().zip(ings) {
                ing[hole_index] = new_val;
            }
            for (ing, new_val) in self.ings_max_insert.iter_mut().zip(ings_max_insert) {
                ing[hole_index] = new_val;
            }
            self.timers[hole_index] = timer;
            self.prod_timers[hole_index] = prod_timer;
            self.base_power_consumption[hole_index] = power;

            self.base_speed[hole_index] = base_speed;
            self.raw_power_consumption_modifier[hole_index] = power_consumption_modifier;
            self.raw_bonus_productivity[hole_index] = bonus_productiviy;
            self.raw_speed_mod[hole_index] = speed_mod;

            self.power_consumption_modifier[hole_index] = (power_consumption_modifier + 20)
                .clamp(data_store.min_power_mod.into(), u8::MAX.into())
                .try_into()
                .expect("Value clamped already");
            self.bonus_productivity[hole_index] = bonus_productiviy
                .clamp(0, u8::MAX.into())
                .try_into()
                .expect("Value clamped already");
            self.combined_speed_mod[hole_index] = ((speed_mod + 20) * i16::from(base_speed) / 20)
                .clamp(0, u8::MAX.into())
                .try_into()
                .expect("Value clamped already");

            self.types[hole_index] = ty;
            self.positions[hole_index] = position;
            return hole_index.try_into().unwrap();
        }

        if self.len == self.timers.len() {
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

            for ing in &mut self.ings_max_insert {
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

            take_mut::take(&mut self.prod_timers, |prod_timers| {
                let mut prod_timers = prod_timers.into_vec();
                prod_timers.resize(new_len, 0);
                prod_timers.into_boxed_slice()
            });

            take_mut::take(&mut self.base_power_consumption, |base_power_consumption| {
                let mut base_power_consumption = base_power_consumption.into_vec();
                base_power_consumption.resize(new_len, Watt(0));
                base_power_consumption.into_boxed_slice()
            });

            take_mut::take(
                &mut self.power_consumption_modifier,
                |power_consumption_modifier| {
                    let mut power_consumption_modifier = power_consumption_modifier.into_vec();
                    power_consumption_modifier.resize(new_len, 0);
                    power_consumption_modifier.into_boxed_slice()
                },
            );

            take_mut::take(&mut self.bonus_productivity, |bonus_productivity| {
                let mut bonus_productivity = bonus_productivity.into_vec();
                bonus_productivity.resize(new_len, 0);
                bonus_productivity.into_boxed_slice()
            });

            take_mut::take(&mut self.base_speed, |speed| {
                let mut speed = speed.into_vec();
                speed.resize(new_len, 0);
                speed.into_boxed_slice()
            });

            take_mut::take(&mut self.base_power_consumption, |base_power_consumption| {
                let mut base_power_consumption = base_power_consumption.into_vec();
                base_power_consumption.resize(new_len, Watt(0));
                base_power_consumption.into_boxed_slice()
            });

            take_mut::take(
                &mut self.raw_power_consumption_modifier,
                |power_consumption_modifier| {
                    let mut power_consumption_modifier = power_consumption_modifier.into_vec();
                    power_consumption_modifier.resize(new_len, 0);
                    power_consumption_modifier.into_boxed_slice()
                },
            );

            take_mut::take(&mut self.raw_bonus_productivity, |bonus_productivity| {
                let mut bonus_productivity = bonus_productivity.into_vec();
                bonus_productivity.resize(new_len, 0);
                bonus_productivity.into_boxed_slice()
            });

            take_mut::take(&mut self.raw_speed_mod, |speed| {
                let mut speed = speed.into_vec();
                speed.resize(new_len, 0);
                speed.into_boxed_slice()
            });

            take_mut::take(&mut self.combined_speed_mod, |speed| {
                let mut speed = speed.into_vec();
                speed.resize(new_len, 0);
                speed.into_boxed_slice()
            });

            take_mut::take(&mut self.combined_speed_mod, |speed| {
                let mut speed = speed.into_vec();
                speed.resize(new_len, 0);
                speed.into_boxed_slice()
            });

            take_mut::take(&mut self.positions, |pos| {
                let mut pos = pos.into_vec();
                pos.resize(
                    new_len,
                    Position {
                        x: i32::MAX,
                        y: i32::MAX,
                    },
                );
                pos.into_boxed_slice()
            });

            take_mut::take(&mut self.types, |ty| {
                let mut ty = ty.into_vec();
                ty.resize(new_len, u8::MAX);
                ty.into_boxed_slice()
            });
        }

        for (output, new_val) in self.outputs.iter_mut().zip(out) {
            output[self.len] = new_val;
        }
        for (ing, new_val) in self.ings.iter_mut().zip(ings) {
            ing[self.len] = new_val;
        }
        for (ing, new_val) in self.ings_max_insert.iter_mut().zip(ings_max_insert) {
            ing[self.len] = new_val;
        }
        self.timers[self.len] = timer;
        self.prod_timers[self.len] = prod_timer;
        self.base_power_consumption[self.len] = power;

        self.base_speed[self.len] = base_speed;
        self.raw_power_consumption_modifier[self.len] = power_consumption_modifier;
        self.raw_bonus_productivity[self.len] = bonus_productiviy;
        self.raw_speed_mod[self.len] = speed_mod;

        self.power_consumption_modifier[self.len] = (power_consumption_modifier + 20)
            .clamp(data_store.min_power_mod.into(), u8::MAX.into())
            .try_into()
            .expect("Values already clamped");
        self.bonus_productivity[self.len] = bonus_productiviy
            .clamp(0, u8::MAX.into())
            .try_into()
            .expect("Values already clamped");
        self.combined_speed_mod[self.len] = ((speed_mod + 20) * i16::from(base_speed) / 20)
            .clamp(0, u8::MAX.into())
            .try_into()
            .expect("Values already clamped");

        self.positions[self.len] = position;
        self.types[self.len] = ty;

        self.len += 1;
        (self.len - 1).try_into().unwrap()
    }

    fn remove_assembler_data<ItemIdxType: IdxTrait>(
        &mut self,
        index: u32,
        _data_store: &DataStore<ItemIdxType, RecipeIdxType>,
    ) -> (
        [ITEMCOUNTTYPE; NUM_INGS],
        [ITEMCOUNTTYPE; NUM_INGS],
        [ITEMCOUNTTYPE; NUM_OUTPUTS],
        TIMERTYPE,
        TIMERTYPE,
        Watt,
        i16,
        i16,
        u8,
        i16,
        u8,
        Position,
    ) {
        let index = index as usize;
        debug_assert!(!self.holes.contains(&index));
        self.holes.push(index);

        let data = (
            (0..NUM_INGS)
                .map(|i| self.ings_max_insert[i][index])
                .collect_array()
                .unwrap(),
            (0..NUM_INGS)
                .map(|i| self.ings[i][index])
                .collect_array()
                .unwrap(),
            (0..NUM_OUTPUTS)
                .map(|i| self.outputs[i][index])
                .collect_array()
                .unwrap(),
            self.timers[index],
            self.prod_timers[index],
            self.base_power_consumption[index],
            self.raw_power_consumption_modifier[index],
            self.raw_bonus_productivity[index],
            self.base_speed[index],
            self.raw_speed_mod[index],
            self.types[index],
            self.positions[index],
        );
        for ing in &mut self.ings {
            ing[index] = 0;
        }
        for out in &mut self.outputs {
            out[index] = ITEMCOUNTTYPE::MAX;
        }
        self.timers[index] = 0;
        self.prod_timers[index] = 0;
        self.base_power_consumption[index] = Watt(0);
        self.positions[index] = Position {
            x: i32::MAX,
            y: i32::MAX,
        };

        (
            data.0.into(),
            data.1.into(),
            data.2.into(),
            data.3.into(),
            data.4.into(),
            data.5.into(),
            data.6.into(),
            data.7.into(),
            data.8.into(),
            data.9.into(),
            data.10.into(),
            data.11.into(),
        )
    }
}
