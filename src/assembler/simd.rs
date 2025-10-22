use std::{
    array, i32,
    ops::{Add, Sub},
    simd::{
        Simd,
        cmp::SimdPartialOrd,
        num::{SimdInt, SimdUint},
    },
    u8,
};

use crate::{
    MASKTYPE, SIMDTYPE,
    data::{AssemblerInfo, DataStore, ItemRecipeDir},
    frontend::world::Position,
    item::{ITEMCOUNTTYPE, IdxTrait, Indexable, Recipe, WeakIdxTrait},
    power::{
        Watt,
        power_grid::{IndexUpdateInfo, MAX_POWER_MULT, PowerGridEntity, PowerGridIdentifier},
    },
};
use itertools::{Either, Itertools};

use super::{AssemblerOnclickInfo, PowerUsageInfo, Simdtype, TIMERTYPE, arrays};

#[cfg(feature = "client")]
use egui_show_info_derive::ShowInfo;
#[cfg(feature = "client")]
use get_size2::GetSize;

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

    single_type: Option<u8>,

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

        // FIXME: This is broken since it does not account for the fact that in the case  of the single_type optimization we do not have a list of base_power_consumption, leading to assembler just never being updated!
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

    pub fn update_explicit(
        &mut self,
        power_mult: u8,
        recipe_lookup: &[(usize, usize)],
        recipe_ings: &[[ITEMCOUNTTYPE; NUM_INGS]],
        recipe_outputs: &[[ITEMCOUNTTYPE; NUM_OUTPUTS]],
        recipe_maximums: &[[ITEMCOUNTTYPE; NUM_OUTPUTS]],
        times: &[TIMERTYPE],
        power_list: &[AssemblerInfo],
    ) -> (Watt, u32, u32) {
        let (ing_idx, out_idx) = recipe_lookup[self.recipe.id.into()];

        let our_ings: &[ITEMCOUNTTYPE; NUM_INGS] = &recipe_ings[ing_idx];
        let our_outputs: &[ITEMCOUNTTYPE; NUM_OUTPUTS] = &recipe_outputs[out_idx];
        let our_maximums: &[ITEMCOUNTTYPE; NUM_OUTPUTS] = &recipe_maximums[out_idx];

        let mut times_ings_used: u32 = 0;
        let mut num_finished_crafts: u32 = 0;

        assert!(power_mult <= MAX_POWER_MULT);

        // TODO: Is this amount of accuracy enough?
        let power_level_recipe_increase: TIMERTYPE = (u32::from(power_mult)
            * u32::from(TIMERTYPE::MAX)
            / u32::from(MAX_POWER_MULT)
            / u32::from(times[self.recipe.id.into()]))
        .try_into()
        .unwrap_or(TIMERTYPE::MAX);

        let mut power = Simd::<u64, 16>::splat(0);

        let mut power_const_type: u32 = 0;

        let power_level_recipe_increase = SIMDTYPE::splat(power_level_recipe_increase);

        // FIXME: THIS IS SILLY
        let arr = match self.single_type {
            Some(ty) => [power_list[ty as usize].base_power_consumption; { SIMDTYPE::LEN }],
            None => [Watt(0); { SIMDTYPE::LEN }],
        };

        let power_iter = match self.single_type {
            Some(_ty) => Either::Left(std::iter::repeat(&arr)),
            None => Either::Right(self.base_power_consumption.as_chunks().0.into_iter()),
        };

        for (
            index,
            (timer_arr, (prod_timer_arr, (speed_mod, (bonus_prod, (base_power, power_mod))))),
        ) in self
            .timers
            .as_chunks_mut::<{ SIMDTYPE::LEN }>()
            .0
            .into_iter()
            .zip(self.prod_timers.as_chunks_mut().0.into_iter().zip(
                self.combined_speed_mod.as_chunks().0.into_iter().zip(
                    self.bonus_productivity.as_chunks().0.into_iter().zip(
                        power_iter.zip(self.power_consumption_modifier.as_chunks().0.into_iter()),
                    ),
                ),
            ))
            .enumerate()
        {
            let index = index * 16;
            // ~~Remove the items from the ings at the start of the crafting process~~
            // We will do this as part of the frontend ui!

            let speed_mod = Simd::<u8, 16>::from_array(*speed_mod);
            let speed_mod: SIMDTYPE = speed_mod.cast();

            let increase: SIMDTYPE = (power_level_recipe_increase.cast::<u32>()
                * speed_mod.cast::<u32>()
                / Simd::<u32, 16>::splat(20))
            .cast();

            let mut ing_mask: MASKTYPE = MASKTYPE::splat(true);
            for i in 0..NUM_INGS {
                let ings: Simd<u8, 16> = Simd::<u8, 16>::from_slice(&self.ings[i][index..]);
                let ings = ings.cast();
                let our_ings = SIMDTYPE::splat(our_ings[i].into());
                ing_mask = ing_mask & ings.simd_ge(our_ings);
            }

            if !ing_mask.any() {
                continue;
            }

            let mut ing_mask_for_two_crafts: MASKTYPE = MASKTYPE::splat(true);
            for i in 0..NUM_INGS {
                let ings: Simd<u8, 16> = Simd::<u8, 16>::from_slice(&self.ings[i][index..]);
                let ings = ings.cast();
                let our_ings_two_crafts = SIMDTYPE::splat((our_ings[i] * 2u8).into());
                ing_mask_for_two_crafts =
                    ing_mask_for_two_crafts & ings.simd_ge(our_ings_two_crafts);
            }

            let timer = SIMDTYPE::from_array(*timer_arr);

            let new_timer_output_space = ing_mask.select(timer + increase, timer);
            let new_timer_output_full = ing_mask.select(timer.saturating_add(increase), timer);

            let mut space_mask: MASKTYPE = MASKTYPE::splat(true);
            for i in 0..NUM_OUTPUTS {
                let outputs: Simd<u8, 16> = Simd::<u8, 16>::from_slice(&self.outputs[i][index..]);
                let outputs = outputs.cast();
                let our_outputs = SIMDTYPE::splat(our_outputs[i].into());
                space_mask = space_mask
                    & (SimdUint::saturating_add(outputs, our_outputs))
                        .simd_le(SIMDTYPE::splat(our_maximums[i].into()));
            }

            let new_timer = space_mask.select(new_timer_output_space, new_timer_output_full);

            // Power calculation
            // We use power if any work was done
            let uses_power =
                ing_mask & (space_mask | timer.simd_lt(SIMDTYPE::splat(TIMERTYPE::MAX)));
            if self.single_type.is_some() {
                power_const_type += u32::from(
                    uses_power
                        .cast()
                        .select(Simd::<u8, 16>::from(*power_mod).cast(), SIMDTYPE::splat(0))
                        .reduce_sum(),
                );
            } else {
                let base_power = Simd::<u64, 16>::from_array(base_power.map(|Watt(v)| v));
                power = uses_power.cast().select(
                    power
                        + base_power * Simd::<u8, 16>::from(*power_mod).cast()
                            / Simd::<u64, 16>::splat(20),
                    power,
                );
            }

            if timer == new_timer {
                continue;
            }

            let timer_mask: MASKTYPE = new_timer.simd_lt(timer);

            // if we have enough items for another craft keep the wrapped value (This improves the accuracy), else clamp it to 0
            let new_timer =
                (!timer_mask | ing_mask_for_two_crafts).select(new_timer, SIMDTYPE::splat(0));

            let prod_timer = SIMDTYPE::from_array(*prod_timer_arr);
            let bonus_prod = Simd::<u8, 16>::from_array(*bonus_prod);
            // This needs be calculated in u32 to prevent overflows in intermediate values
            let progress = (new_timer.sub(timer)).cast::<u32>();
            let new_prod_timer: SIMDTYPE = prod_timer.add(SimdUint::cast::<u16>(
                progress * SimdUint::cast::<u32>(bonus_prod) / Simd::<u32, 16>::splat(100),
            ));

            let prod_timer_mask: MASKTYPE = new_prod_timer.simd_lt(prod_timer);

            *timer_arr = new_timer.to_array();
            *prod_timer_arr = new_prod_timer.to_array();
            if timer_mask.any() || prod_timer_mask.any() {
                for i in 0..NUM_OUTPUTS {
                    let our_outputs = SIMDTYPE::splat(our_outputs[i].into());
                    let outputs: Simd<u8, 16> =
                        Simd::<u8, 16>::from_slice(&self.outputs[i][index..]);
                    let outputs = outputs.cast();
                    let int_output = timer_mask.select(outputs + our_outputs, outputs);
                    self.outputs[i][index..(index + 16)].copy_from_slice(
                        (prod_timer_mask.select(int_output + our_outputs, int_output))
                            .cast()
                            .as_array(),
                    );
                }
            }
            if timer_mask.any() {
                for i in 0..NUM_INGS {
                    let ings: Simd<u8, 16> = Simd::<u8, 16>::from_slice(&self.ings[i][index..]);
                    let ings = ings.cast();
                    let our_ings = SIMDTYPE::splat(our_ings[i].into());
                    self.ings[i][index..(index + 16)].copy_from_slice(
                        timer_mask.select(ings - our_ings, ings).cast().as_array(),
                    );
                }
            }
            times_ings_used += (timer_mask.to_int().reduce_sum() * -1) as u32;
            num_finished_crafts += ((timer_mask.to_int().reduce_sum()
                + prod_timer_mask.to_int().reduce_sum())
                * -1) as u32;
        }

        let total_power = if let Some(single_ty) = self.single_type {
            power_list[single_ty as usize].base_power_consumption * (power_const_type as u64) / 20
        } else {
            Watt(power.reduce_sum())
        };
        (total_power, times_ings_used, num_finished_crafts)
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

            single_type: None,

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
        let items = data_store
            .recipe_to_items
            .get(self.recipe.into_usize())
            .unwrap();

        let power_consumption = if let Some(ty) = self.single_type {
            data_store.assembler_info[ty as usize].base_power_consumption
        } else {
            assert!(self.positions.get(index as usize).is_some());
            self.base_power_consumption[index as usize]
        };

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
            base_power_consumption: power_consumption,
        }
    }

    fn join<ItemIdxType: IdxTrait>(
        mut self,
        mut other: Self,
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

        let new_single_type = match (self.single_type, other.single_type) {
            (None, v) => {
                if self.len == 0 {
                    v
                } else {
                    None
                }
            },
            (v, None) => {
                if other.len == 0 {
                    v
                } else {
                    None
                }
            },
            (Some(a_ty), Some(b_ty)) => {
                if a_ty == b_ty {
                    Some(a_ty)
                } else {
                    None
                }
            },
        };

        if new_single_type != self.single_type
            && let Some(ty) = self.single_type
        {
            self.base_power_consumption = vec![
                data_store.assembler_info[ty as usize]
                    .base_power_consumption;
                self.positions.len()
            ]
            .into_boxed_slice();
        }

        if new_single_type != other.single_type
            && let Some(ty) = other.single_type
        {
            other.base_power_consumption = vec![
                data_store.assembler_info[ty as usize]
                    .base_power_consumption;
                self.positions.len()
            ]
            .into_boxed_slice();
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
            single_type: new_single_type,

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
        recipe_maximums: &[[ITEMCOUNTTYPE; NUM_OUTPUTS]],
        times: &[TIMERTYPE],
        data_store: &DataStore<ItemIdxType, RecipeIdxType>,
    ) -> (PowerUsageInfo, u32, u32)
    where
        RecipeIdxType: IdxTrait,
    {
        let (power, ings_used, produced) = self.update_explicit(
            power_mult,
            recipe_lookup,
            recipe_ings,
            recipe_outputs,
            recipe_maximums,
            times,
            &data_store.assembler_info,
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
        if let Some(single_type) = self.single_type {
            if single_type != ty {
                self.single_type = None;

                self.base_power_consumption = vec![
                    data_store.assembler_info[single_type as usize]
                        .base_power_consumption;
                    self.positions.len()
                ]
                .into_boxed_slice();
            } else {
                // The new inserter matches the single_type of this
            }
        } else if self.len - self.holes.len() == 0 {
            // This is the only assembler
            self.single_type = Some(ty);
        } else {
            // We have multiple types already
        }

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
            if self.single_type.is_none() {
                self.base_power_consumption[hole_index] = power;
            }

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

            if self.single_type.is_none() {
                take_mut::take(&mut self.base_power_consumption, |base_power_consumption| {
                    let mut base_power_consumption = base_power_consumption.into_vec();
                    base_power_consumption.resize(new_len, Watt(0));
                    base_power_consumption.into_boxed_slice()
                });
            }

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
        if self.single_type.is_none() {
            self.base_power_consumption[self.len] = power;
        }

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
        data_store: &DataStore<ItemIdxType, RecipeIdxType>,
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
            if self.single_type.is_none() {
                self.base_power_consumption[index]
            } else {
                data_store.assembler_info[self.single_type.unwrap() as usize].base_power_consumption
            },
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
        if self.single_type.is_none() {
            self.base_power_consumption[index] = Watt(0);
        }
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

#[cfg(test)]
mod test {
    use ::test::{Bencher, black_box};
    use rand::Rng;
    use rayon::iter::{IndexedParallelIterator, IntoParallelRefMutIterator, ParallelIterator};

    use crate::{DATA_STORE, assembler::MultiAssemblerStore as MultiAssemblerStoreTrait};

    use super::*;

    #[bench]
    fn bench_multithreaded_assembler_update(bencher: &mut Bencher) {
        const NUM_RECIPES: usize = 12;
        const NUM_ASSEMBLERS: usize = 30_000_000;

        let mut assemblers: Vec<MultiAssemblerStore<u8, 1, 1>> = (0..NUM_RECIPES as u8)
            .map(|_| MultiAssemblerStore::new(Recipe { id: 11 }, &DATA_STORE))
            .collect_vec();

        let items: Vec<Vec<_>> = vec![
            rayon::iter::repeat(rand::thread_rng().gen_range(250..=255))
                .flat_map_iter(|v| std::iter::repeat_n(v, 10))
                .take_any(NUM_ASSEMBLERS)
                .collect();
            NUM_RECIPES
        ];
        assemblers
            .par_iter_mut()
            .zip(items)
            .for_each(|(assembler_store, item)| {
                for i in 0..NUM_ASSEMBLERS {
                    let _index = assembler_store.add_assembler_with_data(
                        [255],
                        [item[i]],
                        [0],
                        rand::random(),
                        rand::random(),
                        Watt(100_000),
                        00,
                        20,
                        20,
                        20,
                        0,
                        Position { x: 0, y: 0 },
                        &DATA_STORE,
                    );
                }
            });

        let mut i: u32 = 0;
        let mut num_produced = 0;
        bencher.iter(|| {
            let outputs: Vec<_> = assemblers
                .par_iter_mut()
                .map(|assembler_store| {
                    assembler_store.update_explicit(
                        64,
                        &DATA_STORE.recipe_index_lookups,
                        &DATA_STORE.recipe_ings.ing1,
                        &DATA_STORE.recipe_outputs.out1,
                        &[[100; 1]; NUM_RECIPES],
                        &DATA_STORE.recipe_timers,
                        &DATA_STORE.assembler_info,
                    )
                })
                .collect();

            num_produced += outputs.iter().map(|(_, _, v)| v).copied().sum::<u32>();
            black_box(outputs);

            i += 1;

            if i % 2_500 == 0 {
                assemblers.par_iter_mut().for_each(|assembler_store| {
                    let [ing] = assembler_store.get_all_ings_mut();

                    for ing in ing {
                        *ing = 255;
                    }

                    let [out] = assembler_store.get_all_outputs_mut();

                    for out in out {
                        *out = 0;
                    }
                });
            }
        });

        dbg!(i);
        dbg!(num_produced);
        dbg!(assemblers[11].get_info(0, &DATA_STORE));
    }
}
