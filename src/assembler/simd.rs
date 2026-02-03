use crate::belt::belt::BeltLenType;
use std::{
    array,
    cmp::min,
    i32,
    num::NonZero,
    ops::{Add, Sub},
    simd::{
        Simd,
        cmp::{SimdPartialEq, SimdPartialOrd},
        num::{SimdInt, SimdUint},
    },
    u8,
};

use crate::{
    MASKTYPE, SIMDTYPE,
    data::{AssemblerInfo, DataStore, ItemRecipeDir},
    frontend::world::Position,
    inserter::{FakeUnionStorage, Storage, storage_storage_with_buckets_indirect::InserterId},
    item::{ITEMCOUNTTYPE, IdxTrait, Indexable, Item, Recipe, WeakIdxTrait},
    power::{
        Watt,
        power_grid::{IndexUpdateInfo, MAX_POWER_MULT, PowerGridEntity, PowerGridIdentifier},
    },
    storage_list::{MaxInsertionLimit, PANIC_ON_INSERT},
};
use itertools::{Either, Itertools};
use static_assertions::const_assert;

use super::{AssemblerOnclickInfo, PowerUsageInfo, Simdtype, TIMERTYPE, arrays};

#[cfg(feature = "client")]
use egui_show_info_derive::ShowInfo;
#[cfg(feature = "client")]
use get_size2::GetSize;

const WAITLIST_LEN: usize = 3;

#[cfg_attr(feature = "client", derive(ShowInfo), derive(GetSize))]
#[derive(Debug, Clone, Default, serde::Serialize, serde::Deserialize)]
#[repr(align(64))]
pub struct InserterWaitList {
    pub(crate) inserters: [Option<InserterWithBelts>; WAITLIST_LEN],
}

const_assert!(std::mem::size_of::<InserterWaitList>() <= 64);

#[cfg_attr(feature = "client", derive(ShowInfo), derive(GetSize))]
#[derive(Debug, Clone, serde::Serialize, serde::Deserialize)]
pub struct Inserter {
    // item: u8,
    // TODO: This is not needed for assemblers, just for chests.
    pub self_is_source: bool,
    // We track the hand here so we avoid having to reinsert them each time the assembler produces anything
    // This does mean we can only fit 3 Inserters per cacheline :/
    // This is fixed by the item arena optimization
    pub current_hand: ITEMCOUNTTYPE,
    pub max_hand: NonZero<ITEMCOUNTTYPE>,
    pub movetime: u16,
    pub(crate) index: InserterId,
    pub other: FakeUnionStorage,
}

const_assert!(std::mem::size_of::<Option<InserterWithBelts>>() <= 20);
const_assert!(std::mem::size_of::<InserterWithBelts>() <= 20);

#[cfg_attr(feature = "client", derive(ShowInfo), derive(GetSize))]
#[derive(Debug, Clone, serde::Serialize, serde::Deserialize)]
pub(crate) struct InserterWithBelts {
    pub(crate) current_hand: ITEMCOUNTTYPE,
    pub(crate) max_hand: ITEMCOUNTTYPE,

    pub(crate) rest: InserterWithBeltsEnum,
    pub(crate) movetime: NonZero<u16>,
}

#[cfg_attr(feature = "client", derive(ShowInfo), derive(GetSize))]
#[derive(Debug, Clone, serde::Serialize, serde::Deserialize)]
pub(crate) enum InserterWithBeltsEnum {
    StorageStorage {
        // TODO: This is not needed for assemblers, just for chests.
        self_is_source: bool,
        index: InserterId,

        other: FakeUnionStorage,
    },
    BeltStorage {
        self_is_source: bool,
        belt_id: u32,
        belt_pos: u16,
    },
}

#[derive(Debug)]
pub struct InserterReinsertionInfo<ItemIdxType: WeakIdxTrait> {
    pub movetime: NonZero<u16>,
    pub item: Item<ItemIdxType>,
    pub current_hand: ITEMCOUNTTYPE,
    pub max_hand: ITEMCOUNTTYPE,

    pub(crate) conn: Conn,
}

#[derive(Debug)]
pub enum Conn {
    Storage {
        index: InserterId,
        storage_id_in: FakeUnionStorage,
        storage_id_out: FakeUnionStorage,
    },
    Belt {
        belt_id: u32,
        belt_pos: BeltLenType,
        self_storage: FakeUnionStorage,
        self_is_source: bool,
    },
}

#[cfg_attr(feature = "client", derive(ShowInfo), derive(GetSize))]
#[derive(Debug, Clone)]
struct InternalInserterReinsertionInfo {
    pub movetime: NonZero<u16>,
    pub item: u8,
    pub max_hand: ITEMCOUNTTYPE,
    pub self_index: u32,

    pub(crate) rest: Rest,
}

#[cfg_attr(feature = "client", derive(ShowInfo), derive(GetSize))]
#[derive(Debug, Clone)]
enum Rest {
    Storage {
        index: InserterId,
        other: FakeUnionStorage,
    },
    Belt {
        belt_id: u32,
        belt_pos: BeltLenType,
        self_is_source: bool,
    },
}

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

    // FIXME: For me to be able to add inserters to waitlists during parallel/per item updates,
    //        We would need a list of waitlists per ING and OUTPUT item
    //        This is a pretty big commitment in terms of memory
    #[serde(with = "arrays")]
    waitlists_ings: [Box<[InserterWaitList]>; NUM_INGS],
    #[serde(with = "arrays")]
    waitlists_ings_needed: [Box<[ITEMCOUNTTYPE]>; NUM_INGS],
    #[serde(with = "arrays")]
    waitlists_outputs: [Box<[InserterWaitList]>; NUM_OUTPUTS],
    #[serde(with = "arrays")]
    waitlists_outputs_needed: [Box<[ITEMCOUNTTYPE]>; NUM_OUTPUTS],
    holes: Vec<usize>,

    #[cfg(feature = "assembler-craft-tracking")]
    number_of_crafts_finished: Vec<u32>,

    positions: Box<[Position]>,
    types: Box<[u8]>,
    len: usize,

    #[serde(skip)]
    inserter_waitlist_output_vec: Vec<InternalInserterReinsertionInfo>,
    #[serde(with = "arrays")]
    self_fake_union_ing: [FakeUnionStorage; NUM_INGS],
    #[serde(with = "arrays")]
    self_fake_union_out: [FakeUnionStorage; NUM_OUTPUTS],
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
        // FIXME: This could technically not be enough if enough items are produced in a single tick
        // self.inserter_waitlist_output_vec.reserve(
        //     (self.len * 4 * (NUM_INGS + NUM_OUTPUTS))
        //         .saturating_sub(self.inserter_waitlist_output_vec.len()),
        // );

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

        let (timers, overlap) = self.timers.as_chunks_mut::<{ SIMDTYPE::LEN }>();

        // FIXME:
        assert!(overlap.is_empty());

        for (
            index,
            (timer_arr, (prod_timer_arr, (speed_mod, (bonus_prod, (base_power, power_mod))))),
        ) in timers
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
            // let timer = SIMDTYPE::from_array(*timer_arr);
            // let stopped_empty = dbg!(timer).simd_eq(SIMDTYPE::splat(0));
            // let stopped_full = timer.simd_eq(SIMDTYPE::splat(TIMERTYPE::MAX));

            // let speed_mod = Simd::<u8, 16>::from_array(*speed_mod);
            // let increase: SIMDTYPE = (power_level_recipe_increase.cast::<u32>()
            //     * speed_mod.cast::<u32>()
            //     / Simd::<u32, 16>::splat(20))
            // .cast();
            // let new_timer_assuming_free_run = timer + increase;
            // let timer_mask_assuming_free_run: MASKTYPE = new_timer_assuming_free_run.simd_lt(timer);
            // let progress = (new_timer_assuming_free_run.sub(timer)).cast::<u32>();
            // let prod_timer = SIMDTYPE::from_array(*prod_timer_arr);
            // let bonus_prod = Simd::<u8, 16>::from_array(*bonus_prod);
            // let new_prod_timer_assuming_free_run: SIMDTYPE = prod_timer.add(SimdUint::cast::<u16>(
            //     progress * SimdUint::cast::<u32>(bonus_prod) / Simd::<u32, 16>::splat(100),
            // ));
            // let prod_timer_mask: MASKTYPE = if bonus_prod
            //     .simd_gt(Simd::<u8, { SIMDTYPE::LEN }>::splat(0))
            //     .any()
            // {
            //     let prod_timer = SIMDTYPE::from_array(*prod_timer_arr);
            //     // This needs be calculated in u32 to prevent overflows in intermediate values
            //     let progress = (new_timer_assuming_free_run.sub(timer)).cast::<u32>();
            //     let new_prod_timer: SIMDTYPE = prod_timer.add(SimdUint::cast::<u16>(
            //         progress * SimdUint::cast::<u32>(bonus_prod) / Simd::<u32, 16>::splat(100),
            //     ));

            //     *prod_timer_arr = new_prod_timer.to_array();

            //     new_prod_timer.simd_lt(prod_timer)
            // } else {
            //     MASKTYPE::splat(false)
            // };
            // if !stopped_empty.any()
            //     && !stopped_full.any()
            //     && !timer_mask_assuming_free_run.any()
            //     && !prod_timer_mask.any()
            // {
            //     *timer_arr = new_timer_assuming_free_run.to_array();
            //     *prod_timer_arr = new_prod_timer_assuming_free_run.to_array();
            //     continue;
            // }

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

            if timer == new_timer {
                continue;
            }

            // Power calculation
            // We use power if any work was done
            let uses_power = timer.simd_ne(new_timer);
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

            let timer_mask: MASKTYPE = new_timer.simd_lt(timer);
            // if we have enough items for another craft keep the wrapped value (This improves the accuracy), else clamp it to 0
            let new_timer =
                (!timer_mask | ing_mask_for_two_crafts).select(new_timer, SIMDTYPE::splat(0));
            *timer_arr = new_timer.to_array();
            let bonus_prod = Simd::<u8, 16>::from_array(*bonus_prod);
            let prod_timer_mask: MASKTYPE = if bonus_prod
                .simd_gt(Simd::<u8, { SIMDTYPE::LEN }>::splat(0))
                .any()
            {
                let prod_timer = SIMDTYPE::from_array(*prod_timer_arr);
                // This needs be calculated in u32 to prevent overflows in intermediate values
                let progress = (new_timer.sub(timer)).cast::<u32>();
                let new_prod_timer: SIMDTYPE = prod_timer.add(SimdUint::cast::<u16>(
                    progress * SimdUint::cast::<u32>(bonus_prod) / Simd::<u32, 16>::splat(100),
                ));

                *prod_timer_arr = new_prod_timer.to_array();

                new_prod_timer.simd_lt(prod_timer)
            } else {
                MASKTYPE::splat(false)
            };

            if timer_mask.any() || prod_timer_mask.any() {
                // for i in 0..NUM_OUTPUTS {
                //     let our_outputs = SIMDTYPE::splat(our_outputs[i].into());
                //     let outputs: Simd<u8, 16> =
                //         Simd::<u8, 16>::from_slice(&self.outputs[i][index..]);
                //     let outputs = outputs.cast();
                //     let int_output = timer_mask.select(outputs + our_outputs, outputs);
                //     self.outputs[i][index..(index + 16)].copy_from_slice(
                //         (prod_timer_mask.select(int_output + our_outputs, int_output))
                //             .cast()
                //             .as_array(),
                //     );
                // }

                // FIXME: We are missing production if main and prod tick are at the same time!
                for (i, (has_produced_base, has_produced_prod)) in timer_mask
                    .to_array()
                    .into_iter()
                    .zip(prod_timer_mask.to_array().into_iter())
                    .enumerate()
                {
                    if has_produced_base || has_produced_prod {
                        let final_idx = index + i;
                        #[cfg(feature = "assembler-craft-tracking")]
                        {
                            self.number_of_crafts_finished[final_idx] +=
                                u32::from(has_produced_base) + u32::from(has_produced_prod);
                        }

                        let mut items = our_outputs.clone();

                        for (item, (out, items_to_distribute)) in self
                            .waitlists_outputs
                            .iter_mut()
                            .zip(&mut items)
                            .enumerate()
                        {
                            if *items_to_distribute + self.outputs[item][final_idx]
                                >= min(
                                    self.waitlists_outputs_needed[item][final_idx],
                                    our_maximums[item],
                                )
                            {
                                *items_to_distribute += self.outputs[item][final_idx];
                                self.outputs[item][final_idx] = 0;
                                for idx in 0..WAITLIST_LEN {
                                    let ins = &mut out[final_idx].inserters[idx];
                                    if let Some(v) = ins {
                                        let amount_taken_by_this_inserter = min(
                                            *items_to_distribute,
                                            ITEMCOUNTTYPE::from(v.max_hand) - v.current_hand,
                                        );
                                        if v.current_hand + amount_taken_by_this_inserter
                                            == ITEMCOUNTTYPE::from(v.max_hand)
                                        {
                                            let ins = ins.take().unwrap();
                                            for move_left_idx in idx..WAITLIST_LEN {
                                                out[final_idx].inserters[move_left_idx] = out
                                                    [final_idx]
                                                    .inserters
                                                    .get_mut(move_left_idx + 1)
                                                    .map(|v| v.take())
                                                    .unwrap_or(None);
                                            }
                                            let () =
                                                self.inserter_waitlist_output_vec
                                                    .push(InternalInserterReinsertionInfo {
                                                    movetime: ins.movetime.into(),
                                                    item: (NUM_INGS + item) as u8,
                                                    max_hand: ins.max_hand.into(),
                                                    self_index: final_idx as u32,
                                                    rest: match ins.rest {
                                                        InserterWithBeltsEnum::StorageStorage {
                                                            self_is_source: _,
                                                            index,
                                                            other,
                                                        } => Rest::Storage { index, other },
                                                        InserterWithBeltsEnum::BeltStorage {
                                                            belt_id,
                                                            belt_pos,
                                                            self_is_source,
                                                        } => Rest::Belt {
                                                            belt_id,
                                                            belt_pos,
                                                            self_is_source,
                                                        },
                                                    },
                                                })
                                            else {
                                                panic!(
                                                    "Not enough space in inserter readdition vec. Capacity is {}",
                                                    self.inserter_waitlist_output_vec.capacity()
                                                );
                                            };
                                            self.waitlists_outputs_needed[item][final_idx] =
                                                out[final_idx].inserters[0]
                                                    .as_ref()
                                                    .map(|ins| ins.max_hand - ins.current_hand)
                                                    .unwrap_or(ITEMCOUNTTYPE::MAX);
                                        } else {
                                            v.current_hand += amount_taken_by_this_inserter;
                                        }
                                        *items_to_distribute -= amount_taken_by_this_inserter;

                                        // TODO: Check if this is good or bad
                                        if *items_to_distribute == 0 {
                                            break;
                                        }
                                    }
                                }
                            }

                            if *items_to_distribute > 0 {
                                self.outputs[item][final_idx] += *items_to_distribute;
                            }
                        }
                    }
                }
            }
            if timer_mask.any() {
                // for i in 0..NUM_INGS {
                //     let ings: Simd<u8, 16> = Simd::<u8, 16>::from_slice(&self.ings[i][index..]);
                //     let ings = ings.cast();
                //     let our_ings = SIMDTYPE::splat(our_ings[i].into());
                //     self.ings[i][index..(index + 16)].copy_from_slice(
                //         timer_mask.select(ings - our_ings, ings).cast().as_array(),
                //     );
                // }

                let has_consumed = timer_mask;
                for (i, has_consumed) in has_consumed.to_array().into_iter().enumerate() {
                    if has_consumed {
                        let final_idx = index + i;
                        let mut items = our_ings.clone();

                        for (item, (ing, items_to_drain)) in
                            self.waitlists_ings.iter_mut().zip(&mut items).enumerate()
                        {
                            if *items_to_drain
                                + (self.ings_max_insert[item][final_idx]
                                    - self.ings[item][final_idx])
                                >= self.waitlists_ings_needed[item][final_idx]
                            {
                                *items_to_drain += self.ings_max_insert[item][final_idx]
                                    - self.ings[item][final_idx];
                                self.ings[item][final_idx] = self.ings_max_insert[item][final_idx];

                                for idx in 0..WAITLIST_LEN {
                                    let ins = &mut ing[final_idx].inserters[idx];
                                    if let Some(v) = ins {
                                        let amount_taken_by_this_inserter =
                                            min(*items_to_drain, v.current_hand);
                                        if v.current_hand - amount_taken_by_this_inserter == 0 {
                                            let ins = ins.take().unwrap();
                                            for move_left_idx in idx..WAITLIST_LEN {
                                                ing[final_idx].inserters[move_left_idx] = ing
                                                    [final_idx]
                                                    .inserters
                                                    .get_mut(move_left_idx + 1)
                                                    .map(|v| v.take())
                                                    .unwrap_or(None);
                                            }
                                            let () =
                                                self.inserter_waitlist_output_vec
                                                    .push(InternalInserterReinsertionInfo {
                                                    movetime: ins.movetime.into(),
                                                    item: item as u8,
                                                    max_hand: ins.max_hand.into(),
                                                    self_index: final_idx as u32,
                                                    rest: match ins.rest {
                                                        InserterWithBeltsEnum::StorageStorage {
                                                            self_is_source: _,
                                                            index,
                                                            other,
                                                        } => Rest::Storage { index, other },
                                                        InserterWithBeltsEnum::BeltStorage {
                                                            belt_id,
                                                            belt_pos,
                                                            self_is_source,
                                                        } => Rest::Belt {
                                                            belt_id,
                                                            belt_pos,
                                                            self_is_source,
                                                        },
                                                    },
                                                })
                                            else {
                                                panic!(
                                                    "Not enough space in inserter readdition vec. Capacity is {}.",
                                                    self.inserter_waitlist_output_vec.capacity()
                                                );
                                            };
                                            self.waitlists_ings_needed[item][final_idx] =
                                                ing[final_idx].inserters[0]
                                                    .as_ref()
                                                    .map(|ins| ins.current_hand)
                                                    .unwrap_or(ITEMCOUNTTYPE::MAX);
                                        } else {
                                            v.current_hand -= amount_taken_by_this_inserter;
                                        }
                                        *items_to_drain -= amount_taken_by_this_inserter;

                                        // TODO: Check if this is good or bad
                                        if *items_to_drain == 0 {
                                            break;
                                        }
                                    }
                                }
                            }

                            if *items_to_drain > 0 {
                                self.ings[item][final_idx] -= *items_to_drain;
                            }
                        }
                    }
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
        grid: PowerGridIdentifier,
        data_store: &DataStore<ItemIdxType, RecipeIdxType>,
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

            waitlists_ings: array::from_fn(|_| vec![].into_boxed_slice()),
            waitlists_ings_needed: array::from_fn(|_| vec![].into_boxed_slice()),
            waitlists_outputs: array::from_fn(|_| vec![].into_boxed_slice()),
            waitlists_outputs_needed: array::from_fn(|_| vec![].into_boxed_slice()),
            holes: vec![],
            positions: vec![].into_boxed_slice(),
            types: vec![].into_boxed_slice(),
            len: 0,

            inserter_waitlist_output_vec: vec![],

            self_fake_union_ing: {
                array::from_fn(|index| {
                    let item = data_store.recipe_item_index_to_item[recipe.into_usize()][index];

                    FakeUnionStorage::from_storage_with_statics_at_zero(
                        item,
                        Storage::Assembler {
                            grid,
                            recipe_idx_with_this_item: recipe.id,
                            index: 0,
                        },
                        data_store,
                    )
                    .unwrap()
                })
            },
            self_fake_union_out: {
                array::from_fn(|index| {
                    let item =
                        data_store.recipe_item_index_to_item[recipe.into_usize()][index + NUM_INGS];

                    FakeUnionStorage::from_storage_with_statics_at_zero(
                        item,
                        Storage::Assembler {
                            grid,
                            recipe_idx_with_this_item: recipe.id,
                            index: 0,
                        },
                        data_store,
                    )
                    .unwrap()
                })
            },

            #[cfg(feature = "assembler-craft-tracking")]
            number_of_crafts_finished: vec![],
        }
    }

    fn set_grid_id<ItemIdxType: IdxTrait>(
        &mut self,
        new_grid_id: PowerGridIdentifier,
        data_store: &DataStore<ItemIdxType, RecipeIdxType>,
    ) {
        self.self_fake_union_ing = {
            array::from_fn(|index| {
                let item = data_store.recipe_item_index_to_item[self.recipe.into_usize()][index];

                FakeUnionStorage::from_storage_with_statics_at_zero(
                    item,
                    Storage::Assembler {
                        grid: new_grid_id,
                        recipe_idx_with_this_item: self.recipe.id,
                        index: 0,
                    },
                    data_store,
                )
                .unwrap()
            })
        };
        self.self_fake_union_out = {
            array::from_fn(|index| {
                let item = data_store.recipe_item_index_to_item[self.recipe.into_usize()]
                    [index + NUM_INGS];

                FakeUnionStorage::from_storage_with_statics_at_zero(
                    item,
                    Storage::Assembler {
                        grid: new_grid_id,
                        recipe_idx_with_this_item: self.recipe.id,
                        index: 0,
                    },
                    data_store,
                )
                .unwrap()
            })
        };
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

    fn is_hole(&self, index: u32) -> bool {
        self.holes.contains(&(index as usize)) && (index as usize) < self.len
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
            assert!(self.base_power_consumption.get(index as usize).is_some());
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

            #[cfg(feature = "assembler-craft-tracking")]
            times_craft_finished: self.number_of_crafts_finished[index as usize],
        }
    }

    fn join<ItemIdxType: IdxTrait>(
        mut self,
        mut other: Self,
        other_grid_id: PowerGridIdentifier,
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

        assert_eq!(self.timers.len() % SIMDTYPE::LEN, 0);
        assert_eq!(other.timers.len() % SIMDTYPE::LEN, 0);

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
        self.holes.extend(
            other
                .holes
                .iter()
                .map(|&old_idx| old_idx + self.timers.len()),
        );
        self.holes.extend(other.len..(other.timers.len()));
        self.len = old_len_stored;

        let new_ings_max: [Box<[u8]>; NUM_INGS] = self
            .ings_max_insert
            .into_iter()
            .zip(other.ings_max_insert)
            .map(|(s, o)| {
                let mut s = s.into_vec();
                s.extend(o.into_vec().into_iter().enumerate().map(|(_, v)| v));
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
                s.extend(o.into_vec().into_iter().enumerate().map(|(_, v)| v));
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
                s.extend(o.into_vec().into_iter().enumerate().map(|(_, v)| v));
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
                .map(|(_, v)| v),
        );

        let mut new_prod_timers = self.prod_timers.into_vec();
        new_prod_timers.extend(
            other
                .prod_timers
                .into_vec()
                .into_iter()
                .enumerate()
                .map(|(_, v)| v),
        );

        let mut new_base_speed = self.base_speed.into_vec();
        new_base_speed.extend(
            other
                .base_speed
                .into_vec()
                .into_iter()
                .enumerate()
                .map(|(_, v)| v),
        );

        let mut new_speed = self.combined_speed_mod.into_vec();
        new_speed.extend(
            other
                .combined_speed_mod
                .into_vec()
                .into_iter()
                .enumerate()
                .map(|(_, v)| v),
        );

        let mut new_prod = self.bonus_productivity.into_vec();
        new_prod.extend(
            other
                .bonus_productivity
                .into_vec()
                .into_iter()
                .enumerate()
                .map(|(_, v)| v),
        );

        let mut new_base_power_consumption = self.base_power_consumption.into_vec();
        new_base_power_consumption.extend(
            other
                .base_power_consumption
                .into_vec()
                .into_iter()
                .enumerate()
                .map(|(_, v)| v),
        );

        let mut new_power_consumption_modifier = self.power_consumption_modifier.into_vec();
        new_power_consumption_modifier.extend(
            other
                .power_consumption_modifier
                .into_vec()
                .into_iter()
                .enumerate()
                .map(|(_, v)| v),
        );

        let mut new_raw_speed_mod = self.raw_speed_mod.into_vec();
        new_raw_speed_mod.extend(
            other
                .raw_speed_mod
                .into_vec()
                .into_iter()
                .enumerate()
                .map(|(_, v)| v),
        );

        let mut new_raw_bonus_productivity = self.raw_bonus_productivity.into_vec();
        new_raw_bonus_productivity.extend(
            other
                .raw_bonus_productivity
                .into_vec()
                .into_iter()
                .enumerate()
                .map(|(_, v)| v),
        );

        let mut new_raw_power_consumption_modifier = self.raw_power_consumption_modifier.into_vec();
        new_raw_power_consumption_modifier.extend(
            other
                .raw_power_consumption_modifier
                .into_vec()
                .into_iter()
                .enumerate()
                .map(|(_, v)| v),
        );

        let mut new_positions = self.positions.into_vec();
        new_positions.extend(other.positions.iter().copied().enumerate().map(|(_, v)| v));

        let mut new_types = self.types.into_vec();
        new_types.extend(other.types.iter().copied().enumerate().map(|(_, v)| v));

        let mut new_waitlists_ings = self.waitlists_ings.map(|v| v.into_vec());
        for (new, other) in new_waitlists_ings.iter_mut().zip(other.waitlists_ings) {
            new.extend(other);
        }

        let mut new_waitlists_ings_needed = self.waitlists_ings_needed.map(|v| v.into_vec());
        for (new, other) in new_waitlists_ings_needed
            .iter_mut()
            .zip(other.waitlists_ings_needed)
        {
            new.extend(other);
        }

        let mut new_waitlists_outputs = self.waitlists_outputs.map(|v| v.into_vec());
        for (new, other) in new_waitlists_outputs
            .iter_mut()
            .zip(other.waitlists_outputs)
        {
            new.extend(other);
        }

        let mut new_waitlists_outputs_needed = self
            .waitlists_outputs_needed
            .map(|v: Box<[u8]>| v.into_vec());
        for (new, other) in new_waitlists_outputs_needed
            .iter_mut()
            .zip(other.waitlists_outputs_needed)
        {
            new.extend(other);
        }

        let updates = IntoIterator::into_iter(other.positions)
            .take(other.len)
            .zip(other.types)
            .enumerate()
            .take(other.len)
            .enumerate()
            .filter(move |(_offs, (i, _))| !other.holes.contains(i))
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
                    old_grid: other_grid_id,
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

        assert_eq!(new_timers.len() % SIMDTYPE::LEN, 0);

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

            waitlists_ings: new_waitlists_ings.map(|v| v.into_boxed_slice()),
            waitlists_ings_needed: new_waitlists_ings_needed.map(|v| v.into_boxed_slice()),
            waitlists_outputs: new_waitlists_outputs.map(|v| v.into_boxed_slice()),
            waitlists_outputs_needed: new_waitlists_outputs_needed.map(|v| v.into_boxed_slice()),
            positions: new_positions.into_boxed_slice(),
            types: new_types.into_boxed_slice(),

            inserter_waitlist_output_vec: if self.inserter_waitlist_output_vec.capacity()
                > other.inserter_waitlist_output_vec.capacity()
            {
                self.inserter_waitlist_output_vec
            } else {
                other.inserter_waitlist_output_vec
            },

            self_fake_union_ing: {
                array::from_fn(|index| {
                    let item =
                        data_store.recipe_item_index_to_item[self.recipe.into_usize()][index];

                    FakeUnionStorage::from_storage_with_statics_at_zero(
                        item,
                        Storage::Assembler {
                            grid: new_grid_id,
                            recipe_idx_with_this_item: self.recipe.id,
                            index: 0,
                        },
                        data_store,
                    )
                    .unwrap()
                })
            },
            self_fake_union_out: {
                array::from_fn(|index| {
                    let item = data_store.recipe_item_index_to_item[self.recipe.into_usize()]
                        [index + NUM_INGS];

                    FakeUnionStorage::from_storage_with_statics_at_zero(
                        item,
                        Storage::Assembler {
                            grid: new_grid_id,
                            recipe_idx_with_this_item: self.recipe.id,
                            index: 0,
                        },
                        data_store,
                    )
                    .unwrap()
                })
            },

            #[cfg(feature = "assembler-craft-tracking")]
            number_of_crafts_finished: {
                self.number_of_crafts_finished
                    .extend(other.number_of_crafts_finished);
                self.number_of_crafts_finished
            },
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
    ) -> (
        PowerUsageInfo,
        u32,
        u32,
        impl Iterator<Item = InserterReinsertionInfo<ItemIdxType>>,
    )
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

        (
            PowerUsageInfo::Combined(power),
            ings_used,
            produced,
            self.inserter_waitlist_output_vec
                .drain(..)
                .map(|internal| InserterReinsertionInfo {
                    movetime: internal.movetime,
                    item: data_store.recipe_item_index_to_item[self.recipe.into_usize()]
                        [internal.item as usize],
                    current_hand: if (internal.item as usize) < NUM_INGS {
                        // This is an ingredient inserter
                        // So it is now empty
                        0
                    } else {
                        internal.max_hand
                    },
                    max_hand: internal.max_hand,

                    conn: match internal.rest {
                        Rest::Storage { index, other } => Conn::Storage {
                            index,
                            storage_id_in: if (internal.item as usize) < NUM_INGS {
                                // This is an ingredient inserter
                                other
                            } else {
                                FakeUnionStorage {
                                    index: internal.self_index,
                                    grid_or_static_flag: self.self_fake_union_out
                                        [internal.item as usize - NUM_INGS]
                                        .grid_or_static_flag,
                                    recipe_idx_with_this_item: self.self_fake_union_out
                                        [internal.item as usize - NUM_INGS]
                                        .recipe_idx_with_this_item,
                                }
                            },
                            storage_id_out: if (internal.item as usize) < NUM_INGS {
                                // This is an ingredient inserter
                                FakeUnionStorage {
                                    index: internal.self_index,
                                    grid_or_static_flag: self.self_fake_union_ing
                                        [internal.item as usize]
                                        .grid_or_static_flag,
                                    recipe_idx_with_this_item: self.self_fake_union_ing
                                        [internal.item as usize]
                                        .recipe_idx_with_this_item,
                                }
                            } else {
                                other
                            },
                        },
                        Rest::Belt {
                            belt_id,
                            belt_pos,
                            self_is_source,
                        } => Conn::Belt {
                            belt_id,
                            belt_pos,
                            self_is_source,
                            self_storage: if (internal.item as usize) < NUM_INGS {
                                FakeUnionStorage {
                                    index: internal.self_index,
                                    grid_or_static_flag: self.self_fake_union_ing
                                        [internal.item as usize]
                                        .grid_or_static_flag,
                                    recipe_idx_with_this_item: self.self_fake_union_ing
                                        [internal.item as usize]
                                        .recipe_idx_with_this_item,
                                }
                            } else {
                                FakeUnionStorage {
                                    index: internal.self_index,
                                    grid_or_static_flag: self.self_fake_union_out
                                        [internal.item as usize - NUM_INGS]
                                        .grid_or_static_flag,
                                    recipe_idx_with_this_item: self.self_fake_union_out
                                        [internal.item as usize - NUM_INGS]
                                        .recipe_idx_with_this_item,
                                }
                            },
                        },
                    },
                }),
        )
    }

    fn get_all_mut(
        &mut self,
    ) -> (
        (
            [MaxInsertionLimit<'_>; NUM_INGS],
            [&mut [ITEMCOUNTTYPE]; NUM_INGS],
            [(&mut [InserterWaitList], &mut [ITEMCOUNTTYPE]); NUM_INGS],
        ),
        (
            [&mut [ITEMCOUNTTYPE]; NUM_OUTPUTS],
            [(&mut [InserterWaitList], &mut [ITEMCOUNTTYPE]); NUM_OUTPUTS],
        ),
    ) {
        (
            (
                self.ings_max_insert.each_mut().map(|b| match b.get(0) {
                    Some(v) => MaxInsertionLimit::Global(*v),
                    None => PANIC_ON_INSERT,
                }),
                // self.ings_max_insert.each_mut().map(|b| &**b),
                self.ings.each_mut().map(|b| &mut **b),
                self.waitlists_ings
                    .each_mut()
                    .into_iter()
                    .zip(self.waitlists_ings_needed.each_mut())
                    .map(|(wait, needed)| (&mut **wait, &mut **needed))
                    .collect_array()
                    .unwrap(),
            ),
            (
                self.outputs.each_mut().map(|b| &mut **b),
                self.waitlists_outputs
                    .each_mut()
                    .into_iter()
                    .zip(self.waitlists_outputs_needed.each_mut())
                    .map(|(wait, needed)| (&mut **wait, &mut **needed))
                    .collect_array()
                    .unwrap(),
            ),
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

            for ings in &mut self.waitlists_ings {
                ings[hole_index] = InserterWaitList::default();
            }
            for out in &mut self.waitlists_outputs {
                out[hole_index] = InserterWaitList::default();
            }
            for ings in &mut self.waitlists_ings_needed {
                ings[hole_index] = ITEMCOUNTTYPE::MAX;
            }
            for out in &mut self.waitlists_outputs_needed {
                out[hole_index] = ITEMCOUNTTYPE::MAX;
            }

            self.types[hole_index] = ty;
            self.positions[hole_index] = position;

            // TODO: Actually pass this
            #[cfg(feature = "assembler-craft-tracking")]
            {
                self.number_of_crafts_finished[hole_index] = 0;
            }
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
                new_len =
                    Some((output.capacity() - (Simdtype::LEN - 1)).next_multiple_of(Simdtype::LEN));
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
            for ing in &mut self.waitlists_ings {
                take_mut::take(ing, |list| {
                    let mut list = list.into_vec();
                    list.resize(new_len, InserterWaitList::default());
                    list.into_boxed_slice()
                });
            }
            for out in &mut self.waitlists_outputs {
                take_mut::take(out, |list| {
                    let mut list = list.into_vec();
                    list.resize(new_len, InserterWaitList::default());
                    list.into_boxed_slice()
                });
            }
            for ing in &mut self.waitlists_ings_needed {
                take_mut::take(ing, |list| {
                    let mut list = list.into_vec();
                    list.resize(new_len, ITEMCOUNTTYPE::MAX);
                    list.into_boxed_slice()
                });
            }
            for out in &mut self.waitlists_outputs_needed {
                take_mut::take(out, |list| {
                    let mut list = list.into_vec();
                    list.resize(new_len, ITEMCOUNTTYPE::MAX);
                    list.into_boxed_slice()
                });
            }

            take_mut::take(&mut self.types, |ty| {
                let mut ty = ty.into_vec();
                ty.resize(new_len, u8::MAX);
                ty.into_boxed_slice()
            });

            #[cfg(feature = "assembler-craft-tracking")]
            {
                self.number_of_crafts_finished.resize(new_len, 0);
            }
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
        for ing in &mut self.waitlists_ings {
            ing[self.len] = InserterWaitList::default();
        }
        for out in &mut self.waitlists_outputs {
            out[self.len] = InserterWaitList::default();
        }
        for ing in &mut self.waitlists_ings_needed {
            ing[self.len] = ITEMCOUNTTYPE::MAX;
        }
        for out in &mut self.waitlists_outputs_needed {
            out[self.len] = ITEMCOUNTTYPE::MAX;
        }

        self.positions[self.len] = position;
        self.types[self.len] = ty;

        #[cfg(feature = "assembler-craft-tracking")]
        {
            self.number_of_crafts_finished[self.len] = 0;
        }

        self.len += 1;
        assert_eq!(self.timers.len() % SIMDTYPE::LEN, 0);
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
        for ing in &mut self.waitlists_ings {
            ing[index] = InserterWaitList::default();
        }
        for out in &mut self.waitlists_outputs {
            out[index] = InserterWaitList::default();
        }
        for ing in &mut self.waitlists_ings_needed {
            ing[index] = ITEMCOUNTTYPE::MAX;
        }
        for out in &mut self.waitlists_outputs_needed {
            out[index] = ITEMCOUNTTYPE::MAX;
        }

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

    fn num_assemblers(&self) -> usize {
        self.len - self.holes.len()
    }

    fn remove_wait_list_inserter<ItemIdxType: IdxTrait>(
        &mut self,
        self_index: u32,
        item: Item<ItemIdxType>,
        id: InserterId,
        data_store: &DataStore<ItemIdxType, RecipeIdxType>,
    ) -> self::InserterReinsertionInfo<ItemIdxType> {
        let item_index = data_store.recipe_item_index_to_item[self.recipe.into_usize()]
            .iter()
            .position(|recipe_item| recipe_item == &item)
            .unwrap();

        if item_index < NUM_INGS {
            let v = self.waitlists_ings[item_index][self_index as usize]
                .inserters
                .iter_mut()
                .filter(|v| v.is_some())
                .find(|ins| match ins.as_ref().unwrap().rest {
                    InserterWithBeltsEnum::StorageStorage { index, .. } => index == id,
                    InserterWithBeltsEnum::BeltStorage { .. } => false,
                })
                .unwrap();

            let ins = v.take().unwrap();

            InserterReinsertionInfo {
                movetime: ins.movetime.into(),
                item: item,
                current_hand: ins.current_hand,
                max_hand: ins.max_hand.into(),

                conn: match ins.rest {
                    InserterWithBeltsEnum::StorageStorage { index, other, .. } => Conn::Storage {
                        index,
                        // This is an ingredient inserter
                        storage_id_in: other,
                        // This is an ingredient inserter
                        storage_id_out: FakeUnionStorage {
                            index: self_index,
                            grid_or_static_flag: self.self_fake_union_ing[item_index]
                                .grid_or_static_flag,
                            recipe_idx_with_this_item: self.self_fake_union_ing[item_index]
                                .recipe_idx_with_this_item,
                        },
                    },
                    InserterWithBeltsEnum::BeltStorage {
                        self_is_source,
                        belt_id,
                        belt_pos,
                    } => Conn::Belt {
                        self_is_source,
                        belt_id,
                        belt_pos,
                        self_storage: FakeUnionStorage {
                            index: self_index,
                            grid_or_static_flag: self.self_fake_union_ing[item_index]
                                .grid_or_static_flag,
                            recipe_idx_with_this_item: self.self_fake_union_ing[item_index]
                                .recipe_idx_with_this_item,
                        },
                    },
                },
            }
        } else {
            let item_index = item_index - NUM_INGS;
            let v = self.waitlists_outputs[item_index][self_index as usize]
                .inserters
                .iter_mut()
                .filter(|v| v.is_some())
                .find(|ins| match ins.as_ref().unwrap().rest {
                    InserterWithBeltsEnum::StorageStorage { index, .. } => index == id,
                    InserterWithBeltsEnum::BeltStorage { .. } => false,
                })
                .unwrap();

            let ins = v.take().unwrap();

            InserterReinsertionInfo {
                movetime: ins.movetime.into(),
                item: item,
                current_hand: ins.current_hand,
                max_hand: ins.max_hand.into(),

                conn: match ins.rest {
                    InserterWithBeltsEnum::StorageStorage { index, other, .. } => Conn::Storage {
                        index,
                        // This is an output inserter
                        storage_id_in: FakeUnionStorage {
                            index: self_index,
                            grid_or_static_flag: self.self_fake_union_out[item_index]
                                .grid_or_static_flag,
                            recipe_idx_with_this_item: self.self_fake_union_out[item_index]
                                .recipe_idx_with_this_item,
                        },
                        // This is an output inserter
                        storage_id_out: other,
                    },
                    InserterWithBeltsEnum::BeltStorage {
                        self_is_source,
                        belt_id,
                        belt_pos,
                    } => Conn::Belt {
                        self_is_source,
                        belt_id,
                        belt_pos,
                        self_storage: FakeUnionStorage {
                            index: self_index,
                            grid_or_static_flag: self.self_fake_union_ing[item_index]
                                .grid_or_static_flag,
                            recipe_idx_with_this_item: self.self_fake_union_ing[item_index]
                                .recipe_idx_with_this_item,
                        },
                    },
                },
            }
        }
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
            .map(|_| MultiAssemblerStore::new(Recipe { id: 11 }, 0, &DATA_STORE))
            .collect_vec();

        let items: Vec<Vec<_>> = vec![
            rayon::iter::repeat(rand::rng().random_range(250..=255))
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
