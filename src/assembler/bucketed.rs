use std::cmp::{max, min};
use std::{array, iter, u8};

use itertools::Itertools;
use log::warn;

use crate::assembler::{PowerUsageInfo, TIMERTYPE};
use crate::data::{DataStore, ItemRecipeDir};
use crate::frontend::world::Position;
use crate::item::{ITEMCOUNTTYPE, IdxTrait, Indexable, Recipe};
use crate::power::Watt;
use crate::power::power_grid::{IndexUpdateInfo, MAX_POWER_MULT};

use crate::assembler::arrays;

use crate::WeakIdxTrait;

#[cfg(feature = "client")]
use egui_show_info_derive::ShowInfo;
#[cfg(feature = "client")]
use get_size::GetSize;

// TODO: This does not work correctly yet
// Also I do not update the waitlist if modifiers change
#[cfg_attr(feature = "client", derive(ShowInfo), derive(GetSize))]
#[derive(Debug, Clone, serde::Serialize, serde::Deserialize)]
pub struct MultiAssemblerStore<
    RecipeIdxType: WeakIdxTrait,
    const NUM_INGS: usize,
    const NUM_OUTPUTS: usize,
> {
    recipe: Recipe<RecipeIdxType>,

    num_by_types: Vec<u32>,

    buckets: BucketStore,

    hot_data: Vec<AssemblerDataStruct>,

    #[serde(with = "arrays")]
    ings: [Vec<ITEMCOUNTTYPE>; NUM_INGS],
    #[serde(with = "arrays")]
    ings_max_insert: [Vec<ITEMCOUNTTYPE>; NUM_INGS],
    #[serde(with = "arrays")]
    outputs: [Vec<ITEMCOUNTTYPE>; NUM_OUTPUTS],

    cold_data: Vec<ColdAssemblerData>,

    holes: Vec<usize>,
}

#[cfg_attr(feature = "client", derive(ShowInfo), derive(GetSize))]
#[derive(Debug, Clone, serde::Serialize, serde::Deserialize)]
struct BucketStore {
    // outer len is
    // max(min(ticks_per_main_prod, ???), ...);
    // so ticks_per_main_prod
    waiting_for_update: Box<[Vec<AssemblerUpdateInfo>]>,
}

#[cfg_attr(feature = "client", derive(ShowInfo), derive(GetSize))]
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, serde::Serialize, serde::Deserialize)]
struct AssemblerUpdateInfo {
    assembler: u32,
    ticks_wanted: u16,
}

struct NextUpdateInfo {
    when: u16,
}

impl<RecipeIdxType: IdxTrait, const NUM_INGS: usize, const NUM_OUTPUTS: usize>
    MultiAssemblerStore<RecipeIdxType, NUM_INGS, NUM_OUTPUTS>
{
    fn do_single_assembler_update(
        mut ings: [&mut ITEMCOUNTTYPE; NUM_INGS],
        mut outs: [&mut ITEMCOUNTTYPE; NUM_OUTPUTS],
        data: &mut AssemblerDataStruct,

        recipe_ings: &[ITEMCOUNTTYPE; NUM_INGS],
        recipe_outs: &[ITEMCOUNTTYPE; NUM_OUTPUTS],
        power_subticks_per_craft: TIMERTYPE,

        power_subtick_overshoot: u8,
    ) -> (NextUpdateInfo, bool, bool, bool) {
        let ticks_per_main: u16 =
            power_subticks_per_craft * 20 / u16::from(data.combined_speed_mod);

        let tick_till_main_done: u16 = ticks_per_main - data.timer;

        let power_subticks_passed = if data.bonus_productivity > 0 {
            let ticks_per_prod: u16 = ticks_per_main * 100 / u16::from(data.bonus_productivity);
            let tick_till_prod_done: u16 = ticks_per_prod - data.prod_timer;

            min(tick_till_main_done, tick_till_prod_done) + u16::from(power_subtick_overshoot)
        } else {
            tick_till_main_done + u16::from(power_subtick_overshoot)
        };

        let (is_idle, main_produced, prod_produced) = Self::apply_subticks(
            &mut ings,
            &mut outs,
            data,
            recipe_ings,
            recipe_outs,
            power_subticks_per_craft,
            power_subticks_passed,
        );

        let next_update = if !is_idle {
            Self::get_next_update_info_running(power_subticks_per_craft, data)
        } else {
            // We are waiting for ingredients or space
            // Check again next tick
            NextUpdateInfo { when: 1 }
        };

        (next_update, is_idle, main_produced, prod_produced)
    }

    /// Only works correctly if the num_subticks will only cause a single craft/prod
    fn apply_subticks(
        ings: &mut [&mut ITEMCOUNTTYPE; NUM_INGS],
        outs: &mut [&mut ITEMCOUNTTYPE; NUM_OUTPUTS],
        data: &mut AssemblerDataStruct,

        recipe_ings: &[ITEMCOUNTTYPE; NUM_INGS],
        recipe_outs: &[ITEMCOUNTTYPE; NUM_OUTPUTS],
        power_subticks_per_craft: TIMERTYPE,
        num_subticks: TIMERTYPE,
    ) -> (bool, bool, bool) {
        let ticks_per_main: u16 =
            power_subticks_per_craft * 20 / u16::from(data.combined_speed_mod);

        if ings
            .into_iter()
            .zip(recipe_ings.iter())
            .any(|(ing, recipe)| **ing < *recipe)
        {
            return (true, false, false);
        }

        // TODO: Be careful of overflow
        data.timer += num_subticks;
        data.prod_timer += num_subticks;

        let (main_done, prod_done) = (
            (data.timer >= ticks_per_main).then_some(data.timer - ticks_per_main),
            if data.bonus_productivity > 0 {
                let ticks_per_prod: u16 = ticks_per_main * 100 / u16::from(data.bonus_productivity);
                (data.prod_timer >= ticks_per_prod).then_some(data.prod_timer - ticks_per_prod)
            } else {
                None
            },
        );

        // TODO: Maximum hardcoded
        let check_space =
            |outs: &mut [&mut ITEMCOUNTTYPE; NUM_OUTPUTS]| outs.iter().all(|output| **output < 100);

        let (running, main_done, prod_done) = match (main_done, prod_done) {
            (None, None) => {
                for (ing, recipe) in ings.into_iter().zip(recipe_ings.iter()) {
                    debug_assert!(**ing >= *recipe);
                }
                data.timer += num_subticks;
                data.prod_timer += num_subticks;
                (true, false, false)
            },
            (None, Some(prod_overshoot)) => {
                if check_space(outs) {
                    // There is space
                    for (out, recipe) in outs.into_iter().zip(recipe_outs) {
                        **out += *recipe;
                    }

                    data.prod_timer = prod_overshoot;
                    data.timer += num_subticks;

                    (true, false, true)
                } else {
                    // No space (The last tick, the timer will not increase)
                    data.timer += num_subticks - prod_overshoot - 1;
                    data.prod_timer += num_subticks - prod_overshoot - 1;

                    // Since prod_timer is right below ticks_per_prod we will check again next tick by get_next_update_info_running

                    // We are not running
                    (false, false, false)
                }
            },
            (Some(main_overshoot), None) => {
                if check_space(outs) {
                    // There is space

                    for (ing, recipe) in ings.into_iter().zip(recipe_ings.iter()) {
                        debug_assert!(**ing >= *recipe);
                        **ing -= *recipe;
                    }
                    for (out, recipe) in outs.into_iter().zip(recipe_outs) {
                        **out += *recipe;
                    }

                    data.timer = main_overshoot;
                    data.prod_timer += num_subticks;

                    (true, true, false)
                } else {
                    // No space (The last tick, the timer will not increase)
                    data.timer += num_subticks - main_overshoot - 1;
                    data.prod_timer = num_subticks - main_overshoot - 1;

                    // Since prod_timer is right below ticks_per_prod we will check again next tick by get_next_update_info_running

                    // We are not running
                    (false, false, false)
                }
            },
            (Some(main_overshoot), Some(prod_overshoot)) => {
                let first_overshoot = max(main_overshoot, prod_overshoot);

                let (idle, main_done, prod_done) = Self::apply_subticks(
                    ings,
                    outs,
                    data,
                    recipe_ings,
                    recipe_outs,
                    power_subticks_per_craft,
                    num_subticks - first_overshoot,
                );

                let rest = min(main_overshoot, prod_overshoot);

                let (second_idle, second_main_done, second_prod_done) = Self::apply_subticks(
                    ings,
                    outs,
                    data,
                    recipe_ings,
                    recipe_outs,
                    power_subticks_per_craft,
                    rest,
                );

                debug_assert!(!(main_done && second_main_done));
                debug_assert!(!(prod_done && second_prod_done));

                (
                    !idle || !second_idle,
                    main_done && second_main_done,
                    prod_done && second_prod_done,
                )
            },
        };

        (!running, main_done, prod_done)
    }

    fn get_next_update_info_running(
        power_subticks_per_main: u16,
        data: &AssemblerDataStruct,
    ) -> NextUpdateInfo {
        let tick_till_main_done: u16 = power_subticks_per_main - data.timer;

        let when = if data.bonus_productivity > 0 {
            let subticks_per_prod: u16 =
                power_subticks_per_main * 100 / u16::from(data.bonus_productivity);
            let tick_till_prod_done: u16 = subticks_per_prod - data.prod_timer;

            min(tick_till_main_done, tick_till_prod_done)
        } else {
            tick_till_main_done
        };

        NextUpdateInfo { when }
    }
}

#[cfg_attr(feature = "client", derive(ShowInfo), derive(GetSize))]
#[derive(Debug, Clone, serde::Serialize, serde::Deserialize)]
struct AssemblerDataStruct {
    // In 64th of a tick
    timer: TIMERTYPE,
    prod_timer: TIMERTYPE,

    power_consumption_modifier: u8,
    bonus_productivity: u8,
    combined_speed_mod: u8,
    ty: u8,
}

#[cfg_attr(feature = "client", derive(ShowInfo), derive(GetSize))]
#[derive(Debug, Clone, serde::Serialize, serde::Deserialize)]
struct ColdAssemblerData {
    raw_speed_mod: i16,
    raw_bonus_productivity: i16,
    raw_power_consumption_modifier: i16,
    position: Position,
}

impl<RecipeIdxType: WeakIdxTrait, const NUM_INGS: usize, const NUM_OUTPUTS: usize>
    super::MultiAssemblerStore<RecipeIdxType, NUM_INGS, NUM_OUTPUTS>
    for MultiAssemblerStore<RecipeIdxType, NUM_INGS, NUM_OUTPUTS>
{
    fn new<ItemIdxType: IdxTrait>(
        recipe: Recipe<RecipeIdxType>,
        data_store: &DataStore<ItemIdxType, RecipeIdxType>,
    ) -> Self {
        let ticks_per_craft = data_store.recipe_timers[recipe.into_usize()];
        let power_subticks_per_craft =
            if let Some(v) = ticks_per_craft.checked_mul(TIMERTYPE::from(MAX_POWER_MULT)) {
                v
            } else {
                warn!(
                    "Time (in power subticks) for recipe {} exceeds TIMERTYPE::MAX",
                    data_store.recipe_names[recipe.into_usize()]
                );
                TIMERTYPE::MAX
            };

        Self {
            recipe,
            num_by_types: vec![0; data_store.assembler_info.len()],

            buckets: BucketStore {
                waiting_for_update: vec![
                    vec![];
                    usize::from(power_subticks_per_craft)
                        + usize::from(MAX_POWER_MULT)
                ]
                .into_boxed_slice(),
            },
            hot_data: vec![],
            ings: array::from_fn(|_| vec![]),
            ings_max_insert: array::from_fn(|_| vec![]),
            outputs: array::from_fn(|_| vec![]),
            cold_data: vec![],
            holes: vec![],
        }
    }

    fn get_recipe(&self) -> Recipe<RecipeIdxType> {
        self.recipe
    }

    /// This will not get the correct info (since it is only updated lazily)
    /// Instead use get_info_batched
    fn get_info<ItemIdxType: IdxTrait>(
        &self,
        index: u32,
        data_store: &DataStore<ItemIdxType, RecipeIdxType>,
    ) -> super::AssemblerOnclickInfo<ItemIdxType> {
        let items = data_store.recipe_to_items.get(&self.recipe).unwrap();

        super::AssemblerOnclickInfo {
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
            timer_percentage: f32::from(self.hot_data[index as usize].timer)
                / f32::from(TIMERTYPE::MAX),
            prod_timer_percentage: f32::from(self.hot_data[index as usize].prod_timer)
                / f32::from(TIMERTYPE::MAX),
            base_speed: f32::from(
                data_store.assembler_info[self.hot_data[index as usize].ty as usize].base_speed,
            ) * 0.05,
            speed_mod: f32::from(self.cold_data[index as usize].raw_speed_mod) * 0.05,
            prod_mod: f32::from(self.hot_data[index as usize].bonus_productivity) * 0.01,
            power_consumption_mod: f32::from(
                self.hot_data[index as usize].power_consumption_modifier,
            ) * 0.05
                - 1.0,
            base_power_consumption: data_store.assembler_info
                [self.hot_data[index as usize].ty as usize]
                .base_power_consumption,
        }
    }

    fn join<ItemIdxType: IdxTrait>(
        self,
        other: Self,
        new_grid_id: crate::power::power_grid::PowerGridIdentifier,
        _data_store: &DataStore<ItemIdxType, RecipeIdxType>,
    ) -> (
        Self,
        impl IntoIterator<Item = crate::power::power_grid::IndexUpdateInfo<ItemIdxType, RecipeIdxType>>,
    ) {
        let Self {
            recipe,
            mut num_by_types,
            mut buckets,
            mut hot_data,
            mut ings,
            mut ings_max_insert,
            mut outputs,
            mut cold_data,
            mut holes,
        } = self;

        let Self {
            recipe: other_recipe,
            num_by_types: other_num_by_types,
            buckets: other_buckets,
            hot_data: other_hot_data,
            ings: other_ings,
            ings_max_insert: other_ings_max_insert,
            outputs: other_outputs,
            cold_data: other_cold_data,
            holes: other_holes,
        } = other;

        // assert_eq!(recipe, other_recipe);

        for (bucket, other_bucket) in buckets
            .waiting_for_update
            .iter_mut()
            .zip(other_buckets.waiting_for_update)
        {
            bucket.extend(other_bucket);
        }

        for (sel, other) in num_by_types.iter_mut().zip(other_num_by_types) {
            *sel += other;
        }

        let self_len: u32 = hot_data.len().try_into().unwrap();

        hot_data.extend_from_slice(&other_hot_data);
        cold_data.extend_from_slice(&other_cold_data);
        holes.extend(other_holes.into_iter().map(|hole| hole + self_len as usize));

        for ing in 0..NUM_INGS {
            ings[ing].extend_from_slice(&other_ings[ing]);
            ings_max_insert[ing].extend_from_slice(&other_ings_max_insert[ing]);
        }
        for out in 0..NUM_OUTPUTS {
            outputs[out].extend_from_slice(&other_outputs[out]);
        }

        let new = Self {
            recipe,
            num_by_types,
            buckets,
            hot_data,
            ings,
            ings_max_insert,
            outputs,
            cold_data,
            holes,
        };

        let updates = other_cold_data
            .into_iter()
            .zip(other_hot_data)
            .enumerate()
            .map(|(i, (cold, hot))| (i, (cold.position, hot.ty)))
            .map(move |(i, (position, ty))| IndexUpdateInfo {
                position,
                old_pg_entity: crate::power::power_grid::PowerGridEntity::Assembler {
                    ty,
                    recipe,
                    index: i.try_into().unwrap(),
                },
                new_pg_entity: crate::power::power_grid::PowerGridEntity::Assembler {
                    ty,
                    recipe,
                    index: (self_len + i as u32),
                },
                new_grid: new_grid_id,
            });

        (new, updates)
    }

    fn do_single_tick_update<ItemIdxType: IdxTrait>(
        &mut self,
        power_mult: u8,
        recipe_lookup: &[(usize, usize)],
        recipe_ings: &[[ITEMCOUNTTYPE; NUM_INGS]],
        recipe_outputs: &[[ITEMCOUNTTYPE; NUM_OUTPUTS]],
        times: &[TIMERTYPE],
        data_store: &DataStore<ItemIdxType, RecipeIdxType>,
    ) -> (PowerUsageInfo, u32, u32)
    where
        RecipeIdxType: IdxTrait,
    {
        let recipe = self.get_recipe();
        assert!(power_mult <= MAX_POWER_MULT);

        #[cfg(debug_assertions)]
        {
            assert!(
                self.buckets
                    .waiting_for_update
                    .iter()
                    .flat_map(|v| v.iter())
                    .all_unique(),
                "Some Assembler in multiple update buckets"
            );
            assert_eq!(
                self.hot_data.len(),
                self.buckets
                    .waiting_for_update
                    .iter()
                    .map(|v| v.len())
                    .sum::<usize>(),
                "Some Assembler are not being updated!"
            );
        }

        let (ing_idx, out_idx) = recipe_lookup[recipe.into_usize()];

        let our_ings: &[ITEMCOUNTTYPE; NUM_INGS] = &recipe_ings[ing_idx];
        let our_outputs: &[ITEMCOUNTTYPE; NUM_OUTPUTS] = &recipe_outputs[out_idx];

        let ticks_per_craft = times[recipe.into_usize()];
        let power_subticks_per_craft = ticks_per_craft * TIMERTYPE::from(MAX_POWER_MULT);

        let (now_empty_buckets, all_other_buckets) = self
            .buckets
            .waiting_for_update
            .split_at_mut(power_mult.into());

        // Instead of accumulating the power used while updating all assembling machines,
        // We start off by assuming all assemblers are running
        // Since we update all NON-running assemblers every tick, we will encounter them during this frames update
        // Whenever we find an assembler, which is not running we subtract its power consumption from the total
        let mut power_used = self.num_by_types.clone();
        let mut times_ing_used = 0;
        let mut times_main_timer_done = 0;
        let mut times_prod_timer_done = 0;

        let new_updates = now_empty_buckets
            .iter_mut()
            .enumerate()
            .flat_map(|(i, v)| {
                let overshoot = u8::try_from(i).unwrap();

                v.drain(..).zip(iter::repeat(overshoot))
            })
            .map(|(update, overshoot)| {
                let (update_info, is_idle, produced_main, produced_prod) =
                    Self::do_single_assembler_update(
                        self.ings
                            .each_mut()
                            .map(|ing| &mut ing[update.assembler as usize]),
                        self.outputs
                            .each_mut()
                            .map(|out| &mut out[update.assembler as usize]),
                        &mut self.hot_data[update.assembler as usize],
                        our_ings,
                        our_outputs,
                        power_subticks_per_craft,
                        overshoot,
                    );

                if is_idle {
                    debug_assert!(!produced_main);
                    debug_assert!(!produced_main);
                    debug_assert!(!produced_prod);
                    power_used[usize::from(self.hot_data[update.assembler as usize].ty)] -= 1;
                }
                times_ing_used += u32::from(produced_main);
                times_main_timer_done += u32::from(produced_main);
                times_prod_timer_done += u32::from(produced_prod);

                (update_info, update)
            });

        for (time, update) in new_updates {
            all_other_buckets[usize::from(time.when) - 1].push(update);
        }

        // TODO: Maybe use ringbuffer
        self.buckets.waiting_for_update.rotate_left(1);

        (
            PowerUsageInfo::ByType(
                power_used
                    .into_iter()
                    .enumerate()
                    .map(|(ty, v)| {
                        data_store.assembler_info[ty].base_power_consumption * v.into() / 20
                    })
                    .collect(),
            ),
            times_ing_used,
            times_main_timer_done + times_prod_timer_done,
        )
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

    fn modify_modifiers<ItemIdxType: IdxTrait>(
        &mut self,
        index: u32,
        speed: i16,
        prod: i16,
        power: i16,
        data_store: &DataStore<ItemIdxType, RecipeIdxType>,
    ) {
        let index = index as usize;
        self.cold_data[usize::from(index)].raw_speed_mod = self.cold_data[usize::from(index)]
            .raw_speed_mod
            .checked_add(speed)
            .expect("Over/Underflowed");
        self.cold_data[usize::from(index)].raw_bonus_productivity = self.cold_data
            [usize::from(index)]
        .raw_bonus_productivity
        .checked_add(prod)
        .expect("Over/Underflowed");
        self.cold_data[usize::from(index)].raw_power_consumption_modifier = self.cold_data
            [usize::from(index)]
        .raw_power_consumption_modifier
        .checked_add(power)
        .expect("Over/Underflowed");

        self.hot_data[usize::from(index)].power_consumption_modifier =
            (self.cold_data[usize::from(index)].raw_power_consumption_modifier + 20)
                .clamp(data_store.min_power_mod.into(), u8::MAX.into())
                .try_into()
                .expect("Values already clamped");
        self.hot_data[usize::from(index)].bonus_productivity = self.cold_data[usize::from(index)]
            .raw_bonus_productivity
            .clamp(0, u8::MAX.into())
            .try_into()
            .expect("Values already clamped");
        self.hot_data[usize::from(index)].combined_speed_mod =
            ((self.cold_data[usize::from(index)].raw_speed_mod + 20)
                // * i16::from(self.cold_data[usize::from(index)].base_speed)
                * i16::from(data_store.assembler_info[self.hot_data[usize::from(index)].ty as usize].base_speed)
                / 20)
                .clamp(0, u8::MAX.into())
                .try_into()
                .expect("Values already clamped");
    }

    fn add_assembler_with_data<ItemIdxType: IdxTrait>(
        &mut self,
        ings_max_insert: [ITEMCOUNTTYPE; NUM_INGS],
        ings: [ITEMCOUNTTYPE; NUM_INGS],
        out: [ITEMCOUNTTYPE; NUM_OUTPUTS],
        new_timer: TIMERTYPE,
        new_prod_timer: TIMERTYPE,
        _new_power: Watt,
        new_power_consumption_modifier: i16,
        new_bonus_productiviy: i16,
        new_base_speed: u8,
        new_speed_mod: i16,
        new_ty: u8,
        new_position: Position,
        data_store: &DataStore<ItemIdxType, RecipeIdxType>,
    ) -> u32 {
        let index = if let Some(hole_idx) = self.holes.pop() {
            hole_idx
        } else {
            let old_len: usize = self.hot_data.len();
            for v in self.ings_max_insert.iter_mut() {
                v.push(0);
            }
            for v in self.ings.iter_mut() {
                v.push(0);
            }
            for v in self.outputs.iter_mut() {
                v.push(0);
            }
            self.hot_data.push(AssemblerDataStruct {
                timer: 0,
                prod_timer: 0,
                power_consumption_modifier: 0,
                bonus_productivity: 0,
                combined_speed_mod: 0,
                ty: u8::MAX,
            });
            self.cold_data.push(ColdAssemblerData {
                raw_speed_mod: 0,
                raw_bonus_productivity: 0,
                raw_power_consumption_modifier: 0,
                position: Position { x: 0, y: 0 },
            });

            old_len
        };

        for (new, hole) in ings_max_insert
            .into_iter()
            .zip(self.ings_max_insert.iter_mut().map(|v| &mut v[index]))
        {
            *hole = new;
        }
        for (new, hole) in ings
            .into_iter()
            .zip(self.ings.iter_mut().map(|v| &mut v[index]))
        {
            *hole = new;
        }
        for (new, hole) in out
            .into_iter()
            .zip(self.outputs.iter_mut().map(|v| &mut v[index]))
        {
            *hole = new;
        }
        let AssemblerDataStruct {
            timer,
            prod_timer,
            power_consumption_modifier,
            bonus_productivity,
            combined_speed_mod,
            ty,
        } = &mut self.hot_data[index];
        let ColdAssemblerData {
            raw_speed_mod,
            raw_bonus_productivity,
            raw_power_consumption_modifier,
            position,
        } = &mut self.cold_data[index];

        *timer = new_timer;
        *prod_timer = new_prod_timer;
        *ty = new_ty;

        *power_consumption_modifier = (new_power_consumption_modifier + 20)
            .clamp(data_store.min_power_mod.into(), u8::MAX.into())
            .try_into()
            .expect("Value clamped already");
        *bonus_productivity = new_bonus_productiviy
            .clamp(0, u8::MAX.into())
            .try_into()
            .expect("Value clamped already");
        assert_eq!(
            new_base_speed,
            data_store.assembler_info[usize::from(new_ty)].base_speed
        );
        *combined_speed_mod = ((new_speed_mod + 20) * i16::from(new_base_speed) / 20)
            .clamp(0, u8::MAX.into())
            .try_into()
            .expect("Value clamped already");

        *raw_speed_mod = new_speed_mod;
        *raw_bonus_productivity = new_bonus_productiviy;
        *raw_power_consumption_modifier = new_power_consumption_modifier;
        *position = new_position;

        self.buckets.waiting_for_update[0].push(AssemblerUpdateInfo {
            assembler: index.try_into().unwrap(),
            ticks_wanted: 1,
        });

        index.try_into().unwrap()
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
            self.hot_data[index].timer,
            self.hot_data[index].prod_timer,
            data_store.assembler_info[self.hot_data[index].ty as usize].base_power_consumption,
            self.cold_data[index].raw_power_consumption_modifier,
            self.cold_data[index].raw_bonus_productivity,
            data_store.assembler_info[self.hot_data[index].ty as usize].base_speed,
            self.cold_data[index].raw_speed_mod,
            self.hot_data[index].ty,
            self.cold_data[index].position,
        );
        for ing in &mut self.ings {
            ing[index] = 0;
        }
        for out in &mut self.outputs {
            out[index] = ITEMCOUNTTYPE::MAX;
        }
        self.hot_data[index].timer = 0;
        self.hot_data[index].prod_timer = 0;
        self.cold_data[index].position = Position {
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
