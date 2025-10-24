use crate::{
    assembler::TIMERTYPE,
    data::DataStore,
    frontend::world::Position,
    item::{ITEMCOUNTTYPE, IdxTrait, Item, WeakIdxTrait},
    power::{
        Joule, Watt,
        power_grid::{IndexUpdateInfo, MAX_POWER_MULT, PowerGridEntity, PowerGridIdentifier},
    },
};

#[cfg(feature = "client")]
use egui_show_info_derive::ShowInfo;
#[cfg(feature = "client")]
use get_size2::GetSize;

use crate::frontend::world::tile::ModuleTy;

pub const TICKS_PER_SCIENCE: TIMERTYPE = 60;

// TODO: Add variable power consumption and speed
#[cfg_attr(feature = "client", derive(ShowInfo), derive(GetSize))]
#[derive(Debug, Clone, serde::Deserialize, serde::Serialize)]
pub struct MultiLabStore {
    pub max_insert: Box<[Vec<ITEMCOUNTTYPE>]>,
    pub sciences: Box<[Vec<ITEMCOUNTTYPE>]>,
    timer: Vec<TIMERTYPE>,
    holes: Vec<usize>,

    /// Base Crafting Speed in 5% increments
    /// i.e. 28 => 140% Crafting speed
    /// Maximum is 1275% Crafting Speed
    base_speed: Vec<u8>,

    /// Crafting Speed in 5% increments
    /// i.e. 28 => 140% Crafting speed
    /// Maximum is 1275% Crafting Speed
    combined_speed_mod: Vec<u8>,
    /// Bonus Productivity in %
    bonus_productivity: Vec<u8>,
    /// Power Consumption in 5% increments
    /// i.e. 28 => 140% Crafting speed
    /// Maximum is 1275% x Base Power Consumption
    power_consumption_modifier: Vec<u8>,

    raw_speed_mod: Vec<i16>,
    raw_bonus_productivity: Vec<i16>,
    raw_power_consumption_modifier: Vec<i16>,

    // TODO: This can likely be smaller than full u64
    base_power_consumption: Vec<Watt>,

    // This is not used in normal updates, but only for when the indices change (i.e. when merging power networks)
    positions: Vec<Position>,
    types: Vec<u8>,
}

pub struct LabViewInfo<ItemIdxType: WeakIdxTrait> {
    pub items: Vec<(Item<ItemIdxType>, ITEMCOUNTTYPE)>,
    pub timer_percentage: f32,
    pub prod_timer_percentage: f32,
    pub base_speed: f32,
    pub speed_mod: f32,
    pub prod_mod: f32,
    pub power_consumption_mod: f32,
    pub base_power_consumption: Watt,
}

impl MultiLabStore {
    #[must_use]
    pub fn new<ItemIdxType: IdxTrait>(science_bottle_items: &[Item<ItemIdxType>]) -> Self {
        Self {
            max_insert: vec![Vec::new(); science_bottle_items.len()].into_boxed_slice(),
            sciences: vec![Vec::new(); science_bottle_items.len()].into_boxed_slice(),
            timer: vec![],

            base_speed: vec![],
            combined_speed_mod: vec![],
            bonus_productivity: vec![],
            power_consumption_modifier: vec![],

            raw_speed_mod: vec![],
            raw_bonus_productivity: vec![],
            raw_power_consumption_modifier: vec![],

            base_power_consumption: vec![],

            holes: vec![],
            positions: vec![],
            types: vec![],
        }
    }

    #[must_use]
    pub fn join<ItemIdxType: IdxTrait, RecipeIdxType: IdxTrait>(
        mut self,
        other: Self,
        other_grid_id: PowerGridIdentifier,
        new_grid_id: PowerGridIdentifier,
        _data_store: &DataStore<ItemIdxType, RecipeIdxType>,
    ) -> (
        Self,
        impl IntoIterator<Item = IndexUpdateInfo<ItemIdxType, RecipeIdxType>>
        + use<ItemIdxType, RecipeIdxType>,
    ) {
        let old_len = self.positions.len();

        self.max_insert
            .iter_mut()
            .zip(other.max_insert)
            .for_each(|(s, o)| {
                s.extend(
                    o.into_iter()
                        .enumerate()
                        .filter(|(i, _)| !other.holes.contains(i))
                        .map(|(_, v)| v),
                );
            });

        self.sciences
            .iter_mut()
            .zip(other.sciences)
            .for_each(|(s, o)| {
                s.extend(
                    o.into_iter()
                        .enumerate()
                        .filter(|(i, _)| !other.holes.contains(i))
                        .map(|(_, v)| v),
                );
            });

        self.timer.extend(
            other
                .timer
                .into_iter()
                .enumerate()
                .filter(|(i, _)| !other.holes.contains(i))
                .map(|(_, v)| v),
        );

        self.base_speed.extend(
            other
                .base_speed
                .into_iter()
                .enumerate()
                .filter(|(i, _)| !other.holes.contains(i))
                .map(|(_, v)| v),
        );

        self.combined_speed_mod.extend(
            other
                .combined_speed_mod
                .into_iter()
                .enumerate()
                .filter(|(i, _)| !other.holes.contains(i))
                .map(|(_, v)| v),
        );

        self.bonus_productivity.extend(
            other
                .bonus_productivity
                .into_iter()
                .enumerate()
                .filter(|(i, _)| !other.holes.contains(i))
                .map(|(_, v)| v),
        );

        self.power_consumption_modifier.extend(
            other
                .power_consumption_modifier
                .into_iter()
                .enumerate()
                .filter(|(i, _)| !other.holes.contains(i))
                .map(|(_, v)| v),
        );

        self.raw_speed_mod.extend(
            other
                .raw_speed_mod
                .into_iter()
                .enumerate()
                .filter(|(i, _)| !other.holes.contains(i))
                .map(|(_, v)| v),
        );

        self.raw_bonus_productivity.extend(
            other
                .raw_bonus_productivity
                .into_iter()
                .enumerate()
                .filter(|(i, _)| !other.holes.contains(i))
                .map(|(_, v)| v),
        );

        self.raw_power_consumption_modifier.extend(
            other
                .raw_power_consumption_modifier
                .into_iter()
                .enumerate()
                .filter(|(i, _)| !other.holes.contains(i))
                .map(|(_, v)| v),
        );

        self.base_power_consumption.extend(
            other
                .base_power_consumption
                .into_iter()
                .enumerate()
                .filter(|(i, _)| !other.holes.contains(i))
                .map(|(_, v)| v),
        );

        self.positions.extend(
            other
                .positions
                .iter()
                .copied()
                .enumerate()
                .filter(|(i, _)| !other.holes.contains(i))
                .map(|(_, v)| v),
        );

        self.types.extend(
            other
                .types
                .iter()
                .copied()
                .enumerate()
                .filter(|(i, _)| !other.holes.contains(i))
                .map(|(_, v)| v),
        );

        let updates = other
            .positions
            .into_iter()
            .enumerate()
            .filter(move |(i, _)| !other.holes.contains(i))
            .enumerate()
            .map(move |(new_index_offs, (old_index, pos))| IndexUpdateInfo {
                position: pos,
                old_pg_entity: PowerGridEntity::Lab {
                    index: old_index.try_into().unwrap(),
                    ty: other.types[old_index],
                },
                new_pg_entity: PowerGridEntity::Lab {
                    index: (old_len + new_index_offs).try_into().unwrap(),
                    ty: other.types[old_index],
                },
                old_grid: other_grid_id,
                new_grid: new_grid_id,
            });

        let ret = Self {
            max_insert: self.max_insert,
            sciences: self.sciences,
            timer: self.timer,
            holes: self.holes,
            positions: self.positions,

            base_power_consumption: self.base_power_consumption,
            base_speed: self.base_speed,
            bonus_productivity: self.bonus_productivity,
            combined_speed_mod: self.combined_speed_mod,
            power_consumption_modifier: self.power_consumption_modifier,
            raw_bonus_productivity: self.raw_bonus_productivity,
            raw_power_consumption_modifier: self.raw_power_consumption_modifier,
            raw_speed_mod: self.raw_speed_mod,

            types: self.types,
        };

        (ret, updates)
    }

    // TODO: Ensure good compilation results (i.e. vectorization)
    // FIXME: Use module modifiers
    pub fn update(
        &mut self,
        power_mult: u8,
        current_research_costs: Option<&[u8]>,
    ) -> (Joule, u32, u16) {
        const POWER_CONSUMPTION: Watt = Watt(600);

        let Some(current_research_costs) = current_research_costs else {
            // We are not currently researching anything. This means we do not use any items any power or gained any progress
            return (Joule(0), 0, 0);
        };

        let needed = current_research_costs;

        let mut times_ings_used = 0;

        let mut running = 0;
        let mut finished_cycle: u16 = 0;
        let increase = (TIMERTYPE::from(power_mult)
            * (TIMERTYPE::MAX / TIMERTYPE::from(MAX_POWER_MULT)))
            / TICKS_PER_SCIENCE;

        let mut sciences: Box<[_]> = self
            .sciences
            .iter_mut()
            .map(|v| v.iter_mut().peekable())
            .collect::<Box<[_]>>();

        // TODO: use iterators/check that this compiles well
        for timer in self.timer.iter_mut() {
            // // FIXME: FOR TESTING ONLY
            // for science in sciences.iter_mut() {
            //     **science.peek_mut().unwrap() = 0;
            // }
            // // FIXME: FOR TESTING ONLY END

            let science_mul: u16 = sciences
                .iter_mut()
                .zip(needed)
                .map(|(science_iter, needed)| (science_iter.peek().unwrap(), needed))
                .map(|(v, needed)| u16::from(**v >= *needed))
                .product();

            let new_timer_if_all_required = timer.wrapping_add(increase);

            let new_timer = new_timer_if_all_required * science_mul + *timer * (1 - science_mul);

            let did_finish_work: u8 = (new_timer < *timer).into();

            times_ings_used += u32::from(did_finish_work);

            finished_cycle += u16::from(did_finish_work);

            // Power calculation
            // We use power if any work was done
            // This is also used to run the tech progress?
            running += u32::from(science_mul);

            *timer = new_timer;
            sciences
                .iter_mut()
                .zip(needed)
                .map(|(science_iter, needed)| (science_iter.next().unwrap(), needed))
                .for_each(|(v, needed)| *v -= *needed * did_finish_work)
        }

        (
            POWER_CONSUMPTION.joules_per_tick() * running.into(),
            times_ings_used,
            finished_cycle,
        )
    }

    pub fn add_lab<ItemIdxType: IdxTrait, RecipeIdxType: IdxTrait>(
        &mut self,
        ty: u8,
        position: Position,
        modules: &[Option<ModuleTy>],
        data_store: &DataStore<ItemIdxType, RecipeIdxType>,
    ) -> u32 {
        let base_speed = data_store.lab_info[usize::from(ty)].base_speed;
        let base_prod = data_store.lab_info[usize::from(ty)].base_prod;
        let base_power = data_store.lab_info[usize::from(ty)].base_power_consumption;

        let (speed, prod, power) = modules
            .iter()
            .flatten()
            .map(|module| {
                (
                    data_store.module_info[*module as usize].speed_mod as i16,
                    data_store.module_info[*module as usize].prod_mod as i16,
                    data_store.module_info[*module as usize].power_mod as i16,
                )
            })
            .reduce(|acc, v| (acc.0 + v.0, acc.1 + v.1, acc.2 + v.2))
            .unwrap_or((0, 0, 0));

        let idx = if let Some(hole_idx) = self.holes.pop() {
            self.positions[hole_idx] = position;
            // TODO:
            self.max_insert.iter_mut().for_each(|v| v[hole_idx] = 10);
            self.sciences.iter_mut().for_each(|v| v[hole_idx] = 0);
            self.timer[hole_idx] = 0;
            self.types[hole_idx] = ty;

            self.base_power_consumption[hole_idx] = base_power;

            self.base_speed[hole_idx] = base_speed;
            self.raw_power_consumption_modifier[hole_idx] = power;
            self.raw_bonus_productivity[hole_idx] = i16::from(base_prod) + prod;
            self.raw_speed_mod[hole_idx] = speed;

            self.power_consumption_modifier[hole_idx] = (power + 20)
                .clamp(data_store.min_power_mod.into(), u8::MAX.into())
                .try_into()
                .expect("Value clamped already");
            self.bonus_productivity[hole_idx] = (i16::from(base_prod) + prod)
                .clamp(0, u8::MAX.into())
                .try_into()
                .expect("Value clamped already");
            self.combined_speed_mod[hole_idx] = ((speed + 20) * i16::from(base_speed) / 20)
                .clamp(0, u8::MAX.into())
                .try_into()
                .expect("Value clamped already");

            hole_idx
        } else {
            self.positions.push(position);
            self.max_insert.iter_mut().for_each(|v| v.push(10));
            self.sciences.iter_mut().for_each(|v| v.push(0));
            self.timer.push(0);
            self.types.push(ty);

            self.base_power_consumption.push(base_power);

            self.base_speed.push(base_speed);
            self.raw_power_consumption_modifier.push(power);
            self.raw_bonus_productivity
                .push(i16::from(base_prod) + prod);
            self.raw_speed_mod.push(speed);

            self.power_consumption_modifier.push(
                (power + 20)
                    .clamp(data_store.min_power_mod.into(), u8::MAX.into())
                    .try_into()
                    .expect("Value clamped already"),
            );
            self.bonus_productivity.push(
                (i16::from(base_prod) + prod)
                    .clamp(0, u8::MAX.into())
                    .try_into()
                    .expect("Value clamped already"),
            );
            self.combined_speed_mod.push(
                ((speed + 20) * i16::from(base_speed) / 20)
                    .clamp(0, u8::MAX.into())
                    .try_into()
                    .expect("Value clamped already"),
            );

            self.positions.len() - 1
        };

        idx.try_into().expect("More than u32::MAX Labs in a grid")
    }

    pub fn remove_lab(&mut self, index: u32) -> Box<[ITEMCOUNTTYPE]> {
        let index = index as usize;
        self.holes.push(index);

        let ret = self
            .sciences
            .iter_mut()
            .map(|v| {
                let r = v[index];
                v[index] = 0;
                r
            })
            .collect();

        self.base_power_consumption[index] = Watt(0);
        self.sciences
            .iter_mut()
            .for_each(|v| v[usize::from(index)] = 0);

        ret
    }

    pub fn move_lab(&mut self, index: u32, other: &mut Self) -> u32 {
        let index = index as usize;
        self.holes.push(index);

        // FIXME: respect ty
        let idx = if let Some(hole_idx) = other.holes.pop() {
            other.positions[hole_idx] = self.positions[index];
            other
                .max_insert
                .iter_mut()
                .zip(self.max_insert.iter().map(|v| v[index]))
                .for_each(|(v, value)| v[hole_idx] = value);
            other
                .sciences
                .iter_mut()
                .zip(self.sciences.iter().map(|v| v[index]))
                .for_each(|(v, value)| v[hole_idx] = value);
            other.timer[hole_idx] = self.timer[index];
            other.types[hole_idx] = self.types[index];

            other.base_power_consumption[hole_idx] = self.base_power_consumption[index];
            other.base_speed[hole_idx] = self.base_speed[index];
            other.raw_bonus_productivity[hole_idx] = self.raw_bonus_productivity[index];
            other.raw_power_consumption_modifier[hole_idx] =
                self.raw_power_consumption_modifier[index];
            other.raw_speed_mod[hole_idx] = self.raw_speed_mod[index];
            other.bonus_productivity[hole_idx] = self.bonus_productivity[index];
            other.combined_speed_mod[hole_idx] = self.combined_speed_mod[index];
            other.power_consumption_modifier[hole_idx] = self.power_consumption_modifier[index];

            hole_idx
        } else {
            other.positions.push(self.positions[index]);
            other
                .sciences
                .iter_mut()
                .zip(self.sciences.iter().map(|v| v[index]))
                .for_each(|(v, value)| v.push(value));
            other
                .max_insert
                .iter_mut()
                .zip(self.max_insert.iter().map(|v| v[index]))
                .for_each(|(v, value)| v.push(value));
            other.timer.push(self.timer[index]);
            other.types.push(self.types[index]);

            other
                .base_power_consumption
                .push(self.base_power_consumption[index]);
            other.base_speed.push(self.base_speed[index]);
            other
                .raw_bonus_productivity
                .push(self.raw_bonus_productivity[index]);
            other
                .raw_power_consumption_modifier
                .push(self.raw_power_consumption_modifier[index]);
            other.raw_speed_mod.push(self.raw_speed_mod[index]);
            other
                .bonus_productivity
                .push(self.bonus_productivity[index]);
            other
                .combined_speed_mod
                .push(self.combined_speed_mod[index]);
            other
                .power_consumption_modifier
                .push(self.power_consumption_modifier[index]);

            other.positions.len() - 1
        };

        self.sciences.iter_mut().for_each(|v| {
            v[index] = 0;
        });

        idx.try_into()
            .expect("More than u32::MAX Labs in a single grid")
    }

    pub fn modify_modifiers<ItemIdxType: IdxTrait, RecipeIdxType: IdxTrait>(
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
            / 10)
            .clamp(0, u8::MAX.into())
            .try_into()
            .expect("Values already clamped");
    }

    pub fn get_lab_info<ItemIdxType: WeakIdxTrait, RecipeIdxType: IdxTrait>(
        &self,
        index: u32,
        data_store: &DataStore<ItemIdxType, RecipeIdxType>,
    ) -> LabViewInfo<ItemIdxType> {
        LabViewInfo {
            items: data_store
                .science_bottle_items
                .iter()
                .copied()
                .zip(self.sciences.iter().map(|list| list[index as usize]))
                .collect(),
            timer_percentage: self.timer[index as usize] as f32 / TIMERTYPE::MAX as f32,
            // prod_timer_percentage: self.prod_timer[index as usize] as f32 / TIMERTYPE::MAX as f32,
            prod_timer_percentage: 0.0 / TIMERTYPE::MAX as f32,
            base_speed: self.base_speed[index as usize] as f32 / 20.0,
            speed_mod: self.raw_speed_mod[index as usize] as f32 / 20.0,
            prod_mod: self.raw_bonus_productivity[index as usize] as f32 / 100.0,
            power_consumption_mod: self.power_consumption_modifier[index as usize] as f32 / 20.0,
            base_power_consumption: self.base_power_consumption[index as usize],
        }
    }
}
