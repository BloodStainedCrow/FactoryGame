use crate::{
    assembler::TIMERTYPE,
    data::DataStore,
    frontend::world::Position,
    item::{IdxTrait, Item, ITEMCOUNTTYPE},
    power::{
        power_grid::{IndexUpdateInfo, PowerGridEntity, PowerGridIdentifier, MAX_POWER_MULT},
        Joule, Watt,
    },
    research::Technology,
};

// TODO: Add variable power consumption and speed
#[derive(Debug, Clone, serde::Deserialize, serde::Serialize)]
pub struct MultiLabStore {
    pub max_insert: Box<[Vec<ITEMCOUNTTYPE>]>,
    pub sciences: Box<[Vec<ITEMCOUNTTYPE>]>,
    timer: Vec<TIMERTYPE>,
    holes: Vec<usize>,

    // This is not used in normal updates, but only for when the indices change (i.e. when merging power networks)
    positions: Vec<Position>,
    types: Vec<u8>,
}

impl MultiLabStore {
    #[must_use]
    pub fn new<ItemIdxType: IdxTrait>(science_bottle_items: &[Item<ItemIdxType>]) -> Self {
        Self {
            max_insert: vec![Vec::new(); science_bottle_items.len()].into_boxed_slice(),
            sciences: vec![Vec::new(); science_bottle_items.len()].into_boxed_slice(),
            timer: vec![],
            holes: vec![],
            positions: vec![],
            types: vec![],
        }
    }

    #[must_use]
    pub fn join<ItemIdxType: IdxTrait, RecipeIdxType: IdxTrait>(
        mut self,
        other: Self,
        new_grid_id: PowerGridIdentifier,
        data_store: &DataStore<ItemIdxType, RecipeIdxType>,
    ) -> (
        Self,
        impl IntoIterator<Item = IndexUpdateInfo<ItemIdxType, RecipeIdxType>>,
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
                new_storage: PowerGridEntity::Lab {
                    index: (old_len + new_index_offs).try_into().unwrap(),
                    ty: other.types[old_index],
                },
                new_grid: new_grid_id,
            });

        let ret = Self {
            max_insert: self.max_insert,
            sciences: self.sciences,
            timer: self.timer,
            holes: self.holes,
            positions: self.positions,
            types: self.types,
        };

        (ret, updates)
    }

    // TODO: Ensure good compilation results (i.e. vectorization)
    pub fn update<ItemIdxType: IdxTrait, RecipeIdxType: IdxTrait>(
        &mut self,
        power_mult: u8,
        current_research: &Option<Technology>,
        data_store: &DataStore<ItemIdxType, RecipeIdxType>,
    ) -> (Joule, u32, u16) {
        const POWER_CONSUMPTION: Watt = Watt(600);
        const TICKS_PER_SCIENCE: TIMERTYPE = 60;

        let Some(current_research) = current_research else {
            // We are not currently researching anything. This means we do not use any items any power or gained any progress
            return (Joule(0), 0, 0);
        };

        let mut times_ings_used = 0;

        let mut running = 0;
        let mut finished_cycle: u16 = 0;
        let increase = (TIMERTYPE::from(power_mult)
            * (TIMERTYPE::MAX / TIMERTYPE::from(MAX_POWER_MULT)))
            / TICKS_PER_SCIENCE;

        let needed: Box<[_]> = data_store.technology_costs[usize::from(current_research.id)]
            .1
            .iter()
            .map(|needed| *needed)
            .collect();

        let mut sciences: Box<[_]> = self
            .sciences
            .iter_mut()
            .map(|v| v.iter_mut().peekable())
            .collect::<Box<[_]>>();

        // TODO: use iterators/check that this compiles well
        for timer in self.timer.iter_mut() {
            let science_mul: u16 = sciences
                .iter_mut()
                .zip(&needed)
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
                .zip(&needed)
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
        data_store: &DataStore<ItemIdxType, RecipeIdxType>,
    ) -> usize {
        // FIXME: respect ty
        let idx = if let Some(hole_idx) = self.holes.pop() {
            self.positions[hole_idx] = position;
            // TODO:
            self.sciences.iter_mut().for_each(|v| v[hole_idx] = 10);
            self.sciences.iter_mut().for_each(|v| v[hole_idx] = 0);
            self.timer[hole_idx] = 0;
            self.types[hole_idx] = ty;

            hole_idx
        } else {
            self.positions.push(position);
            self.sciences.iter_mut().for_each(|v| v.push(10));
            self.sciences.iter_mut().for_each(|v| v.push(0));
            self.timer.push(0);
            self.types.push(ty);

            self.positions.len() - 1
        };

        idx
    }

    pub fn remove_lab(&mut self, index: usize) -> Box<[ITEMCOUNTTYPE]> {
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

        ret
    }

    pub fn move_lab(&mut self, index: usize, other: &mut Self) -> usize {
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

            other.positions.len() - 1
        };

        self.sciences.iter_mut().for_each(|v| {
            v[index] = 0;
        });

        idx
    }
}
