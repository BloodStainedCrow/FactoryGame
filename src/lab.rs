use crate::{
    assembler::TIMERTYPE,
    data::DataStore,
    item::{IdxTrait, Item, ITEMCOUNTTYPE},
    power::{
        power_grid::{IndexUpdateInfo, PowerGridIdentifier, MAX_POWER_MULT},
        Joule, Watt,
    },
    research::Technology,
};

#[derive(Debug, Clone, serde::Deserialize, serde::Serialize)]
pub struct MultiLabStore {
    pub sciences: Box<[Vec<ITEMCOUNTTYPE>]>,
    timer: Vec<TIMERTYPE>,
    holes: Vec<usize>,
}

impl MultiLabStore {
    #[must_use]
    pub fn new<ItemIdxType: IdxTrait>(science_bottle_items: &[Item<ItemIdxType>]) -> Self {
        Self {
            sciences: vec![Vec::new(); science_bottle_items.len()].into_boxed_slice(),
            timer: vec![],
            holes: vec![],
        }
    }

    #[must_use]
    pub fn join<ItemIdxType: IdxTrait, RecipeIdxType: IdxTrait>(
        mut self,
        other: Self,
        self_grid: PowerGridIdentifier,
        other_grid: PowerGridIdentifier,
        data_store: &DataStore<ItemIdxType, RecipeIdxType>,
    ) -> (
        Self,
        impl IntoIterator<Item = IndexUpdateInfo<ItemIdxType, RecipeIdxType>>,
    ) {
        let mut updates = vec![];

        for (new_idx_offs, (other_idx, _)) in other
            .timer
            .iter()
            .enumerate()
            .filter(|(i, _)| !other.holes.contains(i))
            .enumerate()
        {
            assert_eq!(data_store.science_bottle_items.len(), other.sciences.len());

            for science in &data_store.science_bottle_items {
                updates.push(IndexUpdateInfo {
                    old: (
                        *science,
                        data_store.get_storage_id_for_lab_science(
                            self_grid,
                            other_idx.try_into().unwrap(),
                        ),
                    ),
                    new: (
                        *science,
                        data_store.get_storage_id_for_lab_science(
                            other_grid,
                            (self.timer.len() + new_idx_offs).try_into().unwrap(),
                        ),
                    ),
                });
            }
        }

        let new_sciences = self
            .sciences
            .into_vec()
            .into_iter()
            .zip(other.sciences)
            .map(|(mut s, o)| {
                s.extend(
                    o.into_iter()
                        .enumerate()
                        .filter(|(i, _)| !other.holes.contains(i))
                        .map(|(_, v)| v),
                );
                s
            })
            .collect();

        self.timer.extend(other.timer);

        let ret = Self {
            sciences: new_sciences,
            timer: self.timer,
            holes: self.holes,
        };

        (ret, updates)
    }

    // TODO: Ensure good compilation results (i.e. vectorization)
    pub fn update(&mut self, power_mult: u8, current_research: &Technology) -> (Joule, u32, u16) {
        const POWER_CONSUMPTION: Watt = Watt(600);
        const TICKS_PER_SCIENCE: TIMERTYPE = 1800;

        // assert_eq!(self.red.len(), self.green.len());
        // assert_eq!(self.red.len(), self.military.len());
        // assert_eq!(self.red.len(), self.purple.len());
        // assert_eq!(self.red.len(), self.yellow.len());
        // assert_eq!(self.red.len(), self.space.len());
        // assert_eq!(self.red.len(), self.timer.len());

        let mut times_ings_used = 0;

        let mut running = 0;
        let mut finished_cycle: u16 = 0;
        let increase = (TIMERTYPE::from(power_mult)
            * (TIMERTYPE::MAX / TIMERTYPE::from(MAX_POWER_MULT)))
            / TICKS_PER_SCIENCE;

        // TODO: Only use/check science packs needed by the current technology
        let r_min = ITEMCOUNTTYPE::from(current_research.cost[0] > 0);
        let g_min = ITEMCOUNTTYPE::from(current_research.cost[1] > 0);
        let m_min = ITEMCOUNTTYPE::from(current_research.cost[2] > 0);
        let p_min = ITEMCOUNTTYPE::from(current_research.cost[3] > 0);
        let y_min = ITEMCOUNTTYPE::from(current_research.cost[4] > 0);
        let s_min = ITEMCOUNTTYPE::from(current_research.cost[5] > 0);

        // TODO: use iterators/check that this compiles well
        for (i, timer) in self.timer.iter_mut().enumerate() {
            let science_mul: u16 = self
                .sciences
                .iter()
                .map(|science_list| u16::from(science_list[i] > 0))
                .product();

            let new_timer = timer.wrapping_add(increase);

            let new_timer = new_timer * science_mul;

            let did_finish_work: u8 = (new_timer < *timer).into();

            times_ings_used += u32::from(did_finish_work);

            finished_cycle += u16::from(did_finish_work);

            // Power calculation
            // We use power if any work was done
            // This is also used to run the tech progress?
            running += u32::from(*timer != new_timer);

            *timer = new_timer;
            self.sciences
                .iter_mut()
                .for_each(|science_list| science_list[i] -= did_finish_work);
        }

        (
            POWER_CONSUMPTION.joules_per_tick() * running.into(),
            times_ings_used,
            finished_cycle,
        )
    }
}
