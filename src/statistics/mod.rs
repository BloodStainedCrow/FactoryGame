use std::{array, ops::AddAssign};

use charts_rs::{LineChart, Series};
use consumption::ConsumptionInfo;
use production::ProductionInfo;

use crate::{
    NewWithDataStore,
    data::DataStore,
    item::{IdxTrait, Item},
    research::ResearchProgress,
};

#[cfg(feature = "client")]
use egui_show_info_derive::ShowInfo;
#[cfg(feature = "client")]
use get_size2::GetSize;

pub mod consumption;
mod power;
pub mod production;
pub mod recipe;
pub mod research;
pub mod time_usage;

pub const NUM_DIFFERENT_TIMESCALES: usize = 5;
pub const SAMPLES_FOR_SMOOTHING_BASE: usize = 600;

const SAMPLES_FOR_SMOOTHING: [usize; NUM_DIFFERENT_TIMESCALES] = {
    let mut b = [0; NUM_DIFFERENT_TIMESCALES];
    let mut i = 0;
    while i < NUM_DIFFERENT_TIMESCALES {
        let mut lower_prod = 1;
        let mut j = 0;
        while j < i {
            lower_prod *= RELATIVE_INTERVAL_MULTS[j + 1];
            j += 1;
        }
        b[i] = SAMPLES_FOR_SMOOTHING_BASE.div_ceil(lower_prod); // map
        i += 1;
    }
    b
};
pub const NUM_SAMPLES_AT_INTERVALS: [usize; NUM_DIFFERENT_TIMESCALES] = [600, 60, 60, 60, 50];
pub const NUM_SAMPLES_AT_INTERVALS_STORED: [usize; NUM_DIFFERENT_TIMESCALES] = {
    let mut b = [0; NUM_DIFFERENT_TIMESCALES];
    let mut i = 0;
    while i < NUM_DIFFERENT_TIMESCALES {
        b[i] = NUM_SAMPLES_AT_INTERVALS[i] + (SAMPLES_FOR_SMOOTHING[i] - 1); // map
        i += 1;
    }
    b
};
pub const NUM_X_AXIS_TICKS: [usize; NUM_DIFFERENT_TIMESCALES] = [10, 6, 6, 10, 10];
pub const RELATIVE_INTERVAL_MULTS: [usize; NUM_DIFFERENT_TIMESCALES] = [1, 60, 60, 10, 5];

pub const TIMESCALE_NAMES: [&'static str; NUM_DIFFERENT_TIMESCALES] =
    ["10 seconds", "1 minute", "1 hour", "10 hours", "50 hours"];

pub const TIMESCALE_LEGEND: [fn(f64) -> String; NUM_DIFFERENT_TIMESCALES] = [
    |t| format!("{:.0}s", t / 60.0),
    |t| format!("{:.0}s", t),
    |t| format!("{:.0}m", t),
    |t| format!("{:.0}m", t * 10.0),
    |t| format!("{:.0}h", t),
];

#[cfg_attr(feature = "client", derive(ShowInfo), derive(GetSize))]
#[derive(Debug, Clone, serde::Deserialize, serde::Serialize)]
pub struct GenStatistics {
    pub production: Timeline<ProductionInfo>,
    pub consumption: Timeline<ConsumptionInfo>,
    pub research: Timeline<u64>,
}

impl GenStatistics {
    pub fn new<ItemIdxType: IdxTrait, RecipeIdxType: IdxTrait>(
        data_store: &DataStore<ItemIdxType, RecipeIdxType>,
    ) -> Self {
        GenStatistics {
            production: Timeline::new(true, data_store),
            consumption: Timeline::new(true, data_store),
            research: Timeline::new(true, data_store),
        }
    }

    #[profiling::function]
    pub fn append_single_set_of_samples(
        &mut self,
        samples: (ProductionInfo, ConsumptionInfo, ResearchProgress),
    ) {
        self.production.append_single_set_of_samples(samples.0);
        self.consumption.append_single_set_of_samples(samples.1);
        self.research.append_single_set_of_samples(samples.2 as u64);
    }

    pub fn get_chart<ItemIdxType: IdxTrait, RecipeIdxType: IdxTrait>(
        &self,
        timescale: usize,
        data_store: &DataStore<ItemIdxType, RecipeIdxType>,
        filter: Option<impl Fn(Item<ItemIdxType>) -> bool>,
    ) -> LineChart {
        let prod_values: Vec<Series> = self
            .production
            .get_series(timescale, data_store, filter)
            .into_iter()
            .map(|v| v.1)
            .collect();

        LineChart::new(
            prod_values,
            vec![".".to_string(); NUM_SAMPLES_AT_INTERVALS[timescale]],
        )
    }
}

pub trait IntoSeries<T, ItemIdxType: IdxTrait, RecipeIdxType: IdxTrait>: Sized {
    fn into_series(
        values: &[Self],
        smoothing_window: usize,
        filter: Option<impl Fn(T) -> bool>,
        data_store: &DataStore<ItemIdxType, RecipeIdxType>,
    ) -> impl Iterator<Item = (usize, Series)>;
}

#[cfg_attr(feature = "client", derive(ShowInfo), derive(GetSize))]
#[derive(Debug, Clone, serde::Deserialize, serde::Serialize)]
pub struct Timeline<T> {
    pub num_samples_pushed: usize,
    samples: [Vec<T>; NUM_DIFFERENT_TIMESCALES],
    pub total: Option<T>,
}

impl<T: NewWithDataStore + Clone + for<'a> AddAssign<&'a T>> Timeline<T> {
    pub fn new<ItemIdxType: IdxTrait, RecipeIdxType: IdxTrait>(
        collect_total: bool,
        data_store: &DataStore<ItemIdxType, RecipeIdxType>,
    ) -> Self {
        Self {
            num_samples_pushed: 0,
            samples: array::from_fn(|i| {
                vec![T::new(data_store); NUM_SAMPLES_AT_INTERVALS_STORED[i]]
            }),
            total: collect_total.then_some(T::new(data_store)),
        }
    }

    pub fn append_single_set_of_samples(&mut self, sample: T) {
        self.num_samples_pushed += 1;
        self.total.as_mut().map(|total| *total += &sample);

        self.samples[0].rotate_right(1);
        self.samples[0][0] = sample;

        // Percolate up the different levels
        for current_level_idx in 1..NUM_DIFFERENT_TIMESCALES {
            let scale_relative_to_base: usize = RELATIVE_INTERVAL_MULTS
                .iter()
                .skip(1)
                .take(current_level_idx)
                .product();

            if self.num_samples_pushed % scale_relative_to_base != 0 {
                break;
            }

            let (level_to_read_from, current_level) = self.samples.split_at_mut(current_level_idx);

            let (level_to_read_from, current_level) =
                (level_to_read_from.last().unwrap(), &mut current_level[0]);

            let relative = RELATIVE_INTERVAL_MULTS[current_level_idx];

            let list_of_samples = level_to_read_from.split_at(relative).0;

            assert!(list_of_samples.len() == relative);

            // FIXME: Use Iter::sum
            let new_sample =
                list_of_samples
                    .iter()
                    .skip(1)
                    .fold(list_of_samples[0].clone(), |mut acc, v| {
                        acc += v;
                        acc
                    });

            current_level.rotate_right(1);
            current_level[0] = new_sample;
        }
    }

    pub fn get_series<
        'a,
        'b,
        Item,
        ItemIdxType: IdxTrait,
        RecipeIdxType: IdxTrait,
        Filter: Fn(Item) -> bool,
    >(
        &'a self,
        timescale: usize,
        data_store: &'b DataStore<ItemIdxType, RecipeIdxType>,
        filter: Option<Filter>,
    ) -> impl Iterator<Item = (usize, Series)> + use<'a, 'b, T, Item, ItemIdxType, RecipeIdxType, Filter>
    where
        T: IntoSeries<Item, ItemIdxType, RecipeIdxType>,
    {
        T::into_series(
            &self.samples[timescale],
            SAMPLES_FOR_SMOOTHING[timescale],
            filter,
            data_store,
        )
    }

    pub fn get_data_points(&self, timescale: usize) -> &[T] {
        &self.samples[timescale]
    }
}
