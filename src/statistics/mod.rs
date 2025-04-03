use std::{array, ops::AddAssign};

use charts_rs::{LineChart, Series};
use log::error;
use production::ProductionInfo;

use crate::{
    data::DataStore,
    item::{IdxTrait, Item},
    research::ResearchProgress,
    NewWithDataStore,
};

mod power;
pub mod production;
pub mod recipe;
pub mod research;

const NUM_DIFFERENT_TIMESCALES: usize = 3;
const NUM_SAMPLES_AT_INTERVALS: [usize; NUM_DIFFERENT_TIMESCALES] = [600, 60, 60];
const RELATIVE_INTERVAL_MULTS: [usize; NUM_DIFFERENT_TIMESCALES] = [1, 60, 60];

const TIMESCALE_NAMES: [&'static str; NUM_DIFFERENT_TIMESCALES] =
    ["10 seconds", "1 minute", "1 hour"];

#[derive(Debug, Clone, serde::Deserialize, serde::Serialize)]
pub struct GenStatistics {
    pub production: Timeline<ProductionInfo>,
    research: Timeline<ResearchProgress>,
}

impl GenStatistics {
    pub fn new<ItemIdxType: IdxTrait, RecipeIdxType: IdxTrait>(
        data_store: &DataStore<ItemIdxType, RecipeIdxType>,
    ) -> Self {
        GenStatistics {
            production: Timeline::new(data_store),
            research: Timeline::new(data_store),
        }
    }

    pub fn append_single_set_of_samples(&mut self, samples: (ProductionInfo, ResearchProgress)) {
        self.production.append_single_set_of_samples(samples.0);
        self.research.append_single_set_of_samples(samples.1);
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
            .collect();

        LineChart::new(
            prod_values,
            vec![".".to_string(); NUM_SAMPLES_AT_INTERVALS[timescale]],
        )
    }
}

trait IntoSeries<T, ItemIdxType: IdxTrait, RecipeIdxType: IdxTrait>: Sized {
    fn into_series(
        values: &[Self],
        filter: Option<impl Fn(T) -> bool>,
        data_store: &DataStore<ItemIdxType, RecipeIdxType>,
    ) -> impl IntoIterator<Item = Series>;
}

#[derive(Debug, Clone, serde::Deserialize, serde::Serialize)]
pub struct Timeline<T> {
    pub num_samples_pushed: usize,
    production_samples: [Vec<T>; NUM_DIFFERENT_TIMESCALES],
    production_total: T,
}

impl<T: NewWithDataStore + Clone + for<'a> AddAssign<&'a T>> Timeline<T> {
    pub fn new<ItemIdxType: IdxTrait, RecipeIdxType: IdxTrait>(
        data_store: &DataStore<ItemIdxType, RecipeIdxType>,
    ) -> Self {
        Self {
            num_samples_pushed: 0,
            production_samples: array::from_fn(|i| {
                vec![T::new(data_store); NUM_SAMPLES_AT_INTERVALS[i]]
            }),
            production_total: T::new(data_store),
        }
    }

    pub fn append_single_set_of_samples(&mut self, sample: T) {
        self.production_total += &sample;

        self.production_samples[0].rotate_right(1);
        self.production_samples[0][0] = sample;

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

            let (level_to_read_from, current_level) =
                // TODO: mid might be wrong here
                self.production_samples.split_at_mut(current_level_idx);

            let (level_to_read_from, current_level) =
                (level_to_read_from.last().unwrap(), &mut current_level[0]);

            let relative = RELATIVE_INTERVAL_MULTS[current_level_idx];

            let list_of_samples = level_to_read_from.split_at(relative).0;

            assert!(list_of_samples.len() == relative);

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

        self.num_samples_pushed += 1;
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
    ) -> impl IntoIterator<Item = Series> + use<'a, 'b, T, Item, ItemIdxType, RecipeIdxType, Filter>
    where
        T: IntoSeries<Item, ItemIdxType, RecipeIdxType>,
    {
        T::into_series(&self.production_samples[timescale], filter, data_store)
    }
}
