use std::array;

use charts_rs::{LineChart, Series};
use itertools::Itertools;
use log::error;
use production::ProductionInfo;
use research::ResearchInfo;

use crate::{
    data::DataStore,
    item::{IdxTrait, Item},
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
pub struct Statistics {
    pub num_samples_pushed: usize,
    production_samples: [Vec<ProductionInfo>; NUM_DIFFERENT_TIMESCALES],
    production_total: ProductionInfo,
}

impl Statistics {
    pub fn new<ItemIdxType: IdxTrait, RecipeIdxType: IdxTrait>(
        data_store: &DataStore<ItemIdxType, RecipeIdxType>,
    ) -> Self {
        Statistics {
            num_samples_pushed: 0,
            production_samples: array::from_fn(|i| {
                vec![ProductionInfo::new(data_store); NUM_SAMPLES_AT_INTERVALS[i]]
            }),
            production_total: ProductionInfo::new(data_store),
        }
    }

    pub fn append_single_set_of_samples(&mut self, samples: (ProductionInfo, ResearchInfo)) {
        if samples.0.items_produced.iter().any(|v| *v > 0) {
            error!("PRODUCED SOMETHING");
        }

        self.production_total += &samples.0;

        self.production_samples[0].rotate_right(1);
        self.production_samples[0][0] = samples.0;

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

            dbg!(scale_relative_to_base);

            let (level_to_read_from, current_level) =
                // TODO: mid might be wrong here
                self.production_samples.split_at_mut(current_level_idx);

            let (level_to_read_from, current_level) =
                (level_to_read_from.last().unwrap(), &mut current_level[0]);

            let relative = RELATIVE_INTERVAL_MULTS[current_level_idx];

            let list_of_samples = level_to_read_from.split_at(relative).0;

            assert!(list_of_samples.len() == relative);

            let new_sample: ProductionInfo = list_of_samples
                .iter()
                .skip(1)
                .fold(list_of_samples[0].clone(), |acc, v| acc + v);

            current_level.rotate_right(1);
            current_level[0] = new_sample;
        }

        self.num_samples_pushed += 1;
    }

    pub fn get_chart<ItemIdxType: IdxTrait, RecipeIdxType: IdxTrait>(
        &self,
        timescale: usize,
        data_store: &DataStore<ItemIdxType, RecipeIdxType>,
        filter: Option<impl Fn(Item<ItemIdxType>) -> bool>,
    ) -> LineChart {
        let values: Vec<Series> = self.production_samples[timescale]
            .iter()
            .map(|info| {
                info.items_produced
                    .iter()
                    .zip(data_store.item_names.iter())
                    .enumerate()
                    .filter_map(|(item_id, v)| {
                        filter
                            .as_ref()
                            .map(|filter| {
                                filter(Item {
                                    id: ItemIdxType::try_from(item_id).unwrap(),
                                })
                            })
                            .unwrap_or(true)
                            .then_some(v)
                    })
            })
            .flatten()
            .map(|(v, k)| (k, v))
            .into_group_map()
            .into_iter()
            .map(|a| (a.0.as_str(), a.1.into_iter().map(|v| *v as f32).collect()).into())
            .collect();

        dbg!(values.len());

        LineChart::new(
            values,
            vec![".".to_string(); NUM_SAMPLES_AT_INTERVALS[timescale]],
        )
    }
}
