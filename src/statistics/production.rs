use std::{
    borrow::Borrow,
    collections::BTreeMap,
    ops::{Add, AddAssign},
};

use charts_rs::Series;
use itertools::Itertools;

use crate::{
    NewWithDataStore,
    data::DataStore,
    item::{IdxTrait, Item},
};

use super::{IntoSeries, recipe::RecipeTickInfo};

#[cfg(feature = "client")]
use egui_show_info_derive::ShowInfo;
#[cfg(feature = "client")]
use get_size2::GetSize;

#[cfg_attr(feature = "client", derive(ShowInfo), derive(GetSize))]
#[derive(Debug, Clone, serde::Deserialize, serde::Serialize)]
pub struct ProductionInfo {
    pub items_produced: Vec<u64>,
}

impl ProductionInfo {
    pub fn from_recipe_info_and_per_item<ItemIdxType: IdxTrait, RecipeIdxType: IdxTrait>(
        info: &RecipeTickInfo,
        per_item_counts: impl IntoIterator<Item = u32>,
        data_store: &DataStore<ItemIdxType, RecipeIdxType>,
    ) -> Self {
        Self {
            items_produced: data_store
                .item_to_recipe_where_its_output
                .iter()
                .map(|values| {
                    values
                        .iter()
                        .map(|(recipe, amount)| {
                            (info.num_crafts_finished[recipe.id.into()].full_crafts
                                + info.num_crafts_finished[recipe.id.into()].prod_crafts)
                                * u64::from(*amount)
                        })
                        .sum()
                })
                .zip(per_item_counts)
                .map(|(a, b): (u64, u32)| a + b as u64)
                .collect(),
        }
    }
}

impl Add<&ProductionInfo> for ProductionInfo {
    type Output = ProductionInfo;

    fn add(mut self, rhs: &ProductionInfo) -> Self::Output {
        for (s, rhs) in self
            .items_produced
            .iter_mut()
            .zip(rhs.items_produced.iter())
        {
            *s += rhs;
        }

        self
    }
}

impl AddAssign<&ProductionInfo> for ProductionInfo {
    fn add_assign(&mut self, rhs: &ProductionInfo) {
        for (s, rhs) in self
            .items_produced
            .iter_mut()
            .zip(rhs.items_produced.iter())
        {
            *s += rhs;
        }
    }
}

impl<ItemIdxType: IdxTrait, RecipeIdxType: IdxTrait>
    IntoSeries<Item<ItemIdxType>, ItemIdxType, RecipeIdxType> for ProductionInfo
{
    fn into_series(
        values: &[Self],
        smoothing_window: usize,
        filter: Option<impl Fn(Item<ItemIdxType>) -> bool>,
        data_store: &DataStore<ItemIdxType, RecipeIdxType>,
    ) -> impl Iterator<Item = (usize, Series)> {
        BTreeMap::from_iter(
            values
                .windows(smoothing_window)
                .map(|infos| {
                    infos
                        .iter()
                        .fold(ProductionInfo::new(data_store), |a, b| a + b)
                        .items_produced
                        .into_iter()
                        .zip(data_store.item_display_names.iter())
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
                                .then_some(((item_id, v.1), v.0))
                        })
                })
                .flatten()
                .into_group_map()
                .into_iter()
                .map(|(k, v)| (k.0, (k.1, v))),
        )
        .into_iter()
        .map(move |(item_id, a)| {
            (
                item_id.try_into().unwrap(),
                (
                    a.0.as_str(),
                    a.1.into_iter()
                        .map(|v| v as f32 / smoothing_window as f32)
                        .collect(),
                )
                    .into(),
            )
        })
    }
}

impl NewWithDataStore for ProductionInfo {
    fn new<ItemIdxType: IdxTrait, RecipeIdxType: IdxTrait>(
        data_store: impl Borrow<DataStore<ItemIdxType, RecipeIdxType>>,
    ) -> Self {
        Self {
            items_produced: vec![0; data_store.borrow().item_names.len()],
        }
    }
}
