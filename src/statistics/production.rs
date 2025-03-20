use std::{
    borrow::Borrow,
    ops::{Add, AddAssign},
};

use charts_rs::Series;
use itertools::Itertools;

use crate::{
    data::DataStore,
    item::{IdxTrait, Item},
    NewWithDataStore,
};

use super::{recipe::RecipeTickInfo, IntoSeries};

#[derive(Debug, Clone, serde::Deserialize, serde::Serialize)]
pub struct ProductionInfo {
    pub(super) items_produced: Vec<u64>,
    items_used: Vec<u64>,
}

impl ProductionInfo {
    pub fn from_recipe_info<ItemIdxType: IdxTrait, RecipeIdxType: IdxTrait>(
        info: &RecipeTickInfo,
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
                .collect(),
            items_used: data_store
                .item_to_recipe_count_where_its_ingredient
                .iter()
                .map(|values| {
                    values
                        .iter()
                        .map(|(recipe, amount)| {
                            info.num_crafts_finished[recipe.id.into()].full_crafts
                                * u64::from(*amount)
                        })
                        .sum()
                })
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

        for (s, rhs) in self.items_used.iter_mut().zip(rhs.items_used.iter()) {
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

        for (s, rhs) in self.items_used.iter_mut().zip(rhs.items_used.iter()) {
            *s += rhs;
        }
    }
}

impl<ItemIdxType: IdxTrait, RecipeIdxType: IdxTrait>
    IntoSeries<Item<ItemIdxType>, ItemIdxType, RecipeIdxType> for ProductionInfo
{
    fn into_series(
        values: &[Self],
        filter: Option<impl Fn(Item<ItemIdxType>) -> bool>,
        data_store: &DataStore<ItemIdxType, RecipeIdxType>,
    ) -> impl IntoIterator<Item = Series> {
        values
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
    }
}

impl NewWithDataStore for ProductionInfo {
    fn new<ItemIdxType: IdxTrait, RecipeIdxType: IdxTrait>(
        data_store: impl Borrow<DataStore<ItemIdxType, RecipeIdxType>>,
    ) -> Self {
        Self {
            items_produced: vec![0; data_store.borrow().item_names.len()],
            items_used: vec![0; data_store.borrow().item_names.len()],
        }
    }
}
