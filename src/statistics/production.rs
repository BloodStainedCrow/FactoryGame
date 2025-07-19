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

#[derive(Debug, Clone, serde::Deserialize, serde::Serialize)]
pub struct ProductionInfo {
    pub items_produced: Vec<u64>,
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
        filter: Option<impl Fn(Item<ItemIdxType>) -> bool>,
        data_store: &DataStore<ItemIdxType, RecipeIdxType>,
    ) -> impl Iterator<Item = (usize, Series)> {
        BTreeMap::from_iter(
            values
                .iter()
                .map(|info| {
                    info.items_produced
                        .iter()
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
                                .then_some(((item_id, v.1), *v.0))
                        })
                })
                .flatten()
                .into_group_map()
                .into_iter()
                .map(|(k, v)| (k.0, (k.1, v))),
        )
        .into_iter()
        .map(|(item_id, a)| {
            (
                item_id.try_into().unwrap(),
                (a.0.as_str(), a.1.into_iter().map(|v| v as f32).collect()).into(),
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
