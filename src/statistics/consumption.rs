use std::{
    borrow::Borrow,
    collections::BTreeMap,
    ops::{Add, AddAssign},
};

use charts_rs::Series;
use itertools::Itertools;

use crate::{
    data::DataStore,
    item::{IdxTrait, Indexable, Item},
    NewWithDataStore,
};

use crate::research::LabTickInfo;

use super::{recipe::RecipeTickInfo, IntoSeries};

#[derive(Debug, Clone, serde::Deserialize, serde::Serialize)]
pub struct ConsumptionInfo {
    items_consumed: Vec<u64>,
}

impl ConsumptionInfo {
    pub fn from_infos<ItemIdxType: IdxTrait, RecipeIdxType: IdxTrait>(
        recipe_info: &RecipeTickInfo,
        lab_info: &Option<LabTickInfo>,
        data_store: &DataStore<ItemIdxType, RecipeIdxType>,
    ) -> Self {
        let mut ret = Self {
            items_consumed: data_store
                .item_to_recipe_count_where_its_ingredient
                .iter()
                .map(|values| {
                    values
                        .iter()
                        .map(|(recipe, amount)| {
                            recipe_info.num_crafts_finished[recipe.id.into()].full_crafts
                                * u64::from(*amount)
                        })
                        .sum()
                })
                .collect(),
        };

        if let Some(lab_info) = lab_info {
            let costs = &data_store.technology_costs[lab_info.tech.id as usize].1;

            for (cost, item) in costs.iter().zip(data_store.science_bottle_items.iter()) {
                ret.items_consumed[item.into_usize()] +=
                    u64::from(*cost) * lab_info.times_labs_used_science;
            }
        }

        ret
    }
}

impl Add<&ConsumptionInfo> for ConsumptionInfo {
    type Output = ConsumptionInfo;

    fn add(mut self, rhs: &ConsumptionInfo) -> Self::Output {
        for (s, rhs) in self
            .items_consumed
            .iter_mut()
            .zip(rhs.items_consumed.iter())
        {
            *s += rhs;
        }

        self
    }
}

impl AddAssign<&ConsumptionInfo> for ConsumptionInfo {
    fn add_assign(&mut self, rhs: &ConsumptionInfo) {
        for (s, rhs) in self
            .items_consumed
            .iter_mut()
            .zip(rhs.items_consumed.iter())
        {
            *s += rhs;
        }
    }
}

impl<ItemIdxType: IdxTrait, RecipeIdxType: IdxTrait>
    IntoSeries<Item<ItemIdxType>, ItemIdxType, RecipeIdxType> for ConsumptionInfo
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
                    info.items_consumed
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

impl NewWithDataStore for ConsumptionInfo {
    fn new<ItemIdxType: IdxTrait, RecipeIdxType: IdxTrait>(
        data_store: impl Borrow<DataStore<ItemIdxType, RecipeIdxType>>,
    ) -> Self {
        Self {
            items_consumed: vec![0; data_store.borrow().item_names.len()],
        }
    }
}
