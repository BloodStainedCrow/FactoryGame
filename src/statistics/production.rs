use std::ops::{Add, AddAssign};

use crate::{data::DataStore, item::IdxTrait};

use super::recipe::RecipeTickInfo;

#[derive(Debug, Clone, serde::Deserialize, serde::Serialize)]
pub struct ProductionInfo {
    pub(super) items_produced: Vec<u64>,
    items_used: Vec<u64>,
}

impl ProductionInfo {
    pub fn new<ItemIdxType: IdxTrait, RecipeIdxType: IdxTrait>(
        data_store: &DataStore<ItemIdxType, RecipeIdxType>,
    ) -> Self {
        Self {
            items_produced: vec![0; data_store.item_names.len()],
            items_used: vec![0; data_store.item_names.len()],
        }
    }

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
                .item_to_recipe_where_its_ingredient
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
