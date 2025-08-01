use std::{iter::Sum, ops::Add};

use crate::{data::DataStore, item::IdxTrait};

#[derive(Debug, Clone)]
pub struct RecipeTickInfo {
    pub(super) num_crafts_finished: Vec<SingleRecipeTickInfo>,
}

#[derive(Debug, Clone, Default)]
pub struct SingleRecipeTickInfo {
    pub full_crafts: u64,
    pub prod_crafts: u64,
}

pub struct RecipeTickInfoParts {
    pub recipes_0_1: Vec<SingleRecipeTickInfo>,
    pub recipes_1_1: Vec<SingleRecipeTickInfo>,
    pub recipes_2_1: Vec<SingleRecipeTickInfo>,
    pub recipes_2_2: Vec<SingleRecipeTickInfo>,
    pub recipes_2_3: Vec<SingleRecipeTickInfo>,
    pub recipes_3_1: Vec<SingleRecipeTickInfo>,
    pub recipes_4_1: Vec<SingleRecipeTickInfo>,
    pub recipes_5_1: Vec<SingleRecipeTickInfo>,
    pub recipes_6_1: Vec<SingleRecipeTickInfo>,
    // etc.
}

impl RecipeTickInfo {
    pub fn new<ItemIdxType: IdxTrait, RecipeIdxType: IdxTrait>(
        data_store: &DataStore<ItemIdxType, RecipeIdxType>,
    ) -> Self {
        Self {
            num_crafts_finished: vec![
                SingleRecipeTickInfo::default();
                data_store.recipe_timers.len()
            ],
        }
    }

    pub fn from_parts<ItemIdxType: IdxTrait, RecipeIdxType: IdxTrait>(
        parts: RecipeTickInfoParts,
        data_store: &DataStore<ItemIdxType, RecipeIdxType>,
    ) -> Self {
        let mut recipes_0_1 = parts.recipes_0_1.into_iter();
        let mut recipes_1_1 = parts.recipes_1_1.into_iter();
        let mut recipes_2_1 = parts.recipes_2_1.into_iter();
        let mut recipes_2_2 = parts.recipes_2_2.into_iter();
        let mut recipes_2_3 = parts.recipes_2_3.into_iter();
        let mut recipes_3_1 = parts.recipes_3_1.into_iter();
        let mut recipes_4_1 = parts.recipes_4_1.into_iter();
        let mut recipes_5_1 = parts.recipes_5_1.into_iter();
        let mut recipes_6_1 = parts.recipes_6_1.into_iter();

        let num_crafts_finished = data_store
            .recipe_num_ing_lookup
            .iter()
            .zip(data_store.recipe_num_out_lookup.iter())
            .map(move |(num_ings, num_outs)| match (num_ings, num_outs) {
                (0, 1) => recipes_0_1.next().expect(
                    "Number of recipes in parts does not match number of recipes in datastore",
                ),
                (1, 1) => recipes_1_1.next().expect(
                    "Number of recipes in parts does not match number of recipes in datastore",
                ),
                (2, 1) => recipes_2_1.next().expect(
                    "Number of recipes in parts does not match number of recipes in datastore",
                ),
                (2, 2) => recipes_2_2.next().expect(
                    "Number of recipes in parts does not match number of recipes in datastore",
                ),
                (2, 3) => recipes_2_3.next().expect(
                    "Number of recipes in parts does not match number of recipes in datastore",
                ),
                (3, 1) => recipes_3_1.next().expect(
                    "Number of recipes in parts does not match number of recipes in datastore",
                ),
                (4, 1) => recipes_4_1.next().expect(
                    "Number of recipes in parts does not match number of recipes in datastore",
                ),
                (5, 1) => recipes_5_1.next().expect(
                    "Number of recipes in parts does not match number of recipes in datastore",
                ),
                (6, 1) => recipes_6_1.next().expect(
                    "Number of recipes in parts does not match number of recipes in datastore",
                ),
                // etc
                _ => unreachable!(),
            })
            .collect();

        Self {
            num_crafts_finished,
        }
    }
}

impl Add<&RecipeTickInfo> for RecipeTickInfo {
    type Output = RecipeTickInfo;

    fn add(mut self, rhs: &RecipeTickInfo) -> Self::Output {
        for (s, rhs) in self
            .num_crafts_finished
            .iter_mut()
            .zip(rhs.num_crafts_finished.iter())
        {
            s.full_crafts += rhs.full_crafts;
            s.prod_crafts += rhs.prod_crafts;
        }

        self
    }
}

impl Sum for RecipeTickInfo {
    fn sum<I: Iterator<Item = Self>>(iter: I) -> Self {
        iter.fold(
            RecipeTickInfo {
                num_crafts_finished: vec![],
            },
            |mut acc, v| {
                acc.num_crafts_finished
                    .extend_from_slice(&v.num_crafts_finished);
                acc
            },
        )
    }
}
