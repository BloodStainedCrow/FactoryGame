use std::iter;

use charts_rs::Series;

use crate::{
    data::DataStore,
    item::{IdxTrait, Item},
};

use super::IntoSeries;

impl<ItemIdxType: IdxTrait, RecipeIdxType: IdxTrait>
    IntoSeries<Item<ItemIdxType>, ItemIdxType, RecipeIdxType> for u32
{
    fn into_series(
        values: &[Self],
        filter: Option<impl Fn(Item<ItemIdxType>) -> bool>,
        data_store: &DataStore<ItemIdxType, RecipeIdxType>,
    ) -> impl IntoIterator<Item = Series> {
        iter::once(
            (
                "Power Satisfaction",
                values
                    .iter()
                    .map(|power_mult| *power_mult as f32)
                    .collect::<Vec<_>>(),
            )
                .into(),
        )
    }
}
