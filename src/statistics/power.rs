use std::iter;

use crate::{
    data::DataStore,
    item::{IdxTrait, Item},
    statistics::Series,
};

use super::IntoSeries;

impl<ItemIdxType: IdxTrait, RecipeIdxType: IdxTrait>
    IntoSeries<Item<ItemIdxType>, ItemIdxType, RecipeIdxType> for u32
{
    fn into_series(
        values: &[Self],
        smoothing_window: usize,
        _filter: Option<impl Fn(Item<ItemIdxType>) -> bool>,
        _data_store: &DataStore<ItemIdxType, RecipeIdxType>,
    ) -> impl Iterator<Item = (usize, Series)> {
        iter::once((
            0,
            (
                "Power Satisfaction",
                values
                    .windows(smoothing_window)
                    .map(|power_mult| {
                        power_mult.iter().sum::<u32>() as f32 / smoothing_window as f32
                    })
                    .collect::<Vec<_>>(),
            )
                .into(),
        ))
    }
}
