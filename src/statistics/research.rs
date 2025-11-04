use std::iter;

use crate::{item::IdxTrait, research::ResearchProgress};

use super::IntoSeries;

pub struct ResearchInfo {}

impl<ItemIdxType: IdxTrait, RecipeIdxType: IdxTrait> IntoSeries<(), ItemIdxType, RecipeIdxType>
    for u64
{
    fn into_series(
        values: &[Self],
        smoothing_window: usize,
        filter: Option<impl Fn(()) -> bool>,
        _data_store: &crate::data::DataStore<ItemIdxType, RecipeIdxType>,
    ) -> impl Iterator<Item = (usize, charts_rs::Series)> {
        iter::once((
            0,
            (
                "Research",
                values
                    .windows(smoothing_window)
                    .filter(|_| filter.as_ref().map(|f| f(())).unwrap_or(true))
                    .map(|v| {
                        v.iter().copied().map(|v| v as u32).sum::<u32>() as f32
                            / smoothing_window as f32
                    })
                    .collect(),
            )
                .into(),
        ))
    }
}
