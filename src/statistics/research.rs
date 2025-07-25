use std::iter;

use crate::{item::IdxTrait, research::ResearchProgress};

use super::IntoSeries;

pub struct ResearchInfo {}

impl<ItemIdxType: IdxTrait, RecipeIdxType: IdxTrait> IntoSeries<(), ItemIdxType, RecipeIdxType>
    for ResearchProgress
{
    fn into_series(
        values: &[Self],
        filter: Option<impl Fn(()) -> bool>,
        _data_store: &crate::data::DataStore<ItemIdxType, RecipeIdxType>,
    ) -> impl Iterator<Item = (usize, charts_rs::Series)> {
        iter::once((
            0,
            (
                "Research",
                values
                    .iter()
                    .filter(|_| filter.as_ref().map(|f| f(())).unwrap_or(true))
                    .map(|v| *v as f32)
                    .collect(),
            )
                .into(),
        ))
    }
}
