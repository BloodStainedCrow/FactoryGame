use std::iter;

use crate::{research::ResearchProgress, NewWithDataStore};

use super::IntoSeries;

pub struct ResearchInfo {}

impl NewWithDataStore for ResearchProgress {
    fn new<ItemIdxType: crate::item::IdxTrait, RecipeIdxType: crate::item::IdxTrait>(
        _data_store: impl std::borrow::Borrow<crate::data::DataStore<ItemIdxType, RecipeIdxType>>,
    ) -> Self {
        Self::default()
    }
}

impl<ItemIdxType: crate::item::IdxTrait, RecipeIdxType: crate::item::IdxTrait>
    IntoSeries<(), ItemIdxType, RecipeIdxType> for ResearchProgress
{
    fn into_series(
        values: &[Self],
        filter: Option<impl Fn(()) -> bool>,
        _data_store: &crate::data::DataStore<ItemIdxType, RecipeIdxType>,
    ) -> impl IntoIterator<Item = charts_rs::Series> {
        iter::once(
            (
                "Research",
                values
                    .iter()
                    .filter(|_| filter.as_ref().map(|f| f(())).unwrap_or(true))
                    .map(|v| *v as f32)
                    .collect(),
            )
                .into(),
        )
    }
}
