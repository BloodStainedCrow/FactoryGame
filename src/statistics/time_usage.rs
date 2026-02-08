use std::{
    collections::BTreeMap,
    iter,
    ops::{Add, AddAssign},
    time::Duration,
};

use crate::{item::IdxTrait, statistics::Series};

use super::IntoSeries;

#[cfg(feature = "client")]
use egui_show_info_derive::ShowInfo;
#[cfg(feature = "client")]
use get_size2::GetSize;
use itertools::Itertools;

#[cfg_attr(feature = "client", derive(ShowInfo), derive(GetSize))]
#[derive(Debug, Clone, Default, serde::Deserialize, serde::Serialize)]
pub struct TimeUsageInfo {
    full_update_time: Duration,
}

impl Add<&TimeUsageInfo> for TimeUsageInfo {
    type Output = TimeUsageInfo;

    fn add(mut self, rhs: &TimeUsageInfo) -> Self::Output {
        self.full_update_time += rhs.full_update_time;

        self
    }
}

impl AddAssign<&TimeUsageInfo> for TimeUsageInfo {
    fn add_assign(&mut self, rhs: &TimeUsageInfo) {
        self.full_update_time += rhs.full_update_time;
    }
}

impl<ItemIdxType: IdxTrait, RecipeIdxType: IdxTrait> IntoSeries<(), ItemIdxType, RecipeIdxType>
    for TimeUsageInfo
{
    fn into_series(
        values: &[Self],
        smoothing_window: usize,
        _filter: Option<impl Fn(()) -> bool>,
        _data_store: &crate::data::DataStore<ItemIdxType, RecipeIdxType>,
    ) -> impl Iterator<Item = (usize, Series)> {
        BTreeMap::from_iter(
            values
                .windows(smoothing_window)
                .map(|infos| {
                    let TimeUsageInfo { full_update_time } =
                        infos.iter().fold(TimeUsageInfo::default(), |a, b| a + b);

                    iter::once((
                        (0, "Full Update Time"),
                        full_update_time.as_secs_f32() * 1000.0,
                    ))
                })
                .flatten()
                .into_group_map()
                .into_iter()
                .map(|(k, v)| (k.0, (k.1, v))),
        )
        .into_iter()
        .map(move |(time_id, a)| {
            (
                time_id,
                (
                    a.0,
                    a.1.into_iter()
                        .map(|v| v as f32 / smoothing_window as f32)
                        .collect(),
                )
                    .into(),
            )
        })
    }
}
