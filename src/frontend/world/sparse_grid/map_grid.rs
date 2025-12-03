#[cfg(feature = "client")]
use egui_show_info_derive::ShowInfo;
#[cfg(feature = "client")]
use get_size2::GetSize;
use itertools::Itertools;
use rayon::iter::IntoParallelIterator;
use rayon::iter::ParallelIterator;
use std::cmp::{max, min};
use std::collections::BTreeMap;
use std::fs::File;
use std::fs::create_dir_all;
use std::hash::Hash;
use std::ops::RangeInclusive;

use crate::frontend::world::sparse_grid::{FILE_CHUNK_SIZE, SparseGrid};
use crate::saving::load_at;
use crate::saving::save_at_fork;

#[cfg_attr(feature = "client", derive(ShowInfo), derive(GetSize))]
#[derive(Debug, Clone, serde::Serialize, serde::Deserialize)]
pub struct BtreeMapGrid<I: Ord, T> {
    extent: Option<[[I; 2]; 2]>,
    values: BTreeMap<(I, I), T>,
}

impl<I: PartialEq + Eq + Hash + Copy + Ord, T: 'static> SparseGrid<I, T> for BtreeMapGrid<I, T> {
    fn new() -> Self {
        Self {
            extent: None,
            values: Default::default(),
        }
    }

    fn get_extent(&self) -> Option<[RangeInclusive<I>; 2]> {
        self.extent.map(|v| v.map(|[start, end]| start..=end))
    }

    fn get_default(&mut self, x: I, y: I) -> &T
    where
        T: Default,
    {
        self.include_in_extent(x, y);
        self.values.entry((x, y)).or_default()
    }

    fn get(&self, x: I, y: I) -> Option<&T> {
        // if let Some(extent) = &self.extent {
        //     if x < extent[0][0] || x > extent[0][1] || y < extent[1][0] || y > extent[1][1] {
        //         return None;
        //     }
        // }

        self.values.get(&(x, y))
    }

    fn get_mut(&mut self, x: I, y: I) -> Option<&mut T> {
        self.values.get_mut(&(x, y))
    }

    fn insert(&mut self, x: I, y: I, value: T) -> Option<T> {
        self.include_in_extent(x, y);
        self.values.insert((x, y), value)
    }

    fn insert_deduplicate(&mut self, x: I, y: I, value: T) -> Option<T>
    where
        T: PartialEq + Default,
    {
        self.include_in_extent(x, y);
        if value == T::default() {
            self.values.remove(&(x, y))
        } else {
            self.values.insert((x, y), value)
        }
    }

    fn occupied_entries(&self) -> impl Iterator<Item = ((I, I), &T)> {
        self.values
            .iter()
            // .sorted_by_key(|(k, _)| **k)
            .map(|(a, b)| (*a, b))
    }

    fn occupied_entries_mut(&mut self) -> impl Iterator<Item = ((I, I), &mut T)> {
        self.values
            .iter_mut()
            // .sorted_by_key(|(k, _)| **k)
            .map(|(a, b)| (*a, b))
    }

    fn save_single_thread(&self, base_path: std::path::PathBuf)
    where
        T: serde::Serialize,
        I: serde::Serialize,
    {
        create_dir_all(&base_path).expect("Failed to create world dir");

        self.values
            .iter()
            .chunks(FILE_CHUNK_SIZE)
            .into_iter()
            .enumerate()
            .for_each(|(i, chunks)| {
                save_at_fork(&chunks.collect_vec(), base_path.join(format!("chunk-{i}")));
            });
    }

    fn par_save(&self, base_path: std::path::PathBuf)
    where
        T: Send + Sync + serde::Serialize,
        I: Send + Sync + serde::Serialize,
    {
        // FIXME: This is single threaded
        self.values
            .iter()
            .chunks(FILE_CHUNK_SIZE)
            .into_iter()
            .enumerate()
            .for_each(|(i, chunks)| {
                save_at_fork(&chunks.collect_vec(), base_path.join(format!("chunk-{i}")));
            });
    }

    fn par_load(base_path: std::path::PathBuf) -> Self
    where
        for<'a> T: Send + serde::Deserialize<'a>,
        for<'a> I: Send + serde::Deserialize<'a>,
    {
        let values: BTreeMap<_, _> = (0..)
            .map(|chunk_id| base_path.join(format!("chunk-{chunk_id}")))
            .take_while(|path| {
                // FIXME: Use another function
                File::open(path).is_ok()
            })
            .collect_vec()
            .into_par_iter()
            .map(|file_path| load_at::<Vec<((I, I), T)>>(file_path))
            .flatten()
            .collect();

        let top_left = values
            .iter()
            .map(|value| *value.0)
            .reduce(|a, b| (min(a.0, b.0), min(a.1, b.1)));

        let bottom_right = values
            .iter()
            .map(|value| *value.0)
            .reduce(|a, b| (max(a.0, b.0), max(a.1, b.1)));

        let extent = match (top_left, bottom_right) {
            (None, None) => None,
            (Some((min_x, min_y)), Some((max_x, max_y))) => Some([[min_x, max_x], [min_y, max_y]]),

            _ => unreachable!(),
        };

        Self { extent, values }
    }
}

impl<I: PartialEq + Eq + Hash + Copy + Ord, T> BtreeMapGrid<I, T> {
    pub(super) fn new_with_values(
        extent: [RangeInclusive<I>; 2],
        values: impl IntoIterator<Item = ((I, I), T)>,
    ) -> Self {
        Self {
            extent: Some(extent.map(|extent| [*extent.start(), *extent.end()])),
            values: BTreeMap::from_iter(values),
        }
    }

    fn include_in_extent(&mut self, x: I, y: I) {
        if let Some(extent) = &mut self.extent {
            let [x_range, y_range] = extent;

            x_range[0] = min(x_range[0], x);
            x_range[1] = max(x_range[1], x);

            y_range[0] = min(y_range[0], y);
            y_range[1] = max(y_range[1], y);
        } else {
            self.extent = Some([[x, x], [y, y]]);
        }
    }
}
