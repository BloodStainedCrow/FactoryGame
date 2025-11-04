use crate::saving::{save_at, save_at_fork};

use super::GetGridIndex;
use itertools::Itertools;
use rayon::iter::{IndexedParallelIterator, IntoParallelIterator, ParallelIterator};
use rayon::slice::ParallelSlice;
use std::cmp::{max, min};
use std::fmt::Debug;
use std::fs::create_dir_all;
use std::ops::RangeInclusive;
use std::path::PathBuf;

use crate::saving::load_at;
use std::fs::File;

#[cfg(feature = "client")]
use egui_show_info_derive::ShowInfo;
#[cfg(feature = "client")]
use get_size2::GetSize;

#[cfg_attr(feature = "client", derive(ShowInfo), derive(GetSize))]
#[derive(Debug, Clone, serde::Serialize, serde::Deserialize)]
pub struct BoundingBoxGrid<I, T> {
    extent: Option<[[I; 2]; 2]>,
    values: Vec<Option<T>>,
}

// FIXME: currently only for i32 to allow getting the number of slots
type I = i32;
impl<T: GetGridIndex<I>> BoundingBoxGrid<I, T> {
    pub fn new() -> Self {
        Self {
            extent: None,
            values: vec![],
        }
    }

    pub fn new_with_filled_grid<F>(top_left: [I; 2], bottom_right: [I; 2], generation_fn: F) -> Self
    where
        F: Fn([I; 2]) -> T + Sync,
        T: Send,
    {
        let extent = Some([
            [top_left[0], bottom_right[0]],
            [top_left[1], bottom_right[1]],
        ]);

        let width = usize::try_from(bottom_right[0] - top_left[0])
            .expect("Check bounding box argument order")
            + 1;

        let height = usize::try_from(bottom_right[1] - top_left[1])
            .expect("Check bounding box argument order")
            + 1;

        let mut values = Vec::with_capacity(width * height);

        (0..(width * height))
            .into_par_iter()
            .map(|v| (v % width, v / width))
            .map(|(x_offs, y_offs)| {
                (
                    top_left[0]
                        .checked_add_unsigned(x_offs.try_into().unwrap())
                        .unwrap(),
                    top_left[1]
                        .checked_add_unsigned(y_offs.try_into().unwrap())
                        .unwrap(),
                )
            })
            .map(|(x, y)| Some(generation_fn([x, y])))
            .collect_into_vec(&mut values);

        Self { extent, values }
    }

    // TODO: Do I want to save None values?
    pub fn save_fork(&self, base_path: PathBuf)
    where
        T: Sync + serde::Serialize,
    {
        create_dir_all(&base_path).expect("Failed to create world dir");

        // TODO: Choose a chunk size
        self.values
            .chunks(100_000)
            .enumerate()
            .for_each(|(i, chunks)| {
                save_at_fork(chunks, base_path.join(format!("chunk-{i}")));
            });

        // TODO: Serialize the rest
        // todo!("Serialize the rest")
    }

    // TODO: Do I want to save None values?
    pub fn par_save(&self, base_path: PathBuf)
    where
        T: Sync + serde::Serialize,
    {
        create_dir_all(&base_path).expect("Failed to create world dir");

        // TODO: Choose a chunk size
        self.values
            .par_chunks(100_000)
            .enumerate()
            .for_each(|(i, chunks)| {
                save_at(chunks, base_path.join(format!("chunk-{i}")));
            });

        // TODO: Serialize the rest
        // todo!("Serialize the rest")
    }

    pub fn par_load(base_path: PathBuf) -> Self
    where
        for<'a> T: Send + serde::Deserialize<'a>,
    {
        let values: Vec<_> = (0..)
            .map(|chunk_id| base_path.join(format!("chunk-{chunk_id}")))
            .take_while(|path| {
                // FIXME: Use another function
                File::open(path).is_ok()
            })
            .collect_vec()
            .into_par_iter()
            .map(|file_path| load_at::<Vec<Option<T>>>(file_path))
            .flatten()
            .collect();

        let top_left = values
            .iter()
            .flatten()
            .map(|value| value.get_grid_index())
            .reduce(|a, b| (min(a.0, b.0), min(a.1, b.1)));

        let bottom_right = values
            .iter()
            .flatten()
            .map(|value| value.get_grid_index())
            .reduce(|a, b| (max(a.0, b.0), max(a.1, b.1)));

        let extent = match (top_left, bottom_right) {
            (None, None) => None,
            (Some((min_x, min_y)), Some((max_x, max_y))) => Some([[min_x, max_x], [min_y, max_y]]),

            _ => unreachable!(),
        };

        for (index, chunk) in values
            .iter()
            .enumerate()
            .filter_map(|(index, chunk)| chunk.as_ref().map(|chunk| (index, chunk)))
        {
            assert_eq!(
                Self::calculate_index(&extent.unwrap(), chunk.get_grid_index().into()),
                index
            );
        }

        Self { extent, values }
    }

    fn include_in_extent(&mut self, x: I, y: I) {
        if let Some(extent) = &mut self.extent {
            let [x_range, y_range] = extent;

            if (x_range[0]..=x_range[1]).contains(&x) && (y_range[0]..=y_range[1]).contains(&y) {
                return;
            }

            self.reorder_for_new_extent([x, y]);
        } else {
            self.reorder_for_new_extent([x, y]);
            self.extent = Some([[x, x], [y, y]]);
        }
    }

    fn calculate_index(extent: &[[I; 2]; 2], point: [I; 2]) -> usize {
        let width = extent[0][1] - extent[0][0] + 1;
        assert!(width > 0);

        let width_offs = point[0] - extent[0][0];
        assert!(width_offs >= 0);

        let height_offs = point[1] - extent[1][0];
        assert!(height_offs >= 0);

        height_offs as usize * width as usize + width_offs as usize
    }

    fn reorder_for_new_extent(&mut self, new_point: [I; 2]) {
        let [x, y] = new_point;

        let extent = self.extent.get_or_insert([[x, x], [y, y]]);
        let [x_range, y_range] = extent;

        x_range[0] = min(x_range[0], x);
        x_range[1] = max(x_range[1], x);

        y_range[0] = min(y_range[0], y);
        y_range[1] = max(y_range[1], y);

        let values: Vec<_> = self.values.drain(..).filter_map(|v| v).collect();

        let new_size =
            (extent[0][0]..=extent[0][1]).count() * (extent[1][0]..=extent[1][1]).count();

        self.values.resize_with(new_size, || None);

        for old_val in values {
            let pos = old_val.get_grid_index();
            let index = Self::calculate_index(extent, pos.into());
            self.values[index] = Some(old_val);
        }
    }

    pub fn get_extent(&self) -> Option<[RangeInclusive<I>; 2]> {
        self.extent.map(|v| v.map(|[start, end]| start..=end))
    }

    pub fn insert(&mut self, x: I, y: I, value: T) {
        self.include_in_extent(x, y);

        let index = Self::calculate_index(self.extent.as_ref().unwrap(), [x, y]);
        debug_assert!(self.values[index].is_none());
        self.values[index] = Some(value);
    }

    pub fn insert_many(
        &mut self,
        positions: impl IntoIterator<Item = (I, I)> + Clone,
        values: impl IntoIterator<Item = T> + Clone,
    ) where
        T: PartialEq + Debug,
    {
        let x_min = positions
            .clone()
            .into_iter()
            .map(|(x, _y)| x)
            .min()
            .unwrap();
        let x_max = positions
            .clone()
            .into_iter()
            .map(|(x, _y)| x)
            .max()
            .unwrap();

        let y_min = positions
            .clone()
            .into_iter()
            .map(|(_x, y)| y)
            .min()
            .unwrap();
        let y_max = positions
            .clone()
            .into_iter()
            .map(|(_x, y)| y)
            .max()
            .unwrap();

        self.include_in_extent(x_min, y_min);
        self.include_in_extent(x_max, y_max);

        #[cfg(debug_assertions)]
        {
            assert!(
                positions
                    .clone()
                    .into_iter()
                    .zip(values.clone())
                    .all(|(pos, v)| pos == v.get_grid_index())
            );
        }

        for (value, (x, y)) in values.into_iter().zip(positions) {
            let index = Self::calculate_index(self.extent.as_ref().unwrap(), [x, y]);
            debug_assert!(self.values[index].is_none());
            self.values[index] = Some(value);
        }
    }

    pub fn get(&self, x: I, y: I) -> Option<&T> {
        if let Some(extent) = &self.extent {
            if x < extent[0][0] || x > extent[0][1] || y < extent[1][0] || y > extent[1][1] {
                return None;
            }
        }

        let index = Self::calculate_index(self.extent.as_ref().unwrap(), [x, y]);

        self.values[index].as_ref()
    }

    pub fn get_mut(&mut self, x: I, y: I) -> Option<&mut T> {
        if let Some(extent) = &self.extent {
            if x < extent[0][0] || x > extent[0][1] || y < extent[1][0] || y > extent[1][1] {
                return None;
            }
        }

        let index = Self::calculate_index(self.extent.as_ref().unwrap(), [x, y]);

        self.values[index].as_mut()
    }

    pub fn occupied_entries(&self) -> impl Iterator<Item = ((I, I), &T)> {
        self.values
            .iter()
            .filter_map(|v| v.as_ref().map(|v| (v.get_grid_index(), v)))
    }

    pub fn occupied_entries_mut(&mut self) -> impl Iterator<Item = ((I, I), &mut T)> {
        self.values
            .iter_mut()
            .filter_map(|v| v.as_mut().map(|v| (v.get_grid_index(), v)))
    }
}
