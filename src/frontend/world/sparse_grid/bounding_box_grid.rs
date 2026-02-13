use crate::frontend::world::sparse_grid::SparseGrid;
use crate::saving::{save_at, save_at_fork};

use super::GetGridIndex;
use itertools::Itertools;
use rayon::iter::{IndexedParallelIterator, IntoParallelIterator, ParallelIterator};
use rayon::slice::ParallelSlice;
use std::cmp::{max, min};
use std::fmt::Debug;
use std::fs::create_dir_all;
use std::mem;
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
impl<T: GetGridIndex<I> + 'static> SparseGrid<I, T> for BoundingBoxGrid<I, T> {
    fn new() -> Self {
        Self {
            extent: None,
            values: vec![],
        }
    }

    fn get_extent(&self) -> Option<[RangeInclusive<I>; 2]> {
        self.extent.map(|v| v.map(|[start, end]| start..=end))
    }

    fn get_default(&mut self, x: I, y: I) -> &T
    where
        T: Default,
    {
        if self.get(x, y).is_none() {
            let old = self.insert(x, y, T::default());
            assert!(old.is_none());
        }
        self.get(x, y).unwrap()
    }

    fn insert(&mut self, x: I, y: I, value: T) -> Option<T> {
        self.include_in_extent(x, y);

        let index = Self::calculate_index(self.extent.as_ref().unwrap(), [x, y]);
        debug_assert!(self.values[index].is_none());
        let mut old = Some(value);
        mem::swap(&mut old, &mut self.values[index]);
        old
    }

    fn get(&self, x: I, y: I) -> Option<&T> {
        if let Some(extent) = &self.extent {
            if x < extent[0][0] || x > extent[0][1] || y < extent[1][0] || y > extent[1][1] {
                return None;
            }
        }

        let index = Self::calculate_index(self.extent.as_ref().unwrap(), [x, y]);

        self.values[index].as_ref()
    }

    fn get_mut(&mut self, x: I, y: I) -> Option<&mut T> {
        if let Some(extent) = &self.extent {
            if x < extent[0][0] || x > extent[0][1] || y < extent[1][0] || y > extent[1][1] {
                return None;
            }
        }

        let index = Self::calculate_index(self.extent.as_ref().unwrap(), [x, y]);

        self.values[index].as_mut()
    }

    fn occupied_entries(&self) -> impl Iterator<Item = ((I, I), &T)> {
        self.values.iter().enumerate().filter_map(|(idx, v)| {
            let [x, y] = Self::calculate_pos(&self.extent.unwrap(), idx);
            v.as_ref().map(|v| ((x, y), v))
        })
    }

    fn occupied_entries_mut(&mut self) -> impl Iterator<Item = ((I, I), &mut T)> {
        self.values.iter_mut().enumerate().filter_map(|(idx, v)| {
            let [x, y] = Self::calculate_pos(&self.extent.unwrap(), idx);
            v.as_mut().map(|v| ((x, y), v))
        })
    }

    // TODO: Do I want to save None values?
    fn save_single_thread(&self, base_path: PathBuf)
    where
        T: serde::Serialize,
    {
        create_dir_all(&base_path).expect("Failed to create chunk dir");

        save_at_fork(&self.extent, base_path.join("extent"));

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
    fn par_save(&self, base_path: PathBuf)
    where
        T: Sync + serde::Serialize,
    {
        create_dir_all(&base_path).expect("Failed to create chunk dir");

        save_at(&self.extent, base_path.join("extent"));

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

    fn par_load(base_path: PathBuf) -> Self
    where
        for<'a> T: Send + serde::Deserialize<'a>,
    {
        let extent = load_at(base_path.join("extent"));

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

        // TODO: This assertion requires storing the pos
        // for (index, chunk) in values
        //     .iter()
        //     .enumerate()
        //     .filter_map(|(index, chunk)| chunk.as_ref().map(|chunk| (index, chunk)))
        // {
        //     assert_eq!(
        //         Self::calculate_index(&extent.unwrap(), chunk.get_grid_index().into()),
        //         index
        //     );
        // }

        Self { extent, values }
    }

    fn insert_many(
        &mut self,
        positions: impl IntoIterator<Item = (I, I)> + Clone,
        values: impl IntoIterator<Item = T>,
    ) {
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

        for (value, (x, y)) in values.into_iter().zip(positions) {
            let index = Self::calculate_index(self.extent.as_ref().unwrap(), [x, y]);
            debug_assert!(self.values[index].is_none());
            self.values[index] = Some(value);
        }
    }
}

impl<T: GetGridIndex<I>> BoundingBoxGrid<I, T> {
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

    fn calculate_pos(extent: &[[I; 2]; 2], idx: usize) -> [I; 2] {
        let width = extent[0][1] - extent[0][0];
        let x_offs = idx as i32 % width;
        let y_offs = idx as i32 / width;

        let x = extent[0][0] + x_offs;
        let y = extent[1][0] + y_offs;

        [x, y]
    }

    fn reorder_for_new_extent(&mut self, new_point: [I; 2]) {
        let [x, y] = new_point;

        let old_extent = self.extent.unwrap_or([[x, x], [y, y]]);

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

        for (idx, old_val) in values.into_iter().enumerate() {
            let pos = Self::calculate_pos(&old_extent, idx);
            let index = Self::calculate_index(extent, pos.into());
            self.values[index] = Some(old_val);
        }
    }

    pub(super) fn get_extent_after_insertion(
        &self,
        positions: impl IntoIterator<Item = [I; 2]>,
    ) -> [RangeInclusive<I>; 2] {
        let mut positions = positions.into_iter();
        let [x, y] = positions.next().unwrap();
        let extent = self.extent.unwrap_or([[x, x], [y, y]]);
        let [mut x_range, mut y_range] = extent;

        for [x, y] in positions {
            x_range[0] = min(x_range[0], x);
            x_range[1] = max(x_range[1], x);

            y_range[0] = min(y_range[0], y);
            y_range[1] = max(y_range[1], y);
        }

        [x_range[0]..=x_range[1], y_range[0]..=y_range[1]]
    }

    pub(super) fn into_iter(self) -> impl Iterator<Item = ((I, I), T)> {
        self.values
            .into_iter()
            .enumerate()
            .filter_map(|(idx, v)| v.map(|v| (idx, v)))
            .map(move |(idx, chunk)| {
                (
                    Self::calculate_pos(
                        &self
                            .extent
                            .expect("Since we have any chunks, we must have an extent"),
                        idx,
                    )
                    .into(),
                    chunk,
                )
            })
    }
}
