use std::{ops::RangeInclusive, path::PathBuf};

pub mod bounding_box_grid;
// pub mod perfect_grid;
pub mod dynamic;
pub mod map_grid;

const FILE_CHUNK_SIZE: usize = 100_000;

pub trait SparseGrid<I, T: 'static> {
    fn new() -> Self;
    fn get_extent(&self) -> Option<[RangeInclusive<I>; 2]>;
    fn get_default(&mut self, x: I, y: I) -> &T
    where
        T: Default;

    fn get(&self, x: I, y: I) -> Option<&T>;
    fn get_mut(&mut self, x: I, y: I) -> Option<&mut T>;
    fn insert(&mut self, x: I, y: I, value: T) -> Option<T>;

    fn occupied_entries(&self) -> impl Iterator<Item = ((I, I), &T)>;
    fn occupied_entries_mut(&mut self) -> impl Iterator<Item = ((I, I), &mut T)>;

    fn save_single_thread(&self, base_path: PathBuf)
    where
        T: serde::Serialize,
        I: serde::Serialize;

    // TODO: Do I want to save None values?
    fn par_save(&self, base_path: PathBuf)
    where
        T: Send + Sync + serde::Serialize,
        I: Send + Sync + serde::Serialize;

    fn par_load(base_path: PathBuf) -> Self
    where
        for<'a> T: Send + serde::Deserialize<'a>,
        for<'a> I: Send + serde::Deserialize<'a>;

    fn insert_many(
        &mut self,
        positions: impl IntoIterator<Item = (I, I)> + Clone,
        values: impl IntoIterator<Item = T>,
    ) {
        for (v, pos) in values.into_iter().zip(positions) {
            self.insert(pos.0, pos.1, v);
        }
    }
}

pub trait GetGridIndex<I> {
    fn get_grid_index(&self) -> (I, I);
}
