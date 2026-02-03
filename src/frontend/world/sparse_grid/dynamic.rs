use std::{fs::create_dir_all, iter};

use itertools::Either;

use crate::{
    frontend::world::sparse_grid::{
        GetGridIndex, SparseGrid, bounding_box_grid::BoundingBoxGrid, map_grid::BtreeMapGrid,
    },
    saving::{load_at, save_at, save_at_fork},
};

#[cfg(feature = "client")]
use egui_show_info_derive::ShowInfo;
#[cfg(feature = "client")]
use get_size2::GetSize;

// If more than every 20th chunk is inhabited, switch to map
// TODO: Find a good value for this
const SWITCH_RATIO: usize = 20;

#[cfg_attr(feature = "client", derive(ShowInfo), derive(GetSize))]
#[derive(Debug, Clone, serde::Deserialize, serde::Serialize)]
pub(crate) struct DynamicGrid<I: Ord, T> {
    pub(crate) num_chunks: usize,
    store: Backing<I, T>,
}

#[cfg_attr(feature = "client", derive(ShowInfo), derive(GetSize))]
#[derive(Debug, Clone, serde::Deserialize, serde::Serialize)]
enum Backing<I: Ord, T> {
    BoundingBoxGrid(BoundingBoxGrid<I, T>),
    Map(BtreeMapGrid<I, T>),
}

type I = i32;
impl<T: GetGridIndex<I> + 'static> SparseGrid<I, T> for DynamicGrid<I, T> {
    fn new() -> Self {
        Self {
            num_chunks: 0,
            store: Backing::BoundingBoxGrid(BoundingBoxGrid::new()),
        }
    }

    fn get_extent(&self) -> Option<[std::ops::RangeInclusive<I>; 2]> {
        match &self.store {
            Backing::BoundingBoxGrid(grid) => grid.get_extent(),
            Backing::Map(grid) => grid.get_extent(),
        }
    }

    fn get_default(&mut self, _x: I, _y: I) -> &T
    where
        T: Default,
    {
        unimplemented!()
    }

    fn get(&self, x: I, y: I) -> Option<&T> {
        match &self.store {
            Backing::BoundingBoxGrid(grid) => grid.get(x, y),
            Backing::Map(grid) => grid.get(x, y),
        }
    }

    fn get_mut(&mut self, x: I, y: I) -> Option<&mut T> {
        match &mut self.store {
            Backing::BoundingBoxGrid(grid) => grid.get_mut(x, y),
            Backing::Map(grid) => grid.get_mut(x, y),
        }
    }

    fn insert(&mut self, x: I, y: I, value: T) -> Option<T> {
        self.num_chunks += 1;
        match &mut self.store {
            Backing::BoundingBoxGrid(grid) => {
                let [x_ext, y_ext] = grid.get_extent_after_insertion(iter::once([x, y]));

                let new_size = x_ext.count() * y_ext.count();

                if self.num_chunks * SWITCH_RATIO < new_size {
                    log::error!("SWITCHING TO SLOW WORLD REPRESENTATION TO SAVE RAM!");
                    let mut ret = None;
                    take_mut::take(&mut self.store, |store| {
                        let Backing::BoundingBoxGrid(grid) = store else {
                            unreachable!()
                        };
                        let mut new_grid = BtreeMapGrid::new_with_values(
                            grid.get_extent().unwrap(),
                            grid.into_iter(),
                        );
                        ret = Some(new_grid.insert(x, y, value));
                        Backing::Map(new_grid)
                    });

                    ret.unwrap()
                } else {
                    grid.insert(x, y, value)
                }
            },
            Backing::Map(grid) => grid.insert(x, y, value),
        }
    }

    fn insert_many(
        &mut self,
        positions: impl IntoIterator<Item = (I, I)> + Clone,
        values: impl IntoIterator<Item = T>,
    ) {
        self.num_chunks += positions.clone().into_iter().count();
        match &mut self.store {
            Backing::BoundingBoxGrid(grid) => {
                let [x_ext, y_ext] = grid
                    .get_extent_after_insertion(positions.clone().into_iter().map(|v| v.into()));

                let new_size = x_ext.count() * y_ext.count();

                if self.num_chunks * SWITCH_RATIO < new_size {
                    log::error!("SWITCHING TO SLOW WORLD REPRESENTATION TO SAVE RAM!");
                    take_mut::take(&mut self.store, |store| {
                        let Backing::BoundingBoxGrid(grid) = store else {
                            unreachable!()
                        };
                        let mut new_grid = BtreeMapGrid::new_with_values(
                            grid.get_extent().unwrap(),
                            grid.into_iter(),
                        );
                        new_grid.insert_many(positions, values);
                        Backing::Map(new_grid)
                    });
                } else {
                    grid.insert_many(positions, values)
                }
            },
            Backing::Map(grid) => grid.insert_many(positions, values),
        }
    }

    fn occupied_entries(&self) -> impl Iterator<Item = ((I, I), &T)> {
        match &self.store {
            Backing::BoundingBoxGrid(grid) => Either::Left(grid.occupied_entries()),
            Backing::Map(grid) => Either::Right(grid.occupied_entries()),
        }
    }

    fn occupied_entries_mut(&mut self) -> impl Iterator<Item = ((I, I), &mut T)> {
        match &mut self.store {
            Backing::BoundingBoxGrid(grid) => Either::Left(grid.occupied_entries_mut()),
            Backing::Map(grid) => Either::Right(grid.occupied_entries_mut()),
        }
    }

    fn save_single_thread(&self, base_path: std::path::PathBuf)
    where
        T: serde::Serialize,
        I: serde::Serialize,
    {
        create_dir_all(&base_path).expect("Failed to create world dir");

        let is_map = match &self.store {
            Backing::BoundingBoxGrid(_) => false,
            Backing::Map(_) => true,
        };

        save_at_fork(&is_map, base_path.join("is_map"));

        match &self.store {
            Backing::BoundingBoxGrid(grid) => {
                grid.save_single_thread(base_path);
            },
            Backing::Map(grid) => {
                grid.save_single_thread(base_path);
            },
        }
    }

    fn par_save(&self, base_path: std::path::PathBuf)
    where
        T: Send + Sync + serde::Serialize,
        I: Send + Sync + serde::Serialize,
    {
        create_dir_all(&base_path).expect("Failed to create world dir");

        let is_map = match &self.store {
            Backing::BoundingBoxGrid(_) => false,
            Backing::Map(_) => true,
        };

        save_at(&is_map, base_path.join("is_map"));

        match &self.store {
            Backing::BoundingBoxGrid(grid) => {
                grid.par_save(base_path);
            },
            Backing::Map(grid) => {
                grid.par_save(base_path);
            },
        }
    }

    fn par_load(base_path: std::path::PathBuf) -> Self
    where
        for<'a> T: Send + serde::Deserialize<'a>,
        for<'a> I: Send + serde::Deserialize<'a>,
    {
        let is_map = load_at::<bool>(base_path.join("is_map"));

        let store = match is_map {
            true => Backing::Map(BtreeMapGrid::par_load(base_path)),
            false => Backing::BoundingBoxGrid(BoundingBoxGrid::par_load(base_path)),
        };

        let count = match &store {
            Backing::BoundingBoxGrid(grid) => grid.occupied_entries().count(),
            Backing::Map(grid) => grid.occupied_entries().count(),
        };

        Self {
            num_chunks: count,
            store,
        }
    }
}

impl<T: GetGridIndex<I> + 'static> DynamicGrid<I, T> {
    pub fn new_with_filled_grid<F>(top_left: [I; 2], bottom_right: [I; 2], generation_fn: F) -> Self
    where
        F: Fn([I; 2]) -> T + Sync,
        T: Send,
    {
        let store = BoundingBoxGrid::new_with_filled_grid(top_left, bottom_right, generation_fn);
        Self {
            num_chunks: (store.occupied_entries().count()),
            store: Backing::BoundingBoxGrid(store),
        }
    }
}
