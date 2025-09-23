#[cfg(feature = "client")]
use egui_show_info_derive::ShowInfo;
#[cfg(feature = "client")]
use get_size::GetSize;
use std::cmp::{max, min};
use std::collections::BTreeMap;
use std::hash::Hash;
use std::ops::RangeInclusive;

pub mod bounding_box_grid;
pub mod perfect_grid;

#[cfg_attr(feature = "client", derive(ShowInfo), derive(GetSize))]
#[derive(Debug, Clone, serde::Serialize, serde::Deserialize)]
pub struct SparseGrid<I: PartialEq + Eq + Copy + Ord + Hash, T> {
    extent: Option<[[I; 2]; 2]>,
    values: BTreeMap<(I, I), T>,
}

impl<I: PartialEq + Eq + Hash + Copy + Ord, T> SparseGrid<I, T> {
    pub fn new() -> Self
    where
        I: Default,
    {
        Self {
            extent: None,
            values: Default::default(),
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

    pub fn get_extent(&self) -> Option<[RangeInclusive<I>; 2]> {
        self.extent.map(|v| v.map(|[start, end]| start..=end))
    }

    pub fn get_default(&mut self, x: I, y: I) -> &T
    where
        T: Default,
    {
        self.include_in_extent(x, y);
        self.values.entry((x, y)).or_default()
    }

    pub fn get(&self, x: I, y: I) -> Option<&T> {
        if let Some(extent) = &self.extent {
            if x < extent[0][0] || x > extent[0][1] || y < extent[1][0] || y > extent[1][1] {
                return None;
            }
        }

        self.values.get(&(x, y))
    }

    pub fn get_mut(&mut self, x: I, y: I) -> Option<&mut T> {
        self.values.get_mut(&(x, y))
    }

    pub fn insert(&mut self, x: I, y: I, value: T) -> Option<T> {
        self.include_in_extent(x, y);
        self.values.insert((x, y), value)
    }

    pub fn insert_deduplicate(&mut self, x: I, y: I, value: T) -> Option<T>
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

    pub fn occupied_entries(&self) -> impl Iterator<Item = ((I, I), &T)> {
        self.values
            .iter()
            // .sorted_by_key(|(k, _)| **k)
            .map(|(a, b)| (*a, b))
    }

    pub fn occupied_entries_mut(&mut self) -> impl Iterator<Item = ((I, I), &mut T)> {
        self.values
            .iter_mut()
            // .sorted_by_key(|(k, _)| **k)
            .map(|(a, b)| (*a, b))
    }
}

pub trait GetGridIndex<I> {
    fn get_grid_index(&self) -> (I, I);
}
