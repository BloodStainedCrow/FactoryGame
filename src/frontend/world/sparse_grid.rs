#[cfg(feature = "client")]
use egui_show_info_derive::ShowInfo;
#[cfg(feature = "client")]
use get_size::GetSize;
use std::collections::BTreeMap;
use std::hash::Hash;

#[cfg_attr(feature = "client", derive(ShowInfo), derive(GetSize))]
#[derive(Debug, Clone, serde::Serialize, serde::Deserialize)]
pub struct SparseGrid<I: PartialEq + Eq + Copy + Ord, T> {
    values: BTreeMap<(I, I), T>,
}

impl<I: PartialEq + Eq + Hash + Copy + Ord, T> SparseGrid<I, T> {
    pub fn new() -> Self {
        Self {
            values: BTreeMap::new(),
        }
    }

    pub fn get_default(&mut self, x: I, y: I) -> &T
    where
        T: Default,
    {
        self.values.entry((x, y)).or_default()
    }

    pub fn get(&self, x: I, y: I) -> Option<&T> {
        self.values.get(&(x, y))
    }

    pub fn get_mut(&mut self, x: I, y: I) -> Option<&mut T> {
        self.values.get_mut(&(x, y))
    }

    pub fn insert(&mut self, x: I, y: I, value: T) -> Option<T> {
        self.values.insert((x, y), value)
    }

    pub fn insert_deduplicate(&mut self, x: I, y: I, value: T) -> Option<T>
    where
        T: PartialEq + Default,
    {
        if value == T::default() {
            self.values.remove(&(x, y))
        } else {
            self.values.insert((x, y), value)
        }
    }

    pub fn occupied_entries(&self) -> impl Iterator<Item = ((I, I), &T)> {
        self.values.iter().map(|(a, b)| (*a, b))
    }

    pub fn occupied_entries_mut(&mut self) -> impl Iterator<Item = ((I, I), &mut T)> {
        self.values.iter_mut().map(|(a, b)| (*a, b))
    }
}
