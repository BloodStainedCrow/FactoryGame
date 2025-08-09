use super::GetGridIndex;
use ph::fmph::Bits8;
use std::cmp::{max, min};
use std::fmt::Debug;
use std::hash::Hash;
use std::ops::RangeInclusive;

#[cfg(feature = "client")]
use egui_show_info::{EguiDisplayable, InfoExtractor, ShowInfo};
#[cfg(feature = "client")]
use get_size::GetSize;

pub struct PerfectGrid<I, T> {
    extent: Option<[[I; 2]; 2]>,
    indices: Vec<(I, I)>,
    values: Vec<T>,
    function: ph::phast::Function<Bits8>,
}

#[derive(serde::Serialize)]
struct SerPerfectGrid<'a, I, T> {
    extent: &'a Option<[[I; 2]; 2]>,
    indices: &'a Vec<(I, I)>,
    values: &'a Vec<T>,
}

#[derive(serde::Deserialize)]
struct DePerfectGrid<I, T> {
    extent: Option<[[I; 2]; 2]>,
    indices: Vec<(I, I)>,
    values: Vec<T>,
}

impl<I: serde::Serialize, T: serde::Serialize> serde::Serialize for PerfectGrid<I, T> {
    fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
    where
        S: serde::Serializer,
    {
        SerPerfectGrid::from(self).serialize(serializer)
    }
}

impl<
    'de,
    I: serde::Deserialize<'de> + Hash + Eq + Clone + Send + Sync,
    T: serde::Deserialize<'de> + GetGridIndex<I>,
> serde::Deserialize<'de> for PerfectGrid<I, T>
{
    fn deserialize<D>(deserializer: D) -> Result<Self, D::Error>
    where
        D: serde::Deserializer<'de>,
    {
        DePerfectGrid::deserialize(deserializer).map(|de| de.into())
    }
}

impl<I: Hash + Eq + Clone + Send + Sync, T: GetGridIndex<I>> From<DePerfectGrid<I, T>>
    for PerfectGrid<I, T>
{
    fn from(value: DePerfectGrid<I, T>) -> Self {
        let DePerfectGrid {
            extent,
            mut values,
            mut indices,
        } = value;

        let function = ph::phast::Function::from_slice_mt::<(I, I)>(&indices);
        indices.sort_unstable_by_key(|v| function.get(v));
        values.sort_unstable_by_key(|v| function.get(&v.get_grid_index()));

        Self {
            extent,
            indices,
            values,
            function,
        }
    }
}

impl<'a, I, T> From<&'a PerfectGrid<I, T>> for SerPerfectGrid<'a, I, T> {
    fn from(value: &'a PerfectGrid<I, T>) -> Self {
        let PerfectGrid {
            extent,
            values,
            indices,
            function: _,
        } = value;

        Self {
            extent,
            indices,
            values,
        }
    }
}

impl<I: Debug, T: Debug> Debug for PerfectGrid<I, T> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        todo!()
    }
}

impl<I: Clone + Hash + Eq + Send + Sync, T: Clone> Clone for PerfectGrid<I, T> {
    fn clone(&self) -> Self {
        Self {
            extent: self.extent.clone(),
            indices: self.indices.clone(),
            values: self.values.clone(),
            function: ph::phast::Function::from_slice_mt::<(I, I)>(&self.indices),
        }
    }
}

#[cfg(feature = "client")]
impl<I: GetSize, T: GetSize> GetSize for PerfectGrid<I, T> {
    fn get_heap_size(&self) -> usize {
        <ph::phast::Function<Bits8> as ph::GetSize>::size_bytes(&self.function)
            + self.indices.get_heap_size()
            + self.values.get_heap_size()
    }
}

#[cfg(feature = "client")]
impl<
    I,
    T,
    E: InfoExtractor<Self, Info>
        + InfoExtractor<Option<[[I; 2]; 2]>, Info>
        + InfoExtractor<Vec<(I, I)>, Info>
        + InfoExtractor<Vec<T>, Info>,
    Info: EguiDisplayable,
> ShowInfo<E, Info> for PerfectGrid<I, T>
where
    Option<[[I; 2]; 2]>: ShowInfo<E, Info>,
    Vec<(I, I)>: ShowInfo<E, Info>,
    Vec<T>: ShowInfo<E, Info>,
{
    fn show_fields<C: egui_show_info::Cache<String, Info>>(
        &self,
        extractor: &mut E,
        ui: &mut egui::Ui,
        path: String,
        cache: &mut C,
    ) {
        self.extent.show_info(extractor, ui, &path, cache);
        self.indices.show_info(extractor, ui, &path, cache);
        self.values.show_info(extractor, ui, &path, cache);
    }
}

impl<I: PartialEq + Eq + Hash + Copy + Ord + Send + Sync, T: GetGridIndex<I>> PerfectGrid<I, T> {
    pub fn new() -> Self {
        let function = ph::phast::Function::from_slice_st::<(I, I)>(&[]);

        Self {
            extent: None,
            indices: vec![],
            values: vec![],
            function,
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

    fn fix_order(&mut self) {
        let mut i = 0;
        while i < self.indices.len() {
            // Everything left of i is already correct
            let correct_index = self.function.get(&self.indices[i]);

            if correct_index == i {
                i += 1;
                continue;
            }

            self.indices.swap(i, correct_index);
            self.values.swap(i, correct_index);
        }
    }

    pub fn get_extent(&self) -> Option<[RangeInclusive<I>; 2]> {
        self.extent.map(|v| v.map(|[start, end]| start..=end))
    }

    pub fn insert(&mut self, x: I, y: I, value: T) {
        self.include_in_extent(x, y);

        self.indices.push((x, y));
        self.values.push(value);
        self.function = ph::phast::Function::from_slice_mt::<(I, I)>(&self.indices);
        self.fix_order();
    }

    pub fn insert_many(
        &mut self,
        positions: impl IntoIterator<Item = (I, I)> + Clone,
        values: impl IntoIterator<Item = T> + Clone,
    ) where
        T: PartialEq + Debug,
    {
        assert_eq!(self.indices.len(), self.values.len());
        #[cfg(debug_assertions)]
        for ((x, y), value) in self.indices.iter().zip(&self.values) {
            assert_eq!(Some(value), self.get(*x, *y))
        }
        for (x, y) in positions.clone() {
            self.include_in_extent(x, y);
        }

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

        self.indices.extend(positions);
        self.values.extend(values);
        assert_eq!(self.indices.len(), self.values.len());
        self.function = ph::phast::Function::from_slice_mt::<(I, I)>(&self.indices);

        self.fix_order();

        #[cfg(debug_assertions)]
        for ((x, y), value) in self.indices.iter().zip(&self.values) {
            assert_eq!(Some(value), self.get(*x, *y))
        }
    }

    pub fn get(&self, x: I, y: I) -> Option<&T> {
        // if let Some(extent) = &self.extent {
        //     if x < extent[0][0] || x > extent[0][1] || y < extent[1][0] || y > extent[1][1] {
        //         return None;
        //     }
        // }

        let index = self.function.get(&(x, y));

        let value = &self.values[index];

        if value.get_grid_index() == (x, y) {
            Some(value)
        } else {
            None
        }
    }

    pub fn get_mut(&mut self, x: I, y: I) -> Option<&mut T> {
        if let Some(extent) = &self.extent {
            if x < extent[0][0] || x > extent[0][1] || y < extent[1][0] || y > extent[1][1] {
                return None;
            }
        }

        let index = self.function.get(&(x, y));

        let value = &mut self.values[index];

        if value.get_grid_index() == (x, y) {
            Some(value)
        } else {
            None
        }
    }

    pub fn occupied_entries(&self) -> impl Iterator<Item = ((I, I), &T)> {
        self.values.iter().zip(&self.indices).map(|(v, i)| (*i, v))
    }

    pub fn occupied_entries_mut(&mut self) -> impl Iterator<Item = ((I, I), &mut T)> {
        self.values
            .iter_mut()
            .zip(&self.indices)
            .map(|(v, i)| (*i, v))
    }
}
