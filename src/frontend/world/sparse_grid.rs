use std::collections::HashMap;

#[derive(Debug, serde::Serialize, serde::Deserialize)]
pub struct SparseGrid<T> {
    width: usize,
    height: usize,
    values: HashMap<(usize, usize), T>,
}

impl<T> SparseGrid<T> {
    pub fn new(width: usize, height: usize) -> Self {
        Self {
            width,
            height,
            values: HashMap::new(),
        }
    }

    pub fn get_default(&mut self, x: usize, y: usize) -> &T
    where
        T: Default,
    {
        assert!(
            x < self.width,
            "index {x} out of bounds for width {}",
            self.width
        );
        assert!(
            y < self.height,
            "index {y} out of bounds for height {}",
            self.height
        );

        self.values.entry((x, y)).or_default()
    }

    pub fn get(&self, x: usize, y: usize) -> Option<&T> {
        assert!(
            x < self.width,
            "index {x} out of bounds for width {}",
            self.width
        );
        assert!(
            y < self.height,
            "index {y} out of bounds for height {}",
            self.height
        );

        self.values.get(&(x, y))
    }

    pub fn get_mut(&mut self, x: usize, y: usize) -> Option<&mut T> {
        assert!(
            x < self.width,
            "index {x} out of bounds for width {}",
            self.width
        );
        assert!(
            y < self.height,
            "index {y} out of bounds for height {}",
            self.height
        );

        self.values.get_mut(&(x, y))
    }

    pub fn insert(&mut self, x: usize, y: usize, value: T) -> Option<T> {
        assert!(
            x < self.width,
            "index {x} out of bounds for width {}",
            self.width
        );
        assert!(
            y < self.height,
            "index {y} out of bounds for height {}",
            self.height
        );

        self.values.insert((x, y), value)
    }

    pub fn insert_deduplicate(&mut self, x: usize, y: usize, value: T) -> Option<T>
    where
        T: PartialEq + Default,
    {
        assert!(
            x < self.width,
            "index {x} out of bounds for width {}",
            self.width
        );
        assert!(
            y < self.height,
            "index {y} out of bounds for height {}",
            self.height
        );

        if value == T::default() {
            self.values.remove(&(x, y))
        } else {
            self.values.insert((x, y), value)
        }
    }

    pub fn occupied_entries(&self) -> impl Iterator<Item = ((usize, usize), &T)> {
        self.values.iter().map(|(a, b)| (*a, b))
    }

    pub fn occupied_entries_mut(&mut self) -> impl Iterator<Item = ((usize, usize), &mut T)> {
        self.values.iter_mut().map(|(a, b)| (*a, b))
    }
}
