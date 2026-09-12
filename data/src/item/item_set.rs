use std::collections::BTreeSet;

use itertools::Itertools;

use crate::{DATA_STORE, item::Item};

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct ItemSet {
    // TODO: This is probably really slow, and really big
    items: BTreeSet<Item>,
}

impl ItemSet {
    #[must_use]
    pub const fn empty() -> Self {
        Self {
            items: BTreeSet::new(),
        }
    }

    #[must_use]
    pub fn all() -> Self {
        Self {
            items: (0..DATA_STORE.items.len())
                .map(|idx| Item(idx as u16))
                .collect(),
        }
    }

    #[must_use]
    pub fn single_item(item: Item) -> Self {
        Self {
            items: BTreeSet::from_iter([item]),
        }
    }

    pub fn union(&mut self, other: &Self) {
        self.items.extend(other.iter());
    }

    pub fn intersection(&mut self, other: &Self) {
        self.items.retain(|item| other.items.contains(item));
    }

    pub fn difference(&mut self, other: &Self) {
        self.items.retain(|item| !other.items.contains(item));
    }

    #[must_use]
    pub fn is_subset(bigger: &Self, smaller: &Self) -> bool {
        smaller.items.is_subset(&bigger.items)
    }

    #[must_use]
    pub fn is_empty(&self) -> bool {
        self.items.is_empty()
    }

    /// # Errors
    /// If the `ItemSet` is not pure or it is empty
    pub fn is_pure(&self) -> Result<Item, Option<[Item; 2]>> {
        self.items
            .iter()
            .all_equal_value()
            .copied()
            .map_err(|err| err.0.map(|[a, b]| [*a, *b]))
    }

    pub fn iter(&self) -> impl Iterator<Item = Item> {
        self.items.iter().copied()
    }
}

impl FromIterator<Item> for ItemSet {
    fn from_iter<T: IntoIterator<Item = Item>>(iter: T) -> Self {
        Self {
            items: iter.into_iter().collect(),
        }
    }
}
