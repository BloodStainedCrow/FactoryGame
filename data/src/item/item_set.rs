use std::collections::BTreeSet;

use itertools::Itertools;

use crate::{DATA_STORE, item::Item};

#[derive(Debug, Clone)]
pub enum LimitedItemSet<const N: usize = 5> {
    All,
    Only([Item; N]),
    None,
}

impl<const N: usize> From<LimitedItemSet<N>> for ItemSet {
    fn from(set: LimitedItemSet<N>) -> Self {
        match set {
            LimitedItemSet::All => Self::all(),
            LimitedItemSet::Only(items) => Self {
                items: items.into_iter().collect(),
            },
            LimitedItemSet::None => Self::empty(),
        }
    }
}

impl<const N: usize> ItemSetTrait for LimitedItemSet<N> {
    fn contains(&self, item: Item) -> bool {
        match self {
            Self::All => true,
            Self::Only(items) => items.contains(&item),
            Self::None => false,
        }
    }

    fn is_empty(&self) -> bool {
        matches!(self, Self::None)
    }
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord)]
pub struct ItemSet {
    // TODO: This is probably really slow, and really big
    items: BTreeSet<Item>,
}

pub trait ItemSetTrait {
    #[must_use]
    fn contains(&self, item: Item) -> bool;
    #[must_use]
    fn is_empty(&self) -> bool;
}

impl ItemSetTrait for ItemSet {
    fn contains(&self, item: Item) -> bool {
        self.items.contains(&item)
    }

    fn is_empty(&self) -> bool {
        self.items.is_empty()
    }
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
                .map(|idx| Item(idx.try_into().expect("More than u16::MAX Items")))
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

    pub fn intersection(&mut self, other: &impl ItemSetTrait) {
        self.items.retain(|item| other.contains(*item));
    }

    pub fn difference(&mut self, other: &impl ItemSetTrait) {
        self.items.retain(|item| !other.contains(*item));
    }

    #[must_use]
    pub fn is_subset(smaller: &Self, bigger: &Self) -> bool {
        smaller.items.is_subset(&bigger.items)
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
