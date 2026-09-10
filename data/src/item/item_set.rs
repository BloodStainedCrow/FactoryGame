use crate::item::Item;

// FIXME: Currently only pure item sets are allowed
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct ItemSet {
    item: Option<Item>,
}

impl ItemSet {
    #[must_use]
    pub const fn empty() -> Self {
        Self { item: None }
    }

    #[must_use]
    pub const fn all() -> Self {
        Self {
            item: Some(Item(0)),
        }
    }

    #[must_use]
    pub const fn single_item(item: Item) -> Self {
        Self { item: Some(item) }
    }

    pub fn union(&mut self, other: &Self) {
        match (&mut self.item, other.item) {
            (None, Some(item)) => self.item = Some(item),
            (_, None) => {},
            (Some(a), Some(b)) => {
                if *a != b {
                    todo!("FIXME: Currently only pure item sets are allowed")
                }
            },
        }
    }

    pub fn intersection(&mut self, other: &Self) {
        match (&mut self.item, other.item) {
            (None, _) => {},
            (Some(_), None) => {
                self.item = None;
            },
            (Some(a), Some(b)) => {
                if *a != b {
                    self.item = None;
                }
            },
        }
    }

    #[must_use]
    pub fn difference(&self, other: &Self) -> Self {
        match (self.item, other.item) {
            (None, _) => Self { item: None },
            (Some(_), None) => self.clone(),
            (Some(a), Some(b)) => {
                if a == b {
                    Self { item: None }
                } else {
                    Self { item: Some(a) }
                }
            },
        }
    }

    #[must_use]
    pub fn is_subset(bigger: &Self, smaller: &Self) -> bool {
        match (bigger.item, smaller.item) {
            (None, Some(_)) => false,
            (_, None) => true,
            (Some(a), Some(b)) => a == b,
        }
    }

    #[must_use]
    pub const fn is_empty(&self) -> bool {
        self.item.is_none()
    }

    /// # Errors
    /// If the `ItemSet` is not pure or it is empty
    pub const fn is_pure(&self) -> Result<Item, Option<(Item, Item)>> {
        match self.item {
            Some(item) => Ok(item),
            None => Err(None),
        }
    }

    pub fn iter(&self) -> impl Iterator<Item = Item> {
        self.item.into_iter()
    }
}
