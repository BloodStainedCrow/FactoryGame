use std::num::NonZero;

pub type ItemCountType = u16;

pub mod item_set;

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
pub struct Item(u16);

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct ItemStack {
    pub item: Item,
    pub count: NonZero<ItemCountType>,
}

#[must_use]
pub const fn max_stack_size(item: Item) -> NonZero<ItemCountType> {
    // TODO
    NonZero::new(2).expect("Hardcoded")
}

#[cfg(feature = "test")]
pub mod strategies {
    use proptest::{
        prelude::{Just, Strategy},
        prop_compose, prop_oneof,
    };

    use super::{Item, ItemCountType, ItemStack};

    pub fn random_item() -> impl Strategy<Value = Item> {
        // TODO:
        prop_oneof![Just(Item(0))]
    }

    prop_compose! {
      pub fn random_item_stack(max_count: ItemCountType)
                           (count in 1..max_count, item in random_item())
                           -> ItemStack {
        ItemStack { item, count: count.try_into().expect("Range starts at 1") }
      }
    }
}
