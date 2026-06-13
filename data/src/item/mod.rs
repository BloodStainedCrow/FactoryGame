use std::num::NonZero;

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct Item(u16);

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct ItemStack {
    pub item: Item,
    pub count: NonZero<u16>,
}

#[cfg(feature = "test")]
pub mod strategies {
    use proptest::{
        prelude::{Just, Strategy},
        prop_compose, prop_oneof,
    };

    use super::{Item, ItemStack};

    pub fn random_item() -> impl Strategy<Value = Item> {
        // TODO:
        prop_oneof![Just(Item(0))]
    }

    prop_compose! {
      pub fn random_item_stack(max_count: u16)
                           (count in 1..max_count, item in random_item())
                           -> ItemStack {
        ItemStack { item, count: count.try_into().expect("Range starts at 1") }
      }
    }
}
