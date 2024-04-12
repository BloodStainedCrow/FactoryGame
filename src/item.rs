use std::{marker::PhantomData, sync::atomic::AtomicU64};

use enum_map::Enum;
use proptest::{
    prelude::prop,
    prop_oneof,
    strategy::{Just, Strategy},
};

#[derive(Debug, PartialEq, Eq, Clone, Copy, Enum)]
pub enum Item {
    Iron,
}

pub fn all_items() -> impl Strategy<Value = Item> {
    prop_oneof![
        // For cases without data, `Just` is all you need
        Just(Item::Iron),
    ]
}

pub fn option() -> impl Strategy<Value = Option<Item>> {
    all_items()
        .prop_flat_map(|item| Just(Some(item)))
        .boxed()
        .prop_union(Just(None).boxed())
}

#[must_use]
pub const fn get_char(item: Item) -> char {
    match item {
        Item::Iron => 'I',
    }
}

#[must_use]
pub const fn get_time_to_generate(item: Item) -> u16 {
    match item {
        Item::Iron => 2,
    }
}

#[must_use]
pub const fn get_max_stack_size(item: Item) -> u64 {
    match item {
        Item::Iron => 2_000_000_000_000,
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct Recipe {
    pub ingredient: Item,
    pub ingredient_amount: u64,
    pub result: Item,
    pub result_amount: u64,
    pub time: u16,
}

pub const RECIPES: [Recipe; 2] = [
    Recipe {
        ingredient: Item::Iron,
        ingredient_amount: 1,
        result: Item::Iron,
        result_amount: 1,
        time: 20,
    },
    Recipe {
        ingredient: Item::Iron,
        ingredient_amount: 1,
        result: Item::Iron,
        result_amount: 2,
        time: 5,
    },
];

pub fn all_recipes() -> impl Strategy<Value = Recipe> {
    prop_oneof![
        Just(Recipe {
            ingredient: Item::Iron,
            ingredient_amount: 1,
            result: Item::Iron,
            result_amount: 2,
            time: 5,
        }),
        Just(Recipe {
            ingredient: Item::Iron,
            ingredient_amount: 1,
            result: Item::Iron,
            result_amount: 2,
            time: 5,
        }),
    ]
}

#[allow(clippy::module_name_repetitions)]
#[const_trait]
pub trait ItemTrait {
    fn get_item() -> Item;
}

#[derive(Debug, Default, PartialEq, Eq, Clone, Copy)]
pub struct Iron;
// #[derive(Debug, Default, PartialEq, Eq, Clone, Copy)]
// pub struct Copper;

impl const ItemTrait for Iron {
    fn get_item() -> Item {
        Item::Iron
    }
}

// impl ItemTrait for Copper {
//     fn get_item() -> Item {
//         Item::Copper
//     }
// }

#[derive(Debug)]
#[allow(clippy::module_name_repetitions)]
#[repr(transparent)]
pub struct ItemStorageStrict<T: ItemTrait> {
    pub count: AtomicU64,
    marker: PhantomData<T>,
}

impl<T: ItemTrait> Default for ItemStorageStrict<T> {
    fn default() -> Self {
        Self {
            count: AtomicU64::default(),
            marker: PhantomData,
        }
    }
}

impl<T: ItemTrait> From<ItemStorageStrict<T>> for ItemStorage {
    fn from(val: ItemStorageStrict<T>) -> Self {
        Self {
            count: val.count,
            item: T::get_item(),
        }
    }
}

impl<T: ItemTrait> ItemStorer for ItemStorageStrict<T> {
    fn get_item(&self) -> Item {
        T::get_item()
    }

    fn get_count(&self) -> &AtomicU64 {
        &self.count
    }
}

#[derive(Debug)]
#[allow(clippy::module_name_repetitions)]
pub struct ItemStorage {
    pub count: AtomicU64,
    pub item: Item,
}

#[allow(clippy::module_name_repetitions)]
pub trait ItemStorer {
    fn get_item(&self) -> Item;
    fn get_count(&self) -> &AtomicU64;
}
