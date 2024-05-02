use std::{
    marker::PhantomData,
    ops::{Deref, DerefMut},
};

use bytemuck::TransparentWrapper;
use enum_map::Enum;
use proptest::{
    prop_oneof,
    strategy::{Just, Strategy},
};

#[derive(Debug, PartialEq, Eq, Clone, Copy, Enum)]
pub enum Item {
    Iron,
    Copper,
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
        Item::Copper => 'C',
    }
}

#[allow(clippy::match_same_arms)]
#[must_use]
pub const fn get_time_to_generate(item: Item) -> u16 {
    match item {
        Item::Iron => 2,
        Item::Copper => 2,
    }
}

#[must_use]
pub const fn get_max_stack_size(item: Item) -> u16 {
    match item {
        Item::Iron => 2_000,
        Item::Copper => 100,
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
    fn get_time_to_generate() -> u16;
}

macro_rules! item {
    ( $x:ident, $y:expr) => {
        #[derive(Debug, Default, PartialEq, Eq, Clone, Copy)]
        pub struct $x;
        impl const ItemTrait for $x {
            fn get_item() -> Item {
                Item::$x
            }

            fn get_time_to_generate() -> u16 {
                $y
            }
        }
    };
}

item!(Iron, 2);
item!(Copper, 2);

#[allow(clippy::module_name_repetitions)]
#[repr(transparent)]
#[derive(Debug, TransparentWrapper, Default)]
#[transparent(u16)]
pub struct ItemStorage<T: ItemTrait>(u16, PhantomData<T>);

impl<T: ItemTrait> ItemStorage<T> {
    #[must_use]
    pub const fn new(val: u16) -> Self {
        Self(val, PhantomData {})
    }
}
