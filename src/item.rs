use atomic_enum::atomic_enum;
use proptest::{
    prop_oneof,
    strategy::{Just, Strategy},
};

// TODO: Do I want to have a None value in here
#[atomic_enum]
#[derive(PartialEq, Eq)]
pub enum Item {
    None,
    Iron,
}

pub fn all_items() -> impl Strategy<Value = Item> {
    prop_oneof![
        // For cases without data, `Just` is all you need
        Just(Item::Iron),
    ]
}

#[must_use]
pub const fn get_time_to_generate(item: Item) -> u16 {
    match item {
        Item::None => 0,
        Item::Iron => 2,
    }
}

#[must_use]
pub const fn get_max_stack_size(item: Item) -> u64 {
    match item {
        Item::None => 0,
        Item::Iron => 20,
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
        result_amount: 2,
        time: 5,
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
