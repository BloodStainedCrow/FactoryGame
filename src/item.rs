use proptest::{
    prop_oneof,
    strategy::{Just, Strategy},
};

#[derive(Debug, Clone, Copy)]
pub enum Item {
    Iron,
}

pub fn my_enum_strategy() -> impl Strategy<Value = Item> {
    prop_oneof![
        // For cases without data, `Just` is all you need
        Just(Item::Iron),
    ]
}
