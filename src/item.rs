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
    IronOre,
    CopperOre,
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
        Item::CopperOre => 'c',
        Item::IronOre => 'i',
        Item::Iron => 'I',
        Item::Copper => 'C',
    }
}

#[allow(clippy::module_name_repetitions)]
// #[const_trait]
pub trait ItemTrait {
    fn get_item() -> Item;
    fn max_stack_size() -> u16;
}

#[allow(clippy::module_name_repetitions)]
// #[const_trait]
pub trait GeneratableItem: ItemTrait {
    fn get_time_to_generate() -> u16;
}

#[allow(clippy::module_name_repetitions)]
// #[const_trait]
pub trait CraftableItem<Ing: IngList<NUM_ING>, const NUM_ING: usize>: ItemTrait {
    fn get_time_to_craft() -> u16;
}

#[const_trait]
pub trait IngList<const N: usize> {}

impl<A: ItemTrait> IngList<1> for A {}

impl<A: ItemTrait, B: ItemTrait> IngList<2> for (A, B) {}

macro_rules! item {
    ( $x:ident, $max_stack: literal) => {
        #[derive(Debug, Default, PartialEq, Eq, Clone, Copy)]
        pub struct $x;
        impl ItemTrait for $x {
            fn get_item() -> Item {
                Item::$x
            }

            fn max_stack_size() -> u16 {
                $max_stack
            }
        }
    };
}

macro_rules! generatable {
    ( $x:ty, $y: literal) => {
        impl GeneratableItem for $x {
            fn get_time_to_generate() -> u16 {
                $y
            }
        }
    };
}

macro_rules! craftable {
    ( $x:ty, $y: expr, $num_ing: literal, $ing: ty ) => {
        impl CraftableItem<$ing, $num_ing> for $x {
            fn get_time_to_craft() -> u16 {
                $y
            }
        }
    };
}

item!(IronOre, 200);
generatable!(IronOre, 20);
item!(CopperOre, 200);
generatable!(CopperOre, 20);
item!(Iron, 100);
craftable!(Iron, 10, 1, IronOre);
item!(Copper, 100);
craftable!(Copper, 5, 1, CopperOre);

#[allow(clippy::module_name_repetitions)]
#[repr(transparent)]
#[derive(Debug, TransparentWrapper, Default)]
#[transparent(u16)]
pub struct ItemStorage<T: ItemTrait>(u16, PhantomData<T>);

impl<T: ItemTrait> ItemStorage<T> {
    #[must_use]
    pub const fn new(val: u16) -> Self {
        Self(val, PhantomData)
    }
}
