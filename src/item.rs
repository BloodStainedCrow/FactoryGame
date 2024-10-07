use std::marker::PhantomData;

use bytemuck::TransparentWrapper;

#[derive(Debug, PartialEq, Eq, Clone, Copy)]
pub enum Item {
    IronOre,
    CopperOre,
    Iron,
    Copper,
    Circuit,
}

pub trait ItemTrait {
    const MAX_STACK_SIZE: u16;
    const ITEM_ENUM: Item;
}

pub trait GeneratableItem: ItemTrait {
    const TIME_TO_GENERATE: u16;
}

pub trait CraftableItem: ItemTrait {
    const TIME_TO_CRAFT: u16;
    const OUTPUT_AMOUNT: u16;
}

pub trait SingleIngCraft: CraftableItem {
    type ING1: ItemTrait;
    const AMOUNT1: u16;
}

pub trait TwoIngCraft: CraftableItem {
    type ING1: ItemTrait;
    const AMOUNT1: u16;

    type ING2: ItemTrait;
    const AMOUNT2: u16;
}

pub trait ThreeIngCraft: CraftableItem {
    type ING1: ItemTrait;
    const AMOUNT1: u16;

    type ING2: ItemTrait;
    const AMOUNT2: u16;

    type ING3: ItemTrait;
    const AMOUNT3: u16;
}

macro_rules! item {
    ( $x:ident, $max_stack: literal) => {
        #[derive(Debug, Default, PartialEq, Eq, Clone, Copy)]
        pub struct $x;
        impl ItemTrait for $x {
            const MAX_STACK_SIZE: u16 = $max_stack;
            const ITEM_ENUM: Item = Item::$x;
        }
    };
}

macro_rules! generatable {
    ( $x:ty, $y: literal) => {
        impl GeneratableItem for $x {
            const TIME_TO_GENERATE: u16 = $y;
        }
    };
}

macro_rules! count {
    () => (0);
    ( $x:tt $($xs:tt)* ) => (1 + count!($($xs)*));
}

macro_rules! craftable {
    ( $result:ty, $amount: expr, $time: expr, ($ing: ty, $num_ing: literal) ) => {
        impl CraftableItem for $result {
            const TIME_TO_CRAFT: u16 = $time;
            const OUTPUT_AMOUNT: u16 = $amount;
        }

        impl SingleIngCraft for $result {
            type ING1 = $ing;
            const AMOUNT1: u16 = $num_ing;
        }
    };
    ( $result:ty, $amount: expr, $time: expr, ($ing1: ty, $num_ing1: literal), ($ing2: ty, $num_ing2: literal) ) => {
        impl CraftableItem for $result {
            const TIME_TO_CRAFT: u16 = $time;
            const OUTPUT_AMOUNT: u16 = $amount;
        }

        impl TwoIngCraft for $result {
            type ING1 = $ing1;
            const AMOUNT1: u16 = $num_ing1;

            type ING2 = $ing2;
            const AMOUNT2: u16 = $num_ing2;
        }
    };
    ( $result:ty, $amount: expr, $time: expr, ($ing1: ty, $num_ing1: literal), ($ing2: ty, $num_ing2: literal), ($ing3: ty, $num_ing3: literal) ) => {
        impl CraftableItem for $result {
            const TIME_TO_CRAFT: u16 = $time;
            const OUTPUT_AMOUNT: u16 = $amount;
        }

        impl ThreeIngCraft for $result {
            type ING1 = $ing1;
            const AMOUNT1: u16 = $num_ing1;

            type ING2 = $ing2;
            const AMOUNT2: u16 = $num_ing2;

            type ING3 = $ing3;
            const AMOUNT3: u16 = $num_ing3;
        }
    };
}

#[repr(transparent)]
#[derive(Debug, TransparentWrapper, Default, Clone)]
#[transparent(u16)]
pub struct ItemStorage<T: ItemTrait>(u16, PhantomData<T>);

impl<T: ItemTrait> ItemStorage<T> {
    #[must_use]
    pub const fn new(val: u16) -> Self {
        Self(val, PhantomData)
    }
}

// LIST OF ITEMS:
item!(IronOre, 200);
generatable!(IronOre, 20);
item!(CopperOre, 200);
generatable!(CopperOre, 20);
item!(Iron, 100);
craftable!(Iron, 1, 10, (IronOre, 1));
item!(Copper, 100);
craftable!(Copper, 1, 5, (CopperOre, 1));
item!(Circuit, 200);
craftable!(Circuit, 1, 32, (Copper, 3), (Iron, 1));

#[must_use]
pub const fn get_char(item: Item) -> char {
    match item {
        Item::CopperOre => 'c',
        Item::IronOre => 'i',
        Item::Iron => 'I',
        Item::Copper => 'C',
        Item::Circuit => 'g',
    }
}
