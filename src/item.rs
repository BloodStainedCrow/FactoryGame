use std::fmt::Debug;
use std::marker::PhantomData;

use bytemuck::TransparentWrapper;

use crate::power::Joule;

#[derive(Debug, PartialEq, Eq, Clone, Copy, serde::Serialize, serde::Deserialize)]
pub enum Item {
    IronOre,
    CopperOre,
    Iron,
    Copper,
    Circuit,
    Water,
    Wood,
    Coal,
    Gear,
}

pub trait ItemTrait: Debug + Default + Clone + Copy {
    const MAX_STACK_SIZE: u16;
    const ITEM_ENUM: Item;

    const NUM_RECIPES: usize;
}

pub trait GeneratableItem: ItemTrait {
    const TIME_TO_GENERATE: u16;
}

pub trait CraftableItem: ItemTrait {
    const HANDCRAFTABLE: bool;

    const TIME_TO_CRAFT: u16;
    const OUTPUT_AMOUNT: u16;
}

pub trait BurnableItem: ItemTrait {
    const OUTPUT_JOULES: Joule;
}

pub trait SmeltableItem: SingleIngCraft {}

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
    ( $x:ident, $max_stack: literal, $num_recipes: literal) => {
        #[derive(
            Debug, Default, PartialEq, Eq, Clone, Copy, serde::Deserialize, serde::Serialize,
        )]
        pub struct $x;
        impl ItemTrait for $x {
            const MAX_STACK_SIZE: u16 = $max_stack;
            const ITEM_ENUM: Item = Item::$x;

            const NUM_RECIPES: usize = $num_recipes;
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

macro_rules! burnable {
    ( $x:ty, $y: literal) => {
        impl BurnableItem for $x {
            const OUTPUT_JOULES: Joule = Joule($y);
        }

        // const _: () = assert!(
        //     $y % (MAX_BURNER_RATE.joules_per_tick().0 / 30) == 0,
        //     "Burn value must be a multiple of 1000"
        // );
    };
}

macro_rules! craftable {
    ( $result:ty, $amount: expr, $time: expr, $hand: expr, ($ing: ty, $num_ing: literal) ) => {
        impl CraftableItem for $result {
            const HANDCRAFTABLE: bool = $hand;

            const TIME_TO_CRAFT: u16 = $time;
            const OUTPUT_AMOUNT: u16 = $amount;
        }

        impl SingleIngCraft for $result {
            type ING1 = $ing;
            const AMOUNT1: u16 = $num_ing;
        }
    };
    ( $result:ty, $amount: expr, $time: expr, $hand: expr, ($ing1: ty, $num_ing1: literal), ($ing2: ty, $num_ing2: literal) ) => {
        impl CraftableItem for $result {
            const HANDCRAFTABLE: bool = $hand;

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
    ( $result:ty, $amount: expr, $time: expr, $hand: expr, ($ing1: ty, $num_ing1: literal), ($ing2: ty, $num_ing2: literal), ($ing3: ty, $num_ing3: literal) ) => {
        impl CraftableItem for $result {
            const HANDCRAFTABLE: bool = $hand;

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

macro_rules! smeltable {
    ( $result:ty, $amount: expr, $time: expr, ($ing: ty, $num_ing: literal) ) => {
        impl CraftableItem for $result {
            const HANDCRAFTABLE: bool = false;

            const TIME_TO_CRAFT: u16 = $time;
            const OUTPUT_AMOUNT: u16 = $amount;
        }

        impl SingleIngCraft for $result {
            type ING1 = $ing;
            const AMOUNT1: u16 = $num_ing;
        }

        impl SmeltableItem for $result {}
    };
}

#[repr(transparent)]
#[derive(Debug, TransparentWrapper, Default, Clone, serde::Deserialize, serde::Serialize)]
#[transparent(u16)]
pub struct ItemStorage<T: ItemTrait> {
    val: u16,
    phantom: PhantomData<T>,
}

impl<T: ItemTrait> ItemStorage<T> {
    #[must_use]
    pub const fn new(val: u16) -> Self {
        Self {
            val,
            phantom: PhantomData {},
        }
    }
}

// LIST OF ITEMS:
item!(Coal, 200, 1);
generatable!(Coal, 20);
burnable!(Coal, 4_000_000);
item!(IronOre, 200, 1);
generatable!(IronOre, 20);
item!(CopperOre, 200, 1);
generatable!(CopperOre, 20);
item!(Iron, 100, 1);
craftable!(Iron, 1, 192, true, (IronOre, 1));
item!(Copper, 100, 1);
smeltable!(Copper, 1, 192, (CopperOre, 1));
item!(Circuit, 200, 1);
craftable!(Circuit, 1, 30, true, (Copper, 3), (Iron, 1));

item!(Water, 20, 1);

item!(Wood, 100, 1);
burnable!(Wood, 2_000_000);

item!(Gear, 100, 1);
craftable!(Gear, 1, 20, true, (Iron, 2));

#[must_use]
pub const fn get_char(item: Item) -> char {
    match item {
        Item::CopperOre => 'c',
        Item::IronOre => 'i',
        Item::Iron => 'I',
        Item::Copper => 'C',
        Item::Circuit => 'g',
        Item::Water => 'W',
        Item::Wood => 'w',
        Item::Coal => 'o',
        Item::Gear => 'G',
    }
}
