use std::fmt::Debug;

use std::hash::Hash;

use crate::assembler::TIMERTYPE;

pub type ITEMCOUNTTYPE = u8;

type AllItems = Vec<ItemDescriptor>;
type AllMachines<ItemIdxType: IdxTrait> = Vec<MachineDescriptor<ItemIdxType>>;

pub trait IdxTrait:
    Into<usize>
    + Copy
    + Send
    + Sync
    + From<u8>
    + TryFrom<usize, Error: Debug>
    + Debug
    + serde::Serialize
    + PartialEq
    + Eq
    + Hash
    + Ord
    + 'static
{
}

struct ItemDescriptor {
    name: &'static str,
    stack_size: u8,
    // etc
}

struct MachineDescriptor<ItemIdxType: IdxTrait> {
    name: &'static str,
    item_to_place: Option<Item<ItemIdxType>>,
}

#[derive(
    Debug, PartialEq, Eq, Hash, Clone, Copy, serde::Serialize, serde::Deserialize, PartialOrd, Ord,
)]
pub struct Item<ItemIdxType: IdxTrait> {
    pub id: ItemIdxType,
}

impl<ItemIdxType: IdxTrait> From<ItemIdxType> for Item<ItemIdxType> {
    fn from(value: ItemIdxType) -> Self {
        Self { id: value }
    }
}

impl<ItemIdxType: IdxTrait> Item<ItemIdxType> {
    pub fn print(self) -> String {
        format!("{}", self.id.into())
    }
}

#[derive(
    Debug, PartialEq, Eq, Hash, Clone, Copy, serde::Serialize, serde::Deserialize, PartialOrd, Ord,
)]
pub struct Recipe<RecipeIdxType: IdxTrait> {
    pub id: RecipeIdxType,
}

impl<RecipeIdxType: IdxTrait> From<RecipeIdxType> for Recipe<RecipeIdxType> {
    fn from(value: RecipeIdxType) -> Self {
        Self { id: value }
    }
}

#[derive(Debug, Clone, Copy)]
struct Machine {
    id: u8,
}

pub type ItemStack<ItemIdxType: IdxTrait> = (Item<ItemIdxType>, ITEMCOUNTTYPE);

pub fn recipe_item_idx<RecipeIdxType: IdxTrait, ItemIdxType: IdxTrait>(
    recipe: Recipe<RecipeIdxType>,
    item: Item<ItemIdxType>,
) -> Option<usize> {
    todo!()
}
