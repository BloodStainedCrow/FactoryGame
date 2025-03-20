use std::fmt::Debug;

use std::hash::Hash;

pub type ITEMCOUNTTYPE = u8;

pub trait IdxTrait:
    Debug + serde::Serialize + for<'de> serde::Deserialize<'de> + WeakIdxTrait
{
}

pub trait WeakIdxTrait:
    Into<usize>
    + Copy
    + Send
    + Sync
    + From<u8>
    + TryFrom<usize, Error: Debug>
    + Eq
    + Hash
    + Ord
    + 'static
{
}

pub fn usize_from<T: IdxTrait>(t: T) -> usize {
    Into::<usize>::into(t)
}

//impl<T: WeakIdxTrait> From<T> for usize {
//    fn from(value: T) -> Self {
//        Into::<usize>::into(value)
//    }
//}

#[derive(
    Debug, PartialEq, Eq, Hash, Clone, Copy, serde::Serialize, serde::Deserialize, PartialOrd, Ord,
)]
pub struct Item<ItemIdxType: WeakIdxTrait> {
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
pub struct Recipe<RecipeIdxType: WeakIdxTrait> {
    pub id: RecipeIdxType,
}

impl<RecipeIdxType: IdxTrait> From<RecipeIdxType> for Recipe<RecipeIdxType> {
    fn from(value: RecipeIdxType) -> Self {
        Self { id: value }
    }
}
