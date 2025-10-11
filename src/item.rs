#[cfg(feature = "client")]
use egui_show_info_derive::ShowInfo;
#[cfg(feature = "client")]
use get_size::GetSize;
use std::fmt::Debug;
use std::hash::Hash;

pub type ITEMCOUNTTYPE = u8;

pub trait Indexable {
    fn into_usize(self) -> usize;
}

pub trait IdxTrait:
    Debug + serde::Serialize + for<'de> serde::Deserialize<'de> + WeakIdxTrait
{
}

#[cfg(feature = "client")]
pub trait WeakIdxTrait:
    Indexable
    + Into<usize>
    + Copy
    + Send
    + Sync
    + From<u8>
    + TryFrom<usize, Error: Debug>
    + Eq
    + Hash
    + Ord
    + 'static
    + GetSize
    + Debug
{
}

#[cfg(not(feature = "client"))]
pub trait WeakIdxTrait:
    Indexable
    + Into<usize>
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

#[cfg_attr(feature = "client", derive(ShowInfo), derive(GetSize))]
#[derive(
    Debug, PartialEq, Eq, Hash, Clone, Copy, serde::Serialize, serde::Deserialize, PartialOrd, Ord,
)]
pub struct Item<ItemIdxType: WeakIdxTrait> {
    pub id: ItemIdxType,
}

impl<ItemIdxType: IdxTrait> Indexable for Item<ItemIdxType> {
    fn into_usize(self) -> usize {
        self.id.into_usize()
    }
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

#[cfg_attr(feature = "client", derive(ShowInfo), derive(GetSize))]
#[derive(
    Debug, PartialEq, Eq, Hash, Clone, Copy, serde::Serialize, serde::Deserialize, PartialOrd, Ord,
)]
pub struct Recipe<RecipeIdxType: WeakIdxTrait> {
    pub id: RecipeIdxType,
}

impl<RecipeIdxType: WeakIdxTrait> Indexable for Recipe<RecipeIdxType> {
    fn into_usize(self) -> usize {
        self.id.into_usize()
    }
}

impl<RecipeIdxType: WeakIdxTrait> From<RecipeIdxType> for Recipe<RecipeIdxType> {
    fn from(value: RecipeIdxType) -> Self {
        Self { id: value }
    }
}
