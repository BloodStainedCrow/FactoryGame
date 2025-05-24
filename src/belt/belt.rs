use std::{error::Error, fmt::Display, marker::PhantomData};

use crate::item::{IdxTrait, Item, WeakIdxTrait};

use super::smart::Side;

pub type BeltLenType = u16;

pub trait Belt<ItemIdxType: IdxTrait> {
    fn query_item(&self, pos: BeltLenType) -> Option<ItemInfo<ItemIdxType>>;
    fn get_front(&self) -> Option<ItemInfo<ItemIdxType>> {
        self.query_item(0)
    }
    fn get_back(&self) -> Option<ItemInfo<ItemIdxType>> {
        let len = self.get_len();
        self.query_item(len - 1)
    }
    fn remove_item(&mut self, pos: BeltLenType) -> Option<ItemInfo<ItemIdxType>>;
    /// # Errors
    /// When there is no space at `pos`
    fn try_insert_item(
        &mut self,
        pos: BeltLenType,
        item: Item<ItemIdxType>,
    ) -> Result<(), NoSpaceError>;

    // TODO: I need to choose here. using impl Trait means no dyn-compatibility,
    //       but a vec adds another allocation
    // fn items(&self) -> impl IntoIterator<Item = Option<Item<ItemIdxType>>>;
    fn items(&self) -> Vec<Option<Item<ItemIdxType>>>;

    fn get_len(&self) -> BeltLenType;
    fn add_length(&mut self, amount: BeltLenType, side: Side) -> BeltLenType;

    fn update(&mut self);

    fn item_hint(&self) -> Option<Vec<Item<ItemIdxType>>>;
}

pub enum ItemInfo<ItemIdxType: WeakIdxTrait> {
    Implicit,
    Sushi(Item<ItemIdxType>),
}

struct PrintMe<ItemIdxType: IdxTrait, T: Belt<ItemIdxType>>(T, PhantomData<ItemIdxType>);

impl<ItemIdxType: IdxTrait, T> Display for PrintMe<ItemIdxType, T>
where
    T: Belt<ItemIdxType>,
{
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let s = String::new();

        for i in 0..self.0.get_len() {
            // match self.query_item(i) {
            //     Some(item) => f.write_str(&item.print()),
            //     None => f.write_char("."),
            // }
        }

        write!(f, "{s}")
    }
}

#[derive(Debug)]
pub struct NoSpaceError;

impl Display for NoSpaceError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.write_str("No space in Belt!")
    }
}

impl Error for NoSpaceError {}
