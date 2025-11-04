use std::{error::Error, fmt::Display};

use crate::item::{IdxTrait, Item, WeakIdxTrait};

use super::{smart::Side, splitter::SushiSplitter};

pub type BeltLenType = u16;

pub trait Belt<ItemIdxType: IdxTrait> {
    fn query_item(&self, pos: BeltLenType) -> Option<Item<ItemIdxType>>;
    fn get_front(&self) -> Option<Item<ItemIdxType>> {
        self.query_item(0)
    }
    fn get_back(&self) -> Option<Item<ItemIdxType>> {
        let len = self.get_len();
        self.query_item(len - 1)
    }
    fn remove_item(&mut self, pos: BeltLenType) -> Option<Item<ItemIdxType>>;
    /// # Errors
    /// When there is no space at `pos`
    fn try_insert_item(
        &mut self,
        pos: BeltLenType,
        item: Item<ItemIdxType>,
    ) -> Result<(), NoSpaceError>;

    // TODO: I need to choose here. using impl Trait means no dyn-compatibility,
    //       but a vec adds another allocation
    // fn items(&self) -> impl Iterator<Item = Option<Item<ItemIdxType>>>;
    fn items(&self) -> impl Iterator<Item = Option<Item<ItemIdxType>>>;
    fn items_in_range(
        &self,
        range: std::ops::RangeInclusive<BeltLenType>,
    ) -> impl Iterator<Item = Option<Item<ItemIdxType>>>;

    fn get_len(&self) -> BeltLenType;
    fn add_length(&mut self, amount: BeltLenType, side: Side) -> BeltLenType;
    /// Returns the items removed from the belt and the new length
    fn remove_length(
        &mut self,
        amount: BeltLenType,
        side: Side,
    ) -> (Vec<(Item<ItemIdxType>, u32)>, BeltLenType);

    fn update(&mut self, splitter_list: &[SushiSplitter<ItemIdxType>]);

    fn item_hint(&self) -> Option<Vec<Item<ItemIdxType>>>;
}

pub enum ItemInfo<ItemIdxType: WeakIdxTrait> {
    Implicit,
    Sushi(Item<ItemIdxType>),
}

#[derive(Debug)]
pub struct NoSpaceError;

impl Display for NoSpaceError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.write_str("No space in Belt!")
    }
}

impl Error for NoSpaceError {}
