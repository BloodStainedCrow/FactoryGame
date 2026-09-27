use data::item::Item;

use crate::chests::ChestBackendID;

#[derive(Debug, Clone, Copy)]
pub struct SingleItemSlotIndex(u32);

impl SingleItemSlotIndex {
    pub fn invalid() -> Self {
        Self(u32::MAX)
    }

    pub fn for_pure_chest(_item: Item, id: ChestBackendID) -> Self {
        Self(id.0)
    }
}

pub struct SingleItemSlice<'a> {
    pub current: &'a mut [u8],
    pub max: &'a [u8],
    pub inserter_wait_list: &'a mut [!],
}
