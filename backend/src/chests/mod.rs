use data::item::item_set::ItemSet;
use middle_indices::ChestMiddleID;

use crate::{
    AdditionResult, Backend,
    chests::sushi::{ItemStackIndex, SushiChest},
};

#[expect(clippy::cast_possible_truncation)]
pub(crate) mod sushi;

#[derive(Debug, Clone, Copy)]
pub struct ChestBackendID(u32);

#[derive(Debug, Clone, Copy)]
pub struct FullChestIdentifier<'a> {
    pub items: &'a ItemSet,
    pub id: ChestBackendID,
}

pub struct ChestAdditionInfo<'a> {
    pub items: &'a ItemSet,
    pub num_slots: ItemStackIndex,
}

impl Backend {
    pub fn add_chest(
        &mut self,
        info: ChestAdditionInfo,
    ) -> AdditionResult<ChestMiddleID, ChestBackendID> {
        // TODO: Implement pure chests
        let index = self.sushi_chests.push(SushiChest::new(info.num_slots));

        AdditionResult::Added {
            new_id: ChestBackendID(index.try_into().expect("More than u32::MAX chests")),
            relocations: vec![],
        }
    }

    pub(crate) fn change_chest_items(
        &mut self,
        chest: FullChestIdentifier,
        new_items: &ItemSet,
    ) -> AdditionResult<ChestMiddleID, ChestBackendID> {
        todo!()
    }

    pub fn remove_chest(&mut self, chest: FullChestIdentifier) -> ! {
        todo!()
    }
}
