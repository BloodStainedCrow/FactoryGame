use std::num::NonZero;

use data::item::{ItemCountType, item_set::ItemSet};
use middle_indices::ChestMiddleID;

use crate::{
    AdditionResult, Backend,
    chests::sushi::{ItemStackIndex, SushiChest, SushiSlot},
};

#[expect(clippy::cast_possible_truncation)]
pub(crate) mod sushi;

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
pub struct ChestBackendID(pub(crate) u32);

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
pub struct FullChestIdentifier<'a> {
    pub items: &'a ItemSet,
    pub id: ChestBackendID,
}

#[derive(Debug, Clone, Copy)]
pub struct ChestAdditionInfo<'a> {
    pub items: &'a ItemSet,
    pub num_slots: ItemStackIndex,
}

#[derive(Debug)]
pub(crate) struct FullChestState {
    pub slots: Vec<SushiSlot>,
    pub stack_size_override: Option<NonZero<ItemCountType>>,
}

impl Backend {
    pub fn add_chest(
        &mut self,
        info: ChestAdditionInfo,
    ) -> AdditionResult<ChestMiddleID, ChestBackendID> {
        self.add_chest_internal(info.items, SushiChest::new(info.num_slots).into())
    }

    pub(crate) fn change_chest_items(
        &mut self,
        _chest: FullChestIdentifier,
        _new_items: &ItemSet,
    ) -> AdditionResult<ChestMiddleID, ChestBackendID> {
        todo!()
    }

    pub(crate) fn add_chest_internal(
        &mut self,
        _items: &ItemSet,
        state: FullChestState,
    ) -> AdditionResult<ChestMiddleID, ChestBackendID> {
        let index = self.sushi_chests.push(state.into());

        AdditionResult::Added {
            new_id: ChestBackendID(index.try_into().expect("More than u32::MAX chests")),
            relocations: vec![],
        }
    }

    pub(crate) fn remove_chest_internal(&mut self, chest: FullChestIdentifier) -> FullChestState {
        let data = self
            .sushi_chests
            .remove(chest.id.0 as usize)
            .expect("Tried to remove non existant chest");

        data.into()
    }

    pub fn remove_chest(&mut self, _chest: FullChestIdentifier) -> ! {
        todo!()
    }
}
