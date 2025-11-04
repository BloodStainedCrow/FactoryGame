use std::{
    cmp::min,
    ops::{Deref, DerefMut},
};

use crate::{
    item::ITEMCOUNTTYPE,
    storage_list::{SingleItemStorages, index_fake_union},
};

use super::{FakeUnionStorage, InserterState};
#[cfg(feature = "client")]
use egui_show_info_derive::ShowInfo;
#[cfg(feature = "client")]
use get_size2::GetSize;

use crate::inserter::belt_storage_inserter::Dir;

#[cfg_attr(feature = "client", derive(ShowInfo), derive(GetSize))]
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, serde::Deserialize, serde::Serialize)]
pub enum DynInserterState {
    SBWaitingForSourceItems(ITEMCOUNTTYPE),
    SBWaitingForSpaceInDestination(ITEMCOUNTTYPE),
    SBFullAndMovingOut(u8),
    SBEmptyAndMovingBack(u8),
    BSWaitingForSourceItems(ITEMCOUNTTYPE),
    BSWaitingForSpaceInDestination(ITEMCOUNTTYPE),
    BSFullAndMovingOut(u8),
    BSEmptyAndMovingBack(u8),
}

impl From<DynInserterState> for (Dir, InserterState) {
    fn from(value: DynInserterState) -> Self {
        match value {
            DynInserterState::SBWaitingForSourceItems(v) => {
                (Dir::StorageToBelt, InserterState::WaitingForSourceItems(v))
            },
            DynInserterState::SBWaitingForSpaceInDestination(v) => (
                Dir::StorageToBelt,
                InserterState::WaitingForSpaceInDestination(v),
            ),
            DynInserterState::SBFullAndMovingOut(v) => {
                (Dir::StorageToBelt, InserterState::FullAndMovingOut(v))
            },
            DynInserterState::SBEmptyAndMovingBack(v) => {
                (Dir::StorageToBelt, InserterState::EmptyAndMovingBack(v))
            },
            DynInserterState::BSWaitingForSourceItems(v) => {
                (Dir::BeltToStorage, InserterState::WaitingForSourceItems(v))
            },
            DynInserterState::BSWaitingForSpaceInDestination(v) => (
                Dir::BeltToStorage,
                InserterState::WaitingForSpaceInDestination(v),
            ),
            DynInserterState::BSFullAndMovingOut(v) => {
                (Dir::BeltToStorage, InserterState::FullAndMovingOut(v))
            },
            DynInserterState::BSEmptyAndMovingBack(v) => {
                (Dir::BeltToStorage, InserterState::EmptyAndMovingBack(v))
            },
        }
    }
}

#[cfg_attr(feature = "client", derive(ShowInfo), derive(GetSize))]
#[derive(Debug, Clone, serde::Deserialize, serde::Serialize)]
pub struct BeltStorageInserterDyn {
    pub offset: u16,
    pub storage_id: FakeUnionStorage,
    pub state: DynInserterState,
}

impl BeltStorageInserterDyn {
    #[must_use]
    pub const fn new(dir: Dir, offset: u16, id: FakeUnionStorage) -> Self {
        Self {
            offset,
            storage_id: id,
            state: match dir {
                Dir::BeltToStorage => DynInserterState::BSWaitingForSourceItems(0),
                Dir::StorageToBelt => DynInserterState::SBWaitingForSourceItems(0),
            },
        }
    }
}

impl BeltStorageInserterDyn {
    #[inline(always)]
    pub fn update(
        &mut self,
        mut loc: impl DerefMut + Deref<Target = bool>,
        storages: SingleItemStorages,
        movetime: u8,
        max_hand_size: ITEMCOUNTTYPE,
        grid_size: usize,
    ) -> bool {
        match self.state {
            DynInserterState::BSWaitingForSourceItems(count) => {
                if *loc {
                    *loc = false;
                    if count + 1 == max_hand_size {
                        self.state = DynInserterState::BSFullAndMovingOut(movetime);
                    } else {
                        self.state = DynInserterState::BSWaitingForSourceItems(count + 1);
                    }
                    true
                } else {
                    false
                }
            },
            DynInserterState::BSWaitingForSpaceInDestination(count) => {
                let (max_insert, old) = index_fake_union(storages, self.storage_id, grid_size);
                let to_insert = min(count, *max_insert - *old);

                if to_insert > 0 {
                    *old += to_insert;
                    if to_insert == count {
                        self.state = DynInserterState::BSEmptyAndMovingBack(movetime);
                    } else {
                        self.state =
                            DynInserterState::BSWaitingForSpaceInDestination(count - to_insert);
                    }
                    true
                } else {
                    false
                }
            },
            DynInserterState::BSFullAndMovingOut(time) => {
                if time > 0 {
                    self.state = DynInserterState::BSFullAndMovingOut(time - 1);
                } else {
                    // TODO: Do I want to try inserting immediately?
                    self.state = DynInserterState::BSWaitingForSpaceInDestination(max_hand_size);
                }
                false
            },
            DynInserterState::BSEmptyAndMovingBack(time) => {
                if time > 0 {
                    self.state = DynInserterState::BSEmptyAndMovingBack(time - 1);
                } else {
                    // TODO: Do I want to try getting a new item immediately?
                    self.state = DynInserterState::BSWaitingForSourceItems(0);
                }
                false
            },
            DynInserterState::SBWaitingForSourceItems(count) => {
                let (_max_insert, old) = index_fake_union(storages, self.storage_id, grid_size);

                let to_extract = min(max_hand_size - count, *old);

                if to_extract > 0 {
                    // There is an item in the machine
                    *old -= to_extract;

                    if to_extract + count == max_hand_size {
                        self.state = DynInserterState::SBFullAndMovingOut(movetime);
                    } else {
                        self.state = DynInserterState::SBWaitingForSourceItems(count + to_extract);
                    }
                    true
                } else {
                    false
                }
            },
            DynInserterState::SBWaitingForSpaceInDestination(count) => {
                if !*loc {
                    *loc = true;

                    if count == 1 {
                        self.state = DynInserterState::SBEmptyAndMovingBack(movetime);
                    } else {
                        self.state = DynInserterState::SBWaitingForSpaceInDestination(count - 1);
                    }
                    true
                } else {
                    false
                }
            },
            DynInserterState::SBFullAndMovingOut(time) => {
                if time > 0 {
                    self.state = DynInserterState::SBFullAndMovingOut(time - 1);
                } else {
                    // TODO: Do I want to try inserting immediately?
                    self.state = DynInserterState::SBWaitingForSpaceInDestination(max_hand_size);
                }
                false
            },
            DynInserterState::SBEmptyAndMovingBack(time) => {
                if time > 0 {
                    self.state = DynInserterState::SBEmptyAndMovingBack(time - 1);
                } else {
                    // TODO: Do I want to try getting a new item immediately?
                    self.state = DynInserterState::SBWaitingForSourceItems(0);
                }
                false
            },
        }
    }
}
