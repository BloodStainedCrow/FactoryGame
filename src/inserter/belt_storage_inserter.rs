use std::{cmp::min, marker::ConstParamTy};

use crate::{
    item::ITEMCOUNTTYPE,
    storage_list::{SingleItemStorages, index_fake_union},
};

use super::{FakeUnionStorage, InserterState};
#[cfg(feature = "client")]
use egui_show_info_derive::ShowInfo;
#[cfg(feature = "client")]
use get_size2::GetSize;

#[cfg_attr(feature = "client", derive(ShowInfo), derive(GetSize))]
#[derive(
    Debug,
    ConstParamTy,
    PartialEq,
    Eq,
    PartialOrd,
    Ord,
    Hash,
    Clone,
    Copy,
    serde::Deserialize,
    serde::Serialize,
)]
pub enum Dir {
    BeltToStorage = 0,
    StorageToBelt = 1,
}

#[cfg_attr(feature = "client", derive(ShowInfo), derive(GetSize))]
#[derive(Debug, Clone, serde::Deserialize, serde::Serialize)]
pub struct BeltStorageInserter<const DIR: Dir> {
    pub storage_id: FakeUnionStorage,
    pub state: InserterState,
}

// TODO: This is the main issue to be solved.
//       The current implementation is very likely to have VERY poor cache utilization since it might load a whole
//       cache line, just to increase/decrease the value in a storage by one.
//       This problem could be somewhat reduces by just having stack inserters (since they write less often), but ensuring successive accesses
//       hit the same cache line is EXTREMELY important.
//       Luckily since inserter only have limited range (3 tiles or whatever) there is inherent locality in the accesses, if the MultiStores are somewhat spacially aligned.
//       Though this could also lead to particularly poor access patterns if the belt/line of inserters is perpendicular to the stride pattern of the Multistore
//       (maybe some quadtree weirdness could help?)
impl<const DIR: Dir> BeltStorageInserter<DIR> {
    #[must_use]
    pub const fn new(id: FakeUnionStorage) -> Self {
        Self {
            storage_id: id,
            state: InserterState::WaitingForSourceItems(0),
        }
    }
}

impl BeltStorageInserter<{ Dir::BeltToStorage }> {
    pub fn update(
        &mut self,
        loc: &mut bool,
        storages: SingleItemStorages,
        movetime: u8,
        max_hand_size: ITEMCOUNTTYPE,
        grid_size: usize,
    ) -> bool {
        // TODO: I just added InserterStates and it is a lot slower (unsurprisingly),
        // Try and find a faster implementation of similar logic

        match self.state {
            InserterState::WaitingForSourceItems(count) => {
                if *loc {
                    *loc = false;
                    if count + 1 == max_hand_size {
                        self.state = InserterState::FullAndMovingOut(movetime);
                    } else {
                        self.state = InserterState::WaitingForSourceItems(count + 1);
                    }
                    true
                } else {
                    false
                }
            },
            InserterState::WaitingForSpaceInDestination(count) => {
                let (max_insert, old) =
                    index_fake_union(todo!(), storages, self.storage_id, grid_size);
                let to_insert = min(count, *max_insert - *old);

                if to_insert > 0 {
                    *old += to_insert;
                    if to_insert == count {
                        self.state = InserterState::EmptyAndMovingBack(movetime);
                    } else {
                        self.state = InserterState::WaitingForSpaceInDestination(count - to_insert);
                    }
                    true
                } else {
                    false
                }
            },
            InserterState::FullAndMovingOut(time) => {
                if time > 0 {
                    self.state = InserterState::FullAndMovingOut(time - 1);
                } else {
                    // TODO: Do I want to try inserting immediately?
                    self.state = InserterState::WaitingForSpaceInDestination(max_hand_size);
                }
                false
            },
            InserterState::EmptyAndMovingBack(time) => {
                if time > 0 {
                    self.state = InserterState::EmptyAndMovingBack(time - 1);
                } else {
                    // TODO: Do I want to try getting a new item immediately?
                    self.state = InserterState::WaitingForSourceItems(0);
                }
                false
            },
        }
    }
}

impl BeltStorageInserter<{ Dir::StorageToBelt }> {
    pub fn update(
        &mut self,
        loc: &mut bool,
        storages: SingleItemStorages,
        movetime: u8,
        max_hand_size: ITEMCOUNTTYPE,
        grid_size: usize,
    ) -> bool {
        // TODO: I just added InserterStates and it is a lot slower (unsurprisingly),
        // Try and find a faster implementation of similar logic,
        // Ideally reduce branch mispredictions as much as possible, while also reducing random loads from storages

        match self.state {
            InserterState::WaitingForSourceItems(count) => {
                let (_max_insert, old) =
                    index_fake_union(todo!(), storages, self.storage_id, grid_size);

                let to_extract = min(max_hand_size - count, *old);

                if to_extract > 0 {
                    // There is an item in the machine
                    *old -= to_extract;

                    if to_extract + count == max_hand_size {
                        self.state = InserterState::FullAndMovingOut(movetime);
                    } else {
                        self.state = InserterState::WaitingForSourceItems(count + to_extract);
                    }
                    true
                } else {
                    false
                }
            },
            InserterState::WaitingForSpaceInDestination(count) => {
                if !*loc {
                    *loc = true;

                    if count == 1 {
                        self.state = InserterState::EmptyAndMovingBack(movetime);
                    } else {
                        self.state = InserterState::WaitingForSpaceInDestination(count - 1);
                    }
                    true
                } else {
                    false
                }
            },
            InserterState::FullAndMovingOut(time) => {
                if time > 0 {
                    self.state = InserterState::FullAndMovingOut(time - 1);
                } else {
                    // TODO: Do I want to try inserting immediately?
                    self.state = InserterState::WaitingForSpaceInDestination(max_hand_size);
                }
                false
            },
            InserterState::EmptyAndMovingBack(time) => {
                if time > 0 {
                    self.state = InserterState::EmptyAndMovingBack(time - 1);
                } else {
                    // TODO: Do I want to try getting a new item immediately?
                    self.state = InserterState::WaitingForSourceItems(0);
                }
                false
            },
        }
    }
}
