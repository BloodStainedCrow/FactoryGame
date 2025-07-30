use std::cmp::min;

use crate::{
    item::ITEMCOUNTTYPE,
    storage_list::{SingleItemStorages, index_fake_union},
};

use super::{FakeUnionStorage, InserterState};

#[cfg(feature = "client")]
use egui_show_info_derive::ShowInfo;
#[cfg(feature = "client")]
use get_size::GetSize;

// FIXME: the storage_id cannot properly represent an index into multiple slices (which I have here, since
// there are multiple lists of storages in the different MultiAssemblerStores (since multiple different recipes take for example Iron Plates))
#[cfg_attr(feature = "client", derive(ShowInfo), derive(GetSize))]
#[derive(Debug, Clone, serde::Deserialize, serde::Serialize)]
pub struct StorageStorageInserter {
    pub storage_id_in: FakeUnionStorage,
    pub storage_id_out: FakeUnionStorage,
    pub state: InserterState,
}

// This issue is less important then BeltStorage since these inserters are less common
// TODO: This is the main issue to be solved.
//       The current implementation is very likely to have VERY poor cache utilization since it might load a whole
//       cache line, just to increase/decrease the value in a storage by one.
//       This problem could be somewhat reduces by just having stack inserters (since they write less often), but ensuring successive accesses
//       hit the same cache line is EXTREMELY important.
//       Luckily since inserter only have limited range (3 tiles or whatever) there is inherent locality in the accesses, if the MultiStores are somewhat spacially aligned.
//       Though this could also lead to particularly poor access patterns if the belt/line of inserters is perpendicular to the stride pattern of the Multistore
//       (maybe some weird quadtree weirdness could help?)
impl StorageStorageInserter {
    #[must_use]
    pub const fn new(in_id: FakeUnionStorage, out_id: FakeUnionStorage) -> Self {
        Self {
            storage_id_in: in_id,
            storage_id_out: out_id,
            state: InserterState::WaitingForSourceItems(0),
        }
    }

    pub fn update(
        &mut self,
        storages: SingleItemStorages,
        movetime: u8,
        max_hand_size: ITEMCOUNTTYPE,
        grid_size: usize,
    ) {
        // TODO: I just added InserterStates and it is a lot slower (unsurprisingly),
        // Try and find a faster implementation of similar logic

        match self.state {
            InserterState::WaitingForSourceItems(count) => {
                let (_max_insert, old) = index_fake_union(storages, self.storage_id_in, grid_size);

                let to_extract = min(max_hand_size - count, *old);

                if to_extract > 0 {
                    // There is an item in the machine
                    *old -= to_extract;

                    if to_extract + count == max_hand_size {
                        self.state = InserterState::FullAndMovingOut(movetime);
                    } else {
                        self.state = InserterState::WaitingForSourceItems(count + to_extract);
                    }
                }
            },
            InserterState::WaitingForSpaceInDestination(count) => {
                let (max_insert, old) = index_fake_union(storages, self.storage_id_out, grid_size);

                let to_insert = min(count, *max_insert - *old);

                if to_insert > 0 {
                    *old += to_insert;
                    if to_insert == count {
                        self.state = InserterState::EmptyAndMovingBack(movetime);
                    } else {
                        self.state = InserterState::WaitingForSpaceInDestination(count - to_insert);
                    }
                }
            },
            InserterState::FullAndMovingOut(time) => {
                if time > 0 {
                    self.state = InserterState::FullAndMovingOut(time - 1);
                } else {
                    // TODO: Do I want to try inserting immediately?
                    self.state = InserterState::WaitingForSpaceInDestination(max_hand_size);
                }
            },
            InserterState::EmptyAndMovingBack(time) => {
                if time > 0 {
                    self.state = InserterState::EmptyAndMovingBack(time - 1);
                } else {
                    // TODO: Do I want to try getting a new item immediately?
                    self.state = InserterState::WaitingForSourceItems(0);
                }
            },
        }
    }
}
