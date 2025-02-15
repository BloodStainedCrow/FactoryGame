use std::marker::ConstParamTy;

use crate::{assembler::SingleItemSlice, item::IdxTrait};

use super::{InserterState, StorageID, MOVETIME};

#[derive(ConstParamTy, PartialEq, Eq)]
pub enum Dir {
    BeltToStorage = 0,
    StorageToBelt = 1,
}

#[derive(Debug, Clone, serde::Deserialize, serde::Serialize)]
pub struct BeltStorageInserter<RecipeIdxType: IdxTrait, const DIR: Dir> {
    pub storage_id: StorageID<RecipeIdxType>,
    pub state: InserterState,
}

// TODO: This is the main issue to be solved.
//       The current implementation is very likely to have VERY poor cache utilization since it might load a whole
//       cache line, just to increase/decrease the value in a storage by one.
//       This problem could be somewhat reduces by just having stack inserters (since they write less often), but ensuring successive accesses
//       hit the same cache line is EXTREMELY important.
//       Luckily since inserter only have limited range (3 tiles or whatever) there is inherent locality in the accesses, if the MultiStores are somewhat spacially aligned.
//       Though this could also lead to particularly poor access patterns if the belt/line of inserters is perpendicular to the stride pattern of the Multistore
//       (maybe some weird quadtree weirdness could help?)
impl<RecipeIdxType: IdxTrait, const DIR: Dir> BeltStorageInserter<RecipeIdxType, DIR> {
    #[must_use]
    pub const fn new(id: StorageID<RecipeIdxType>) -> Self {
        Self {
            storage_id: id,
            state: InserterState::Empty,
        }
    }
}

impl<RecipeIdxType: IdxTrait> BeltStorageInserter<RecipeIdxType, { Dir::BeltToStorage }> {
    pub fn update(&mut self, loc: &mut bool, storages: SingleItemSlice) {
        // TODO: I just added InserterStates and it is a lot slower (unsurprisingly),
        // Try and find a faster implementation of similar logic

        match self.state {
            InserterState::Empty => {
                if *loc {
                    *loc = false;
                    self.state = InserterState::FullAndMovingOut(MOVETIME);
                }
            },
            InserterState::FullAndWaitingForSlot => {
                if storages[usize::from(self.storage_id.storage_list_idx)]
                    [self.storage_id.machine_idx as usize]
                    < 0
                {
                    // There is space in the machine
                    storages[usize::from(self.storage_id.storage_list_idx)]
                        [self.storage_id.machine_idx as usize] += 1;

                    self.state = InserterState::EmptyAndMovingBack(MOVETIME);
                }
            },
            InserterState::FullAndMovingOut(time) => {
                if time > 0 {
                    self.state = InserterState::FullAndMovingOut(time - 1);
                } else {
                    // TODO: Do I want to try inserting immediately?
                    self.state = InserterState::FullAndWaitingForSlot;
                }
            },
            InserterState::EmptyAndMovingBack(time) => {
                if time > 0 {
                    self.state = InserterState::EmptyAndMovingBack(time - 1);
                } else {
                    // TODO: Do I want to try getting a new item immediately?
                    self.state = InserterState::Empty;
                }
            },
        }
    }
}

impl<RecipeIdxType: IdxTrait> BeltStorageInserter<RecipeIdxType, { Dir::StorageToBelt }> {
    pub fn update(&mut self, loc: &mut bool, storages: SingleItemSlice) {
        // TODO: I just added InserterStates and it is a lot slower (unsurprisingly),
        // Try and find a faster implementation of similar logic,
        // Ideally reduce branch mispredictions as much as possible, while also reducing random loads from storages

        match self.state {
            InserterState::Empty => {
                if storages[usize::from(self.storage_id.storage_list_idx)]
                    [self.storage_id.machine_idx as usize]
                    > 0
                {
                    // There is an item in the machine
                    storages[usize::from(self.storage_id.storage_list_idx)]
                        [self.storage_id.machine_idx as usize] -= 1;

                    self.state = InserterState::FullAndMovingOut(MOVETIME);
                }
            },
            InserterState::FullAndWaitingForSlot => {
                if !*loc {
                    *loc = true;
                    self.state = InserterState::EmptyAndMovingBack(MOVETIME);
                }
            },
            InserterState::FullAndMovingOut(time) => {
                if time > 0 {
                    self.state = InserterState::FullAndMovingOut(time - 1);
                } else {
                    // TODO: Do I want to try inserting immediately?
                    self.state = InserterState::FullAndWaitingForSlot;
                }
            },
            InserterState::EmptyAndMovingBack(time) => {
                if time > 0 {
                    self.state = InserterState::EmptyAndMovingBack(time - 1);
                } else {
                    // TODO: Do I want to try getting a new item immediately?
                    self.state = InserterState::Empty;
                }
            },
        }
    }
}
