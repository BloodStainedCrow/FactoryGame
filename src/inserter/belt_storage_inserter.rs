use std::marker::PhantomData;

use bytemuck::TransparentWrapper;

use crate::item::ItemTrait;
use std::marker::ConstParamTy;

use super::{InserterState, StorageID, Storages, MOVETIME};

#[derive(ConstParamTy, PartialEq, Eq)]
pub enum Dir {
    BeltToStorage = 0,
    StorageToBelt = 1,
}

#[derive(Debug, Clone, serde::Deserialize, serde::Serialize)]
pub struct BeltStorageInserter<T: ItemTrait, const DIR: Dir> {
    marker: PhantomData<T>,
    pub storage_id: StorageID,
    state: InserterState,
}

// TODO: This is the main issue to be solved.
//       The current implementation is very likely to have VERY poor cache utilization since it might load a whole
//       cache line, just to increase/decrease the value in a storage by one.
//       This problem could be somewhat reduces by just having stack inserters (since they write less often), but ensuring successive accesses
//       hit the same cache line is EXTREMELY important.
//       Luckily since inserter only have limited range (3 tiles or whatever) there is inherent locality in the accesses, if the MultiStores are somewhat spacially aligned.
//       Though this could also lead to particularly poor access patterns if the belt/line of inserters is perpendicular to the stride pattern of the Multistore
//       (maybe some weird quadtree weirdness could help?)
impl<T: ItemTrait, const DIR: Dir> BeltStorageInserter<T, DIR> {
    #[must_use]
    pub const fn new(id: StorageID) -> Self {
        Self {
            marker: PhantomData,
            storage_id: id,
            state: InserterState::Empty,
        }
    }
}

impl<T: ItemTrait> BeltStorageInserter<T, { Dir::BeltToStorage }> {
    pub fn update(&mut self, loc: &mut bool, storages: &mut Storages<T>) {
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
                if *TransparentWrapper::peel_ref(
                    &storages[self.storage_id.recipe as usize][self.storage_id.grid as usize]
                        [self.storage_id.storage as usize],
                ) < T::MAX_STACK_SIZE
                {
                    // There is space in the machine
                    *TransparentWrapper::peel_mut(
                        &mut storages[self.storage_id.recipe as usize]
                            [self.storage_id.grid as usize]
                            [self.storage_id.storage as usize],
                    ) += 1;

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

impl<T: ItemTrait> BeltStorageInserter<T, { Dir::StorageToBelt }> {
    pub fn update(&mut self, loc: &mut bool, storages: &mut Storages<T>) {
        // TODO: I just added InserterStates and it is a lot slower (unsurprisingly),
        // Try and find a faster implementation of similar logic,
        // Ideally reduce branch mispredictions as much as possible, while also reducing random loads from storages

        match self.state {
            InserterState::Empty => {
                if *TransparentWrapper::peel_ref(
                    &storages[self.storage_id.recipe as usize][self.storage_id.grid as usize]
                        [self.storage_id.storage as usize],
                ) > 0
                {
                    // There is an item in the machine
                    *TransparentWrapper::peel_mut(
                        &mut storages[self.storage_id.recipe as usize]
                            [self.storage_id.grid as usize]
                            [self.storage_id.storage as usize],
                    ) -= 1;

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
