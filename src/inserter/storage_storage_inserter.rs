use std::num::NonZero;

use crate::item::ITEMCOUNTTYPE;

use super::{InserterState, MOVETIME};

// FIXME: the storage_id cannot properly represent an index into multiple slices (which I have here, since
// there are multiple lists of storages in the different MultiAssemblerStores (since multiple different recipes take for example Iron Plates))
#[derive(Debug, Clone)]
pub struct StorageStorageInserter {
    storage_id_in: NonZero<u16>,
    storage_id_out: NonZero<u16>,
    state: InserterState,
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
    pub const fn new(in_id: NonZero<u16>, out_id: NonZero<u16>) -> Self {
        Self {
            storage_id_in: in_id,
            storage_id_out: out_id,
            state: InserterState::Empty,
        }
    }

    pub fn update(&mut self, item_max_stack_size: ITEMCOUNTTYPE, storages: &mut [ITEMCOUNTTYPE]) {
        // TODO: I just added InserterStates and it is a lot slower (unsurprisingly),
        // Try and find a faster implementation of similar logic

        match self.state {
            InserterState::Empty => {
                if storages[usize::from(Into::<u16>::into(self.storage_id_in))] > 0 {
                    // There is an item in the machine
                    storages[usize::from(Into::<u16>::into(self.storage_id_in))] -= 1;

                    self.state = InserterState::FullAndMovingOut(MOVETIME);
                }
            },
            InserterState::FullAndWaitingForSlot => {
                if storages[usize::from(Into::<u16>::into(self.storage_id_out))]
                    < item_max_stack_size
                {
                    // There is space in the machine
                    storages[usize::from(Into::<u16>::into(self.storage_id_out))] += 1;

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
