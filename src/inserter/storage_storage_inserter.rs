use crate::{
    item::{IdxTrait, WeakIdxTrait},
    storage_list::{index, SingleItemStorages},
};

use super::{InserterState, Storage};

// FIXME: the storage_id cannot properly represent an index into multiple slices (which I have here, since
// there are multiple lists of storages in the different MultiAssemblerStores (since multiple different recipes take for example Iron Plates))
#[derive(Debug, Clone, serde::Deserialize, serde::Serialize)]
pub struct StorageStorageInserter<RecipeIdxType: WeakIdxTrait> {
    storage_id_in: Storage<RecipeIdxType>,
    storage_id_out: Storage<RecipeIdxType>,
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
impl<RecipeIdxType: IdxTrait> StorageStorageInserter<RecipeIdxType> {
    #[must_use]
    pub const fn new(in_id: Storage<RecipeIdxType>, out_id: Storage<RecipeIdxType>) -> Self {
        Self {
            storage_id_in: in_id,
            storage_id_out: out_id,
            state: InserterState::WaitingForSourceItems,
        }
    }

    pub fn update(
        &mut self,
        storages: SingleItemStorages,
        movetime: u8,
        num_grids_total: usize,
        num_recipes: usize,
        grid_size: usize,
    ) {
        // TODO: I just added InserterStates and it is a lot slower (unsurprisingly),
        // Try and find a faster implementation of similar logic

        match self.state {
            InserterState::WaitingForSourceItems => {
                let (_max_insert, old) = index(
                    storages,
                    self.storage_id_in,
                    num_grids_total,
                    num_recipes,
                    grid_size,
                );

                if *old > 0 {
                    // There is an item in the machine
                    *old -= 1;

                    self.state = InserterState::FullAndMovingOut(movetime);
                }
            },
            InserterState::WaitingForSpaceInDestination => {
                let (max_insert, old) = index(
                    storages,
                    self.storage_id_out,
                    num_grids_total,
                    num_recipes,
                    grid_size,
                );

                if *old < *max_insert {
                    // There is space in the machine
                    *old += 1;

                    self.state = InserterState::EmptyAndMovingBack(movetime);
                }
            },
            InserterState::FullAndMovingOut(time) => {
                if time > 0 {
                    self.state = InserterState::FullAndMovingOut(time - 1);
                } else {
                    // TODO: Do I want to try inserting immediately?
                    self.state = InserterState::WaitingForSpaceInDestination;
                }
            },
            InserterState::EmptyAndMovingBack(time) => {
                if time > 0 {
                    self.state = InserterState::EmptyAndMovingBack(time - 1);
                } else {
                    // TODO: Do I want to try getting a new item immediately?
                    self.state = InserterState::WaitingForSourceItems;
                }
            },
        }
    }
}
