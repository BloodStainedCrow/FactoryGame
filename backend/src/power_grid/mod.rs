use std::collections::BTreeMap;

use crate::{Backend, power_grid::inserter::store::InserterStore};
use data::entity::assember::Recipe;
use middle_indices::{AssemblerMiddleID, PowerGridMiddleID};
use stable_vec::StableVec;

pub mod addition;
pub mod assembler;
pub mod inserter;
pub mod merge;
pub mod power_mult;
pub mod split;
mod update;

pub const NO_POWER_BACKEND_ID: PowerGridBackendID = PowerGridBackendID(0);
pub const UNLINKED_BACKEND: PowerGridBackendID = PowerGridBackendID(u32::MAX);

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct PowerGridBackendID(u32);

#[derive(Debug, Clone)]
pub(super) struct PowerGrid {
    middle_id: PowerGridMiddleID,
    assemblers: BTreeMap<Recipe, StableVec<SingleRecipeAssemblerInfo>>,
    inserters: InserterStore,
}

#[derive(Debug, Clone)]
struct SingleRecipeAssemblerInfo {
    // TODO: Move this around to not pollute RAM accesses while updating
    middle: AssemblerMiddleID,
}

#[derive(Debug, PartialEq, Eq, PartialOrd, Ord)]
pub struct PowerGridSize(usize);

impl Backend {
    #[must_use]
    pub fn get_power_grid_size(&self, id: PowerGridBackendID) -> PowerGridSize {
        // This is just an optimization. The number means nothing and should just encode the cost of merging power
        PowerGridSize(
            self.power_grids[id.0 as usize]
                .assemblers
                .values()
                .map(StableVec::next_push_index)
                .sum(),
        )
    }
}
