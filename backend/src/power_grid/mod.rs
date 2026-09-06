use std::collections::HashMap;

use crate::Backend;
use data::entity::assember::Recipe;
use middle_indices::{AssemblerMiddleID, PowerGridMiddleID};
use stable_vec::StableVec;

pub mod addition;
pub mod assembler;
pub mod inserter;
pub mod merge;
mod update;

pub const NO_POWER_BACKEND_ID: PowerGridBackendID = PowerGridBackendID(0);

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct PowerGridBackendID(usize);

#[derive(Debug, Clone)]
pub(super) struct PowerGrid {
    middle_id: PowerGridMiddleID,
    assemblers: HashMap<Recipe, StableVec<SingleRecipeAssemblerInfo>>,
}

#[derive(Debug, Clone)]
struct SingleRecipeAssemblerInfo {
    // TODO: Move this around to not pollute RAM accesses while updating
    middle: AssemblerMiddleID,
}

#[derive(Debug, PartialEq, Eq, PartialOrd, Ord)]
pub struct PowerGridSize(usize);

impl Backend {
    pub fn get_power_grid_size(&self, id: PowerGridBackendID) -> PowerGridSize {
        // This is just an optimization. The number means nothing and should just encode the cost of merging power
        // TODO: Experimentally determine a good formula
        PowerGridSize(0)
    }
}
