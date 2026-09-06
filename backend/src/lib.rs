#![feature(int_roundings)]

use stable_vec::StableVec;

use crate::power_grid::PowerGrid;

pub mod chests;
mod inserter;
pub mod liquid;
pub mod power_grid;
pub mod slot_arenas;
pub mod train;
pub mod transport_lines;

#[derive(Debug, Clone)]
pub struct Backend {
    power_grids: StableVec<PowerGrid>,
}

#[derive(Debug)]
pub struct RelocationInfo<MiddleID, BackendID> {
    pub middle: MiddleID,
    pub new_backend: BackendID,
}

pub enum AdditionResult<MiddleID, BackendID> {
    Added {
        new_id: BackendID,
        relocations: Vec<RelocationInfo<MiddleID, BackendID>>,
    },
    Failed {
        info: !,
    },
}

impl Backend {
    pub fn new() -> Self {
        Self {
            power_grids: StableVec::new(),
        }
    }
}

#[cfg(test)]
mod tests {}
