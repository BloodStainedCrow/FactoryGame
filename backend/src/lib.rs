use stable_vec::StableVec;

use crate::power_grid::PowerGrid;

pub mod chests;
pub mod liquid;
pub mod power_grid;
pub mod slot_arenas;
pub mod train;
pub mod transport_lines;

#[derive(Debug, Clone)]
pub struct Backend {
    power_grids: StableVec<PowerGrid>,
}

pub struct RelocationInfo<ID> {
    pub old: ID,
    pub new: ID,
}

pub enum AdditionResult<ID> {
    Added {
        new_id: ID,
        relocations: Vec<RelocationInfo<ID>>,
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
