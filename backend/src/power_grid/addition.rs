use std::collections::BTreeMap;

use crate::{
    AdditionResult, Backend, PowerGrid,
    power_grid::{PowerGridBackendID, inserter::store::InserterStore},
};

use middle_indices::PowerGridMiddleID;

pub struct PowerGridAdditionInfo {
    pub middle_id: PowerGridMiddleID,
}

impl Backend {
    pub fn add_power_grid(
        &mut self,
        info: PowerGridAdditionInfo,
    ) -> AdditionResult<PowerGridMiddleID, PowerGridBackendID> {
        let PowerGridAdditionInfo { middle_id } = info;

        let next_id = self.power_grids.next_push_index();

        self.power_grids.push(PowerGrid {
            middle_id,
            assemblers: BTreeMap::new(),
            inserters: InserterStore::default(),
        });

        AdditionResult::Added {
            new_id: PowerGridBackendID(next_id),
            relocations: vec![],
        }
    }
}
