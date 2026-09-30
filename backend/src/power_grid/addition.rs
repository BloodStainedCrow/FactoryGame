use std::collections::BTreeMap;

use crate::{
    AdditionResult, Backend, PowerGrid,
    power_grid::{PowerGridBackendID, inserter::store::InserterStore},
};

use data::entity::solar_panel::num_solar_panel_tys;
use middle_indices::PowerGridMiddleID;

pub struct PowerGridAdditionInfo {
    pub middle_id: PowerGridMiddleID,
}

impl Backend {
    pub(super) fn next_power_grid_id(&self) -> PowerGridBackendID {
        let next_id = self.power_grids.next_push_index();

        PowerGridBackendID(next_id.try_into().expect("More than u32::MAX power grids"))
    }

    pub fn add_power_grid(
        &mut self,
        info: &PowerGridAdditionInfo,
    ) -> AdditionResult<PowerGridMiddleID, PowerGridBackendID> {
        let PowerGridAdditionInfo { middle_id } = info;

        let next_id = self.power_grids.next_push_index();

        self.power_grids.push(PowerGrid {
            middle_id: *middle_id,
            assemblers: BTreeMap::new(),
            inserters: InserterStore::default(),
            solar_panel_counts: vec![0; num_solar_panel_tys()].into(),
        });

        AdditionResult::Added {
            new_id: PowerGridBackendID(next_id.try_into().expect("More than u32::MAX power grids")),
            relocations: vec![],
        }
    }

    pub fn remove_power_grid(&mut self, id: PowerGridBackendID) {
        assert_ne!(id, PowerGridBackendID(0), "Tried to remove catchall pg");

        // TODO: assert this pg is empty
        self.power_grids.remove(id.0 as usize);
    }
}
