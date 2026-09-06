use backend::{
    Backend,
    power_grid::{PowerGridBackendID, PowerGridSize, merge::PowerGridMergeResult},
};
use middle_indices::PowerGridMiddleID;

use crate::Middle;

#[derive(Debug, Clone)]
pub(crate) struct PowerGridInfo {
    pub backend_id: PowerGridBackendID,
}

impl PowerGridInfo {
    pub fn set_backend_id(&mut self, new_id: PowerGridBackendID) {
        self.backend_id = new_id;
    }
}

impl Middle {
    pub(crate) fn add_power_grid(&mut self, backend_id: PowerGridBackendID) -> PowerGridMiddleID {
        let id = self.power_grid_list.push(PowerGridInfo { backend_id });

        PowerGridMiddleID(id.try_into().unwrap())
    }

    pub(crate) fn get_next_power_grid_id(&mut self) -> PowerGridMiddleID {
        let id = self.power_grid_list.next_push_index();

        PowerGridMiddleID(id.try_into().unwrap())
    }

    pub(crate) fn get_power_grid_size(
        &self,
        id: PowerGridMiddleID,
        backend: &Backend,
    ) -> PowerGridSize {
        backend.get_power_grid_size(self.power_grid_list[id.0 as usize].backend_id)
    }

    pub(crate) fn merge_power_grids(
        &mut self,
        kept: PowerGridMiddleID,
        removed: PowerGridMiddleID,
        backend: &mut Backend,
    ) -> PowerGridMergeResult {
        let kept_back = self.power_grid_list[kept.0 as usize].backend_id;
        let removed_back = self.power_grid_list[removed.0 as usize].backend_id;

        let PowerGridMergeResult {
            kept_id,
            assemblers_which_are_now_in_this_grid: changed_assembler_ids,
        } = backend.merge_power_grids(kept_back, removed_back);

        assert_eq!(kept_back, kept_id);

        PowerGridMergeResult {
            kept_id,
            assemblers_which_are_now_in_this_grid: changed_assembler_ids,
        }
    }
}
