use middle_indices::AssemblerMiddleID;

use crate::{
    Backend, RelocationInfo,
    power_grid::{PowerGridBackendID, assembler::AssemblerBackendID},
};

pub struct PowerGridMergeResult {
    pub kept_id: PowerGridBackendID,
    pub assemblers_which_are_now_in_this_grid:
        Vec<RelocationInfo<AssemblerMiddleID, AssemblerBackendID>>,
}

impl Backend {
    pub fn merge_power_grids(
        &mut self,
        kept: PowerGridBackendID,
        removed: PowerGridBackendID,
    ) -> PowerGridMergeResult {
        assert_ne!(kept, removed, "Cannot merge grid with itself");

        let removed_grid = self
            .power_grids
            .remove(removed.0)
            .expect("Tried to merge non-existent power grid");

        let kept_grid = &mut self.power_grids[kept.0];

        let mut assembler_updates = vec![];

        for (recipe, removed) in removed_grid.assemblers {
            let kept = kept_grid.assemblers.entry(recipe).or_default();

            for (_old_index, removed) in removed {
                let removed_middle = removed.middle;
                let new_index = kept.push(removed);

                assembler_updates.push(RelocationInfo {
                    middle: removed_middle,
                    new_backend: AssemblerBackendID(new_index),
                });
            }
        }

        PowerGridMergeResult {
            kept_id: kept,
            assemblers_which_are_now_in_this_grid: assembler_updates,
        }
    }
}
