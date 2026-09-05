use crate::{Backend, RelocationInfo, power_grid::PowerGridBackendID};

pub struct PowerGridMergeResult {
    pub kept_id: PowerGridBackendID,
    pub changed_assembler_ids: Vec<RelocationInfo<!>>,
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

        // TODO: Do the merging into the kept grid

        PowerGridMergeResult {
            kept_id: kept,
            changed_assembler_ids: vec![],
        }
    }
}
