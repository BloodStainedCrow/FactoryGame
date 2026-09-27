use std::collections::BTreeMap;

use itertools::Itertools;
use middle_indices::{AssemblerMiddleID, InserterMiddleID, PowerGridMiddleID};

use crate::{
    Backend, RelocationInfo,
    power_grid::{
        PowerGridBackendID,
        addition::PowerGridAdditionInfo,
        assembler::{AssemblerBackendID, FullAssemblerIdentifier},
        inserter::{FullInserterIdentifier, InserterBackendID},
    },
};

pub struct PowerGridSplitInfo<'a> {
    pub id: PowerGridBackendID,

    /// The middle ids for the new grids, created by the middle *before* the
    /// split. Must be `new_count` long.
    pub new_middle_ids: Vec<PowerGridMiddleID>,
    pub assemblers: BTreeMap<FullAssemblerIdentifier, u8>,
    pub inserters: BTreeMap<FullInserterIdentifier<'a>, u8>,
}

pub struct PowerGridSplitResult {
    /// The middle ids of all resulting grids, the first being the kept grid.
    pub new_grid_ids: Vec<PowerGridMiddleID>,
    /// The backend ids of all resulting grids, parallel to `new_grid_ids`.
    pub new_grid_backend_ids: Vec<PowerGridBackendID>,
    pub grid_updates: Vec<RelocationInfo<PowerGridMiddleID, PowerGridBackendID>>,
    pub assembler_updates: Vec<(AssemblerMiddleID, (PowerGridMiddleID, AssemblerBackendID))>,
    pub inserter_updates: Vec<(InserterMiddleID, (PowerGridMiddleID, InserterBackendID))>,
}

impl Backend {
    pub fn split_power_grid(&mut self, mut info: PowerGridSplitInfo<'_>) -> PowerGridSplitResult {
        let mut grid_updates = vec![];

        let mut new_grid_backend_ids = vec![info.id];
        new_grid_backend_ids.extend(info.new_middle_ids.iter().map(|middle_id| {
            match self.add_power_grid(&PowerGridAdditionInfo {
                middle_id: *middle_id,
            }) {
                crate::AdditionResult::Added {
                    new_id,
                    relocations,
                } => {
                    grid_updates.extend(relocations);
                    new_id
                },
            }
        }));

        let new_grid_middle_ids = new_grid_backend_ids
            .iter()
            .map(|id| self.power_grids[id.0 as usize].middle_id)
            .collect_vec();

        let assembler_updates = self.split_assemblers(
            info.assemblers
                .into_iter()
                .map(|(a, idx)| (a, new_grid_backend_ids[usize::from(idx)])),
        );

        let inserter_updates = self.split_inserters(
            info.inserters
                .into_iter()
                .map(|(a, idx)| (a, new_grid_backend_ids[usize::from(idx)])),
        );

        PowerGridSplitResult {
            new_grid_ids: new_grid_middle_ids,
            new_grid_backend_ids,
            grid_updates,
            assembler_updates,
            inserter_updates,
        }
    }

    fn split_assemblers(
        &mut self,
        assembler_map: impl IntoIterator<Item = (FullAssemblerIdentifier, PowerGridBackendID)>,
    ) -> Vec<(AssemblerMiddleID, (PowerGridMiddleID, AssemblerBackendID))> {
        let mut updates = vec![];

        for (assembler, new_grid) in assembler_map {
            let (id, res) = self.move_assembler_internal(assembler, new_grid);
            match res {
                crate::AdditionResult::Added {
                    new_id,
                    relocations,
                } => {
                    let new_grid = self.power_grids[new_grid.0 as usize].middle_id;
                    updates.push((id, (new_grid, new_id)));
                    updates.extend(relocations.into_iter().map(
                        |RelocationInfo {
                             middle,
                             new_backend,
                         }| { (middle, (new_grid, new_backend)) },
                    ));
                },
            }
        }

        updates
    }

    fn split_inserters<'a>(
        &mut self,
        inserter_map: impl IntoIterator<Item = (FullInserterIdentifier<'a>, PowerGridBackendID)>,
    ) -> Vec<(InserterMiddleID, (PowerGridMiddleID, InserterBackendID))> {
        let mut updates = vec![];

        for (inserter, new_grid) in inserter_map {
            let (id, res) = self.move_inserter_into_new_grid_internal(inserter, new_grid);
            match res {
                crate::AdditionResult::Added {
                    new_id,
                    relocations,
                } => {
                    let new_grid = self.power_grids[new_grid.0 as usize].middle_id;
                    updates.push((id, (new_grid, new_id)));
                    updates.extend(relocations.into_iter().map(
                        |RelocationInfo {
                             middle,
                             new_backend,
                         }| { (middle, (new_grid, new_backend)) },
                    ));
                },
            }
        }

        updates
    }
}
