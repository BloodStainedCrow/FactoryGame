use backend::{
    AdditionResult, Backend,
    power_grid::{NO_POWER_BACKEND_ID, addition::PowerGridAdditionInfo},
};
use middle_indices::PowerGridMiddleID;
use stable_vec::StableVec;

use crate::{
    assembler::MiddleAssemblerInfo,
    belt::{BeltTileInfo, SplitterInfo, TransportLineInfo},
    chest::ChestInfo,
    inserter::InserterInfo,
    pipe::MiddlePipeInfo,
    power_grid::PowerGridInfo,
    power_pole::MiddlePowerPoleInfo,
};

pub mod assembler;
pub mod belt;
pub mod chest;
pub mod inserter;
pub mod pipe;
pub mod power_grid;
pub mod power_pole;

#[expect(clippy::struct_field_names)]
#[derive(Debug, Clone)]
pub struct Middle {
    assembler_list: StableVec<MiddleAssemblerInfo>,
    power_pole_list: StableVec<MiddlePowerPoleInfo>,
    pipe_list: StableVec<MiddlePipeInfo>,
    power_grid_list: StableVec<PowerGridInfo>,
    inserter_list: StableVec<InserterInfo>,
    chest_list: StableVec<ChestInfo>,
    belt_tile_list: StableVec<BeltTileInfo>,
    transport_line_list: StableVec<TransportLineInfo>,
    splitter_list: StableVec<SplitterInfo>,
}

pub const UNATTACHED_POWER_GRID_ID: PowerGridMiddleID = PowerGridMiddleID(0);

impl Middle {
    #[must_use]
    pub fn new(backend: &mut Backend) -> Self {
        let AdditionResult::Added {
            new_id,
            relocations,
        } = backend.add_power_grid(&PowerGridAdditionInfo {
            middle_id: UNATTACHED_POWER_GRID_ID,
        })
        else {
            unreachable!()
        };

        assert_eq!(new_id, NO_POWER_BACKEND_ID);
        assert!(relocations.is_empty());

        Self {
            assembler_list: vec![].into(),
            power_pole_list: vec![].into(),
            pipe_list: vec![].into(),
            power_grid_list: vec![PowerGridInfo { backend_id: new_id }].into(),
            inserter_list: vec![].into(),
            chest_list: vec![].into(),
            belt_tile_list: vec![].into(),
            transport_line_list: vec![].into(),
            splitter_list: vec![].into(),
        }
    }
}

#[cfg(test)]
mod tests {
    use std::collections::BTreeMap;

    use backend::power_grid::assembler::FullAssemblerIdentifier;

    fn test() {
        let mut a: BTreeMap<FullAssemblerIdentifier, u8> = Default::default();

        let iter = [(
            FullAssemblerIdentifier {
                recipe: todo!(),
                grid: todo!(),
                assembler_id: todo!(),
            },
            0 as u8,
        )]
        .as_slice()
        .iter()
        .copied();

        a.extend(iter);
    }
}
