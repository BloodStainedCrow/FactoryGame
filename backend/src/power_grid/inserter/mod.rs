use data::item::item_set::ItemSet;
use middle_indices::InserterMiddleID;

use crate::{AdditionResult, Backend, power_grid::PowerGridBackendID};

#[derive(Debug, Clone, Copy)]
pub struct InserterBackendID(pub(crate) u32);

#[derive(Debug)]
pub struct InserterAdditionInfo {
    pub power_grid: PowerGridBackendID,
    pub middle_id: InserterMiddleID,

    pub source: !,
    pub dest: !,
    pub items: ItemSet,
    // TODO: Stats
}

#[derive(Debug, Clone, Copy)]
pub struct FullInserterIdentifier {
    pub grid: PowerGridBackendID,
    pub inserter_id: InserterBackendID,
}

pub(crate) struct SingleInserterInfo {
    middle: InserterMiddleID,
}

impl Backend {
    pub fn add_inserter(
        &mut self,
        info: InserterAdditionInfo,
    ) -> AdditionResult<InserterMiddleID, InserterBackendID> {
        self.add_inserter_internal(
            info.power_grid,
            SingleInserterInfo {
                middle: info.middle_id,
            },
        )
    }

    fn add_inserter_internal(
        &mut self,
        grid: PowerGridBackendID,
        data: SingleInserterInfo,
    ) -> AdditionResult<InserterMiddleID, InserterBackendID> {
        let inserter_list = &mut todo!();

        // let index = inserter_list.push(data);

        AdditionResult::Added {
            new_id: InserterBackendID(todo!()),
            relocations: vec![],
        }
    }

    /// NOTE: This inserter needs to already not have any inserter connections
    pub fn remove_inserter(&mut self, inserter: FullInserterIdentifier) {
        todo!()
    }

    fn remove_inserter_internal(&mut self, inserter: FullInserterIdentifier) -> SingleInserterInfo {
        let FullInserterIdentifier { grid, inserter_id } = inserter;

        let inserter_list = &mut todo!();

        // let inserter = inserter_list
        //     .remove(inserter_id.0)
        //     .expect("Tried to move inserter which did not exist");

        todo!()
    }

    pub fn move_inserter(
        &mut self,
        inserter: FullInserterIdentifier,
        new_grid: PowerGridBackendID,
    ) -> AdditionResult<InserterMiddleID, InserterBackendID> {
        let data = self.remove_inserter_internal(inserter);

        self.add_inserter_internal(new_grid, data)
    }

    pub fn get_inserter_state(&self) {}
}
