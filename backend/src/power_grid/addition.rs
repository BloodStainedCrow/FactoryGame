use crate::{AdditionResult, Backend, PowerGrid, power_grid::PowerGridBackendID};

pub struct PowerGridAdditionInfo {}

impl Backend {
    pub fn add_power_grid(
        &mut self,
        info: PowerGridAdditionInfo,
    ) -> AdditionResult<PowerGridBackendID> {
        let next_id = self.power_grids.next_push_index();

        self.power_grids.push(PowerGrid {});

        AdditionResult::Added {
            new_id: PowerGridBackendID(next_id),
            relocations: vec![],
        }
    }
}
