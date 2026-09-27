use middle_indices::PowerPoleMiddleID;

use crate::{Middle, power_pole::PowerPoleConnectedThing};

impl Middle {
    pub fn assert_pole_and_thing_agree_on_grid(
        &self,
        pole: PowerPoleMiddleID,
        thing: PowerPoleConnectedThing,
    ) {
        let grid = match thing {
            PowerPoleConnectedThing::Assembler(id) => self.get_assembler_power_grid(id),
            PowerPoleConnectedThing::Inserter(id) => self.get_inserter_power_grid(id),
        };

        assert_eq!(self.power_pole_list[pole.0 as usize].grid_id, grid)
    }
}
