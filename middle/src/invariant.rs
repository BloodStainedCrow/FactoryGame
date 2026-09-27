use middle_indices::PowerPoleMiddleID;

use crate::{Middle, UNATTACHED_POWER_GRID_ID, power_pole::PowerPoleConnectedThing};

impl Middle {
    pub fn assert_pole_contains_thing(
        &self,
        pole: PowerPoleMiddleID,
        thing: PowerPoleConnectedThing,
    ) {
        assert!(
            self.power_pole_list[pole.0 as usize]
                .connected_things
                .contains(&thing)
        )
    }

    pub fn assert_thing_unconnected(&self, thing: PowerPoleConnectedThing) {
        let grid = match thing {
            PowerPoleConnectedThing::Assembler(id) => self.get_assembler_power_grid(id),
            PowerPoleConnectedThing::Inserter(id) => self.get_inserter_power_grid(id),
        };

        assert_eq!(grid, UNATTACHED_POWER_GRID_ID)
    }
}
