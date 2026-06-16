use itertools::Itertools;
use smallvec::SmallVec;

use crate::{Middle, lists::PowerPoleIndex};

pub const AUTOMATIC_POLE_CONNECTION_LIMIT: usize = 4;

pub(crate) struct MiddlePowerPoleInfo {
    connections: SmallVec<[PowerPoleIndex; AUTOMATIC_POLE_CONNECTION_LIMIT]>,
    grid_id: (),
    // TODO: Do I want this in here?
    // connected_entities: Vec<!>
}

pub struct PowerPoleAdditionInfo {
    pub connections: SmallVec<[PowerPoleIndex; AUTOMATIC_POLE_CONNECTION_LIMIT]>,
    // connected_entities: Vec<!>
}

impl Middle {
    // TODO: All additional info
    #[expect(clippy::let_unit_value)]
    #[must_use]
    pub fn add_power_pole(
        &mut self,
        info: PowerPoleAdditionInfo,
        backend: &mut !,
    ) -> PowerPoleIndex {
        let PowerPoleAdditionInfo { connections } = info;

        let index = self.power_pole_list.next_push_index();

        #[expect(clippy::semicolon_if_nothing_returned)]
        let grid_id = match connections
            .iter()
            .map(|index| self.power_pole_list[index.0 as usize].grid_id)
            .all_equal_value()
        {
            Ok(grid_id) => {
                for &conn in &connections {
                    self.power_pole_list[conn.0 as usize]
                        .connections
                        .push(PowerPoleIndex(
                            index.try_into().expect("More than u32::MAX assemblers"),
                        ));
                }

                grid_id
            },
            Err(None) => {
                // TODO: Create New Grid
                let new_grid = ();

                ()
            },
            Err(Some(_)) => todo!("Merge grids"),
        };

        let real_index = self.power_pole_list.push(MiddlePowerPoleInfo {
            connections,
            grid_id,
        });

        assert_eq!(index, real_index);

        PowerPoleIndex(index.try_into().expect("More than u32::MAX assemblers"))
    }

    #[must_use]
    pub fn are_poles_connected(&self, ids: [PowerPoleIndex; 2]) -> bool {
        self.power_pole_list[ids[0].0 as usize]
            .connections
            .contains(&ids[1])
    }

    pub fn remove_power_pole(&mut self, id: PowerPoleIndex, backend: &mut !) {
        todo!()
    }
}
