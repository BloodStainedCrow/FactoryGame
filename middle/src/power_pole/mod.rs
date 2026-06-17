use data::spacial::Position;
use itertools::Itertools;
use smallvec::SmallVec;

use crate::{Middle, lists::PowerPoleIndex};

pub const AUTOMATIC_POLE_CONNECTION_LIMIT: usize = 4;

#[derive(Debug, Clone)]
pub(crate) struct MiddlePowerPoleInfo {
    position: Position,
    connections: SmallVec<[PowerPoleIndex; AUTOMATIC_POLE_CONNECTION_LIMIT]>,
    grid_id: (),
    // TODO: Do I want this in here?
    // connected_entities: Vec<!>
}

pub struct PowerPoleAdditionInfo {
    pub position: Position,
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
        backend: &mut (),
    ) -> PowerPoleIndex {
        let PowerPoleAdditionInfo {
            position,
            mut connections,
        } = info;

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

                new_grid
            },
            Err(Some(_)) => {
                connections.sort_by_key(|grid| todo!("Get Grid size"));

                let kept_pole = connections
                    .pop()
                    .expect("If we have a merge, we will also have at least one connection");

                // We keep the largest grid untouched (since this means that we minimize work)
                let kept = self.power_pole_list[kept_pole.0 as usize].grid_id;

                for connected_pole in connections {
                    self.set_power_pole_grid_id(connected_pole, kept);
                }

                todo!("Merge grids");
            },
        };

        let real_index = self.power_pole_list.push(MiddlePowerPoleInfo {
            position,
            connections,
            grid_id,
        });

        assert_eq!(index, real_index);

        PowerPoleIndex(index.try_into().expect("More than u32::MAX assemblers"))
    }

    #[expect(clippy::unit_cmp)]
    /// This does a DFS and sets the `grid_id` of all connected poles.
    fn set_power_pole_grid_id(&mut self, id: PowerPoleIndex, grid_id: ()) {
        let pole = &mut self.power_pole_list[id.0 as usize];

        if pole.grid_id == grid_id {
            return;
        }

        pole.grid_id = grid_id;

        let conns = pole.connections.iter().copied().collect_vec();

        for connection in conns {
            self.set_power_pole_grid_id(connection, grid_id);
        }
    }

    #[must_use]
    pub fn are_poles_connected(&self, ids: [PowerPoleIndex; 2]) -> bool {
        self.power_pole_list[ids[0].0 as usize]
            .connections
            .contains(&ids[1])
    }

    fn get_pole_pos(&self, id: PowerPoleIndex) -> Position {
        self.power_pole_list[id.0 as usize].position
    }

    pub fn get_pole_connected_positions(
        &self,
        id: PowerPoleIndex,
    ) -> impl Iterator<Item = Position> {
        self.power_pole_list[id.0 as usize]
            .connections
            .iter()
            .map(|conn| self.get_pole_pos(*conn))
    }

    pub fn remove_power_pole(&mut self, id: PowerPoleIndex, backend: &mut !) {
        todo!()
    }
}
