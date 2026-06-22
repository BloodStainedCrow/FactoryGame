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
                // Merge everyting into the largest grid, to minimize swaps
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

        for connection in 0..pole.connections.len() {
            // Note: This is correct, since `set_power_pole_grid_id` will not change the connection graph edges, only the data in the nodes
            let connection = self.power_pole_list[id.0 as usize].connections[connection];
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
        // Remove the removed pole from the connected poles' connection lists
        for i in 0..self.power_pole_list[id.0 as usize].connections.len() {
            let connected = self.power_pole_list[id.0 as usize].connections[i];

            assert_ne!(connected, id, "Power pole connected to itself");

            self.power_pole_list[connected.0 as usize]
                .connections
                .retain(|v| *v != id);
        }

        let pole = &self.power_pole_list[id.0 as usize];

        match pole.connections.len() {
            0 => {
                // This is the last pole of this grid. Remove it.
                todo!("Remove grid")
            },
            1 => {
                // No chance of splitting

                todo!("Remove pole from grid/remove connected stuff from grid")
            },
            2.. => {
                todo!("Split grid if needed")
            },
        }

        self.power_pole_list
            .remove(id.0 as usize)
            .expect("Must exist");
    }
}

#[cfg(test)]
mod test {
    use std::collections::HashSet;

    use data::spacial::strategies::random_position;
    use proptest::{bool::ANY, collection, prop_assert, prop_assert_eq, proptest};

    use crate::Middle;

    use super::*;

    #[test]
    fn do_not_repeat_ids() {
        let mut used_ids = HashSet::new();

        let mut middle = Middle::new();

        for _ in 0..10000 {
            let id = middle.add_power_pole(
                PowerPoleAdditionInfo {
                    position: Position { x: 0, y: 0 },
                    connections: vec![].into(),
                },
                &mut (),
            );

            assert!(used_ids.insert(id), "Reused id");
        }
    }

    proptest! {
        #[test]
        fn connected_poles_report_connected(connected in ANY) {
            let mut middle = Middle::new();

            let first = middle.add_power_pole(PowerPoleAdditionInfo { position: Position { x: 0, y: 0 }, connections: vec![].into() }, &mut ());
            let connections = if connected {
                vec![first].into()
            } else {
                vec![].into()
            };
            let second = middle.add_power_pole(PowerPoleAdditionInfo { position: Position { x: 0, y: 0 }, connections }, &mut ());

            prop_assert_eq!(middle.are_poles_connected([first, second]), middle.are_poles_connected([second, first]));
            prop_assert_eq!(middle.are_poles_connected([first, second]), connected);
        }

        #[test]
        fn get_pole_pos_works(position in random_position()) {
            let mut middle = Middle::new();

            let id = middle.add_power_pole(PowerPoleAdditionInfo { position, connections: vec![].into() }, &mut ());

            prop_assert_eq!(middle.get_pole_pos(id), position);
        }

        #[test]
        fn add_poles(pole_positions in collection::vec(random_position(), 0..10), pole_connections in collection::vec(collection::vec(0..10usize, 0..3), 0..10)) {
            let mut middle = Middle::new();

            let mut ids = vec![];

            for (pos, conns) in pole_positions.into_iter().zip(pole_connections) {
                let connections: SmallVec<[PowerPoleIndex; 4]> = conns.into_iter().filter_map(|index| ids.get(index).copied()).collect();

                let id = middle.add_power_pole(PowerPoleAdditionInfo { position: pos, connections: connections.clone() }, &mut ());
                ids.push(id);

                for other in &ids {
                    prop_assert_eq!(middle.are_poles_connected([id, *other]), middle.are_poles_connected([*other, id]));
                }

                for connected in &connections {
                    prop_assert!(middle.are_poles_connected([id, *connected]));
                }
            }

        }
    }
}
