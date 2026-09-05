use backend::{
    Backend,
    power_grid::{PowerGridBackendID, addition::PowerGridAdditionInfo},
};
use data::spacial::Position;
use itertools::Itertools;
use smallvec::SmallVec;

use crate::{Middle, lists::PowerPoleIndex};

pub const AUTOMATIC_POLE_CONNECTION_LIMIT: usize = 4;

#[derive(Debug, Clone)]
pub(crate) struct MiddlePowerPoleInfo {
    position: Position,
    connections: SmallVec<[PowerPoleIndex; AUTOMATIC_POLE_CONNECTION_LIMIT]>,
    grid_id: PowerGridBackendID,
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
        backend: &mut Backend,
    ) -> PowerPoleIndex {
        let PowerPoleAdditionInfo {
            position,
            mut connections,
        } = info;

        let index = self.power_pole_list.next_push_index();

        #[expect(clippy::semicolon_if_nothing_returned)]
        let backend_grid_id: PowerGridBackendID = match connections
            .iter()
            .map(|index| self.power_pole_list[index.0 as usize].grid_id)
            .all_equal_value()
        {
            Ok(grid_id) => {
                log::trace!("Join Power Pole to existing Grid");
                for &connected_pole in &connections {
                    self.power_pole_list[connected_pole.0 as usize]
                        .connections
                        .push(PowerPoleIndex(
                            index.try_into().expect("More than u32::MAX power poles"),
                        ));
                }

                grid_id
            },
            Err(None) => {
                log::trace!("Add new grid");
                let new_grid = match backend.add_power_grid(PowerGridAdditionInfo {}) {
                    backend::AdditionResult::Added {
                        new_id,
                        relocations,
                    } => {
                        // FIXME: Apply relocations
                        log::trace!("Added new grid with id {:?}", new_id);
                        new_id
                    },
                    backend::AdditionResult::Failed { info } => {
                        todo!("Do I want to handle this failure?")
                    },
                };

                new_grid
            },
            Err(Some(_)) => {
                // Merge everyting into the largest grid, to minimize swaps
                connections.sort_by_key(|grid| {
                    backend.get_power_grid_size(self.power_pole_list[grid.0 as usize].grid_id)
                });

                let kept_pole = connections
                    .last()
                    .expect("If we have a merge, we will also have at least one connection");

                // We keep the largest grid untouched (since this means that we minimize work)
                let kept = self.power_pole_list[kept_pole.0 as usize].grid_id;

                log::trace!("Merge grids");
                for &connected_pole in &connections {
                    let removed =
                        self.power_pole_list[connected_pole.0.try_into().unwrap()].grid_id;

                    if removed == kept {
                        continue;
                    }

                    let result = backend.merge_power_grids(kept, removed);

                    assert_eq!(result.kept_id, kept);
                    assert!(result.changed_assembler_ids.is_empty());

                    self.set_power_pole_grid_id(connected_pole, kept);

                    assert!(
                        self.power_pole_list
                            .iter()
                            .all(|(_, pole)| pole.grid_id != removed),
                    );
                }

                for &connected_pole in &connections {
                    // Add other connection direction
                    self.power_pole_list[connected_pole.0 as usize]
                        .connections
                        .push(PowerPoleIndex(index.try_into().unwrap()));
                }

                kept
            },
        };

        let real_index = self.power_pole_list.push(MiddlePowerPoleInfo {
            position,
            connections,
            grid_id: backend_grid_id,
        });

        assert_eq!(index, real_index);

        #[cfg(debug_assertions)]
        {
            assert!(
                self.power_pole_list.iter().all(|(_, pole)| {
                    pole.connections.iter().all(|connected_pole| {
                        self.power_pole_list[connected_pole.0.try_into().unwrap()].grid_id
                            == pole.grid_id
                    })
                }),
                "A pole does not have the same ID as a neighbor???"
            );

            assert!(
                self.power_pole_list.iter().all(|(idx, pole)| {
                    pole.connections.iter().all(|connected_pole| {
                        self.power_pole_list[connected_pole.0.try_into().unwrap()]
                            .connections
                            .contains(&PowerPoleIndex(idx.try_into().unwrap()))
                    })
                }),
                "Missing bi-directional connection"
            );

            assert!(
                self.power_pole_list
                    .iter()
                    .all(|(_idx, pole)| { pole.connections.iter().all_unique() }),
                "Duplicated connection entry"
            );
        }

        PowerPoleIndex(index.try_into().expect("More than u32::MAX power poles"))
    }

    // FIXME: This is recursive and may cause a stack overflow for large grids!
    /// This does a DFS and sets the `grid_id` of all connected poles.
    fn set_power_pole_grid_id(&mut self, id: PowerPoleIndex, grid_id: PowerGridBackendID) {
        let pole = &mut self.power_pole_list[id.0 as usize];

        if pole.grid_id == grid_id {
            return;
        }

        pole.grid_id = grid_id;

        for connection in 0..pole.connections.len() {
            // Note: This is correct, since `set_power_pole_grid_id` will not change the connection graph edges, only the data in the nodes
            let connected_pole = self.power_pole_list[id.0 as usize].connections[connection];
            self.set_power_pole_grid_id(connected_pole, grid_id);
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
        let mut backend = Backend::new();

        for _ in 0..10000 {
            let id = middle.add_power_pole(
                PowerPoleAdditionInfo {
                    position: Position { x: 0, y: 0 },
                    connections: vec![].into(),
                },
                &mut backend,
            );

            assert!(used_ids.insert(id), "Reused id");
        }
    }

    proptest! {
        #[test]
        fn connected_poles_report_connected(connected in ANY) {
            let mut middle = Middle::new();
            let mut backend = Backend::new();

            let first = middle.add_power_pole(PowerPoleAdditionInfo { position: Position { x: 0, y: 0 }, connections: vec![].into() }, &mut backend);
            let connections = if connected {
                vec![first].into()
            } else {
                vec![].into()
            };
            let second = middle.add_power_pole(PowerPoleAdditionInfo { position: Position { x: 0, y: 0 }, connections }, &mut backend);

            prop_assert_eq!(middle.are_poles_connected([first, second]), middle.are_poles_connected([second, first]));
            prop_assert_eq!(middle.are_poles_connected([first, second]), connected);
        }

        #[test]
        fn get_pole_pos_works(position in random_position()) {
            let mut middle = Middle::new();
            let mut backend = Backend::new();

            let id = middle.add_power_pole(PowerPoleAdditionInfo { position, connections: vec![].into() }, &mut backend);

            prop_assert_eq!(middle.get_pole_pos(id), position);
        }

        #[test]
        fn add_poles(pole_positions in collection::vec(random_position(), 0..10), pole_connections in collection::vec(collection::vec(0..10usize, 0..3), 0..10)) {
            let mut middle = Middle::new();
            let mut backend = Backend::new();

            let mut ids = vec![];

            for (pos, conns) in pole_positions.into_iter().zip(pole_connections) {
                let connections: SmallVec<[PowerPoleIndex; 4]> = conns.into_iter().filter_map(|index| ids.get(index).copied()).collect();

                let id = middle.add_power_pole(PowerPoleAdditionInfo { position: pos, connections: connections.clone() }, &mut backend);
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
