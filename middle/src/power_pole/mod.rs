use backend::{
    Backend,
    power_grid::{addition::PowerGridAdditionInfo, assembler::FullAssemblerIdentifier},
};
use data::spacial::Position;
use entity_info::{EntityInfo, EntityInfoKind};
use itertools::Itertools;
use middle_indices::{PowerGridMiddleID, PowerPoleMiddleID};
use smallvec::SmallVec;

use crate::Middle;

pub const AUTOMATIC_POLE_CONNECTION_LIMIT: usize = 4;

#[derive(Debug, Clone)]
pub(crate) struct MiddlePowerPoleInfo {
    position: Position,
    connections: SmallVec<[PowerPoleMiddleID; AUTOMATIC_POLE_CONNECTION_LIMIT]>,
    grid_id: PowerGridMiddleID,
}

pub struct PowerPoleAdditionInfo<I: IntoIterator<Item = EntityInfo>> {
    pub position: Position,
    pub connections: SmallVec<[PowerPoleMiddleID; AUTOMATIC_POLE_CONNECTION_LIMIT]>,
    pub connected_entities: I,
}

impl Middle {
    // TODO: All additional info
    #[expect(clippy::let_unit_value)]
    #[must_use]
    pub fn add_power_pole(
        &mut self,
        info: PowerPoleAdditionInfo<impl IntoIterator<Item = EntityInfo>>,
        backend: &mut Backend,
    ) -> PowerPoleMiddleID {
        let PowerPoleAdditionInfo {
            position,
            mut connections,
            connected_entities,
        } = info;

        assert!(connections.iter().all_unique());

        let index = self.power_pole_list.next_push_index();

        let middle_grid_id: PowerGridMiddleID = match connections
            .iter()
            .map(|index| self.power_pole_list[index.0 as usize].grid_id)
            .all_equal_value()
        {
            Ok(grid_id) => {
                log::trace!("Join Power Pole to existing Grid");
                for &connected_pole in &connections {
                    self.power_pole_list[connected_pole.0 as usize]
                        .connections
                        .push(PowerPoleMiddleID(
                            index.try_into().expect("More than u32::MAX power poles"),
                        ));
                }

                grid_id
            },
            Err(None) => {
                log::trace!("Add new grid");
                let next_middle = self.get_next_power_grid_id();
                let new_grid = match backend.add_power_grid(PowerGridAdditionInfo {
                    middle_id: next_middle,
                }) {
                    backend::AdditionResult::Added {
                        new_id,
                        relocations,
                    } => {
                        log::trace!("Added new grid with id {:?}", new_id);

                        let actual_middle = self.add_power_grid(new_id);

                        assert_eq!(actual_middle, next_middle);

                        // Apply relocations
                        for relocation in relocations {
                            self.power_grid_list[relocation.middle.0 as usize]
                                .set_backend_id(relocation.new_backend);
                        }

                        actual_middle
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
                    self.get_power_grid_size(self.power_pole_list[grid.0 as usize].grid_id, backend)
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

                    let result = self.merge_power_grids(kept, removed, backend);

                    for relocation in result.assemblers_which_are_now_in_this_grid {
                        let assembler = &mut self.assembler_list[relocation.middle.0 as usize];

                        assembler.backend_id = relocation.new_backend;
                        assembler.power_grid_id = kept;
                    }

                    {
                        assert!(
                            self.assembler_list
                                .iter()
                                .all(|(_, info)| info.power_grid_id != removed),
                            "{:?}",
                            self.assembler_list
                                .iter()
                                .find(|(_, info)| info.power_grid_id == removed)
                        );
                    }

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
                        .push(PowerPoleMiddleID(index.try_into().unwrap()));
                }

                kept
            },
        };

        let real_index = self.power_pole_list.push(MiddlePowerPoleInfo {
            position,
            connections,
            grid_id: middle_grid_id,
        });

        assert_eq!(index, real_index);

        for connected_entity in connected_entities {
            self.make_entity_powered_by_grid(connected_entity, middle_grid_id, backend);
        }

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
                            .contains(&PowerPoleMiddleID(idx.try_into().unwrap()))
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

        PowerPoleMiddleID(index.try_into().expect("More than u32::MAX power poles"))
    }

    // FIXME: This is recursive and may cause a stack overflow for large grids!
    /// This does a DFS and sets the `grid_id` of all connected poles.
    fn set_power_pole_grid_id(&mut self, id: PowerPoleMiddleID, grid_id: PowerGridMiddleID) {
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
    pub fn are_poles_connected(&self, ids: [PowerPoleMiddleID; 2]) -> bool {
        self.power_pole_list[ids[0].0 as usize]
            .connections
            .contains(&ids[1])
    }

    fn get_pole_pos(&self, id: PowerPoleMiddleID) -> Position {
        self.power_pole_list[id.0 as usize].position
    }

    pub fn get_pole_power_grid(&self, id: PowerPoleMiddleID) -> PowerGridMiddleID {
        self.power_pole_list[id.0 as usize].grid_id
    }

    pub fn get_pole_connected_positions(
        &self,
        id: PowerPoleMiddleID,
    ) -> impl Iterator<Item = Position> {
        self.power_pole_list[id.0 as usize]
            .connections
            .iter()
            .map(|conn| self.get_pole_pos(*conn))
    }

    pub fn remove_power_pole(&mut self, id: PowerPoleMiddleID, backend: &mut Backend) {
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

    fn make_entity_powered_by_grid(
        &mut self,
        entity: EntityInfo,
        grid: PowerGridMiddleID,
        backend: &mut Backend,
    ) {
        match entity.kind {
            EntityInfoKind::Assembler { middle_id, .. } => {
                let info = &mut self.assembler_list[middle_id.0 as usize];

                let current_grid_backend =
                    self.power_grid_list[info.power_grid_id.0 as usize].backend_id;

                match backend.move_assembler(
                    FullAssemblerIdentifier {
                        recipe: info.current_recipe,
                        grid: current_grid_backend,
                        assembler_id: info.backend_id,
                    },
                    self.power_grid_list[grid.0 as usize].backend_id,
                ) {
                    backend::AdditionResult::Added {
                        new_id,
                        relocations,
                    } => {
                        info.backend_id = new_id;
                        info.power_grid_id = grid;
                        self.handle_assembler_relocations(relocations);
                    },
                    backend::AdditionResult::Failed { info } => todo!(),
                }
            },
            EntityInfoKind::PowerPole { .. } => unreachable!(),
            EntityInfoKind::Chest { .. } => unreachable!(),
        }
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

        let mut backend = Backend::new();
        let mut middle = Middle::new(&mut backend);

        for _ in 0..10000 {
            let id = middle.add_power_pole(
                PowerPoleAdditionInfo {
                    position: Position { x: 0, y: 0 },
                    connections: vec![].into(),
                    connected_entities: vec![],
                },
                &mut backend,
            );

            assert!(used_ids.insert(id), "Reused id");
        }
    }

    proptest! {
        #[test]
        fn connected_poles_report_connected(connected in ANY) {
            let mut backend = Backend::new();
            let mut middle = Middle::new(&mut backend);

            let first = middle.add_power_pole(PowerPoleAdditionInfo { position: Position { x: 0, y: 0 }, connections: vec![].into(), connected_entities: vec![], }, &mut backend);
            let connections = if connected {
                vec![first].into()
            } else {
                vec![].into()
            };
            let second = middle.add_power_pole(PowerPoleAdditionInfo { position: Position { x: 0, y: 0 }, connections, connected_entities: vec![], }, &mut backend);

            prop_assert_eq!(middle.are_poles_connected([first, second]), middle.are_poles_connected([second, first]));
            prop_assert_eq!(middle.are_poles_connected([first, second]), connected);
        }

        #[test]
        fn get_pole_pos_works(position in random_position()) {
            let mut backend = Backend::new();
            let mut middle = Middle::new(&mut backend);

            let id = middle.add_power_pole(PowerPoleAdditionInfo { position, connections: vec![].into(), connected_entities: vec![], }, &mut backend);

            prop_assert_eq!(middle.get_pole_pos(id), position);
        }

        #[test]
        fn add_poles(pole_positions in collection::vec(random_position(), 0..10), pole_connections in collection::vec(collection::vec(0..10usize, 0..3), 0..10)) {
            let mut backend = Backend::new();
            let mut middle = Middle::new(&mut backend);

            let mut ids = vec![];

            for (pos, conns) in pole_positions.into_iter().zip(pole_connections) {
                let connections: SmallVec<[PowerPoleMiddleID; 4]> = conns.into_iter().filter_map(|index| ids.get(index).copied()).unique().collect();

                let id = middle.add_power_pole(PowerPoleAdditionInfo { position: pos, connections: connections.clone(), connected_entities: vec![], }, &mut backend);
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
