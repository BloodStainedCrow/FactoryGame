use std::{collections::BTreeMap, hash::Hash, iter};

use backend::{
    Backend,
    power_grid::{
        addition::PowerGridAdditionInfo,
        assembler::FullAssemblerIdentifier,
        inserter::FullInserterIdentifier,
        merge::PowerGridMergeResult,
        split::{PowerGridSplitInfo, PowerGridSplitResult},
    },
};
use data::spacial::Position;
use entity_info::{EntityDescriptor, EntityDescriptorKind};
use indexmap::{IndexMap, map::Entry::Vacant};
use itertools::{Either, Itertools};
use middle_indices::{AssemblerMiddleID, InserterMiddleID, PowerGridMiddleID, PowerPoleMiddleID};
use pathfinding::directed::dfs::dfs_reach;
use smallvec::SmallVec;

use crate::{Middle, UNATTACHED_POWER_GRID_ID};

mod invariant;

pub const AUTOMATIC_POLE_CONNECTION_LIMIT: usize = 4;

#[derive(Debug, Clone)]
pub(crate) struct MiddlePowerPoleInfo {
    position: Position,
    connections: SmallVec<[PowerPoleMiddleID; AUTOMATIC_POLE_CONNECTION_LIMIT]>,
    pub(crate) connected_things: SmallVec<[PowerPoleConnectedThing; 2]>,
    grid_id: PowerGridMiddleID,
}

#[derive(Debug, Clone, Copy, PartialEq)]
pub enum PowerPoleConnectedThing {
    Assembler(AssemblerMiddleID),
    Inserter(InserterMiddleID),
}

pub trait GetPoleConn {
    #[must_use]
    fn get_pole_connection(&self) -> Option<PowerPoleConnectedThing>;
}

impl GetPoleConn for EntityDescriptor {
    fn get_pole_connection(&self) -> Option<PowerPoleConnectedThing> {
        // TODO: Some kinds might not want to be powered
        match self.kind {
            EntityDescriptorKind::Pipe { .. }
            | EntityDescriptorKind::Belt { .. }
            | EntityDescriptorKind::PowerPole { .. }
            | EntityDescriptorKind::Chest { .. } => None,
            EntityDescriptorKind::Assembler { id } => Some(PowerPoleConnectedThing::Assembler(id)),
            EntityDescriptorKind::Inserter { id } => Some(PowerPoleConnectedThing::Inserter(id)),
            EntityDescriptorKind::SolarPanel { .. } => todo!(),
        }
    }
}

// FIXME: This naming is garbage
#[derive(Debug)]
pub struct PowerPoleTransfer {
    pub entity: EntityDescriptor,
    pub prev_pole: Option<PowerPoleMiddleID>,
}

#[derive(Debug)]
pub struct PowerPoleAdditionInfo<I: IntoIterator<Item = PowerPoleTransfer>> {
    pub position: Position,
    pub connections: SmallVec<[PowerPoleMiddleID; AUTOMATIC_POLE_CONNECTION_LIMIT]>,
    pub connected_entities: I,
}

#[derive(Debug)]
pub struct PowerPoleRemovalInfo<I: IntoIterator<Item = EntityPowerPoleTransfer>> {
    pub id: PowerPoleMiddleID,
    pub transfers: I,
}

// FIXME: This naming is garbage
#[derive(Debug)]
pub struct EntityPowerPoleTransfer {
    pub entity: EntityDescriptor,
    pub new_pole: Option<PowerPoleMiddleID>,
}

impl Middle {
    // TODO: All additional info
    #[expect(clippy::too_many_lines)]
    #[must_use]
    pub fn add_power_pole(
        &mut self,
        info: PowerPoleAdditionInfo<impl IntoIterator<Item = PowerPoleTransfer>>,
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

                match backend.add_power_grid(&PowerGridAdditionInfo {
                    middle_id: next_middle,
                }) {
                    backend::AdditionResult::Added {
                        new_id,
                        relocations,
                    } => {
                        log::trace!("Added new grid with id {new_id:?}");

                        let actual_middle = self.add_power_grid(new_id);

                        assert_eq!(actual_middle, next_middle);

                        // Apply relocations
                        for relocation in relocations {
                            self.power_grid_list[relocation.middle.0 as usize]
                                .set_backend_id(relocation.new_backend);
                        }

                        actual_middle
                    },
                    backend::AdditionResult::Failed { info: _ } => {
                        todo!("Do I want to handle this failure?")
                    },
                }
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
                    let removed = self.power_pole_list[connected_pole.0 as usize].grid_id;

                    if removed == kept {
                        continue;
                    }

                    let PowerGridMergeResult {
                        kept_id: _,
                        assemblers_which_are_now_in_this_grid,
                        inserters_which_are_now_in_this_grid,
                    } = self.merge_power_grids(kept, removed, backend);

                    for relocation in assemblers_which_are_now_in_this_grid {
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

                    for relocation in inserters_which_are_now_in_this_grid {
                        let inserter = &mut self.inserter_list[relocation.middle.0 as usize];

                        inserter.backend_id = relocation.new_backend;
                        inserter.power_grid_id = kept;
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
                        .push(PowerPoleMiddleID(
                            index.try_into().expect("More than u32::MAX power poles"),
                        ));
                }

                kept
            },
        };

        let connected_entities = connected_entities.into_iter().collect_vec();
        let connected_things = connected_entities
            .iter()
            .map(|e| {
                e.entity
                    .get_pole_connection()
                    .expect("Entity without power support")
            })
            .collect();

        let real_index = self.power_pole_list.push(MiddlePowerPoleInfo {
            position,
            connections,
            connected_things,
            grid_id: middle_grid_id,
        });

        assert_eq!(index, real_index);

        for transfer in connected_entities {
            if let Some(old_pole) = transfer.prev_pole {
                self.power_pole_list[old_pole.0 as usize]
                    .connected_things
                    .retain(|v| {
                        *v != transfer
                            .entity
                            .get_pole_connection()
                            .expect("Entity without power support")
                    });
            }

            self.make_entity_powered_by_grid(transfer.entity, middle_grid_id, backend);
        }

        // #[cfg(debug_assertions)]
        // {
        //     assert!(
        //         self.power_pole_list.iter().all(|(_, pole)| {
        //             pole.connections.iter().all(|connected_pole| {
        //                 self.power_pole_list[connected_pole.0 as usize].grid_id == pole.grid_id
        //             })
        //         }),
        //         "A pole does not have the same ID as a neighbor???"
        //     );

        //     assert!(
        //         self.power_pole_list.iter().all(|(idx, pole)| {
        //             pole.connections.iter().all(|connected_pole| {
        //                 self.power_pole_list[connected_pole.0 as usize]
        //                     .connections
        //                     .contains(&PowerPoleMiddleID(
        //                         idx.try_into().expect("More than u32::MAX power poles"),
        //                     ))
        //             })
        //         }),
        //         "Missing bi-directional connection"
        //     );

        //     assert!(
        //         self.power_pole_list
        //             .iter()
        //             .all(|(_idx, pole)| { pole.connections.iter().all_unique() }),
        //         "Duplicated connection entry"
        //     );
        // }

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

    #[must_use]
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

    #[must_use]
    pub fn get_num_connected_poles(&self, id: PowerPoleMiddleID) -> usize {
        self.power_pole_list[id.0 as usize].connections.len()
    }

    pub fn remove_power_pole(
        &mut self,
        info: PowerPoleRemovalInfo<impl IntoIterator<Item = EntityPowerPoleTransfer>>,
        backend: &mut Backend,
    ) {
        let pole = self
            .power_pole_list
            .remove(info.id.0 as usize)
            .expect("Tried to remove pole that does not exist");

        // Remove the removed pole from the connected poles' connection lists
        for &connected in &pole.connections {
            assert_ne!(connected, info.id, "Power pole connected to itself");

            self.power_pole_list[connected.0 as usize]
                .connections
                .retain(|v| *v != info.id);
        }

        // Remove the connected entities from this pole
        for transfer in info.transfers {
            assert_ne!(Some(info.id), transfer.new_pole);
            let new_grid = transfer.new_pole.map_or(UNATTACHED_POWER_GRID_ID, |pole| {
                self.power_pole_list[pole.0 as usize].grid_id
            });
            self.make_entity_powered_by_grid(transfer.entity, new_grid, backend);

            if let Some(new_pole) = transfer.new_pole {
                self.power_pole_list[new_pole.0 as usize]
                    .connected_things
                    .push(transfer.entity.get_pole_connection().unwrap());
            }
        }

        let grid = &mut self.power_grid_list[pole.grid_id.0 as usize];

        match pole.connections.len() {
            0 => {
                // This is the last pole of this grid. Remove it.
                backend.remove_power_grid(grid.backend_id);
            },
            1 => {
                // No chance of splitting and all entities are already moved
            },
            2.. => {
                let components = connected_components_for_poles::<PowerPoleMiddleID, _>(
                    &pole.connections,
                    |node| {
                        self.power_pole_list[node.0 as usize]
                            .connections
                            .iter()
                            .copied()
                    },
                );

                match components {
                    ConnCompResult::Solo => {
                        // No splitting required, all poles are still connected
                    },

                    ConnCompResult::Multiple(mut components) => {
                        assert!(components.len() >= 1);
                        components.sort_by_key(|c| -(c.len() as isize));

                        let grid = grid.backend_id;
                        let new_middle_ids: Vec<PowerGridMiddleID> = (0..components.len() - 1)
                            .map(|_| self.add_unlinked_power_grid())
                            .collect();

                        let (mut assemblers, mut inserters) = (BTreeMap::new(), BTreeMap::new());
                        for (a, b) in components
                            .iter()
                            .enumerate()
                            .flat_map(|(comp_index, component)| {
                                component
                                    .iter()
                                    .flat_map(|pole| {
                                        self.power_pole_list[pole.0 as usize]
                                            .connected_things
                                            .iter()
                                            .map(|thing| thing)
                                    })
                                    .map(move |thing| (comp_index, thing))
                            })
                            .map(|(comp_index, pole_conn)| match pole_conn {
                                PowerPoleConnectedThing::Assembler(assembler_middle_id) => {
                                    let assembler =
                                        &self.assembler_list[assembler_middle_id.0 as usize];

                                    let grid = self.power_grid_list
                                        [assembler.power_grid_id.0 as usize]
                                        .backend_id;

                                    (
                                        Either::Left(iter::once((
                                            FullAssemblerIdentifier {
                                                recipe: assembler.current_recipe,
                                                grid,
                                                assembler_id: assembler.backend_id,
                                            },
                                            comp_index.try_into().expect(
                                                "Tried to split into more than u8::MAX components",
                                            ),
                                        ))),
                                        Either::Right(iter::empty()),
                                    )
                                },
                                PowerPoleConnectedThing::Inserter(inserter_middle_id) => (
                                    Either::Right(iter::empty()),
                                    Either::Left(iter::once((
                                        self.get_inserter_identifier(*inserter_middle_id),
                                        comp_index.try_into().expect(
                                            "Tried to split into more than u8::MAX components",
                                        ),
                                    ))),
                                ),
                            })
                        {
                            assemblers.extend(a);
                            inserters.extend(b);
                        }

                        let PowerGridSplitResult {
                            new_grid_ids,
                            new_grid_backend_ids,
                            grid_updates,
                            assembler_updates,
                            inserter_updates,
                        } = backend.split_power_grid(PowerGridSplitInfo {
                            id: grid,
                            new_middle_ids,
                            assemblers,
                            inserters,
                        });

                        for (middle_id, backend_id) in
                            new_grid_ids.iter().zip(new_grid_backend_ids.iter())
                        {
                            self.power_grid_list[middle_id.0 as usize].set_backend_id(*backend_id);
                        }

                        for (new_grid, seed_pole) in new_grid_ids.iter().zip(
                            components
                                .iter()
                                .map(|c| c.first().expect("Component with no poles?")),
                        ) {
                            self.set_power_pole_grid_id(*seed_pole, *new_grid);
                        }

                        for grid_update in grid_updates {
                            todo!("Handle grid updates");
                        }

                        for (assembler, (new_grid, new_id)) in assembler_updates {
                            self.assembler_list[assembler.0 as usize].backend_id = new_id;
                            self.assembler_list[assembler.0 as usize].power_grid_id = new_grid;
                        }

                        for (inserter, (new_grid, new_id)) in inserter_updates {
                            self.inserter_list[inserter.0 as usize].backend_id = new_id;
                            self.inserter_list[inserter.0 as usize].power_grid_id = new_grid;
                        }
                    },
                }
            },
        }
    }

    fn make_entity_powered_by_grid(
        &mut self,
        entity: EntityDescriptor,
        new_grid: PowerGridMiddleID,
        backend: &mut Backend,
    ) {
        match entity.kind {
            EntityDescriptorKind::Assembler { id: middle_id, .. } => {
                let info = &mut self.assembler_list[middle_id.0 as usize];

                let current_grid_backend =
                    self.power_grid_list[info.power_grid_id.0 as usize].backend_id;

                match backend.move_assembler(
                    FullAssemblerIdentifier {
                        recipe: info.current_recipe,
                        grid: current_grid_backend,
                        assembler_id: info.backend_id,
                    },
                    self.power_grid_list[new_grid.0 as usize].backend_id,
                ) {
                    backend::AdditionResult::Added {
                        new_id,
                        relocations,
                    } => {
                        info.backend_id = new_id;
                        info.power_grid_id = new_grid;
                        self.handle_assembler_relocations(relocations);
                    },
                    backend::AdditionResult::Failed { info: _ } => todo!(),
                }
            },
            EntityDescriptorKind::Inserter { id: middle_id, .. } => {
                let info = &self.inserter_list[middle_id.0 as usize];

                let current_grid_backend =
                    self.power_grid_list[info.power_grid_id.0 as usize].backend_id;

                let sources = info
                    .sources
                    .map(|slot| slot.map(|conn| self.get_backend_conn(conn)));

                match backend.move_inserter_into_new_grid(
                    FullInserterIdentifier {
                        grid: current_grid_backend,
                        inserter_id: info.backend_id,
                        inferred_items: &info.inferred_items,
                        source: sources,
                        dest: info.dest.map(|dest| self.get_backend_conn(dest)),
                        movetime: info.movetime,
                    },
                    self.power_grid_list[new_grid.0 as usize].backend_id,
                ) {
                    backend::AdditionResult::Added {
                        new_id,
                        relocations,
                    } => {
                        let info = &mut self.inserter_list[middle_id.0 as usize];
                        info.backend_id = new_id;
                        info.power_grid_id = new_grid;

                        if !relocations.is_empty() {
                            todo!("Handle relocations")
                        }
                    },
                    backend::AdditionResult::Failed { info: _ } => todo!(),
                }
            },
            EntityDescriptorKind::SolarPanel { .. } => todo!(),
            EntityDescriptorKind::PowerPole { .. }
            | EntityDescriptorKind::Chest { .. }
            | EntityDescriptorKind::Belt { .. }
            | EntityDescriptorKind::Pipe { .. } => unreachable!(),
        }
    }
}

#[derive(Debug)]
enum ConnCompResult<T> {
    Solo,
    Multiple(Vec<Vec<T>>),
}

pub fn connected_components_for_poles<T: Copy + Eq + Ord + Hash, I: IntoIterator<Item = T>>(
    starts: &[T],
    neighbors: impl Fn(&T) -> I + Copy,
) -> ConnCompResult<T> {
    match starts {
        [solo] => ConnCompResult::Solo,
        [a, b] => {
            let connected = bfs_bidirectional(*a, *b, neighbors);

            match connected {
                BFSResult::Found(items) => ConnCompResult::Solo,
                BFSResult::NotFound(a, b) => ConnCompResult::Multiple(vec![a, b]),
            }
        },
        lots => {
            if starts
                .array_windows()
                .all(|[a, b]| matches!(bfs_bidirectional(*a, *b, neighbors), BFSResult::Found(_)))
            {
                return ConnCompResult::Solo;
            }

            let mut components: Vec<Vec<T>> = Vec::new();

            for &start in starts {
                if components.iter().any(|comp| comp.contains(&start)) {
                    // Already in one component
                    continue;
                }

                let reachable = dfs_reach(start, &neighbors).collect();

                components.push(reachable);
            }

            ConnCompResult::Multiple(components)
        },
    }
}

enum BFSResult<N> {
    Found(Vec<N>),
    NotFound(Vec<N>, Vec<N>),
}

fn bfs_bidirectional<'a, N, FNS, IN>(start: N, end: N, neighbor_fn: FNS) -> BFSResult<N>
where
    N: Eq + Ord + Hash + Clone + 'a,
    FNS: Fn(&N) -> IN,
    IN: IntoIterator<Item = N>,
{
    if start == end {
        return BFSResult::Found(vec![start]);
    }

    let mut predecessors: IndexMap<N, usize> = Default::default();
    predecessors.insert(start, usize::MAX);
    let mut successors: IndexMap<N, usize> = Default::default();
    successors.insert(end, usize::MAX);

    let mut i_forwards = 0;
    let mut i_backwards = 0;
    let middle = 'l: loop {
        let forward_layer = predecessors.len() - i_forwards;
        let backward_layer = successors.len() - i_backwards;
        if forward_layer == 0 && backward_layer == 0 {
            break 'l None;
        }

        // Always expand the smaller frontier so the searches meet sooner.
        if backward_layer == 0 || (forward_layer > 0 && forward_layer <= backward_layer) {
            let layer_end = predecessors.len();
            while i_forwards < layer_end {
                let node = predecessors.get_index(i_forwards).unwrap().0;
                for successor_node in neighbor_fn(node) {
                    if let Vacant(e) = predecessors.entry(successor_node) {
                        if successors.contains_key(e.key()) {
                            let mid = e.key().clone();
                            e.insert(i_forwards);
                            break 'l Some(mid);
                        }
                        e.insert(i_forwards);
                    }
                }
                i_forwards += 1;
            }
        } else {
            let layer_end = successors.len();
            while i_backwards < layer_end {
                let node = successors.get_index(i_backwards).unwrap().0;
                for predecessor_node in neighbor_fn(node) {
                    if let Vacant(e) = successors.entry(predecessor_node) {
                        if predecessors.contains_key(e.key()) {
                            let mid = e.key().clone();
                            e.insert(i_backwards);
                            break 'l Some(mid);
                        }
                        e.insert(i_backwards);
                    }
                }
                i_backwards += 1;
            }
        }
    };

    match middle {
        Some(middle) => {
            let mid_idx = predecessors.get_index_of(&middle).unwrap();
            let mut path = Vec::new();
            let mut i = successors[&middle];
            while let Some((node, &parent)) = successors.get_index(i) {
                path.push(node.clone());
                i = parent;
            }
            let mut i = successors[&middle];
            while let Some((node, &parent)) = successors.get_index(i) {
                path.push(node.clone());
                i = parent;
            }
            BFSResult::Found(path)
        },
        None => BFSResult::NotFound(
            predecessors.into_keys().collect(),
            successors.into_keys().collect(),
        ),
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
