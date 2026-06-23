use data::item::Item;
use itertools::Itertools;
use smallvec::SmallVec;

use crate::{Middle, lists::PipeIndex};

#[derive(Debug, Clone)]
pub(crate) struct MiddlePipeInfo {
    connections: SmallVec<[PipeIndex; 4]>,
    fluid_system_id: (),
}

#[derive(Debug)]
pub struct PipeAdditionInfo {
    pub connections: SmallVec<[PipeIndex; 4]>,
    // connected_entities: Vec<!>
}

#[derive(Debug)]
pub enum PipeAdditionError {
    FluidMixing {
        which: [Item; 2],
        // This could be very convenient
        // wher: Position
    },
}

impl Middle {
    /// # Errors
    /// If adding this pipe would result in the mixing of fluids
    // TODO: All additional info
    #[expect(clippy::let_unit_value)]
    pub fn try_add_pipe(
        &mut self,
        info: PipeAdditionInfo,
        backend: &mut (),
    ) -> Result<PipeIndex, PipeAdditionError> {
        let PipeAdditionInfo { mut connections } = info;

        let index = self.pipe_list.next_push_index();

        #[expect(clippy::semicolon_if_nothing_returned)]
        let fluid_system_id = match connections
            .iter()
            .map(|index| self.pipe_list[index.0 as usize].fluid_system_id)
            .all_equal_value()
        {
            Ok(fluid_system_id) => {
                for &conn in &connections {
                    self.pipe_list[conn.0 as usize].connections.push(PipeIndex(
                        index.try_into().expect("More than u32::MAX pipes"),
                    ));
                }

                fluid_system_id
            },
            Err(None) => {
                // TODO: Create New Grid
                let new_fluid_system_id = ();

                new_fluid_system_id
            },
            Err(Some(_)) => {
                match connections
                    .iter()
                    .map(|index| self.pipe_list[index.0 as usize].fluid_system_id)
                    .filter_map(|system_id| todo!("Get fluid") as Option<Item>)
                    .all_equal_value()
                {
                    Ok(_) | Err(None) => {
                        // None have a fluid set, or all are the same
                    },
                    Err(Some(which)) => {
                        return Err(PipeAdditionError::FluidMixing {
                            which: which.into(),
                        });
                    },
                }

                // Merge everyting into the largest grid, to minimize swaps
                connections.sort_by_key(|pipe| todo!("Get fluid networks size"));

                let kept_pipe = connections
                    .pop()
                    .expect("If we have a merge, we will also have at least one connection");

                // We keep the largest grid untouched (since this means that we minimize work)
                let kept = self.pipe_list[kept_pipe.0 as usize].fluid_system_id;

                for connected_pipe in connections {
                    self.set_pipe_network_id(connected_pipe, kept);
                }

                todo!("Merge fluid networks");

                kept
            },
        };

        let real_index = self.pipe_list.push(MiddlePipeInfo {
            connections,
            fluid_system_id,
        });

        assert_eq!(index, real_index);

        Ok(PipeIndex(
            index.try_into().expect("More than u32::MAX pipes"),
        ))
    }

    #[expect(clippy::unit_cmp)]
    /// This does a DFS and sets the `network_id` of all connected pipes.
    fn set_pipe_network_id(&mut self, id: PipeIndex, network_id: ()) {
        let pipe = &mut self.pipe_list[id.0 as usize];

        if pipe.fluid_system_id == network_id {
            return;
        }

        pipe.fluid_system_id = network_id;

        for connection in 0..pipe.connections.len() {
            // Note: This is correct, since `set_pipe_network_id` will not change the connection graph edges, only the data in the nodes
            let connection = self.pipe_list[id.0 as usize].connections[connection];
            self.set_pipe_network_id(connection, network_id);
        }
    }

    pub fn remove_pipe(&mut self, id: PipeIndex, backend: &mut !) {
        // Remove the removed pipe from the connected pipes' connection lists
        for i in 0..self.pipe_list[id.0 as usize].connections.len() {
            let connected = self.pipe_list[id.0 as usize].connections[i];

            assert_ne!(connected, id, "Pipe connected to itself");

            self.pipe_list[connected.0 as usize]
                .connections
                .retain(|v| *v != id);
        }

        let pipe = &self.pipe_list[id.0 as usize];

        match pipe.connections.len() {
            0 => {
                // This is the last pipe of this network. Remove it.
                todo!("Remove network")
            },
            1 => {
                // No chance of splitting

                todo!("Remove pipe from network/remove connected stuff from network")
            },
            2.. => {
                todo!("Split network if needed")
            },
        }

        self.pipe_list.remove(id.0 as usize).expect("Must exist");
    }
}

#[cfg(test)]
mod test {
    use std::collections::HashSet;

    use crate::Middle;

    use super::*;

    #[test]
    fn do_not_repeat_ids() {
        let mut used_ids = HashSet::new();

        let mut middle = Middle::new();

        for _ in 0..10000 {
            let id = middle
                .try_add_pipe(
                    PipeAdditionInfo {
                        connections: vec![].into(),
                    },
                    &mut (),
                )
                .expect("No conns means no way to mix");

            assert!(used_ids.insert(id), "Reused id");
        }
    }
}
