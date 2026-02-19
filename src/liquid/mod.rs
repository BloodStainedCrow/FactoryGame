use std::cmp::min;
use std::collections::HashMap;
use std::collections::hash_map::Entry;
use std::u8;

use itertools::Itertools;
use log::warn;

use crate::inserter::FakeUnionStorage;
use crate::item::Indexable;
use crate::storage_list::{SingleItemStorages, index_fake_union};
use crate::{
    data::DataStore,
    frontend::world::Position,
    item::{IdxTrait, Item, WeakIdxTrait},
    network_graph::{Network, WeakIndex},
};

#[cfg(feature = "client")]
use egui_show_info_derive::ShowInfo;
#[cfg(feature = "client")]
use get_size2::GetSize;

pub mod connection_logic;

#[cfg_attr(feature = "client", derive(ShowInfo), derive(GetSize))]
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, serde::Deserialize, serde::Serialize)]
pub struct FluidSystemId<ItemIdxType: WeakIdxTrait> {
    pub fluid: Option<Item<ItemIdxType>>,
    pub index: usize,
}

#[cfg_attr(feature = "client", derive(ShowInfo), derive(GetSize))]
#[derive(Debug, Clone, serde::Deserialize, serde::Serialize)]
pub struct FluidSystemStore<ItemIdxType: WeakIdxTrait> {
    pub fluid_systems_with_fluid: Box<[Vec<Option<FluidSystem<ItemIdxType>>>]>,
    fluid_systems_with_fluid_holes: Box<[Vec<usize>]>,

    pub empty_fluid_systems: Vec<Option<FluidSystem<ItemIdxType>>>,
    empty_fluid_systems_holes: Vec<usize>,

    pub fluid_box_pos_to_network_id: HashMap<Position, FluidSystemId<ItemIdxType>>,
}

#[derive(Debug, Clone, Copy)]
pub struct CannotMixFluidsError<ItemIdxType: WeakIdxTrait> {
    pub items: [Item<ItemIdxType>; 2],
}

#[cfg_attr(feature = "client", derive(ShowInfo), derive(GetSize))]
#[derive(Debug, Clone, Copy, PartialEq, Eq, serde::Deserialize, serde::Serialize)]
pub enum FluidConnectionDir {
    Output,
    Input,
}

impl<ItemIdxType: IdxTrait> FluidSystemStore<ItemIdxType> {
    #[must_use]
    pub fn new<RecipeIdxType: IdxTrait>(
        data_store: &DataStore<ItemIdxType, RecipeIdxType>,
    ) -> Self {
        // TODO: We can save some space here by only having fluid systems for items which are actually a fluid
        Self {
            fluid_systems_with_fluid: vec![vec![]; data_store.item_display_names.len()]
                .into_boxed_slice(),
            fluid_systems_with_fluid_holes: vec![vec![]; data_store.item_display_names.len()]
                .into_boxed_slice(),
            empty_fluid_systems: vec![],
            empty_fluid_systems_holes: vec![],
            fluid_box_pos_to_network_id: Default::default(),
        }
    }

    pub fn get_fluid_network(&self, id: FluidSystemId<ItemIdxType>) -> &FluidSystem<ItemIdxType> {
        match id.fluid {
            Some(fluid) => self.fluid_systems_with_fluid[fluid.into_usize()][id.index]
                .as_ref()
                .unwrap(),
            None => self.empty_fluid_systems[id.index].as_ref().unwrap(),
        }
    }

    fn get_fluid_network_mut(
        &mut self,
        id: FluidSystemId<ItemIdxType>,
    ) -> &mut FluidSystem<ItemIdxType> {
        match id.fluid {
            Some(fluid) => self.fluid_systems_with_fluid[fluid.into_usize()][id.index]
                .as_mut()
                .unwrap(),
            None => self.empty_fluid_systems[id.index].as_mut().unwrap(),
        }
    }

    pub fn trusted_add_fluid_network_without_fluid_box(
        &mut self,
        fluid: Option<Item<ItemIdxType>>,
    ) -> FluidSystemId<ItemIdxType> {
        let new_network = FluidSystem::trusted_new_without_fluid_box(fluid);

        let index = match fluid {
            Some(fluid) => {
                let index = self.fluid_systems_with_fluid_holes[fluid.into_usize()].pop();

                if let Some(hole_idx) = index {
                    assert!(self.fluid_systems_with_fluid[fluid.into_usize()][hole_idx].is_none());
                    self.fluid_systems_with_fluid[fluid.into_usize()][hole_idx] = Some(new_network);
                    hole_idx
                } else {
                    self.fluid_systems_with_fluid[fluid.into_usize()].push(Some(new_network));
                    self.fluid_systems_with_fluid[fluid.into_usize()].len() - 1
                }
            },
            None => {
                let index = self.empty_fluid_systems_holes.pop();

                if let Some(hole_idx) = index {
                    assert!(self.empty_fluid_systems[hole_idx].is_none());
                    self.empty_fluid_systems[hole_idx] = Some(new_network);
                    hole_idx
                } else {
                    self.empty_fluid_systems.push(Some(new_network));
                    self.empty_fluid_systems.len() - 1
                }
            },
        };

        let new_id: FluidSystemId<ItemIdxType> = FluidSystemId { fluid, index };

        new_id
    }

    pub fn trusted_add_fluid_box(
        &mut self,

        fluid_network_id: FluidSystemId<ItemIdxType>,

        new_fluid_box_position: Position,
        fluid_box_capacity: u32,

        connected_fluid_box_positions: impl IntoIterator<Item = Position>,
        connected_storages: impl IntoIterator<
            Item = (
                FluidConnectionDir,
                Item<ItemIdxType>,
                FakeUnionStorage,
                Position,
            ),
        > + Clone,
    ) {
        self.fluid_box_pos_to_network_id
            .insert(new_fluid_box_position, fluid_network_id);

        let network = self.get_fluid_network_mut(fluid_network_id);

        network.add_fluid_box_trusted(
            new_fluid_box_position,
            fluid_box_capacity,
            connected_fluid_box_positions,
        );

        for (dir, fluid, storage, pos) in connected_storages {
            assert_eq!(fluid, fluid_network_id.fluid.unwrap());

            let weak_index = match dir {
                FluidConnectionDir::Input => {
                    network.add_input(fluid, new_fluid_box_position, storage, pos)
                },
                FluidConnectionDir::Output => {
                    network.add_output(fluid, new_fluid_box_position, storage, pos)
                },
            };
        }
    }

    /// # Errors:
    /// If adding this fluid box would connect fluid systems with different fluids
    pub fn try_add_fluid_box(
        &mut self,
        new_fluid_box_position: Position,
        fluid_box_capacity: u32,

        connected_fluid_box_positions: impl IntoIterator<Item = Position>,
        connected_storages: impl IntoIterator<
            Item = (
                FluidConnectionDir,
                Item<ItemIdxType>,
                FakeUnionStorage,
                Position,
                Box<dyn FnOnce(WeakIndex) -> ()>,
            ),
        > + Clone,
    ) -> Result<(), CannotMixFluidsError<ItemIdxType>> {
        let connected_boxes: Vec<_> = connected_fluid_box_positions.into_iter().collect();

        let connected_storages_fluid = match connected_storages
            .clone()
            .into_iter()
            .map(|(_dir, fluid, _storage, _pos, _cb)| fluid)
            .all_equal_value()
        {
            Ok(fluid) => Some(fluid),
            Err(Some((a, b))) => return Err(CannotMixFluidsError { items: [a, b] }),
            Err(None) => None,
        };

        let id_the_box_ends_up_with = if let Some(_first_connection) = connected_boxes.last() {
            let network_ids: Vec<_> = connected_boxes
                .iter()
                .map(|pos| self.fluid_box_pos_to_network_id[pos])
                .collect();

            let network_to_join_into = network_ids
                .iter()
                .copied()
                .max_by_key(|id| match id.fluid {
                    Some(fluid) => self.fluid_systems_with_fluid[fluid.into_usize()]
                        [id.index as usize]
                        .as_ref()
                        .unwrap()
                        .graph
                        .node_count(),
                    None => 0,
                })
                .unwrap();

            let need_to_merge = !network_ids.iter().all_equal();

            let final_id = if need_to_merge {
                let merge_fluid = match network_ids
                    .iter()
                    // Remove all fluid networks without a set fluid type since we can just set them to whatever fluid we might decide on
                    .flat_map(|id| id.fluid)
                    .all_equal_value()
                {
                    Ok(fluid) => Some(fluid),
                    Err(Some((a, b))) => {
                        return Err(CannotMixFluidsError { items: [a, b] });
                    },
                    Err(None) => None,
                };

                assert_eq!(network_to_join_into.fluid, merge_fluid);

                let _final_fluid = match (merge_fluid, connected_storages_fluid) {
                    (None, None) => None,
                    (None, Some(fluid)) => Some(fluid),
                    (Some(fluid), None) => Some(fluid),
                    (Some(a), Some(b)) => {
                        if a == b {
                            Some(a)
                        } else {
                            return Err(CannotMixFluidsError { items: [a, b] });
                        }
                    },
                };

                let removed_ids: Vec<_> = connected_boxes
                    .iter()
                    .map(|pos| self.fluid_box_pos_to_network_id[pos])
                    .unique()
                    .collect();

                assert!(removed_ids.iter().all_unique());
                for removed_id in removed_ids {
                    if removed_id == network_to_join_into {
                        // Do not join into itself
                        continue;
                    }
                    self.merge_fluid_system(
                        network_to_join_into,
                        removed_id,
                        new_fluid_box_position,
                        fluid_box_capacity,
                        connected_boxes.iter().copied(),
                    );
                }

                network_to_join_into
            } else {
                // We have connections, but they are all part of the same fluid network
                let network = match network_to_join_into.fluid {
                    Some(fluid) => {
                        &mut self.fluid_systems_with_fluid[fluid.into_usize()]
                            [network_to_join_into.index]
                    },
                    None => &mut self.empty_fluid_systems[network_to_join_into.index],
                };

                let (network, new_id) = match (
                    network
                        .as_ref()
                        .expect("network_to_join_into points to placeholder fluidsystem!")
                        .state,
                    connected_storages_fluid,
                ) {
                    (FluidSystemState::NoFluid, None) => {
                        // We still do not have any fluid
                        (network.as_mut().unwrap(), network_to_join_into)
                    },
                    (FluidSystemState::NoFluid, Some(fluid)) => {
                        let mut network = network.take().unwrap();

                        network.set_fluid(fluid);

                        let index = self.fluid_systems_with_fluid_holes[fluid.into_usize()].pop();

                        let fluid_box_positions = network.graph.keys().copied().collect_vec();

                        let index = if let Some(hole_idx) = index {
                            assert!(
                                self.fluid_systems_with_fluid[fluid.into_usize()][hole_idx]
                                    .is_none()
                            );
                            self.fluid_systems_with_fluid[fluid.into_usize()][hole_idx] =
                                Some(network);
                            hole_idx
                        } else {
                            self.fluid_systems_with_fluid[fluid.into_usize()].push(Some(network));
                            self.fluid_systems_with_fluid[fluid.into_usize()].len() - 1
                        };

                        let new_id = FluidSystemId {
                            fluid: Some(fluid),
                            index,
                        };

                        for box_pos in fluid_box_positions {
                            assert_eq!(
                                self.fluid_box_pos_to_network_id
                                    .insert(box_pos, new_id)
                                    .unwrap(),
                                network_to_join_into
                            );
                            debug_assert!(
                                self.fluid_box_pos_to_network_id
                                    .values()
                                    .all(|v| *v != network_to_join_into)
                            );
                        }

                        (
                            self.fluid_systems_with_fluid[fluid.into_usize()][index]
                                .as_mut()
                                .unwrap(),
                            new_id,
                        )
                    },
                    (FluidSystemState::HasFluid { .. }, None) => {
                        // We just keep the fluid
                        (network.as_mut().unwrap(), network_to_join_into)
                    },
                    (FluidSystemState::HasFluid { fluid, .. }, Some(connection_fluid)) => {
                        if fluid == connection_fluid {
                            // We just keep the fluid
                            (network.as_mut().unwrap(), network_to_join_into)
                        } else {
                            return Err(CannotMixFluidsError {
                                items: [fluid, connection_fluid],
                            });
                        }
                    },
                };

                network.add_fluid_box(new_fluid_box_position, fluid_box_capacity, connected_boxes);

                for (dir, fluid, storage, pos, callback) in connected_storages {
                    let weak_index = match dir {
                        FluidConnectionDir::Input => {
                            network.add_input(fluid, new_fluid_box_position, storage, pos)
                        },
                        FluidConnectionDir::Output => {
                            network.add_output(fluid, new_fluid_box_position, storage, pos)
                        },
                    };

                    callback(weak_index);
                }

                new_id
            };

            final_id
        } else {
            // We are not connected to anything, make a new network
            let mut new_network = FluidSystem::new_from_single_fluid_box(
                connected_storages_fluid,
                new_fluid_box_position,
                fluid_box_capacity,
            );

            for (dir, fluid, storage, pos, callback) in connected_storages {
                let weak_index = match dir {
                    FluidConnectionDir::Input => {
                        new_network.add_input(fluid, new_fluid_box_position, storage, pos)
                    },
                    FluidConnectionDir::Output => {
                        new_network.add_output(fluid, new_fluid_box_position, storage, pos)
                    },
                };

                callback(weak_index);
            }

            let index = match connected_storages_fluid {
                Some(fluid) => {
                    let index = self.fluid_systems_with_fluid_holes[fluid.into_usize()].pop();

                    if let Some(hole_idx) = index {
                        assert!(
                            self.fluid_systems_with_fluid[fluid.into_usize()][hole_idx].is_none()
                        );
                        self.fluid_systems_with_fluid[fluid.into_usize()][hole_idx] =
                            Some(new_network);
                        hole_idx
                    } else {
                        self.fluid_systems_with_fluid[fluid.into_usize()].push(Some(new_network));
                        self.fluid_systems_with_fluid[fluid.into_usize()].len() - 1
                    }
                },
                None => {
                    let index = self.empty_fluid_systems_holes.pop();

                    if let Some(hole_idx) = index {
                        assert!(self.empty_fluid_systems[hole_idx].is_none());
                        self.empty_fluid_systems[hole_idx] = Some(new_network);
                        hole_idx
                    } else {
                        self.empty_fluid_systems.push(Some(new_network));
                        self.empty_fluid_systems.len() - 1
                    }
                },
            };

            let new_id: FluidSystemId<ItemIdxType> = FluidSystemId {
                fluid: connected_storages_fluid,
                index,
            };

            match self
                .fluid_box_pos_to_network_id
                .entry(new_fluid_box_position)
            {
                Entry::Occupied(_) => {
                    unreachable!("Two Fluid Boxes at the same position")
                },
                Entry::Vacant(vacant_entry) => {
                    vacant_entry.insert(new_id);
                },
            }

            new_id
        };

        self.fluid_box_pos_to_network_id
            .insert(new_fluid_box_position, id_the_box_ends_up_with);

        #[cfg(debug_assertions)]
        match id_the_box_ends_up_with.fluid {
            Some(fluid) => self.fluid_systems_with_fluid[fluid.into_usize()]
                [id_the_box_ends_up_with.index]
                .as_ref()
                .unwrap()
                .check_consistency(),
            None => self.empty_fluid_systems[id_the_box_ends_up_with.index]
                .as_ref()
                .unwrap()
                .check_consistency(),
        }

        Ok(())
    }

    pub fn remove_fluid_box(&mut self, fluid_box_position: Position) {
        let old_id = self
            .fluid_box_pos_to_network_id
            .remove(&fluid_box_position)
            .unwrap();

        let (new_systems, delete) = match old_id.fluid {
            Some(fluid) => self.fluid_systems_with_fluid[fluid.into_usize()][old_id.index]
                .as_mut()
                .unwrap()
                .remove_fluid_box(fluid_box_position),
            None => self.empty_fluid_systems[old_id.index]
                .as_mut()
                .unwrap()
                .remove_fluid_box(fluid_box_position),
        };

        if delete {
            match old_id.fluid {
                Some(fluid) => {
                    self.fluid_systems_with_fluid_holes[fluid.into_usize()].push(old_id.index);
                    self.fluid_systems_with_fluid[fluid.into_usize()][old_id.index] = None
                },
                None => {
                    self.empty_fluid_systems_holes.push(old_id.index);
                    self.empty_fluid_systems[old_id.index] = None;
                },
            }
        }

        for new_system in new_systems {
            match new_system.state {
                FluidSystemState::NoFluid => {
                    let index = self.empty_fluid_systems_holes.pop();

                    let new_index = if let Some(hole_idx) = index {
                        assert!(self.empty_fluid_systems[hole_idx].is_none());
                        self.empty_fluid_systems[hole_idx] = Some(new_system);
                        hole_idx
                    } else {
                        self.empty_fluid_systems.push(Some(new_system));
                        self.empty_fluid_systems.len() - 1
                    };

                    let new_id = FluidSystemId {
                        fluid: None,
                        index: new_index,
                    };

                    for pos in self.empty_fluid_systems[new_index]
                        .as_ref()
                        .unwrap()
                        .graph
                        .keys()
                    {
                        let old = self.fluid_box_pos_to_network_id.insert(*pos, new_id);
                        assert_eq!(old, Some(old_id));
                    }
                },
                FluidSystemState::HasFluid { fluid } => {
                    let index = self.fluid_systems_with_fluid_holes[fluid.into_usize()].pop();

                    let new_index = if let Some(hole_idx) = index {
                        assert!(
                            self.fluid_systems_with_fluid[fluid.into_usize()][hole_idx].is_none()
                        );
                        self.fluid_systems_with_fluid[fluid.into_usize()][hole_idx] =
                            Some(new_system);
                        hole_idx
                    } else {
                        self.fluid_systems_with_fluid[fluid.into_usize()].push(Some(new_system));
                        self.fluid_systems_with_fluid[fluid.into_usize()].len() - 1
                    };

                    let new_id = FluidSystemId {
                        fluid: Some(fluid),
                        index: new_index,
                    };

                    for pos in self.fluid_systems_with_fluid[fluid.into_usize()][new_index]
                        .as_ref()
                        .unwrap()
                        .graph
                        .keys()
                    {
                        let old = self.fluid_box_pos_to_network_id.insert(*pos, new_id);
                        assert_eq!(old, Some(old_id));
                    }
                },
            }
        }
    }

    pub fn remove_fluid_box_connection_if_exists<RecipeIdxType: IdxTrait>(
        &mut self,
        first_fluid_box_position: Position,
        second_fluid_box_position: Position,
    ) {
        let old_id = *self
            .fluid_box_pos_to_network_id
            .get(&first_fluid_box_position)
            .unwrap();

        let (new_systems, delete) = match old_id.fluid {
            Some(fluid) => self.fluid_systems_with_fluid[fluid.into_usize()][old_id.index]
                .as_mut()
                .unwrap()
                .remove_fluid_box_connection(first_fluid_box_position, second_fluid_box_position),
            None => self.empty_fluid_systems[old_id.index]
                .as_mut()
                .unwrap()
                .remove_fluid_box_connection(first_fluid_box_position, second_fluid_box_position),
        };

        if delete {
            match old_id.fluid {
                Some(fluid) => {
                    self.fluid_systems_with_fluid_holes[fluid.into_usize()].push(old_id.index);
                    self.fluid_systems_with_fluid[fluid.into_usize()][old_id.index] = None
                },
                None => {
                    self.empty_fluid_systems_holes.push(old_id.index);
                    self.empty_fluid_systems[old_id.index] = None;
                },
            }
        }

        for new_system in new_systems {
            match new_system.state {
                FluidSystemState::NoFluid => {
                    let index = self.empty_fluid_systems_holes.pop();

                    let new_index = if let Some(hole_idx) = index {
                        assert!(self.empty_fluid_systems[hole_idx].is_none());
                        self.empty_fluid_systems[hole_idx] = Some(new_system);
                        hole_idx
                    } else {
                        self.empty_fluid_systems.push(Some(new_system));
                        self.empty_fluid_systems.len() - 1
                    };

                    let new_id = FluidSystemId {
                        fluid: None,
                        index: new_index,
                    };

                    for pos in self.empty_fluid_systems[new_index]
                        .as_ref()
                        .unwrap()
                        .graph
                        .keys()
                    {
                        let old = self.fluid_box_pos_to_network_id.insert(*pos, new_id);
                        assert_eq!(old, Some(old_id));
                    }
                },
                FluidSystemState::HasFluid { fluid } => {
                    let index = self.fluid_systems_with_fluid_holes[fluid.into_usize()].pop();

                    let new_index = if let Some(hole_idx) = index {
                        assert!(
                            self.fluid_systems_with_fluid[fluid.into_usize()][hole_idx].is_none()
                        );
                        self.fluid_systems_with_fluid[fluid.into_usize()][hole_idx] =
                            Some(new_system);
                        hole_idx
                    } else {
                        self.fluid_systems_with_fluid[fluid.into_usize()].push(Some(new_system));
                        self.fluid_systems_with_fluid[fluid.into_usize()].len() - 1
                    };

                    let new_id = FluidSystemId {
                        fluid: Some(fluid),
                        index: new_index,
                    };

                    for pos in self.fluid_systems_with_fluid[fluid.into_usize()][new_index]
                        .as_ref()
                        .unwrap()
                        .graph
                        .keys()
                    {
                        let old = self.fluid_box_pos_to_network_id.insert(*pos, new_id);
                        assert_eq!(old, Some(old_id));
                    }
                },
            }
        }
    }

    pub fn update_fluid_conn_if_needed(
        &mut self,
        fluid_box_position: Position,
        old_storage: FakeUnionStorage,
        new_storage: FakeUnionStorage,
    ) {
        let id = self.fluid_box_pos_to_network_id[&fluid_box_position];

        match id.fluid {
            Some(fluid) => {
                for inc in self.fluid_systems_with_fluid[fluid.into_usize()][id.index]
                    .as_mut()
                    .unwrap()
                    .hot_data
                    .incoming_connections
                    .iter_mut()
                {
                    if *inc == old_storage {
                        *inc = new_storage;
                        log::trace!("Found connection to update");
                    }
                }
                for outgoing in self.fluid_systems_with_fluid[fluid.into_usize()][id.index]
                    .as_mut()
                    .unwrap()
                    .hot_data
                    .outgoing_connections
                    .iter_mut()
                {
                    if *outgoing == old_storage {
                        *outgoing = new_storage;
                        log::trace!("Found connection to update");
                    }
                }
            },
            None => {
                log::trace!("No need to update fluid connections for empty fluid network");
            },
        }
    }

    pub fn try_add_output(
        &mut self,
        fluid_box_position: Position,
        conn_fluid: Item<ItemIdxType>,
        dest: FakeUnionStorage,
        dest_pos: Position,
    ) -> Result<WeakIndex, CannotMixFluidsError<ItemIdxType>> {
        let id = self.fluid_box_pos_to_network_id[&fluid_box_position];

        match id.fluid {
            Some(fluid) => {
                if fluid != conn_fluid {
                    return Err(CannotMixFluidsError {
                        items: [fluid, conn_fluid],
                    });
                }
                let weak_index = self.fluid_systems_with_fluid[fluid.into_usize()][id.index]
                    .as_mut()
                    .unwrap()
                    .add_output(fluid, fluid_box_position, dest, dest_pos);
                Ok(weak_index)
            },
            None => {
                let mut removed = self.empty_fluid_systems[id.index].take().unwrap();
                let removed_id = id;
                removed.set_fluid(conn_fluid);
                let weak_index = removed.add_output(conn_fluid, fluid_box_position, dest, dest_pos);

                let fluid_box_positions = removed.graph.keys().copied().collect_vec();

                let new_idx = self.fluid_systems_with_fluid_holes[conn_fluid.into_usize()].pop();

                let new_idx = if let Some(hole_idx) = new_idx {
                    self.fluid_systems_with_fluid[conn_fluid.into_usize()][hole_idx] =
                        Some(removed);
                    hole_idx
                } else {
                    self.fluid_systems_with_fluid[conn_fluid.into_usize()].push(Some(removed));
                    self.fluid_systems_with_fluid[conn_fluid.into_usize()].len() - 1
                };

                let new_id = FluidSystemId {
                    fluid: Some(conn_fluid),
                    index: new_idx,
                };

                for box_pos in fluid_box_positions {
                    assert_eq!(
                        self.fluid_box_pos_to_network_id
                            .insert(box_pos, new_id)
                            .unwrap(),
                        removed_id
                    );
                }
                debug_assert!(
                    self.fluid_box_pos_to_network_id
                        .values()
                        .all(|v| *v != removed_id)
                );

                Ok(weak_index)
            },
        }
    }

    pub fn try_add_input(
        &mut self,
        fluid_box_position: Position,
        conn_fluid: Item<ItemIdxType>,
        source: FakeUnionStorage,
        source_pos: Position,
    ) -> Result<WeakIndex, CannotMixFluidsError<ItemIdxType>> {
        let id = self.fluid_box_pos_to_network_id[&fluid_box_position];

        match id.fluid {
            Some(fluid) => {
                if fluid != conn_fluid {
                    return Err(CannotMixFluidsError {
                        items: [fluid, conn_fluid],
                    });
                }
                let weak_index = self.fluid_systems_with_fluid[fluid.into_usize()][id.index]
                    .as_mut()
                    .unwrap()
                    .add_input(fluid, fluid_box_position, source, source_pos);
                Ok(weak_index)
            },
            None => {
                let mut removed = self.empty_fluid_systems[id.index].take().unwrap();
                let removed_id = id;
                removed.set_fluid(conn_fluid);
                let weak_index =
                    removed.add_input(conn_fluid, fluid_box_position, source, source_pos);

                let fluid_box_positions = removed.graph.keys().copied().collect_vec();

                let new_idx = self.fluid_systems_with_fluid_holes[conn_fluid.into_usize()].pop();

                let new_idx = if let Some(hole_idx) = new_idx {
                    self.fluid_systems_with_fluid[conn_fluid.into_usize()][hole_idx] =
                        Some(removed);
                    hole_idx
                } else {
                    self.fluid_systems_with_fluid[conn_fluid.into_usize()].push(Some(removed));
                    self.fluid_systems_with_fluid[conn_fluid.into_usize()].len() - 1
                };

                let new_id = FluidSystemId {
                    fluid: Some(conn_fluid),
                    index: new_idx,
                };

                for box_pos in fluid_box_positions {
                    assert_eq!(
                        self.fluid_box_pos_to_network_id
                            .insert(box_pos, new_id)
                            .unwrap(),
                        removed_id
                    );
                }
                debug_assert!(
                    self.fluid_box_pos_to_network_id
                        .values()
                        .all(|v| *v != removed_id)
                );

                Ok(weak_index)
            },
        }
    }

    fn merge_fluid_system(
        &mut self,
        kept_id: FluidSystemId<ItemIdxType>,
        removed_id: FluidSystemId<ItemIdxType>,

        new_fluid_box_position: Position,
        fluid_box_capacity: u32,
        connected_fluid_box_positions: impl IntoIterator<Item = Position>,
    ) {
        if kept_id == removed_id {
            warn!("Tried to merge a fluid system with itself!");
            return;
        }

        match (kept_id.fluid, removed_id.fluid) {
            (None, None) => (),
            (None, Some(_)) => unreachable!(),
            (Some(_), None) => (),
            (Some(a), Some(b)) => assert_eq!(a, b),
        }

        let removed = match removed_id.fluid {
            Some(fluid) => self.fluid_systems_with_fluid[fluid.into_usize()][removed_id.index]
                .take()
                .expect("Tried to merge placeholder"),
            None => self.empty_fluid_systems[removed_id.index]
                .take()
                .expect("Tried to merge placeholder"),
        };

        for box_pos_in_removed in removed.graph.keys() {
            let id_removed_from_map = self
                .fluid_box_pos_to_network_id
                .insert(*box_pos_in_removed, kept_id)
                .unwrap();
            assert_eq!(id_removed_from_map, removed_id);
        }
        debug_assert!(
            self.fluid_box_pos_to_network_id
                .values()
                .all(|v| *v != removed_id)
        );

        match kept_id.fluid {
            Some(fluid) => self.fluid_systems_with_fluid[fluid.into_usize()][kept_id.index]
                .as_mut()
                .unwrap()
                .join(
                    removed,
                    new_fluid_box_position,
                    fluid_box_capacity,
                    connected_fluid_box_positions,
                ),
            None => self.empty_fluid_systems[kept_id.index]
                .as_mut()
                .unwrap()
                .join(
                    removed,
                    new_fluid_box_position,
                    fluid_box_capacity,
                    connected_fluid_box_positions,
                ),
        }
    }
}

#[cfg_attr(feature = "client", derive(ShowInfo), derive(GetSize))]
#[derive(Debug, Clone, serde::Deserialize, serde::Serialize)]
pub enum FluidSystemEntity {
    OutgoingPump { inserter_id: PumpID },
    IncomingPump { inserter_id: PumpID },
    Input { inserter_id: u32 },
    Output { inserter_id: u32 },
}

#[cfg_attr(feature = "client", derive(ShowInfo), derive(GetSize))]
#[derive(Debug, Clone, serde::Deserialize, serde::Serialize)]
pub struct PumpID {
    index: u32,
}

#[cfg_attr(feature = "client", derive(ShowInfo), derive(GetSize))]
#[derive(Debug, Clone, serde::Deserialize, serde::Serialize)]
pub struct FluidBox {
    capacity: u32,
}

#[cfg_attr(feature = "client", derive(ShowInfo), derive(GetSize))]
#[derive(Debug, Clone, Copy, PartialEq, Eq, serde::Deserialize, serde::Serialize)]
pub enum FluidSystemState<ItemIdxType: WeakIdxTrait> {
    NoFluid,
    HasFluid { fluid: Item<ItemIdxType> },
}

#[cfg_attr(feature = "client", derive(ShowInfo), derive(GetSize))]
#[derive(Debug, Clone, serde::Deserialize, serde::Serialize)]
pub struct FluidSystem<ItemIdxType: WeakIdxTrait> {
    pub graph: Network<Position, FluidBox, FluidSystemEntity>,
    pub state: FluidSystemState<ItemIdxType>,
    // TODO: Maybe move the hot_data out of here for better cache efficiency
    pub hot_data: FluidSystemHotData,
}

#[cfg_attr(feature = "client", derive(ShowInfo), derive(GetSize))]
#[derive(Debug, Clone, serde::Deserialize, serde::Serialize)]
pub struct FluidSystemHotData {
    pub storage_capacity: u32,
    current_fluid_level: u32,
    incoming_connections: Box<[FakeUnionStorage]>,
    incoming_start_index: u32,
    outgoing_connections: Box<[FakeUnionStorage]>,
    outgoing_start_index: u32,
}

impl<ItemIdxType: IdxTrait> FluidSystem<ItemIdxType> {
    pub fn trusted_new_without_fluid_box(fluid: Option<Item<ItemIdxType>>) -> Self {
        let mut ret = Self {
            graph: Network::trusted_new_empty(),
            state: FluidSystemState::NoFluid,
            hot_data: FluidSystemHotData {
                storage_capacity: 0,
                current_fluid_level: 0,
                incoming_connections: vec![].into_boxed_slice(),
                incoming_start_index: 0,
                outgoing_connections: vec![].into_boxed_slice(),
                outgoing_start_index: 0,
            },
        };

        if let Some(fluid) = fluid {
            ret.state = FluidSystemState::HasFluid { fluid }
        }

        ret
    }

    #[must_use]
    pub fn new_from_single_fluid_box(
        fluid: Option<Item<ItemIdxType>>,
        fluid_box_position: Position,
        fluid_box_capacity: u32,
    ) -> Self {
        let mut ret = Self {
            graph: Network::new(
                FluidBox {
                    capacity: fluid_box_capacity,
                },
                fluid_box_position,
            ),
            state: FluidSystemState::NoFluid,
            hot_data: FluidSystemHotData {
                storage_capacity: fluid_box_capacity,
                current_fluid_level: 0,
                incoming_connections: vec![].into_boxed_slice(),
                incoming_start_index: 0,
                outgoing_connections: vec![].into_boxed_slice(),
                outgoing_start_index: 0,
            },
        };
        if let Some(fluid) = fluid {
            ret.state = FluidSystemState::HasFluid { fluid }
        }

        ret
    }

    fn new_from_graph(
        graph: Network<Position, FluidBox, FluidSystemEntity>,
        old_fluid: Option<Item<ItemIdxType>>,
        fluid_level_to_distribute: &mut u32,
        fluid_capacity_to_distribute: &mut u32,
    ) -> Self {
        let new_capacity = graph
            .nodes()
            .into_iter()
            .map(|fluid_box| fluid_box.capacity)
            .sum::<u32>();

        // TODO: Ensure this does not wrap
        let our_share_of_fluid = u16::try_from(
            u32::from(*fluid_level_to_distribute) * new_capacity / *fluid_capacity_to_distribute,
        )
        .unwrap();

        // Clamp the fluid in this network to the capacity
        let fluid_left_for_us =
            u32::try_from(min(our_share_of_fluid.into(), new_capacity)).unwrap();
        *fluid_level_to_distribute -= fluid_left_for_us;
        *fluid_capacity_to_distribute -= new_capacity;

        let new_fluid =
            if fluid_left_for_us > 0 || graph.weak_components().into_iter().next().is_some() {
                old_fluid
            } else {
                None
            };

        Self {
            graph,
            state: match new_fluid {
                Some(fluid) => FluidSystemState::HasFluid { fluid },
                None => FluidSystemState::NoFluid,
            },
            hot_data: FluidSystemHotData {
                storage_capacity: new_capacity,
                current_fluid_level: fluid_left_for_us,
                incoming_connections: vec![].into_boxed_slice(),
                incoming_start_index: 0,
                outgoing_connections: vec![].into_boxed_slice(),
                outgoing_start_index: 0,
            },
        }
    }

    fn set_fluid(&mut self, fluid: Item<ItemIdxType>) {
        if let FluidSystemState::HasFluid {
            fluid: our_fluid, ..
        } = self.state
        {
            assert_eq!(our_fluid, fluid);
            return;
        } else {
            self.state = FluidSystemState::HasFluid { fluid };
        }
    }

    pub fn get_fluid(&self) -> Option<Item<ItemIdxType>> {
        match self.state {
            FluidSystemState::NoFluid => None,
            FluidSystemState::HasFluid { fluid, .. } => Some(fluid),
        }
    }

    pub fn get_capacity(&self) -> u32 {
        self.hot_data.storage_capacity
    }

    fn add_output(
        &mut self,
        _fluid: Item<ItemIdxType>,
        source_pipe_position: Position,
        dest: FakeUnionStorage,
        _dest_pos: Position,
    ) -> WeakIndex {
        let id = {
            // This is take_mut but instead of aborting we have emptied the vec
            let mut outgoing = std::mem::take(&mut self.hot_data.outgoing_connections).into_vec();
            outgoing.push(dest);
            self.hot_data.outgoing_connections = outgoing.into_boxed_slice();
            self.hot_data.outgoing_connections.len() - 1
        };

        let weak_index = self.graph.add_weak_element(
            source_pipe_position,
            FluidSystemEntity::Output {
                inserter_id: id.try_into().unwrap(),
            },
        );

        weak_index
    }

    fn add_input(
        &mut self,
        _fluid: Item<ItemIdxType>,
        dest_pipe_position: Position,
        source: FakeUnionStorage,
        _source_pos: Position,
    ) -> WeakIndex {
        let id = {
            // This is take_mut but instead of aborting we have emptied the vec
            let mut incoming = std::mem::take(&mut self.hot_data.incoming_connections).into_vec();
            incoming.push(source);
            self.hot_data.incoming_connections = incoming.into_boxed_slice();
            self.hot_data.incoming_connections.len() - 1
        };

        let weak_index = self.graph.add_weak_element(
            dest_pipe_position,
            FluidSystemEntity::Input {
                inserter_id: id.try_into().unwrap(),
            },
        );

        weak_index
    }

    fn add_pump<RecipeIdxType: IdxTrait>(
        &mut self,
        // FIXME: For now each pump has to have a filter set. This could be avoided by adding another graph like we do to track
        //        if a belt is sushi.
        pump_filter: Item<ItemIdxType>,
        source_pipe_position: Position,
        dest_fluid_network: &mut Self,
        dest_pipe_position: Position,
        data_store: &DataStore<ItemIdxType, RecipeIdxType>,
    ) -> (WeakIndex, WeakIndex) {
        todo!()
    }

    pub fn remove_output(&mut self, source_pipe_position: Position, weak_index: WeakIndex) {
        let FluidSystemEntity::Output { inserter_id, .. } = self
            .graph
            .remove_weak_element(source_pipe_position, weak_index)
        else {
            unreachable!("Tried to remove output with non output weakindex");
        };

        let removed_inserter_id = inserter_id;

        let mut v = std::mem::take(&mut self.hot_data.incoming_connections).into_vec();
        // TODO: Maybe swap_remove?
        v.remove(removed_inserter_id as usize);
        self.hot_data.incoming_connections = v.into_boxed_slice();
        if self.hot_data.incoming_start_index > removed_inserter_id {
            self.hot_data.incoming_start_index -= 1;
        }

        for weak in self.graph.weak_components_mut() {
            match weak {
                FluidSystemEntity::Output { inserter_id } => {
                    if *inserter_id > removed_inserter_id {
                        // Effect of remove
                        *inserter_id -= 1;
                    }
                },
                FluidSystemEntity::Input { .. }
                | FluidSystemEntity::OutgoingPump { .. }
                | FluidSystemEntity::IncomingPump { .. } => {},
            }
        }
    }

    pub fn remove_input(&mut self, dest_pipe_position: Position, weak_index: WeakIndex) {
        let FluidSystemEntity::Input { inserter_id, .. } = self
            .graph
            .remove_weak_element(dest_pipe_position, weak_index)
        else {
            unreachable!("Tried to remove output with non input weakindex");
        };

        let removed_inserter_id = inserter_id;

        let mut v = std::mem::take(&mut self.hot_data.outgoing_connections).into_vec();
        // TODO: Maybe swap_remove?
        v.remove(removed_inserter_id as usize);
        self.hot_data.outgoing_connections = v.into_boxed_slice();
        if self.hot_data.outgoing_start_index > removed_inserter_id {
            self.hot_data.outgoing_start_index -= 1;
        }

        for weak in self.graph.weak_components_mut() {
            match weak {
                FluidSystemEntity::Input { inserter_id } => {
                    if *inserter_id > removed_inserter_id {
                        // Effect of remove
                        *inserter_id -= 1;
                    }
                },
                FluidSystemEntity::Output { .. }
                | FluidSystemEntity::OutgoingPump { .. }
                | FluidSystemEntity::IncomingPump { .. } => {},
            }
        }
    }

    fn add_fluid_box_trusted(
        &mut self,
        fluid_box_position: Position,
        fluid_box_capacity: u32,
        fluid_box_connections: impl IntoIterator<Item = Position>,
    ) {
        self.hot_data.storage_capacity = self
            .hot_data
            .storage_capacity
            .checked_add(fluid_box_capacity)
            .expect("TODO: Fluid network size exceeded u32::MAX");

        self.graph.add_node_trusted(
            FluidBox {
                capacity: fluid_box_capacity,
            },
            fluid_box_position,
            fluid_box_connections,
        );
    }

    fn add_fluid_box(
        &mut self,
        fluid_box_position: Position,
        fluid_box_capacity: u32,
        fluid_box_connections: impl IntoIterator<Item = Position>,
    ) {
        let mut fluid_box_connections = fluid_box_connections.into_iter();
        self.hot_data.storage_capacity = self
            .hot_data
            .storage_capacity
            .checked_add(fluid_box_capacity)
            .expect("TODO: Fluid network size exceeded u32::MAX");

        self.graph.add_node(
            FluidBox {
                capacity: fluid_box_capacity,
            },
            fluid_box_position,
            (fluid_box_connections.next().unwrap(), fluid_box_connections),
        );
    }

    fn join(
        &mut self,
        mut other: Self,
        new_fluid_box_position: Position,
        fluid_box_capacity: u32,
        connected_fluid_box_positions: impl IntoIterator<Item = Position>,
    ) {
        self.check_consistency();
        other.check_consistency();

        if !self
            .graph
            .keys()
            .into_iter()
            .any(|pos| *pos == new_fluid_box_position)
        {
            self.hot_data.storage_capacity += fluid_box_capacity;
        }
        self.hot_data.storage_capacity = self
            .hot_data
            .storage_capacity
            .checked_add(other.hot_data.storage_capacity)
            .expect("TODO: Fluid network size exceeded u32::MAX");

        let mut connected_fluid_box_positions = connected_fluid_box_positions.into_iter();

        match (self.state, other.state) {
            (FluidSystemState::NoFluid, FluidSystemState::NoFluid) => {
                // We do not have any fluid, so no chest and no inserters
                debug_assert!(
                    self.graph
                        .weak_components()
                        .into_iter()
                        .all(|conn| match conn {
                            FluidSystemEntity::OutgoingPump { .. } => true,
                            FluidSystemEntity::IncomingPump { .. } => true,
                            // Since input/outputs are always a specific fluid, we cannot have any (otherwise we shoudl already have a fluid)
                            FluidSystemEntity::Input { .. } => false,
                            FluidSystemEntity::Output { .. } => false,
                        })
                );
                debug_assert!(
                    other
                        .graph
                        .weak_components()
                        .into_iter()
                        .all(|conn| match conn {
                            FluidSystemEntity::OutgoingPump { .. } => true,
                            FluidSystemEntity::IncomingPump { .. } => true,
                            // Since input/outputs are always a specific fluid, we cannot have any (otherwise we shoudl already have a fluid)
                            FluidSystemEntity::Input { .. } => false,
                            FluidSystemEntity::Output { .. } => false,
                        })
                );
            },
            (FluidSystemState::NoFluid, FluidSystemState::HasFluid { fluid }) => {
                self.state = FluidSystemState::HasFluid { fluid }
            },
            (FluidSystemState::HasFluid { .. }, FluidSystemState::NoFluid) => {},
            (
                FluidSystemState::HasFluid { fluid: fluid_a },
                FluidSystemState::HasFluid { fluid: fluid_b },
            ) => {
                assert_eq!(fluid_a, fluid_b);

                let removed_fluid = other.hot_data.current_fluid_level;

                self.hot_data.current_fluid_level += removed_fluid;

                for conn in other.graph.weak_components_mut() {
                    match conn {
                        FluidSystemEntity::OutgoingPump { .. } => {
                            todo!()
                        },
                        FluidSystemEntity::IncomingPump { .. } => {
                            todo!()
                        },
                        FluidSystemEntity::Input { inserter_id, .. } => {
                            // TODO: This code is terrible and O(n^2) beacuse of reallocations!
                            let fake = other.hot_data.incoming_connections[*inserter_id as usize];
                            let id = {
                                // This is take_mut but instead of aborting we have emptied the vec
                                let mut incoming =
                                    std::mem::take(&mut self.hot_data.incoming_connections)
                                        .into_vec();
                                incoming.push(fake);
                                self.hot_data.incoming_connections = incoming.into_boxed_slice();
                                self.hot_data.incoming_connections.len() - 1
                            };
                            *inserter_id = id.try_into().unwrap();
                        },
                        FluidSystemEntity::Output { inserter_id, .. } => {
                            // TODO: This code is terrible and O(n^2) beacuse of reallocations!
                            let fake = other.hot_data.outgoing_connections[*inserter_id as usize];
                            let id = {
                                // This is take_mut but instead of aborting we have emptied the vec
                                let mut outgoing =
                                    std::mem::take(&mut self.hot_data.outgoing_connections)
                                        .into_vec();
                                outgoing.push(fake);
                                self.hot_data.outgoing_connections = outgoing.into_boxed_slice();
                                self.hot_data.outgoing_connections.len() - 1
                            };
                            *inserter_id = id.try_into().unwrap();
                        },
                    }
                }
            },
        }

        self.graph.add_node_merging(
            FluidBox {
                capacity: fluid_box_capacity,
            },
            new_fluid_box_position,
            (
                connected_fluid_box_positions.next().unwrap(),
                connected_fluid_box_positions,
            ),
            other.graph,
        );
    }

    fn remove_fluid_box(
        &mut self,
        fluid_box_position: Position,
    ) -> (impl Iterator<Item = Self> + use<ItemIdxType>, bool) {
        let old_fluid = self.get_fluid();

        let (removed_fluid_box, connections_to_remove, new_graphs) =
            self.graph.remove_node(fluid_box_position);

        let new_graphs = new_graphs.into_iter().flatten().collect_vec();

        let old_fluid_level = self.hot_data.current_fluid_level;
        let mut fluid_distribution = old_fluid_level;

        for (weak_index, connection_to_remove) in connections_to_remove.into_iter().collect_vec() {
            let _fluid = old_fluid.expect("If we have any connections we MUST have a fluid set");
            match connection_to_remove {
                FluidSystemEntity::OutgoingPump { .. } => {
                    todo!()
                },
                FluidSystemEntity::IncomingPump { .. } => {
                    todo!()
                },
                FluidSystemEntity::Input { .. } => {
                    self.remove_input(fluid_box_position, weak_index);
                },
                FluidSystemEntity::Output { .. } => {
                    self.remove_output(fluid_box_position, weak_index);
                },
            }
        }

        let new_grids: Vec<_> = new_graphs
            .into_iter()
            .map(|(graph, _positions)| {
                let mut new_system = Self::new_from_graph(
                    graph,
                    old_fluid,
                    &mut fluid_distribution,
                    &mut self.hot_data.storage_capacity,
                );

                match new_system.state {
                    FluidSystemState::NoFluid => {},
                    FluidSystemState::HasFluid { fluid: _ } => {
                        for connection in new_system.graph.weak_components_mut() {
                            match connection {
                                FluidSystemEntity::OutgoingPump { .. } => {
                                    todo!()
                                },
                                FluidSystemEntity::IncomingPump { .. } => {
                                    todo!()
                                },
                                FluidSystemEntity::Input { .. } => {
                                    todo!()
                                },
                                FluidSystemEntity::Output { .. } => {
                                    todo!()
                                },
                            }
                        }
                    },
                }

                new_system
            })
            .collect();

        self.hot_data.storage_capacity -= removed_fluid_box.capacity;

        let fluid_left_for_us = u32::try_from(min(
            fluid_distribution.into(),
            self.hot_data.storage_capacity,
        ))
        .unwrap();

        if self.graph.nodes().into_iter().next().is_none() {
            // We no longer exist
            assert!(self.hot_data.storage_capacity == 0);
            assert!(fluid_left_for_us == 0);
            return (new_grids.into_iter(), true);
        }

        match self.state {
            FluidSystemState::NoFluid => {},
            FluidSystemState::HasFluid { fluid } => {
                // chest_store.stores[fluid.into_usize()]
                //     .remove_items_from_chest(chest_id, old_fluid_level - fluid_left_for_us)
                //     .expect("Not enough items in fluid system chest");
                // assert!(fluid_left_for_us <= self.hot_data.try_into().unwrap());
                // let 0 = chest_store.stores[fluid.into_usize()]
                //     .change_chest_size(chest_id, self.storage_capacity.try_into().unwrap())
                // else {
                //     unreachable!();
                // };
                todo!()
            },
        }

        (new_grids.into_iter(), false)
    }

    fn remove_fluid_box_connection(
        &mut self,
        first_fluid_box_position: Position,
        second_fluid_box_position: Position,
    ) -> (impl Iterator<Item = Self> + use<ItemIdxType>, bool) {
        let old_fluid = self.get_fluid();

        let new_graphs = self
            .graph
            .remove_edge(first_fluid_box_position, second_fluid_box_position);

        let old_fluid_level = self.hot_data.current_fluid_level;
        let mut fluid_distribution = old_fluid_level;

        let new_grids: Vec<_> = new_graphs
            .into_iter()
            .flatten()
            .map(|(graph, _positions)| {
                let mut new_system = Self::new_from_graph(
                    graph,
                    old_fluid,
                    &mut fluid_distribution,
                    &mut self.hot_data.storage_capacity,
                );

                match new_system.state {
                    FluidSystemState::NoFluid => {},
                    FluidSystemState::HasFluid { fluid } => {
                        for connection in new_system.graph.weak_components_mut() {
                            match connection {
                                FluidSystemEntity::OutgoingPump { .. } => {
                                    todo!()
                                },
                                FluidSystemEntity::IncomingPump { .. } => {
                                    todo!()
                                },
                                FluidSystemEntity::Input { .. } => {
                                    todo!()
                                },
                                FluidSystemEntity::Output { .. } => {
                                    todo!()
                                },
                            }
                        }
                    },
                }

                new_system
            })
            .collect();

        let fluid_left_for_us = u32::try_from(min(
            fluid_distribution.into(),
            self.hot_data.storage_capacity,
        ))
        .unwrap();

        if self.graph.nodes().into_iter().next().is_none() {
            // We no longer exist
            assert!(self.hot_data.storage_capacity == 0);
            assert!(fluid_left_for_us == 0);
            return (new_grids.into_iter(), true);
        }

        match self.state {
            FluidSystemState::NoFluid => {},
            FluidSystemState::HasFluid { fluid: _ } => {
                assert!(fluid_left_for_us <= self.hot_data.storage_capacity.try_into().unwrap());
                self.hot_data.current_fluid_level = fluid_left_for_us;
            },
        }

        (new_grids.into_iter(), false)
    }

    pub fn check_consistency(&self) {
        let calculated_capacity: u32 = self
            .graph
            .nodes()
            .into_iter()
            .map(|node| node.capacity)
            .sum();

        assert_eq!(self.hot_data.storage_capacity, calculated_capacity);
    }
}

pub fn update_fluid_system(
    item_id: usize,
    hot_data: &mut FluidSystemHotData,
    storages: SingleItemStorages,
    grid_size: usize,
) {
    // Do outgoing first
    let mut i = hot_data.outgoing_start_index;
    for &outgoing_conn in hot_data.outgoing_connections[(hot_data.outgoing_start_index as usize)..]
        .iter()
        .chain(hot_data.outgoing_connections[..(hot_data.outgoing_start_index as usize)].iter())
    {
        if hot_data.current_fluid_level == 0 {
            break;
        }
        let (max, data, _) = index_fake_union(Some(item_id), storages, outgoing_conn, grid_size);
        let amount_wanted = *max - *data;

        let amount_extracted = min(
            amount_wanted,
            u8::try_from(hot_data.current_fluid_level).unwrap_or(u8::MAX),
        );

        *data += amount_extracted;
        hot_data.current_fluid_level -= u32::from(amount_extracted);
        i += 1;
    }
    hot_data.outgoing_start_index = if i > hot_data.outgoing_connections.len() as u32 {
        i - hot_data.outgoing_connections.len() as u32
    } else {
        i
    };

    let mut i = hot_data.incoming_start_index;
    for &incoming_conn in hot_data.incoming_connections[(hot_data.incoming_start_index as usize)..]
        .iter()
        .chain(hot_data.incoming_connections[..(hot_data.incoming_start_index as usize)].iter())
    {
        if hot_data.current_fluid_level == hot_data.storage_capacity {
            break;
        }
        let (_max, data, _) = index_fake_union(Some(item_id), storages, incoming_conn, grid_size);
        let amount_wanted = *data;

        let amount_extracted = min(
            amount_wanted,
            u8::try_from(hot_data.storage_capacity - hot_data.current_fluid_level)
                .unwrap_or(u8::MAX),
        );

        *data -= amount_extracted;
        hot_data.current_fluid_level += u32::from(amount_extracted);
        i += 1;
    }
    hot_data.incoming_start_index = if i > hot_data.incoming_connections.len() as u32 {
        i - hot_data.incoming_connections.len() as u32
    } else {
        i
    };
}
