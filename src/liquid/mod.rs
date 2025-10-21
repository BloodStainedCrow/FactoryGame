use std::cmp::min;
use std::collections::HashMap;
use std::collections::hash_map::Entry;

use itertools::Itertools;
use log::warn;

use crate::chest::ChestSize;
use crate::inserter::FakeUnionStorage;
use crate::inserter::storage_storage_with_buckets_indirect::InserterIdentifier;
use crate::item::Indexable;
use crate::{
    app_state::StorageStorageInserterStore,
    chest::FullChestStore,
    data::DataStore,
    frontend::world::Position,
    inserter::{StaticID, Storage},
    item::{IdxTrait, Item, WeakIdxTrait},
    network_graph::{Network, WeakIndex},
};

#[cfg(feature = "client")]
use egui_show_info_derive::ShowInfo;
#[cfg(feature = "client")]
use get_size::GetSize;

pub mod connection_logic;

const FLUID_INSERTER_MOVETIME: u16 = 1;

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
        chest_store: &mut FullChestStore<ItemIdxType>,
    ) -> FluidSystemId<ItemIdxType> {
        let new_network = FluidSystem::trusted_new_without_fluid_box(fluid, chest_store);

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

    pub fn trusted_add_fluid_box<RecipeIdxType: IdxTrait>(
        &mut self,

        fluid_network_id: FluidSystemId<ItemIdxType>,

        new_fluid_box_position: Position,
        fluid_box_capacity: u32,

        connected_fluid_box_positions: impl IntoIterator<Item = Position>,
        connected_storages: impl IntoIterator<
            Item = (
                FluidConnectionDir,
                Item<ItemIdxType>,
                Storage<RecipeIdxType>,
                Position,
            ),
        > + Clone,

        chest_store: &mut FullChestStore<ItemIdxType>,
        inserter_store: &mut StorageStorageInserterStore,

        data_store: &DataStore<ItemIdxType, RecipeIdxType>,
    ) {
        self.fluid_box_pos_to_network_id
            .insert(new_fluid_box_position, fluid_network_id);

        let network = self.get_fluid_network_mut(fluid_network_id);

        network.add_fluid_box_trusted(
            new_fluid_box_position,
            fluid_box_capacity,
            connected_fluid_box_positions,
            chest_store,
        );

        for (dir, fluid, storage, pos) in connected_storages {
            assert_eq!(fluid, fluid_network_id.fluid.unwrap());

            let weak_index = match dir {
                FluidConnectionDir::Input => network.add_input(
                    fluid,
                    new_fluid_box_position,
                    storage,
                    pos,
                    inserter_store,
                    data_store,
                ),
                FluidConnectionDir::Output => network.add_output(
                    fluid,
                    new_fluid_box_position,
                    storage,
                    pos,
                    inserter_store,
                    data_store,
                ),
            };
        }
    }

    /// # Errors:
    /// If adding this fluid box would connect fluid systems with different fluids
    pub fn try_add_fluid_box<RecipeIdxType: IdxTrait>(
        &mut self,
        new_fluid_box_position: Position,
        fluid_box_capacity: u32,

        connected_fluid_box_positions: impl IntoIterator<Item = Position>,
        connected_storages: impl IntoIterator<
            Item = (
                FluidConnectionDir,
                Item<ItemIdxType>,
                Storage<RecipeIdxType>,
                Position,
                Box<dyn FnOnce(WeakIndex) -> ()>,
            ),
        > + Clone,

        chest_store: &mut FullChestStore<ItemIdxType>,
        inserter_store: &mut StorageStorageInserterStore,

        data_store: &DataStore<ItemIdxType, RecipeIdxType>,
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
                        chest_store,
                        inserter_store,
                        new_fluid_box_position,
                        fluid_box_capacity,
                        connected_boxes.iter().copied(),
                        data_store,
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

                        network.set_fluid(fluid, chest_store);

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

                network.add_fluid_box(
                    new_fluid_box_position,
                    fluid_box_capacity,
                    connected_boxes,
                    chest_store,
                );

                for (dir, fluid, storage, pos, callback) in connected_storages {
                    let weak_index = match dir {
                        FluidConnectionDir::Input => network.add_input(
                            fluid,
                            new_fluid_box_position,
                            storage,
                            pos,
                            inserter_store,
                            data_store,
                        ),
                        FluidConnectionDir::Output => network.add_output(
                            fluid,
                            new_fluid_box_position,
                            storage,
                            pos,
                            inserter_store,
                            data_store,
                        ),
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
                chest_store,
            );

            for (dir, fluid, storage, pos, callback) in connected_storages {
                let weak_index = match dir {
                    FluidConnectionDir::Input => new_network.add_input(
                        fluid,
                        new_fluid_box_position,
                        storage,
                        pos,
                        inserter_store,
                        data_store,
                    ),
                    FluidConnectionDir::Output => new_network.add_output(
                        fluid,
                        new_fluid_box_position,
                        storage,
                        pos,
                        inserter_store,
                        data_store,
                    ),
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
                .check_consistency(chest_store),
            None => self.empty_fluid_systems[id_the_box_ends_up_with.index]
                .as_ref()
                .unwrap()
                .check_consistency(chest_store),
        }

        Ok(())
    }

    pub fn remove_fluid_box<RecipeIdxType: IdxTrait>(
        &mut self,
        fluid_box_position: Position,
        chest_store: &mut FullChestStore<ItemIdxType>,
        inserter_store: &mut StorageStorageInserterStore,
        data_store: &DataStore<ItemIdxType, RecipeIdxType>,
    ) {
        let old_id = self
            .fluid_box_pos_to_network_id
            .remove(&fluid_box_position)
            .unwrap();

        let (new_systems, delete) = match old_id.fluid {
            Some(fluid) => self.fluid_systems_with_fluid[fluid.into_usize()][old_id.index]
                .as_mut()
                .unwrap()
                .remove_fluid_box(fluid_box_position, chest_store, inserter_store, data_store),
            None => self.empty_fluid_systems[old_id.index]
                .as_mut()
                .unwrap()
                .remove_fluid_box(fluid_box_position, chest_store, inserter_store, data_store),
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
                FluidSystemState::HasFluid { fluid, chest_id: _ } => {
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
        chest_store: &mut FullChestStore<ItemIdxType>,
        inserter_store: &mut StorageStorageInserterStore,
        data_store: &DataStore<ItemIdxType, RecipeIdxType>,
    ) {
        let old_id = *self
            .fluid_box_pos_to_network_id
            .get(&first_fluid_box_position)
            .unwrap();

        let (new_systems, delete) = match old_id.fluid {
            Some(fluid) => self.fluid_systems_with_fluid[fluid.into_usize()][old_id.index]
                .as_mut()
                .unwrap()
                .remove_fluid_box_connection(
                    first_fluid_box_position,
                    second_fluid_box_position,
                    chest_store,
                    inserter_store,
                    data_store,
                ),
            None => self.empty_fluid_systems[old_id.index]
                .as_mut()
                .unwrap()
                .remove_fluid_box_connection(
                    first_fluid_box_position,
                    second_fluid_box_position,
                    chest_store,
                    inserter_store,
                    data_store,
                ),
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
                FluidSystemState::HasFluid { fluid, chest_id: _ } => {
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

    pub fn update_fluid_conn_if_needed<RecipeIdxType: IdxTrait>(
        &mut self,
        fluid_box_position: Position,
        old_storage: FakeUnionStorage,
        new_storage: Storage<RecipeIdxType>,
        inserter_store: &mut StorageStorageInserterStore,
        data_store: &DataStore<ItemIdxType, RecipeIdxType>,
    ) {
        let id = self.fluid_box_pos_to_network_id[&fluid_box_position];

        match id.fluid {
            Some(fluid) => {
                self.fluid_systems_with_fluid[fluid.into_usize()][id.index]
                    .as_mut()
                    .unwrap()
                    .update_fluid_conn_if_needed(
                        fluid,
                        old_storage,
                        new_storage,
                        inserter_store,
                        data_store,
                    );
            },
            None => {},
        }
    }

    pub fn try_add_output<RecipeIdxType: IdxTrait>(
        &mut self,
        fluid_box_position: Position,
        conn_fluid: Item<ItemIdxType>,
        dest: Storage<RecipeIdxType>,
        dest_pos: Position,
        chest_store: &mut FullChestStore<ItemIdxType>,
        inserter_store: &mut StorageStorageInserterStore,
        data_store: &DataStore<ItemIdxType, RecipeIdxType>,
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
                    .add_output(
                        fluid,
                        fluid_box_position,
                        dest,
                        dest_pos,
                        inserter_store,
                        data_store,
                    );
                Ok(weak_index)
            },
            None => {
                let mut removed = self.empty_fluid_systems[id.index].take().unwrap();
                let removed_id = id;
                removed.set_fluid(conn_fluid, chest_store);
                let weak_index = removed.add_output(
                    conn_fluid,
                    fluid_box_position,
                    dest,
                    dest_pos,
                    inserter_store,
                    data_store,
                );

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

    pub fn try_add_input<RecipeIdxType: IdxTrait>(
        &mut self,
        fluid_box_position: Position,
        conn_fluid: Item<ItemIdxType>,
        source: Storage<RecipeIdxType>,
        source_pos: Position,
        chest_store: &mut FullChestStore<ItemIdxType>,
        inserter_store: &mut StorageStorageInserterStore,
        data_store: &DataStore<ItemIdxType, RecipeIdxType>,
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
                    .add_input(
                        fluid,
                        fluid_box_position,
                        source,
                        source_pos,
                        inserter_store,
                        data_store,
                    );
                Ok(weak_index)
            },
            None => {
                let mut removed = self.empty_fluid_systems[id.index].take().unwrap();
                let removed_id = id;
                removed.set_fluid(conn_fluid, chest_store);
                let weak_index = removed.add_input(
                    conn_fluid,
                    fluid_box_position,
                    source,
                    source_pos,
                    inserter_store,
                    data_store,
                );

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

    fn merge_fluid_system<RecipeIdxType: IdxTrait>(
        &mut self,
        kept_id: FluidSystemId<ItemIdxType>,
        removed_id: FluidSystemId<ItemIdxType>,

        chest_store: &mut FullChestStore<ItemIdxType>,
        inserter_store: &mut StorageStorageInserterStore,

        new_fluid_box_position: Position,
        fluid_box_capacity: u32,
        connected_fluid_box_positions: impl IntoIterator<Item = Position>,

        data_store: &DataStore<ItemIdxType, RecipeIdxType>,
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
                    chest_store,
                    inserter_store,
                    data_store,
                ),
            None => self.empty_fluid_systems[kept_id.index]
                .as_mut()
                .unwrap()
                .join(
                    removed,
                    new_fluid_box_position,
                    fluid_box_capacity,
                    connected_fluid_box_positions,
                    chest_store,
                    inserter_store,
                    data_store,
                ),
        }
    }
}

#[cfg_attr(feature = "client", derive(ShowInfo), derive(GetSize))]
#[derive(Debug, Clone, serde::Deserialize, serde::Serialize)]
pub enum FluidSystemEntity {
    OutgoingPump { inserter_id: InserterIdentifier },
    IncomingPump { inserter_id: InserterIdentifier },
    Input { inserter_id: InserterIdentifier },
    Output { inserter_id: InserterIdentifier },
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
    HasFluid {
        fluid: Item<ItemIdxType>,
        chest_id: u32,
    },
}

#[cfg_attr(feature = "client", derive(ShowInfo), derive(GetSize))]
#[derive(Debug, Clone, serde::Deserialize, serde::Serialize)]
pub struct FluidSystem<ItemIdxType: WeakIdxTrait> {
    pub graph: Network<Position, FluidBox, FluidSystemEntity>,
    pub storage_capacity: u32,
    pub state: FluidSystemState<ItemIdxType>,
}

impl<ItemIdxType: IdxTrait> FluidSystem<ItemIdxType> {
    pub fn trusted_new_without_fluid_box(
        fluid: Option<Item<ItemIdxType>>,
        chest_store: &mut FullChestStore<ItemIdxType>,
    ) -> Self {
        let mut ret = Self {
            graph: Network::trusted_new_empty(),
            storage_capacity: 0,
            state: FluidSystemState::NoFluid,
        };

        if let Some(fluid) = fluid {
            ret.state = FluidSystemState::HasFluid {
                fluid,
                chest_id: chest_store.stores[fluid.into_usize()].add_custom_chest(0),
            }
        }

        ret
    }

    #[must_use]
    pub fn new_from_single_fluid_box(
        fluid: Option<Item<ItemIdxType>>,
        fluid_box_position: Position,
        fluid_box_capacity: u32,
        chest_store: &mut FullChestStore<ItemIdxType>,
    ) -> Self {
        let mut ret = Self {
            graph: Network::new(
                FluidBox {
                    capacity: fluid_box_capacity,
                },
                fluid_box_position,
            ),
            storage_capacity: fluid_box_capacity,
            state: FluidSystemState::NoFluid,
        };
        if let Some(fluid) = fluid {
            ret.state = FluidSystemState::HasFluid {
                fluid,
                // TODO: Use fluid atoms to increase max size
                chest_id: chest_store.stores[fluid.into_usize()].add_custom_chest(
                    fluid_box_capacity
                        .try_into()
                        .expect("Fluid system too large"),
                ),
            }
        }

        ret
    }

    pub fn update_fluid_conn_if_needed<RecipeIdxType: IdxTrait>(
        &mut self,
        fluid: Item<ItemIdxType>,
        old_storage: FakeUnionStorage,
        new_storage: Storage<RecipeIdxType>,
        inserter_store: &mut StorageStorageInserterStore,
        data_store: &DataStore<ItemIdxType, RecipeIdxType>,
    ) {
        todo!();
        for e in self.graph.weak_components_mut() {
            // match e {
            //     FluidSystemEntity::OutgoingPump { .. } => {},
            //     FluidSystemEntity::IncomingPump { .. } => {},
            //     FluidSystemEntity::Input { inserter_id } => {
            //         if old_storage
            //             == inserter_store
            //                 .get_inserter(fluid, FLUID_INSERTER_MOVETIME, *inserter_id, todo!())
            //                 .source
            //         {
            //             *inserter_id = inserter_store.update_inserter_src(
            //                 fluid,
            //                 FLUID_INSERTER_MOVETIME,
            //                 *inserter_id,
            //                 new_storage,
            //                 data_store,
            //             );
            //         }
            //     },
            //     FluidSystemEntity::Output { inserter_id } => {
            //         if old_storage
            //             == inserter_store
            //                 .get_inserter(fluid, FLUID_INSERTER_MOVETIME, *inserter_id, todo!())
            //                 .dest
            //         {
            //             *inserter_id = inserter_store.update_inserter_dest(
            //                 fluid,
            //                 FLUID_INSERTER_MOVETIME,
            //                 *inserter_id,
            //                 new_storage,
            //                 data_store,
            //             );
            //         }
            //     },
            // }
        }
    }

    fn new_from_graph(
        graph: Network<Position, FluidBox, FluidSystemEntity>,
        old_fluid: Option<Item<ItemIdxType>>,
        fluid_level_to_distribute: &mut ChestSize,
        fluid_capacity_to_distribute: &mut u32,
        chest_store: &mut FullChestStore<ItemIdxType>,
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
            ChestSize::try_from(min(our_share_of_fluid.into(), new_capacity)).unwrap();
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
            storage_capacity: new_capacity,
            state: match new_fluid {
                Some(fluid) => {
                    // TODO: Use fluid atoms to increase max size
                    let chest_id = chest_store.stores[fluid.into_usize()]
                        .add_custom_chest(new_capacity.try_into().expect("Fluid system too large"));

                    chest_store.stores[fluid.into_usize()]
                        .add_items_to_chest(chest_id, fluid_left_for_us)
                        .expect(
                            "Adding the contents of two chests (and their sizes) should always fit",
                        );

                    FluidSystemState::HasFluid { fluid, chest_id }
                },
                None => FluidSystemState::NoFluid,
            },
        }
    }

    fn set_fluid(
        &mut self,
        fluid: Item<ItemIdxType>,
        chest_store: &mut FullChestStore<ItemIdxType>,
    ) {
        if let FluidSystemState::HasFluid {
            fluid: our_fluid, ..
        } = self.state
        {
            assert_eq!(our_fluid, fluid);
            return;
        } else {
            let chest_id = chest_store.stores[fluid.into_usize()].add_custom_chest(
                self.storage_capacity
                    .try_into()
                    .expect("Fluid system too large"),
            );
            self.state = FluidSystemState::HasFluid { fluid, chest_id };
        }
    }

    pub fn get_fluid(&self) -> Option<Item<ItemIdxType>> {
        match self.state {
            FluidSystemState::NoFluid => None,
            FluidSystemState::HasFluid { fluid, .. } => Some(fluid),
        }
    }

    fn get_chest_id(&self) -> Option<u32> {
        match self.state {
            FluidSystemState::NoFluid => None,
            FluidSystemState::HasFluid { chest_id, .. } => Some(chest_id),
        }
    }

    fn add_output<RecipeIdxType: IdxTrait>(
        &mut self,
        fluid: Item<ItemIdxType>,
        source_pipe_position: Position,
        dest: Storage<RecipeIdxType>,
        _dest_pos: Position,
        inserter_store: &mut StorageStorageInserterStore,
        data_store: &DataStore<ItemIdxType, RecipeIdxType>,
    ) -> WeakIndex {
        let inserter_id = inserter_store.add_ins(
            fluid,
            FLUID_INSERTER_MOVETIME,
            crate::inserter::Storage::Static {
                static_id: StaticID::Chest as u16,
                index: self.get_chest_id().unwrap(),
            },
            dest,
            50,
            data_store,
        );

        let weak_index = self.graph.add_weak_element(
            source_pipe_position,
            FluidSystemEntity::Output { inserter_id },
        );

        weak_index
    }

    fn add_input<RecipeIdxType: IdxTrait>(
        &mut self,
        fluid: Item<ItemIdxType>,
        dest_pipe_position: Position,
        source: Storage<RecipeIdxType>,
        _source_pos: Position,
        inserter_store: &mut StorageStorageInserterStore,
        data_store: &DataStore<ItemIdxType, RecipeIdxType>,
    ) -> WeakIndex {
        let inserter_id = inserter_store.add_ins(
            fluid,
            FLUID_INSERTER_MOVETIME,
            source,
            crate::inserter::Storage::Static {
                static_id: StaticID::Chest as u16,
                index: self.get_chest_id().unwrap(),
            },
            50,
            data_store,
        );

        let weak_index = self
            .graph
            .add_weak_element(dest_pipe_position, FluidSystemEntity::Input { inserter_id });

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
        inserter_store: &mut StorageStorageInserterStore,
        data_store: &DataStore<ItemIdxType, RecipeIdxType>,
    ) -> (WeakIndex, WeakIndex) {
        let inserter_id = inserter_store.add_ins(
            self.get_fluid().unwrap(),
            FLUID_INSERTER_MOVETIME,
            crate::inserter::Storage::Static {
                static_id: StaticID::Chest as u16,
                index: self.get_chest_id().unwrap(),
            },
            crate::inserter::Storage::Static {
                static_id: StaticID::Chest as u16,
                index: dest_fluid_network.get_chest_id().unwrap(),
            },
            50,
            data_store,
        );

        let weak_index_self = self.graph.add_weak_element(
            source_pipe_position,
            FluidSystemEntity::OutgoingPump { inserter_id },
        );

        let weak_index_dest = self.graph.add_weak_element(
            dest_pipe_position,
            FluidSystemEntity::IncomingPump { inserter_id },
        );

        (weak_index_self, weak_index_dest)
    }

    pub fn remove_output(
        &mut self,
        source_pipe_position: Position,
        weak_index: WeakIndex,
        inserter_store: &mut StorageStorageInserterStore,
    ) {
        let FluidSystemEntity::Output { inserter_id, .. } = self
            .graph
            .remove_weak_element(source_pipe_position, weak_index)
        else {
            unreachable!("Tried to remove output with non output weakindex");
        };

        let _inserter_removal_info: () = inserter_store.remove_ins(
            self.get_fluid()
                .expect("Fluid Networks without a set fluid cannot have outputs"),
            FLUID_INSERTER_MOVETIME,
            inserter_id,
        );
    }

    pub fn remove_input(
        &mut self,
        dest_pipe_position: Position,
        weak_index: WeakIndex,
        inserter_store: &mut StorageStorageInserterStore,
    ) {
        let FluidSystemEntity::Input { inserter_id, .. } = self
            .graph
            .remove_weak_element(dest_pipe_position, weak_index)
        else {
            unreachable!("Tried to remove output with non input weakindex");
        };

        let _inserter_removal_info: () = inserter_store.remove_ins(
            self.get_fluid()
                .expect("Fluid Networks without a set fluid cannot have inputs"),
            FLUID_INSERTER_MOVETIME,
            inserter_id,
        );
    }

    fn add_fluid_box_trusted(
        &mut self,
        fluid_box_position: Position,
        fluid_box_capacity: u32,
        fluid_box_connections: impl IntoIterator<Item = Position>,
        chest_store: &mut FullChestStore<ItemIdxType>,
    ) {
        self.storage_capacity = self
            .storage_capacity
            .checked_add(fluid_box_capacity)
            .expect("TODO: Fluid network size exceeded u32::MAX");
        if let FluidSystemState::HasFluid { fluid, chest_id } = self.state {
            let 0 = chest_store.stores[fluid.into_usize()].change_chest_size(
                chest_id,
                self.storage_capacity
                    .try_into()
                    .expect("Fluid system too large"),
            ) else {
                unreachable!(
                    "We increase the size of the fluid network. It should not be possible remove items with it"
                );
            };
        }

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
        chest_store: &mut FullChestStore<ItemIdxType>,
    ) {
        let mut fluid_box_connections = fluid_box_connections.into_iter();
        self.storage_capacity = self
            .storage_capacity
            .checked_add(fluid_box_capacity)
            .expect("TODO: Fluid network size exceeded u32::MAX");
        if let FluidSystemState::HasFluid { fluid, chest_id } = self.state {
            let 0 = chest_store.stores[fluid.into_usize()].change_chest_size(
                chest_id,
                self.storage_capacity
                    .try_into()
                    .expect("Fluid system too large"),
            ) else {
                unreachable!(
                    "We increase the size of the fluid network. It should not be possible remove items with it"
                );
            };
        }

        self.graph.add_node(
            FluidBox {
                capacity: fluid_box_capacity,
            },
            fluid_box_position,
            (fluid_box_connections.next().unwrap(), fluid_box_connections),
        );
    }

    fn join<RecipeIdxType: IdxTrait>(
        &mut self,
        mut other: Self,
        new_fluid_box_position: Position,
        fluid_box_capacity: u32,
        connected_fluid_box_positions: impl IntoIterator<Item = Position>,
        chest_store: &mut FullChestStore<ItemIdxType>,
        inserter_store: &mut StorageStorageInserterStore,
        data_store: &DataStore<ItemIdxType, RecipeIdxType>,
    ) {
        self.check_consistency(chest_store);
        other.check_consistency(chest_store);

        if !self
            .graph
            .keys()
            .into_iter()
            .any(|pos| *pos == new_fluid_box_position)
        {
            self.storage_capacity += fluid_box_capacity;
        }
        self.storage_capacity = self
            .storage_capacity
            .checked_add(other.storage_capacity)
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
            (FluidSystemState::NoFluid, FluidSystemState::HasFluid { fluid, chest_id }) => {
                let 0 = chest_store.stores[fluid.into_usize()].change_chest_size(
                    chest_id,
                    (self.storage_capacity)
                        .try_into()
                        .expect("Fluid system too large"),
                ) else {
                    unreachable!();
                };

                self.state = FluidSystemState::HasFluid { fluid, chest_id }
            },
            (FluidSystemState::HasFluid { fluid, chest_id }, FluidSystemState::NoFluid) => {
                let 0 = chest_store.stores[fluid.into_usize()].change_chest_size(
                    chest_id,
                    (self.storage_capacity)
                        .try_into()
                        .expect("Fluid system too large"),
                ) else {
                    unreachable!();
                };
            },
            (
                FluidSystemState::HasFluid {
                    fluid: fluid_a,
                    chest_id: chest_id_a,
                },
                FluidSystemState::HasFluid {
                    fluid: fluid_b,
                    chest_id: chest_id_b,
                },
            ) => {
                assert_eq!(fluid_a, fluid_b);
                let fluid = fluid_a;

                let removed_fluid = chest_store.stores[fluid.into_usize()].remove_chest(chest_id_b);

                let 0 = chest_store.stores[fluid.into_usize()].change_chest_size(
                    chest_id_a,
                    (self.storage_capacity)
                        .try_into()
                        .expect("Fluid system too large"),
                ) else {
                    unreachable!();
                };

                chest_store.stores[fluid.into_usize()]
                    .add_items_to_chest(chest_id_a, removed_fluid)
                    .expect(
                        "Adding the contents of two chests (and their sizes) should always fit",
                    );

                for conn in other.graph.weak_components_mut() {
                    match conn {
                        FluidSystemEntity::OutgoingPump { inserter_id } => {
                            *inserter_id = inserter_store.update_inserter_src(
                                fluid,
                                FLUID_INSERTER_MOVETIME,
                                *inserter_id,
                                Storage::Static {
                                    static_id: StaticID::Chest as u16,
                                    index: chest_id_a,
                                },
                                data_store,
                            );
                        },
                        FluidSystemEntity::IncomingPump { inserter_id } => {
                            *inserter_id = inserter_store.update_inserter_dest(
                                fluid,
                                FLUID_INSERTER_MOVETIME,
                                *inserter_id,
                                Storage::Static {
                                    static_id: StaticID::Chest as u16,
                                    index: chest_id_a,
                                },
                                data_store,
                            );
                        },
                        FluidSystemEntity::Input { inserter_id, .. } => {
                            *inserter_id = inserter_store.update_inserter_dest(
                                fluid,
                                FLUID_INSERTER_MOVETIME,
                                *inserter_id,
                                Storage::Static {
                                    static_id: StaticID::Chest as u16,
                                    index: chest_id_a,
                                },
                                data_store,
                            );
                        },
                        FluidSystemEntity::Output { inserter_id, .. } => {
                            *inserter_id = inserter_store.update_inserter_src(
                                fluid,
                                FLUID_INSERTER_MOVETIME,
                                *inserter_id,
                                Storage::Static {
                                    static_id: StaticID::Chest as u16,
                                    index: chest_id_a,
                                },
                                data_store,
                            );
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

    fn remove_fluid_box<RecipeIdxType: IdxTrait>(
        &mut self,
        fluid_box_position: Position,
        chest_store: &mut FullChestStore<ItemIdxType>,
        inserter_store: &mut StorageStorageInserterStore,
        data_store: &DataStore<ItemIdxType, RecipeIdxType>,
    ) -> (
        impl Iterator<Item = Self> + use<RecipeIdxType, ItemIdxType>,
        bool,
    ) {
        let old_fluid = self.get_fluid();

        let (removed_fluid_box, connections_to_remove, new_graphs) =
            self.graph.remove_node(fluid_box_position);

        let old_fluid_level = match self.state {
            FluidSystemState::NoFluid => 0,
            FluidSystemState::HasFluid { fluid, chest_id } => {
                let (fluid_level, _max) =
                    chest_store.stores[fluid.into_usize()].get_chest(chest_id);
                fluid_level
            },
        };
        let mut fluid_distribution = old_fluid_level;

        for (_weak_index, connection_to_remove) in connections_to_remove {
            let fluid = old_fluid.expect("If we have any connections we MUST have a fluid set");
            match connection_to_remove {
                FluidSystemEntity::OutgoingPump { inserter_id } => {
                    inserter_store.remove_ins(fluid, FLUID_INSERTER_MOVETIME, inserter_id)
                },
                FluidSystemEntity::IncomingPump { inserter_id } => {
                    inserter_store.remove_ins(fluid, FLUID_INSERTER_MOVETIME, inserter_id)
                },
                FluidSystemEntity::Input { inserter_id, .. } => {
                    inserter_store.remove_ins(fluid, FLUID_INSERTER_MOVETIME, inserter_id)
                },
                FluidSystemEntity::Output { inserter_id, .. } => {
                    inserter_store.remove_ins(fluid, FLUID_INSERTER_MOVETIME, inserter_id)
                },
            }
        }

        let new_grids: Vec<_> = new_graphs
            .into_iter()
            .flatten()
            .map(|(graph, _positions)| {
                let mut new_system = Self::new_from_graph(
                    graph,
                    old_fluid,
                    &mut fluid_distribution,
                    &mut self.storage_capacity,
                    chest_store,
                );

                match new_system.state {
                    FluidSystemState::NoFluid => {},
                    FluidSystemState::HasFluid { fluid, chest_id } => {
                        let our_storage = Storage::Static {
                            static_id: StaticID::Chest as u16,
                            index: chest_id,
                        };

                        for connection in new_system.graph.weak_components_mut() {
                            match connection {
                                FluidSystemEntity::OutgoingPump { inserter_id } => {
                                    *inserter_id = inserter_store.update_inserter_src(
                                        fluid,
                                        FLUID_INSERTER_MOVETIME,
                                        *inserter_id,
                                        our_storage,
                                        data_store,
                                    );
                                },
                                FluidSystemEntity::IncomingPump { inserter_id } => {
                                    *inserter_id = inserter_store.update_inserter_dest(
                                        fluid,
                                        FLUID_INSERTER_MOVETIME,
                                        *inserter_id,
                                        our_storage,
                                        data_store,
                                    );
                                },
                                FluidSystemEntity::Input { inserter_id, .. } => {
                                    *inserter_id = inserter_store.update_inserter_dest(
                                        fluid,
                                        FLUID_INSERTER_MOVETIME,
                                        *inserter_id,
                                        our_storage,
                                        data_store,
                                    );
                                },
                                FluidSystemEntity::Output { inserter_id, .. } => {
                                    *inserter_id = inserter_store.update_inserter_src(
                                        fluid,
                                        FLUID_INSERTER_MOVETIME,
                                        *inserter_id,
                                        our_storage,
                                        data_store,
                                    );
                                },
                            }
                        }
                    },
                }

                new_system
            })
            .collect();

        self.storage_capacity -= removed_fluid_box.capacity;

        let fluid_left_for_us =
            ChestSize::try_from(min(fluid_distribution.into(), self.storage_capacity)).unwrap();

        if self.graph.nodes().into_iter().next().is_none() {
            // We no longer exist
            assert!(self.storage_capacity == 0);
            assert!(fluid_left_for_us == 0);
            match self.state {
                FluidSystemState::NoFluid => {},
                FluidSystemState::HasFluid { fluid, chest_id } => {
                    let _ = chest_store.stores[fluid.into_usize()].remove_chest(chest_id);
                },
            }
            return (new_grids.into_iter(), true);
        }

        match self.state {
            FluidSystemState::NoFluid => {},
            FluidSystemState::HasFluid { fluid, chest_id } => {
                chest_store.stores[fluid.into_usize()]
                    .remove_items_from_chest(chest_id, old_fluid_level - fluid_left_for_us)
                    .expect("Not enough items in fluid system chest");
                assert!(fluid_left_for_us <= self.storage_capacity.try_into().unwrap());
                let 0 = chest_store.stores[fluid.into_usize()]
                    .change_chest_size(chest_id, self.storage_capacity.try_into().unwrap())
                else {
                    unreachable!();
                };
            },
        }

        (new_grids.into_iter(), false)
    }

    fn remove_fluid_box_connection<RecipeIdxType: IdxTrait>(
        &mut self,
        first_fluid_box_position: Position,
        second_fluid_box_position: Position,
        chest_store: &mut FullChestStore<ItemIdxType>,
        inserter_store: &mut StorageStorageInserterStore,
        data_store: &DataStore<ItemIdxType, RecipeIdxType>,
    ) -> (
        impl Iterator<Item = Self> + use<RecipeIdxType, ItemIdxType>,
        bool,
    ) {
        let old_fluid = self.get_fluid();

        let new_graphs = self
            .graph
            .remove_edge(first_fluid_box_position, second_fluid_box_position);

        let old_fluid_level = match self.state {
            FluidSystemState::NoFluid => 0,
            FluidSystemState::HasFluid { fluid, chest_id } => {
                let (fluid_level, _max) =
                    chest_store.stores[fluid.into_usize()].get_chest(chest_id);
                fluid_level
            },
        };
        let mut fluid_distribution = old_fluid_level;

        let new_grids: Vec<_> = new_graphs
            .into_iter()
            .flatten()
            .map(|(graph, _positions)| {
                let mut new_system = Self::new_from_graph(
                    graph,
                    old_fluid,
                    &mut fluid_distribution,
                    &mut self.storage_capacity,
                    chest_store,
                );

                match new_system.state {
                    FluidSystemState::NoFluid => {},
                    FluidSystemState::HasFluid { fluid, chest_id } => {
                        let our_storage = Storage::Static {
                            static_id: StaticID::Chest as u16,
                            index: chest_id,
                        };

                        for connection in new_system.graph.weak_components_mut() {
                            match connection {
                                FluidSystemEntity::OutgoingPump { inserter_id } => {
                                    *inserter_id = inserter_store.update_inserter_src(
                                        fluid,
                                        FLUID_INSERTER_MOVETIME,
                                        *inserter_id,
                                        our_storage,
                                        data_store,
                                    );
                                },
                                FluidSystemEntity::IncomingPump { inserter_id } => {
                                    *inserter_id = inserter_store.update_inserter_dest(
                                        fluid,
                                        FLUID_INSERTER_MOVETIME,
                                        *inserter_id,
                                        our_storage,
                                        data_store,
                                    );
                                },
                                FluidSystemEntity::Input { inserter_id, .. } => {
                                    *inserter_id = inserter_store.update_inserter_dest(
                                        fluid,
                                        FLUID_INSERTER_MOVETIME,
                                        *inserter_id,
                                        our_storage,
                                        data_store,
                                    );
                                },
                                FluidSystemEntity::Output { inserter_id, .. } => {
                                    *inserter_id = inserter_store.update_inserter_src(
                                        fluid,
                                        FLUID_INSERTER_MOVETIME,
                                        *inserter_id,
                                        our_storage,
                                        data_store,
                                    );
                                },
                            }
                        }
                    },
                }

                new_system
            })
            .collect();

        let fluid_left_for_us =
            ChestSize::try_from(min(fluid_distribution.into(), self.storage_capacity)).unwrap();

        if self.graph.nodes().into_iter().next().is_none() {
            // We no longer exist
            assert!(self.storage_capacity == 0);
            assert!(fluid_left_for_us == 0);
            match self.state {
                FluidSystemState::NoFluid => {},
                FluidSystemState::HasFluid { fluid, chest_id } => {
                    let _ = chest_store.stores[fluid.into_usize()].remove_chest(chest_id);
                },
            }
            return (new_grids.into_iter(), true);
        }

        match self.state {
            FluidSystemState::NoFluid => {},
            FluidSystemState::HasFluid { fluid, chest_id } => {
                chest_store.stores[fluid.into_usize()]
                    .remove_items_from_chest(chest_id, old_fluid_level - fluid_left_for_us)
                    .expect("Not enough items in fluid system chest");
                assert!(fluid_left_for_us <= self.storage_capacity.try_into().unwrap());
                let 0 = chest_store.stores[fluid.into_usize()]
                    .change_chest_size(chest_id, self.storage_capacity.try_into().unwrap())
                else {
                    unreachable!();
                };
            },
        }

        (new_grids.into_iter(), false)
    }

    pub fn check_consistency(&self, chest_store: &FullChestStore<ItemIdxType>) {
        let calculated_capacity: u32 = self
            .graph
            .nodes()
            .into_iter()
            .map(|node| node.capacity)
            .sum();

        assert_eq!(self.storage_capacity, calculated_capacity);

        match self.state {
            FluidSystemState::NoFluid => {},
            FluidSystemState::HasFluid { fluid, chest_id } => {
                let (current_fluid_level, max_fluid_level_chest) =
                    chest_store.stores[fluid.into_usize()].get_chest(chest_id);

                assert_eq!(self.storage_capacity, u32::from(max_fluid_level_chest));

                assert!(u32::from(current_fluid_level) <= self.storage_capacity);
            },
        }
    }
}
