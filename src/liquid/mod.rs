use std::collections::HashMap;

use itertools::Itertools;
use log::warn;

use crate::item::Indexable;
use crate::{
    chest::FullChestStore,
    data::DataStore,
    frontend::world::Position,
    inserter::{StaticID, Storage},
    item::{IdxTrait, Item, WeakIdxTrait},
    network_graph::{Network, WeakIndex},
    rendering::app_state::StorageStorageInserterStore,
};

pub mod connection_logic;

#[derive(Debug, Clone, Copy, PartialEq, Eq, serde::Deserialize, serde::Serialize)]
pub struct FluidSystemId<ItemIdxType: WeakIdxTrait> {
    pub fluid: Option<Item<ItemIdxType>>,
    index: usize,
}

#[derive(Debug, Clone, serde::Deserialize, serde::Serialize)]
pub struct FluidSystemStore<ItemIdxType: WeakIdxTrait> {
    fluid_systems_with_fluid: Box<[Vec<Option<FluidSystem<ItemIdxType>>>]>,
    empty_fluid_systems: Vec<Option<FluidSystem<ItemIdxType>>>,
    pub fluid_box_pos_to_network_id: HashMap<Position, FluidSystemId<ItemIdxType>>,
}

#[derive(Debug, Clone, Copy)]
pub struct CannotMixFluidsError<ItemIdxType: WeakIdxTrait> {
    pub items: [Item<ItemIdxType>; 2],
}

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
            fluid_systems_with_fluid: vec![vec![]; data_store.item_names.len()].into_boxed_slice(),
            empty_fluid_systems: vec![],
            fluid_box_pos_to_network_id: HashMap::new(),
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
                Box<dyn FnOnce(WeakIndex) -> ()>,
            ),
        >,

        chest_store: &mut FullChestStore<ItemIdxType>,
        inserter_store: &mut StorageStorageInserterStore,

        data_store: &DataStore<ItemIdxType, RecipeIdxType>,
    ) -> Result<(), CannotMixFluidsError<ItemIdxType>> {
        let connected_boxes: Vec<_> = connected_fluid_box_positions.into_iter().collect();
        let connected_storages: Vec<_> = connected_storages.into_iter().collect();

        let connected_storages_fluid = match connected_storages
            .iter()
            .map(|(_dir, fluid, _storage, _cb)| *fluid)
            .all_equal_value()
        {
            Ok(fluid) => Some(fluid),
            Err(Some((a, b))) => return Err(CannotMixFluidsError { items: [a, b] }),
            Err(None) => None,
        };

        let id_the_box_ends_up_with = if let Some(first_connection) = connected_boxes.last() {
            let network_to_join_into = self.fluid_box_pos_to_network_id[first_connection];

            let need_to_merge = !connected_boxes
                .iter()
                .map(|pos| self.fluid_box_pos_to_network_id[pos])
                .all_equal();

            let final_id = if need_to_merge {
                let merge_fluid = match connected_boxes
                    .iter()
                    // Remove all fluid networks without a set fluid type since we can just set them to whatever fluid we might decide on
                    .flat_map(|pos| self.fluid_box_pos_to_network_id[pos].fluid)
                    .all_equal_value()
                {
                    Ok(fluid) => Some(fluid),
                    Err(Some((a, b))) => {
                        return Err(CannotMixFluidsError { items: [a, b] });
                    },
                    Err(None) => None,
                };

                let network_to_join_into = connected_boxes
                    .iter()
                    .map(|pos| self.fluid_box_pos_to_network_id[pos])
                    .find(|id| id.fluid == merge_fluid)
                    .unwrap();

                assert_eq!(network_to_join_into.fluid, merge_fluid);

                let final_fluid = match (merge_fluid, connected_storages_fluid) {
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
                    .collect();
                for removed_id in removed_ids {
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

                        let index = self.fluid_systems_with_fluid[fluid.into_usize()]
                            .iter()
                            .position(Option::is_none);

                        let index = if let Some(hole_idx) = index {
                            assert!(self.fluid_systems_with_fluid[fluid.into_usize()][hole_idx]
                                .is_none());
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

                        for rhs in self.fluid_box_pos_to_network_id.values_mut() {
                            if *rhs == network_to_join_into {
                                *rhs = new_id;
                            }
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

                for (dir, fluid, storage, callback) in connected_storages {
                    let weak_index = match dir {
                        FluidConnectionDir::Input => network.add_input(
                            fluid,
                            new_fluid_box_position,
                            storage,
                            inserter_store,
                            data_store,
                        ),
                        FluidConnectionDir::Output => network.add_output(
                            fluid,
                            new_fluid_box_position,
                            storage,
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

            for (dir, fluid, storage, callback) in connected_storages {
                let weak_index = match dir {
                    FluidConnectionDir::Input => new_network.add_input(
                        fluid,
                        new_fluid_box_position,
                        storage,
                        inserter_store,
                        data_store,
                    ),
                    FluidConnectionDir::Output => new_network.add_output(
                        fluid,
                        new_fluid_box_position,
                        storage,
                        inserter_store,
                        data_store,
                    ),
                };

                callback(weak_index);
            }

            let index = match connected_storages_fluid {
                Some(fluid) => {
                    let index = self.fluid_systems_with_fluid[fluid.into_usize()]
                        .iter()
                        .position(Option::is_none);

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
                    let index = self.empty_fluid_systems.iter().position(Option::is_none);

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
                std::collections::hash_map::Entry::Occupied(_) => {
                    unreachable!("Two Fluid Boxes at the same position")
                },
                std::collections::hash_map::Entry::Vacant(vacant_entry) => {
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
                .check_consistency(chest_store, data_store),
            None => self.empty_fluid_systems[id_the_box_ends_up_with.index]
                .as_ref()
                .unwrap()
                .check_consistency(chest_store, data_store),
        }

        Ok(())
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
            assert_eq!(
                self.fluid_box_pos_to_network_id[box_pos_in_removed],
                removed_id
            );
            self.fluid_box_pos_to_network_id
                .insert(*box_pos_in_removed, kept_id);
        }

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

        for rhs in self.fluid_box_pos_to_network_id.values_mut() {
            if *rhs == removed_id {
                *rhs = kept_id;
            }
        }
    }
}

#[derive(Debug, Clone, serde::Deserialize, serde::Serialize)]
enum FluidSystemEntity {
    OutgoingPump { inserter_id: usize },
    IncomingPump { inserter_id: usize },
    Input { inserter_id: usize },
    Output { inserter_id: usize },
}

#[derive(Debug, Clone, serde::Deserialize, serde::Serialize)]
struct FluidBox {
    capacity: u32,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, serde::Deserialize, serde::Serialize)]
enum FluidSystemState<ItemIdxType: WeakIdxTrait> {
    NoFluid,
    HasFluid {
        fluid: Item<ItemIdxType>,
        chest_id: u32,
    },
}

#[derive(Debug, Clone, serde::Deserialize, serde::Serialize)]
struct FluidSystem<ItemIdxType: WeakIdxTrait> {
    graph: Network<Position, FluidBox, FluidSystemEntity>,
    storage_capacity: u32,
    state: FluidSystemState<ItemIdxType>,
}

impl<ItemIdxType: IdxTrait> FluidSystem<ItemIdxType> {
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
        inserter_store: &mut StorageStorageInserterStore,
        data_store: &DataStore<ItemIdxType, RecipeIdxType>,
    ) -> WeakIndex {
        let inserter_id = inserter_store.add_ins(
            fluid,
            crate::inserter::Storage::Static {
                static_id: StaticID::Chest as u16,
                index: self.get_chest_id().unwrap(),
            },
            dest,
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
        inserter_store: &mut StorageStorageInserterStore,
        data_store: &DataStore<ItemIdxType, RecipeIdxType>,
    ) -> WeakIndex {
        let inserter_id = inserter_store.add_ins(
            fluid,
            source,
            crate::inserter::Storage::Static {
                static_id: StaticID::Chest as u16,
                index: self.get_chest_id().unwrap(),
            },
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
            crate::inserter::Storage::Static {
                static_id: StaticID::Chest as u16,
                index: self.get_chest_id().unwrap(),
            },
            crate::inserter::Storage::Static {
                static_id: StaticID::Chest as u16,
                index: dest_fluid_network.get_chest_id().unwrap(),
            },
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
        let FluidSystemEntity::Output { inserter_id } = self
            .graph
            .remove_weak_element(source_pipe_position, weak_index)
        else {
            unreachable!("Tried to remove output with non output weakindex");
        };

        let _inserter_removal_info: () = inserter_store.remove_ins(
            self.get_fluid()
                .expect("Fluid Networks without a set fluid cannot have outputs"),
            inserter_id,
        );
    }

    pub fn remove_input(
        &mut self,
        dest_pipe_position: Position,
        weak_index: WeakIndex,
        inserter_store: &mut StorageStorageInserterStore,
    ) {
        let FluidSystemEntity::Input { inserter_id } = self
            .graph
            .remove_weak_element(dest_pipe_position, weak_index)
        else {
            unreachable!("Tried to remove output with non input weakindex");
        };

        let _inserter_removal_info: () = inserter_store.remove_ins(
            self.get_fluid()
                .expect("Fluid Networks without a set fluid cannot have inputs"),
            inserter_id,
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
                unreachable!("We increase the size of the fluid network. It should not be possible remove items with it");
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
        self.check_consistency(chest_store, data_store);
        other.check_consistency(chest_store, data_store);

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
                debug_assert!(self
                    .graph
                    .weak_components()
                    .into_iter()
                    .all(|conn| match conn {
                        FluidSystemEntity::OutgoingPump { .. } => true,
                        FluidSystemEntity::IncomingPump { .. } => true,
                        // Since input/outputs are always a specific fluid, we cannot have any (otherwise we shoudl already have a fluid)
                        FluidSystemEntity::Input { .. } => false,
                        FluidSystemEntity::Output { .. } => false,
                    }));
                debug_assert!(other
                    .graph
                    .weak_components()
                    .into_iter()
                    .all(|conn| match conn {
                        FluidSystemEntity::OutgoingPump { .. } => true,
                        FluidSystemEntity::IncomingPump { .. } => true,
                        // Since input/outputs are always a specific fluid, we cannot have any (otherwise we shoudl already have a fluid)
                        FluidSystemEntity::Input { .. } => false,
                        FluidSystemEntity::Output { .. } => false,
                    }));
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

                for conn in other.graph.weak_components() {
                    match conn {
                        FluidSystemEntity::OutgoingPump { inserter_id } => inserter_store
                            .update_inserter_src(
                                fluid,
                                *inserter_id,
                                Storage::Static {
                                    static_id: StaticID::Chest as u16,
                                    index: chest_id_a,
                                },
                                data_store,
                            ),
                        FluidSystemEntity::IncomingPump { inserter_id } => inserter_store
                            .update_inserter_dest(
                                fluid,
                                *inserter_id,
                                Storage::Static {
                                    static_id: StaticID::Chest as u16,
                                    index: chest_id_a,
                                },
                                data_store,
                            ),
                        FluidSystemEntity::Input { inserter_id } => inserter_store
                            .update_inserter_dest(
                                fluid,
                                *inserter_id,
                                Storage::Static {
                                    static_id: StaticID::Chest as u16,
                                    index: chest_id_a,
                                },
                                data_store,
                            ),
                        FluidSystemEntity::Output { inserter_id } => inserter_store
                            .update_inserter_src(
                                fluid,
                                *inserter_id,
                                Storage::Static {
                                    static_id: StaticID::Chest as u16,
                                    index: chest_id_a,
                                },
                                data_store,
                            ),
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

    pub fn check_consistency<RecipeIdxType: IdxTrait>(
        &self,
        chest_store: &FullChestStore<ItemIdxType>,
        data_store: &DataStore<ItemIdxType, RecipeIdxType>,
    ) {
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
