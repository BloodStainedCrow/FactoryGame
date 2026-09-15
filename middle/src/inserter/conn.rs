use std::collections::BTreeMap;

use backend::{
    Backend,
    chests::FullChestIdentifier,
    graph_changes::{GraphChanges, NewChestState, NewInserterState, NewTransportLineState},
    power_grid::{
        assembler::FullAssemblerIdentifier,
        inserter::{FullInserterIdentifier, conn::BackendInserterConnection},
    },
    transport_lines::FullTransportLineIdentifier,
};
use data::{
    item::item_set::ItemSet,
    recipe::{get_items_consumed_by_recipe, get_items_produced_by_recipe},
};
use itertools::Either;
use middle_indices::{
    AssemblerMiddleID, BeltTileMiddleID, ChestMiddleID, InserterMiddleID, TransportLineMiddleID,
};

use crate::{
    Middle,
    belt::{BeltTileInfo, TransportLineInfo},
};

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Conn {
    Assembler { id: AssemblerMiddleID },
    Chest { id: ChestMiddleID },
    BeltTile { id: BeltTileMiddleID },
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
enum Container {
    Chest { id: ChestMiddleID },
    TransportLine { id: TransportLineMiddleID },
}

impl Conn {
    fn get_container(self, middle: &Middle) -> Option<Container> {
        match self {
            Self::Assembler { id: _ } => None,
            Self::Chest { id } => Some(Container::Chest { id }),
            Self::BeltTile { id, .. } => {
                let id = middle.belt_tile_list[id.0 as usize].transport_line;

                Some(Container::TransportLine { id })
            },
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
enum Edge {
    Inserter { id: InserterMiddleID },
}

impl Middle {
    pub(crate) fn get_backend_conn(&self, conn: Conn) -> BackendInserterConnection<'_> {
        match conn {
            Conn::Assembler { id } => BackendInserterConnection::Assembler {
                ident: FullAssemblerIdentifier {
                    recipe: self.get_assembler_recipe(id),
                    grid: self.power_grid_list[self.get_assembler_power_grid(id).0 as usize]
                        .backend_id,
                    assembler_id: self.assembler_list[id.0 as usize].backend_id,
                },
            },
            Conn::Chest { id } => BackendInserterConnection::Chest {
                ident: FullChestIdentifier {
                    items: &self.chest_list[id.0 as usize].inferred_items,
                    id: self.chest_list[id.0 as usize].backend_id,
                },
            },
            Conn::BeltTile { id } => {
                let BeltTileInfo {
                    transport_line,
                    connected_inserters: _,
                    belt_pos,
                } = &self.belt_tile_list[id.0 as usize];

                let TransportLineInfo {
                    length: _,
                    backend_id,
                    inferred_items,
                    connected_tiles: _,
                } = &self.belt_list[transport_line.0 as usize];

                BackendInserterConnection::TransportLine {
                    ident: FullTransportLineIdentifier {
                        id: *backend_id,
                        items: inferred_items,
                    },
                    belt_pos: *belt_pos,
                }
            },
        }
    }

    /// The items that an inserter could take from this Conn.
    /// Typically this is dependent on either what a machine produces, or what can reach a container type entity
    pub(super) fn get_items_takeable_from(&self, conn: Conn) -> ItemSet {
        match conn {
            Conn::Assembler { id } => {
                let recipe = self.assembler_list[id.0 as usize].current_recipe;

                get_items_produced_by_recipe(recipe)
            },
            Conn::Chest { id } => self
                .get_item_in_container(Container::Chest { id })
                .clone(),
            Conn::BeltTile { id, .. } => self
                .get_item_in_container(Container::TransportLine {
                    id: self.belt_tile_list[id.0 as usize].transport_line,
                })
                .clone(),
        }
    }

    /// The items that an inserter could place in this conn.
    /// Typically this only depends on the kind and settings of the entity.
    pub(super) fn get_items_placeable_into(&self, conn: Conn) -> ItemSet {
        match conn {
            Conn::Assembler { id } => {
                let recipe = self.assembler_list[id.0 as usize].current_recipe;

                get_items_consumed_by_recipe(recipe)
            },
            Conn::Chest { .. } => ItemSet::all(),
            Conn::BeltTile { .. } => ItemSet::all(),
        }
    }

    #[must_use]
    pub fn get_item_in_container(&self, container: Container) -> &ItemSet {
        match container {
            Container::Chest { id } => &self.chest_list[id.0 as usize].inferred_items,
            Container::TransportLine { id } => &self.belt_list[id.0 as usize].inferred_items,
        }
    }

    pub(super) fn apply_effect_of_new_edge(
        &mut self,
        source: Conn,
        dest: Conn,
        item_filter: &ItemSet,
        backend: &mut Backend,
    ) {
        let mut container_changes: BTreeMap<Container, ItemSet> = BTreeMap::new();
        let mut edge_changes: BTreeMap<Edge, ItemSet> = BTreeMap::new();

        self.apply_effect_of_new_edge_internal(
            source,
            dest,
            item_filter,
            &mut container_changes,
            &mut edge_changes,
        );

        if !container_changes.is_empty() || !edge_changes.is_empty() {
            let inserter_changes = edge_changes
                .iter()
                .filter_map(|(edge, items)| match edge {
                    Edge::Inserter { id } => Some((id, items)),
                    _ => None,
                })
                .map(|(ins_id, new_items)| {
                    let ins = &self.inserter_list[ins_id.0 as usize];

                    let sources = ins
                        .sources
                        .map(|slot| slot.map(|conn| self.get_backend_conn(conn)));

                    (
                        FullInserterIdentifier {
                            grid: self.power_grid_list[ins.power_grid_id.0 as usize].backend_id,
                            inserter_id: ins.backend_id,
                            inferred_items: &ins.inferred_items,
                            source: sources,
                            dest: ins.dest.map(|dest| self.get_backend_conn(dest)),
                            movetime: ins.movetime,
                        },
                        NewInserterState {
                            middle_id: *ins_id,
                            inferred_items: new_items.clone(),
                        },
                    )
                })
                .collect();

            let chest_changes = container_changes
                .iter()
                .filter_map(|(container, items)| match container {
                    Container::Chest { id } => Some((id, items)),
                    _ => None,
                })
                .map(|(chest_id, new_items)| {
                    let chest = &self.chest_list[chest_id.0 as usize];

                    (
                        FullChestIdentifier {
                            items: &chest.inferred_items,
                            id: chest.backend_id,
                        },
                        NewChestState {
                            middle_id: *chest_id,
                            inferred_items: new_items.clone(),
                        },
                    )
                })
                .collect();

            let belt_changes = container_changes
                .iter()
                .filter_map(|(container, items)| match container {
                    Container::TransportLine { id } => Some((id, items)),
                    _ => None,
                })
                .map(|(tl_id, new_items)| {
                    let tl = &self.belt_list[tl_id.0 as usize];

                    (
                        FullTransportLineIdentifier {
                            items: &tl.inferred_items,
                            id: tl.backend_id,
                        },
                        NewTransportLineState {
                            middle_id: *tl_id,
                            inferred_items: new_items.clone(),
                        },
                    )
                })
                .collect();

            let res = backend.apply_graph_changes(GraphChanges {
                chest_changes,
                inserter_changes,
                transport_line_changes: belt_changes,
            });

            for (res, new_state) in res.chest_updates {
                match res {
                    backend::AdditionResult::Added {
                        new_id,
                        relocations,
                    } => {
                        if !relocations.is_empty() {
                            todo!("Apply relocations")
                        }

                        let middle_id = new_state.middle_id;

                        self.chest_list[middle_id.0 as usize].backend_id = new_id;
                        self.chest_list[middle_id.0 as usize].inferred_items =
                            new_state.inferred_items;
                    },
                    backend::AdditionResult::Failed { info: _ } => unreachable!(),
                }
            }

            for (res, new_state) in res.transport_lines_updates {
                match res {
                    backend::AdditionResult::Added {
                        new_id,
                        relocations,
                    } => {
                        if !relocations.is_empty() {
                            todo!("Apply relocations")
                        }

                        let middle_id = new_state.middle_id;

                        self.belt_list[middle_id.0 as usize].backend_id = new_id;
                        self.belt_list[middle_id.0 as usize].inferred_items =
                            new_state.inferred_items;
                    },
                    backend::AdditionResult::Failed { info: _ } => unreachable!(),
                }
            }

            for (res, new_state) in res.inserter_updates {
                match res {
                    backend::AdditionResult::Added {
                        new_id,
                        relocations,
                    } => {
                        if !relocations.is_empty() {
                            todo!("Apply relocations")
                        }

                        let middle_id = new_state.middle_id;

                        self.inserter_list[middle_id.0 as usize].backend_id = new_id;
                        self.inserter_list[middle_id.0 as usize].inferred_items =
                            new_state.inferred_items;
                    },
                    backend::AdditionResult::Failed { info: _ } => unreachable!(),
                }
            }
        }
    }

    fn apply_effect_of_new_edge_internal(
        &self,
        _source: Conn,
        dest: Conn,
        item_filter: &ItemSet,
        container_changes: &mut BTreeMap<Container, ItemSet>,
        edge_changes: &mut BTreeMap<Edge, ItemSet>,
    ) {
        let Some(destination_container) = dest.get_container(self) else {
            return;
        };

        let current_destination_items = self.get_item_in_container(destination_container);

        if ItemSet::is_subset(item_filter, current_destination_items) {
        } else {
            let mut destination_items = current_destination_items.clone();
            destination_items.union(item_filter);
            container_changes.insert(destination_container, destination_items);
            self.container_content_has_changed(
                destination_container,
                container_changes,
                edge_changes,
            );
        }
    }

    fn container_content_has_changed(
        &self,
        container: Container,
        container_changes: &mut BTreeMap<Container, ItemSet>,
        edge_changes: &mut BTreeMap<Edge, ItemSet>,
    ) {
        let inserters = match container {
            Container::Chest { id } => {
                Either::Left(self.chest_list[id.0 as usize].connected_inserters.iter())
            },
            Container::TransportLine { id } => Either::Right(
                self.belt_list[id.0 as usize]
                    .connected_tiles
                    .iter()
                    .flat_map(|tile| &self.belt_tile_list[tile.0 as usize].connected_inserters),
            ),
        };

        for inserter in inserters {
            self.inserter_input_has_changed(*inserter, container_changes, edge_changes);
        }
    }

    fn inserter_input_has_changed(
        &self,
        inserter: InserterMiddleID,
        container_changes: &mut BTreeMap<Container, ItemSet>,
        edge_changes: &mut BTreeMap<Edge, ItemSet>,
    ) {
        let new_items = &edge_changes[&Edge::Inserter { id: inserter }];

        let destination = self.inserter_list[inserter.0 as usize].dest;

        let Some(destination) = destination else {
            return;
        };

        let Some(destination_container) = destination.get_container(self) else {
            return;
        };

        let mut placeable_restriction = self.get_items_placeable_into(destination);

        placeable_restriction.intersection(new_items);
        let items_arriving_via_edge = placeable_restriction;

        let items_in_edge = match edge_changes.get(&Edge::Inserter { id: inserter }) {
            Some(already_changed) => already_changed,
            None => todo!(),
        }
        .clone();

        if items_in_edge == items_arriving_via_edge {
            return;
        }

        edge_changes.insert(
            Edge::Inserter { id: inserter },
            items_arriving_via_edge.clone(),
        );

        let items_in_dest = match container_changes.get(&destination_container) {
            Some(already_changed) => already_changed,
            None => self.get_item_in_container(destination_container),
        };

        if ItemSet::is_subset(&items_in_edge, items_in_dest) {
            return;
        }

        let mut new_container_contents = items_arriving_via_edge;
        new_container_contents.union(items_in_dest);

        container_changes.insert(destination_container, new_container_contents);

        self.container_content_has_changed(destination_container, container_changes, edge_changes);
    }
}
