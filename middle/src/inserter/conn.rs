use std::collections::HashMap;

use backend::Backend;
use data::{
    item::item_set::ItemSet,
    recipe::{get_items_consumed_by_recipe, get_items_produced_by_recipe},
};
use middle_indices::{AssemblerMiddleID, ChestMiddleID, InserterMiddleID};

use crate::Middle;

#[derive(Debug, Clone, Copy)]
pub enum Conn {
    Assembler { id: AssemblerMiddleID },
    Chest { id: ChestMiddleID },
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
enum Container {
    Chest { id: ChestMiddleID },
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
enum Edge {
    Inserter { id: InserterMiddleID },
}

impl TryFrom<Conn> for Container {
    type Error = ();

    fn try_from(value: Conn) -> Result<Self, Self::Error> {
        match value {
            Conn::Assembler { id } => Err(()),
            Conn::Chest { id } => Ok(Container::Chest { id }),
        }
    }
}

impl Middle {
    /// The items that an inserter could take from this Conn.
    /// Typically this is dependent on either what a machine produces, or what can reach a container type entity
    pub(super) fn get_items_takeable_from(&self, conn: Conn) -> ItemSet {
        match conn {
            Conn::Assembler { id } => {
                let recipe = self.assembler_list[id.0 as usize].current_recipe;

                get_items_produced_by_recipe(recipe)
            },
            Conn::Chest { id } => self
                .get_item_in_container(Container::Chest { id: id })
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
            Conn::Chest { id } => ItemSet::all(),
        }
    }

    pub fn get_item_in_container(&self, container: Container) -> &ItemSet {
        match container {
            Container::Chest { id } => &self.chest_list[id.0 as usize].inferred_items,
        }
    }

    pub(super) fn apply_effect_of_new_edge(
        &mut self,
        source: Conn,
        dest: Conn,
        item_filter: &ItemSet,
        backend: &mut Backend,
    ) {
        let mut container_changes: HashMap<Container, ItemSet> = HashMap::new();
        let mut edge_changes: HashMap<Edge, ItemSet> = HashMap::new();

        self.apply_effect_of_new_edge_internal(
            source,
            dest,
            item_filter,
            &mut container_changes,
            &mut edge_changes,
        );

        todo!("Apply changes to self and backend")
    }

    fn apply_effect_of_new_edge_internal(
        &self,
        source: Conn,
        dest: Conn,
        item_filter: &ItemSet,
        container_changes: &mut HashMap<Container, ItemSet>,
        edge_changes: &mut HashMap<Edge, ItemSet>,
    ) {
        todo!()
    }

    fn container_content_has_changed(
        &self,
        container: Container,
        container_changes: &mut HashMap<Container, ItemSet>,
        edge_changes: &mut HashMap<Edge, ItemSet>,
    ) {
        let inserters = match container {
            Container::Chest { id } => self.chest_list[id.0 as usize].connected_inserters.iter(),
        };

        for inserter in inserters {
            self.inserter_input_has_changed(*inserter, container_changes, edge_changes);
        }
    }

    fn inserter_input_has_changed(
        &self,
        inserter: InserterMiddleID,
        container_changes: &mut HashMap<Container, ItemSet>,
        edge_changes: &mut HashMap<Edge, ItemSet>,
    ) {
        let new_items = &edge_changes[&Edge::Inserter { id: inserter }];

        let destination = self.inserter_list[inserter.0 as usize].dest;

        let Ok(destination_container) = destination.try_into() else {
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
            None => &self.get_item_in_container(destination_container),
        };

        if ItemSet::is_subset(items_in_dest, &items_in_edge) {
            return;
        }

        let mut new_container_contents = items_arriving_via_edge;
        new_container_contents.union(items_in_dest);

        container_changes.insert(destination_container, new_container_contents);

        self.container_content_has_changed(destination_container, container_changes, edge_changes);
    }
}
