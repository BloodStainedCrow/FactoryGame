use std::{fmt::Debug, iter::once, usize};

use bimap::BiMap;
use log::info;
use petgraph::{
    algo::tarjan_scc,
    prelude::StableUnGraph,
    visit::{EdgeRef, IntoNodeReferences},
};

use std::hash::Hash;

#[derive(Debug, Clone, serde::Deserialize, serde::Serialize)]
pub struct Network<NodeKey: Eq + Hash, S, W> {
    // We use stableUnGraph here to allow remove_node to not invalidate any other indices
    graph: StableUnGraph<NetworkNode<S, W>, ()>,
    key_map: BiMap<NodeKey, petgraph::stable_graph::NodeIndex>,
}

#[derive(Debug, Clone, serde::Deserialize, serde::Serialize)]
struct NetworkNode<S, W> {
    node_info: S,
    connected_weak_components: Vec<Option<W>>,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, serde::Deserialize, serde::Serialize)]
pub struct WeakIndex {
    index: usize,
}

pub struct NetworkUpdate {
    new_network: usize,
}

impl<NodeKey: Eq + Hash + Clone + Debug, S, W> Network<NodeKey, S, W> {
    pub fn new(first_node: S, key: NodeKey) -> Self {
        let mut graph = StableUnGraph::default();

        let index = graph.add_node(NetworkNode {
            node_info: first_node,
            connected_weak_components: vec![],
        });

        Self {
            graph,
            key_map: once((key, index)).collect(),
        }
    }

    pub fn keys(&self) -> impl IntoIterator<Item = &NodeKey> {
        self.key_map.iter().map(|v| v.0)
    }

    pub fn nodes(&self) -> impl IntoIterator<Item = &S> {
        self.graph.node_weights().map(|n| &n.node_info)
    }

    pub fn weak_components(&self) -> impl IntoIterator<Item = &W> {
        self.graph
            .node_weights()
            .flat_map(|n| &n.connected_weak_components)
            .flatten()
    }

    pub fn weak_components_mut(&mut self) -> impl IntoIterator<Item = &mut W> {
        self.graph
            .node_weights_mut()
            .flat_map(|n| &mut n.connected_weak_components)
            .flatten()
    }

    pub fn add_node(
        &mut self,
        value: S,
        key: NodeKey,
        connection_points: (NodeKey, impl IntoIterator<Item = NodeKey>),
    ) {
        let index = self.graph.add_node(NetworkNode {
            node_info: value,
            connected_weak_components: vec![],
        });

        self.key_map
            .insert_no_overwrite(key, index)
            .expect("cannot use the same key for multiple nodes!");

        self.graph.add_edge(
            index,
            *self.key_map.get_by_left(&connection_points.0).unwrap(),
            (),
        );

        for connection in connection_points.1.into_iter() {
            self.graph
                .add_edge(index, *self.key_map.get_by_left(&connection).unwrap(), ());
        }
    }

    #[profiling::function]
    pub fn remove_node<'a>(
        &'a mut self,
        key: NodeKey,
    ) -> (
        S,
        impl IntoIterator<Item = W> + use<'a, NodeKey, S, W>,
        Option<
            impl IntoIterator<Item = (Self, impl IntoIterator<Item = NodeKey>)> + use<'a, NodeKey, S, W>,
        >,
    ) {
        let NetworkNode {
            node_info,
            connected_weak_components,
        } = self
            .graph
            .remove_node(*self.key_map.get_by_left(&key).unwrap())
            .unwrap();

        self.key_map.remove_by_left(&key);

        // Use kosaraju_scc instead of tarjan_scc since tarjan_scc is recursive and will overflow the stack for huge power grids
        let mut components = {
            profiling::scope!("Calculate Graph Components");
            petgraph::algo::kosaraju_scc(&self.graph)
        };

        // Pop the first component, (which will stay in this network)
        // TODO: It is probably good to have the largest component stay, but testing is required
        components.sort_by_key(|v| -(v.len() as isize));

        if components.pop() == None {
            info!("The last node in a network was removed, the network can be deleted");
            return (
                node_info,
                connected_weak_components.into_iter().flatten(),
                None,
            );
        }

        // All remaining components (if any, will be turned into other networks)
        let move_to_another_network = components;

        let new_networks = {
            profiling::scope!("Build new networks");
            move_to_another_network.into_iter().map(|component| {
                let connections: Vec<_> = component
                    .iter()
                    .map(|idx| self.graph.edges(*idx))
                    .flat_map(|edges| edges)
                    .map(|edge| (edge.source(), edge.target()))
                    .collect();

                let mut new_graph = StableUnGraph::default();

                let new_indices: Vec<_> = component
                    .iter()
                    .map(|idx: &petgraph::prelude::NodeIndex| {
                        new_graph.add_node(self.graph.remove_node(*idx).unwrap())
                    })
                    .collect();

                for (source, dest) in connections {
                    let source_idx_old = component.iter().position(|v| *v == source).unwrap();
                    let dest_idx_old = component.iter().position(|v| *v == dest).unwrap();

                    new_graph.add_edge(new_indices[source_idx_old], new_indices[dest_idx_old], ());
                }

                let keys_in_this: Vec<_> = component
                    .iter()
                    .map(|old_node| self.key_map.get_by_right(old_node).unwrap().clone())
                    .collect();

                (
                    Network {
                        graph: new_graph,
                        key_map: component
                            .into_iter()
                            .map(|old_node| self.key_map.remove_by_right(&old_node).unwrap())
                            .collect(),
                    },
                    keys_in_this,
                )
            })
        };

        (
            node_info,
            connected_weak_components.into_iter().flatten(),
            Some(new_networks),
        )
    }

    pub fn add_weak_element(&mut self, key: NodeKey, value: W) -> WeakIndex {
        let weak_components = &mut self
            .graph
            .node_weight_mut(*self.key_map.get_by_left(&key).unwrap())
            .unwrap()
            .connected_weak_components;

        let hole = weak_components.iter().position(Option::is_none);

        let index = if let Some(hole) = hole {
            weak_components[hole] = Some(value);

            hole
        } else {
            weak_components.push(Some(value));

            weak_components.len() - 1
        };

        WeakIndex { index }
    }

    pub fn remove_weak_element(&mut self, key: NodeKey, weak_index: WeakIndex) -> W {
        self.graph
            .node_weight_mut(*self.key_map.get_by_left(&key).unwrap())
            .unwrap()
            .connected_weak_components[weak_index.index]
            .take()
            .unwrap()
    }

    #[profiling::function]
    pub fn add_node_merging(
        &mut self,
        value: S,
        node_key: NodeKey,
        connection_points: (NodeKey, impl IntoIterator<Item = NodeKey>),
        other: Self,
    ) {
        let index = if let Some(index) = self.key_map.get_by_left(&node_key) {
            *index
        } else {
            let index = self.graph.add_node(NetworkNode {
                node_info: value,
                connected_weak_components: vec![],
            });
            self.key_map.insert_no_overwrite(node_key, index).unwrap();
            index
        };

        let Self { graph, key_map } = other;

        join_graphs(&mut self.graph, &mut self.key_map, graph, key_map);

        self.graph.add_edge(
            index,
            *self.key_map.get_by_left(&connection_points.0).unwrap(),
            (),
        );

        for connection in connection_points.1 {
            let Some(conn) = self.key_map.get_by_left(&connection) else {
                // The power pole this connection corresponds with will be added in a future merge. Just ignore it for now
                continue;
            };

            self.graph.add_edge(index, *conn, ());
        }

        debug_assert!(tarjan_scc(&self.graph).len() == 1);
    }

    pub fn modify_weak_component(&mut self, key: NodeKey, index: WeakIndex) -> &mut W {
        self.graph
            .node_weight_mut(*self.key_map.get_by_left(&key).unwrap())
            .unwrap()
            .connected_weak_components[index.index]
            .as_mut()
            .unwrap()
    }
}

#[profiling::function]
fn join_graphs<NodeKey: Eq + Hash + Clone + Debug, T, S: Default>(
    first: &mut StableUnGraph<T, S>,
    first_map: &mut BiMap<NodeKey, petgraph::stable_graph::NodeIndex>,
    mut second: StableUnGraph<T, S>,
    mut second_map: BiMap<NodeKey, petgraph::stable_graph::NodeIndex>,
) {
    #[cfg(debug_assertions)]
    let first_components = petgraph::algo::tarjan_scc(&*first).len();
    #[cfg(debug_assertions)]
    let second_components = petgraph::algo::tarjan_scc(&second).len();

    // Do the merging
    let old_node_indices: Vec<_> = second.node_references().map(|r| r.0).collect();

    let edges: Vec<Vec<_>> = old_node_indices
        .iter()
        .map(|index| {
            second
                .edges(*index)
                .map(|er| (er.source(), er.target()))
                .collect()
        })
        .collect();

    let new_node_indices: Vec<_> = old_node_indices
        .iter()
        .map(|index| first.add_node(second.remove_node(*index).unwrap()))
        .collect();

    for (old_id, (new_id, edges)) in old_node_indices
        .iter()
        .zip(new_node_indices.iter().zip(edges))
    {
        for edge in edges {
            debug_assert!(edge.0 == *old_id || edge.1 == *old_id);

            if edge.0 == *old_id {
                first.add_edge(
                    *new_id,
                    new_node_indices[old_node_indices.iter().position(|i| *i == edge.1).unwrap()],
                    S::default(),
                );
            } else if edge.1 == *old_id {
                first.add_edge(
                    *new_id,
                    new_node_indices[old_node_indices.iter().position(|i| *i == edge.0).unwrap()],
                    S::default(),
                );
            } else {
                unreachable!()
            }
        }
    }

    #[cfg(debug_assertions)]
    {
        let final_components = petgraph::algo::tarjan_scc(&*first).len();
        assert_eq!(final_components, first_components + second_components);
    }

    first_map.extend(
        old_node_indices
            .iter()
            .zip(new_node_indices)
            .map(|(old, new)| (second_map.remove_by_right(old).unwrap().0, new)),
    );
}
