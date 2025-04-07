use std::{collections::HashMap, usize};

use petgraph::{
    prelude::StableUnGraph,
    visit::{EdgeRef, IntoNodeReferences},
};

pub struct Network<S, W> {
    // We use stableUnGraph here to allow remove_node to not invalidate any other indices
    graph: StableUnGraph<NetworkNode<S, W>, ()>,
}

struct NetworkNode<S, W> {
    node_info: S,
    connected_weak_components: Vec<Option<W>>,
}

#[derive(Debug, Clone, Copy)]
pub struct NodeIndex {
    index: petgraph::stable_graph::NodeIndex,
}

pub struct WeakIndex {
    index: usize,
}

pub struct NodeUpdate {
    pub old_node: NodeIndex,
    /// This is is the index of the newly created network this node ended up in
    pub new_network: usize,
    pub new_node: NodeIndex,
}

impl<S, W> Network<S, W> {
    pub fn new(first_node: S) -> (Self, NodeIndex) {
        let mut slf = Self {
            graph: StableUnGraph::default(),
        };

        let index = slf.graph.add_node(NetworkNode {
            node_info: first_node,
            connected_weak_components: vec![],
        });

        (slf, NodeIndex { index })
    }

    pub fn add_node(
        &mut self,
        value: S,
        connection_points: (NodeIndex, impl IntoIterator<Item = NodeIndex>),
    ) -> NodeIndex {
        let index = self.graph.add_node(NetworkNode {
            node_info: value,
            connected_weak_components: vec![],
        });

        self.graph.add_edge(index, connection_points.0.index, ());

        for connection in connection_points.1.into_iter() {
            self.graph.add_edge(index, connection.index, ());
        }

        NodeIndex { index }
    }

    pub fn remove_node<'a>(
        &'a mut self,
        index: NodeIndex,
    ) -> (
        S,
        impl IntoIterator<Item = W> + use<'a, S, W>,
        impl IntoIterator<Item = (Self, impl IntoIterator<Item = NodeUpdate>)> + use<'a, S, W>,
    ) {
        let NetworkNode {
            node_info,
            connected_weak_components,
        } = self.graph.remove_node(index.index).unwrap();

        let mut components = petgraph::algo::tarjan_scc(&self.graph);

        if components.pop() == None {
            todo!("The last node in a network was removed")
        }

        // All remaining components (if any, will be turned into other networks)
        let move_to_another_network = components;

        let new_networks =
            move_to_another_network
                .into_iter()
                .enumerate()
                .map(|(network_index, component)| {
                    let connections: Vec<_> = component
                        .iter()
                        .map(|idx| self.graph.edges(*idx))
                        .flat_map(|edges| edges)
                        .map(|edge| (edge.source(), edge.target()))
                        .collect();

                    debug_assert_eq!(component.len(), connections.len());

                    let mut new_network = Self {
                        graph: StableUnGraph::default(),
                    };

                    let new_indices: Vec<_> = component
                        .iter()
                        .map(|idx| {
                            new_network
                                .graph
                                .add_node(self.graph.remove_node(*idx).unwrap())
                        })
                        .collect();

                    for (source, dest) in connections {
                        let source_idx_old = component.iter().position(|v| *v == source).unwrap();
                        let dest_idx_old = component.iter().position(|v| *v == dest).unwrap();

                        new_network.graph.add_edge(
                            new_indices[source_idx_old],
                            new_indices[dest_idx_old],
                            (),
                        );
                    }

                    let node_updates = new_network
                        .graph
                        .node_references()
                        .map(|node| NodeUpdate {
                            old_node: NodeIndex {
                                index: component
                                    [new_indices.iter().position(|v| *v == node.0).unwrap()],
                            },
                            new_network: network_index,
                            new_node: NodeIndex { index: node.0 },
                        })
                        .collect::<Vec<_>>();

                    (new_network, node_updates)
                });

        (
            node_info,
            connected_weak_components.into_iter().flatten(),
            new_networks,
        )
    }

    pub fn add_weak_element(&mut self, index: NodeIndex, value: W) -> WeakIndex {
        let weak_components = &mut self
            .graph
            .node_weight_mut(index.index)
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

    pub fn remove_weak_element(&mut self, node_index: NodeIndex, weak_index: WeakIndex) -> W {
        self.graph
            .node_weight_mut(node_index.index)
            .unwrap()
            .connected_weak_components[weak_index.index]
            .take()
            .unwrap()
    }

    pub fn add_node_merging(
        &mut self,
        value: S,
        connection_points_first: (NodeIndex, impl IntoIterator<Item = NodeIndex>),
        other: Self,
        connection_points_other: (NodeIndex, impl IntoIterator<Item = NodeIndex>),
    ) -> (NodeIndex, impl IntoIterator<Item = NodeUpdate>) {
        let new_node_id = self.add_node(value, connection_points_first);

        let Self { graph } = other;

        let node_index_lookup = join_graphs(&mut self.graph, graph);

        self.graph.add_edge(
            new_node_id.index,
            node_index_lookup[&connection_points_other.0.index],
            (),
        );

        for connection in connection_points_other.1 {
            self.graph
                .add_edge(new_node_id.index, node_index_lookup[&connection.index], ());
        }

        let v = node_index_lookup.into_iter().map(|v| NodeUpdate {
            old_node: NodeIndex { index: v.0 },
            new_network: usize::MAX,
            new_node: NodeIndex { index: v.1 },
        });

        (new_node_id, v)
    }
}

fn join_graphs<T, S: Default>(
    first: &mut StableUnGraph<T, S>,
    mut second: StableUnGraph<T, S>,
) -> HashMap<petgraph::prelude::NodeIndex, petgraph::prelude::NodeIndex> {
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

    old_node_indices.into_iter().zip(new_node_indices).collect()
}
