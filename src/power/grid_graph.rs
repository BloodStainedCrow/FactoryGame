use std::{collections::HashMap, iter};

use petgraph::prelude::{DiGraphMap, GraphMap};

use crate::{
    frontend::world::{
        tile::{AssemblerID, MachineID},
        Position,
    },
    item::{IdxTrait, WeakIdxTrait},
};

#[derive(Debug, Clone, serde::Deserialize, serde::Serialize)]
pub(super) struct GridGraph<RecipeIdxType: WeakIdxTrait> {
    /// The connections in this Graph denote how "electricity" and therefore the power grid connections flow.
    grid: DiGraphMap<GridElement<RecipeIdxType>, ()>,
}

#[derive(
    Debug, Clone, Copy, serde::Deserialize, serde::Serialize, PartialEq, Eq, Hash, PartialOrd, Ord,
)]
enum GridElement<RecipeIdxType: WeakIdxTrait> {
    Pole(Position),
    Machine((Position, MachineID<RecipeIdxType>)),
}

pub(super) struct PoleRemovalInfo<RecipeIdxType: WeakIdxTrait> {
    now_unpowered_machines: Vec<Position>,
    result: Option<SplitResultInfo<RecipeIdxType>>,
}

pub(super) struct SplitResultInfo<RecipeIdxType: WeakIdxTrait> {
    other_graphs: Vec<GridGraph<RecipeIdxType>>,
    split: PowerGridSplittingInstructions<RecipeIdxType>,
}

pub(super) struct PowerGridSplittingInstructions<RecipeIdxType: WeakIdxTrait> {
    number_of_parts: u8,
    pole_map: HashMap<Position, u8>,
    machine_map_pos: HashMap<Position, u8>,
    machine_map_id: HashMap<MachineID<RecipeIdxType>, u8>,
}

struct SplitInfo<RecipeIdxType: WeakIdxTrait> {
    others: Vec<GridGraph<RecipeIdxType>>,
    now_unpowered: Vec<Position>,
}

impl<RecipeIdxType: IdxTrait> GridGraph<RecipeIdxType> {
    // fn is_contiguous(&self) -> bool {
    //     let pole_graph = self.grid.filter_map(
    //         |_, n| matches!(n, GridElement::Pole(_)).then_some(n),
    //         |_, e| Some(e),
    //     );

    //     petgraph::algo::connected_components(&pole_graph) == 1
    // }

    pub fn new() -> Self {
        Self {
            grid: GraphMap::new(),
        }
    }

    pub fn poles<'a>(&'a self) -> impl IntoIterator<Item = Position> + use<'a, RecipeIdxType> {
        self.grid.nodes().filter_map(|e| match e {
            GridElement::Pole(position) => Some(position),
            GridElement::Machine(_) => None,
        })
    }

    fn split_if_needed(self) -> (Self, SplitInfo<RecipeIdxType>) {
        // TODO: This is overkill. My graph follows very simple rules so probably a simpler algorithm could be used (which would be faster)
        let new_networks = petgraph::algo::tarjan_scc(&self.grid);

        assert!(!new_networks.is_empty());

        let needs_splitting = new_networks.len() == 1;

        if !needs_splitting {
            return (self, todo!());
        }

        // NOTE: The order of the components is not specified (and could probably be non-deterministic) so we need to make sure it is well defined here!
        // TODO: Implement that

        // We are done. Assert that it is now fixed
        self.assert_invariants();

        todo!()
    }

    fn remove_orphaned_machines(&mut self) -> Vec<Position> {
        todo!()
    }

    fn assert_invariants(&self) {
        #[cfg(debug_assertions)]
        {
            let connected_components = petgraph::algo::tarjan_scc(&self.grid);
            debug_assert!(connected_components.len() == 1, "After splitting the graph has multiple sets of powerpoles which are not connected, and should therefore have been split");
            debug_assert!(
                connected_components[0].len()
                    == self
                        .grid
                        .nodes()
                        .filter(|n| matches!(n, GridElement::Pole(_)))
                        .count(),
                "Not all power poles are reachable from all other power poles!"
            )
        }

        // TODO: According to https://en.wikipedia.org/wiki/Reachability#Algorithms Thorup would be more what I want here, but it is not implemented in petgraph
        debug_assert!(
            petgraph::algo::dijkstra(
                &self.grid,
                self.grid
                    .nodes()
                    .find(|n| matches!(n, GridElement::Pole(_)))
                    .unwrap(),
                None,
                |_| 1
            )
            .len()
                == self.grid.node_count(),
            "Not every Node is reachable from some (and therefore all) powerpoles"
        )
    }

    pub fn add_pole(
        &mut self,
        pole_pos: Position,
        pole_connections: impl IntoIterator<Item = Position>,
    ) {
        self.grid.add_node(GridElement::Pole(pole_pos));

        for connection in pole_connections {
            dbg!(self.grid.nodes());
            // TODO:
            // assert!(self.grid.contains_node(GridElement::Pole(connection)));

            self.grid.add_edge(
                GridElement::Pole(pole_pos),
                GridElement::Pole(connection),
                (),
            );
            self.grid.add_edge(
                GridElement::Pole(connection),
                GridElement::Pole(pole_pos),
                (),
            );
        }

        self.assert_invariants();
    }

    pub fn add_machine(
        &mut self,
        machine_pos: Position,
        machine_id: MachineID<RecipeIdxType>,
        pole_connections: impl IntoIterator<Item = Position>,
    ) {
        let new_id = self
            .grid
            .add_node(GridElement::Machine((machine_pos, machine_id)));

        for connection in pole_connections {
            assert!(self.grid.contains_node(GridElement::Pole(connection)));

            self.grid
                .add_edge(GridElement::Pole(connection), new_id, ());
        }

        self.assert_invariants();
    }

    pub fn remove_machine(&mut self, machine_pos: Position, machine_id: MachineID<RecipeIdxType>) {
        self.grid
            .remove_node(GridElement::Machine((machine_pos, machine_id)));

        self.assert_invariants();
    }

    pub fn remove_pole(mut self, pole_pos: Position) -> (Self, PoleRemovalInfo<RecipeIdxType>) {
        self.grid.remove_node(GridElement::Pole(pole_pos));

        let info;
        (self, info) = self.split_if_needed();

        if info.others.is_empty() {
            (
                self,
                PoleRemovalInfo {
                    now_unpowered_machines: info.now_unpowered,
                    result: None,
                },
            )
        } else {
            let (pole_map, machine_map_pos, machine_map_id) = info
                .others
                .iter()
                .enumerate()
                .map(|(grid_number, grid)| (u8::try_from(grid_number + 1).unwrap(), grid))
                .flat_map(|(grid_number, grid)| iter::repeat(grid_number).zip(grid.grid.nodes()))
                .fold(
                    (HashMap::default(), HashMap::default(), HashMap::default()),
                    |(mut pole_map, mut machine_map_pos, mut machine_map_id),
                     (grid_number, element)| {
                        match element {
                            GridElement::Pole(position) => {
                                pole_map.insert(position, grid_number);
                            },
                            GridElement::Machine((pos, id)) => {
                                machine_map_pos.insert(pos, grid_number);
                                machine_map_id.insert(id, grid_number);
                            },
                        };

                        (pole_map, machine_map_pos, machine_map_id)
                    },
                );

            let len = info.others.len();

            (
                self,
                PoleRemovalInfo {
                    now_unpowered_machines: info.now_unpowered,
                    result: Some(SplitResultInfo {
                        other_graphs: info.others,
                        split: PowerGridSplittingInstructions {
                            number_of_parts: (len + 1)
                                .try_into()
                                .expect("Tried to split into more than u8::MAX parts?!?"),
                            pole_map,
                            machine_map_pos,
                            machine_map_id,
                        },
                    }),
                },
            )
        }
    }

    pub fn join(
        self,
        other: Self,
        new_pole_pos: Position,
        new_pole_connections: impl IntoIterator<Item = Position>,
    ) -> Self {
        let mut ret = self;

        for edge in other.grid.all_edges() {
            dbg!(&edge);
            ret.grid.add_edge(edge.0, edge.1, ());
        }

        ret.add_pole(new_pole_pos, new_pole_connections);

        ret.assert_invariants();

        ret
    }
}
