use std::ops::Deref;
use std::ops::DerefMut;

use get_size::GetSize;
use petgraph::Directed;
use petgraph::{EdgeType, csr::IndexType};

use enum_map::EnumArray;

use std::cmp::Eq;
use std::hash::Hash;

#[derive(Debug, Clone, Copy, PartialEq, Eq, serde::Deserialize, serde::Serialize, Hash)]
pub struct NodeIndex<Ix: IndexType = petgraph::stable_graph::DefaultIx> {
    pub node_index: petgraph::stable_graph::NodeIndex<Ix>,
}

impl Deref for NodeIndex {
    type Target = petgraph::stable_graph::NodeIndex;
    fn deref(&self) -> &Self::Target {
        &self.node_index
    }
}

impl DerefMut for NodeIndex {
    fn deref_mut(&mut self) -> &mut Self::Target {
        &mut self.node_index
    }
}

impl GetSize for NodeIndex {}

#[derive(Debug, serde::Deserialize, serde::Serialize)]
pub struct StableGraph<
    N,
    E,
    Ty: EdgeType = Directed,
    Ix: IndexType = petgraph::stable_graph::DefaultIx,
> {
    pub graph: petgraph::stable_graph::StableGraph<N, E, Ty, Ix>,
}

impl<N: Clone, E: Clone, Ty: EdgeType, Ix: IndexType> Clone for StableGraph<N, E, Ty, Ix> {
    fn clone(&self) -> Self {
        Self {
            graph: self.graph.clone(),
        }
    }
}

impl<N, E, Ty: EdgeType, Ix: IndexType> Default for StableGraph<N, E, Ty, Ix> {
    fn default() -> Self {
        Self {
            graph: Default::default(),
        }
    }
}

impl<N, E, Ty: EdgeType, Ix: IndexType> Deref for StableGraph<N, E, Ty, Ix> {
    type Target = petgraph::stable_graph::StableGraph<N, E, Ty, Ix>;
    fn deref(&self) -> &Self::Target {
        &self.graph
    }
}

impl<N, E, Ty: EdgeType, Ix: IndexType> DerefMut for StableGraph<N, E, Ty, Ix> {
    fn deref_mut(&mut self) -> &mut Self::Target {
        &mut self.graph
    }
}

impl<N: GetSize, E: GetSize, Ty: EdgeType, Ix: IndexType + GetSize> GetSize
    for StableGraph<N, E, Ty, Ix>
{
    fn get_heap_size(&self) -> usize {
        self.graph
            .node_weights()
            .map(|node| node.get_size() + 2 * Ix::get_stack_size())
            .sum::<usize>()
            + self
                .graph
                .edge_weights()
                .map(|edge| edge.get_size() + 4 * Ix::get_stack_size())
                .sum::<usize>()
    }
}

#[derive(Debug, serde::Serialize, serde::Deserialize, Default)]
pub struct EnumMap<K: EnumArray<V> + EnumArray<Option<V>>, V> {
    pub enum_map: enum_map::EnumMap<K, V>,
}

impl<K: EnumArray<V> + EnumArray<Option<V>>, V: Clone> Clone for EnumMap<K, V>
where
    <K as EnumArray<V>>::Array: Clone,
{
    fn clone(&self) -> Self {
        Self {
            enum_map: self.enum_map.clone(),
        }
    }
}
impl<K: EnumArray<V> + EnumArray<Option<V>>, V: Copy> Copy for EnumMap<K, V> where
    <K as EnumArray<V>>::Array: Copy
{
}

impl<K: EnumArray<V> + EnumArray<Option<V>>, V> Deref for EnumMap<K, V> {
    type Target = enum_map::EnumMap<K, V>;
    fn deref(&self) -> &Self::Target {
        &self.enum_map
    }
}

impl<K: EnumArray<V> + EnumArray<Option<V>>, V> DerefMut for EnumMap<K, V> {
    fn deref_mut(&mut self) -> &mut Self::Target {
        &mut self.enum_map
    }
}

impl<K: EnumArray<V> + EnumArray<Option<V>>, V: GetSize> GetSize for EnumMap<K, V> {
    fn get_heap_size(&self) -> usize {
        self.values().map(|v| v.get_heap_size()).sum()
    }
}

#[derive(Debug, serde::Serialize, serde::Deserialize, Default)]
pub struct Mutex<T> {
    pub mutex: parking_lot::Mutex<T>,
}

impl<T> Mutex<T> {
    pub fn new(val: T) -> Self {
        Self {
            mutex: parking_lot::Mutex::new(val),
        }
    }
}

impl<T> Deref for Mutex<T> {
    type Target = parking_lot::Mutex<T>;
    fn deref(&self) -> &Self::Target {
        &self.mutex
    }
}

impl<T> DerefMut for Mutex<T> {
    fn deref_mut(&mut self) -> &mut Self::Target {
        &mut self.mutex
    }
}

impl<T: GetSize> GetSize for Mutex<T> {
    fn get_heap_size(&self) -> usize {
        self.mutex.lock().get_heap_size()
    }
}

#[derive(Debug, Clone, serde::Serialize, serde::Deserialize, Default)]
pub struct BiMap<A: Hash + Eq, B: Hash + Eq> {
    pub map: bimap::BiMap<A, B>,
}

impl<A: Hash + Eq, B: Hash + Eq> BiMap<A, B> {
    pub fn new() -> Self {
        Self {
            map: bimap::BiMap::new(),
        }
    }
}

impl<A: Hash + Eq, B: Hash + Eq> Deref for BiMap<A, B> {
    type Target = bimap::BiMap<A, B>;
    fn deref(&self) -> &Self::Target {
        &self.map
    }
}

impl<A: Hash + Eq, B: Hash + Eq> DerefMut for BiMap<A, B> {
    fn deref_mut(&mut self) -> &mut Self::Target {
        &mut self.map
    }
}

impl<A: Hash + Eq + GetSize, B: Hash + Eq + GetSize> GetSize for BiMap<A, B> {
    fn get_heap_size(&self) -> usize {
        self.map.left_values().map(|v| v.get_size()).sum::<usize>()
            + self.map.right_values().map(|v| v.get_size()).sum::<usize>()
    }
}

impl<A: Hash + Eq, B: Hash + Eq> FromIterator<(A, B)> for BiMap<A, B> {
    fn from_iter<T: IntoIterator<Item = (A, B)>>(iter: T) -> Self {
        Self {
            map: bimap::BiMap::from_iter(iter),
        }
    }
}
