#[cfg(feature = "client")]
use egui_show_info::{Cache, EguiDisplayable, InfoExtractor, RemoveSuffix, ShowInfo};
#[cfg(feature = "client")]
use get_size::GetSize;
use petgraph::Directed;
use petgraph::{EdgeType, csr::IndexType};
use std::ops::Deref;
use std::ops::DerefMut;

use enum_map::EnumArray;

use std::cmp::Eq;
use std::hash::Hash;

#[derive(
    Debug,
    Clone,
    Copy,
    Default,
    PartialEq,
    Eq,
    PartialOrd,
    Ord,
    serde::Deserialize,
    serde::Serialize,
    Hash,
)]
pub struct NodeIndex<Ix: IndexType = petgraph::stable_graph::DefaultIx> {
    pub node_index: petgraph::stable_graph::NodeIndex<Ix>,
}

#[cfg(feature = "client")]
impl<E: InfoExtractor<Self, Info>, Info: EguiDisplayable, Ix: IndexType> ShowInfo<E, Info>
    for NodeIndex<Ix>
{
    fn show_fields<C: Cache<String, Info>>(
        &self,
        _extractor: &mut E,
        _ui: &mut egui::Ui,
        _path: String,
        _cache: &mut C,
    ) {
    }
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

#[cfg(feature = "client")]
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

#[cfg(feature = "client")]
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

#[cfg(feature = "client")]
impl<
    N: ShowInfo<Extractor, Info>,
    E: ShowInfo<Extractor, Info>,
    Ty: EdgeType,
    Ix: IndexType + ShowInfo<Extractor, Info>,
    Info: EguiDisplayable,
    Extractor: InfoExtractor<Self, Info>
        + InfoExtractor<N, Info>
        + InfoExtractor<E, Info>
        + InfoExtractor<Ix, Info>,
> ShowInfo<Extractor, Info> for StableGraph<N, E, Ty, Ix>
{
    fn show_fields<C: Cache<String, Info>>(
        &self,
        extractor: &mut Extractor,
        ui: &mut egui::Ui,
        mut path: String,
        cache: &mut C,
    ) {
        egui::CollapsingHeader::new("Nodes")
            .default_open(false)
            .show(ui, |ui| {
                for (i, node) in self.node_weights().enumerate() {
                    let index = format!("{}", i);
                    path.push_str(&index);
                    node.show_info(extractor, ui, &path, cache);
                    path.remove_suffix(&index);
                }
            });

        egui::CollapsingHeader::new("Edges")
            .default_open(false)
            .show(ui, |ui| {
                for (i, edge) in self.edge_weights().enumerate() {
                    let index = format!("{}", i);
                    path.push_str(&index);
                    edge.show_info(extractor, ui, &path, cache);
                    path.remove_suffix(&index);
                }
            });
    }
}

#[derive(Debug, serde::Deserialize, serde::Serialize)]
pub struct Graph<N, E, Ty: EdgeType = Directed, Ix: IndexType = petgraph::graph::DefaultIx> {
    pub graph: petgraph::graph::Graph<N, E, Ty, Ix>,
}

impl<N: Clone, E: Clone, Ty: EdgeType, Ix: IndexType> Clone for Graph<N, E, Ty, Ix> {
    fn clone(&self) -> Self {
        Self {
            graph: self.graph.clone(),
        }
    }
}

impl<N, E, Ty: EdgeType, Ix: IndexType> Default for Graph<N, E, Ty, Ix> {
    fn default() -> Self {
        Self {
            graph: Default::default(),
        }
    }
}

impl<N, E, Ty: EdgeType, Ix: IndexType> Deref for Graph<N, E, Ty, Ix> {
    type Target = petgraph::graph::Graph<N, E, Ty, Ix>;
    fn deref(&self) -> &Self::Target {
        &self.graph
    }
}

impl<N, E, Ty: EdgeType, Ix: IndexType> DerefMut for Graph<N, E, Ty, Ix> {
    fn deref_mut(&mut self) -> &mut Self::Target {
        &mut self.graph
    }
}

#[cfg(feature = "client")]
impl<N: GetSize, E: GetSize, Ty: EdgeType, Ix: IndexType + GetSize> GetSize
    for Graph<N, E, Ty, Ix>
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

#[cfg(feature = "client")]
impl<
    N: ShowInfo<Extractor, Info>,
    E: ShowInfo<Extractor, Info>,
    Ty: EdgeType,
    Ix: IndexType + ShowInfo<Extractor, Info>,
    Info: EguiDisplayable,
    Extractor: InfoExtractor<Self, Info>
        + InfoExtractor<N, Info>
        + InfoExtractor<E, Info>
        + InfoExtractor<Ix, Info>,
> ShowInfo<Extractor, Info> for Graph<N, E, Ty, Ix>
{
    fn show_fields<C: Cache<String, Info>>(
        &self,
        extractor: &mut Extractor,
        ui: &mut egui::Ui,
        mut path: String,
        cache: &mut C,
    ) {
        egui::CollapsingHeader::new("Nodes")
            .default_open(false)
            .show(ui, |ui| {
                for (i, node) in self.node_weights().enumerate() {
                    let index = format!("{}", i);
                    path.push_str(&index);
                    node.show_info(extractor, ui, &path, cache);
                    path.remove_suffix(&index);
                }
            });

        egui::CollapsingHeader::new("Edges")
            .default_open(false)
            .show(ui, |ui| {
                for (i, edge) in self.edge_weights().enumerate() {
                    let index = format!("{}", i);
                    path.push_str(&index);
                    edge.show_info(extractor, ui, &path, cache);
                    path.remove_suffix(&index);
                }
            });
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

#[cfg(feature = "client")]
impl<K: EnumArray<V> + EnumArray<Option<V>>, V: GetSize> GetSize for EnumMap<K, V> {
    fn get_heap_size(&self) -> usize {
        self.values().map(|v| v.get_heap_size()).sum()
    }
}

#[cfg(feature = "client")]
impl<
    K: EnumArray<V> + EnumArray<Option<V>>,
    V: ShowInfo<E, Info>,
    E: InfoExtractor<Self, Info> + InfoExtractor<V, Info>,
    Info: EguiDisplayable,
> ShowInfo<E, Info> for EnumMap<K, V>
{
    fn show_fields<C: Cache<String, Info>>(
        &self,
        extractor: &mut E,
        ui: &mut egui::Ui,
        mut path: String,
        cache: &mut C,
    ) {
        for (i, v) in self.values().enumerate() {
            let str = format!("{}", i);
            path.push_str(&str);
            v.show_info(extractor, ui, &path, cache);
            path.remove_suffix(&str);
        }
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

#[cfg(feature = "client")]
impl<T: GetSize> GetSize for Mutex<T> {
    fn get_heap_size(&self) -> usize {
        self.mutex.lock().get_heap_size()
    }
}

#[cfg(feature = "client")]
impl<
    T: ShowInfo<E, Info>,
    E: InfoExtractor<Self, Info> + InfoExtractor<T, Info>,
    Info: EguiDisplayable,
> ShowInfo<E, Info> for Mutex<T>
{
    fn show_fields<C: Cache<String, Info>>(
        &self,
        extractor: &mut E,
        ui: &mut egui::Ui,
        path: String,
        cache: &mut C,
    ) {
        self.lock().show_info(extractor, ui, &path, cache);
    }
}

#[derive(Debug, Clone, serde::Serialize, serde::Deserialize, Default)]
pub struct BiMap<A: Hash + Eq + Ord, B: Hash + Eq + Ord> {
    pub map: bimap::BiBTreeMap<A, B>,
}

impl<A: Hash + Eq + Ord, B: Hash + Eq + Ord> BiMap<A, B> {
    pub fn new() -> Self {
        Self {
            map: bimap::BiBTreeMap::new(),
        }
    }
}

impl<A: Hash + Eq + Ord, B: Hash + Eq + Ord> Deref for BiMap<A, B> {
    type Target = bimap::BiBTreeMap<A, B>;
    fn deref(&self) -> &Self::Target {
        &self.map
    }
}

impl<A: Hash + Eq + Ord, B: Hash + Eq + Ord> DerefMut for BiMap<A, B> {
    fn deref_mut(&mut self) -> &mut Self::Target {
        &mut self.map
    }
}

#[cfg(feature = "client")]
impl<A: Hash + Eq + Ord + GetSize, B: Hash + Eq + Ord + GetSize> GetSize for BiMap<A, B> {
    fn get_heap_size(&self) -> usize {
        self.map.left_values().map(|v| v.get_size()).sum::<usize>()
            + self.map.right_values().map(|v| v.get_size()).sum::<usize>()
    }
}

impl<A: Hash + Eq + Ord, B: Hash + Eq + Ord> FromIterator<(A, B)> for BiMap<A, B> {
    fn from_iter<T: IntoIterator<Item = (A, B)>>(iter: T) -> Self {
        Self {
            map: iter.into_iter().collect(),
        }
    }
}

#[cfg(feature = "client")]
impl<
    A: ShowInfo<Extractor, Info> + Eq + Ord + Hash,
    B: ShowInfo<Extractor, Info> + Eq + Ord + Hash,
    Info: EguiDisplayable,
    Extractor: InfoExtractor<Self, Info> + InfoExtractor<A, Info> + InfoExtractor<B, Info>,
> ShowInfo<Extractor, Info> for BiMap<A, B>
{
    fn show_fields<C: Cache<String, Info>>(
        &self,
        _extractor: &mut Extractor,
        _ui: &mut egui::Ui,
        _path: String,
        _cache: &mut C,
    ) {
    }
}

#[derive(Debug, Clone, serde::Serialize, serde::Deserialize)]
#[repr(transparent)]
pub struct BitBox {
    bitbox: bitvec::prelude::BitBox,
}

#[cfg(feature = "client")]
impl GetSize for BitBox {
    fn get_heap_size(&self) -> usize {
        self.bitbox.len().div_ceil(std::mem::size_of::<usize>())
    }
}

#[cfg(feature = "client")]
impl<Info: EguiDisplayable, Extractor: InfoExtractor<Self, Info>> ShowInfo<Extractor, Info>
    for BitBox
{
    fn show_fields<C: Cache<String, Info>>(
        &self,
        _extractor: &mut Extractor,
        _ui: &mut egui::Ui,
        _path: String,
        _cache: &mut C,
    ) {
    }
}

impl Deref for BitBox {
    type Target = bitvec::prelude::BitBox;

    fn deref(&self) -> &Self::Target {
        &self.bitbox
    }
}

impl DerefMut for BitBox {
    fn deref_mut(&mut self) -> &mut Self::Target {
        &mut self.bitbox
    }
}

impl From<bitvec::prelude::BitBox> for BitBox {
    fn from(value: bitvec::prelude::BitBox) -> Self {
        Self { bitbox: value }
    }
}

impl From<BitBox> for bitvec::prelude::BitBox {
    fn from(value: BitBox) -> Self {
        value.bitbox
    }
}

#[derive(Debug, Clone)]
pub struct RamUsage(pub usize);

impl std::fmt::Display for RamUsage {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let s = match self.0 {
            v @ 0..1024 => format!("{:.02} B", v as f64),
            v @ 1024..1048576 => format!("{:.02} KB", v as f64 / 1024.0),
            v @ 1048576..1073741824 => format!("{:.02} MB", v as f64 / 1048576.0),
            v @ 1073741824.. => format!("{:.02} GB", v as f64 / 1073741824.0),
        };

        write!(f, "{}", s)
    }
}

pub struct RAMExtractor;

#[cfg(feature = "client")]
impl<T: GetSize> InfoExtractor<T, RamUsage> for RAMExtractor {
    fn extract_info(&mut self, value: &T) -> RamUsage {
        RamUsage(value.get_size())
    }
}
