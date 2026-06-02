use std::num::NonZero;

use smallvec::SmallVec;

use crate::interface::backend::{AssemblerID, Item, ItemStackCount, PowerGridID, Recipe};

pub struct MiddleInserterID;
pub struct MiddleAssemblerID;
pub struct MiddleTransportLineID;

// This module has most of the complicated logic with regards to pureness of belts/inserters

// TODO: I need to properly define which direction I am counting!!!
pub type TransportLineLength = u32;

trait Middle: TransportMiddle {}

enum TransportLineEnd {
    Front,
    Back,
}

// This is an interface to the item graph which will handle updates to the underlying storage in the backend
// The frontend uses this interface for communicating with the simulation
trait TransportMiddle: ItemGraphMiddle {
    fn add_transport_line(length: TransportLineLength) -> MiddleTransportLineID;
    fn delete_transport_line(id: MiddleTransportLineID) -> impl Iterator<Item = (Item, usize)>;

    fn merge_transport_line(
        front: MiddleTransportLineID,
        back: MiddleTransportLineID,
    ) -> MiddleTransportLineID;
    fn split_transport_line(
        id: MiddleTransportLineID,
        first_pos_of_back_line: TransportLineLength,
    ) -> [MiddleTransportLineID; 2]; // TODO: This should be maybe an enum map or smth

    // Add and merge can emulate lengthening.
    // I might want to add some specialization for that for performance
    fn lengthen(
        id: MiddleTransportLineID,
        amount: TransportLineLength,
        side: TransportLineEnd,
    ) -> MiddleTransportLineID {
        let intermediate = Self::add_transport_line(amount);

        match side {
            TransportLineEnd::Front => Self::merge_transport_line(intermediate, id),
            TransportLineEnd::Back => Self::merge_transport_line(id, intermediate),
        }
    }

    // Split and delete can emulate shortening
    // I might want to add some specialization for that for performance
    fn shorten(
        id: MiddleTransportLineID,
        amount: TransportLineLength,
        side: TransportLineEnd,
    ) -> (MiddleTransportLineID, impl Iterator<Item = (Item, usize)>) {
        let split = match side {
            TransportLineEnd::Front => amount,
            TransportLineEnd::Back => todo!("Get line length") as u32 - amount,
        };

        let [front, back] = Self::split_transport_line(id, split);

        let (kept, delete) = match side {
            TransportLineEnd::Front => (back, front),
            TransportLineEnd::Back => (front, back),
        };

        (kept, Self::delete_transport_line(delete))
    }

    /// The source must NOT have a sideload already!
    fn add_sideload(
        source: MiddleTransportLineID,
        dest: (MiddleTransportLineID, TransportLineLength),
    );
    // TODO: Will this return items?
    fn remove_sideload(source: MiddleTransportLineID);

    // TODO: Rendering
}

struct SideloadID;
struct InserterID(NonZero<u32>);

pub struct ItemBitSet {
    items: SmallVec<[Item; 8]>,
}

// This will likely be a weird bitset shenanigans to fit this in a u32;
struct ItemGraphNodeID(u32);

const NUM_INSERTER_FILTER_SLOTS: usize = 4;
struct InserterFilterList([Item; NUM_INSERTER_FILTER_SLOTS]);

struct Inserter {
    filter: Option<InserterFilterList>,

    // TODO: Actual BackendID

    // This is effectively an intrusive linked list
    next_inserter_with_same_src: Option<InserterID>,
    dest_node: ItemGraphNodeID,
}

// This will likely be a weird bitset shenanigans to fit this in a u32;
struct TransportLineOrSplitterID(NonZero<u32>);

struct TransportLine {
    end_of_line_output: Option<TransportLineOrSplitterID>,
    // This will likely not be Option but conceptionally it should be
    /// This is the head of a linked list
    output_inserters: Option<InserterID>,
}

// This is the full Assembler info used by the frontend
struct Assembler {
    modules: !,
    recipe: Recipe,
    grid: PowerGridID,
    id: AssemblerID,

    // This is the head of a linked list
    output_inserters: Option<InserterID>,
}

struct Chest {
    slot_limit: Option<!>,
    /// An addition limit to the stack size inside this chest. Used for floor chests (which is how I handle inserters putting items on the floor)
    // TODO: Do I want this? It will add some overhead for 99% of chests to never use it
    stack_size_limit: Option<ItemStackCount>,
}

/// Cannot be the destination of an inserter
struct MiningDrill {
    // TODO: Do I want this, or have an "internal inserter" situation?
    dest_node: ItemGraphNodeID,
}

struct Splitter {
    // Settings
    // TODO: Maybe these need to store their inputs, since they might not have a tile in the world but must still be available for rendering unless I want to do a O(n) search
    outputs: [MiddleTransportLineID; 2],
}

struct Lab;

struct ItemGraph {
    // Due to the way our graph traversal works (GenKill algorithm which always starts at a node so we can just keep track)
    // We only need to know each Nodes output edges.
    // Notably, I can (if I want to) even make the edges unaddressable without the source node.
    // This would mean that to get information about say an inserter, you would need the ID of the source Assembler. This seems slightly inconvenient but might be worth it if that saves a lot of RAM
    // Downside is, that storing a Vec in each Node would bloat its size a LOT and might lead to a lot of heap fragmentation which is already a huge issue. (One I hope to improve in this version)
    // Petgraph has a vec of edges and a intrusive linked list inside the edges to iterate over the edges of a node, and I think I will copy that idea

    // NODES
    // -- SOURCES
    // TODO: Train stations
    // TODO: Logistic Bot Network
    // TODO: Anything with a fuel slot? Do I want fuel slots, or should that just be a separate entity without power usage with their own recipes which need fuel (which would limit the fuel usage a lot)?
    assemblers: Vec<Assembler>,
    mining_drills: Vec<MiningDrill>,

    transport_lines: Vec<TransportLine>,
    splitters: Vec<Splitter>,
    chests: Vec<Chest>,
    // I can either have Labs act as a chest like factorio, or as a sink
    labs: Vec<Lab>,

    // EDGES
    inserters: Vec<Inserter>,
}

trait ItemGraphMiddle: ItemGraphMiddleInner {
    // TODO:
    // Note(BSC): I want to avoid using an Enum Node, since that will very likely blow up my RAM usage a bunch (as it has in the previous implementation)
    //            But that would mean not using petgraph and rewriting a graph datastructure + algorithms OR adding ANOTHER layer of indirection which also stinks
}

type Backend = !;

trait ItemGraphMiddleInner {
    fn propagate_items(&mut self, start_node: ItemGraphNodeID, backend: &mut Backend);
}
