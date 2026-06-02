use std::num::NonZero;

use crate::interface::{
    TODO,
    middle::{ItemBitSet, MiddleAssemblerID, MiddleInserterID},
};

struct EmptyBeltID;
struct PureBeltID(Item, TODO);
struct SushiBeltID;
pub struct Item(NonZero<u16>);
pub struct Recipe;
struct BeltPos;

pub struct PowerGridID;
struct PoleID;

pub struct StaticProducerType;
pub struct AccumulatorType;
pub struct AssemblerType;

pub struct AssemblerID;

pub type ItemStackCount = u8;

struct FullAssemblerInfo {
    inputs: Vec<(Item, ItemStackCount)>,
    outputs: Vec<(Item, ItemStackCount)>,
    timer: f32,
    prod: f32,
}

struct PowerMult;

struct SingleGridUpdaters {
    assembler_updaters: Vec<!>,
}

type InserterID = TODO;

struct InserterChange<'a> {
    prev_items: &'a ItemBitSet,
    new_items: &'a ItemBitSet,
    old_backend_id: InserterID,
    middle_id: MiddleInserterID,
    // TODO: Connections
    sources: Vec<!>,
    dests: Vec<!>,
}

struct ItemTransaction<'a> {
    inserters: Vec<InserterChange<'a>>,
}

trait Backend: BeltBackend + PowerBackend {
    fn apply_transaction(&mut self, changes: ItemTransaction) {
        // Note: when I say inserter, this could also apply to all other connections between stuff

        // Removed inserters + all inserters touching anything that is changing
        let inserters_to_remove: !;

        let canonical_inserters = todo!("remove_all_inserters(inserters_to_remove)");

        // Update the chests, belts which should not have any inserters connected anymore

        let belt_id_map = todo!("Apply belt changes");
        let chest_id_map = todo!("Apply chest changes");

        let updated_inserters = todo!("canonical_inserters.map(|id| id = id_map[id])");

        // Reinsert all inserters again
    }
}

struct ItemLists {
    item_lists: Vec<SingleItemList>,
}

struct SingleItemList;

type EntityCount = u32;

struct PowerGridSplitter<const N: usize> {
    // These should be FnOnce
    static_power_split: fn(ty: StaticProducerType) -> [EntityCount; N],
    accumulator_split: fn(ty: AccumulatorType) -> [EntityCount; N],
    assembler_split: fn(recipe: Recipe, id: AssemblerID) -> usize,
}

trait PowerBackend {
    fn new_grid() -> PowerGridID;
    /// The grid needs to be empty.
    fn delete_grid(id: PowerGridID);

    fn get_power_mult(id: PowerGridID) -> PowerMult;

    fn add_static_producer(grid: PowerGridID, ty: StaticProducerType);
    fn remove_static_producer(grid: PowerGridID, ty: StaticProducerType);

    fn add_accumulator(grid: PowerGridID, ty: AccumulatorType);
    fn remove_accumulator(grid: PowerGridID, ty: AccumulatorType);
    fn get_accumulator_charge_perc(grid: PowerGridID) -> f32;

    // If needed I will make a get_info_full which might be O(n) and a get_info_cheap which will only get you anything that is easy to get.
    // Similar for inserters, transport lines, etc
    fn get_assembler_info_full(
        grid: PowerGridID,
        _ty: AssemblerType,
        recipe: Recipe,
        id: AssemblerID,
        lists: &ItemLists,
    ) -> FullAssemblerInfo;
    fn add_assembler(
        grid: PowerGridID,
        ty: AssemblerType,
        recipe: Recipe,
        lists: &mut ItemLists,
    ) -> AssemblerID;
    fn remove_assembler(
        grid: PowerGridID,
        _ty: AssemblerType,
        recipe: Recipe,
        id: AssemblerID,
        lists: &mut ItemLists,
    ) -> impl Iterator<Item = (Item, ItemStackCount)>;

    fn merge_power_grids(
        grids: impl Iterator<Item = PowerGridID>,
        lists: &mut ItemLists,
    ) -> PowerGridID;
    fn split_power_grid<const N: usize>(
        grid: PowerGridID,
        split: PowerGridSplitter<N>,
    ) -> [PowerGridID; N];

    fn get_update_functions(&mut self) -> Vec<SingleGridUpdaters>;
}

// FIXME: These should be called TransportLines
trait BeltBackend {
    fn new_empty() -> EmptyBeltID;

    fn make_sushi_from_empty(id: EmptyBeltID) -> SushiBeltID;
    fn make_sushi(id: PureBeltID) -> SushiBeltID;
    fn get_sushi_belt_content(id: SushiBeltID) -> impl Iterator<Item = Item>;
    fn make_pure_from_empty(id: EmptyBeltID, goal_item: Item) -> PureBeltID;
    /// This panics if it is not valid
    fn make_pure(id: SushiBeltID, goal_item: Item) -> PureBeltID;

    fn merge_sushi(front: SushiBeltID, back: SushiBeltID) -> SushiBeltID;
    fn merge_pure(front: PureBeltID, back: PureBeltID) -> PureBeltID;

    fn get_sushi_content(id: SushiBeltID, pos: BeltPos) -> Item;
    fn get_pure_content(id: PureBeltID, pos: BeltPos) -> bool;

    /// The pos is the belt pos that will become the frontmost position of the back belt
    fn split_sushi(id: SushiBeltID, pos: BeltPos) -> [SushiBeltID; 2];
    /// The pos is the belt pos that will become the frontmost position of the back belt
    fn split_pure(id: PureBeltID, pos: BeltPos) -> [PureBeltID; 2];

    fn delete_sushi(id: SushiBeltID) -> impl Iterator<Item = Item>;
    fn delete_pure(id: PureBeltID) -> u32;

    // TODO: Update procs
}

trait InserterBackend {
    // Requires
    // fn get_info
}
