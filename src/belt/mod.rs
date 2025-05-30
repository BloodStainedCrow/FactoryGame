#[allow(clippy::module_inception)]
pub mod belt;
pub mod smart;

pub mod splitter;

mod sushi;

use std::{
    collections::{HashMap, HashSet},
    iter::once,
    marker::PhantomData,
    mem, usize,
};

use crate::{
    data::DataStore,
    inserter::{
        belt_belt_inserter::{BeltBeltInserter, SushiBeltBeltInserter},
        belt_storage_inserter::{BeltStorageInserter, Dir},
        Storage,
    },
    item::{usize_from, Item},
    storage_list::{grid_size, num_recipes, SingleItemStorages},
};
use belt::{Belt, BeltLenType};
use itertools::Itertools;
use log::info;
use petgraph::{
    graph::NodeIndex,
    prelude::StableDiGraph,
    visit::{EdgeRef, NodeRef},
    Direction::Outgoing,
};
use rayon::iter::IntoParallelRefMutIterator;
use rayon::iter::{IndexedParallelIterator, ParallelIterator};
use smart::{InserterAdditionError, Side, SmartBelt, SpaceOccupiedError};
use splitter::{Splitter, SplitterDistributionMode, SushiSplitter};
use sushi::{SushiBelt, SushiInfo};

#[derive(Debug, PartialEq, Clone, Copy, serde::Deserialize, serde::Serialize)]
enum FreeIndex {
    FreeIndex(BeltLenType),
    OldFreeIndex(BeltLenType),
}

#[derive(Debug, Clone, serde::Deserialize, serde::Serialize)]
enum Inserter<RecipeIdxType: WeakIdxTrait> {
    Out(BeltStorageInserter<RecipeIdxType, { Dir::BeltToStorage }>),
    In(BeltStorageInserter<RecipeIdxType, { Dir::StorageToBelt }>),
}

#[derive(
    Debug, Clone, Copy, PartialEq, Eq, Hash, serde::Deserialize, serde::Serialize, PartialOrd, Ord,
)]
pub enum BeltTileId<ItemIdxType: WeakIdxTrait> {
    AnyBelt(usize, PhantomData<ItemIdxType>),
    // SushiBeltId(usize),
    // SmartBeltId(BeltId<ItemIdxType>),
}

use crate::{
    frontend::world::tile::BeltId,
    item::{IdxTrait, WeakIdxTrait},
};

#[cfg(test)]
fn do_update_test<ItemIdxType: IdxTrait>(items: &mut [Option<Item<ItemIdxType>>]) {
    match items {
        [] => {},
        [Some(_), rest @ ..] => do_update_test(rest),
        [None, _rest @ ..] => {
            items.rotate_left(1);
        },
    }
}

#[cfg(test)]
fn do_update_test_bools(items: &mut [bool]) {
    match items {
        [] => {},
        [true, rest @ ..] => do_update_test_bools(rest),
        [false, _rest @ ..] => {
            items.rotate_left(1);
        },
    }
}

#[derive(Debug, Clone, serde::Deserialize, serde::Serialize)]
pub struct BeltStore<ItemIdxType: WeakIdxTrait, RecipeIdxType: WeakIdxTrait> {
    inner: InnerBeltStore<ItemIdxType, RecipeIdxType>,

    any_belts: Vec<AnyBelt<ItemIdxType>>,
    any_belt_holes: Vec<usize>,

    any_splitters: Vec<AnySplitter<ItemIdxType>>,
    any_splitter_holes: Vec<usize>,

    belt_graph: StableDiGraph<BeltTileId<ItemIdxType>, BeltGraphConnection<ItemIdxType>>,
    belt_graph_lookup: HashMap<BeltTileId<ItemIdxType>, NodeIndex>,
}

#[derive(Debug, Clone, Copy, serde::Deserialize, serde::Serialize)]
enum BeltGraphConnection<ItemIdxType: WeakIdxTrait> {
    Sideload {
        dest_belt_pos: BeltLenType,
    },
    BeltBeltInserter {
        source_belt_pos: BeltLenType,
        dest_belt_pos: BeltLenType,
        filter: Item<ItemIdxType>,
    },
    Connected, // Used for handling extremely long belts and splitters
               // Always connects the end of the source to the beginning of the destination
}

#[derive(Debug, Clone, serde::Deserialize, serde::Serialize)]
struct InnerBeltStore<ItemIdxType: WeakIdxTrait, RecipeIdxType: WeakIdxTrait> {
    sushi_belts: Vec<SushiBelt<ItemIdxType, RecipeIdxType>>,
    sushi_belt_holes: Vec<usize>,

    smart_belts: Box<[MultiBeltStore<ItemIdxType, RecipeIdxType>]>,

    belt_belt_inserters: BeltBeltInserterStore<ItemIdxType>,

    pure_splitters: Box<[SplitterStore<ItemIdxType>]>,

    sushi_splitters: Vec<SushiSplitter>,
    sushi_splitter_holes: Vec<usize>,
}

#[derive(Debug, Clone, serde::Deserialize, serde::Serialize)]
struct SplitterStore<ItemIdxType: WeakIdxTrait> {
    // TODO: Holes
    pub pure_splitters: Vec<Splitter>,

    item: PhantomData<ItemIdxType>,
}

impl<ItemIdxType: IdxTrait> SplitterStore<ItemIdxType> {
    pub fn get_splitter_belt_ids<'a>(
        &'a self,
        id: usize,
    ) -> [impl IntoIterator<Item = usize> + use<'a, ItemIdxType>; 2] {
        [
            self.pure_splitters[id].input_belts.iter().copied(),
            self.pure_splitters[id].output_belts.iter().copied(),
        ]
    }
}

#[derive(Debug, Clone, serde::Deserialize, serde::Serialize)]
enum AnyBeltBeltInserter {
    PurePure(usize),
    PureSushi(usize),
    SushiPure(usize),
    SushiSushi(usize),
}

#[derive(Debug, Clone, serde::Deserialize, serde::Serialize)]
struct BeltBeltInserterStore<ItemIdxType: WeakIdxTrait> {
    // FIXME: This is likely VERY slow
    pub pure_to_pure_inserters: Box<
        [Vec<
            Option<(
                BeltBeltInserter,
                (
                    (usize, BeltLenType),
                    (usize, BeltLenType),
                    u8,
                    Option<Item<ItemIdxType>>,
                ),
            )>,
        >],
    >,
    pub pure_to_sushi_inserters: Vec<
        Option<(
            BeltBeltInserter,
            (
                (BeltId<ItemIdxType>, BeltLenType),
                (usize, BeltLenType),
                u8,
                Option<Item<ItemIdxType>>,
            ),
        )>,
    >,
    pub sushi_to_sushi_inserters: Vec<
        Option<(
            BeltBeltInserter,
            (
                (usize, BeltLenType),
                (usize, BeltLenType),
                u8,
                Option<Item<ItemIdxType>>,
            ),
        )>,
    >,

    /// THESE NEVER GET UPDATED, they should only exist temprarily
    temp_sushi_to_smart_inserters: Vec<(
        BeltBeltInserter,
        (
            (usize, BeltLenType),
            (BeltId<ItemIdxType>, BeltLenType),
            u8,
            Option<Item<ItemIdxType>>,
        ),
    )>,
}

#[derive(Debug, Clone, serde::Deserialize, serde::Serialize)]
pub struct BeltBeltInserterInfo<ItemIdxType: WeakIdxTrait> {
    source: (BeltTileId<ItemIdxType>, u16),
    dest: (BeltTileId<ItemIdxType>, u16),
    cooldown: u8,
    item: PhantomData<ItemIdxType>,
}

#[derive(Debug, Clone, serde::Deserialize, serde::Serialize)]
pub enum BeltBeltInserterType {
    Normal,
    Sideload,
}

pub struct BeltBeltInserterAdditionInfo<ItemIdxType: WeakIdxTrait> {
    pub filter: Item<ItemIdxType>,
    pub cooldown: u8,
}

pub struct BreakBeltResultInfo<ItemIdxType: WeakIdxTrait> {
    pub kept_id: BeltTileId<ItemIdxType>,
    pub new_belt: Option<(BeltTileId<ItemIdxType>, Side)>,
}

impl<ItemIdxType: IdxTrait, RecipeIdxType: IdxTrait> InnerBeltStore<ItemIdxType, RecipeIdxType> {
    fn remove_smart_belt(
        &mut self,
        id: BeltId<ItemIdxType>,
    ) -> SmartBelt<ItemIdxType, RecipeIdxType> {
        self.smart_belts[usize_from(id.item.id)].remove_belt(id.index)
    }

    fn get_pure_splitter_belt_ids<'a>(
        &'a self,
        item: Item<ItemIdxType>,
        id: usize,
    ) -> [impl IntoIterator<Item = BeltId<ItemIdxType>> + use<'a, ItemIdxType, RecipeIdxType>; 2]
    {
        let index_to_id = move |index| BeltId { item, index };

        let [input, output] = self.pure_splitters[usize_from(item.id)].get_splitter_belt_ids(id);

        [
            input.into_iter().map(index_to_id),
            output.into_iter().map(index_to_id),
        ]
    }

    fn get_sushi_splitter_belt_ids<'a>(
        &'a self,
        id: usize,
    ) -> [impl IntoIterator<Item = usize> + use<'a, ItemIdxType, RecipeIdxType>; 2] {
        [
            self.sushi_splitters[id].input_belts.iter().copied(),
            self.sushi_splitters[id].output_belts.iter().copied(),
        ]
    }

    fn remove_sushi_belt(&mut self, id: usize) -> SushiBelt<ItemIdxType, RecipeIdxType> {
        let mut temp = SushiBelt::new(1);
        mem::swap(&mut temp, &mut self.sushi_belts[id]);
        temp
    }

    fn get_smart(
        &self,
        smart_belt_id: BeltId<ItemIdxType>,
    ) -> &SmartBelt<ItemIdxType, RecipeIdxType> {
        &self.smart_belts[usize_from(smart_belt_id.item.id)].belts[smart_belt_id.index]
    }

    fn get_smart_mut(
        &mut self,
        smart_belt_id: BeltId<ItemIdxType>,
    ) -> &mut SmartBelt<ItemIdxType, RecipeIdxType> {
        &mut self.smart_belts[usize_from(smart_belt_id.item.id)].belts[smart_belt_id.index]
    }

    fn get_smart_many_mut<const N: usize>(
        &mut self,
        item: Item<ItemIdxType>,
        indices: [usize; N],
    ) -> [&mut SmartBelt<ItemIdxType, RecipeIdxType>; N] {
        self.smart_belts[usize_from(item.id)]
            .belts
            .get_disjoint_mut(indices)
            .unwrap()
    }

    fn get_sushi(&self, sushi_belt_id: usize) -> &SushiBelt<ItemIdxType, RecipeIdxType> {
        &self.sushi_belts[sushi_belt_id]
    }

    fn get_sushi_mut(
        &mut self,
        sushi_belt_id: usize,
    ) -> &mut SushiBelt<ItemIdxType, RecipeIdxType> {
        &mut self.sushi_belts[sushi_belt_id]
    }

    fn make_sushi(&mut self, id: BeltId<ItemIdxType>) -> usize {
        let belt = self.remove_smart_belt(id);

        let sushi = belt.into_sushi_belt();

        let new_id = self.add_sushi_belt(sushi);

        // TODO: This scales poorly
        for bb_inserter in
            &mut self.belt_belt_inserters.pure_to_pure_inserters[usize_from(id.item.id)]
        {
            if let Some(ins) = bb_inserter {
                if ins.1 .0 .0 == id.index {
                    // The (now sushi) belt was the input of this inserter
                    todo!()
                } else if ins.1 .1 .0 == id.index {
                    // The (now sushi) belt was the destination of this inserter
                    todo!()
                }
            }
        }

        for bb_inserter in &mut self.belt_belt_inserters.pure_to_sushi_inserters {
            if let Some(ins) = bb_inserter {
                if ins.1 .0 .0 == id {
                    // The (now sushi) belt was the input of this inserter
                    todo!()
                }
            }
        }

        for bb_inserter in &mut self.belt_belt_inserters.temp_sushi_to_smart_inserters {
            if bb_inserter.1 .1 .0 == id {
                // The (now sushi) belt was the dest of this inserter
                todo!()
            }
        }

        // TODO: Update id whereever necessary

        for ins in &mut self.belt_belt_inserters.pure_to_pure_inserters[usize_from(id.item.id)] {
            if let Some(inner_ins) = ins {
                match (inner_ins.1 .0 .0 == id.index, inner_ins.1 .1 .0 == id.index) {
                    (true, true) => {
                        let Some(old) = ins.take() else {
                            unreachable!()
                        };

                        self.belt_belt_inserters
                            .sushi_to_sushi_inserters
                            .push(Some((
                                old.0,
                                (
                                    (new_id, old.1 .0 .1),
                                    (new_id, old.1 .1 .1),
                                    old.1 .2,
                                    Some(id.item),
                                ),
                            )));
                    },
                    (true, false) => {
                        let Some(old) = ins.take() else {
                            unreachable!()
                        };

                        // This inserter starts on the (now sushi) belt and end on a smart belt. This means that the destination belt HAS to be changed to a sushi belt later
                        self.belt_belt_inserters
                            .temp_sushi_to_smart_inserters
                            .push((
                                old.0,
                                (
                                    (new_id, old.1 .0 .1),
                                    (
                                        BeltId {
                                            item: id.item,
                                            index: old.1 .1 .0,
                                        },
                                        old.1 .1 .1,
                                    ),
                                    old.1 .2,
                                    old.1 .3,
                                ),
                            ));
                    },
                    (false, true) => {
                        let Some(old) = ins.take() else {
                            unreachable!()
                        };

                        // This inserter starts on the (now sushi) belt and end on a smart belt. This means that the destination belt HAS to be changed to a sushi belt later
                        self.belt_belt_inserters.pure_to_sushi_inserters.push(Some((
                            old.0,
                            (
                                (
                                    BeltId {
                                        item: id.item,
                                        index: old.1 .0 .0,
                                    },
                                    old.1 .0 .1,
                                ),
                                (new_id, old.1 .1 .1),
                                old.1 .2,
                                old.1 .3,
                            ),
                        )));
                    },
                    (false, false) => {},
                }
            }
        }

        for ins in &mut self.belt_belt_inserters.pure_to_sushi_inserters {
            if let Some(inner_ins) = ins {
                if inner_ins.1 .0 .0 == id {
                    let Some(old) = ins.take() else {
                        unreachable!()
                    };

                    self.belt_belt_inserters
                        .sushi_to_sushi_inserters
                        .push(Some((
                            old.0,
                            (
                                (new_id, old.1 .0 .1),
                                (old.1 .1 .0, old.1 .1 .1),
                                old.1 .2,
                                old.1 .3,
                            ),
                        )));
                }
            }
        }

        new_id
    }

    fn make_smart(&mut self, index: usize, item: Item<ItemIdxType>) -> BeltId<ItemIdxType> {
        let belt = self.remove_sushi_belt(index);

        let smart = belt.into_smart_belt(item);

        let new_id = self.add_belt(smart);

        // TODO: Update id whereever necessary

        new_id
    }

    fn add_sushi_belt(&mut self, belt: SushiBelt<ItemIdxType, RecipeIdxType>) -> usize {
        let sushi_idx = if let Some(hole) = self.sushi_belt_holes.pop() {
            self.sushi_belts[hole] = belt;
            hole
        } else {
            self.sushi_belts.push(belt);
            self.sushi_belts.len() - 1
        };

        sushi_idx
    }

    fn add_belt(&mut self, belt: SmartBelt<ItemIdxType, RecipeIdxType>) -> BeltId<ItemIdxType> {
        let item = belt.item;
        let smart_idx = self.smart_belts[usize_from(belt.item.id)].add_belt(belt);

        BeltId {
            item,
            index: smart_idx,
        }
    }

    /// Returns the new length of the belt
    fn add_length_smart(
        &mut self,
        id: BeltId<ItemIdxType>,
        amount: BeltLenType,
        side: Side,
    ) -> BeltLenType {
        self.get_smart_mut(id).add_length(amount, side)
    }

    /// Returns the new length of the belt
    fn add_length_sushi(&mut self, id: usize, amount: BeltLenType, side: Side) -> BeltLenType {
        self.get_sushi_mut(id).add_length(amount, side)
    }

    /// Returns the new length of the belt
    fn remove_length_smart(
        &mut self,
        id: BeltId<ItemIdxType>,
        amount: BeltLenType,
        side: Side,
    ) -> BeltLenType {
        todo!()
    }

    /// Returns the new length of the belt
    fn remove_length_sushi(&mut self, id: usize, amount: BeltLenType, side: Side) -> BeltLenType {
        todo!()
    }

    // TODO: What does this return?
    fn merge_smart_belts(&mut self, front: BeltId<ItemIdxType>, back: BeltId<ItemIdxType>) -> () {
        if front.item != back.item {
            todo!("Item mismatch. Do I want to error or panic?");
            panic!("We defintively cannot continue.");
        }

        if front == back {
            todo!("Make circular");

            return;
        }

        let back_belt = self.remove_smart_belt(back);

        take_mut::take(self.get_smart_mut(front), |front| {
            SmartBelt::join(front, back_belt)
        });
    }

    // TODO: What does this return?
    fn merge_sushi_belts(&mut self, front: usize, back: usize) {
        if front == back {
            todo!("Make circular");

            return;
        }

        let back_belt = self.remove_sushi_belt(back);

        take_mut::take(self.get_sushi_mut(front), |front| {
            SushiBelt::join(front, back_belt)
        });
    }

    // TODO: What does this return?
    fn break_smart_belt_at(
        &mut self,
        id: BeltId<ItemIdxType>,
        pos: BeltLenType,
    ) -> Option<BeltId<ItemIdxType>> {
        assert!(pos > 0);

        // FIXME: Which side is kept? Front or back?
        let res = self.get_smart_mut(id).break_belt_at(pos);

        if let Some(residual_belt) = res {
            let new_id = self.add_belt(residual_belt);
            Some(new_id)
        } else {
            None
        }
    }

    // TODO: What does this return?
    fn break_sushi_belt_at(&mut self, id: usize, pos: BeltLenType) -> Option<usize> {
        assert!(pos > 0);

        // FIXME: Which side is kept? Front or back?
        let res = self.get_sushi_mut(id).break_belt_at(pos);

        if let Some(residual_belt) = res {
            let new_id = self.add_sushi_belt(residual_belt);
            Some(new_id)
        } else {
            None
        }
    }
}

#[derive(Debug, Clone, Copy, serde::Deserialize, serde::Serialize, PartialEq, Eq)]
pub enum SplitterTileId {
    Any(usize),
}

#[derive(Debug, Clone, serde::Deserialize, serde::Serialize)]
enum AnySplitter<ItemIdxType: WeakIdxTrait> {
    Pure(Item<ItemIdxType>, usize),
    Sushi(usize),
}

#[derive(Debug, Clone, serde::Deserialize, serde::Serialize)]
pub struct SplitterInfo<ItemIdxType: WeakIdxTrait> {
    pub in_mode: SplitterDistributionMode,
    pub out_mode: SplitterDistributionMode,

    /// 0 is left
    pub input_belts: [BeltTileId<ItemIdxType>; 2],
    /// 0 is left
    pub output_belts: [BeltTileId<ItemIdxType>; 2],
}

enum MakePureError {
    ErrorEmpty,
    ErrorSushi,
}

impl<ItemIdxType: IdxTrait, RecipeIdxType: IdxTrait> BeltStore<ItemIdxType, RecipeIdxType> {
    pub fn new(data_store: &DataStore<ItemIdxType, RecipeIdxType>) -> Self {
        Self {
            inner: InnerBeltStore {
                sushi_belts: vec![],
                sushi_belt_holes: vec![],
                smart_belts: vec![
                    MultiBeltStore {
                        belts: vec![],
                        holes: vec![]
                    };
                    data_store.item_names.len()
                ]
                .into_boxed_slice(),
                belt_belt_inserters: BeltBeltInserterStore {
                    pure_to_pure_inserters: vec![vec![]; data_store.item_names.len()]
                        .into_boxed_slice(),
                    pure_to_sushi_inserters: vec![],
                    sushi_to_sushi_inserters: vec![],
                    temp_sushi_to_smart_inserters: vec![],
                },

                pure_splitters: vec![
                    SplitterStore {
                        pure_splitters: vec![],
                        item: PhantomData,
                    };
                    data_store.item_names.len()
                ]
                .into_boxed_slice(),

                sushi_splitters: vec![],
                sushi_splitter_holes: vec![],
            },
            any_belts: vec![],
            any_belt_holes: vec![],

            any_splitters: vec![],
            any_splitter_holes: vec![],

            belt_graph: StableDiGraph::default(),
            belt_graph_lookup: HashMap::default(),
        }
    }

    fn try_make_belt_pure(
        &mut self,
        sushi_idx: usize,
        tile_id: BeltTileId<ItemIdxType>,
        bias: Option<Item<ItemIdxType>>,
    ) -> Result<(Item<ItemIdxType>, BeltTileId<ItemIdxType>), MakePureError> {
        let inserter_item_filter = self
            .inner
            .get_sushi(sushi_idx)
            .inserters
            .inserters
            .iter()
            .filter_map(|(ins, item)| (matches!(ins, Inserter::In(_)).then_some(*item)))
            .all_equal_value();

        fn check_incoming_edges<ItemIdxType: IdxTrait, RecipeIdxType: IdxTrait>(
            store: &mut BeltStore<ItemIdxType, RecipeIdxType>,
            tile_id: BeltTileId<ItemIdxType>,
            mut goal_item: Option<Item<ItemIdxType>>,
            incoming_edges: &[(NodeIndex, BeltGraphConnection<ItemIdxType>)],
            bias: Option<Item<ItemIdxType>>,
        ) -> Result<Item<ItemIdxType>, MakePureError> {
            // Make sure that all belts which feed into this belt ALSO are pure
            for (incoming_belt, connection_type) in incoming_edges {
                let source_tile_id = store.belt_graph.node_weight(*incoming_belt).unwrap();

                if *source_tile_id == tile_id {
                    // The source of this item_stream is the same belt we are currently considering,
                    // So it cannot change the result of our analysis
                    continue;
                }

                match source_tile_id {
                    BeltTileId::AnyBelt(index, _) => match store.any_belts[*index] {
                        AnyBelt::Smart(belt_id) => {
                            if belt_id.item == *goal_item.get_or_insert(belt_id.item) {
                                // This will only ever add a single item to this!
                            } else {
                                // This could potentially add an item to this belt, other than what we already have
                                return Err(MakePureError::ErrorSushi);
                            }
                        },
                        AnyBelt::Sushi(idx) => {
                            match connection_type {
                                BeltGraphConnection::Sideload { dest_belt_pos: _ }
                                | BeltGraphConnection::Connected => {
                                    // Every item on the belt could end up on out belt!

                                    match store.try_make_belt_pure(idx, *source_tile_id, bias) {
                                        Ok((incoming_belt_item, _new_id)) => {
                                            if incoming_belt_item
                                                == *goal_item.get_or_insert(incoming_belt_item)
                                            {
                                                // This will only ever add a single item to this!
                                            }
                                        },
                                        Err(MakePureError::ErrorEmpty) => {
                                            // This belt will never contain any items, so it is fine
                                        },
                                        Err(MakePureError::ErrorSushi) => {
                                            // We could not ensure the
                                            return Err(MakePureError::ErrorSushi);
                                        },
                                    }
                                },
                                BeltGraphConnection::BeltBeltInserter {
                                    source_belt_pos: _,
                                    dest_belt_pos: _,
                                    filter,
                                } => {
                                    // Only the filter could end up on our belt!
                                    if *filter == *goal_item.get_or_insert(*filter) {
                                        // This will only ever add a The same item we already have
                                    } else {
                                        // This could potentially add an item to this belt, other than what we already have
                                        return Err(MakePureError::ErrorSushi);
                                    }
                                },
                            }
                        },
                    },
                }
            }

            match goal_item {
                Some(item) => Ok(item),
                None => Err(MakePureError::ErrorEmpty),
            }
        }

        let incoming_edges: Vec<_> = self
            .belt_graph
            .edges_directed(
                *self.belt_graph_lookup.get(&tile_id).unwrap(),
                petgraph::Direction::Incoming,
            )
            .map(|edge| (edge.source(), *edge.weight()))
            .collect();

        match inserter_item_filter {
            Ok(inserter_item) => {
                match check_incoming_edges(
                    self,
                    tile_id,
                    Some(inserter_item),
                    &incoming_edges,
                    bias,
                ) {
                    Ok(item) => {
                        assert_eq!(item, inserter_item);
                    },
                    Err(MakePureError::ErrorEmpty) => unreachable!(),
                    Err(MakePureError::ErrorSushi) => return Err(MakePureError::ErrorSushi),
                }

                // We have checked all incoming item streams!

                // Check the items on the belt
                let items = self.inner.get_sushi(sushi_idx).items();

                let belt_item_check = items.iter().flatten().all_equal_value();

                match belt_item_check {
                    Ok(belt_items) => {
                        if *belt_items == inserter_item {
                            // All items are the correct type!
                        } else {
                            return Err(MakePureError::ErrorSushi);
                        }
                    },
                    Err(None) => {
                        // No on the belt, so also no items which are a problem
                    },
                    Err(Some(_)) => return Err(MakePureError::ErrorSushi),
                }

                let new_belt_id = self.inner.make_smart(sushi_idx, inserter_item);
                match tile_id {
                    BeltTileId::AnyBelt(index, _) => {
                        self.any_belts[index] = AnyBelt::Smart(new_belt_id);
                        Ok(((inserter_item), tile_id))
                    },
                }
            },
            Err(None) => {
                let mut incoming_belts_item =
                    match check_incoming_edges(self, tile_id, None, &incoming_edges, bias) {
                        Ok(item) => Some(item),
                        Err(MakePureError::ErrorEmpty) => None,
                        Err(MakePureError::ErrorSushi) => return Err(MakePureError::ErrorSushi),
                    };

                // We have checked all incoming item streams!

                // Check the items on the belt
                let items = self.inner.get_sushi(sushi_idx).items();

                let belt_item_check = items.iter().flatten().all_equal_value();

                let goal_item = match belt_item_check {
                    Ok(belt_items) => {
                        if *belt_items == *incoming_belts_item.get_or_insert(*belt_items) {
                            // All items are the correct type!
                            *belt_items
                        } else {
                            return Err(MakePureError::ErrorSushi);
                        }
                    },
                    Err(None) => {
                        // No on the belt, so also no items which are a problem

                        if let Some(item) = incoming_belts_item {
                            item
                        } else {
                            // No incoming inserters,
                            // No other belts leading onto this
                            // And no items on the belt

                            if let Some(bias_item) = bias {
                                bias_item
                            } else {
                                return Err(MakePureError::ErrorEmpty);
                            }
                        }
                    },
                    Err(Some(_)) => return Err(MakePureError::ErrorSushi),
                };

                let new_belt_id = self.inner.make_smart(sushi_idx, goal_item);
                match tile_id {
                    BeltTileId::AnyBelt(index, _) => {
                        self.any_belts[index] = AnyBelt::Smart(new_belt_id);
                        Ok(((goal_item), tile_id))
                    },
                }
            },
            Err(Some(_)) => return Err(MakePureError::ErrorSushi),
        }
    }

    fn merge_and_fix(
        &mut self,
        front_tile_id: BeltTileId<ItemIdxType>,
        back_tile_id: BeltTileId<ItemIdxType>,
    ) {
        let front_len = self.get_len(front_tile_id);

        let inner_edges: Vec<_> = self
            .belt_graph
            .edges_connecting(
                self.belt_graph_lookup[&front_tile_id],
                self.belt_graph_lookup[&back_tile_id],
            )
            .map(|edge| edge.id())
            .collect();

        for edge in inner_edges {
            match self.belt_graph.edge_weight(edge).unwrap() {
                BeltGraphConnection::Sideload { .. } => {},
                BeltGraphConnection::BeltBeltInserter { .. } => {},
                BeltGraphConnection::Connected => {},
            }
        }

        let edges: Vec<_> = self
            .belt_graph
            .edges(self.belt_graph_lookup[&back_tile_id])
            .map(|edge| (edge.source(), edge.target(), edge.id()))
            .collect();

        for (source, target, id) in edges {
            let conn = self.belt_graph.remove_edge(id).unwrap();

            let new_conn = match conn {
                BeltGraphConnection::Sideload { dest_belt_pos } => {
                    if source == self.belt_graph_lookup[&back_tile_id] {
                        BeltGraphConnection::Sideload { dest_belt_pos }
                    } else if target == self.belt_graph_lookup[&back_tile_id] {
                        BeltGraphConnection::Sideload {
                            dest_belt_pos: dest_belt_pos + front_len,
                        }
                    } else {
                        unreachable!()
                    }
                },
                BeltGraphConnection::BeltBeltInserter {
                    source_belt_pos,
                    dest_belt_pos,
                    filter,
                } => BeltGraphConnection::BeltBeltInserter {
                    source_belt_pos: {
                        if source == self.belt_graph_lookup[&back_tile_id] {
                            source_belt_pos + front_len
                        } else if target == self.belt_graph_lookup[&back_tile_id] {
                            source_belt_pos
                        } else {
                            unreachable!()
                        }
                    },
                    dest_belt_pos: {
                        if source == self.belt_graph_lookup[&back_tile_id] {
                            dest_belt_pos
                        } else if target == self.belt_graph_lookup[&back_tile_id] {
                            dest_belt_pos + front_len
                        } else {
                            unreachable!()
                        }
                    },
                    filter,
                },
                BeltGraphConnection::Connected => BeltGraphConnection::Connected,
            };

            if source == self.belt_graph_lookup[&back_tile_id] {
                self.belt_graph
                    .add_edge(self.belt_graph_lookup[&front_tile_id], target, new_conn);
            } else if target == self.belt_graph_lookup[&back_tile_id] {
                self.belt_graph
                    .add_edge(target, self.belt_graph_lookup[&front_tile_id], new_conn);
            } else {
                unreachable!()
            }
        }

        assert!(
            self.belt_graph
                .edges(self.belt_graph_lookup[&back_tile_id])
                .count()
                == 0
        );

        self.belt_graph
            .remove_node(self.belt_graph_lookup[&back_tile_id])
            .expect("Node not found");

        self.belt_graph_lookup
            .remove(&back_tile_id)
            .expect("Lookup not found");

        let mut done_propagating = HashSet::default();
        let mut dedup = HashSet::default();
        let mut done = HashMap::default();

        self.propagate_sushi_if_necessary(
            front_tile_id,
            &mut done_propagating,
            &mut dedup,
            &mut done,
        );
    }

    fn add_graph_connection_and_fix(
        &mut self,
        source_tile_id: BeltTileId<ItemIdxType>,
        dest_tile_id: BeltTileId<ItemIdxType>,
        belt_connection: BeltGraphConnection<ItemIdxType>,
    ) {
        self.belt_graph.add_edge(
            self.belt_graph_lookup[&source_tile_id],
            self.belt_graph_lookup[&dest_tile_id],
            belt_connection,
        );

        let mut done_propagating = HashSet::default();
        let mut dedup = HashSet::default();
        let mut done = HashMap::default();

        self.propagate_sushi_if_necessary(
            dest_tile_id,
            &mut done_propagating,
            &mut dedup,
            &mut done,
        );
    }

    fn propagate_sushi_if_necessary(
        &mut self,
        tile_id: BeltTileId<ItemIdxType>,
        done_propagating: &mut HashSet<BeltTileId<ItemIdxType>>,
        dedup: &mut HashSet<BeltTileId<ItemIdxType>>,
        done: &mut HashMap<BeltTileId<ItemIdxType>, Vec<Item<ItemIdxType>>>,
    ) {
        if done_propagating.contains(&tile_id) {
            return;
        }

        done_propagating.insert(tile_id);

        match tile_id {
            BeltTileId::AnyBelt(index, _) => match self.any_belts[index] {
                AnyBelt::Smart(id) => {
                    self.get_items_which_could_end_up_on_that_belt(tile_id, dedup, done);

                    let items = done.get(&tile_id).unwrap();

                    match items.iter().all_equal_value() {
                        Ok(item) => {
                            if *item == id.item {
                                // No need to make this into sushi
                            } else {
                                // Lets make this sushi (instead of a different smart for safety)
                                let sushi_idx = self.inner.make_sushi(id);
                                self.any_belts[index] = AnyBelt::Sushi(sushi_idx);
                            }
                        },
                        Err(None) => {
                            // The belt is empty, lets make is sushi for safety
                            let sushi_idx = self.inner.make_sushi(id);
                            self.any_belts[index] = AnyBelt::Sushi(sushi_idx);
                        },
                        Err(Some(_)) => {
                            // This needs to be sushi
                            let sushi_idx = self.inner.make_sushi(id);
                            self.any_belts[index] = AnyBelt::Sushi(sushi_idx);
                        },
                    }
                },
                AnyBelt::Sushi(idx) => {
                    // We are already sushi
                    // Just continue

                    match self.try_make_belt_pure(idx, tile_id, None) {
                        Ok(_) => {},
                        Err(_) => {},
                    }
                },
            },
        }
        let outgoing_edges: Vec<_> = self
            .belt_graph
            .edges_directed(self.belt_graph_lookup[&tile_id], Outgoing)
            .map(|edge| *self.belt_graph.node_weight(edge.target()).unwrap())
            .collect();

        for outgoing_edge in outgoing_edges {
            self.propagate_sushi_if_necessary(outgoing_edge, done_propagating, dedup, done);
        }
    }

    // TODO: This needs tests!
    // TODO: This HashMap could be kept until the graph is modified
    fn get_items_which_could_end_up_on_that_belt(
        &self,
        tile_id: BeltTileId<ItemIdxType>,
        dedup: &mut HashSet<BeltTileId<ItemIdxType>>,
        done: &mut HashMap<BeltTileId<ItemIdxType>, Vec<Item<ItemIdxType>>>,
    ) {
        if dedup.contains(&tile_id) {
            return;
        }

        let (inserter_item_sources, items_on_belt): (Vec<_>, Vec<_>) = match tile_id {
            BeltTileId::AnyBelt(index, _) => match self.any_belts[index] {
                AnyBelt::Smart(belt_id) => {
                    let belt = self.inner.get_smart(belt_id);
                    let items_all_empty = belt.items().iter().all(|loc| loc.is_none());
                    match (belt.inserters.inserters.is_empty(), items_all_empty) {
                        (true, true) => (vec![], vec![]),
                        (true, false) => (vec![], vec![belt_id.item]),
                        (false, true) => (vec![belt_id.item], vec![]),
                        (false, false) => (vec![belt_id.item], vec![belt_id.item]),
                    }
                },
                AnyBelt::Sushi(sushi_idx) => {
                    let belt = self.inner.get_sushi(sushi_idx);

                    (
                        belt.inserters
                            .inserters
                            .iter()
                            .filter_map(|(ins, item)| {
                                (matches!(ins, Inserter::In(_)).then_some(*item))
                            })
                            .dedup()
                            .collect(),
                        belt.items().into_iter().flatten().dedup().collect(),
                    )
                },
            },
        };

        let incoming_belts: Vec<_> = self
            .belt_graph
            .edges_directed(
                self.belt_graph_lookup[&tile_id],
                petgraph::Direction::Incoming,
            )
            .map(|edge| {
                (
                    self.belt_graph.node_weight(edge.source()).unwrap(),
                    *edge.weight(),
                )
            })
            .collect();

        let incoming_belt_items: Vec<_> = incoming_belts
            .into_iter()
            .flat_map(|(belt, connection)| match connection {
                BeltGraphConnection::Sideload { dest_belt_pos: _ }
                | BeltGraphConnection::Connected => {
                    self.get_items_which_could_end_up_on_that_belt(*belt, dedup, done);
                    done.get(belt)
                        .unwrap_or(&vec![])
                        .iter()
                        .copied()
                        .collect::<Vec<_>>()
                },
                BeltGraphConnection::BeltBeltInserter {
                    source_belt_pos: _,
                    dest_belt_pos: _,
                    filter,
                } => once(filter).collect(),
            })
            .collect();

        done.insert(
            tile_id,
            items_on_belt
                .into_iter()
                .chain(inserter_item_sources)
                .chain(incoming_belt_items)
                .collect(),
        );
    }

    pub fn update<'a, 'b>(
        &mut self,
        num_grids_total: usize,
        storages_by_item: impl IndexedParallelIterator<Item = SingleItemStorages<'a, 'b>>,
        data_store: &DataStore<ItemIdxType, RecipeIdxType>,
    ) where
        'b: 'a,
    {
        // TODO: Once every (maybe more or less) check a single belt and check if it still needs to be sushi

        // Update all the "Pure Belts"
        self.inner
            .smart_belts
            .par_iter_mut()
            .zip(storages_by_item)
            .zip(
                self.inner
                    .belt_belt_inserters
                    .pure_to_pure_inserters
                    .par_iter_mut(),
            )
            .zip(self.inner.pure_splitters.par_iter_mut())
            .enumerate()
            .for_each(
                |(item_id, (((belt_store, item_storages), pure_to_pure_inserters), splitters))| {
                    let item = Item {
                        id: item_id.try_into().unwrap(),
                    };

                    let grid_size = grid_size(item, data_store);
                    let num_recipes = num_recipes(item, data_store);

                    for belt in &mut belt_store.belts {
                        belt.update();
                        belt.update_inserters(
                            item_storages,
                            num_grids_total,
                            num_recipes,
                            grid_size,
                        );
                    }

                    for (ins, ((source, source_pos), (dest, dest_pos), cooldown, filter)) in
                        pure_to_pure_inserters.iter_mut().flatten()
                    {
                        let [source_loc, dest_loc] = if *source == *dest {
                            assert_ne!(
                                source_pos, dest_pos,
                                "An inserter cannot take and drop off on the same tile"
                            );
                            // We are taking and placing onto the same belt
                            let belt = &mut belt_store.belts[*source];

                            belt.get_two([(*source_pos).into(), (*dest_pos).into()])
                        } else {
                            let [inp, out] =
                                belt_store.belts.get_disjoint_mut([*source, *dest]).unwrap();

                            [inp.get_mut(*source_pos), out.get_mut(*dest_pos)]
                        };

                        if *cooldown == 0 {
                            ins.update_instant(source_loc, dest_loc);
                        } else {
                            ins.update(source_loc, dest_loc, *cooldown, (), |_| {
                                filter
                                    .map(|filter_item| filter_item == item)
                                    .unwrap_or(true)
                            });
                        }

                        let source_loc = *source_loc;
                        let dest_loc = *dest_loc;

                        if !source_loc {
                            belt_store.belts[*source].update_first_free_pos(*source_pos);
                        }

                        if dest_loc {
                            belt_store.belts[*dest].remove_first_free_pos_maybe(*dest_pos);
                        }
                    }

                    for splitter in &mut splitters.pure_splitters {
                        // TODO: Assert that the item is the same!
                        //splitter.update(belt_store);
                    }
                },
            );

        self.inner.sushi_belts.iter_mut().for_each(|sushi_belt| {
            sushi_belt.update();

            // TODO: Update inserters!
        });
    }

    pub fn get_splitter_belt_ids(
        &self,
        splitter_id: SplitterTileId,
    ) -> [[BeltTileId<ItemIdxType>; 2]; 2] {
        let belts: [[AnyBelt<ItemIdxType>; 2]; 2] = match splitter_id {
            SplitterTileId::Any(index) => match self.any_splitters[index] {
                AnySplitter::Pure(item, id) => self
                    .inner
                    .get_pure_splitter_belt_ids(item, id)
                    .into_iter()
                    .map(|v| {
                        v.into_iter()
                            .map(|belt_id| AnyBelt::Smart(belt_id))
                            .collect_array()
                            .unwrap()
                    })
                    .collect_array()
                    .unwrap(),
                AnySplitter::Sushi(id) => self
                    .inner
                    .get_sushi_splitter_belt_ids(id)
                    .into_iter()
                    .map(|v| {
                        v.into_iter()
                            .map(|index| AnyBelt::Sushi(index))
                            .collect_array()
                            .unwrap()
                    })
                    .collect_array()
                    .unwrap(),
            },
        };

        let [inputs, outputs] = belts;

        // FIXME: This is O(n) over the number of splitters :/
        [
            inputs
                .into_iter()
                .map(|belt| {
                    self.any_belts
                        .iter()
                        .position(|any_belt| *any_belt == belt)
                        .unwrap()
                })
                .map(|any_belt_index| BeltTileId::AnyBelt(any_belt_index, PhantomData))
                .collect_array()
                .unwrap(),
            outputs
                .into_iter()
                .map(|belt| {
                    self.any_belts
                        .iter()
                        .position(|any_belt| *any_belt == belt)
                        .unwrap()
                })
                .map(|any_belt_index| BeltTileId::AnyBelt(any_belt_index, PhantomData))
                .collect_array()
                .unwrap(),
        ]
    }

    pub fn add_length(&mut self, belt: BeltTileId<ItemIdxType>, amount: u16, side: Side) -> u16 {
        match belt {
            BeltTileId::AnyBelt(index, _) => match self.any_belts[index] {
                AnyBelt::Smart(belt_id) => self.inner.add_length_smart(belt_id, amount, side),
                AnyBelt::Sushi(id) => self.inner.add_length_sushi(id, amount, side),
            },
        }
    }

    fn add_belt(&mut self, belt: SmartBelt<ItemIdxType, RecipeIdxType>) -> BeltTileId<ItemIdxType> {
        let id = self.inner.add_belt(belt);

        let new_id = self.add_smart_to_any_list(id);

        let index = self.belt_graph.add_node(new_id);
        self.belt_graph_lookup.insert(new_id, index);

        new_id
    }

    pub fn add_empty_belt(&mut self, len: u16) -> BeltTileId<ItemIdxType> {
        let sushi_idx = self.inner.add_sushi_belt(SushiBelt::new(len));

        let new_id = self.add_sushi_to_any_list(sushi_idx);

        let index = self.belt_graph.add_node(new_id);
        self.belt_graph_lookup.insert(new_id, index);

        new_id
    }

    fn add_sushi_belt(
        &mut self,
        belt: SushiBelt<ItemIdxType, RecipeIdxType>,
    ) -> BeltTileId<ItemIdxType> {
        let sushi_idx = self.inner.add_sushi_belt(belt);

        let new_id = self.add_sushi_to_any_list(sushi_idx);

        let index = self.belt_graph.add_node(new_id);
        self.belt_graph_lookup.insert(new_id, index);

        new_id
    }

    fn add_smart_to_any_list(&mut self, belt_id: BeltId<ItemIdxType>) -> BeltTileId<ItemIdxType> {
        let index = if let Some(hole) = self.any_belt_holes.pop() {
            self.any_belts[hole] = AnyBelt::Smart(belt_id);
            hole
        } else {
            self.any_belts.push(AnyBelt::Smart(belt_id));
            self.any_belts.len() - 1
        };

        BeltTileId::AnyBelt(index, PhantomData)
    }

    fn add_sushi_to_any_list(&mut self, sushi_idx: usize) -> BeltTileId<ItemIdxType> {
        let index = if let Some(hole) = self.any_belt_holes.pop() {
            self.any_belts[hole] = AnyBelt::Sushi(sushi_idx);
            hole
        } else {
            self.any_belts.push(AnyBelt::Sushi(sushi_idx));
            self.any_belts.len() - 1
        };

        BeltTileId::AnyBelt(index, PhantomData)
    }

    pub fn break_belt_at(
        &mut self,
        id: BeltTileId<ItemIdxType>,
        belt_pos_to_break_at: u16,
    ) -> BreakBeltResultInfo<ItemIdxType> {
        let ret = match id {
            BeltTileId::AnyBelt(index, _) => match &mut self.any_belts[index] {
                AnyBelt::Smart(smart_belt_id) => {
                    let new_belt_id = self
                        .inner
                        .break_smart_belt_at(*smart_belt_id, belt_pos_to_break_at);

                    if let Some(new_belt_id) = new_belt_id {
                        let new_id = self.add_smart_to_any_list(new_belt_id);
                        BreakBeltResultInfo {
                            kept_id: id,
                            new_belt: Some((new_id, Side::BACK)),
                        }
                    } else {
                        BreakBeltResultInfo {
                            kept_id: id,
                            new_belt: None,
                        }
                    }
                },
                AnyBelt::Sushi(sushi_belt_index) => {
                    let new_belt_id = self
                        .inner
                        .break_sushi_belt_at(*sushi_belt_index, belt_pos_to_break_at);

                    if let Some(new_belt_id) = new_belt_id {
                        let new_id = self.add_sushi_to_any_list(new_belt_id);
                        BreakBeltResultInfo {
                            kept_id: id,
                            new_belt: Some((new_id, Side::BACK)),
                        }
                    } else {
                        BreakBeltResultInfo {
                            kept_id: id,
                            new_belt: None,
                        }
                    }
                },
            },
        };

        if let Some((new_id, new_side)) = ret.new_belt {
            let index = self.belt_graph.add_node(new_id);
            self.belt_graph_lookup.insert(new_id, index);

            // TODO: Update the graph to reflect the moved connections!
            // let outgoing_edges = self
            //     .belt_graph
            //     .edges_directed(id, petgraph::Direction::Outgoing)
            //     .map(|(source, dest, connection)| {});
        }

        ret
    }

    pub fn add_storage_belt_inserter(
        &mut self,
        filter: Item<ItemIdxType>,
        id: BeltTileId<ItemIdxType>,
        pos: BeltLenType,
        storage_id: Storage<RecipeIdxType>,
    ) -> Result<(), SpaceOccupiedError> {
        let handle_sushi_belt = |belt: &mut SushiBelt<ItemIdxType, RecipeIdxType>| {
            belt.add_in_inserter(filter, pos, storage_id)
        };

        match id {
            BeltTileId::AnyBelt(index, _) => {
                match &mut self.any_belts[index] {
                    AnyBelt::Smart(smart_belt_id) => {
                        let smart_belt = self.inner.get_smart_mut(*smart_belt_id);

                        match smart_belt.add_in_inserter(filter, pos, storage_id) {
                            Ok(()) => {
                                // It succeeded!
                            },
                            Err(InserterAdditionError::SpaceOccupied) => {
                                return Err(SpaceOccupiedError)
                            },
                            Err(InserterAdditionError::ItemMismatch) => {
                                // We need to transition to sushi belt
                                let new_index = self.inner.make_sushi(*smart_belt_id);

                                self.any_belts[index] = AnyBelt::Sushi(new_index);

                                // Retry adding inserter
                                let now_sushi_belt = self.inner.get_sushi_mut(new_index);

                                now_sushi_belt
                                    .add_in_inserter(filter, pos, storage_id)
                                    .expect("We already became sushi, it should now work!");

                                let AnyBelt::Sushi(sushi_belt_id) = &mut self.any_belts[index]
                                else {
                                    unreachable!();
                                };
                                let sushi_belt = self.inner.get_sushi_mut(*sushi_belt_id);
                                debug_assert!(handle_sushi_belt(sushi_belt).is_ok());
                            },
                        }
                    },
                    AnyBelt::Sushi(sushi_belt_index) => {
                        let sushi_belt = self.inner.get_sushi_mut(*sushi_belt_index);
                        match handle_sushi_belt(sushi_belt) {
                            Ok(()) => {
                                let sushi_belt_index = *sushi_belt_index;
                                let _ = self.try_make_belt_pure(sushi_belt_index, id, None);
                            },
                            Err(SpaceOccupiedError) => return Err(SpaceOccupiedError),
                        }
                    },
                }
            },
        }

        return Ok(());
    }

    pub fn add_belt_storage_inserter(
        &mut self,
        filter: Item<ItemIdxType>,
        id: BeltTileId<ItemIdxType>,
        pos: BeltLenType,
        storage_id: Storage<RecipeIdxType>,
    ) -> Result<(), SpaceOccupiedError> {
        let handle_sushi_belt = |belt: &mut SushiBelt<ItemIdxType, RecipeIdxType>| {
            belt.add_out_inserter(filter, pos, storage_id)
        };

        match id {
            BeltTileId::AnyBelt(index, _) => {
                match &mut self.any_belts[index] {
                    AnyBelt::Smart(smart_belt_id) => {
                        let smart_belt = self.inner.get_smart_mut(*smart_belt_id);

                        match smart_belt.add_out_inserter(filter, pos, storage_id) {
                            Ok(()) => {
                                // It succeeded!
                            },
                            Err(InserterAdditionError::SpaceOccupied) => {
                                return Err(SpaceOccupiedError)
                            },
                            Err(InserterAdditionError::ItemMismatch) => {
                                // We need to transition to sushi belt
                                let new_index = self.inner.make_sushi(*smart_belt_id);

                                self.any_belts[index] = AnyBelt::Sushi(new_index);

                                // Retry adding inserter
                                let now_sushi_belt = self.inner.get_sushi_mut(new_index);

                                now_sushi_belt
                                    .add_out_inserter(filter, pos, storage_id)
                                    .expect("We already became sushi, it should now work!");
                            },
                        }
                    },
                    AnyBelt::Sushi(sushi_belt_id) => {
                        let sushi_belt = &mut self.inner.sushi_belts[*sushi_belt_id];
                        // Adding an inserter cannot change a sushi belt to a smart belt
                        match handle_sushi_belt(sushi_belt) {
                            Ok(()) => {
                                let sushi_belt_index = *sushi_belt_id;
                                let _ = self.try_make_belt_pure(sushi_belt_index, id, None);
                            },
                            Err(SpaceOccupiedError) => return Err(SpaceOccupiedError),
                        }
                    },
                }
            },
        }

        return Ok(());
    }

    pub fn add_sideloading_inserter(
        &mut self,
        source: BeltTileId<ItemIdxType>,
        dest: (BeltTileId<ItemIdxType>, BeltLenType),
    ) -> usize {
        self.add_graph_connection_and_fix(
            source,
            dest.0,
            BeltGraphConnection::Sideload {
                dest_belt_pos: dest.1,
            },
        );

        // FIXME: Holes

        // FIXME: We need any belt index stuff here as well!
        let ret = match (source, dest.0) {
            (BeltTileId::AnyBelt(source_index, _), BeltTileId::AnyBelt(dest_index, _)) => {
                match (&self.any_belts[source_index], &self.any_belts[dest_index]) {
                    (AnyBelt::Smart(source_belt_id), AnyBelt::Smart(dest_belt_id)) => {
                        assert_eq!(source_belt_id.item, dest_belt_id.item);

                        self.inner.belt_belt_inserters.pure_to_pure_inserters
                            [usize_from(source_belt_id.item.id)]
                        .push(Some((
                            BeltBeltInserter::new(),
                            (
                                (source_belt_id.index, 0),
                                (dest_belt_id.index, dest.1),
                                0,
                                None,
                            ),
                        )));

                        self.inner.belt_belt_inserters.pure_to_pure_inserters
                            [usize_from(source_belt_id.item.id)]
                        .len()
                    },
                    (AnyBelt::Smart(source_belt_id), AnyBelt::Sushi(dest_index)) => {
                        self.inner
                            .belt_belt_inserters
                            .pure_to_sushi_inserters
                            .push(Some((
                                BeltBeltInserter::new(),
                                ((*source_belt_id, 0), (*dest_index, dest.1), 0, None),
                            )));

                        self.inner.belt_belt_inserters.pure_to_sushi_inserters.len()
                    },
                    (AnyBelt::Sushi(_), AnyBelt::Smart(_)) => unreachable!(
                        "If a sushi belt sideloads onto a smart belt, it can never be pure"
                    ),
                    (AnyBelt::Sushi(source_index), AnyBelt::Sushi(dest_index)) => {
                        self.inner
                            .belt_belt_inserters
                            .sushi_to_sushi_inserters
                            .push(Some((
                                BeltBeltInserter::new(),
                                ((*source_index, 0), (*dest_index, dest.1), 0, None),
                            )));

                        self.inner
                            .belt_belt_inserters
                            .sushi_to_sushi_inserters
                            .len()
                    },
                }
            },
        };

        ret
    }

    pub fn remove_sideloading_inserter(&mut self, index: usize) {
        todo!()
    }

    fn get_belt_belt_inserter_sushi_lists<'a>(
        &'a self,
        id: BeltTileId<ItemIdxType>,
    ) -> (
        impl IntoIterator<Item = SushiInfo<ItemIdxType>> + Clone + use<'a, ItemIdxType, RecipeIdxType>,
        impl IntoIterator<Item = SushiInfo<ItemIdxType>> + Clone + use<'a, ItemIdxType, RecipeIdxType>,
    ) {
        // FIXME: Consider splitters!!!!
        (
            self.inner
                .belt_belt_inserters
                .pure_to_pure_inserters
                .iter()
                .enumerate()
                .map(|(i, v)| {
                    (
                        v,
                        Item::<ItemIdxType> {
                            id: i.try_into().unwrap(),
                        },
                    )
                })
                .map(move |(v, item)| {
                    v.iter().flatten().filter_map(move |ins| {
                        // (ins.1 .1 .0 == id).then_some(SushiInfo::Pure(Some(item)))
                        // FIXME:
                        None
                    })
                })
                .flatten()
                .chain({
                    let idx = self.belt_graph_lookup[&id];

                    let edges = self
                        .belt_graph
                        .edges_directed(idx, petgraph::Direction::Incoming);

                    edges.map(|edge| match edge.weight() {
                        BeltGraphConnection::BeltBeltInserter {
                            source_belt_pos,
                            dest_belt_pos,
                            filter,
                        } => SushiInfo::Pure(Some(*filter)),
                        BeltGraphConnection::Sideload { dest_belt_pos: _ }
                        | BeltGraphConnection::Connected => {
                            match self.belt_graph.node_weight(edge.source()).unwrap() {
                                BeltTileId::AnyBelt(index, _) => match self.any_belts[*index] {
                                    AnyBelt::Smart(belt_id) => SushiInfo::Pure(Some(belt_id.item)),
                                    AnyBelt::Sushi(_) => SushiInfo::Sushi,
                                },
                            }
                        },
                    })
                }),
            self.inner
                .belt_belt_inserters
                .pure_to_pure_inserters
                .iter()
                .enumerate()
                .map(|(i, v)| {
                    (
                        v,
                        Item::<ItemIdxType> {
                            id: i.try_into().unwrap(),
                        },
                    )
                })
                .map(move |(v, item)| {
                    v.iter().filter_map(move |ins| {
                        // (ins.1.source.0 == id).then_some(SushiInfo::Pure(Some(item)))
                        // FIXME:
                        None
                    })
                })
                .flatten(), // .chain(
                            //     self.inner.belt_belt_inserters
                            //         .sideload_inserters
                            //         .iter()
                            //         .filter_map(move |ins| {
                            //             if ins.1.source.0 == id {
                            //                 match ins.1.dest.0 {
                            //                     BeltTileId::AnyBelt(index, _) => match &self.any_belts[index] {
                            //                         AnyBelt::Smart(_) => {
                            //                             unreachable!("Here a sushi belt is sideloading onto a smart belt. this MUST be impossible")
                            //                         },
                            //                         AnyBelt::Sushi(_) => Some(SushiInfo::Sushi),
                            //                     },
                            //                 }
                            //             } else {
                            //                 None
                            //             }
                            //         }),
                            // ),
        )
    }

    pub fn add_belt_belt_inserter(
        &mut self,
        from: (BeltTileId<ItemIdxType>, u16),
        to: (BeltTileId<ItemIdxType>, u16),
        info: BeltBeltInserterAdditionInfo<ItemIdxType>,
    ) -> usize {
        self.add_graph_connection_and_fix(
            from.0,
            to.0,
            BeltGraphConnection::BeltBeltInserter {
                source_belt_pos: from.1,
                dest_belt_pos: to.1,
                filter: info.filter,
            },
        );

        // FIXME: The index returned is is complete bugus!
        match (from.0, to.0) {
            (BeltTileId::AnyBelt(source_idx, _), BeltTileId::AnyBelt(dest_idx, _)) => {
                match (&self.any_belts[source_idx], &self.any_belts[dest_idx]) {
                    (AnyBelt::Smart(source_belt_id), AnyBelt::Smart(dest_belt_id)) => {
                        assert_eq!(source_belt_id.item, dest_belt_id.item);
                        assert_eq!(source_belt_id.item, info.filter);
                        self.inner.belt_belt_inserters.pure_to_pure_inserters
                            [usize_from(source_belt_id.item.id)]
                        .push(Some((
                            BeltBeltInserter::new(),
                            (
                                (source_belt_id.index, from.1),
                                (dest_belt_id.index, to.1),
                                info.cooldown,
                                Some(info.filter),
                            ),
                        )));

                        self.inner.belt_belt_inserters.pure_to_pure_inserters
                            [usize_from(source_belt_id.item.id)]
                        .len()
                            - 1
                    },
                    (AnyBelt::Smart(source_belt_id), AnyBelt::Sushi(dest_index)) => todo!(),
                    (AnyBelt::Sushi(source_index), AnyBelt::Smart(dest_belt_id)) => todo!(),
                    (AnyBelt::Sushi(source_index), AnyBelt::Sushi(dest_index)) => {
                        self.inner
                            .belt_belt_inserters
                            .sushi_to_sushi_inserters
                            .push(Some((
                                BeltBeltInserter::new(),
                                (
                                    (*source_index, from.1),
                                    (*dest_index, to.1),
                                    info.cooldown,
                                    Some(info.filter),
                                ),
                            )));

                        self.inner
                            .belt_belt_inserters
                            .sushi_to_sushi_inserters
                            .len()
                            - 1
                    },
                }
            },
        }
    }

    pub fn remove_belt_belt_inserter(&mut self, inserter_index: usize) {
        todo!()
    }

    pub fn remove_inserter(&mut self, id: BeltTileId<ItemIdxType>, pos: BeltLenType) {
        match id {
            BeltTileId::AnyBelt(index, _) => match &mut self.any_belts[index] {
                AnyBelt::Smart(smart_belt_id) => {
                    let smart_belt = self.inner.get_smart_mut(*smart_belt_id);
                    smart_belt.remove_inserter(pos);
                },
                AnyBelt::Sushi(sushi_belt_id) => {
                    let sushi_belt = &mut self.inner.sushi_belts[*sushi_belt_id];
                    sushi_belt.remove_inserter(pos);
                    let sushi_belt_id = *sushi_belt_id;
                    match self.try_make_belt_pure(sushi_belt_id, id, None) {
                        Ok((item, new_id)) => {
                            assert_eq!(id, new_id);
                        },
                        Err(_) => {
                            info!("Unable to convert belt {id:?} to pure belt");
                        },
                    }
                },
            },
        };
    }

    pub fn get_pure_item(&self, id: BeltTileId<ItemIdxType>) -> Option<Item<ItemIdxType>> {
        match id {
            BeltTileId::AnyBelt(index, _) => match &self.any_belts[index] {
                AnyBelt::Smart(smart_belt) => Some(smart_belt.item),
                AnyBelt::Sushi(sushi_belt) => None,
            },
        }
    }

    pub fn get_item_iter(
        &self,
        id: BeltTileId<ItemIdxType>,
    ) -> impl IntoIterator<Item = Option<Item<ItemIdxType>>> {
        match id {
            BeltTileId::AnyBelt(index, _) => match &self.any_belts[index] {
                AnyBelt::Smart(smart_belt) => self.inner.get_smart(*smart_belt).items(),
                AnyBelt::Sushi(sushi_belt) => self.inner.get_sushi(*sushi_belt).items(),
            },
        }
    }

    pub fn remove_any_belt(&mut self, index: usize) {
        match self.any_belts[index] {
            AnyBelt::Smart(belt_id) => {
                self.inner.remove_smart_belt(belt_id);
            },
            AnyBelt::Sushi(index) => {
                self.inner.remove_sushi_belt(index);
            },
        }

        self.any_belt_holes.push(index);
        // TODO: Return the items so they are not lost?
    }

    pub fn merge_belts(
        &mut self,
        front_tile_id: BeltTileId<ItemIdxType>,
        back_tile_id: BeltTileId<ItemIdxType>,
        data_store: &DataStore<ItemIdxType, RecipeIdxType>,
    ) -> (BeltTileId<ItemIdxType>, BeltLenType) {
        if front_tile_id == back_tile_id {
            todo!("Make circular")
        }

        match (front_tile_id, back_tile_id) {
            (BeltTileId::AnyBelt(front, _), BeltTileId::AnyBelt(back, _)) => {
                assert_ne!(front, back);
                match self.any_belts.get_disjoint_mut([front, back]).unwrap() {
                    [AnyBelt::Smart(front_smart_belt), AnyBelt::Smart(back_smart_belt)] => {
                        if front_smart_belt.item == back_smart_belt.item {
                            self.inner
                                .merge_smart_belts(*front_smart_belt, *back_smart_belt);

                            self.merge_and_fix(front_tile_id, back_tile_id);

                            self.any_belts[back] = AnyBelt::Sushi(usize::MAX);
                            self.any_belt_holes.push(back);

                            (front_tile_id, self.get_len(front_tile_id))
                        } else {
                            let front_smart_belt = *front_smart_belt;
                            let back_smart_belt = *back_smart_belt;

                            let front_sushi_idx = self.inner.make_sushi(front_smart_belt);
                            self.any_belts[front] = AnyBelt::Sushi(front_sushi_idx);

                            let back_sushi_idx = self.inner.make_sushi(back_smart_belt);
                            self.any_belts[back] = AnyBelt::Sushi(back_sushi_idx);

                            // We now have two Sushi belts, retry:
                            self.merge_belts(front_tile_id, back_tile_id, data_store)
                        }
                    },
                    [AnyBelt::Smart(front_smart_belt), AnyBelt::Sushi(back_sushi_belt)] => {
                        let front_smart_belt = *front_smart_belt;
                        let back_sushi_belt = *back_sushi_belt;

                        let bias = Some(front_smart_belt.item);
                        match self.try_make_belt_pure(back_sushi_belt, back_tile_id, bias) {
                            Ok(_) => {
                                // We now have two smart belts, retry:
                                self.merge_belts(front_tile_id, back_tile_id, data_store)
                            },
                            Err(MakePureError::ErrorEmpty) => unreachable!(),
                            Err(MakePureError::ErrorSushi) => {
                                let front_sushi_idx = self.inner.make_sushi(front_smart_belt);
                                self.any_belts[front] = AnyBelt::Sushi(front_sushi_idx);

                                // We now have two Sushi belts, retry:
                                self.merge_belts(front_tile_id, back_tile_id, data_store)
                            },
                        }
                    },
                    [AnyBelt::Sushi(front_sushi_belt), AnyBelt::Smart(back_smart_belt)] => {
                        let front_sushi_belt = *front_sushi_belt;
                        let back_smart_belt = *back_smart_belt;

                        let bias = Some(back_smart_belt.item);
                        match self.try_make_belt_pure(front_sushi_belt, front_tile_id, bias) {
                            Ok(_) => {
                                // We now have two smart belts, retry:
                                self.merge_belts(front_tile_id, back_tile_id, data_store)
                            },
                            Err(MakePureError::ErrorEmpty) => unreachable!(),
                            Err(MakePureError::ErrorSushi) => {
                                let back_sushi_idx = self.inner.make_sushi(back_smart_belt);
                                self.any_belts[back] = AnyBelt::Sushi(back_sushi_idx);

                                // We now have two Sushi belts, retry:
                                self.merge_belts(front_tile_id, back_tile_id, data_store)
                            },
                        }
                    },
                    [AnyBelt::Sushi(front_sushi_belt), AnyBelt::Sushi(back_sushi_belt)] => {
                        let front_sushi_belt = *front_sushi_belt;

                        self.inner
                            .merge_sushi_belts(front_sushi_belt, *back_sushi_belt);

                        self.merge_and_fix(front_tile_id, back_tile_id);

                        self.any_belts[back] = AnyBelt::Sushi(usize::MAX);
                        self.any_belt_holes.push(back);

                        let _ = self.try_make_belt_pure(front_sushi_belt, front_tile_id, None);

                        (front_tile_id, self.get_len(front_tile_id))
                    },
                }
            },
        }
    }

    pub fn add_splitter(&mut self, info: SplitterInfo<ItemIdxType>) -> SplitterTileId {
        todo!()
    }

    pub fn remove_splitter(&mut self, index: usize) {
        todo!()
    }

    pub fn get_len(&self, id: BeltTileId<ItemIdxType>) -> u16 {
        match id {
            BeltTileId::AnyBelt(index, _) => match self.any_belts[index] {
                AnyBelt::Smart(belt_id) => self.inner.get_smart(belt_id).get_len(),
                AnyBelt::Sushi(index) => self.inner.get_sushi(index).get_len(),
            },
        }
    }
}

#[derive(Debug, Clone, serde::Deserialize, serde::Serialize)]
pub struct MultiBeltStore<ItemIdxType: WeakIdxTrait, RecipeIdxType: WeakIdxTrait> {
    pub belts: Vec<SmartBelt<ItemIdxType, RecipeIdxType>>,

    pub holes: Vec<usize>,
}

impl<ItemIdxType: IdxTrait, RecipeIdxType: IdxTrait> Default
    for MultiBeltStore<ItemIdxType, RecipeIdxType>
{
    fn default() -> Self {
        Self {
            belts: vec![],
            holes: vec![],
        }
    }
}

impl<ItemIdxType: IdxTrait, RecipeIdxType: IdxTrait> MultiBeltStore<ItemIdxType, RecipeIdxType> {
    pub fn belts_mut(
        &mut self,
    ) -> impl IntoIterator<Item = &mut SmartBelt<ItemIdxType, RecipeIdxType>> {
        self.belts
            .iter_mut()
            .enumerate()
            .filter_map(|(i, b)| (!self.holes.contains(&i)).then_some(b))
    }

    pub fn add_belt(&mut self, belt: SmartBelt<ItemIdxType, RecipeIdxType>) -> usize {
        if let Some(hole) = self.holes.pop() {
            self.belts[hole] = belt;
            hole
        } else {
            self.belts.push(belt);
            self.belts.len() - 1
        }
    }

    pub fn remove_belt(&mut self, belt: usize) -> SmartBelt<ItemIdxType, RecipeIdxType> {
        self.holes.push(belt);

        let mut temp = SmartBelt::new(1, self.belts[belt].item);
        temp.make_circular();
        mem::swap(&mut temp, &mut self.belts[belt]);
        temp
    }
}

#[derive(Debug, Clone, serde::Deserialize, serde::Serialize, PartialEq, Eq)]
enum AnyBelt<ItemIdxType: WeakIdxTrait> {
    Smart(BeltId<ItemIdxType>),
    Sushi(usize),
}
