#[allow(clippy::module_inception)]
pub mod belt;
pub mod smart;

pub mod splitter;

mod sushi;

use std::{
    cell::UnsafeCell,
    collections::{HashMap, HashSet},
    iter::once,
    marker::PhantomData,
    mem, usize,
};

use crate::inserter::HAND_SIZE;

use serde::ser::{SerializeSeq, SerializeStruct};
use strum::IntoEnumIterator;

use crate::{
    data::DataStore,
    inserter::{
        belt_belt_inserter::BeltBeltInserter,
        belt_storage_inserter::{BeltStorageInserter, Dir},
    },
    item::{Item, usize_from},
    storage_list::{SingleItemStorages, grid_size},
};
use crate::{
    inserter::{FakeUnionStorage, Storage},
    item::Indexable,
};
use belt::{Belt, BeltLenType};
use itertools::{Either, Itertools};
use log::info;
use petgraph::{
    Direction::Outgoing,
    graph::NodeIndex,
    prelude::StableDiGraph,
    visit::{EdgeRef, IntoNodeReferences},
};
use rayon::iter::IntoParallelRefMutIterator;
use rayon::iter::{IndexedParallelIterator, ParallelIterator};
use smart::{BeltInserterInfo, InserterAdditionError, Side, SmartBelt, SpaceOccupiedError};
use splitter::{PureSplitter, SplitterDistributionMode, SplitterSide, SushiSplitter};
use sushi::{SushiBelt, SushiInfo};

use parking_lot::Mutex;

#[derive(Debug, PartialEq, Clone, Copy, serde::Deserialize, serde::Serialize)]
enum FreeIndex {
    FreeIndex(BeltLenType),
    OldFreeIndex(BeltLenType),
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, serde::Deserialize, serde::Serialize)]
struct SplitterID {
    index: u32,
}

#[derive(Debug, Clone, serde::Deserialize, serde::Serialize)]
enum Inserter {
    Out(BeltStorageInserter<{ Dir::BeltToStorage }>),
    In(BeltStorageInserter<{ Dir::StorageToBelt }>),
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
pub struct BeltStore<ItemIdxType: WeakIdxTrait> {
    pub inner: InnerBeltStore<ItemIdxType>,

    any_belts: Vec<AnyBelt<ItemIdxType>>,
    any_belt_holes: Vec<usize>,

    any_splitters: Vec<AnySplitter<ItemIdxType>>,
    any_splitter_holes: Vec<usize>,

    pub belt_graph: StableDiGraph<BeltTileId<ItemIdxType>, BeltGraphConnection<ItemIdxType>>,
    pub belt_graph_lookup: HashMap<BeltTileId<ItemIdxType>, NodeIndex>,
}

#[derive(Debug, Clone, Copy, serde::Deserialize, serde::Serialize)]
pub enum BeltGraphConnection<ItemIdxType: WeakIdxTrait> {
    Sideload {
        dest_belt_pos: BeltLenType,
    },
    BeltBeltInserter {
        source_belt_pos: BeltLenType,
        dest_belt_pos: BeltLenType,
        filter: Item<ItemIdxType>,
    },
    /// Used for handling extremely long belts and splitters
    /// Always connects the end of the source to the beginning of the destination
    Connected {
        filter: Option<Item<ItemIdxType>>,
    },
}

#[derive(Debug, serde::Deserialize)]
pub struct InnerBeltStore<ItemIdxType: WeakIdxTrait> {
    sushi_belts: Vec<SushiBelt<ItemIdxType>>,
    sushi_belt_holes: Vec<usize>,

    smart_belts: Box<[MultiBeltStore<ItemIdxType>]>,

    pub belt_belt_inserters: BeltBeltInserterStore<ItemIdxType>,

    pure_splitters: Box<[SplitterStore<ItemIdxType>]>,

    sushi_splitters: Vec<SushiSplitter<ItemIdxType>>,
    sushi_splitter_connections: Mutex<Vec<[[AnyBelt<ItemIdxType>; 2]; 2]>>,
    sushi_splitter_holes: Vec<usize>,

    pub belt_update_timers: Box<[u8]>,
    pub belt_update_timers_cumulative: Box<[u32]>,
}

impl<ItemIdxType: WeakIdxTrait> Clone for InnerBeltStore<ItemIdxType> {
    fn clone(&self) -> Self {
        Self {
            sushi_belts: self.sushi_belts.clone(),
            sushi_belt_holes: self.sushi_belt_holes.clone(),
            smart_belts: self.smart_belts.clone(),
            belt_belt_inserters: self.belt_belt_inserters.clone(),
            pure_splitters: self.pure_splitters.clone(),
            sushi_splitters: self
                .sushi_splitters
                .iter()
                .map(|sushi_splitter| unsafe {
                    // SAFETY:
                    // The only point in the code, where we use a & for mutating the unsafe cell is during the belt update.
                    // The Belt update requires a &mut of either self.smart_belts or self.sushi_belts.
                    // We currently hold a & of both of them, so no &mut can exist.
                    sushi_splitter.unsafe_clone()
                })
                .collect(),
            sushi_splitter_connections: Mutex::new(self.sushi_splitter_connections.lock().clone()),
            sushi_splitter_holes: self.sushi_splitter_holes.clone(),
            belt_update_timers: self.belt_update_timers.clone(),
            belt_update_timers_cumulative: self.belt_update_timers_cumulative.clone(),
        }
    }
}

impl<ItemIdxType: WeakIdxTrait> serde::Serialize for InnerBeltStore<ItemIdxType>
where
    ItemIdxType: serde::Serialize,
{
    fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
    where
        S: serde::Serializer,
    {
        /// CONSTRUCTING THIS IS unsafe, since it requires a precondition or the trait impl will cause UB
        struct SushiSplitterWrapper<'a, ItemIdxType: WeakIdxTrait> {
            inner: &'a Vec<SushiSplitter<ItemIdxType>>,
        }

        impl<'a, ItemIdxType: WeakIdxTrait> serde::Serialize for SushiSplitterWrapper<'a, ItemIdxType>
        where
            ItemIdxType: serde::Serialize,
        {
            fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
            where
                S: serde::Serializer,
            {
                let mut seq = serializer.serialize_seq(Some(self.inner.len()))?;
                for sushi_splitter in self.inner {
                    // SAFETY:
                    // This is safe, since SushiSplitterWrapper may only be constructed when no writes on the inner SushiSplitters are occuring
                    unsafe {
                        sushi_splitter.unsafe_serialize_elem::<S>(&mut seq)?;
                    }
                }
                seq.end()
            }
        }

        let mut state = serializer.serialize_struct("InnerBeltStore", 10)?;
        state.serialize_field("sushi_belts", &self.sushi_belts)?;
        state.serialize_field("sushi_belt_holes", &self.sushi_belt_holes)?;
        state.serialize_field("smart_belts", &self.smart_belts)?;
        state.serialize_field("belt_belt_inserters", &self.belt_belt_inserters)?;
        state.serialize_field("pure_splitters", &self.pure_splitters)?;
        state.serialize_field(
            "sushi_splitters",
            // SAFETY:
            // The only point in the code, where we use a & for mutating the unsafe cell is during the belt update.
            // The Belt update requires a &mut of either self.smart_belts or self.sushi_belts.
            // We currently hold a & of both of them, so no &mut can exist.
            &SushiSplitterWrapper {
                inner: &self.sushi_splitters,
            },
        )?;
        state.serialize_field(
            "sushi_splitter_connections",
            &self.sushi_splitter_connections,
        )?;
        state.serialize_field("sushi_splitter_holes", &self.sushi_splitter_holes)?;
        state.serialize_field("belt_update_timers", &self.belt_update_timers)?;
        state.serialize_field(
            "belt_update_timers_cumulative",
            &self.belt_update_timers_cumulative,
        )?;
        state.end()
    }
}

#[derive(Debug, Clone, serde::Deserialize, serde::Serialize)]
struct SplitterStore<ItemIdxType: WeakIdxTrait> {
    // TODO: Holes
    pub pure_splitters: Vec<PureSplitter>,

    item: PhantomData<ItemIdxType>,
}

impl<ItemIdxType: IdxTrait> SplitterStore<ItemIdxType> {}

#[derive(Debug, Clone, serde::Deserialize, serde::Serialize)]
enum AnyBeltBeltInserter {
    PurePure(usize),
    PureSushi(usize),
    SushiPure(usize),
    SushiSushi(usize),
}

#[derive(Debug, Clone, serde::Deserialize, serde::Serialize)]
pub struct BeltBeltInserterStore<ItemIdxType: WeakIdxTrait> {
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

impl<ItemIdxType: IdxTrait> InnerBeltStore<ItemIdxType> {
    fn remove_smart_belt(&mut self, id: BeltId<ItemIdxType>) -> SmartBelt<ItemIdxType> {
        self.smart_belts[usize_from(id.item.id)].remove_belt(id.index)
    }

    fn get_pure_splitter_belt_ids<'a>(
        &'a self,
        item: Item<ItemIdxType>,
        id: usize,
    ) -> [[AnyBelt<ItemIdxType>; 2]; 2] {
        info!("Had to search for splitter belt ids!");
        todo!()
    }

    fn get_sushi_splitter_belt_ids(&self, id: SplitterID) -> [[AnyBelt<ItemIdxType>; 2]; 2] {
        let maybe_ret = &mut self.sushi_splitter_connections.lock()[id.index as usize];

        for side in SplitterSide::iter() {
            let input_belt = &mut maybe_ret[0][usize::from(bool::from(side))];

            let needs_update = match input_belt {
                AnyBelt::Smart(belt_id) => {
                    if let Some((out_id, out_side)) = self.get_smart(*belt_id).output_splitter {
                        out_id != id || out_side != side
                    } else {
                        true
                    }
                },
                AnyBelt::Sushi(sushi_index) => {
                    if let Some((out_id, out_side)) = self.get_sushi(*sushi_index).output_splitter {
                        out_id != id || out_side != side
                    } else {
                        true
                    }
                },
            };

            if needs_update {
                *input_belt = self.get_sushi_splitter_belt_ids_uncached_input(id, side);
            }
        }

        for side in SplitterSide::iter() {
            let output_belt = &mut maybe_ret[1][usize::from(bool::from(side))];

            let needs_update = match output_belt {
                AnyBelt::Smart(belt_id) => {
                    if let Some((in_id, in_side)) = self.get_smart(*belt_id).input_splitter {
                        in_id != id || in_side != side
                    } else {
                        true
                    }
                },
                AnyBelt::Sushi(sushi_index) => {
                    if let Some((in_id, in_side)) = self.get_sushi(*sushi_index).input_splitter {
                        in_id != id || in_side != side
                    } else {
                        true
                    }
                },
            };

            if needs_update {
                *output_belt = self.get_sushi_splitter_belt_ids_uncached_output(id, side);
            }
        }

        maybe_ret.clone()
    }

    fn get_sushi_splitter_belt_ids_uncached_input(
        &self,
        id: SplitterID,
        side: SplitterSide,
    ) -> AnyBelt<ItemIdxType> {
        info!("Had to search for splitter belt ids!");

        for (item_index, store) in self.smart_belts.iter().enumerate() {
            let item = Item {
                id: ItemIdxType::try_from(item_index).unwrap(),
            };

            let index = store.belts.iter().position(|belt| {
                if let Some((splitter_id, splitter_side)) = belt.output_splitter {
                    splitter_id == id && splitter_side == side
                } else {
                    false
                }
            });

            if let Some(index) = index {
                return AnyBelt::Smart(BeltId { item, index });
            }
        }

        let index = self.sushi_belts.iter().position(|belt| {
            if let Some((splitter_id, splitter_side)) = belt.output_splitter {
                splitter_id == id && splitter_side == side
            } else {
                false
            }
        });

        if let Some(index) = index {
            return AnyBelt::Sushi(index);
        }

        unreachable!("Splitter is not connected to any belt (including its internal one)");
    }

    fn get_sushi_splitter_belt_ids_uncached_output(
        &self,
        id: SplitterID,
        side: SplitterSide,
    ) -> AnyBelt<ItemIdxType> {
        info!("Had to search for splitter belt ids!");

        for (item_index, store) in self.smart_belts.iter().enumerate() {
            let item = Item {
                id: ItemIdxType::try_from(item_index).unwrap(),
            };

            let index = store.belts.iter().position(|belt| {
                if let Some((splitter_id, splitter_side)) = belt.input_splitter {
                    splitter_id == id && splitter_side == side
                } else {
                    false
                }
            });

            if let Some(index) = index {
                return AnyBelt::Smart(BeltId { item, index });
            }
        }

        let index = self.sushi_belts.iter().position(|belt| {
            if let Some((splitter_id, splitter_side)) = belt.input_splitter {
                splitter_id == id && splitter_side == side
            } else {
                false
            }
        });

        if let Some(index) = index {
            return AnyBelt::Sushi(index);
        }

        unreachable!("Splitter is not connected to any belt (including its internal one)");
    }

    fn remove_sushi_belt(&mut self, id: usize) -> SushiBelt<ItemIdxType> {
        let mut temp = SushiBelt::new(0, 1);
        temp.make_circular();
        mem::swap(&mut temp, &mut self.sushi_belts[id]);
        self.sushi_belt_holes.push(id);
        temp
    }

    fn try_get_smart(&self, smart_belt_id: BeltId<ItemIdxType>) -> Option<&SmartBelt<ItemIdxType>> {
        self.smart_belts
            .get(usize_from(smart_belt_id.item.id))?
            .belts
            .get(smart_belt_id.index)
    }

    fn get_smart(&self, smart_belt_id: BeltId<ItemIdxType>) -> &SmartBelt<ItemIdxType> {
        &self.smart_belts[usize_from(smart_belt_id.item.id)].belts[smart_belt_id.index]
    }

    fn get_smart_mut(&mut self, smart_belt_id: BeltId<ItemIdxType>) -> &mut SmartBelt<ItemIdxType> {
        &mut self.smart_belts[usize_from(smart_belt_id.item.id)].belts[smart_belt_id.index]
    }

    fn get_smart_many_mut<const N: usize>(
        &mut self,
        item: Item<ItemIdxType>,
        indices: [usize; N],
    ) -> [&mut SmartBelt<ItemIdxType>; N] {
        self.smart_belts[usize_from(item.id)]
            .belts
            .get_disjoint_mut(indices)
            .unwrap()
    }

    fn get_sushi(&self, sushi_belt_id: usize) -> &SushiBelt<ItemIdxType> {
        &self.sushi_belts[sushi_belt_id]
    }

    fn get_sushi_mut(&mut self, sushi_belt_id: usize) -> &mut SushiBelt<ItemIdxType> {
        &mut self.sushi_belts[sushi_belt_id]
    }

    fn make_sushi(&mut self, id: BeltId<ItemIdxType>) -> usize {
        let belt = self.remove_smart_belt(id);

        let sushi = belt.into_sushi_belt();

        let new_id = self.add_sushi_belt(sushi);

        // Update inserters
        for ins in
            self.belt_belt_inserters.pure_to_pure_inserters[usize_from(id.item.id)].iter_mut()
        {
            if let Some((_, ((source, _), (dest, _), _, _))) = ins {
                let (is_source, is_dest) = (*source == id.index, *dest == id.index);

                if is_source || is_dest {
                    let Some((ins, ((source, source_pos), (dest, dest_pos), movetime, filter))) =
                        ins.take()
                    else {
                        unreachable!();
                    };

                    match (is_source, is_dest) {
                        (true, true) => {
                            let hole_idx = self
                                .belt_belt_inserters
                                .sushi_to_sushi_inserters
                                .iter()
                                .position(Option::is_none);

                            let new_val = Some((
                                ins,
                                ((new_id, source_pos), (new_id, dest_pos), movetime, filter),
                            ));

                            if let Some(hole_idx) = hole_idx {
                                assert!(
                                    self.belt_belt_inserters.sushi_to_sushi_inserters[hole_idx]
                                        .is_none()
                                );
                                self.belt_belt_inserters.sushi_to_sushi_inserters[hole_idx] =
                                    new_val;
                            } else {
                                self.belt_belt_inserters
                                    .sushi_to_sushi_inserters
                                    .push(new_val);
                            }
                        },
                        (true, false) => {
                            let new_val = (
                                ins,
                                (
                                    (new_id, source_pos),
                                    (
                                        BeltId {
                                            item: id.item,
                                            index: dest,
                                        },
                                        dest_pos,
                                    ),
                                    movetime,
                                    filter,
                                ),
                            );
                            self.belt_belt_inserters
                                .temp_sushi_to_smart_inserters
                                .push(new_val);
                        },
                        (false, true) => {
                            let hole_idx = self
                                .belt_belt_inserters
                                .pure_to_sushi_inserters
                                .iter()
                                .position(Option::is_none);

                            let new_val = Some((
                                ins,
                                (
                                    (
                                        BeltId {
                                            item: id.item,
                                            index: source,
                                        },
                                        source_pos,
                                    ),
                                    (new_id, dest_pos),
                                    movetime,
                                    filter,
                                ),
                            ));

                            if let Some(hole_idx) = hole_idx {
                                assert!(
                                    self.belt_belt_inserters.pure_to_sushi_inserters[hole_idx]
                                        .is_none()
                                );
                                self.belt_belt_inserters.pure_to_sushi_inserters[hole_idx] =
                                    new_val;
                            } else {
                                self.belt_belt_inserters
                                    .pure_to_sushi_inserters
                                    .push(new_val);
                            }
                        },
                        (false, false) => {
                            // Unrelated
                            unreachable!();
                        },
                    }
                }
            }
        }

        for ins in self.belt_belt_inserters.pure_to_sushi_inserters.iter_mut() {
            if let Some((_, ((source, _), (dest, _), _, _))) = ins {
                if *source == id {
                    let Some((ins, ((source, source_pos), (dest, dest_pos), movetime, filter))) =
                        ins.take()
                    else {
                        unreachable!();
                    };

                    let hole_idx = self
                        .belt_belt_inserters
                        .sushi_to_sushi_inserters
                        .iter()
                        .position(Option::is_none);

                    let new_val = Some((
                        ins,
                        ((new_id, source_pos), (dest, dest_pos), movetime, filter),
                    ));

                    if let Some(hole_idx) = hole_idx {
                        assert!(
                            self.belt_belt_inserters.sushi_to_sushi_inserters[hole_idx].is_none()
                        );
                        self.belt_belt_inserters.sushi_to_sushi_inserters[hole_idx] = new_val;
                    } else {
                        self.belt_belt_inserters
                            .sushi_to_sushi_inserters
                            .push(new_val);
                    }
                } else {
                    // Unrelated, do nothing
                }
            }
        }

        self.belt_belt_inserters
            .temp_sushi_to_smart_inserters
            .retain(
                |(ins, ((source, source_pos), (dest, dest_pos), movetime, filter))| {
                    if *dest == id {
                        let hole_idx = self
                            .belt_belt_inserters
                            .sushi_to_sushi_inserters
                            .iter()
                            .position(Option::is_none);

                        let new_val = Some((
                            *ins,
                            (
                                (*source, *source_pos),
                                (new_id, *dest_pos),
                                *movetime,
                                *filter,
                            ),
                        ));

                        if let Some(hole_idx) = hole_idx {
                            assert!(
                                self.belt_belt_inserters.sushi_to_sushi_inserters[hole_idx]
                                    .is_none()
                            );
                            self.belt_belt_inserters.sushi_to_sushi_inserters[hole_idx] = new_val;
                        } else {
                            self.belt_belt_inserters
                                .sushi_to_sushi_inserters
                                .push(new_val);
                        }

                        // Now pure pure
                        false
                    } else {
                        // Unrelated, keep
                        true
                    }
                },
            );

        new_id
    }

    fn make_smart(&mut self, index: usize, item: Item<ItemIdxType>) -> BeltId<ItemIdxType> {
        let belt = self.remove_sushi_belt(index);

        let smart = belt.into_smart_belt(item);

        let new_id = self.add_belt(smart);

        // Update inserters
        for ins in self.belt_belt_inserters.sushi_to_sushi_inserters.iter_mut() {
            if let Some((_, ((source, _), (dest, _), _, _))) = ins {
                let (is_source, is_dest) = (*source == index, *dest == index);

                if is_source || is_dest {
                    let Some((ins, ((source, source_pos), (dest, dest_pos), movetime, filter))) =
                        ins.take()
                    else {
                        unreachable!();
                    };

                    match (is_source, is_dest) {
                        (true, true) => {
                            let hole_idx = self.belt_belt_inserters.pure_to_pure_inserters
                                [usize_from(item.id)]
                            .iter()
                            .position(Option::is_none);

                            let new_val = Some((
                                ins,
                                (
                                    (new_id.index, source_pos),
                                    (new_id.index, dest_pos),
                                    movetime,
                                    filter,
                                ),
                            ));

                            if let Some(hole_idx) = hole_idx {
                                assert!(
                                    self.belt_belt_inserters.pure_to_pure_inserters
                                        [usize_from(item.id)][hole_idx]
                                        .is_none()
                                );
                                self.belt_belt_inserters.pure_to_pure_inserters
                                    [usize_from(item.id)][hole_idx] = new_val;
                            } else {
                                self.belt_belt_inserters.pure_to_pure_inserters
                                    [usize_from(item.id)]
                                .push(new_val);
                            }
                        },
                        (true, false) => {
                            let hole_idx = self
                                .belt_belt_inserters
                                .pure_to_sushi_inserters
                                .iter()
                                .position(Option::is_none);

                            let new_val = Some((
                                ins,
                                ((new_id, source_pos), (dest, dest_pos), movetime, filter),
                            ));

                            if let Some(hole_idx) = hole_idx {
                                assert!(
                                    self.belt_belt_inserters.pure_to_sushi_inserters[hole_idx]
                                        .is_none()
                                );
                                self.belt_belt_inserters.pure_to_sushi_inserters[hole_idx] =
                                    new_val;
                            } else {
                                self.belt_belt_inserters
                                    .pure_to_sushi_inserters
                                    .push(new_val);
                            }
                        },
                        (false, true) => {
                            let new_val = (
                                ins,
                                ((source, source_pos), (new_id, dest_pos), movetime, filter),
                            );

                            self.belt_belt_inserters
                                .temp_sushi_to_smart_inserters
                                .push(new_val);
                        },
                        (false, false) => {
                            // Unrelated
                            unreachable!();
                        },
                    }
                }
            }
        }

        for ins in self.belt_belt_inserters.pure_to_sushi_inserters.iter_mut() {
            if let Some((_, ((_, _), (dest, _), _, _))) = ins {
                if *dest == index {
                    let Some((ins, ((source, source_pos), (dest, dest_pos), movetime, filter))) =
                        ins.take()
                    else {
                        unreachable!();
                    };

                    let hole_idx = self.belt_belt_inserters.pure_to_pure_inserters
                        [usize_from(item.id)]
                    .iter()
                    .position(Option::is_none);

                    let new_val = Some((
                        ins,
                        (
                            (source.index, source_pos),
                            (new_id.index, dest_pos),
                            movetime,
                            filter,
                        ),
                    ));

                    if let Some(hole_idx) = hole_idx {
                        assert!(
                            self.belt_belt_inserters.pure_to_pure_inserters[usize_from(item.id)]
                                [hole_idx]
                                .is_none()
                        );
                        self.belt_belt_inserters.pure_to_pure_inserters[usize_from(item.id)]
                            [hole_idx] = new_val;
                    } else {
                        self.belt_belt_inserters.pure_to_pure_inserters[usize_from(item.id)]
                            .push(new_val);
                    }
                } else {
                    // Unrelated, do nothing
                }
            }
        }

        self.belt_belt_inserters
            .temp_sushi_to_smart_inserters
            .retain(
                |(ins, ((source, source_pos), (dest, dest_pos), movetime, filter))| {
                    if *source == index {
                        let hole_idx = self.belt_belt_inserters.pure_to_pure_inserters
                            [usize_from(item.id)]
                        .iter()
                        .position(Option::is_none);

                        let new_val = Some((
                            *ins,
                            (
                                (new_id.index, *source_pos),
                                (dest.index, *dest_pos),
                                *movetime,
                                *filter,
                            ),
                        ));

                        if let Some(hole_idx) = hole_idx {
                            assert!(
                                self.belt_belt_inserters.pure_to_pure_inserters
                                    [usize_from(item.id)][hole_idx]
                                    .is_none()
                            );
                            self.belt_belt_inserters.pure_to_pure_inserters[usize_from(item.id)]
                                [hole_idx] = new_val;
                        } else {
                            self.belt_belt_inserters.pure_to_pure_inserters[usize_from(item.id)]
                                .push(new_val);
                        }

                        // Now pure pure
                        false
                    } else {
                        // Unrelated, keep
                        true
                    }
                },
            );
        // TODO: Update id whereever necessary

        new_id
    }

    fn add_sushi_belt(&mut self, belt: SushiBelt<ItemIdxType>) -> usize {
        let sushi_idx = if let Some(hole) = self.sushi_belt_holes.pop() {
            self.sushi_belts[hole] = belt;
            hole
        } else {
            self.sushi_belts.push(belt);
            self.sushi_belts.len() - 1
        };

        sushi_idx
    }

    fn add_belt(&mut self, belt: SmartBelt<ItemIdxType>) -> BeltId<ItemIdxType> {
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
    #[profiling::function]
    fn merge_smart_belts(&mut self, front: BeltId<ItemIdxType>, back: BeltId<ItemIdxType>) -> () {
        if front.item != back.item {
            todo!("Item mismatch. Do I want to error or panic?");
            panic!("We defintively cannot continue.");
        }

        let item = front.item;

        if front == back {
            self.get_smart_mut(front).make_circular();

            return;
        }

        let mut front_len = None;

        let back_belt = self.remove_smart_belt(back);

        take_mut::take(self.get_smart_mut(front), |front| {
            front_len = Some(front.get_len());
            SmartBelt::join(front, back_belt)
        });

        let front_len = front_len.unwrap();
        // FIXME: We need to fix inserter ids and offsets!

        for ins in self.belt_belt_inserters.pure_to_pure_inserters[usize_from(item.id)].iter_mut() {
            if let Some((_, ((source, source_pos), (dest, dest_pos), _, _))) = ins {
                if *source == back.index {
                    *source = front.index;
                    *source_pos = (*source_pos)
                        .checked_add(front_len)
                        .expect("Belt too long!");
                }

                if *dest == back.index {
                    *dest = front.index;
                    *dest_pos = (*dest_pos).checked_add(front_len).expect("Belt too long!");
                }
            }
        }

        for ins in self.belt_belt_inserters.pure_to_sushi_inserters.iter_mut() {
            if let Some((_, ((source, source_pos), (des_, _), _, _))) = ins {
                if *source == back {
                    *source = front;
                    *source_pos = (*source_pos)
                        .checked_add(front_len)
                        .expect("Belt too long!");
                }
            }
        }

        for ins in self
            .belt_belt_inserters
            .temp_sushi_to_smart_inserters
            .iter_mut()
        {
            let (_, ((_, _), (dest, dest_pos), _, _)) = ins;
            if *dest == back {
                *dest = front;
                *dest_pos = (*dest_pos).checked_add(front_len).expect("Belt too long!");
            }
        }
    }

    // TODO: What does this return?
    #[profiling::function]
    fn merge_sushi_belts(&mut self, front: usize, back: usize) {
        if front == back {
            self.get_sushi_mut(front).make_circular();
            return;
        }

        let mut front_len = None;

        let back_belt = self.remove_sushi_belt(back);

        take_mut::take(self.get_sushi_mut(front), |front| {
            front_len = Some(front.get_len());
            SushiBelt::join(front, back_belt)
        });

        let front_len = front_len.unwrap();

        for ins in self.belt_belt_inserters.sushi_to_sushi_inserters.iter_mut() {
            if let Some((_, ((source, source_pos), (dest, dest_pos), _, _))) = ins {
                if *source == back {
                    *source = front;
                    *source_pos = (*source_pos)
                        .checked_add(front_len)
                        .expect("Belt too long!");
                }

                if *dest == back {
                    *dest = front;
                    *dest_pos = (*dest_pos).checked_add(front_len).expect("Belt too long!");
                }
            }
        }

        for ins in self.belt_belt_inserters.pure_to_sushi_inserters.iter_mut() {
            if let Some((_, ((_, _), (dest, dest_pos), _, _))) = ins {
                if *dest == back {
                    *dest = front;
                    *dest_pos = (*dest_pos).checked_add(front_len).expect("Belt too long!");
                }
            }
        }

        for ins in self
            .belt_belt_inserters
            .temp_sushi_to_smart_inserters
            .iter_mut()
        {
            let (_, ((source, source_pos), (des_, _), _, _)) = ins;
            if *source == back {
                *source = front;
                *source_pos = (*source_pos)
                    .checked_add(front_len)
                    .expect("Belt too long!");
            }
        }

        // FIXME: We need to fix splitter ids and offsets!
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

        // FIXME: We need to fix inserter ids and offsets!
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

        // FIXME: We need to fix inserter ids and offsets!
    }
}

#[derive(Debug, Clone, Copy, serde::Deserialize, serde::Serialize, PartialEq, Eq)]
pub enum SplitterTileId {
    Any(usize),
}

#[derive(Debug, Clone, serde::Deserialize, serde::Serialize)]
enum AnySplitter<ItemIdxType: WeakIdxTrait> {
    Pure(Item<ItemIdxType>, usize),
    Sushi(SplitterID),
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

impl<ItemIdxType: IdxTrait> BeltStore<ItemIdxType> {
    pub fn new<RecipeIdxType: IdxTrait>(
        data_store: &DataStore<ItemIdxType, RecipeIdxType>,
    ) -> Self {
        Self {
            inner: InnerBeltStore {
                sushi_belts: vec![],
                sushi_belt_holes: vec![],
                smart_belts: vec![
                    MultiBeltStore {
                        belt_ty: vec![],
                        belts: vec![],
                        holes: vec![]
                    };
                    data_store.item_display_names.len()
                ]
                .into_boxed_slice(),
                belt_belt_inserters: BeltBeltInserterStore {
                    pure_to_pure_inserters: vec![vec![]; data_store.item_display_names.len()]
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
                    data_store.item_display_names.len()
                ]
                .into_boxed_slice(),

                sushi_splitters: vec![],
                sushi_splitter_connections: Mutex::new(vec![]),
                sushi_splitter_holes: vec![],

                belt_update_timers: vec![0; data_store.belt_infos.len()].into_boxed_slice(),
                belt_update_timers_cumulative: vec![0; data_store.belt_infos.len()]
                    .into_boxed_slice(),
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

        fn check_incoming_edges<ItemIdxType: IdxTrait>(
            store: &mut BeltStore<ItemIdxType>,
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
                                | BeltGraphConnection::Connected { filter: None } => {
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
                                BeltGraphConnection::Connected {
                                    filter: Some(filter),
                                }
                                | BeltGraphConnection::BeltBeltInserter {
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

                let belt_item_check = items.flatten().all_equal_value();

                match belt_item_check {
                    Ok(belt_items) => {
                        if belt_items == inserter_item {
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

                let belt_item_check = items.flatten().all_equal_value();

                let goal_item = match belt_item_check {
                    Ok(belt_items) => {
                        if belt_items == *incoming_belts_item.get_or_insert(belt_items) {
                            // All items are the correct type!
                            belt_items
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

    fn fix_graph(&mut self, tile_id: BeltTileId<ItemIdxType>) {
        let mut done_propagating = HashSet::default();
        let mut dedup = HashSet::default();
        let mut done = HashMap::default();

        self.propagate_sushi_if_necessary(tile_id, &mut done_propagating, &mut dedup, &mut done);
    }

    fn merge_and_fix(
        &mut self,
        front_tile_id: BeltTileId<ItemIdxType>,
        back_tile_id: BeltTileId<ItemIdxType>,
    ) {
        // if !self.belt_graph_lookup.contains_key(&back_tile_id) {
        //     // We assume we already merged them in the graph
        //     assert!(self.belt_graph_lookup.contains_key(&front_tile_id));
        //     return;
        // }
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
                BeltGraphConnection::Sideload { .. } => {
                    // TODO: Fix sideload inserter
                    todo!()
                },
                BeltGraphConnection::BeltBeltInserter { .. } => {
                    // TODO: Fix BeltBeltInserter inserter
                    todo!()
                },
                BeltGraphConnection::Connected { .. } => {
                    // TODO: Fix Connection inserter
                    todo!()
                },
            }
        }

        let edges: Vec<_> = self
            .belt_graph
            .edges_directed(
                self.belt_graph_lookup[&back_tile_id],
                petgraph::Direction::Incoming,
            )
            .map(|edge| (edge.source(), edge.target(), edge.id()))
            .chain(
                self.belt_graph
                    .edges_directed(
                        self.belt_graph_lookup[&back_tile_id],
                        petgraph::Direction::Outgoing,
                    )
                    .map(|edge| (edge.source(), edge.target(), edge.id())),
            )
            .collect();

        for (source, target, id) in edges {
            let conn = self.belt_graph.remove_edge(id).unwrap();

            // FIXME: Fix the simulation entities as well!!!!
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
                // TODO: Is this correct?
                BeltGraphConnection::Connected { filter } => {
                    assert_eq!(
                        target, self.belt_graph_lookup[&back_tile_id],
                        "Any end of belt connections must be at the end of the back belt, since the front is being merged"
                    );
                    BeltGraphConnection::Connected { filter }
                },
            };

            if source == self.belt_graph_lookup[&back_tile_id] {
                self.belt_graph
                    .add_edge(self.belt_graph_lookup[&front_tile_id], target, new_conn);
            } else if target == self.belt_graph_lookup[&back_tile_id] {
                self.belt_graph
                    .add_edge(source, self.belt_graph_lookup[&front_tile_id], new_conn);
            } else {
                unreachable!()
            }
        }

        assert!(
            self.belt_graph
                .edges_directed(
                    self.belt_graph_lookup[&back_tile_id],
                    petgraph::Direction::Incoming,
                )
                .map(|edge| (edge.source(), edge.target(), edge.id()))
                .chain(
                    self.belt_graph
                        .edges_directed(
                            self.belt_graph_lookup[&back_tile_id],
                            petgraph::Direction::Outgoing,
                        )
                        .map(|edge| (edge.source(), edge.target(), edge.id())),
                )
                .count()
                == 0
        );

        self.belt_graph
            .remove_node(self.belt_graph_lookup[&back_tile_id])
            .expect("Node not found");

        self.belt_graph_lookup
            .remove(&back_tile_id)
            .expect("Lookup not found");

        #[cfg(debug_assertions)]
        {
            assert!(
                self.belt_graph_lookup
                    .iter()
                    .all(|(k, v)| { self.belt_graph.node_weight(*v).unwrap() == k })
            );

            assert!(
                self.belt_graph
                    .node_references()
                    .all(|(node_index, tile_id)| {
                        self.belt_graph_lookup.get(tile_id).unwrap() == &node_index
                    })
            );
        }

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
    pub fn get_items_which_could_end_up_on_that_belt(
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
                    let items_all_empty = belt.items().all(|loc| loc.is_none());
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
                                matches!(ins, Inserter::In(_)).then_some(*item)
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
                | BeltGraphConnection::Connected { filter: None } => {
                    self.get_items_which_could_end_up_on_that_belt(*belt, dedup, done);
                    done.get(belt)
                        .unwrap_or(&vec![])
                        .iter()
                        .copied()
                        .collect::<Vec<_>>()
                },
                BeltGraphConnection::Connected {
                    filter: Some(filter),
                }
                | BeltGraphConnection::BeltBeltInserter {
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

    #[profiling::function]
    pub fn update<'a, 'b, RecipeIdxType: IdxTrait>(
        &mut self,
        storages_by_item: impl IndexedParallelIterator<Item = SingleItemStorages<'a, 'b>>,
        data_store: &DataStore<ItemIdxType, RecipeIdxType>,
    ) where
        'b: 'a,
    {
        #[cfg(debug_assertions)]
        {
            assert!(
                self.belt_graph_lookup
                    .iter()
                    .all(|(belt_tile_id, _)| { self.get_len(*belt_tile_id) > 0 })
            );

            assert!(
                self.any_belt_holes
                    .iter()
                    .map(|hole| BeltTileId::AnyBelt(*hole, PhantomData))
                    .all(|belt_tile_id| { !self.belt_graph_lookup.contains_key(&belt_tile_id) })
            );

            assert!(
                self.any_belt_holes
                    .iter()
                    .map(|hole| BeltTileId::AnyBelt(*hole, PhantomData))
                    .all(|belt_tile_id| {
                        !self.belt_graph.node_weights().contains(&belt_tile_id)
                    })
            );

            assert!(
                (0..self.any_splitters.len())
                    .filter(|idx| !self.any_splitter_holes.contains(idx))
                    .map(|any_splitter_idx| self
                        .get_splitter_belt_ids(SplitterTileId::Any(any_splitter_idx)))
                    .all(|[inputs, outputs]| {
                        inputs.into_iter().all(|id| {
                            self.belt_graph
                                .edges_directed(
                                    self.belt_graph_lookup[&id],
                                    petgraph::Direction::Outgoing,
                                )
                                .count()
                                >= 2
                        }) && outputs.into_iter().all(|id| {
                            self.belt_graph
                                .edges_directed(
                                    self.belt_graph_lookup[&id],
                                    petgraph::Direction::Incoming,
                                )
                                .count()
                                >= 2
                        })
                    })
            );
        }
        // TODO: Once every (maybe more or less) check a single belt and check if it still needs to be sushi

        // Increase the belt timers
        for ((current_timer, increase), cumulative) in self
            .inner
            .belt_update_timers
            .iter_mut()
            .zip(data_store.belt_infos.iter().map(|info| info.timer_increase))
            .zip(self.inner.belt_update_timers_cumulative.iter_mut())
        {
            *current_timer = (*current_timer)
                .checked_add(increase)
                .expect("Belt Timer wrapped!");

            *cumulative += u32::from(increase);
        }

        {
            profiling::scope!("Update Splitters");
            self.inner
                .sushi_splitters
                .par_iter_mut()
                .for_each(|splitter| splitter.update());
        }

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
                    profiling::scope!(
                        "Pure Belt Update",
                        format!("Item: {}", data_store.item_display_names[item_id]).as_str()
                    );

                    let grid_size = grid_size(item, data_store);

                    {
                        profiling::scope!("Update Belt");
                        for (belt, ty) in belt_store.belts.iter_mut().zip(&belt_store.belt_ty) {
                            // TODO: Avoid last minute decision making
                            if self.inner.belt_update_timers[usize::from(*ty)] >= 120 {
                                belt.update(&self.inner.sushi_splitters);
                            }
                            belt.update_inserters(item_storages, grid_size);
                        }
                    }
                    // {
                    //     profiling::scope!("Update BeltStorageInserters");
                    //     for belt in &mut belt_store.belts {

                    //     }
                    // }

                    {
                        profiling::scope!("Update PurePure Inserters");
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
                                ins.update(source_loc, dest_loc, *cooldown, HAND_SIZE, (), |_| {
                                    filter
                                        .map(|filter_item| filter_item == item)
                                        .unwrap_or(true)
                                });
                            }

                            let source_loc = *source_loc;
                            let dest_loc = *dest_loc;

                            {
                                profiling::scope!("Update update_first_free_pos");
                                if !source_loc {
                                    belt_store.belts[*source].update_first_free_pos(*source_pos);
                                }

                                if dest_loc {
                                    belt_store.belts[*dest].remove_first_free_pos_maybe(*dest_pos);
                                }
                            }
                        }
                    }

                    for splitter in &mut splitters.pure_splitters {
                        // TODO: Assert that the item is the same!
                        //splitter.update(belt_store);
                    }
                },
            );

        {
            {
                profiling::scope!("Prune Sushi Belts");
                while !self.inner.sushi_belts.is_empty()
                    && self
                        .inner
                        .sushi_belt_holes
                        .contains(&(self.inner.sushi_belts.len() - 1))
                {
                    let removed_idx = self.inner.sushi_belts.len() - 1;
                    let placeholder_sushi_belt = self.inner.sushi_belts.pop().unwrap();
                    assert_eq!(placeholder_sushi_belt.get_len(), 1);
                    let old_len = self.inner.sushi_belt_holes.len();
                    self.inner.sushi_belt_holes.retain(|v| *v != removed_idx);
                    assert_eq!(self.inner.sushi_belt_holes.len(), old_len - 1);
                }
            }
            profiling::scope!("SushiBelt Update");
            self.inner
                .sushi_belts
                .par_iter_mut()
                .for_each(|sushi_belt| {
                    if self.inner.belt_update_timers[usize::from(sushi_belt.ty)] >= 120 {
                        sushi_belt.update(&self.inner.sushi_splitters);
                    }
                });
        }

        {
            profiling::scope!("SushiBelt Inserter Update");
            for sushi_belt in &mut self.inner.sushi_belts {
                // TODO: Update inserters!
            }
        }

        for current_timer in self.inner.belt_update_timers.iter_mut() {
            *current_timer %= 120;
        }
    }

    pub fn get_splitter_belt_ids(
        &self,
        splitter_id: SplitterTileId,
    ) -> [[BeltTileId<ItemIdxType>; 2]; 2] {
        let belts: [[AnyBelt<ItemIdxType>; 2]; 2] = match splitter_id {
            SplitterTileId::Any(index) => match self.any_splitters[index] {
                AnySplitter::Pure(item, id) => self.inner.get_pure_splitter_belt_ids(item, id),
                AnySplitter::Sushi(id) => self.inner.get_sushi_splitter_belt_ids(id),
            },
        };

        let [inputs, outputs] = belts;

        // FIXME: This is O(n) over the number of belts :/
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

    fn add_belt(&mut self, belt: SmartBelt<ItemIdxType>) -> BeltTileId<ItemIdxType> {
        let id = self.inner.add_belt(belt);

        let new_id = self.add_smart_to_any_list(id);

        let index = self.belt_graph.add_node(new_id);
        self.belt_graph_lookup.insert(new_id, index);

        new_id
    }

    pub fn add_empty_belt(&mut self, ty: u8, len: u16) -> BeltTileId<ItemIdxType> {
        let sushi_idx = self.inner.add_sushi_belt(SushiBelt::new(ty, len));

        let new_id = self.add_sushi_to_any_list(sushi_idx);

        let index = self.belt_graph.add_node(new_id);
        self.belt_graph_lookup.insert(new_id, index);

        new_id
    }

    fn add_sushi_belt(&mut self, belt: SushiBelt<ItemIdxType>) -> BeltTileId<ItemIdxType> {
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
        storage_id: FakeUnionStorage,
    ) -> Result<(), SpaceOccupiedError> {
        let handle_sushi_belt =
            |belt: &mut SushiBelt<ItemIdxType>| belt.add_in_inserter(filter, pos, storage_id);

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
                                unreachable!();
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
                            Err(SpaceOccupiedError) => unreachable!(),
                        }
                    },
                }
            },
        }

        self.fix_graph(id);

        return Ok(());
    }

    pub fn add_belt_storage_inserter(
        &mut self,
        filter: Item<ItemIdxType>,
        id: BeltTileId<ItemIdxType>,
        pos: BeltLenType,
        storage_id: FakeUnionStorage,
    ) -> Result<(), SpaceOccupiedError> {
        let handle_sushi_belt =
            |belt: &mut SushiBelt<ItemIdxType>| belt.add_out_inserter(filter, pos, storage_id);

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
                                return Err(SpaceOccupiedError);
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
                    (AnyBelt::Sushi(source_idx), AnyBelt::Smart(dest_id)) => {
                        self.inner
                            .belt_belt_inserters
                            .temp_sushi_to_smart_inserters
                            .push((
                                BeltBeltInserter::new(),
                                ((*source_idx, 0), (*dest_id, dest.1), 0, None),
                            ));

                        self.inner
                            .belt_belt_inserters
                            .temp_sushi_to_smart_inserters
                            .len()
                    },
                    // unreachable!(
                    //     "If a sushi belt sideloads onto a smart belt, it can never be pure"
                    // ),
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
        impl IntoIterator<Item = SushiInfo<ItemIdxType>> + Clone + use<'a, ItemIdxType>,
        impl IntoIterator<Item = SushiInfo<ItemIdxType>> + Clone + use<'a, ItemIdxType>,
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
                            source_belt_pos: _,
                            dest_belt_pos: _,
                            filter,
                        }
                        | BeltGraphConnection::Connected {
                            filter: Some(filter),
                        } => SushiInfo::Pure(Some(*filter)),
                        BeltGraphConnection::Sideload { dest_belt_pos: _ }
                        | BeltGraphConnection::Connected { filter: None } => {
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

    pub fn get_inserter_info_at(
        &self,
        belt: BeltTileId<ItemIdxType>,
        belt_pos: u16,
    ) -> Option<BeltInserterInfo> {
        match belt {
            BeltTileId::AnyBelt(idx, _) => match self.any_belts[idx] {
                AnyBelt::Smart(belt_id) => self.inner.smart_belts[belt_id.item.into_usize()].belts
                    [belt_id.index as usize]
                    .get_inserter_info_at(belt_pos),
                AnyBelt::Sushi(index) => {
                    self.inner.sushi_belts[index].get_inserter_info_at(belt_pos)
                },
            },
        }
    }

    pub fn get_inserter_item(
        &self,
        belt: BeltTileId<ItemIdxType>,
        belt_pos: u16,
    ) -> Item<ItemIdxType> {
        match belt {
            BeltTileId::AnyBelt(idx, _) => match self.any_belts[idx] {
                AnyBelt::Smart(belt_id) => belt_id.item,
                AnyBelt::Sushi(index) => self.inner.sushi_belts[index].get_inserter_item(belt_pos),
            },
        }
    }

    pub fn update_belt_storage_inserter_src<RecipeIdxType: IdxTrait>(
        &mut self,
        belt: BeltTileId<ItemIdxType>,
        belt_pos: u16,
        src_item: Item<ItemIdxType>,
        new_src: Storage<RecipeIdxType>,
        data_store: &DataStore<ItemIdxType, RecipeIdxType>,
    ) {
        match belt {
            BeltTileId::AnyBelt(idx, _) => match self.any_belts[idx] {
                AnyBelt::Smart(belt_id) => {
                    assert_eq!(src_item, belt_id.item);
                    self.inner.smart_belts[belt_id.item.into_usize()].belts[belt_id.index]
                        .set_inserter_storage_id(
                            belt_pos,
                            FakeUnionStorage::from_storage_with_statics_at_zero(
                                belt_id.item,
                                new_src,
                                data_store,
                            ),
                        );
                },
                AnyBelt::Sushi(index) => {
                    self.inner.sushi_belts[index].set_inserter_storage_id(
                        belt_pos,
                        FakeUnionStorage::from_storage_with_statics_at_zero(
                            src_item, new_src, data_store,
                        ),
                    );
                },
            },
        }
    }

    pub fn update_belt_storage_inserter_dest<RecipeIdxType: IdxTrait>(
        &mut self,
        belt: BeltTileId<ItemIdxType>,
        belt_pos: u16,
        dest_item: Item<ItemIdxType>,
        new_dest: Storage<RecipeIdxType>,
        data_store: &DataStore<ItemIdxType, RecipeIdxType>,
    ) {
        match belt {
            BeltTileId::AnyBelt(idx, _) => match self.any_belts[idx] {
                AnyBelt::Smart(belt_id) => {
                    assert_eq!(dest_item, belt_id.item);
                    self.inner.smart_belts[belt_id.item.into_usize()].belts[belt_id.index]
                        .set_inserter_storage_id(
                            belt_pos,
                            FakeUnionStorage::from_storage_with_statics_at_zero(
                                belt_id.item,
                                new_dest,
                                data_store,
                            ),
                        );
                },
                AnyBelt::Sushi(index) => {
                    self.inner.sushi_belts[index].set_inserter_storage_id(
                        belt_pos,
                        FakeUnionStorage::from_storage_with_statics_at_zero(
                            dest_item, new_dest, data_store,
                        ),
                    );
                },
            },
        }
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
                        Ok((_, new_id)) => {
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
                AnyBelt::Sushi(_) => None,
            },
        }
    }

    pub fn get_item_iter(
        &self,
        id: BeltTileId<ItemIdxType>,
    ) -> Either<
        impl Iterator<Item = Option<Item<ItemIdxType>>>,
        impl Iterator<Item = Option<Item<ItemIdxType>>>,
    > {
        match id {
            BeltTileId::AnyBelt(index, _) => match &self.any_belts[index] {
                AnyBelt::Smart(smart_belt) => {
                    Either::Left(self.inner.get_smart(*smart_belt).items())
                },
                AnyBelt::Sushi(sushi_belt) => {
                    Either::Right(self.inner.get_sushi(*sushi_belt).items())
                },
            },
        }
    }

    pub fn get_belt_progress(&self, ty: u8) -> u8 {
        self.inner.belt_update_timers[usize::from(ty)]
    }

    pub fn get_last_moved_pos(&self, id: BeltTileId<ItemIdxType>) -> BeltLenType {
        match id {
            BeltTileId::AnyBelt(index, _) => match &self.any_belts[index] {
                AnyBelt::Smart(smart_belt) => self.inner.get_smart(*smart_belt).last_moving_spot,
                AnyBelt::Sushi(sushi_belt) => self.inner.get_sushi(*sushi_belt).last_moving_spot,
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

    pub fn merge_belts<RecipeIdxType: IdxTrait>(
        &mut self,
        front_tile_id: BeltTileId<ItemIdxType>,
        back_tile_id: BeltTileId<ItemIdxType>,
        data_store: &DataStore<ItemIdxType, RecipeIdxType>,
    ) -> (BeltTileId<ItemIdxType>, BeltLenType) {
        // self.merge_and_fix(front_tile_id, back_tile_id);

        if front_tile_id == back_tile_id {
            // Make them cicular
            match front_tile_id {
                BeltTileId::AnyBelt(idx, _) => match self.any_belts[idx] {
                    AnyBelt::Smart(belt_id) => self.inner.merge_smart_belts(belt_id, belt_id),
                    AnyBelt::Sushi(idx) => self.inner.merge_sushi_belts(idx, idx),
                },
            }
            return (front_tile_id, self.get_len(front_tile_id));
        }

        let ret = match (front_tile_id, back_tile_id) {
            (BeltTileId::AnyBelt(front, _), BeltTileId::AnyBelt(back, _)) => {
                assert_ne!(front, back);
                match self.any_belts.get_disjoint_mut([front, back]).unwrap() {
                    [
                        AnyBelt::Smart(front_smart_belt),
                        AnyBelt::Smart(back_smart_belt),
                    ] => {
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
                    [
                        AnyBelt::Smart(front_smart_belt),
                        AnyBelt::Sushi(back_sushi_belt),
                    ] => {
                        let front_smart_belt = *front_smart_belt;

                        let front_sushi_idx = self.inner.make_sushi(front_smart_belt);
                        self.any_belts[front] = AnyBelt::Sushi(front_sushi_idx);

                        // We now have two Sushi belts, retry:
                        self.merge_belts(front_tile_id, back_tile_id, data_store)
                    },
                    [
                        AnyBelt::Sushi(front_sushi_belt),
                        AnyBelt::Smart(back_smart_belt),
                    ] => {
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
                    [
                        AnyBelt::Sushi(front_sushi_belt),
                        AnyBelt::Sushi(back_sushi_belt),
                    ] => {
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
        };

        #[cfg(debug_assertions)]
        {
            assert!(
                self.belt_graph_lookup
                    .iter()
                    .all(|(belt_tile_id, _)| { self.get_len(*belt_tile_id) > 0 })
            );

            assert!(
                self.any_belt_holes
                    .iter()
                    .map(|hole| BeltTileId::AnyBelt(*hole, PhantomData))
                    .all(|belt_tile_id| { !self.belt_graph_lookup.contains_key(&belt_tile_id) })
            );

            assert!(
                self.any_belt_holes
                    .iter()
                    .map(|hole| BeltTileId::AnyBelt(*hole, PhantomData))
                    .all(|belt_tile_id| {
                        !self.belt_graph.node_weights().contains(&belt_tile_id)
                    })
            );

            assert!(
                (0..self.any_splitters.len())
                    .filter(|idx| !self.any_splitter_holes.contains(idx))
                    .map(|any_splitter_idx| self
                        .get_splitter_belt_ids(SplitterTileId::Any(any_splitter_idx)))
                    .all(|[inputs, outputs]| {
                        inputs.into_iter().all(|id| {
                            self.belt_graph
                                .edges_directed(
                                    self.belt_graph_lookup[&id],
                                    petgraph::Direction::Outgoing,
                                )
                                .count()
                                >= 2
                        }) && outputs.into_iter().all(|id| {
                            self.belt_graph
                                .edges_directed(
                                    self.belt_graph_lookup[&id],
                                    petgraph::Direction::Incoming,
                                )
                                .count()
                                >= 2
                        })
                    })
            );

            assert!(
                self.inner
                    .belt_belt_inserters
                    .pure_to_pure_inserters
                    .iter()
                    .enumerate()
                    .all(|(item, ins)| {
                        {
                            ins.iter().flatten().all(
                                |(
                                    _ins_info,
                                    ((source, _source_pos), (dest, _dest_pos), _movetime, _filter),
                                )| {
                                    !self.inner.smart_belts[item].holes.contains(source)
                                        && !self.inner.smart_belts[item].holes.contains(dest)
                                },
                            )
                        }
                    }),
                "{:?}",
                self.inner.belt_belt_inserters.pure_to_pure_inserters[0]
            );
            assert!(
                self.inner
                    .belt_belt_inserters
                    .sushi_to_sushi_inserters
                    .iter()
                    .flatten()
                    .all(
                        |(
                            _ins_info,
                            ((source, _source_pos), (dest, _dest_pos), _movetime, _filter),
                        )| {
                            !self.inner.sushi_belt_holes.contains(source)
                                && !self.inner.sushi_belt_holes.contains(dest)
                        },
                    )
            );
            assert!(
                self.inner
                    .belt_belt_inserters
                    .pure_to_sushi_inserters
                    .iter()
                    .flatten()
                    .all(
                        |(
                            _ins_info,
                            ((source, _source_pos), (dest, _dest_pos), _movetime, _filter),
                        )| {
                            !self.inner.smart_belts[usize_from(source.item.id)]
                                .holes
                                .contains(&source.index)
                                && !self.inner.sushi_belt_holes.contains(dest)
                        },
                    )
            );
            assert!(
                self.inner
                    .belt_belt_inserters
                    .temp_sushi_to_smart_inserters
                    .iter()
                    .all(
                        |(
                            _ins_info,
                            ((source, _source_pos), (dest, _dest_pos), _movetime, _filter),
                        )| {
                            !self.inner.sushi_belt_holes.contains(source)
                                && !self.inner.smart_belts[usize_from(dest.item.id)]
                                    .holes
                                    .contains(&dest.index)
                        },
                    )
            );
        }

        ret
    }

    pub fn add_splitter(&mut self, info: SplitterInfo<ItemIdxType>) -> SplitterTileId {
        let new_splitter = SushiSplitter {
            in_mode: info.in_mode,
            out_mode: info.out_mode,
            inputs: [UnsafeCell::new(None), UnsafeCell::new(None)],
            outputs: [UnsafeCell::new(None), UnsafeCell::new(None)],
        };

        // match info.filter {
        // FIXME: Even with this splitter seems to still be transmuting items
        for input in &info.input_belts {
            for output in &info.output_belts {
                self.add_graph_connection_and_fix(
                    *input,
                    *output,
                    BeltGraphConnection::Connected { filter: None },
                );
            }
        }

        // }

        let new_splitter_connections = [
            info.input_belts.map(|any| match any {
                BeltTileId::AnyBelt(idx, _) => match self.any_belts[idx] {
                    AnyBelt::Smart(belt_id) => AnyBelt::Smart(belt_id),
                    AnyBelt::Sushi(index) => AnyBelt::Sushi(index),
                },
            }),
            info.output_belts.map(|any| match any {
                BeltTileId::AnyBelt(idx, _) => match self.any_belts[idx] {
                    AnyBelt::Smart(belt_id) => AnyBelt::Smart(belt_id),
                    AnyBelt::Sushi(index) => AnyBelt::Sushi(index),
                },
            }),
        ];

        let index = if let Some(hole_idx) = self.inner.sushi_splitter_holes.pop() {
            self.inner.sushi_splitters[hole_idx] = new_splitter;
            self.inner.sushi_splitter_connections.lock()[hole_idx] = new_splitter_connections;
            hole_idx
        } else {
            self.inner.sushi_splitters.push(new_splitter);
            self.inner
                .sushi_splitter_connections
                .lock()
                .push(new_splitter_connections);
            self.inner.sushi_splitters.len() - 1
        };

        let any_idx = if let Some(hole_idx) = self.any_splitter_holes.pop() {
            self.any_splitters[hole_idx] = AnySplitter::Sushi(SplitterID {
                index: index.try_into().unwrap(),
            });
            hole_idx
        } else {
            self.any_splitters.push(AnySplitter::Sushi(SplitterID {
                index: index.try_into().unwrap(),
            }));
            self.any_splitters.len() - 1
        };

        // Since we distribute the SplitterID once for each side, no two belts point to the same UnsafeCell in the splitter, maintaining the safety invariant
        for side in SplitterSide::iter() {
            let belt_id = info.input_belts[usize::from(bool::from(side))];

            match belt_id {
                BeltTileId::AnyBelt(idx, _) => match self.any_belts[idx] {
                    AnyBelt::Smart(belt_id) => {
                        self.inner.get_smart_mut(belt_id).add_output_splitter(
                            SplitterID {
                                index: index.try_into().unwrap(),
                            },
                            side,
                        )
                    },
                    AnyBelt::Sushi(sushi_index) => {
                        self.inner.get_sushi_mut(sushi_index).add_output_splitter(
                            SplitterID {
                                index: index.try_into().unwrap(),
                            },
                            side,
                        )
                    },
                },
            }
        }

        for side in SplitterSide::iter() {
            // This is flipped from the add_x_splitter calls below since this is from the POV of the splitter and those calls are from the pow of the belt
            let belt_id = info.output_belts[usize::from(bool::from(side))];

            match belt_id {
                BeltTileId::AnyBelt(idx, _) => match self.any_belts[idx] {
                    AnyBelt::Smart(belt_id) => {
                        self.inner.get_smart_mut(belt_id).add_input_splitter(
                            SplitterID {
                                index: index.try_into().unwrap(),
                            },
                            side,
                        )
                    },
                    AnyBelt::Sushi(sushi_index) => {
                        self.inner.get_sushi_mut(sushi_index).add_input_splitter(
                            SplitterID {
                                index: index.try_into().unwrap(),
                            },
                            side,
                        )
                    },
                },
            }
        }

        SplitterTileId::Any(any_idx)
    }

    /// Remove the Splitter from the update list and its connections to belts.
    /// Does NOT remove any length of belt originally associated with a splitter world entity!
    pub fn remove_splitter(&mut self, tile_id: SplitterTileId) {
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
pub struct MultiBeltStore<ItemIdxType: WeakIdxTrait> {
    pub belt_ty: Vec<u8>,
    pub belts: Vec<SmartBelt<ItemIdxType>>,

    pub holes: Vec<usize>,
}

impl<ItemIdxType: IdxTrait> Default for MultiBeltStore<ItemIdxType> {
    fn default() -> Self {
        Self {
            belt_ty: vec![],
            belts: vec![],
            holes: vec![],
        }
    }
}

impl<ItemIdxType: IdxTrait> MultiBeltStore<ItemIdxType> {
    pub fn belts_mut(&mut self) -> impl Iterator<Item = &mut SmartBelt<ItemIdxType>> {
        self.belts
            .iter_mut()
            .enumerate()
            .filter_map(|(i, b)| (!self.holes.contains(&i)).then_some(b))
    }

    pub fn add_belt(&mut self, belt: SmartBelt<ItemIdxType>) -> usize {
        if let Some(hole) = self.holes.pop() {
            self.belt_ty[hole] = belt.ty;
            self.belts[hole] = belt;
            hole
        } else {
            self.belt_ty.push(belt.ty);
            self.belts.push(belt);
            self.belts.len() - 1
        }
    }

    pub fn remove_belt(&mut self, belt: usize) -> SmartBelt<ItemIdxType> {
        self.holes.push(belt);

        let mut temp = SmartBelt::new(0, 1, self.belts[belt].item);
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
