#[allow(clippy::module_inception)]
pub mod belt;
pub mod smart;

pub mod splitter;

mod sushi;

use std::{marker::PhantomData, mem};

use crate::{
    data::DataStore,
    inserter::{
        belt_belt_inserter::{BeltBeltInserter, SushiBeltBeltInserter},
        belt_storage_inserter::{BeltStorageInserter, Dir},
        Storage,
    },
    item::{usize_from, Item},
    storage_list::{grid_size, num_recipes},
};
use belt::{Belt, BeltLenType, NoSpaceError};
use rayon::iter::IntoParallelRefMutIterator;
use rayon::iter::{IndexedParallelIterator, ParallelIterator};
use smart::{EmptyBelt, InserterAdditionError, Side, SmartBelt, SpaceOccupiedError};
use splitter::Splitter;
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

#[cfg(test)]
use crate::item::Item;
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
}

#[derive(Debug, Clone, serde::Deserialize, serde::Serialize)]
struct InnerBeltStore<ItemIdxType: WeakIdxTrait, RecipeIdxType: WeakIdxTrait> {
    sushi_belts: Vec<SushiBelt<ItemIdxType, RecipeIdxType>>,
    sushi_belt_holes: Vec<usize>,

    smart_belts: Box<[MultiBeltStore<ItemIdxType, RecipeIdxType>]>,

    belt_belt_inserters: BeltBeltInserterStore<ItemIdxType>,

    splitters: SplitterStore<ItemIdxType>,
}

#[derive(Debug, Clone, serde::Deserialize, serde::Serialize)]
struct SplitterStore<ItemIdxType: WeakIdxTrait> {
    // TODO: Holes
    pub splitters: Vec<Splitter>,
    item: PhantomData<ItemIdxType>,
}

impl<ItemIdxType: IdxTrait> SplitterStore<ItemIdxType> {
    pub fn get_splitter_belt_ids<'a>(
        &'a self,
        id: usize,
    ) -> [impl IntoIterator<Item = BeltTileId<ItemIdxType>> + use<'a, ItemIdxType>; 2] {
        let index_to_id = move |index| BeltTileId::AnyBelt(index, PhantomData);

        [
            self.splitters[id]
                .input_belts
                .iter()
                .copied()
                .map(index_to_id),
            self.splitters[id]
                .output_belts
                .iter()
                .copied()
                .map(index_to_id),
        ]
    }
}

impl<ItemIdxType: IdxTrait> BeltBeltInserterStore<ItemIdxType> {
    pub fn update<RecipeIdxType: IdxTrait>(
        &mut self,
        belts: &mut BeltStore<ItemIdxType, RecipeIdxType>,
        data_store: &DataStore<ItemIdxType, RecipeIdxType>,
    ) {
        self.inserters
            .par_iter_mut()
            .zip(belts.belts.par_iter_mut())
            .for_each(|(inserters, belts)| {
                for ins in inserters {
                    let [source, dest] = if ins.1.source.0 == ins.1.dest.0 {
                        // We are taking and inserting from the same belt
                        debug_assert!(ins.1.source.1 != ins.1.dest.1);
                        belts.belts[ins.1.source.0].get_two([ins.1.source.1, ins.1.dest.1])
                    } else {
                        let [source_belt, dest_belt] = belts
                            .belts
                            .get_many_mut([ins.1.source.0, ins.1.dest.0])
                            .expect("Index out of bounds");
                        [
                            source_belt.get_mut(ins.1.source.1),
                            dest_belt.get_mut(ins.1.dest.1),
                        ]
                    };
                    ins.0.update(source, dest, todo!());
                }
            });
    }
}

#[derive(Debug, Clone, serde::Deserialize, serde::Serialize)]
struct BeltBeltInserterStore<ItemIdxType: WeakIdxTrait> {
    // TODO: Holes
    pub inserters: Box<[Vec<(BeltBeltInserter, BeltBeltInserterInfo<ItemIdxType>)>]>,

    sideload_inserters: Vec<(BeltBeltInserter, BeltBeltInserterInfo<ItemIdxType>)>,
    // pub sushi_inserters: Vec<(
    //     SushiBeltBeltInserter<ItemIdxType>,
    //     BeltBeltInserterInfo<ItemIdxType>,
    // )>,
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
        let mut temp = SmartBelt::new(0, id.item);
        mem::swap(
            &mut temp,
            &mut self.smart_belts[usize_from(id.item.id)].belts[id.index],
        );
        temp
    }

    fn remove_sushi_belt(&mut self, id: usize) -> SushiBelt<ItemIdxType, RecipeIdxType> {
        let mut temp = SushiBelt::new(0);
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
            .get_many_mut(indices)
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

        // TODO: Update id whereever necessary

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

            return todo!("Return value");
        }

        let back_belt = self.remove_smart_belt(back);

        take_mut::take(self.get_smart_mut(front), |front| {
            SmartBelt::join(front, back_belt)
        });

        todo!("Return value")
    }

    // TODO: What does this return?
    fn merge_sushi_belts(&mut self, front: usize, back: usize) -> () {
        if front == back {
            todo!("Make circular");

            return todo!("Return value");
        }

        let back_belt = self.remove_sushi_belt(back);

        take_mut::take(self.get_sushi_mut(front), |front| {
            SushiBelt::join(front, back_belt)
        });

        todo!("Return value")
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
                    inserters: vec![vec![]; data_store.item_names.len()].into_boxed_slice(),
                    sideload_inserters: vec![],
                },
                splitters: SplitterStore {
                    splitters: vec![],
                    item: PhantomData,
                },
            },
            any_belts: vec![],
            any_belt_holes: vec![],
        }
    }

    pub fn update<'a>(
        &mut self,
        num_grids_total: usize,
        storages_by_item: impl IntoIterator<Item = &'a mut [&'a mut [u8]]>
            + IndexedParallelIterator<Item = &'a mut [&'a mut [u8]]>,
        data_store: &DataStore<ItemIdxType, RecipeIdxType>,
    ) {
        // FIXME: We need the belts to be sorted by item, to update them in parallel...
        self.inner
            .smart_belts
            .par_iter_mut()
            .zip(storages_by_item)
            .zip(self.inner.belt_belt_inserters.inserters.par_iter_mut())
            .zip(self.splitters.splitters.par_iter_mut())
            .enumerate()
            .for_each(
                |(item_id, (((belt_store, item_storages), belt_belt_inserters), splitters))| {
                    let grid_size = grid_size(
                        Item {
                            id: item_id.try_into().unwrap(),
                        },
                        data_store,
                    );
                    let num_recipes = num_recipes(
                        Item {
                            id: item_id.try_into().unwrap(),
                        },
                        data_store,
                    );

                    for belt in &mut belt_store.belts {
                        belt.update();
                        belt.update_inserters(
                            item_storages,
                            num_grids_total,
                            num_recipes,
                            grid_size,
                        );
                    }

                    for (ins, info) in belt_belt_inserters {
                        let [source, dest] = if info.source.0 == info.dest.0 {
                            assert_ne!(
                                info.source.1, info.dest.1,
                                "An inserter cannot take and drop off on the same tile"
                            );
                            // We are taking and placing onto the same belt
                            let belt = &mut belt_store.belts[info.source.0];

                            belt.get_two([info.source.1.into(), info.dest.1.into()])
                        } else {
                            let [inp, out] = belt_store
                                .belts
                                .get_many_mut([info.source.0, info.dest.0])
                                .unwrap();

                            [inp.get_mut(info.source.1), out.get_mut(info.dest.1)]
                        };
                        ins.update(source, dest, info.cooldown);
                    }

                    for splitter in splitters {
                        splitter.update(belt_store);
                    }
                },
            );

        self.inner
            .sushi_belts
            .iter_mut()
            .for_each(|sushi_belt| todo!("Updating sushi belts..."));
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
        let item = belt.item;
        let id = self.inner.add_belt(belt);

        let index = if let Some(hole) = self.any_belt_holes.pop() {
            self.any_belts[hole] = AnyBelt::Smart(id);
            hole
        } else {
            self.any_belts.push(AnyBelt::Smart(id));
            self.any_belts.len() - 1
        };
        BeltTileId::AnyBelt(index, PhantomData)
    }

    pub fn add_empty_belt(&mut self, len: u16) -> BeltTileId<ItemIdxType> {
        let sushi_idx = self.inner.add_sushi_belt(SushiBelt::new(len));

        let index = if let Some(hole) = self.any_belt_holes.pop() {
            self.any_belts[hole] = AnyBelt::Sushi(sushi_idx);
            hole
        } else {
            self.any_belts.push(AnyBelt::Sushi(sushi_idx));
            self.any_belts.len() - 1
        };
        BeltTileId::AnyBelt(index, PhantomData)
    }

    fn add_sushi_belt(
        &mut self,
        belt: SushiBelt<ItemIdxType, RecipeIdxType>,
    ) -> BeltTileId<ItemIdxType> {
        let sushi_idx = self.inner.add_sushi_belt(belt);

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
        match id {
            BeltTileId::AnyBelt(index, _) => match &mut self.any_belts[index] {
                AnyBelt::Smart(smart_belt_id) => {
                    let new_belt_id = self
                        .inner
                        .break_smart_belt_at(*smart_belt_id, belt_pos_to_break_at);

                    if let Some(new_belt_id) = new_belt_id {
                        BreakBeltResultInfo {
                            kept_id: id,
                            new_belt: Some((todo!(), Side::BACK)),
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
                        BreakBeltResultInfo {
                            kept_id: id,
                            new_belt: Some((todo!(), Side::BACK)),
                        }
                    } else {
                        BreakBeltResultInfo {
                            kept_id: id,
                            new_belt: None,
                        }
                    }
                },
            },
        }
    }

    // pub fn get_len(&self, id: BeltTileId<ItemIdxType>) -> u16 {
    //     match id {
    //         // BeltTileId::SushiBeltId(idx) => self.sushi_belts[idx].get_len(),
    //         // BeltTileId::SmartBeltId(BeltId { item, index }) => {
    //         //     self.belts[Into::<usize>::into(item.id)].belts[index].get_len()
    //         // },
    //         BeltTileId::AnyBelt(index, _) => self.any_belts[index].get_len(),
    //     }
    // }

    // fn get_belt(&self, id: BeltTileId<ItemIdxType>) -> &dyn Belt<ItemIdxType> {
    //     match id {
    //         BeltTileId::AnyBelt(index, _) => &self.any_belts[index],
    //     }
    // }

    // fn get_belt_mut(&mut self, id: BeltTileId<ItemIdxType>) -> &mut dyn Belt<ItemIdxType> {
    //     match id {
    //         BeltTileId::AnyBelt(index, _) => &mut self.any_belts[index],
    //     }
    // }

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
                                self.make_sushi(index);
                            },
                        }

                        let AnyBelt::Sushi(sushi_belt_id) = &mut self.any_belts[index] else {
                            unreachable!();
                        };
                        let sushi_belt = &mut self.inner.sushi_belts[*sushi_belt_id];
                        debug_assert!(handle_sushi_belt(sushi_belt).is_ok());
                    },
                    AnyBelt::Sushi(sushi_belt_index) => {
                        let sushi_belt = &mut self.inner.sushi_belts[*sushi_belt_index];

                        match handle_sushi_belt(sushi_belt) {
                            Ok(()) => {
                                // It succeeded!
                                match sushi_belt.check_sushi() {
                                    SushiInfo::Sushi => {},
                                    SushiInfo::Pure(Some(item)) => {self.make_smart(index, item);},
                                    SushiInfo::Pure(None) => unreachable!("I don't think it is possible to add an inserter and still be empty")
                                }
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
                                self.make_sushi(index);
                            },
                        }

                        let AnyBelt::Sushi(sushi_belt_id) = &mut self.any_belts[index] else {
                            unreachable!();
                        };
                        let sushi_belt = &mut self.inner.sushi_belts[*sushi_belt_id];
                        debug_assert!(handle_sushi_belt(sushi_belt).is_ok());
                    },
                    AnyBelt::Sushi(sushi_belt_id) => {
                        let sushi_belt = &mut self.inner.sushi_belts[*sushi_belt_id];
                        // Adding an inserter cannot change a sushi belt to a smart belt
                        match handle_sushi_belt(sushi_belt) {
                            Ok(()) => {
                                let (belt_belt_filter_in, belt_belt_filter_out) =
                                    self.get_belt_belt_inserter_sushi_lists(id);
                                match sushi_belt.check_sushi(belt_belt_filter_in, belt_belt_filter_out) {
                                    SushiInfo::Sushi => {},
                                    SushiInfo::Pure(Some(item)) => {self.make_smart(index, item);},
                                    SushiInfo::Pure(None) => unreachable!("I don't think it is possible to add an inserter and still be empty")
                                }
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
        // FIXME: Holes
        self.inner.belt_belt_inserters.sideload_inserters.push((
            BeltBeltInserter::new(),
            BeltBeltInserterInfo {
                source: (source, 0),
                dest,
                cooldown: 0,
                item: PhantomData,
            },
        ));
        self.inner.belt_belt_inserters.sideload_inserters.len() - 1
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
            self.inner.belt_belt_inserters
                .inserters
                .iter()
                .enumerate()
                .map(|(i, v)| {
                    (
                        v,
                        Item {
                            id: i.try_into().unwrap(),
                        },
                    )
                })
                .map(move |(v, item)| {
                    v.iter().filter_map(move |ins| {
                        (ins.1.dest.0 == id).then_some(SushiInfo::Pure(Some(item)))
                    })
                })
                .flatten()
                .chain(
                    self.inner.belt_belt_inserters
                        .sideload_inserters
                        .iter()
                        .filter_map(move |ins| {
                            if ins.1.dest.0 == id {
                                match ins.1.source.0 {
                                    BeltTileId::AnyBelt(index, _) => match &self.any_belts[index] {
                                        AnyBelt::Smart(smart_belt) => {
                                            Some(SushiInfo::Pure(Some(smart_belt.item)))
                                        },
                                        AnyBelt::Sushi(_) => Some(SushiInfo::Sushi),
                                    },
                                }
                            } else {
                                None
                            }
                        }),
                ),
            self.inner.belt_belt_inserters
                .inserters
                .iter()
                .enumerate()
                .map(|(i, v)| {
                    (
                        v,
                        Item {
                            id: i.try_into().unwrap(),
                        },
                    )
                })
                .map(move |(v, item)| {
                    v.iter().filter_map(move |ins| {
                        (ins.1.source.0 == id).then_some(SushiInfo::Pure(Some(item)))
                    })
                })
                .flatten()
                .chain(
                    self.inner.belt_belt_inserters
                        .sideload_inserters
                        .iter()
                        .filter_map(move |ins| {
                            if ins.1.source.0 == id {
                                match ins.1.dest.0 {
                                    BeltTileId::AnyBelt(index, _) => match &self.any_belts[index] {
                                        AnyBelt::Smart(_) => {
                                            unreachable!("Here a sushi belt is sideloading onto a smart belt. this MUST be impossible")
                                        },
                                        AnyBelt::Sushi(_) => Some(SushiInfo::Sushi),
                                    },
                                }
                            } else {
                                None
                            }
                        }),
                ),
        )
    }

    fn add_belt_belt_inserter(
        &mut self,
        from: (BeltTileId<ItemIdxType>, u16),
        to: (BeltTileId<ItemIdxType>, u16),
        filter: Item<ItemIdxType>,
        info: BeltBeltInserterAdditionInfo<ItemIdxType>,
    ) -> usize {
        match from.0 {
            BeltTileId::AnyBelt(index, _) => {
                self.inner.any_belt_attached_belt_belt_inserter_filters[index]
                    .push((filter, from.1));
            },
        }

        match to.0 {
            BeltTileId::AnyBelt(index, _) => {
                self.inner.any_belt_attached_belt_belt_inserter_filters[index].push((filter, to.1));
            },
        }

        self.inner.belt_belt_inserters.inserters[info.filter.id.into()].push((
            BeltBeltInserter::new(),
            BeltBeltInserterInfo {
                source: (from.0, from.1),
                dest: (to.0, to.1),
                cooldown: info.cooldown,
                item: PhantomData,
            },
        ));
        self.belt_belt_inserters.inserters[info.filter.id.into()].len() - 1
    }

    fn remove_belt_belt_inserter(&mut self, inserter_index: usize) {
        todo!()
    }

    fn remove_inserter(&mut self, id: BeltTileId<ItemIdxType>, pos: BeltLenType) {
        match id {
            BeltTileId::AnyBelt(index, _) => match &mut self.any_belts[index] {
                AnyBelt::Smart(smart_belt_id) => {
                    let smart_belt = self.inner.get_smart_mut(*smart_belt_id);
                    smart_belt.remove_inserter(pos)
                },
                AnyBelt::Sushi(sushi_belt_id) => {
                    let sushi_belt = &mut self.inner.sushi_belts[*sushi_belt_id];
                    sushi_belt.remove_inserter(pos);
                    match sushi_belt.check_sushi() {
                        SushiInfo::Sushi => {},
                        SushiInfo::Pure(item) => {
                            if let Some(item) = item {
                                self.make_smart(index, item);
                            }
                        },
                    }
                },
            },
        }
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
                AnyBelt::Smart(smart_belt) => smart_belt.items(),
                AnyBelt::Sushi(sushi_belt) => sushi_belt.items(),
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
                match self.any_belts.get_many_mut([front, back]).unwrap() {
                    [AnyBelt::Smart(front_smart_belt), AnyBelt::Smart(back_smart_belt)] => {
                        if front_smart_belt.item == back_smart_belt.item {
                            let back_smart_belt = self.inner.remove_smart_belt(*back_smart_belt);
                            let front_smart_belt = self.inner.get_smart_mut(*front_smart_belt);
                            take_mut::take(front_smart_belt, |front_belt| {
                                SmartBelt::join(front_belt, back_smart_belt)
                            });
                            (front_tile_id, front_smart_belt.get_len())
                        } else {
                            todo!("Convert belts to sushi and merge")
                        }
                    },
                    [AnyBelt::Smart(front_smart_belt), AnyBelt::Sushi(back_sushi_belt)] => {
                        todo!()
                    },
                    [AnyBelt::Sushi(front_sushi_belt), AnyBelt::Smart(back_smart_belt)] => todo!(),
                    [AnyBelt::Sushi(front_sushi_belt), AnyBelt::Sushi(back_sushi_belt)] => todo!(),
                }
            },
        }
    }

    pub fn add_splitter(&mut self, splitter: Splitter) -> usize {
        self.inner.splitters.splitters.push(splitter);
        self.inner.splitters.splitters.len() - 1
    }

    pub fn remove_splitter(&mut self, index: usize) {
        todo!()
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
}

#[derive(Debug, Clone, serde::Deserialize, serde::Serialize)]
enum AnyBelt<ItemIdxType: WeakIdxTrait> {
    Smart(BeltId<ItemIdxType>),
    Sushi(usize),
}
