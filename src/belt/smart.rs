use std::iter::repeat;

use crate::{
    inserter::{InserterState, MOVETIME, belt_storage_inserter::BeltStorageInserter},
    item::{IdxTrait, Item, WeakIdxTrait},
    storage_list::SingleItemStorages,
};
use log::trace;
use std::mem;

use super::{
    FreeIndex, Inserter, SplitterID,
    belt::{Belt, BeltLenType, NoSpaceError},
    splitter::{SplitterSide, SushiSplitter},
    sushi::{SushiBelt, SushiInserterStore},
};
use crate::inserter::FakeUnionStorage;
use crate::inserter::HAND_SIZE;

#[allow(clippy::module_name_repetitions)]
#[derive(Debug, Clone, serde::Deserialize, serde::Serialize)]
pub struct SmartBelt<ItemIdxType: WeakIdxTrait> {
    pub(super) ty: u8,

    pub(super) is_circular: bool,
    pub(super) first_free_index: FreeIndex,
    /// Important, zero_index must ALWAYS be used using mod len
    pub(super) zero_index: BeltLenType,
    pub(super) locs: Box<[bool]>,
    pub(super) inserters: InserterStore,

    pub(super) item: Item<ItemIdxType>,

    pub last_moving_spot: BeltLenType,

    pub(super) input_splitter: Option<(SplitterID, SplitterSide)>,
    pub(super) output_splitter: Option<(SplitterID, SplitterSide)>,
}

#[derive(Debug, Clone, serde::Deserialize, serde::Serialize, Default)]
pub struct EmptyBelt {
    ty: u8,

    is_circular: bool,
    pub len: u16,
}

#[derive(Debug, Clone, serde::Deserialize, serde::Serialize)]
pub struct InserterStore {
    pub(super) inserters: Box<[Inserter]>,
    pub(super) offsets: Box<[u16]>,
}

#[derive(Debug)]
pub struct BeltInserterInfo {
    pub outgoing: bool,
    pub state: InserterState,
    pub connection: FakeUnionStorage,
}

const MIN_INSERTER_SPACING: usize = 8;

#[derive(Debug)]
pub struct SpaceOccupiedError;

pub(super) enum InserterAdditionError {
    SpaceOccupied,
    ItemMismatch,
}

impl<ItemIdxType: IdxTrait> SmartBelt<ItemIdxType> {
    #[must_use]
    pub fn new(ty: u8, len: u16, item: Item<ItemIdxType>) -> Self {
        Self {
            ty,

            is_circular: false,
            first_free_index: FreeIndex::FreeIndex(0),
            zero_index: 0,
            locs: vec![false; len.into()].into_boxed_slice(),
            inserters: InserterStore {
                inserters: vec![].into_boxed_slice(),
                offsets: vec![].into_boxed_slice(),
            },

            item,

            last_moving_spot: len,

            input_splitter: None,
            output_splitter: None,
        }
    }

    fn into_loc_index(&self, pos: BeltLenType) -> usize {
        usize::from((self.zero_index + pos) % self.get_len())
    }

    pub(super) fn into_sushi_belt(self) -> SushiBelt<ItemIdxType> {
        let Self {
            ty,

            is_circular,
            first_free_index,
            zero_index,
            locs,
            inserters: InserterStore { inserters, offsets },
            item,

            last_moving_spot,

            input_splitter,
            output_splitter,
        } = self;

        SushiBelt {
            ty,

            is_circular,
            locs: locs
                .iter()
                .map(|loc| if *loc { Some(item) } else { None })
                .collect(),
            first_free_index,
            zero_index,
            inserters: SushiInserterStore {
                inserters: inserters
                    .into_iter()
                    .map(|inserter| (inserter, item))
                    .collect(),
                offsets,
            },

            last_moving_spot,

            input_splitter,
            output_splitter,
        }
    }

    pub fn make_circular(&mut self) {
        assert!(
            self.input_splitter.is_none(),
            "A circular belt may NOT be connected to a splitter"
        );
        assert!(
            self.output_splitter.is_none(),
            "A circular belt may NOT be connected to a splitter"
        );
        self.is_circular = true;
        self.last_moving_spot = 0;
    }

    pub fn add_input_splitter(&mut self, id: SplitterID, side: SplitterSide) {
        assert!(
            self.input_splitter.is_none(),
            "Tried to add splitter where one already existed"
        );
        assert!(
            !self.is_circular,
            "A circular belt can never be attached to a splitter!"
        );

        self.input_splitter = Some((id, side));
    }

    pub fn add_output_splitter(&mut self, id: SplitterID, side: SplitterSide) {
        assert!(
            self.output_splitter.is_none(),
            "Tried to add splitter where one already existed"
        );
        assert!(
            !self.is_circular,
            "A circular belt can never be attached to a splitter!"
        );

        self.output_splitter = Some((id, side));
    }

    pub fn remove_input_splitter(&mut self) -> Option<(SplitterID, SplitterSide)> {
        self.input_splitter.take()
    }

    pub fn remove_output_splitter(&mut self) -> Option<(SplitterID, SplitterSide)> {
        self.output_splitter.take()
    }

    // pub fn get_take_item_fn<'a>(
    //     &'a mut self,
    //     pos: BeltLenType,
    // ) -> Option<impl FnMut() + use<'a, ItemIdxType, RecipeIdxType>> {
    //     self.locs[self.into_loc_index(pos)].then_some(move || {
    //         self.update_first_free_pos(pos);
    //         self.locs[self.into_loc_index(pos)] = false;
    //     })
    // }

    // pub fn get_put_item_fn<'a>(
    //     &'a mut self,
    //     pos: BeltLenType,
    // ) -> Option<impl FnMut() + use<'a, ItemIdxType, RecipeIdxType>> {
    //     (!self.locs[self.into_loc_index(pos)]).then_some(move || {
    //         let first_possible_free_pos = match self.first_free_index {
    //             FreeIndex::OldFreeIndex(i) | FreeIndex::FreeIndex(i) => i,
    //         };
    //         if pos == first_possible_free_pos {
    //             self.first_free_index =
    //                 FreeIndex::OldFreeIndex(BeltLenType::try_from(pos).unwrap());
    //         }

    //         self.locs[self.into_loc_index(pos)] = true;
    //     })
    // }

    // FIXME: These do not set free pos correctly if items are inserted
    pub fn get_front_mut(&mut self) -> &mut bool {
        self.get_mut(0)
    }

    pub fn get_back_mut(&mut self) -> &mut bool {
        self.get_mut(self.get_len() - 1)
    }

    // FIXME: This function should no be public
    pub fn get_mut(&mut self, index: u16) -> &mut bool {
        &mut self.locs[self.into_loc_index(index)]
    }

    // fn get_take_and_put<'a>(
    //     &mut self,
    //     indices: [BeltLenType; 2],
    // ) -> (
    //     Option<impl FnMut() + use<'a, ItemIdxType, RecipeIdxType>>,
    //     Option<impl FnMut() + use<'a, ItemIdxType, RecipeIdxType>>,
    // ) {
    //     (
    //         self.locs[self.into_loc_index(indices[0])].then_some(move || {
    //             self.update_first_free_pos(indices[0]);
    //             self.locs[self.into_loc_index(indices[0])] = false;
    //         }),
    //         (!self.locs[self.into_loc_index(indices[1])]).then_some(move || {
    //             let first_possible_free_pos = match self.first_free_index {
    //                 FreeIndex::OldFreeIndex(i) | FreeIndex::FreeIndex(i) => i,
    //             };
    //             if indices[1] == first_possible_free_pos {
    //                 self.first_free_index =
    //                     FreeIndex::OldFreeIndex(BeltLenType::try_from(indices[1]).unwrap());
    //             }

    //             self.locs[self.into_loc_index(indices[1])] = true;
    //         }),
    //     )
    // }

    pub fn get_two(&mut self, indices: [BeltLenType; 2]) -> [&mut bool; 2] {
        self.locs
            .get_disjoint_mut(indices.map(|i| self.into_loc_index(i)))
            .expect("Index out of bounds or same")
    }

    pub fn change_inserter_storage_id(&mut self, old: FakeUnionStorage, new: FakeUnionStorage) {
        for inserter in &mut self.inserters.inserters {
            match inserter {
                Inserter::Out(inserter) => {
                    if inserter.storage_id == old {
                        inserter.storage_id = new;
                    }
                },
                Inserter::In(inserter) => {
                    if inserter.storage_id == old {
                        inserter.storage_id = new;
                    }
                },
            }
        }
    }

    pub fn set_inserter_storage_id(&mut self, belt_pos: u16, new: FakeUnionStorage) {
        let mut pos = 0;

        for (offset, inserter) in self
            .inserters
            .offsets
            .iter()
            .zip(self.inserters.inserters.iter_mut())
        {
            pos += offset;
            if pos == belt_pos {
                match inserter {
                    Inserter::Out(belt_storage_inserter) => {
                        belt_storage_inserter.storage_id = new;
                    },
                    Inserter::In(belt_storage_inserter) => {
                        belt_storage_inserter.storage_id = new;
                    },
                }
                return;
            } else if pos > belt_pos {
                unreachable!(
                    "Tried to set_inserter_storage_id with position {belt_pos}, which does not contain an inserter. {:?}",
                    self.inserters.offsets
                );
            }
            pos += 1;
        }
    }

    #[must_use]
    pub fn get_inserter_info_at(&self, belt_pos: u16) -> Option<BeltInserterInfo> {
        let mut pos = 0;

        for (offset, inserter) in self
            .inserters
            .offsets
            .iter()
            .zip(self.inserters.inserters.iter())
        {
            pos += offset;
            if pos == belt_pos {
                return Some(match inserter {
                    Inserter::Out(belt_storage_inserter) => BeltInserterInfo {
                        outgoing: true,
                        state: belt_storage_inserter.state,
                        connection: belt_storage_inserter.storage_id,
                    },
                    Inserter::In(belt_storage_inserter) => BeltInserterInfo {
                        outgoing: false,
                        state: belt_storage_inserter.state,
                        connection: belt_storage_inserter.storage_id,
                    },
                });
            } else if pos > belt_pos {
                return None;
            }
            pos += 1;
        }

        None
    }

    pub fn remove_inserter(&mut self, pos: BeltLenType) -> FakeUnionStorage {
        assert!(
            usize::from(pos) < self.locs.len(),
            "Bounds check {pos} >= {}",
            self.locs.len()
        );

        let mut pos_after_last_inserter = 0;
        let mut i = 0;

        for offset in &self.inserters.offsets {
            let next_inserter_pos = pos_after_last_inserter + offset;

            match next_inserter_pos.cmp(&pos) {
                std::cmp::Ordering::Greater => panic!(
                    "The belt did not have an inserter at position specified to remove inserter from"
                ), // This is the index to insert at
                std::cmp::Ordering::Equal => break,

                std::cmp::Ordering::Less => {
                    pos_after_last_inserter = next_inserter_pos + 1;
                    i += 1;
                },
            }
        }

        let mut old_inserter = None;
        take_mut::take(&mut self.inserters.inserters, |ins| {
            let mut ins = ins.into_vec();
            old_inserter = Some(ins.remove(i));
            ins.into_boxed_slice()
        });
        let old_inserter = old_inserter.unwrap();
        let mut removed = None;
        take_mut::take(&mut self.inserters.offsets, |offs| {
            let mut offs = offs.into_vec();
            removed = Some(offs.remove(i));
            offs.into_boxed_slice()
        });
        let removed = removed.unwrap();
        // The offset after i (which has now shifted left to i)
        if let Some(next_offs) = self.inserters.offsets.get_mut(i) {
            *next_offs += removed + 1
        }

        match old_inserter {
            Inserter::Out(inserter) => inserter.storage_id,
            Inserter::In(inserter) => inserter.storage_id,
        }
    }

    // FIXME: This is horrendously slow. it breaks my tests since they are compiled without optimizations!!!
    // FIXME: This is super slow on belts with lots of inserters
    /// # Errors
    /// If the index is already used by another inserter
    /// # Panics
    /// If the index is greater or equal to the length of the belt
    pub(super) fn add_out_inserter(
        &mut self,
        filter: Item<ItemIdxType>,
        index: u16,
        storage_id: FakeUnionStorage,
    ) -> Result<(), InserterAdditionError> {
        assert!(
            usize::from(index) < self.locs.len(),
            "Bounds check {index} >= {}",
            self.locs.len()
        );

        if filter != self.item {
            return Err(InserterAdditionError::ItemMismatch);
        }

        let mut pos_after_last_inserter = 0;
        let mut i = 0;

        for offset in &self.inserters.offsets {
            let next_inserter_pos = pos_after_last_inserter + offset;

            match next_inserter_pos.cmp(&index) {
                std::cmp::Ordering::Greater => break, // This is the index to insert at
                std::cmp::Ordering::Equal => return Err(InserterAdditionError::SpaceOccupied),

                std::cmp::Ordering::Less => {
                    pos_after_last_inserter = next_inserter_pos + 1;
                    i += 1;
                },
            }
        }

        // Insert at i
        let new_inserter_offset = index - pos_after_last_inserter;
        take_mut::take(&mut self.inserters.inserters, |ins| {
            let mut ins = ins.into_vec();
            ins.insert(i, Inserter::Out(BeltStorageInserter::new(storage_id)));
            ins.into_boxed_slice()
        });
        take_mut::take(&mut self.inserters.offsets, |offs| {
            let mut offs = offs.into_vec();
            offs.insert(i, new_inserter_offset);
            offs.into_boxed_slice()
        });

        let next = self.inserters.offsets.get_mut(i + 1);

        if let Some(next_offs) = next {
            *next_offs -= new_inserter_offset + 1;
        }

        Ok(())
    }

    /// # Errors
    /// If the index is already used by another inserter
    /// # Panics
    /// If the index is greater or equal to the length of the belt
    pub fn add_in_inserter(
        &mut self,
        filter: Item<ItemIdxType>,
        index: u16,
        storage_id: FakeUnionStorage,
    ) -> Result<(), InserterAdditionError> {
        assert!(
            usize::from(index) < self.locs.len(),
            "Bounds check {index} >= {}",
            self.locs.len()
        );

        let mut pos_after_last_inserter = 0;
        let mut i = 0;

        for offset in &self.inserters.offsets {
            let next_inserter_pos = pos_after_last_inserter + offset;

            match next_inserter_pos.cmp(&index) {
                std::cmp::Ordering::Greater => break, // This is the index to insert at
                std::cmp::Ordering::Equal => return Err(InserterAdditionError::SpaceOccupied),

                std::cmp::Ordering::Less => {
                    pos_after_last_inserter = next_inserter_pos + 1;
                    i += 1;
                },
            }
        }

        // We only only return an item mismatch if we know the space is free, so we do not transition to sushi,
        // And then fail anyway
        if filter != self.item {
            return Err(InserterAdditionError::ItemMismatch);
        }

        // Insert at i
        let new_inserter_offset = index - pos_after_last_inserter;
        take_mut::take(&mut self.inserters.inserters, |ins| {
            let mut ins = ins.into_vec();
            ins.insert(i, Inserter::In(BeltStorageInserter::new(storage_id)));
            ins.into_boxed_slice()
        });
        take_mut::take(&mut self.inserters.offsets, |offs| {
            let mut offs = offs.into_vec();
            offs.insert(i, new_inserter_offset);
            offs.into_boxed_slice()
        });

        let next = self.inserters.offsets.get_mut(i + 1);

        if let Some(next_offs) = next {
            *next_offs -= new_inserter_offset + 1;
        }

        Ok(())
    }

    fn get_inserter(&self, index: u16) -> Option<&Inserter> {
        let mut pos_after_last_inserter = 0;

        for (i, offset) in self.inserters.offsets.iter().enumerate() {
            let next_inserter_pos = pos_after_last_inserter + offset;

            match next_inserter_pos.cmp(&index) {
                std::cmp::Ordering::Equal => return Some(&self.inserters.inserters[i]),
                std::cmp::Ordering::Greater => return None,

                std::cmp::Ordering::Less => pos_after_last_inserter = next_inserter_pos + 1,
            }
        }

        None
    }

    #[inline(never)]
    pub fn update_inserters<'a, 'b>(
        &mut self,
        storages: SingleItemStorages<'a, 'b>,
        grid_size: usize,
    ) {
        debug_assert_eq!(self.inserters.inserters.len(), self.inserters.offsets.len());
        let mut items_mut_iter = Self::items_mut(&mut self.locs, self.zero_index);

        let mut i = 0;

        let mut first_possible_free_pos = match self.first_free_index {
            FreeIndex::OldFreeIndex(i) | FreeIndex::FreeIndex(i) => i,
        };

        // TODO: We do a last second check here. Maybe this could be better with two struct, though then we do not have compile time assurance that no two inserters overlap.

        for (offset, ins) in self
            .inserters
            .offsets
            .iter()
            .zip(self.inserters.inserters.iter_mut())
        {
            i += usize::from(*offset);
            let loc = items_mut_iter.nth(usize::from(*offset));

            match loc {
                Some(loc) => {
                    let old = *loc;
                    match ins {
                        Inserter::Out(inserter) => {
                            inserter.update(loc, storages, MOVETIME, HAND_SIZE, grid_size)
                        },
                        Inserter::In(inserter) => {
                            inserter.update(loc, storages, MOVETIME, HAND_SIZE, grid_size)
                        },
                    }

                    // TODO: Make sure this is actually correct
                    if old != *loc {
                        // the inserter changed something.
                        if !*loc && i < usize::from(first_possible_free_pos) {
                            // This is the new first free pos.
                            first_possible_free_pos = BeltLenType::try_from(i).unwrap();
                            self.first_free_index =
                                FreeIndex::FreeIndex(BeltLenType::try_from(i).unwrap());
                        } else if *loc && i == usize::from(first_possible_free_pos) {
                            // This was the old first free pos
                            self.first_free_index =
                                FreeIndex::OldFreeIndex(BeltLenType::try_from(i).unwrap());
                        }
                    }
                },
                None => unreachable!(
                    "Adding the offsets of the inserters is bigger than the length of the belt."
                ),
            }

            i += 1;
        }
    }

    pub fn remove_first_free_pos_maybe(&mut self, now_filled_pos: BeltLenType) {
        match self.first_free_index {
            FreeIndex::OldFreeIndex(index) | FreeIndex::FreeIndex(index) => {
                if now_filled_pos == index {
                    self.first_free_index = FreeIndex::OldFreeIndex(now_filled_pos);
                }
            },
        }
    }

    pub fn update_first_free_pos(&mut self, now_empty_pos: BeltLenType) {
        match self.first_free_index {
            FreeIndex::OldFreeIndex(index) | FreeIndex::FreeIndex(index) => {
                if now_empty_pos <= index {
                    self.first_free_index = FreeIndex::FreeIndex(now_empty_pos);
                }
            },
        }
    }

    fn update_first_free_pos_maybe(&mut self, now_maybe_empty_pos: BeltLenType) {
        match self.first_free_index {
            FreeIndex::OldFreeIndex(index) | FreeIndex::FreeIndex(index) => {
                if now_maybe_empty_pos < index {
                    self.first_free_index = FreeIndex::OldFreeIndex(now_maybe_empty_pos);
                }
            },
        }
    }

    // #[profiling::function]
    fn find_and_update_real_first_free_index(&mut self) -> Option<BeltLenType> {
        let new_free_index = match self.first_free_index {
            FreeIndex::FreeIndex(index) => {
                debug_assert!(
                    self.query_item(index).is_none(),
                    "Free index not free {self:?}"
                );

                index
            },
            FreeIndex::OldFreeIndex(index) => {
                trace!("HAD TO SEARCH FOR FIRST FREE INDEX!");

                let search_start_index = index;

                let mut iter = self
                    .locs
                    .iter()
                    .skip(self.zero_index as usize)
                    .chain(self.locs.iter().take(self.zero_index as usize))
                    .skip(usize::from(search_start_index));

                debug_assert_eq!(
                    iter.clone().count(),
                    self.locs.len() - usize::from(search_start_index)
                );

                debug_assert!(
                    self.locs
                        .iter()
                        .skip(self.zero_index as usize)
                        .chain(self.locs.iter().take(self.zero_index as usize))
                        .take(usize::from(search_start_index))
                        .all(|v| *v)
                );

                // We now have an iterator which is effectively the belt in the correct order,
                // starting at search_start_index

                let new_free_index = BeltLenType::try_from(
                    iter.position(|x| !(*x))
                        .unwrap_or(self.locs.len() - usize::from(search_start_index))
                        + usize::from(search_start_index),
                )
                .unwrap();

                if new_free_index as usize == self.locs.len() {
                    debug_assert!(self.locs.iter().all(|x| *x));
                } else {
                    debug_assert!(self.query_item(new_free_index).is_none());
                    for i in 0..new_free_index {
                        debug_assert!(self.query_item(i).is_some());
                    }
                }

                new_free_index
            },
        };

        if (new_free_index as usize) < self.locs.len() {
            self.first_free_index = FreeIndex::FreeIndex(new_free_index);
        } else {
            self.first_free_index = FreeIndex::OldFreeIndex(new_free_index - 1);
        }
        debug_assert!(
            (new_free_index as usize) == self.locs.len()
                || self.query_item(new_free_index).is_none(),
            "Free index not free {self:?}"
        );

        ((new_free_index as usize) < self.locs.len()).then_some(new_free_index)
    }

    fn items_mut(
        locs: &mut [bool],
        zero_index: BeltLenType,
    ) -> impl DoubleEndedIterator<Item = &mut bool> {
        // TODO: I have another implementation of this:
        // TODO: Check which is faster (or simpler)
        // let mut iter = self
        //     .locs
        //     .iter()
        //     .skip(self.zero_index)
        //     .chain(self.locs.iter().take(self.zero_index));
        let len = locs.len();
        let (start, end) = locs.split_at_mut(usize::from(zero_index) % locs.len());

        debug_assert_eq!(end.iter().chain(start.iter()).count(), len);
        end.iter_mut().chain(start.iter_mut())
    }

    // TODO: When joining belts, we need to update the positions (and ids) of all attached BeltBeltInserters.
    #[must_use]
    /// # Panics
    /// If one of the belts is longer than `u16::MAX`
    pub fn join(front: Self, back: Self) -> Self {
        assert_eq!(
            front.item, back.item,
            "Tried to merge smart belts with different items."
        );

        // TODO: currently we always join the belt, this can potentially result in lag spikes when attaching new pieces of belt to a
        // very long belt, because of having to copy a lot of data around. This could be solved by implementing a maximum length of belt,
        // and attaching two belts back to back when we exceed this length. This does require a lot of testing to make sure this break is
        // invisible though, so I will leave this for now and test if it is actually a problem.

        let front_len = front.get_len() as usize;
        let _back_len = back.get_len() as usize;

        assert!(!front.is_circular);
        assert!(!back.is_circular);

        // TODO: Check if maybe choosing the shorter belt somewhere could make this faster?
        // My guess is that splicing the one with the shorter tail (see Vec::splice) will be best

        let Self {
            ty: ty_front,

            is_circular: _,
            first_free_index: front_first_free_index,
            zero_index: front_zero_index,
            locs: front_locs,
            inserters: front_inserters,
            item: _,

            last_moving_spot: last_moving_spot_front,

            input_splitter: None,
            output_splitter,
        } = front
        else {
            unreachable!()
        };
        // Important, first_free_index must ALWAYS be used using mod len
        let front_zero_index = usize::from(front_zero_index) % front_locs.len();

        let Self {
            ty: ty_back,

            is_circular: _,
            first_free_index: _back_first_free_index,
            zero_index: back_zero_index,
            locs: back_locs,
            inserters: mut back_inserters,
            item,

            last_moving_spot: _,

            input_splitter,
            output_splitter: None,
        } = back
        else {
            unreachable!()
        };

        // HUGE FIXME FIXME FIXME
        //assert_eq!(
        //    ty_front, ty_back,
        //    "Belts can only merge if they are the same type!"
        //);

        // Important, first_free_index must ALWAYS be used using mod len
        let back_zero_index = usize::from(back_zero_index) % back_locs.len();

        let num_front_inserters = front_inserters.offsets.len();
        let _num_back_inserters = back_inserters.offsets.len();

        let free_spots_before_last_inserter_front: u16 = front_inserters.offsets.iter().sum();
        let length_after_last_inserter = TryInto::<u16>::try_into(front_len)
            .expect("Belt should be max u16::MAX long")
            - free_spots_before_last_inserter_front
            - TryInto::<u16>::try_into(num_front_inserters)
                .expect("Belt should be max u16::MAX long");

        if let Some(offs) = back_inserters.offsets.get_mut(0) {
            *offs += length_after_last_inserter;
        }

        let mut new_inserters = front_inserters;
        take_mut::take(&mut new_inserters.inserters, |ins| {
            let mut ins = ins.into_vec();
            let mut other = vec![].into_boxed_slice();
            mem::swap(&mut other, &mut back_inserters.inserters);
            ins.extend(other.into_vec().drain(..));
            ins.into_boxed_slice()
        });
        take_mut::take(&mut new_inserters.offsets, |offs| {
            let mut offs = offs.into_vec();
            let mut other = vec![].into_boxed_slice();
            mem::swap(&mut other, &mut back_inserters.offsets);
            offs.extend(other.into_vec().drain(..));
            offs.into_boxed_slice()
        });

        let mut front_locs_vec = Vec::from(front_locs);

        front_locs_vec.rotate_left(front_zero_index);

        let back_loc_iter = back_locs
            .iter()
            .skip(back_zero_index)
            .chain(back_locs.iter().take(back_zero_index));

        front_locs_vec.extend(back_loc_iter.copied());

        Self {
            ty: ty_front,

            is_circular: false,
            first_free_index: FreeIndex::OldFreeIndex(0),
            zero_index: 0,
            locs: front_locs_vec.into_boxed_slice(),
            inserters: new_inserters,
            item,

            last_moving_spot: last_moving_spot_front,

            input_splitter,
            output_splitter,
        }
    }

    #[must_use]
    /// Side is on which side the empty belt is attached
    pub fn join_with_empty(self, empty: EmptyBelt, side: Side) -> Self {
        assert!(!self.is_circular);

        let len = self.get_len();

        let Self {
            ty,

            is_circular: _,
            first_free_index,
            zero_index,
            locs,
            mut inserters,
            item,

            last_moving_spot,

            input_splitter,
            output_splitter,
        } = self;

        match side {
            Side::FRONT => assert!(output_splitter.is_none()),
            Side::BACK => assert!(input_splitter.is_none()),
        }

        assert_eq!(ty, empty.ty);

        // Important, first_free_index must ALWAYS be used using mod len
        let zero_index = zero_index % len;

        let mut locs = locs.into_vec();

        let old_len = locs.len();

        let (new_empty, new_zero, front_extension_amount) = match side {
            Side::FRONT => {
                locs.splice(
                    usize::from(zero_index)..usize::from(zero_index),
                    repeat(false).take(empty.len.into()),
                );
                (FreeIndex::FreeIndex(0), zero_index, empty.len)
            },
            Side::BACK => {
                locs.splice(
                    ((usize::from(zero_index) + (old_len - 1)) % old_len)
                        ..((usize::from(zero_index) + (old_len - 1)) % old_len),
                    repeat(false).take(empty.len.into()),
                );
                (
                    match first_free_index {
                        FreeIndex::FreeIndex(idx) => FreeIndex::FreeIndex(idx),
                        FreeIndex::OldFreeIndex(idx) => FreeIndex::OldFreeIndex(idx),
                    },
                    zero_index + empty.len,
                    0,
                )
            },
        };

        if side == Side::FRONT {
            inserters.offsets[0] = inserters.offsets[0]
                .checked_add(front_extension_amount)
                .expect("Max length of belt (u16::MAX) reached");
        }

        let mut new = Self {
            ty,

            is_circular: false,
            first_free_index: new_empty,
            zero_index: new_zero,
            locs: locs.into_boxed_slice(),
            inserters,
            item,

            last_moving_spot: match side {
                Side::FRONT => 0,
                Side::BACK => last_moving_spot,
            },

            input_splitter,
            output_splitter,
        };

        new.find_and_update_real_first_free_index();

        new
    }

    pub fn break_belt_at(&mut self, belt_pos_to_break_at: u16) -> Option<Self> {
        // TODO: Is this correct
        if self.is_circular {
            assert!(self.input_splitter.is_none());
            assert!(self.output_splitter.is_none());
            self.is_circular = false;
            self.first_free_index = FreeIndex::OldFreeIndex(0);
            self.zero_index = belt_pos_to_break_at;
            return None;
        }

        let mut new_locs = None;
        take_mut::take(&mut self.locs, |locs| {
            let mut locs_vec = locs.into_vec();

            let len = locs_vec.len();

            locs_vec.rotate_left(usize::from(self.zero_index) % len);

            new_locs = Some(
                locs_vec
                    .split_off(belt_pos_to_break_at.into())
                    .into_boxed_slice(),
            );

            locs_vec.into_boxed_slice()
        });

        self.zero_index = 0;
        self.first_free_index = FreeIndex::OldFreeIndex(0);

        let new_locs = new_locs.unwrap();

        let mut offsets = self.inserters.offsets.iter().copied().enumerate();

        let mut current_pos = 0;

        let (split_at_inserters, new_offs) = loop {
            let Some((i, next_offset)) = offsets.next() else {
                break (self.inserters.offsets.len(), 0);
            };

            current_pos += next_offset;

            if current_pos >= belt_pos_to_break_at {
                break (i, current_pos - belt_pos_to_break_at);
            }
        };

        let mut new_inserters = None;
        take_mut::take(&mut self.inserters.inserters, |ins| {
            let mut ins = ins.into_vec();
            new_inserters = Some(ins.split_off(split_at_inserters).into_boxed_slice());
            ins.into_boxed_slice()
        });
        let new_inserters = new_inserters.unwrap();
        let mut new_offsets = None;
        take_mut::take(&mut self.inserters.offsets, |offs| {
            let mut offs = offs.into_vec();
            new_offsets = Some(offs.split_off(split_at_inserters).into_boxed_slice());
            offs.into_boxed_slice()
        });
        let mut new_offsets = new_offsets.unwrap();

        if let Some(offs) = new_offsets.get_mut(0) {
            // TODO: Make sure this is correct!
            *offs = new_offs;
        }

        // Since we split off the back portion, it will own our input splitter if we have one
        let input_splitter = self.input_splitter.take();

        let new_belt = Self {
            ty: self.ty,

            is_circular: false,
            first_free_index: FreeIndex::OldFreeIndex(0),
            zero_index: 0,
            locs: new_locs,
            inserters: InserterStore {
                inserters: new_inserters,
                offsets: new_offsets,
            },
            item: self.item,

            last_moving_spot: self.last_moving_spot.saturating_sub(belt_pos_to_break_at),

            input_splitter,
            output_splitter: None,
        };

        Some(new_belt)
    }
}

impl EmptyBelt {
    #[must_use]
    pub const fn new(ty: u8, len: u16) -> Self {
        Self {
            ty,

            is_circular: false,
            len,
        }
    }

    #[must_use]
    #[allow(clippy::needless_pass_by_value)]
    pub fn join(front: Self, back: Self) -> Self {
        assert!(!front.is_circular);
        assert!(!back.is_circular);

        assert_eq!(front.ty, back.ty);

        Self {
            ty: front.ty,

            is_circular: false,
            len: front.len + back.len,
        }
    }

    pub fn into_smart_belt<ItemIdxType: IdxTrait>(
        self,
        item: Item<ItemIdxType>,
    ) -> SmartBelt<ItemIdxType> {
        SmartBelt::new(self.ty, self.len.into(), item)
    }

    pub fn add_length(&mut self, amount: u16, _side: Side) -> u16 {
        self.len += amount;
        self.len
    }

    pub fn break_belt_at(&mut self, pos_to_break_at: u16) -> Self {
        if self.is_circular {
            todo!("Handle breaking circular belts")
        }

        let old_len = self.len;
        self.len = pos_to_break_at;
        Self {
            ty: self.ty,

            is_circular: false,
            len: old_len - pos_to_break_at,
        }
    }

    pub fn make_circular(&mut self) {
        todo!()
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Side {
    FRONT,
    BACK,
}

impl<ItemIdxType: IdxTrait> Belt<ItemIdxType> for SmartBelt<ItemIdxType> {
    fn add_length(&mut self, amount: BeltLenType, side: Side) -> BeltLenType {
        assert!(!self.is_circular);
        let ty = self.ty;
        take_mut::take(self, |s| {
            s.join_with_empty(EmptyBelt::new(ty, amount), side)
        });
        self.get_len()
    }

    fn try_insert_item(
        &mut self,
        pos: BeltLenType,
        item: Item<ItemIdxType>,
    ) -> Result<(), super::belt::NoSpaceError> {
        debug_assert_eq!(
            self.item, item,
            "Tried to insert wrong item onto SmartBelt, resulting in item transmutation"
        );

        if Belt::<ItemIdxType>::query_item(self, pos).is_none() {
            self.locs[self.into_loc_index(pos)] = true;

            // TODO: Check that the compiler realizes these are the same
            // Update first_free_index to show that it is old
            match self.first_free_index {
                FreeIndex::OldFreeIndex(free_index) | FreeIndex::FreeIndex(free_index) => {
                    if free_index == pos {
                        self.first_free_index = FreeIndex::OldFreeIndex(free_index);
                    }
                },
            }

            Ok(())
        } else {
            Err(NoSpaceError)
        }
    }

    #[allow(clippy::bool_assert_comparison)]
    fn update(&mut self, splitter_list: &[SushiSplitter<ItemIdxType>]) {
        if self.is_circular {
            // Correctness: Since we always % len henever we access using self.zero_index, we do not need to % len here
            // TODO: This could overflow after usize::MAX ticks which is 9749040289 Years. Should be fine!
            self.zero_index += 1;
            return;
        }

        if let Some((output_id, side)) = &self.output_splitter {
            if let Some(item) = self.query_item(0) {
                let splitter_loc =
                    &splitter_list[output_id.index as usize].inputs[usize::from(bool::from(*side))];

                // SAFETY:
                // This is the only place where we modify splitter_list from a &.
                // This can never race since only one belt ever has the same values for output_id and side, so only a single belt will ever modify each splitter loc
                let splitter_loc = unsafe { &mut *splitter_loc.get() };

                if splitter_loc.is_none() {
                    *splitter_loc = Some(item);
                    let _ = self.remove_item(0);
                }
            }
        }

        if let Some((input_id, side)) = &self.input_splitter {
            // Last pos
            if self.query_item(self.get_len() - 1).is_none() {
                let splitter_loc =
                    &splitter_list[input_id.index as usize].outputs[usize::from(bool::from(*side))];

                // SAFETY:
                // This is the only place where we modify splitter_list from a &.
                // This can never race since only one belt ever has the same values for output_id and side, so only a single belt will ever modify each splitter loc
                let splitter_loc = unsafe { &mut *splitter_loc.get() };

                if let Some(item) = *splitter_loc {
                    *splitter_loc = None;
                    let _ = self
                        .try_insert_item(self.get_len() - 1, item)
                        .expect("Should never fail!");
                }
            }
        }

        match self.first_free_index {
            FreeIndex::FreeIndex(idx) | FreeIndex::OldFreeIndex(idx) => {
                debug_assert!(idx <= self.get_len());
            },
        }
        if Belt::<ItemIdxType>::query_item(self, 0).is_none() {
            // Correctness: Since we always % len whenever we access using self.zero_index, we do not need to % len here
            // TODO: This could overflow after usize::MAX ticks which is 9749040289 Years. Should be fine!
            self.zero_index %= self.get_len();
            self.zero_index += 1;
            self.last_moving_spot = 0;
            match self.first_free_index {
                FreeIndex::FreeIndex(0) | FreeIndex::OldFreeIndex(0) => {
                    if Belt::<ItemIdxType>::query_item(self, 0).is_none() {
                        self.first_free_index = FreeIndex::FreeIndex(0);
                    } else {
                        self.first_free_index = FreeIndex::OldFreeIndex(0);
                    }
                },
                FreeIndex::FreeIndex(_) => {
                    unreachable!(
                        "FreeIndex should always point at the earliest known empty spot and we know that index 0 WAS an empty spot"
                    )
                },
                FreeIndex::OldFreeIndex(_) => {
                    unreachable!(
                        "OldFreeIndex should always point at the earliest potential empty spot and we know that index 0 WAS an empty spot"
                    )
                },
            }
            return;
        }

        self.zero_index %= self.get_len();

        let first_free_index_real = self.find_and_update_real_first_free_index();

        let len = self.get_len();

        self.last_moving_spot = first_free_index_real.unwrap_or(len);

        let Some(first_free_index_real) = first_free_index_real else {
            // All slots are full
            return;
        };

        assert!(
            first_free_index_real < len,
            "first_free_index_real < len: {self:?}"
        );
        assert!(
            first_free_index_real > 0,
            "first_free_index_real > 0: {self:?}"
        );

        let slice = &mut self.locs;

        let (end_slice, start_slice) = slice.split_at_mut(usize::from(self.zero_index));

        if self.zero_index + first_free_index_real >= len {
            // We have two stuck and one moving slice
            let (middle_stuck_slice, moving_slice) = end_slice
                .split_at_mut(usize::from((self.zero_index + first_free_index_real) % len));

            // By definition, we know starting_stuck_slice is fully compressed with items!
            // Also by definition we know that moving_slice[0] MUST be empty (otherwise it would not be moving and part of the stuck slice)

            // We already know the stuck slice is not empty, since otherwise we would just take the fast path above (unlikely if this is necessary)
            if !moving_slice.is_empty() {
                // Move the stuck slice back one step
                debug_assert_eq!(start_slice[0], true);
                start_slice[0] = false;

                debug_assert_eq!(moving_slice[0], false);
                moving_slice[0] = true;

                self.zero_index += 1;
            }
        } else {
            let (starting_stuck_slice, middle_moving_slice) =
                start_slice.split_at_mut(usize::from(first_free_index_real));

            assert!(!middle_moving_slice.is_empty());
            assert!(!starting_stuck_slice.is_empty());

            // By definition, we know starting_stuck_slice is fully compressed with items!
            // Also by definition we know that moving_slice[0] MUST be empty (otherwise it would not be moving and part of the stuck slice)

            // We already know the stuck slice is not empty, since otherwise we would just take the fast path above (unlikely if this is necessary)
            if !middle_moving_slice.is_empty() {
                // Move the stuck slice back one step
                debug_assert_eq!(starting_stuck_slice[0], true);
                starting_stuck_slice[0] = false;

                debug_assert_eq!(middle_moving_slice[0], false);
                middle_moving_slice[0] = true;

                self.zero_index += 1;
            }
        }

        // Instead of finding the real first_free_index after the update, we just use OldFreeIndex since most likely an inserter
        // Will update it for us before the next update
        self.first_free_index = FreeIndex::OldFreeIndex(first_free_index_real);
    }

    fn get_len(&self) -> BeltLenType {
        self.locs.len().try_into().unwrap()
    }

    fn query_item(&self, pos: BeltLenType) -> Option<Item<ItemIdxType>> {
        if self.locs[self.into_loc_index(pos)] {
            Some(self.item)
        } else {
            None
        }
    }

    fn remove_item(&mut self, pos: BeltLenType) -> Option<Item<ItemIdxType>> {
        if self.locs[self.into_loc_index(pos)] {
            self.locs[self.into_loc_index(pos)] = false;
            self.update_first_free_pos(pos);
            Some(self.item)
        } else {
            None
        }
    }

    fn items(&self) -> Vec<Option<Item<ItemIdxType>>> {
        let (start, end) = self
            .locs
            .split_at(usize::from(self.zero_index % self.get_len()));

        debug_assert_eq!(
            end.iter().chain(start.iter()).count(),
            usize::from(self.get_len())
        );
        end.iter()
            .chain(start.iter())
            .map(|loc| if *loc { Some(self.item) } else { None })
            .collect()
    }

    fn item_hint(&self) -> Option<Vec<Item<ItemIdxType>>> {
        Some(vec![self.item])
    }
}
