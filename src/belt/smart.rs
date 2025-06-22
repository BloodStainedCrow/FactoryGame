use std::iter::repeat;

use log::info;

use crate::{
    inserter::{belt_storage_inserter::BeltStorageInserter, InserterState, MOVETIME},
    item::{IdxTrait, Item, WeakIdxTrait},
    storage_list::SingleItemStorages,
};

use crate::inserter::FakeUnionStorage;

use super::{
    belt::{Belt, BeltLenType, ItemInfo, NoSpaceError},
    sushi::{SushiBelt, SushiInserterStore},
    FreeIndex, Inserter,
};

#[allow(clippy::module_name_repetitions)]
#[derive(Debug, Clone, serde::Deserialize, serde::Serialize)]
pub struct SmartBelt<ItemIdxType: WeakIdxTrait> {
    pub(super) is_circular: bool,
    pub(super) first_free_index: FreeIndex,
    /// Important, zero_index must ALWAYS be used using mod len
    pub(super) zero_index: BeltLenType,
    pub(super) locs: Box<[bool]>,
    pub(super) inserters: InserterStore,

    pub(super) item: Item<ItemIdxType>,
}

#[derive(Debug, Clone, serde::Deserialize, serde::Serialize, Default)]
pub struct EmptyBelt {
    is_circular: bool,
    pub len: u16,
}

#[derive(Debug, Clone, serde::Deserialize, serde::Serialize)]
pub struct InserterStore {
    pub(super) inserters: Vec<Inserter>,
    pub(super) offsets: Vec<u16>,
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
    pub fn new(len: u16, item: Item<ItemIdxType>) -> Self {
        Self {
            is_circular: false,
            first_free_index: FreeIndex::FreeIndex(0),
            zero_index: 0,
            locs: vec![false; len.into()].into_boxed_slice(),
            inserters: InserterStore {
                inserters: vec![],
                offsets: vec![],
            },

            item,
        }
    }

    fn into_loc_index(&self, pos: BeltLenType) -> usize {
        usize::from((self.zero_index + pos) % self.get_len())
    }

    pub(super) fn into_sushi_belt(self) -> SushiBelt<ItemIdxType> {
        let Self {
            is_circular,
            first_free_index,
            zero_index,
            locs,
            inserters: InserterStore { inserters, offsets },
            item,
        } = self;

        SushiBelt {
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
        }
    }

    pub fn make_circular(&mut self) {
        self.is_circular = true;
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

        dbg!(&new);

        if new.grid_or_static_flag == 2 {
            dbg!(&new);
        }

        if belt_pos == 143 {
            dbg!(&new);
        }

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
                unreachable!()
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
                std::cmp::Ordering::Greater => panic!("The belt did not have an inserter at position specified to remove inserter from"), // This is the index to insert at
                std::cmp::Ordering::Equal => break,

                std::cmp::Ordering::Less => {
                    pos_after_last_inserter = next_inserter_pos + 1;
                    i += 1;
                },
            }
        }

        let old_inserter = self.inserters.inserters.remove(i);
        let removed = self.inserters.offsets.remove(i);
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

        if index == 143 {
            dbg!(&storage_id);
        }

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
        self.inserters.offsets.insert(i, new_inserter_offset);
        self.inserters
            .inserters
            .insert(i, Inserter::Out(BeltStorageInserter::new(storage_id)));

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
        self.inserters.offsets.insert(i, new_inserter_offset);
        self.inserters
            .inserters
            .insert(i, Inserter::In(BeltStorageInserter::new(storage_id)));

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

    pub fn update_inserters<'a, 'b>(
        &mut self,
        storages: SingleItemStorages<'a, 'b>,
        num_grids_total: usize,
        num_recipes: usize,
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
                        Inserter::Out(inserter) => inserter.update(
                            loc,
                            storages,
                            MOVETIME,
                            num_grids_total,
                            num_recipes,
                            grid_size,
                        ),
                        Inserter::In(inserter) => inserter.update(
                            loc,
                            storages,
                            MOVETIME,
                            num_grids_total,
                            num_recipes,
                            grid_size,
                        ),
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
                info!("HAD TO SEARCH FOR FIRST FREE INDEX!");

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
            is_circular: _,
            first_free_index: front_first_free_index,
            zero_index: front_zero_index,
            locs: front_locs,
            inserters: front_inserters,
            item,
        } = front;
        // Important, first_free_index must ALWAYS be used using mod len
        let front_zero_index = usize::from(front_zero_index) % front_locs.len();

        let Self {
            is_circular: _,
            first_free_index: _back_first_free_index,
            zero_index: back_zero_index,
            locs: back_locs,
            inserters: mut back_inserters,
            item,
        } = back;
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
        new_inserters
            .inserters
            .append(&mut back_inserters.inserters);
        new_inserters.offsets.append(&mut back_inserters.offsets);

        let mut front_locs_vec = Vec::from(front_locs);

        front_locs_vec.rotate_left(front_zero_index);

        let back_loc_iter = back_locs
            .iter()
            .skip(back_zero_index)
            .chain(back_locs.iter().take(back_zero_index));

        front_locs_vec.extend(back_loc_iter.copied());

        Self {
            is_circular: false,
            first_free_index: FreeIndex::OldFreeIndex(0),
            zero_index: 0,
            locs: front_locs_vec.into_boxed_slice(),
            inserters: new_inserters,
            item,
        }
    }

    #[must_use]
    /// Side is on which side the empty belt is attached
    pub fn join_with_empty(self, empty: EmptyBelt, side: Side) -> Self {
        assert!(!self.is_circular);

        let len = self.get_len();

        let Self {
            is_circular: _,
            first_free_index,
            zero_index,
            locs,
            mut inserters,
            item,
        } = self;

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
            is_circular: false,
            first_free_index: new_empty,
            zero_index: new_zero,
            locs: locs.into_boxed_slice(),
            inserters,
            item,
        };

        new.find_and_update_real_first_free_index();

        new
    }

    pub fn break_belt_at(&mut self, belt_pos_to_break_at: u16) -> Option<Self> {
        // TODO: Is this correct
        if self.is_circular {
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

        let new_inserters = self.inserters.inserters.split_off(split_at_inserters);
        let mut new_offsets = self.inserters.offsets.split_off(split_at_inserters);

        if let Some(offs) = new_offsets.get_mut(0) {
            // TODO: Make sure this is correct!
            *offs = new_offs;
        }

        let new_belt = Self {
            is_circular: false,
            first_free_index: FreeIndex::OldFreeIndex(0),
            zero_index: 0,
            locs: new_locs,
            inserters: InserterStore {
                inserters: new_inserters,
                offsets: new_offsets,
            },
            item: self.item,
        };

        Some(new_belt)
    }
}

impl EmptyBelt {
    #[must_use]
    pub const fn new(len: u16) -> Self {
        Self {
            is_circular: false,
            len,
        }
    }

    #[must_use]
    #[allow(clippy::needless_pass_by_value)]
    pub const fn join(front: Self, back: Self) -> Self {
        assert!(!front.is_circular);
        assert!(!back.is_circular);

        Self {
            is_circular: false,
            len: front.len + back.len,
        }
    }

    pub fn into_smart_belt<ItemIdxType: IdxTrait>(
        self,
        item: Item<ItemIdxType>,
    ) -> SmartBelt<ItemIdxType> {
        SmartBelt::new(self.len.into(), item)
    }

    pub fn add_length(&mut self, amount: u16, side: Side) -> u16 {
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
        take_mut::take(self, |s| s.join_with_empty(EmptyBelt::new(amount), side));
        self.get_len()
    }

    fn try_insert_item(
        &mut self,
        pos: BeltLenType,
        item: Item<ItemIdxType>,
    ) -> Result<(), super::belt::NoSpaceError> {
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
    fn update(&mut self) {
        if self.is_circular {
            // Correctness: Since we always % len whenever we access using self.zero_index, we do not need to % len here
            // TODO: This could overflow after usize::MAX ticks which is 9749040289 Years. Should be fine!
            self.zero_index += 1;
            return;
        }

        match self.first_free_index {
            FreeIndex::FreeIndex(idx) | FreeIndex::OldFreeIndex(idx) => {
                debug_assert!(idx <= self.get_len());
            },
        }
        if Belt::<ItemIdxType>::query_item(self, 0).is_none() {
            // Correctness: Since we always % len whenever we access using self.zero_index, we do not need to % len here
            // TODO: This could overflow after usize::MAX ticks which is 9749040289 Years. Should be fine!
            self.zero_index += 1;
            match self.first_free_index {
                FreeIndex::FreeIndex(0) | FreeIndex::OldFreeIndex(0) => {
                    if Belt::<ItemIdxType>::query_item(self, 0).is_none() {
                        self.first_free_index = FreeIndex::FreeIndex(0);
                    } else {
                        self.first_free_index = FreeIndex::OldFreeIndex(0);
                    }
                },
                FreeIndex::FreeIndex(_) => {
                    unreachable!("FreeIndex should always point at the earliest known empty spot and we know that index 0 WAS an empty spot")
                },
                FreeIndex::OldFreeIndex(_) => {
                    unreachable!("OldFreeIndex should always point at the earliest potential empty spot and we know that index 0 WAS an empty spot")
                },
            }
            return;
        }

        self.zero_index %= self.get_len();

        let first_free_index_real = self.find_and_update_real_first_free_index();

        let len = self.get_len();

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

    fn query_item(&self, pos: BeltLenType) -> Option<ItemInfo<ItemIdxType>> {
        if self.locs[self.into_loc_index(pos)] {
            Some(ItemInfo::Implicit)
        } else {
            None
        }
    }

    fn remove_item(&mut self, pos: BeltLenType) -> Option<ItemInfo<ItemIdxType>> {
        if self.locs[self.into_loc_index(pos)] {
            self.locs[self.into_loc_index(pos)] = false;
            Some(ItemInfo::Implicit)
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

// TODO
#[cfg(todotest)]
mod tests {

    extern crate test;

    use std::cmp::min;

    use proptest::{prelude::prop, prop_assert, prop_assert_eq, proptest};
    use rand::random;
    use test::Bencher;

    use crate::{belt::do_update_test_bools, inserter::StorageID, item::ITEMCOUNTTYPE};

    use super::*;

    const MAX_LEN: u16 = 50_000;
    proptest! {

        #[test]
        fn test_belt_moves_item_forward(item_pos in 0..MAX_LEN) {
            let mut belt = SmartBelt::<u8>::new(MAX_LEN);

            let ret = belt.try_insert_item(item_pos);

            // Since the whole belt is empty, it should not fail to put an item in
            assert!(ret.is_ok());

            belt.update();

            if item_pos > 0 {
                // The item should have moved
                for i in 0..MAX_LEN {
                    if i == item_pos - 1 {
                        prop_assert_eq!(belt.query_item(i), Some(()));
                    } else {
                        prop_assert_eq!(belt.query_item(i), None);
                    }
                }
            } else {
                // The item should NOT have moved
                for i in 0..MAX_LEN {
                    if i == item_pos {
                        prop_assert_eq!(belt.query_item(i), Some(()));
                    } else {
                        prop_assert_eq!(belt.query_item(i), None);
                    }
                }
            }
        }

        #[test]
        fn test_smart_belt_agrees_with_functional(mut items in prop::collection::vec(prop::bool::ANY, 1..100)) {
            let mut belt = SmartBelt::<u8>::new(items.len().try_into().unwrap());

            for (i, item_opt) in items.iter().enumerate() {

                if *item_opt {
                    belt.try_insert_item(i.try_into().unwrap()).expect("Since the belt starts empty this should never fail");
                } else {
                    assert!(belt.remove_item(i).is_none());
                }
            }

            for _update_count in 0..items.len() * 2 {
                belt.update();

                do_update_test_bools(&mut items);

                for (i, should_be_filled) in items.iter().enumerate() {
                    let correct = if *should_be_filled {
                        Some(())
                    } else {
                        None
                    };
                    prop_assert_eq!(belt.query_item(i.try_into().unwrap()), correct);
                }
            }
        }

        #[test]
        fn test_join_belt_length(front_len in 0..MAX_LEN, back_len in 0..MAX_LEN) {
            let back = SmartBelt::<u8>::new(back_len);
            let front = SmartBelt::new(front_len);

            prop_assert_eq!(SmartBelt::join(front, back).get_len(), front_len + back_len);
        }

        #[test]
        fn test_join_belt_items(front_items in prop::collection::vec(prop::bool::ANY, 1..100), back_items in prop::collection::vec(prop::bool::ANY, 1..100)) {
            let mut back = SmartBelt::<u8>::new(back_items.len().try_into().unwrap());
            let mut front = SmartBelt::new(front_items.len().try_into().unwrap());

            for (i, item) in front_items.iter().enumerate() {
                if *item {
                    assert!(front.try_insert_item(i.try_into().unwrap()).is_ok());
                }
            }

            for (i, item) in back_items.iter().enumerate() {
                if *item {
                    assert!(back.try_insert_item(i.try_into().unwrap()).is_ok());
                }
            }

            let new_belt = SmartBelt::join(front, back);

            for (i, item) in front_items.iter().chain(back_items.iter()).enumerate() {
                prop_assert_eq!(new_belt.query_item(i.try_into().unwrap()).is_some(), *item, "{:?}", new_belt);
            }
        }

        #[test]
        fn test_join_belt_first_free_index(front_items in prop::collection::vec(prop::bool::ANY, 1..100), back_items in prop::collection::vec(prop::bool::ANY, 1..100)) {
            let mut back = SmartBelt::<u8>::new(back_items.len().try_into().unwrap());
            let mut front = SmartBelt::new(front_items.len().try_into().unwrap());

            for (i, item) in front_items.iter().enumerate() {
                if *item {
                    assert!(front.try_insert_item(i.try_into().unwrap()).is_ok());
                }
            }

            for (i, item) in back_items.iter().enumerate() {
                if *item {
                    assert!(back.try_insert_item(i.try_into().unwrap()).is_ok());
                }
            }

            let new_belt = SmartBelt::join(front, back);

            let Some((index, _)) = front_items.iter().chain(back_items.iter()).enumerate().find(|(_i, item)| !**item) else {
                return Ok(());
            };

            match new_belt.first_free_index {
                FreeIndex::FreeIndex(join_index) => prop_assert_eq!(join_index, index),
                FreeIndex::OldFreeIndex(join_index) => prop_assert!(join_index <= index),
            }
        }

        // #[test]
        // fn test_join_belt_inserters(front_inserters in prop::collection::vec(prop::bool::ANY, 1..100), back_inserters in prop::collection::vec(prop::bool::ANY, 1..100)) {
        //     let mut back = SmartBelt::new(back_inserters.len());
        //     let mut front = SmartBelt::new(front_inserters.len());

        //     for (i, inserter) in front_inserters.iter().enumerate() {
        //         if *inserter {
        //             assert!(front.add_out_inserter(i.try_into().expect("Hardcoded"), StorageID { recipe: 0, grid: 0, storage: i.try_into().expect("Hardcoded") }).is_ok());
        //         }
        //     }

        //     for (i, inserter) in back_inserters.iter().enumerate() {
        //         if *inserter {
        //             dbg!(i);
        //             assert!(back.add_out_inserter(i.try_into().expect("Hardcoded"), StorageID { recipe: 0, grid: 0, storage: i.try_into().expect("Hardcoded") }).is_ok());
        //         }
        //     }

        //     let new_belt = SmartBelt::join(front, back);

        //     for (i, (storage_i, inserter)) in front_inserters.iter().enumerate().chain(back_inserters.iter().enumerate()).enumerate() {
        //         let expected_id = StorageID { recipe: 0, grid: 0, storage: storage_i.try_into().expect("Hardcoded") };

        //         match new_belt.get_inserter(i.try_into().expect("Hardcoded")) {
        //             Some(Inserter::Out(ins)) => prop_assert_eq!(ins.storage_id.clone(), expected_id, "{:?}", new_belt),
        //             None => prop_assert!(!*inserter, "{:?}", new_belt),
        //             _ => prop_assert!(false, "Out inserter became In ?!?"),
        //         }
        //     }
        // }

        // #[test]
        // fn test_add_inserter(inserters in prop::collection::vec(prop::bool::ANY, 1..100)) {
        //     let mut belt = SmartBelt::new(inserters.len());

        //     for (i, inserter) in inserters.iter().enumerate() {
        //         if *inserter {
        //             let id = StorageID { recipe: 0, grid: 0, storage: i.try_into().expect("Hardcoded") };
        //             prop_assert!(belt.add_out_inserter(i.try_into().expect("Hardcoded"), id).is_ok());
        //         }
        //     }

        //     for (i, inserter) in inserters.iter().enumerate() {
        //         let expected_id = StorageID { recipe: 0, grid: 0, storage: i.try_into().expect("Hardcoded") };

        //         match belt.get_inserter(i.try_into().expect("Hardcoded")) {
        //             Some(Inserter::Out(ins)) => prop_assert_eq!(ins.storage_id.clone(), expected_id, "{:?}", belt),
        //             None => prop_assert!(!*inserter, "{:?}", belt),
        //             _ => prop_assert!(false, "Out inserter became In ?!?"),
        //         }
        //     }
        // }
    }

    #[test]
    fn test_smart_belt_does_not_set_free_index_wrong() {
        let mut belt = SmartBelt::new(MAX_LEN);

        // OutInserter::create_and_add(Arc::downgrade(&storage.storage), &mut belt, MAX_LEN - 1);

        belt.try_insert_item(0).expect("Expected insert to work");

        for _ in 0..1_000 {
            belt.update();
            belt.remove_item(1);
        }
    }

    #[test]
    fn test_smart_belt_with_inserters() {
        let mut belt = SmartBelt::new(10_000);

        let storage_unused = ITEMCOUNTTYPE::default();

        let storage_source = 30;
        let storage_dest = ITEMCOUNTTYPE::default();

        let id = StorageID {
            storage_list_idx: todo!(),
            machine_idx: todo!(),
            phantom: std::marker::PhantomData,
        };
        belt.add_out_inserter(5, id);

        let mut steel_producer_storages = [storage_unused, storage_source, storage_dest];

        for _ in 0..20 {
            belt.update();
            belt.update_inserters(&mut [&mut [&mut steel_producer_storages]]);
            // let _ = belt.remove_item(5);

            let _ = belt.try_insert_item(9);

            println!("{}", &belt as &dyn Belt);
            println!("{:?}", steel_producer_storages[2]);
        }
    }

    #[bench]
    fn bench_smart_belt_with_inserters(b: &mut Bencher) {
        let mut belt = SmartBelt::new(MAX_LEN);

        let storage_unused = ITEMCOUNTTYPE::default();

        let storage_source = 30;
        let storage_dest = ITEMCOUNTTYPE::default();

        let id = StorageID {
            recipe: 0,
            grid: 0,
            storage: 2,
        };
        belt.add_out_inserter(5, id);

        let mut steel_producer_storages = [storage_unused, storage_source, storage_dest];

        b.iter(|| {
            belt.update();
            belt.update_inserters(&mut [&mut [&mut steel_producer_storages]]);

            let _ = belt.try_insert_item(9);
        });
    }

    #[bench]
    fn bench_smart_belt_with_10000_inserters(b: &mut Bencher) {
        let mut belt = SmartBelt::new(MAX_LEN);

        let storage_unused = ITEMCOUNTTYPE::default();

        let storage_source = 30;
        let storage_dest = ITEMCOUNTTYPE::default();

        for i in 0..10_000 {
            let id = StorageID {
                recipe: 0,
                grid: 0,
                storage: random(),
            };
            belt.add_out_inserter(i, id);
        }

        let mut storages: Vec<ITEMCOUNTTYPE> =
            [storage_unused.clone(), storage_source, storage_dest].into();

        for _ in 0..100_000 {
            storages.push(storage_unused.clone());
        }

        b.iter(|| {
            belt.update();
            belt.update_inserters(&mut [&mut [&mut storages]]);

            let _ = belt.try_insert_item(9);
        });
    }

    #[bench]
    fn bench_smart_belt_with_10000_inserters_belt_empty(b: &mut Bencher) {
        let mut belt = SmartBelt::new(MAX_LEN);

        let storage_unused = ITEMCOUNTTYPE::default();

        let storage_source = 0;
        let storage_dest = ITEMCOUNTTYPE::default();

        for i in 0..10_000 {
            let id = StorageID {
                recipe: 0,
                grid: 0,
                storage: random(),
            };
            belt.add_out_inserter(i, id);
        }

        let mut storages: Vec<ITEMCOUNTTYPE> =
            [storage_unused.clone(), storage_source, storage_dest].into();

        for _ in 0..100_000 {
            storages.push(storage_unused.clone());
        }

        b.iter(|| {
            belt.update();
            belt.update_inserters(&mut [&mut [&mut storages]]);
        });
    }

    #[test]
    fn test_debug() {
        let mut belt = SmartBelt::new(MAX_LEN);

        for _ in 0..100_000 {
            let bb = test::black_box(&mut belt);
            bb.update();
        }
    }

    #[bench]
    fn bench_smart_belt_update_free_flowing(b: &mut Bencher) {
        let mut belt = SmartBelt::new(MAX_LEN);
        // belt.belt_storage
        //     .try_put_item_in_pos(Item::Iron, MAX_LEN - 1);

        b.iter(|| {
            let bb = test::black_box(&mut belt);
            bb.update();
        });

        // println!("{belt}");
    }

    #[bench]
    fn bench_smart_belt_update_stuck(b: &mut Bencher) {
        let mut belt = SmartBelt::new(MAX_LEN);
        belt.try_insert_item(MAX_LEN - 1)
            .expect("Expected insert to work");

        belt.try_insert_item(0).expect("Expected insert to work");

        b.iter(|| {
            let bb = test::black_box(&mut belt);
            bb.update();
        });

        // println!("{belt}");
    }

    #[bench]
    fn bench_smart_belt_update_full(b: &mut Bencher) {
        let mut belt = SmartBelt::new(MAX_LEN);

        for i in 0..MAX_LEN {
            assert!(belt.try_insert_item(i).is_ok());
        }

        b.iter(|| {
            let bb = test::black_box(&mut belt);
            bb.update();
        });

        // println!("{belt}");
    }

    #[bench]
    fn bench_smart_belt_worst_case(b: &mut Bencher) {
        let mut belt = SmartBelt::new(MAX_LEN);

        for i in 0..MAX_LEN / 2 {
            assert!(belt.try_insert_item(i).is_ok());
        }

        for _ in (MAX_LEN / 2)..MAX_LEN {
            // This spot is empty
        }

        // Insert a single item at the end
        assert!(belt.try_insert_item(MAX_LEN - 1).is_ok());

        b.iter(|| {
            let bb = test::black_box(&mut belt);
            bb.update();
        });

        // println!("{belt:?}");
        let mut num_items = 0;
        let mut last_spot_with_items: Option<usize> = None;
        for i in 0..MAX_LEN {
            if belt.query_item(i).is_some() {
                num_items += 1;
                last_spot_with_items = Some(i);
            }
        }
        println!("{num_items}");
        println!("{last_spot_with_items:?}");
    }

    // TODO: Check on these benchmarks, I saw the 1k belt one be slower than the 50k one?!?
    #[bench]
    fn bench_extend_belt_by_one_front_1_000(b: &mut Bencher) {
        let mut belt = Some(SmartBelt::new(1_000));

        b.iter(move || {
            let back = belt.take().expect("Hardcoded");
            let front = SmartBelt::new(1);

            belt = Some(SmartBelt::join(front, back));
        });
    }

    #[bench]
    fn bench_extend_belt_by_one_back_1_000(b: &mut Bencher) {
        let mut belt = Some(SmartBelt::new(1_000));

        b.iter(move || {
            let front = belt.take().expect("Hardcoded");
            let back = SmartBelt::new(1);

            belt = Some(SmartBelt::join(front, back));
        });
    }

    #[bench]
    fn bench_extend_belt_by_one_front_50_000(b: &mut Bencher) {
        let mut belt = Some(SmartBelt::new(50_000));

        b.iter(move || {
            let back = belt.take().expect("Hardcoded");
            let front = SmartBelt::new(1);

            belt = Some(SmartBelt::join(front, back));
        });
    }

    #[bench]
    fn bench_extend_belt_by_one_back_50_000(b: &mut Bencher) {
        let mut belt = Some(SmartBelt::new(50_000));

        b.iter(move || {
            let front = belt.take().expect("Hardcoded");
            let back = SmartBelt::new(1);

            belt = Some(SmartBelt::join(front, back));
        });
    }

    #[bench]
    fn bench_add_inserter(b: &mut Bencher) {
        let mut belt = SmartBelt::new(MAX_LEN);
        let mut i = 0;

        b.iter(|| {
            let _ = belt.add_in_inserter(
                min(
                    (MAX_LEN - 1).try_into().expect("MAX_LEN too large for u16"),
                    i,
                ),
                StorageID {
                    recipe: 0,
                    grid: 0,
                    storage: 1,
                },
            );
            i += 1;
        });
    }
}
