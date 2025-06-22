use std::{iter::repeat, mem};

use itertools::Itertools;

use crate::{
    belt::belt::NoSpaceError,
    inserter::belt_storage_inserter::BeltStorageInserter,
    item::{IdxTrait, Item, WeakIdxTrait},
};

use super::{
    belt::{Belt, BeltLenType},
    smart::{BeltInserterInfo, InserterStore, Side, SmartBelt, SpaceOccupiedError},
    FreeIndex, Inserter,
};
use crate::inserter::FakeUnionStorage;

#[derive(Debug, Clone, serde::Deserialize, serde::Serialize)]
pub(super) struct SushiBelt<ItemIdxType: WeakIdxTrait> {
    pub(super) is_circular: bool,
    pub(super) locs: Box<[Option<Item<ItemIdxType>>]>,
    pub(super) first_free_index: FreeIndex,
    /// Important, zero_index must ALWAYS be used using mod len
    pub(super) zero_index: BeltLenType,
    pub(super) inserters: SushiInserterStore<ItemIdxType>,
}

#[derive(Debug, Clone, serde::Deserialize, serde::Serialize)]
pub(super) struct SushiInserterStore<ItemIdxType: WeakIdxTrait> {
    pub(super) inserters: Vec<(Inserter, Item<ItemIdxType>)>,
    pub(super) offsets: Vec<u16>,
}

#[derive(Debug, PartialEq, Eq)]
pub(super) enum SushiInfo<ItemIdxType: WeakIdxTrait> {
    Sushi,
    Pure(Option<Item<ItemIdxType>>),
}

impl<ItemIdxType: IdxTrait> SushiBelt<ItemIdxType> {
    pub fn new(len: BeltLenType) -> Self {
        Self {
            is_circular: false,
            locs: vec![None; usize::from(len)].into_boxed_slice(),
            first_free_index: FreeIndex::FreeIndex(0),
            zero_index: 0,
            inserters: SushiInserterStore {
                inserters: vec![],
                offsets: vec![],
            },
        }
    }

    pub fn add_in_inserter(
        &mut self,
        filter: Item<ItemIdxType>,
        pos: BeltLenType,
        storage_id: FakeUnionStorage,
    ) -> Result<(), SpaceOccupiedError> {
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
                std::cmp::Ordering::Greater => break, // This is the index to insert at
                std::cmp::Ordering::Equal => return Err(SpaceOccupiedError),

                std::cmp::Ordering::Less => {
                    pos_after_last_inserter = next_inserter_pos + 1;
                    i += 1;
                },
            }
        }

        // Insert at i
        let new_inserter_offset = pos - pos_after_last_inserter;
        self.inserters.offsets.insert(i, new_inserter_offset);
        self.inserters.inserters.insert(
            i,
            (Inserter::In(BeltStorageInserter::new(storage_id)), filter),
        );

        let next = self.inserters.offsets.get_mut(i + 1);

        if let Some(next_offs) = next {
            *next_offs -= new_inserter_offset + 1;
        }

        Ok(())
    }

    pub fn add_out_inserter(
        &mut self,
        filter: Item<ItemIdxType>,
        pos: BeltLenType,
        storage_id: FakeUnionStorage,
    ) -> Result<(), SpaceOccupiedError> {
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
                std::cmp::Ordering::Greater => break, // This is the index to insert at
                std::cmp::Ordering::Equal => return Err(SpaceOccupiedError),

                std::cmp::Ordering::Less => {
                    pos_after_last_inserter = next_inserter_pos + 1;
                    i += 1;
                },
            }
        }

        // Insert at i
        let new_inserter_offset = pos - pos_after_last_inserter;
        self.inserters.offsets.insert(i, new_inserter_offset);
        self.inserters.inserters.insert(
            i,
            (Inserter::Out(BeltStorageInserter::new(storage_id)), filter),
        );

        let next = self.inserters.offsets.get_mut(i + 1);

        if let Some(next_offs) = next {
            *next_offs -= new_inserter_offset + 1;
        }

        Ok(())
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
                return Some(match &inserter.0 {
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

    pub fn get_inserter_item(&self, belt_pos: u16) -> Item<ItemIdxType> {
        assert!(
            usize::from(belt_pos) < self.locs.len(),
            "Bounds check {belt_pos} >= {}",
            self.locs.len()
        );

        let mut pos_after_last_inserter = 0;
        let mut i = 0;

        for offset in &self.inserters.offsets {
            let next_inserter_pos = pos_after_last_inserter + offset;

            match next_inserter_pos.cmp(&belt_pos) {
                std::cmp::Ordering::Greater => panic!("The belt did not have an inserter at position specified to remove inserter from"), // This is the index to insert at
                std::cmp::Ordering::Equal => break,

                std::cmp::Ordering::Less => {
                    pos_after_last_inserter = next_inserter_pos + 1;
                    i += 1;
                },
            }
        }

        self.inserters.inserters[i].1
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
                match &mut inserter.0 {
                    Inserter::Out(belt_storage_inserter) => {
                        belt_storage_inserter.storage_id = new;
                    },
                    Inserter::In(belt_storage_inserter) => {
                        belt_storage_inserter.storage_id = new;
                    },
                }
                return;
            } else if pos >= belt_pos {
                unreachable!()
            }
            pos += 1;
        }
    }

    pub fn remove_inserter(&mut self, pos: BeltLenType) {
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

        self.inserters.inserters.remove(i);
        let removed = self.inserters.offsets.remove(i);
        // The offset after i (which has now shifted left to i)
        self.inserters.offsets[i] += removed + 1;
    }

    pub fn check_sushi(
        &self,
        belt_belt_filter_in: impl IntoIterator<Item = SushiInfo<ItemIdxType>> + Clone,
        belt_belt_filter_out: impl IntoIterator<Item = SushiInfo<ItemIdxType>> + Clone,
    ) -> SushiInfo<ItemIdxType> {
        if belt_belt_filter_in
            .clone()
            .into_iter()
            .any(|info| info == SushiInfo::Sushi)
        {
            return SushiInfo::Sushi;
        }

        let item_from_inserters = self
            .inserters
            .inserters
            .iter()
            .map(|(_, item)| *item)
            .chain(belt_belt_filter_in.into_iter().filter_map(|info| {
                let SushiInfo::Pure(item) = info else {
                    unreachable!()
                };
                item
            }))
            .chain(
                belt_belt_filter_out
                    .into_iter()
                    .filter_map(|info| match info {
                        SushiInfo::Sushi => None,
                        SushiInfo::Pure(item) => item,
                    }),
            )
            .all_equal_value();

        let item_from_inserters = match item_from_inserters {
            Ok(item) => Some(item),
            Err(None) => None,
            Err(_) => return SushiInfo::Sushi,
        };

        let item_on_belt = self.locs.iter().flatten().copied().all_equal_value();

        let item_on_belt = match item_on_belt {
            Ok(item) => Some(item),
            Err(None) => None,
            Err(_) => return SushiInfo::Sushi,
        };

        match (item_from_inserters, item_on_belt) {
            (None, None) => SushiInfo::Pure(None),
            (None, Some(item)) => SushiInfo::Pure(Some(item)),
            (Some(item), None) => SushiInfo::Pure(Some(item)),
            (Some(item_a), Some(item_b)) => {
                if item_a == item_b {
                    SushiInfo::Pure(Some(item_a))
                } else {
                    SushiInfo::Sushi
                }
            },
        }
    }

    pub fn into_smart_belt(self, item: Item<ItemIdxType>) -> SmartBelt<ItemIdxType> {
        let found_item = match self.locs.iter().copied().flatten().all_equal_value() {
            Ok(found_item) => {
                assert_eq!(found_item, item);
                found_item
            },
            Err(None) => item,
            Err(Some(_)) => panic!("Belt is not pure!"),
        };

        let Self {
            is_circular,
            locs,
            first_free_index,
            zero_index,
            inserters: SushiInserterStore { inserters, offsets },
        } = self;

        SmartBelt {
            is_circular,
            first_free_index,
            zero_index,
            locs: locs
                .iter()
                .map(|loc| {
                    if let Some(found_item) = loc {
                        debug_assert_eq!(*found_item, item);
                    }
                    loc.is_some()
                })
                .collect(),
            inserters: InserterStore {
                // FIXME: Some of these inserters might have a different item than what we are converting to. This will result in crashes and item transmutation
                inserters: inserters.into_iter().map(|(ins, inserter_item)| {
                    assert_eq!(item, inserter_item, "FIXME: We need to handle inserters which will never work again in smart belts");    
                    ins
                }).collect(),
                offsets,
            },
            item,
        }
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

        let split_at_inserters = loop {
            let Some((i, next_offset)) = offsets.next() else {
                break self.inserters.offsets.len();
            };

            current_pos += next_offset;

            if current_pos >= belt_pos_to_break_at {
                break i;
            }
        };

        let new_inserters = self.inserters.inserters.split_off(split_at_inserters);
        let mut new_offsets = self.inserters.offsets.split_off(split_at_inserters);

        if let Some(offs) = new_offsets.get_mut(0) {
            *offs -= belt_pos_to_break_at
        }

        let new_belt = Self {
            is_circular: false,
            first_free_index: FreeIndex::OldFreeIndex(0),
            zero_index: 0,
            locs: new_locs,
            inserters: SushiInserterStore {
                inserters: new_inserters,
                offsets: new_offsets,
            },
        };

        Some(new_belt)
    }

    pub fn join(front: Self, back: Self) -> Self {
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
        } = front;

        // Important, first_free_index must ALWAYS be used using mod len
        let front_zero_index = usize::from(front_zero_index) % front_locs.len();

        let Self {
            is_circular: _,
            first_free_index: _back_first_free_index,
            zero_index: back_zero_index,
            locs: back_locs,
            inserters: mut back_inserters,
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

        let new_first_free_index = front_first_free_index;

        let mut front_locs_vec = Vec::from(front_locs);

        let back_loc_iter = back_locs
            .iter()
            .skip(back_zero_index)
            .chain(back_locs.iter().take(back_zero_index));

        let insert_pos = (front_zero_index + front_len) % (front_len + 1);

        front_locs_vec.splice(insert_pos..insert_pos, back_loc_iter.copied());

        Self {
            is_circular: false,
            first_free_index: new_first_free_index,
            zero_index: BeltLenType::try_from(front_zero_index).unwrap(),
            locs: front_locs_vec.into_boxed_slice(),
            inserters: new_inserters,
        }
    }

    fn find_and_update_real_first_free_index(&mut self) -> BeltLenType {
        let new_free_index = match self.first_free_index {
            FreeIndex::FreeIndex(index) => index,
            FreeIndex::OldFreeIndex(index) => {
                // println!("HAD TO SEARCH FOR FIRST FREE INDEX!");

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

                BeltLenType::try_from(
                    iter.position(|x| x.is_none())
                        .unwrap_or(self.locs.len() - usize::from(search_start_index))
                        + usize::from(search_start_index),
                )
                .unwrap()
            },
        };

        self.first_free_index = FreeIndex::FreeIndex(new_free_index);

        new_free_index
    }
}

impl<ItemIdxType: IdxTrait> Belt<ItemIdxType> for SushiBelt<ItemIdxType> {
    fn query_item(
        &self,
        pos: super::belt::BeltLenType,
    ) -> Option<super::belt::ItemInfo<ItemIdxType>> {
        let pos = usize::from((self.zero_index + pos) % self.get_len());

        self.locs[pos].map(|item| super::belt::ItemInfo::Sushi(item))
    }

    fn remove_item(
        &mut self,
        pos: super::belt::BeltLenType,
    ) -> Option<super::belt::ItemInfo<ItemIdxType>> {
        let pos = usize::from((self.zero_index + pos) % self.get_len());

        self.locs[pos]
            .take()
            .map(|item| super::belt::ItemInfo::Sushi(item))
    }

    fn try_insert_item(
        &mut self,
        pos: super::belt::BeltLenType,
        item: Item<ItemIdxType>,
    ) -> Result<(), super::belt::NoSpaceError> {
        let pos = usize::from((self.zero_index + pos) % self.get_len());

        match self.locs[pos] {
            Some(_) => Err(NoSpaceError),
            None => {
                self.locs[pos] = Some(item);
                Ok(())
            },
        }
    }

    fn items(&self) -> Vec<Option<Item<ItemIdxType>>> {
        let len = self.locs.len();
        let (start, end) = self
            .locs
            .split_at(usize::from(self.zero_index % self.get_len()));

        debug_assert_eq!(end.iter().chain(start.iter()).count(), len);
        end.iter()
            .chain(start.iter())
            .map(|loc| loc.map(|item| item))
            .collect()
    }

    fn get_len(&self) -> super::belt::BeltLenType {
        BeltLenType::try_from(self.locs.len()).expect("Belt too long!")
    }

    #[profiling::function]
    fn update(&mut self) {
        if self.query_item(0).is_none() {
            // Correctness: Since we always % len whenever we access using self.zero_index, we do not need to % len here
            // TODO: This could overflow after usize::MAX ticks which is 9749040289 Years. Should be fine!
            self.zero_index += 1;
            match self.first_free_index {
                FreeIndex::FreeIndex(0) | FreeIndex::OldFreeIndex(0) => {
                    if self.query_item(0).is_none() {
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
        // println!("SLOW path");

        let first_free_index_real = self.find_and_update_real_first_free_index();

        let len = self.get_len();

        let slice = &mut self.locs;

        let (end_slice, start_slice) = slice.split_at_mut(self.zero_index.into());

        if self.zero_index + first_free_index_real >= len {
            // We have two stuck and one moving slice
            let (middle_stuck_slice, moving_slice) =
                end_slice.split_at_mut(((self.zero_index + first_free_index_real) % len).into());

            // We can now either move the "moving_slice" to the left or the "stuck_slice" to the right and update the zero index
            // We choose whichever is shorter

            // TODO: Here it might sometimes be useful not to split at self.zero_index?

            if first_free_index_real < len / 2 {
                // Move the stuck_slice to the right
                // TODO: Zero length checking!

                if !start_slice.is_empty() && !middle_stuck_slice.is_empty() {
                    // TODO: Do I need to check if moving_slice.len() > 0?

                    // Step 1: Copy the empty spot into the stuck start_slice
                    mem::swap(
                        &mut middle_stuck_slice[middle_stuck_slice.len() - 1],
                        &mut moving_slice[0],
                    );

                    // Step 2: Rotate the middle_stuck_slice to the right moving the empty spot to the start
                    middle_stuck_slice.rotate_right(1);

                    debug_assert!(middle_stuck_slice[0].is_none());

                    // Step 3: Copy the empty spot into the stuck start_slice
                    mem::swap(
                        &mut start_slice[start_slice.len() - 1],
                        &mut middle_stuck_slice[0],
                    );

                    // Step 4: Rotate the start_slice to the right moving the empty spot to the start of the belt
                    start_slice.rotate_right(1);
                } else {
                    let non_empty_stuck_slice = if middle_stuck_slice.is_empty() {
                        start_slice
                    } else {
                        middle_stuck_slice
                    };

                    // Step 1: Copy the first empty spot into the stuck slice
                    mem::swap(
                        &mut non_empty_stuck_slice[non_empty_stuck_slice.len() - 1],
                        &mut moving_slice[0],
                    );

                    // Steps 2,3 are not necessary since we only have one stuck slice

                    // Step 4: Rotate the start_slice to the right moving the empty spot to the start of the belt
                    non_empty_stuck_slice.rotate_right(1);
                }

                // Step 5: Update zero_index (which currently points at the new empty spot at the start of the array) to now point one further,
                // effectively moving the empty spot to the end of the belt
                // Correctness: Since we always % len whenever we access using self.zero_index, we do not need to % len here
                // TODO: This could overflow after usize::MAX ticks which is 9749040289 Years. Should be fine!
                self.zero_index += 1;
            } else {
                // Move the moving_slice to the left
                if !moving_slice.is_empty() {
                    // This will rotate the first item to the end.
                    moving_slice.rotate_left(1);
                    // The first item of moving_slice is the first empty slot, therefore we automatically have the empty slot at the end
                    // moving_slice[moving_slice.len() - 1].item = None;
                }
            }
        } else {
            let (starting_stuck_slice, middle_moving_slice) =
                start_slice.split_at_mut(first_free_index_real.into());

            assert!(!middle_moving_slice.is_empty());
            assert!(!starting_stuck_slice.is_empty());

            if first_free_index_real < len / 2 {
                mem::swap(
                    &mut middle_moving_slice[0],
                    &mut starting_stuck_slice[starting_stuck_slice.len() - 1],
                );

                starting_stuck_slice.rotate_right(1);

                self.zero_index += 1;
            } else {
                middle_moving_slice.rotate_left(1);

                if !end_slice.is_empty() {
                    mem::swap(
                        &mut middle_moving_slice[middle_moving_slice.len() - 1],
                        &mut end_slice[0],
                    );

                    end_slice.rotate_left(1);
                }
            }
        }

        // Instead of finding the real first_free_index after the update, we just use OldFreeIndex since most likely an inserter
        // Will update it for us before the next update
        self.first_free_index = FreeIndex::OldFreeIndex(first_free_index_real);
    }

    fn item_hint(&self) -> Option<Vec<Item<ItemIdxType>>> {
        // TODO: Maybe Return something here
        None
    }

    fn add_length(&mut self, amount: BeltLenType, side: super::smart::Side) -> BeltLenType {
        let len = self.get_len();

        take_mut::take(self, |slf| {
            let Self {
                is_circular: _,
                first_free_index,
                zero_index,
                locs,
                mut inserters,
            } = slf;

            // Important, first_free_index must ALWAYS be used using mod len
            let zero_index = zero_index % len;

            let mut locs = locs.into_vec();

            let old_len = locs.len();

            let (new_empty, new_zero, front_extension_amount) = match side {
                Side::FRONT => {
                    locs.splice(
                        usize::from(zero_index)..usize::from(zero_index),
                        repeat(None).take(usize::from(amount)),
                    );
                    (FreeIndex::FreeIndex(0), zero_index, amount)
                },
                Side::BACK => {
                    locs.splice(
                        ((usize::from(zero_index) + (old_len - 1)) % old_len)
                            ..((usize::from(zero_index) + (old_len - 1)) % old_len),
                        repeat(None).take(usize::from(amount)),
                    );
                    (
                        match first_free_index {
                            FreeIndex::FreeIndex(idx) => FreeIndex::FreeIndex(idx),
                            FreeIndex::OldFreeIndex(idx) => FreeIndex::OldFreeIndex(idx),
                        },
                        zero_index + amount,
                        0,
                    )
                },
            };

            if side == Side::FRONT {
                inserters.offsets[0] = inserters.offsets[0]
                    .checked_add(front_extension_amount)
                    .expect("Max length of belt (u16::MAX) reached");
            }

            Self {
                is_circular: false,
                first_free_index: new_empty,
                zero_index: new_zero,
                locs: locs.into_boxed_slice(),
                inserters,
            }
        });

        len + amount
    }
}
