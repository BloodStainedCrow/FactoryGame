use std::iter::repeat;

use itertools::Itertools;

use crate::{
    belt::belt::NoSpaceError,
    inserter::{belt_storage_inserter::BeltStorageInserter, Storage},
    item::{IdxTrait, Item, WeakIdxTrait},
};

use super::{
    belt::{Belt, BeltLenType},
    smart::{InserterStore, Side, SmartBelt, SpaceOccupiedError},
    FreeIndex, Inserter,
};

#[derive(Debug, Clone, serde::Deserialize, serde::Serialize)]
pub(super) struct SushiBelt<ItemIdxType: WeakIdxTrait, RecipeIdxType: WeakIdxTrait> {
    pub(super) is_circular: bool,
    pub(super) locs: Box<[Option<Item<ItemIdxType>>]>,
    pub(super) first_free_index: FreeIndex,
    /// Important, zero_index must ALWAYS be used using mod len
    pub(super) zero_index: BeltLenType,
    pub(super) inserters: SushiInserterStore<ItemIdxType, RecipeIdxType>,
}

#[derive(Debug, Clone, serde::Deserialize, serde::Serialize)]
pub(super) struct SushiInserterStore<ItemIdxType: WeakIdxTrait, RecipeIdxType: WeakIdxTrait> {
    pub(super) inserters: Vec<(Inserter<RecipeIdxType>, Item<ItemIdxType>)>,
    pub(super) offsets: Vec<u16>,
}

#[derive(Debug, PartialEq, Eq)]
pub(super) enum SushiInfo<ItemIdxType: WeakIdxTrait> {
    Sushi,
    Pure(Option<Item<ItemIdxType>>),
}

impl<ItemIdxType: IdxTrait, RecipeIdxType: IdxTrait> SushiBelt<ItemIdxType, RecipeIdxType> {
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
        storage_id: Storage<RecipeIdxType>,
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
        storage_id: Storage<RecipeIdxType>,
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

    pub fn into_smart_belt(self, item: Item<ItemIdxType>) -> SmartBelt<ItemIdxType, RecipeIdxType> {
        let found_item = self
            .locs
            .iter()
            .copied()
            .flatten()
            .all_equal_value()
            .expect("Belt is not pure (or is empty)!");

        assert_eq!(item, found_item);

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
                .into_iter()
                .map(|loc| {
                    if let Some(found_item) = loc {
                        debug_assert_eq!(*found_item, item);
                    }
                    loc.is_some()
                })
                .collect(),
            inserters: InserterStore {
                inserters: inserters.into_iter().map(|(ins, _)| ins).collect(),
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
}

impl<ItemIdxType: IdxTrait, RecipeIdxType: IdxTrait> Belt<ItemIdxType>
    for SushiBelt<ItemIdxType, RecipeIdxType>
{
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

    fn update(&mut self) {
        if self.is_circular {
            self.zero_index = (self.zero_index + 1) % self.get_len();
        }

        todo!()
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
