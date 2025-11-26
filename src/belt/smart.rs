use std::{
    iter::repeat,
    num::NonZero,
    ops::{Deref, DerefMut},
    sync::atomic::AtomicUsize,
    u8,
};

use crate::inserter::{
    belt_storage_inserter_non_const_gen::DynInserterState,
    belt_storage_movement_list::{BeltStorageInserterInMovement, ReinsertionLists},
};
use crate::{
    inserter::{
        InserterState, belt_storage_inserter::Dir,
        belt_storage_inserter_non_const_gen::BeltStorageInserterDyn,
    },
    item::{ITEMCOUNTTYPE, IdxTrait, Item, WeakIdxTrait},
};
use bitvec::{
    access::BitSafeUsize,
    bitbox,
    boxed::BitBox,
    ptr::{BitRef, Mut},
    slice::BitSlice,
};
use itertools::Either;
use itertools::Itertools;
use log::trace;
use static_assertions::const_assert;
use std::mem;

use super::{
    FreeIndex, SplitterID,
    belt::{Belt, BeltLenType, NoSpaceError},
    splitter::{SplitterSide, SushiSplitter},
    sushi::{SushiBelt, SushiInserterStoreDyn},
};
use crate::inserter::FakeUnionStorage;

#[cfg(feature = "client")]
use egui_show_info_derive::ShowInfo;
#[cfg(feature = "client")]
use get_size2::GetSize;

// #[cfg(debug_assertions)]
pub static NUM_BELT_UPDATES: AtomicUsize = AtomicUsize::new(0);
// #[cfg(debug_assertions)]
pub static NUM_BELT_FREE_CACHE_HITS: AtomicUsize = AtomicUsize::new(0);
// #[cfg(debug_assertions)]
pub static NUM_BELT_LOCS_SEARCHED: AtomicUsize = AtomicUsize::new(0);

#[allow(clippy::module_name_repetitions)]
#[cfg_attr(feature = "client", derive(ShowInfo), derive(GetSize))]
#[derive(Debug, Clone, serde::Deserialize, serde::Serialize)]
// FIXME: Make sure a smart belt fits in a cacheline
// #[repr(align(64))]
pub struct SmartBelt<ItemIdxType: WeakIdxTrait = u8> {
    pub(super) ty: u8,

    pub(super) is_circular: bool,
    pub(super) first_free_index: FreeIndex,
    /// Important, zero_index must ALWAYS be used using mod len
    pub(super) zero_index: BeltLenType,
    pub(super) locs: crate::get_size::BitBox,
    pub inserters: InserterStoreDyn,

    pub(super) item: Item<ItemIdxType>,

    pub last_moving_spot: BeltLenType,

    pub(super) input_splitter: Option<(SplitterID, SplitterSide)>,
    pub(super) output_splitter: Option<(SplitterID, SplitterSide)>,
}

// FIXME:
// const_assert! {std::mem::size_of::<SmartBelt<u8>>() <= 64}

#[cfg_attr(feature = "client", derive(ShowInfo), derive(GetSize))]
#[derive(Debug, Clone, serde::Deserialize, serde::Serialize, Default)]
pub struct EmptyBelt {
    ty: u8,

    is_circular: bool,

    pub(super) input_splitter: Option<(SplitterID, SplitterSide)>,
    pub(super) output_splitter: Option<(SplitterID, SplitterSide)>,

    pub len: u16,
}

#[cfg_attr(feature = "client", derive(ShowInfo), derive(GetSize))]
#[derive(Debug, Clone, serde::Deserialize, serde::Serialize)]
pub struct InserterExtractedWhenMoving {
    pub(crate) storage: FakeUnionStorage,
    pub(crate) belt_pos: BeltLenType,
    pub(crate) movetime: NonZero<u8>,
    pub(crate) outgoing: bool,
    pub(crate) max_hand_size: ITEMCOUNTTYPE,
    pub(crate) current_hand: ITEMCOUNTTYPE,
}

// TODO: Idea:
//       Have Belt inserters only be in belts as waitlist.
//       when their hand is full, move them into a "store" of some kind
//       where they are lazily waited on for their movetime. Once that is up, their outputs they either directly interact with the storage,
//       or are put into the respective waitlist on the storage (i.e. assembler)
//       Since the belts only interact with the "stores" 1.. lists and not the zeroth list (which interacts with the assemblers)
//       We should be able to
//          1. Do the belt update in parallel with the belt_storage_store
//          2. Stop the belt updates from needing the storage lists, making them parallelizable with assembler updates
//       This should ~double the amount of parallel processing we can do,
//       and should prevent us from having to wait on the (always going to be) slow belt update for starting on assemblers (significantly improving the parallelism there also)
#[cfg_attr(feature = "client", derive(ShowInfo), derive(GetSize))]
#[derive(Debug, Clone, serde::Deserialize, serde::Serialize)]
pub struct InserterStoreDyn {
    pub inserters: Vec<InserterExtractedWhenMoving>,
}

#[derive(Debug)]
pub struct BeltInserterInfo {
    pub outgoing: bool,
    pub state: InserterState,
    pub connection: FakeUnionStorage,
    pub hand_size: ITEMCOUNTTYPE,
    pub movetime: u8,
}

#[derive(Debug)]
pub struct SpaceOccupiedError;

pub enum InserterAdditionError {
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
            locs: bitbox![0; len.into()].into(),
            inserters: InserterStoreDyn { inserters: vec![] },

            item,

            last_moving_spot: len,

            input_splitter: None,
            output_splitter: None,
        }
    }

    fn calculate_loc_index(
        pos: BeltLenType,
        zero_index: BeltLenType,
        belt_len: BeltLenType,
    ) -> usize {
        (usize::from(zero_index)
            .checked_add(usize::from(pos))
            .unwrap())
            % usize::from(belt_len)
    }

    fn into_loc_index(&self, pos: BeltLenType) -> usize {
        Self::calculate_loc_index(pos, self.zero_index, self.locs.len().try_into().unwrap())
    }

    pub fn try_insert_correct_item(&mut self, belt_pos: BeltLenType) -> Result<(), NoSpaceError> {
        self.try_insert_item(belt_pos, self.item)
    }

    pub(super) fn into_sushi_belt(self) -> SushiBelt<ItemIdxType> {
        let Self {
            ty,

            is_circular,
            first_free_index,
            zero_index,
            locs,
            inserters: InserterStoreDyn { inserters },
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
            inserters: SushiInserterStoreDyn {
                inserters: inserters
                    .into_iter()
                    .map(|ins| {
                        (
                            BeltStorageInserterDyn {
                                belt_pos: ins.belt_pos,
                                storage_id: ins.storage,
                                state: if ins.outgoing {
                                    DynInserterState::BSWaitingForSourceItems(ins.current_hand)
                                } else {
                                    DynInserterState::SBWaitingForSourceItems(ins.current_hand)
                                },
                            },
                            item,
                            ins.movetime,
                            ins.max_hand_size,
                        )
                    })
                    .collect(),
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

    pub(super) fn add_input_splitter(&mut self, id: SplitterID, side: SplitterSide) {
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

    pub(super) fn add_output_splitter(&mut self, id: SplitterID, side: SplitterSide) {
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

    pub(super) fn remove_input_splitter(&mut self) -> Option<(SplitterID, SplitterSide)> {
        self.input_splitter.take()
    }

    pub(super) fn remove_output_splitter(&mut self) -> Option<(SplitterID, SplitterSide)> {
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

    pub fn get(&self, index: u16) -> impl Deref<Target = bool> {
        let idx = self.into_loc_index(index);
        self.locs.get(idx).unwrap()
        // self.into_loc_index(index)]
    }

    fn get_mut(&mut self, index: u16) -> impl DerefMut + Deref<Target = bool> {
        let idx = self.into_loc_index(index);
        let v = self.locs.get_mut(idx).unwrap();

        v
        // &mut self.locs[self.into_loc_index(index)]
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

    pub fn get_two(&self, indices: [BeltLenType; 2]) -> [impl Deref<Target = bool>; 2] {
        indices.map(|i| self.get(i))
    }

    pub fn change_inserter_storage_id(&mut self, old: FakeUnionStorage, new: FakeUnionStorage) {
        for inserter in &mut self.inserters.inserters {
            if inserter.storage == old {
                inserter.storage = new;
            }
        }
    }

    pub fn set_inserter_storage_id(&mut self, belt_pos: u16, new: FakeUnionStorage) {
        let mut pos = 0;

        for inserter in self.inserters.inserters.iter_mut() {
            pos = inserter.belt_pos;
            if pos == belt_pos {
                inserter.storage = new;
                return;
            }
        }
        unreachable!(
            "Tried to set_inserter_storage_id with position {belt_pos}, which does not contain an inserter. {:?}",
            self.inserters
        );
    }

    #[must_use]
    pub fn get_inserter_info_at(&self, belt_pos: u16) -> Option<BeltInserterInfo> {
        let mut pos = 0;

        for inserter in self.inserters.inserters.iter() {
            pos = inserter.belt_pos;
            if pos == belt_pos {
                return Some(BeltInserterInfo {
                    outgoing: inserter.outgoing,
                    state: if inserter.outgoing {
                        InserterState::WaitingForSourceItems(inserter.current_hand)
                    } else {
                        InserterState::WaitingForSpaceInDestination(inserter.current_hand)
                    },
                    connection: inserter.storage,
                    movetime: inserter.movetime.into(),
                    hand_size: inserter.max_hand_size,
                });
            }
        }

        None
    }

    pub(super) fn change_inserter_movetime(
        &mut self,
        belt_pos: BeltLenType,
        new_movetime: NonZero<u16>,
    ) {
        let Some(inserter) = self
            .inserters
            .inserters
            .iter_mut()
            .find(|ins| ins.belt_pos == belt_pos)
        else {
            unreachable!(
                "The belt did not have an inserter at position specified to change movetime"
            )
        };
        inserter.movetime = u16::from(new_movetime)
            .try_into()
            .unwrap_or(u8::MAX)
            .try_into()
            .unwrap();
    }

    pub fn remove_inserter(&mut self, pos: BeltLenType) -> Result<FakeUnionStorage, ()> {
        if usize::from(pos) >= self.locs.len() {
            return Err(());
        }

        match self
            .inserters
            .inserters
            .iter()
            .position(|ins| ins.belt_pos == pos)
        {
            Some(idx) => {
                let removed = self.inserters.inserters.remove(idx);

                Ok(removed.storage)
            },
            None => Err(()),
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
        movetime: u16,
        hand_size: ITEMCOUNTTYPE,
    ) -> Result<(), InserterAdditionError> {
        assert!(
            usize::from(index) < self.locs.len(),
            "Bounds check {index} >= {}",
            self.locs.len()
        );

        if filter != self.item {
            return Err(InserterAdditionError::ItemMismatch);
        }

        self.inserters.inserters.push(InserterExtractedWhenMoving {
            storage: storage_id,
            belt_pos: index,
            movetime: movetime.try_into().unwrap_or(u8::MAX).try_into().unwrap(),
            outgoing: true,
            max_hand_size: hand_size,
            current_hand: 0,
        });

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
        movetime: u16,
        hand_size: ITEMCOUNTTYPE,
    ) -> Result<(), InserterAdditionError> {
        assert!(
            usize::from(index) < self.locs.len(),
            "Bounds check {index} >= {}",
            self.locs.len()
        );

        // We only only return an item mismatch if we know the space is free, so we do not transition to sushi,
        // And then fail anyway
        if filter != self.item {
            return Err(InserterAdditionError::ItemMismatch);
        }
        self.inserters.inserters.push(InserterExtractedWhenMoving {
            storage: storage_id,
            belt_pos: index,
            movetime: movetime.try_into().unwrap_or(u8::MAX).try_into().unwrap(),
            outgoing: false,
            max_hand_size: hand_size,
            current_hand: 0,
        });

        Ok(())
    }

    // fn get_inserter(&self, index: u16) -> Option<&Inserter> {
    //     let mut pos_after_last_inserter = 0;

    //     for (i, offset) in self
    //         .inserters
    //         .inserters
    //         .iter()
    //         .map(|i| i.offset)
    //         .enumerate()
    //     {
    //         let next_inserter_pos = pos_after_last_inserter + offset;

    //         match next_inserter_pos.cmp(&index) {
    //             std::cmp::Ordering::Equal => return Some(&self.inserters.inserters[i]),
    //             std::cmp::Ordering::Greater => return None,

    //             std::cmp::Ordering::Less => pos_after_last_inserter = next_inserter_pos + 1,
    //         }
    //     }

    //     None
    // }

    pub fn get_num_inserters(&self) -> usize {
        self.inserters.inserters.len()
    }

    pub fn update_inserters<'a, 'b>(
        &mut self,
        self_index: u32,
        reinsertion_outgoing: &mut ReinsertionLists<
            '_,
            { Dir::BeltToStorage },
            { Dir::BeltToStorage },
        >,
        reinsertion_incoming: &mut ReinsertionLists<
            '_,
            { Dir::BeltToStorage },
            { Dir::StorageToBelt },
        >,
    ) {
        if self.get_len() == 0 {
            return;
        }

        let old_first_free = match self.first_free_index {
            FreeIndex::FreeIndex(idx) => idx,
            FreeIndex::OldFreeIndex(idx) => idx,
        };

        let mut new_first_free = old_first_free;

        let extracted = self.inserters.inserters.extract_if(.., |ins| {
            // FIXME: This should not be needed, if we did not incorrectly insert inserters always in the belt
            if ins.current_hand == 0 && !ins.outgoing {
                return true;
            }

            // Taken from VecDeque::wrap_index
            let logical_index = usize::from(self.zero_index) + usize::from(ins.belt_pos);
            let loc_idx = if logical_index >= self.locs.len() {
                logical_index - self.locs.len()
            } else {
                logical_index
            };

            if ins.belt_pos < old_first_free {
                // We KNOW this position is filled
                debug_assert!(self.locs[loc_idx]);
                if ins.outgoing {
                    ins.current_hand += 1;
                    self.locs.set(loc_idx, false);
                    if ins.belt_pos <= new_first_free {
                        self.first_free_index = FreeIndex::FreeIndex(ins.belt_pos);
                        new_first_free = ins.belt_pos;
                    }

                    if ins.current_hand == ins.max_hand_size {
                        true
                    } else {
                        false
                    }
                } else {
                    false
                }
            } else {
                let mut loc = self.locs.get_mut(loc_idx).unwrap();

                if ins.outgoing && *loc {
                    *loc = false;
                    ins.current_hand += 1;
                    if ins.current_hand == ins.max_hand_size {
                        true
                    } else {
                        false
                    }
                } else if !ins.outgoing && !*loc {
                    *loc = true;
                    ins.current_hand -= 1;

                    if ins.belt_pos == new_first_free && *loc {
                        // This was the old first free pos
                        self.first_free_index = FreeIndex::OldFreeIndex(ins.belt_pos);
                    }

                    if ins.current_hand == 0 { true } else { false }
                } else {
                    false
                }
            }
        });

        for ins in extracted {
            let in_movement = BeltStorageInserterInMovement {
                movetime: ins.movetime,
                storage: ins.storage,
                belt: self_index,
                belt_pos: ins.belt_pos,
                max_hand_size: ins.max_hand_size,
                current_hand: ins.current_hand,
            };
            if ins.outgoing {
                reinsertion_outgoing.reinsert(ins.movetime.into(), in_movement);
            } else {
                reinsertion_incoming.reinsert(ins.movetime.into(), in_movement);
            }
        }
    }

    fn remove_first_free_pos_maybe(&mut self, now_filled_pos: BeltLenType) {
        match self.first_free_index {
            FreeIndex::OldFreeIndex(index) | FreeIndex::FreeIndex(index) => {
                if now_filled_pos == index {
                    self.first_free_index = FreeIndex::OldFreeIndex(now_filled_pos);
                }
            },
        }
    }

    fn update_first_free_pos(&mut self, now_empty_pos: BeltLenType) {
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

    pub fn get_update_size(&self) -> (usize, usize, usize, usize, usize) {
        let free_index_search_indices = match self.first_free_index {
            FreeIndex::FreeIndex(idx) => vec![],
            FreeIndex::OldFreeIndex(idx) => self
                .items()
                .skip(usize::from(idx))
                .enumerate()
                .take_while(|(_, loc)| loc.is_some())
                .map(|(i, _)| i)
                .collect(),
        };

        let cache_lines_from_free_index_search = free_index_search_indices
            .into_iter()
            .map(|i| i / 8 / 64)
            .counts()
            .len();

        let cache_lines_from_inserter_structs = (self.inserters.inserters.len()
            * std::mem::size_of::<BeltStorageInserterDyn>())
        .div_ceil(64);

        let cache_lines_from_inserter_belt_lookup = self
            .inserters
            .inserters
            .iter()
            .map(|ins| match (ins.outgoing,) {
                (false,) => (ins.belt_pos, true),
                (true,) => (ins.belt_pos, true),

                _ => (ins.belt_pos, false),
            })
            .fold(0usize, |old_count, (belt_pos, needs)| {
                // let our_idx = belt_pos;
                // (old_count
                //     + usize::from(
                //         needs && (old_idx == 0 || (old_idx - 1) / 8 / 64 != our_idx / 8 / 64),
                //     ),)
                todo!()
            });

        let cache_lines_from_storage_lookup = self
            .inserters
            .inserters
            .iter()
            .filter_map(|ins| match (ins.outgoing,) {
                (false,) => Some(ins.storage),
                (true,) => Some(ins.storage),

                _ => None,
            })
            .map(|id| FakeUnionStorage {
                index: id.index / 64,
                grid_or_static_flag: id.grid_or_static_flag,
                recipe_idx_with_this_item: id.recipe_idx_with_this_item,
            })
            .counts()
            .len();

        let splitter_cache_lines = usize::from(self.input_splitter.is_some())
            + usize::from(self.output_splitter.is_some());

        (
            cache_lines_from_free_index_search,
            cache_lines_from_inserter_structs,
            cache_lines_from_inserter_belt_lookup,
            cache_lines_from_storage_lookup,
            splitter_cache_lines,
        )
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

                let first_slice = if (usize::from(self.zero_index)
                    + usize::from(search_start_index))
                    < self.locs.len()
                {
                    &self.locs[(usize::from(self.zero_index) + usize::from(search_start_index))..]
                } else {
                    BitSlice::empty()
                };

                let new_free_index = if let Some(idx) = first_slice.first_zero() {
                    idx + usize::from(search_start_index)
                } else {
                    let start = if (usize::from(self.zero_index) + usize::from(search_start_index))
                        < self.locs.len()
                    {
                        0
                    } else {
                        usize::from(self.zero_index) + usize::from(search_start_index)
                            - self.locs.len()
                    };

                    // TODO: I just added the start of this slice (which made a huge difference in the lots_of_belts benchmark)
                    // But suprisingly it did nothing on the gigabase?
                    // Investigate if that makes sense
                    let second_slice = &self.locs[start..usize::from(self.zero_index)];
                    second_slice
                        .first_zero()
                        .map(|v| v + start + (self.locs.len() - usize::from(self.zero_index)))
                        .unwrap_or(self.locs.len())
                };

                let new_free_index = new_free_index.try_into().unwrap();

                #[cfg(debug_assertions)]
                {
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

                    let iter_new_free_index = BeltLenType::try_from(
                        iter.position(|x| !(*x))
                            .unwrap_or(self.locs.len() - usize::from(search_start_index))
                            + usize::from(search_start_index),
                    )
                    .unwrap();

                    debug_assert_eq!(iter_new_free_index, new_free_index);
                }

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
        locs: &mut BitSlice,
        zero_index: BeltLenType,
    ) -> impl DoubleEndedIterator<Item = BitRef<'_, Mut, BitSafeUsize>> {
        // TODO: I have another implementation of this:
        // TODO: Check which is faster (or simpler)
        // let mut iter = self
        //     .locs
        //     .iter()
        //     .skip(self.zero_index)
        //     .chain(self.locs.iter().take(self.zero_index));
        let len = locs.len();
        let (start, end) = locs.split_at_mut(usize::from(zero_index) % locs.len());

        start.set(0, true);

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
            first_free_index: _front_first_free_index,
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

        // HUGE TODO: Make this work somehow
        assert_eq!(
            ty_front, ty_back,
            "Belts can only merge if they are the same type!"
        );

        // Important, first_free_index must ALWAYS be used using mod len
        let back_zero_index = usize::from(back_zero_index) % back_locs.len();

        let num_front_inserters = front_inserters.inserters.len();
        let _num_back_inserters = back_inserters.inserters.len();

        let mut new_inserters = front_inserters;
        new_inserters
            .inserters
            .extend(back_inserters.inserters.drain(..).map(|mut back_ins| {
                back_ins.belt_pos = back_ins
                    .belt_pos
                    .checked_add(front_len.try_into().unwrap())
                    .unwrap();
                back_ins
            }));

        let mut front_locs_vec = BitBox::from(front_locs).into_bitvec();

        front_locs_vec.rotate_left(front_zero_index);

        let back_loc_iter = back_locs
            .iter()
            .skip(back_zero_index)
            .chain(back_locs.iter().take(back_zero_index));

        front_locs_vec.extend(back_loc_iter.map(|v| *v));

        Self {
            ty: ty_front,

            is_circular: false,
            first_free_index: FreeIndex::OldFreeIndex(0),
            zero_index: 0,
            locs: front_locs_vec.into_boxed_bitslice().into(),
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

        let mut locs = BitBox::from(locs).into_bitvec();

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
            for ins in &mut inserters.inserters {
                ins.belt_pos = ins
                    .belt_pos
                    .checked_add(len)
                    .expect("Max length of belt (u16::MAX) reached");
            }
        }

        let mut new = Self {
            ty,

            is_circular: false,
            first_free_index: new_empty,
            zero_index: new_zero,
            locs: locs.into_boxed_bitslice().into(),
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
        todo!()
        // // TODO: Is this correct
        // if self.is_circular {
        //     assert!(self.input_splitter.is_none());
        //     assert!(self.output_splitter.is_none());
        //     self.is_circular = false;
        //     self.first_free_index = FreeIndex::OldFreeIndex(0);
        //     self.zero_index = belt_pos_to_break_at;
        //     // FIXME: This will teleport items
        //     return None;
        // }

        // if belt_pos_to_break_at == 0 || belt_pos_to_break_at == self.get_len() {
        //     return None;
        // }

        // let mut new_locs = None;
        // take_mut::take(&mut self.locs, |locs| {
        //     let mut locs_vec = BitBox::from(locs).into_bitvec();

        //     let len = locs_vec.len();

        //     locs_vec.rotate_left(usize::from(self.zero_index) % len);

        //     new_locs = Some(
        //         locs_vec
        //             .split_off(belt_pos_to_break_at.into())
        //             .into_boxed_bitslice(),
        //     );

        //     locs_vec.into_boxed_bitslice().into()
        // });

        // self.zero_index = 0;
        // self.first_free_index = FreeIndex::OldFreeIndex(0);

        // let new_locs = new_locs.unwrap();

        // let mut offsets = self
        //     .inserters
        //     .inserters
        //     .iter()
        //     .map(|i| i.0.offset)
        //     .enumerate();

        // let mut current_pos = 0;

        // let (split_at_inserters, new_offs) = loop {
        //     let Some((i, next_offset)) = offsets.next() else {
        //         break (self.inserters.inserters.len(), 0);
        //     };

        //     // Skip next_offset spots
        //     current_pos += next_offset;

        //     if current_pos >= belt_pos_to_break_at {
        //         break (i, current_pos - belt_pos_to_break_at);
        //     }

        //     // The spot, the inserter corresponding to this offset is placed
        //     current_pos += 1;
        // };

        // let mut new_inserters = None;
        // take_mut::take(&mut self.inserters.inserters, |ins| {
        //     let mut ins = ins.into_vec();
        //     new_inserters = Some(ins.split_off(split_at_inserters).into_boxed_slice());
        //     ins.into_boxed_slice()
        // });
        // let mut new_inserters = new_inserters.unwrap();

        // if let Some(ins) = new_inserters.get_mut(0) {
        //     ins.0.offset = new_offs;
        // }

        // // Since we split off the back portion, it will own our input splitter if we have one
        // let input_splitter = self.input_splitter.take();

        // let new_belt = Self {
        //     ty: self.ty,

        //     is_circular: false,
        //     first_free_index: FreeIndex::OldFreeIndex(0),
        //     zero_index: 0,
        //     locs: new_locs.into(),
        //     inserters: InserterStoreDyn {
        //         inserters: new_inserters,
        //     },
        //     item: self.item,

        //     last_moving_spot: self.last_moving_spot.saturating_sub(belt_pos_to_break_at),

        //     input_splitter,
        //     output_splitter: None,
        // };

        // Some(new_belt)
    }
}

impl EmptyBelt {
    #[must_use]
    pub const fn new(ty: u8, len: u16) -> Self {
        Self {
            ty,

            input_splitter: None,
            output_splitter: None,

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

        assert!(back.output_splitter.is_none());
        assert!(front.input_splitter.is_none());

        Self {
            ty: front.ty,

            output_splitter: front.output_splitter,
            input_splitter: back.input_splitter,

            is_circular: false,
            len: front.len + back.len,
        }
    }

    pub fn into_smart_belt<ItemIdxType: IdxTrait>(
        self,
        item: Item<ItemIdxType>,
    ) -> SmartBelt<ItemIdxType> {
        assert!(self.len > 0);
        SmartBelt {
            ty: self.ty,
            is_circular: self.is_circular,
            first_free_index: FreeIndex::FreeIndex(0),
            zero_index: 0,
            locs: bitbox![0; self.len as usize].into(),
            inserters: InserterStoreDyn { inserters: vec![] },
            item,
            last_moving_spot: 0,
            input_splitter: self.input_splitter,
            output_splitter: self.output_splitter,
        }
    }

    pub fn into_sushi_belt<ItemIdxType: IdxTrait>(self) -> SushiBelt<ItemIdxType> {
        assert!(self.len > 0);
        SushiBelt {
            ty: self.ty,
            is_circular: self.is_circular,
            first_free_index: FreeIndex::FreeIndex(0),
            zero_index: 0,
            locs: vec![None; self.len as usize].into_boxed_slice(),
            inserters: SushiInserterStoreDyn {
                inserters: vec![].into_boxed_slice(),
            },
            last_moving_spot: 0,
            input_splitter: self.input_splitter,
            output_splitter: self.output_splitter,
        }
    }

    pub fn add_length(&mut self, amount: u16, _side: Side) -> u16 {
        self.len += amount;
        self.len
    }

    pub fn break_belt_at(&mut self, pos_to_break_at: u16) -> Option<Self> {
        if self.is_circular {
            self.is_circular = false;
            return None;
        }

        if pos_to_break_at == 0 || pos_to_break_at == self.len {
            return None;
        }

        let input_splitter = self.input_splitter.take();

        let old_len = self.len;
        self.len = pos_to_break_at;

        // This is the back belt
        Some(Self {
            ty: self.ty,

            is_circular: false,
            len: old_len - pos_to_break_at,

            input_splitter,
            output_splitter: None,
        })
    }

    pub fn make_circular(&mut self) {
        self.is_circular = true;
    }

    pub(super) fn add_input_splitter(&mut self, id: SplitterID, side: SplitterSide) {
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

    pub(super) fn add_output_splitter(&mut self, id: SplitterID, side: SplitterSide) {
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

    pub(super) fn remove_input_splitter(&mut self) -> Option<(SplitterID, SplitterSide)> {
        self.input_splitter.take()
    }

    pub(super) fn remove_output_splitter(&mut self) -> Option<(SplitterID, SplitterSide)> {
        self.output_splitter.take()
    }
}

impl<ItemIdxType: IdxTrait> Belt<ItemIdxType> for EmptyBelt {
    fn query_item(&self, _pos: BeltLenType) -> Option<Item<ItemIdxType>> {
        None
    }

    fn remove_item(&mut self, _pos: BeltLenType) -> Option<Item<ItemIdxType>> {
        None
    }

    fn try_insert_item(
        &mut self,
        _pos: BeltLenType,
        _item: Item<ItemIdxType>,
    ) -> Result<(), NoSpaceError> {
        unimplemented!()
    }

    fn items(&self) -> impl Iterator<Item = Option<Item<ItemIdxType>>> {
        std::iter::repeat_n(None, self.len as usize)
    }

    fn items_in_range(
        &self,
        range: std::ops::RangeInclusive<BeltLenType>,
    ) -> impl Iterator<Item = Option<Item<ItemIdxType>>> {
        std::iter::repeat_n(None, range.len() as usize)
    }

    fn get_len(&self) -> BeltLenType {
        self.len
    }

    fn add_length(&mut self, amount: BeltLenType, side: Side) -> BeltLenType {
        self.len += amount;
        self.len
    }

    fn remove_length(
        &mut self,
        amount: BeltLenType,
        _side: Side,
    ) -> (Vec<(Item<ItemIdxType>, u32)>, BeltLenType) {
        self.len -= amount;
        (vec![], self.len)
    }

    fn update(&mut self, _splitter_list: &[SushiSplitter<ItemIdxType>]) {
        // TODO: Do i want to stop this from being called or just do nothing
        unimplemented!()
    }

    fn item_hint(&self) -> Option<Vec<Item<ItemIdxType>>> {
        None
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

    fn remove_length(
        &mut self,
        amount: BeltLenType,
        side: Side,
    ) -> (Vec<(Item<ItemIdxType>, u32)>, BeltLenType) {
        if amount == 0 {
            return (vec![], self.get_len());
        }

        let kept_range = match side {
            Side::FRONT => amount..self.get_len(),
            Side::BACK => 0..(self.get_len().checked_sub(amount).unwrap()),
        };

        let before_inserter_positions = self
            .inserters
            .inserters
            .iter()
            .map(|ins| ins.belt_pos)
            .collect_vec();

        assert!(!self.is_circular);
        assert!(amount <= self.get_len());

        let len = self.locs.len();
        self.locs.rotate_left(self.zero_index as usize % len);
        self.zero_index = 0;
        let mut item_count = 0;
        take_mut::take(&mut self.locs, |locs| {
            let mut locs = BitBox::from(locs).into_bitvec();

            let removed_items = match side {
                Side::FRONT => locs.drain(..(amount as usize)),
                Side::BACK => locs.drain((locs.len() - (amount as usize))..),
            };

            item_count = removed_items.filter(|loc| *loc).count() as u32;

            locs.into_boxed_bitslice().into()
        });

        self.inserters.inserters.retain(|inserter| {
            if !kept_range.contains(&inserter.belt_pos) {
                false
            } else {
                true
            }
        });

        if side == Side::FRONT {
            for ins in &mut self.inserters.inserters {
                ins.belt_pos = ins.belt_pos.checked_sub(amount).unwrap();
            }
        }

        self.first_free_index = FreeIndex::OldFreeIndex(0);

        let after_inserter_positions = self
            .inserters
            .inserters
            .iter()
            .map(|ins| ins.belt_pos)
            .collect_vec();

        match side {
            Side::FRONT => {
                for (before, after) in before_inserter_positions
                    .iter()
                    .rev()
                    .zip(after_inserter_positions.iter().rev())
                {
                    assert_eq!(
                        *before - amount,
                        *after,
                        "before: {:?}\n after: {:?}",
                        before_inserter_positions,
                        after_inserter_positions
                    );
                }
            },
            Side::BACK => {
                for (before, after) in before_inserter_positions
                    .iter()
                    .zip(after_inserter_positions.iter())
                {
                    assert_eq!(before, after);
                }
            },
        }

        (vec![(self.item, item_count)], self.get_len())
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
            *self.get_mut(pos) = true;

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
    #[inline(always)]
    fn update(&mut self, splitter_list: &[SushiSplitter<ItemIdxType>]) {
        if self.locs.len() == 0 {
            return;
        }
        if self.is_circular {
            let new_val = self.zero_index.checked_add(1).unwrap();
            self.zero_index = if new_val == self.get_len() {
                0
            } else {
                new_val
            };
            return;
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
                    assert_eq!(self.first_free_index, FreeIndex::FreeIndex(0));
                    self.last_moving_spot = 0;
                    let new_val = self.zero_index.checked_add(1).unwrap();
                    self.zero_index = if new_val == self.get_len() {
                        0
                    } else {
                        new_val
                    };
                    self.first_free_index = FreeIndex::OldFreeIndex(0);
                    return;
                }
            }
        }

        match self.first_free_index {
            FreeIndex::FreeIndex(idx) | FreeIndex::OldFreeIndex(idx) => {
                debug_assert!(idx <= self.get_len());
            },
        }

        #[cfg(debug_assertions)]
        {
            NUM_BELT_UPDATES.fetch_add(1, std::sync::atomic::Ordering::Relaxed);
            match self.first_free_index {
                FreeIndex::FreeIndex(_) => {
                    NUM_BELT_FREE_CACHE_HITS.fetch_add(1, std::sync::atomic::Ordering::Relaxed);
                },
                FreeIndex::OldFreeIndex(_) => {},
            }
        }
        let (old_free, need_to_check) = match self.first_free_index {
            FreeIndex::FreeIndex(idx) => (idx, false),
            FreeIndex::OldFreeIndex(idx) => (idx, true),
        };

        let first_free_index_real = self.find_and_update_real_first_free_index();

        let len = self.get_len();

        self.last_moving_spot = first_free_index_real.unwrap_or(len);

        let Some(first_free_index_real) = first_free_index_real else {
            // All slots are full
            #[cfg(debug_assertions)]
            {
                NUM_BELT_LOCS_SEARCHED.fetch_add(
                    (len - old_free) as usize,
                    std::sync::atomic::Ordering::Relaxed,
                );
            }
            return;
        };

        #[cfg(debug_assertions)]
        {
            NUM_BELT_LOCS_SEARCHED.fetch_add(
                (first_free_index_real - old_free) as usize,
                std::sync::atomic::Ordering::Relaxed,
            );
        }

        if first_free_index_real == 0 {
            let new_val = self.zero_index.checked_add(1).unwrap();
            self.zero_index = if new_val == self.get_len() {
                0
            } else {
                new_val
            };
            self.last_moving_spot = 0;
            if need_to_check {
                // We already loaded this cacheline, so we can cheaply get this to freeIndex here
                if self.query_item(0).is_some() {
                    self.first_free_index = FreeIndex::OldFreeIndex(0);
                } else {
                    self.first_free_index = FreeIndex::FreeIndex(0);
                }
            } else {
                self.first_free_index = FreeIndex::OldFreeIndex(0);
            }
            return;
        }

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

        // Prevent this addition from overflowing
        if u32::from(self.zero_index)
            .checked_add(u32::from(first_free_index_real))
            .unwrap()
            >= u32::from(len)
        {
            // We have two stuck and one moving slice
            let (_middle_stuck_slice, moving_slice) = end_slice.split_at_mut(
                (usize::from(self.zero_index) + usize::from(first_free_index_real))
                    % usize::from(len),
            );

            // By definition, we know starting_stuck_slice is fully compressed with items!
            // Also by definition we know that moving_slice[0] MUST be empty (otherwise it would not be moving and part of the stuck slice)

            // We already know the stuck slice is not empty, since otherwise we would just take the fast path above (unlikely if this is necessary)
            if !moving_slice.is_empty() {
                // Move the stuck slice back one step
                debug_assert_eq!(start_slice[0], true);
                *start_slice.get_mut(0).unwrap() = false;

                debug_assert_eq!(moving_slice[0], false);
                *moving_slice.get_mut(0).unwrap() = true;

                let new_val = self.zero_index.checked_add(1).unwrap();
                self.zero_index = if new_val == self.get_len() {
                    0
                } else {
                    new_val
                };
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
                *starting_stuck_slice.get_mut(0).unwrap() = false;

                debug_assert_eq!(middle_moving_slice[0], false);
                *middle_moving_slice.get_mut(0).unwrap() = true;

                let new_val = self.zero_index.checked_add(1).unwrap();
                self.zero_index = if new_val == self.get_len() {
                    0
                } else {
                    new_val
                };
            }
        }

        // Instead of finding the real first_free_index after the update, we just use OldFreeIndex since most likely an inserter
        // Will update it for us before the next update

        // This does not really hold.
        // Since we already have loaded first_free_index_real into cache, just check it now
        // TODO: Maybe we could even look at more indices as longs as we do not cross any cache line boundries
        if self.query_item(first_free_index_real).is_some() {
            self.first_free_index = FreeIndex::OldFreeIndex(first_free_index_real);
        } else {
            self.first_free_index = FreeIndex::FreeIndex(first_free_index_real);
        }
    }

    fn get_len(&self) -> BeltLenType {
        self.locs.len().try_into().unwrap()
    }

    fn query_item(&self, pos: BeltLenType) -> Option<Item<ItemIdxType>> {
        let idx = match self.first_free_index {
            FreeIndex::FreeIndex(idx) => idx,
            FreeIndex::OldFreeIndex(idx) => idx,
        };
        if pos < idx {
            return Some(self.item);
        }

        if self.locs[self.into_loc_index(pos)] {
            Some(self.item)
        } else {
            None
        }
    }

    fn remove_item(&mut self, pos: BeltLenType) -> Option<Item<ItemIdxType>> {
        if self.query_item(pos).is_some() {
            *self.get_mut(pos) = false;
            self.update_first_free_pos(pos);
            Some(self.item)
        } else {
            None
        }
    }

    fn items(&self) -> impl Iterator<Item = Option<Item<ItemIdxType>>> {
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
    }

    fn items_in_range(
        &self,
        range: std::ops::RangeInclusive<BeltLenType>,
    ) -> impl Iterator<Item = Option<Item<ItemIdxType>>> {
        let start_idx = (self.zero_index + range.start()) % self.get_len();
        let end_idx = (self.zero_index + range.end()) % self.get_len();

        if start_idx <= end_idx {
            // No chain needed
            Either::Left(
                self.locs[(start_idx as usize)..=(end_idx as usize)]
                    .iter()
                    .map(|loc| if *loc { Some(self.item) } else { None }),
            )
        } else {
            Either::Right(
                self.locs[(start_idx as usize)..]
                    .iter()
                    .chain(self.locs[..=(end_idx as usize)].iter())
                    .map(|loc| if *loc { Some(self.item) } else { None }),
            )
        }
    }

    fn item_hint(&self) -> Option<Vec<Item<ItemIdxType>>> {
        Some(vec![self.item])
    }
}
