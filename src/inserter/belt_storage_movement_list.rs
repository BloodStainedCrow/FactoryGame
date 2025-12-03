use std::{cmp::min, num::NonZero};

use crate::{
    assembler::simd::{InserterWithBelts, InserterWithBeltsEnum},
    belt::{
        belt::{Belt, BeltLenType},
        smart::{InserterExtractedWhenMoving, SmartBelt},
    },
    inserter::{FakeUnionStorage, belt_storage_inserter::Dir},
    item::{ITEMCOUNTTYPE, IdxTrait},
    storage_list::{SingleItemStorages, index_fake_union},
};

#[cfg(feature = "client")]
use egui_show_info_derive::ShowInfo;
#[cfg(feature = "client")]
use get_size2::GetSize;

#[cfg_attr(feature = "client", derive(ShowInfo), derive(GetSize))]
#[derive(Debug, Clone, Copy, serde::Deserialize, serde::Serialize)]
pub struct BeltStorageInserterInMovement {
    pub(crate) movetime: NonZero<u8>,
    pub(crate) storage: FakeUnionStorage,
    pub(crate) belt: u32,
    pub(crate) belt_pos: BeltLenType,

    pub(crate) current_hand: ITEMCOUNTTYPE,
    pub(crate) max_hand_size: ITEMCOUNTTYPE,
}

#[cfg_attr(feature = "client", derive(ShowInfo), derive(GetSize))]
#[derive(Debug, Clone, serde::Deserialize, serde::Serialize)]
pub struct List<const SWING_DIR: Dir, const ITEM_FLOW_DIR: Dir> {
    zero_index: usize,
    lists: Box<[Vec<BeltStorageInserterInMovement>]>,
}

impl<const SWING_DIR: Dir, const ITEM_FLOW_DIR: Dir> Default for List<SWING_DIR, ITEM_FLOW_DIR> {
    fn default() -> Self {
        Self {
            zero_index: 0,
            lists: vec![vec![]; u8::MAX as usize + 2].into_boxed_slice(),
        }
    }
}

pub struct FinishedMovingLists<'a, const SWING_DIR: Dir, const ITEM_FLOW_DIR: Dir> {
    list: &'a mut Vec<BeltStorageInserterInMovement>,
}

pub struct ReinsertionLists<'a, const SWING_DIR: Dir, const ITEM_FLOW_DIR: Dir> {
    first: &'a mut [Vec<BeltStorageInserterInMovement>],
    second: &'a mut [Vec<BeltStorageInserterInMovement>],
}

impl<'a, const SWING_DIR: Dir, const ITEM_FLOW_DIR: Dir>
    ReinsertionLists<'a, SWING_DIR, ITEM_FLOW_DIR>
{
    pub fn reinsert(&mut self, movetime: NonZero<u16>, ins: BeltStorageInserterInMovement) {
        // TODO: Maybe it is worth it to rotate the lists to avoid this branch?
        if (u16::from(movetime) as usize) < self.first.len() {
            self.first
                .get_mut(u16::from(movetime) as usize)
                .expect(&format!("movetime: {:?}", movetime))
                .push(ins);
        } else {
            self.second
                .get_mut((u16::from(movetime) as usize) - self.first.len())
                .expect(&format!("movetime: {:?}", movetime))
                .push(ins);
        }
    }
}

impl<const SWING_DIR: Dir, const ITEM_FLOW_DIR: Dir> List<SWING_DIR, ITEM_FLOW_DIR> {
    pub fn tick(&mut self) {
        self.zero_index += 1;
        self.zero_index %= self.lists.len();
    }

    pub fn get(
        &mut self,
    ) -> (
        FinishedMovingLists<'_, SWING_DIR, ITEM_FLOW_DIR>,
        ReinsertionLists<'_, SWING_DIR, ITEM_FLOW_DIR>,
    ) {
        let (second, rest) = self.lists.split_at_mut(self.zero_index);

        let ([finished], first) = rest.split_at_mut(1) else {
            unreachable!()
        };

        (
            FinishedMovingLists { list: finished },
            ReinsertionLists { first, second },
        )
    }

    pub fn reinsert(&mut self, movetime: NonZero<u16>, ins: BeltStorageInserterInMovement) {
        let index = (usize::from(u16::from(movetime)) + self.zero_index) % self.lists.len();
        self.lists[index].push(ins);
    }
}

impl<'a> FinishedMovingLists<'a, { Dir::BeltToStorage }, { Dir::BeltToStorage }> {
    pub fn update(
        self,
        item_id: usize,
        grid_size: usize,
        reinsertion_list: &mut ReinsertionLists<'_, { Dir::StorageToBelt }, { Dir::BeltToStorage }>,
        storages: SingleItemStorages,
    ) {
        self.list.retain_mut(|inserter| {
            let (max_insert, data, wait_list) =
                index_fake_union(item_id, storages, inserter.storage, grid_size);

            let items_moved = min(inserter.current_hand, *max_insert - *data);

            inserter.current_hand -= items_moved;
            *data += items_moved;

            if inserter.current_hand == 0 {
                reinsertion_list.reinsert(inserter.movetime.into(), *inserter);
                false
            } else {
                if let Some((wait_list, wait_list_needed)) = wait_list {
                    if let Some((pos, empty)) = wait_list.inserters.iter_mut().enumerate().find(|(_i, v)| v.is_none()) {
                        if pos == 0 {
                            *wait_list_needed = inserter.current_hand;
                        }
                        *empty = Some(InserterWithBelts {
                            current_hand: inserter.current_hand,
                            max_hand: inserter.max_hand_size.into(),
                            rest: InserterWithBeltsEnum::BeltStorage {
                                self_is_source: false,
                                belt_id: inserter.belt,
                                belt_pos: inserter.belt_pos,
                            },
                            movetime: inserter.movetime.into(),
                        });
                        false
                    } else {
                        true
                    }
                } else {
                    true
                }
            }
        });
    }
}

impl<'a> FinishedMovingLists<'a, { Dir::BeltToStorage }, { Dir::StorageToBelt }> {
    pub fn update(
        self,
        item_id: usize,
        grid_size: usize,
        reinsertion_list: &mut ReinsertionLists<'_, { Dir::StorageToBelt }, { Dir::StorageToBelt }>,
        storages: SingleItemStorages,
    ) {
        self.list.retain_mut(|inserter| {
            let (max_insert, data, wait_list) =
                index_fake_union(item_id, storages, inserter.storage, grid_size);

            let items_moved = min(inserter.max_hand_size - inserter.current_hand, *data);

            inserter.current_hand += items_moved;
            *data -= items_moved;

            if inserter.current_hand == inserter.max_hand_size {
                reinsertion_list.reinsert(inserter.movetime.into(), *inserter);
                false
            } else {
                if let Some((wait_list, wait_list_needed)) = wait_list {
                    if let Some((pos, empty)) = wait_list.inserters.iter_mut().enumerate().find(|(_i, v)| v.is_none()) {
                        if pos == 0 {
                            *wait_list_needed = inserter.max_hand_size - inserter.current_hand;

                        }
                        *empty = Some(InserterWithBelts {
                            current_hand: inserter.current_hand,
                            max_hand: inserter.max_hand_size.into(),
                            rest: InserterWithBeltsEnum::BeltStorage {
                                self_is_source: true,
                                belt_id: inserter.belt,
                                belt_pos: inserter.belt_pos,
                            },
                            movetime: inserter.movetime.into(),
                        });
                        false
                    } else {
                        true
                    }
                } else {
                    true
                }
            }
        });
    }
}

impl<'a> FinishedMovingLists<'a, { Dir::StorageToBelt }, { Dir::StorageToBelt }> {
    pub fn update<ItemIdxType: IdxTrait>(
        self,
        reinsertion_list: &mut ReinsertionLists<'_, { Dir::BeltToStorage }, { Dir::StorageToBelt }>,
        belts: &mut [SmartBelt<ItemIdxType>],
    ) {
        self.list.retain(|inserter| {
            let belt = &mut belts[inserter.belt as usize];

            let mut current_hand = inserter.max_hand_size;

            if belt.try_insert_correct_item(inserter.belt_pos).is_ok() {
                current_hand -= 1;
            }

            if current_hand == 0 {
                reinsertion_list.reinsert(inserter.movetime.into(), *inserter);
                false
            } else {
                belt.inserters.inserters.push(InserterExtractedWhenMoving {
                    storage: inserter.storage,
                    belt_pos: inserter.belt_pos,
                    movetime: inserter.movetime,
                    outgoing: false,
                    max_hand_size: inserter.max_hand_size,
                    current_hand,
                });
                false
            }
        });
    }
}

impl<'a> FinishedMovingLists<'a, { Dir::StorageToBelt }, { Dir::BeltToStorage }> {
    pub fn update<ItemIdxType: IdxTrait>(
        self,
        reinsertion_list: &mut ReinsertionLists<'_, { Dir::BeltToStorage }, { Dir::BeltToStorage }>,
        belts: &mut [SmartBelt<ItemIdxType>],
    ) {
        self.list.retain(|inserter| {
            let belt = &mut belts[inserter.belt as usize];

            let mut current_hand = 0;

            if belt.remove_item(inserter.belt_pos).is_some() {
                current_hand += 1;
            }

            if current_hand == inserter.max_hand_size {
                reinsertion_list.reinsert(inserter.movetime.into(), *inserter);
                false
            } else {
                belt.inserters.inserters.push(InserterExtractedWhenMoving {
                    storage: inserter.storage,
                    belt_pos: inserter.belt_pos,
                    movetime: inserter.movetime,
                    outgoing: true,
                    max_hand_size: inserter.max_hand_size,
                    current_hand,
                });
                false
            }
        });
    }
}
