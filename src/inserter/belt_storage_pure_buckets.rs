use itertools::Itertools;
use std::collections::HashMap;

use super::FakeUnionStorage;
use crate::{
    belt::{belt::Belt, smart::SmartBelt},
    item::{ITEMCOUNTTYPE, IdxTrait},
    storage_list::{SingleItemStorages, index_fake_union},
};
use log::{info, warn};
use std::{cmp::min, iter};

#[cfg(feature = "client")]
use egui_show_info_derive::ShowInfo;
#[cfg(feature = "client")]
use get_size2::GetSize;

use crate::belt::belt::BeltLenType;
use crate::inserter::belt_storage_inserter::Dir;

pub(super) const MAX_MOVE_TIME: usize = 1_000;

#[cfg_attr(feature = "client", derive(ShowInfo), derive(GetSize))]
#[derive(Debug, Clone, Copy, serde::Deserialize, serde::Serialize)]
// #[repr(packed)]
pub struct UpdatingInserter {
    pub storage_id: FakeUnionStorage,
    pub belt_id: u32,
    pub belt_pos: BeltLenType,
    pub max_hand_size: ITEMCOUNTTYPE,
    pub current_hand: ITEMCOUNTTYPE,
    pub id: InserterId,
    // pub dir: Dir,
}

#[cfg_attr(feature = "client", derive(ShowInfo), derive(GetSize))]
#[derive(Debug, Clone, Copy, serde::Deserialize, serde::Serialize)]
pub struct MovingInserter {
    pub storage_id: FakeUnionStorage,
    pub belt_id: u32,
    pub belt_pos: BeltLenType,
    pub max_hand_size: ITEMCOUNTTYPE,
    pub id: InserterId,
}

#[cfg_attr(feature = "client", derive(ShowInfo), derive(GetSize))]
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, serde::Deserialize, serde::Serialize)]
pub enum LargeInserterState {
    WaitingForSourceItems(ITEMCOUNTTYPE),
    WaitingForSpaceInDestination(ITEMCOUNTTYPE),
    FullAndMovingOut(u16),
    EmptyAndMovingBack(u16),
}

// This means at most u8::MAX inserters connecting any pair of Storages, that seems plenty
#[cfg_attr(feature = "client", derive(ShowInfo), derive(GetSize))]
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, serde::Deserialize, serde::Serialize)]
pub struct InserterId(pub(super) u8);

#[cfg_attr(feature = "client", derive(ShowInfo), derive(GetSize))]
#[derive(Debug, Clone, serde::Deserialize, serde::Serialize)]
pub struct BucketedStorageStorageInserterStoreFrontend {
    pub(super) lookup: HashMap<InserterIdentifier, (u32, LargeInserterState)>,
    pub(super) next_tick: NextTick,
}

#[cfg_attr(feature = "client", derive(ShowInfo), derive(GetSize))]
#[derive(Debug, Clone, Default, serde::Deserialize, serde::Serialize)]
pub(super) struct NextTick {
    pub(super) time: u32,
    pub(super) waiting_for_item: Vec<InserterIdentifier>,
    pub(super) waiting_for_item_result: Vec<UpdatingInserter>,
    pub(super) waiting_for_space: Vec<InserterIdentifier>,
    pub(super) waiting_for_space_result: Vec<UpdatingInserter>,
}

#[cfg_attr(feature = "client", derive(ShowInfo), derive(GetSize))]
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, serde::Deserialize, serde::Serialize)]
pub struct InserterIdentifier {
    pub storage: FakeUnionStorage,
    pub belt_id: u32,
    pub belt_pos: u16,
    pub id: InserterId,
}

impl BucketedStorageStorageInserterStoreFrontend {
    pub fn new() -> Self {
        Self {
            lookup: HashMap::default(),
            next_tick: NextTick {
                time: 0,
                waiting_for_item: vec![],
                waiting_for_item_result: vec![],
                waiting_for_space: vec![],
                waiting_for_space_result: vec![],
            },
        }
    }

    #[profiling::function]
    pub fn get_info_batched<'a>(
        &mut self,
        to_find: impl IntoIterator<Item = InserterIdentifier>,
        store: &BucketedStorageStorageInserterStore,
        do_next_tick_storing: bool,
        current_time: u32,
    ) -> HashMap<InserterIdentifier, LargeInserterState> {
        let mut num_to_find = 0;

        assert!(
            self.next_tick.waiting_for_item.len() >= self.next_tick.waiting_for_item_result.len()
        );
        assert!(
            self.next_tick.waiting_for_space.len() >= self.next_tick.waiting_for_space_result.len()
        );

        info!(
            "Frontend requested info for {} Inserters on tick {}",
            num_to_find, current_time
        );

        let mut ret = HashMap::new();

        let mut waiting_for_item = vec![];
        let mut waiting_for_space = vec![];
        let mut moving_out = vec![vec![]; store.list_len()];
        let mut moving_in = vec![vec![]; store.list_len()];

        {
            profiling::scope!("Build Search Lists");
            for to_find in to_find {
                num_to_find += 1;

                match self.lookup.get(&to_find) {
                    Some((old_time, old_state)) => {
                        let possible_states = get_possible_new_states_after_n_ticks(
                            (*old_state).into(),
                            store.movetime,
                            current_time - *old_time,
                        );

                        if let Ok(possible_states) = possible_states {
                            if possible_states.len() == 1 {
                                ret.insert(to_find, possible_states[0]);
                            } else {
                                assert!(possible_states.iter().all_unique());
                                for possible in possible_states {
                                    match possible {
                                        LargeInserterState::WaitingForSourceItems(_) => {
                                            waiting_for_item.push(to_find)
                                        },
                                        LargeInserterState::WaitingForSpaceInDestination(_) => {
                                            waiting_for_space.push(to_find)
                                        },
                                        LargeInserterState::FullAndMovingOut(time) => {
                                            moving_out[usize::from(time) - 1].push(to_find)
                                        },
                                        LargeInserterState::EmptyAndMovingBack(time) => {
                                            moving_in[usize::from(time) - 1].push(to_find)
                                        },
                                    }
                                }
                            }
                        } else {
                            // Search evern - MOVING_IN

                            for moving_in in &mut moving_in {
                                moving_in.push(to_find);
                            }
                        }
                    },
                    None => {
                        // Search everywhere
                        waiting_for_item.push(to_find);
                        waiting_for_space.push(to_find);

                        for moving_out in &mut moving_out {
                            moving_out.push(to_find);
                        }

                        for moving_in in &mut moving_in {
                            moving_in.push(to_find);
                        }
                    },
                }
            }
        }

        info!(
            "Did not have to search for {} out of {} requests",
            ret.len(),
            num_to_find
        );

        let sizes = store.get_list_sizes();

        if self.next_tick.time == current_time {
            // We know only one tick has passed!

            waiting_for_item.retain(|to_find| {
                if self.next_tick.waiting_for_item.contains(to_find) {
                    if let Some(ins) = self.next_tick.waiting_for_item_result.iter().find(|ins| {
                        ins.storage_id == to_find.storage
                            && ins.belt_id == to_find.belt_id
                            && ins.belt_pos == to_find.belt_pos
                            && ins.id == to_find.id
                    }) {
                        // TODO: Do I want to remove this inserter from next_tick result list?
                        ret.insert(
                            *to_find,
                            LargeInserterState::WaitingForSourceItems(ins.current_hand),
                        );
                    } else {
                        let to_remove = moving_out[store.movetime as usize - 1].iter().position(|v| v == to_find).expect("Inserter with last state being WaitingForSourceItems not in moving_out list?");
                        moving_out[store.movetime as usize - 1].swap_remove(to_remove);
                        ret.insert(*to_find, LargeInserterState::FullAndMovingOut(store.movetime));
                    }
                    false
                } else {
                    true
                }
            });

            waiting_for_space.retain(|to_find| {
                if self.next_tick.waiting_for_space.contains(to_find) {
                    if let Some(ins) = self.next_tick.waiting_for_space_result.iter().find(|ins| {
                        ins.storage_id == to_find.storage
                            && ins.belt_id == to_find.belt_id
                            && ins.belt_pos == to_find.belt_pos
                            && ins.id == to_find.id
                    }) {
                        // TODO: Do I want to remove this inserter from next_tick result list?
                        ret.insert(
                            *to_find,
                            LargeInserterState::WaitingForSpaceInDestination(ins.current_hand),
                        );
                    } else {
                        let to_remove = moving_in[store.movetime as usize - 1].iter().position(|v| v == to_find).expect("Inserter with last state being WaitingForSpaceInDestination not in moving_in list?");
                        moving_in[store.movetime as usize - 1].swap_remove(to_remove);
                        ret.insert(*to_find, LargeInserterState::EmptyAndMovingBack(store.movetime));
                    }
                    false
                } else {
                    true
                }
            });
        }

        #[allow(non_snake_case)]
        for (i, _) in sizes.into_iter().enumerate().sorted_by_key(|v| v.0) {
            let MOVING_OUT_END: usize = store.list_len();
            let WATING_FOR_SPACE: usize = MOVING_OUT_END;
            let MOVING_IN: usize = MOVING_OUT_END + 1;
            let MOVING_IN_END: usize = MOVING_OUT_END + 1 + store.list_len();

            let search_list = match i {
                0 => &mut waiting_for_item,
                n if (1..MOVING_OUT_END).contains(&n) => &mut moving_out[n - 1],
                x if x == WATING_FOR_SPACE => &mut waiting_for_space,
                n if (MOVING_IN..MOVING_IN_END).contains(&n) => &mut moving_in[n - MOVING_IN],

                _ => unreachable!(),
            };

            if search_list.is_empty() {
                continue;
            }

            let found = store.find_in(search_list, i);

            {
                profiling::scope!("Remove already found inserters");
                for n in (i + 1)..MOVING_IN_END {
                    let search_list = match n {
                        0 => &mut waiting_for_item,
                        n if (1..MOVING_OUT_END).contains(&n) => &mut moving_out[n - 1],
                        x if x == WATING_FOR_SPACE => &mut waiting_for_space,
                        n if (MOVING_IN..MOVING_IN_END).contains(&n) => {
                            &mut moving_in[n - MOVING_IN]
                        },

                        _ => unreachable!(),
                    };
                    search_list.retain(|v| !found.contains_key(v));
                }
            }

            ret.extend(found.into_iter().map(|(k, inserter)| {
                (
                    k,
                    // TODO: Respect store.current_tick
                    match i {
                        0 => LargeInserterState::WaitingForSourceItems(inserter.current_hand),
                        n if (1..MOVING_OUT_END).contains(&n) => {
                            LargeInserterState::FullAndMovingOut((n - 1).try_into().unwrap())
                        },
                        x if x == WATING_FOR_SPACE => {
                            LargeInserterState::WaitingForSpaceInDestination(inserter.current_hand)
                        },
                        n if (MOVING_IN..MOVING_IN_END).contains(&n) => {
                            LargeInserterState::EmptyAndMovingBack(
                                (n - MOVING_IN).try_into().unwrap(),
                            )
                        },

                        _ => unreachable!(),
                    },
                )
            }));
        }

        let mut waiting_for_item = vec![];
        let mut waiting_for_space = vec![];

        for (k, v) in ret.iter() {
            if matches!(*v, LargeInserterState::WaitingForSourceItems(_)) {
                waiting_for_item.push(*k);
            } else if matches!(*v, LargeInserterState::WaitingForSpaceInDestination(_)) {
                waiting_for_space.push(*k);
            }
        }

        if do_next_tick_storing {
            self.next_tick = NextTick {
                time: current_time + 1,
                waiting_for_item,
                waiting_for_item_result: vec![],
                waiting_for_space,
                waiting_for_space_result: vec![],
            };
        } else {
            self.next_tick = NextTick {
                time: 0,
                waiting_for_item: vec![],
                waiting_for_item_result: vec![],
                waiting_for_space: vec![],
                waiting_for_space_result: vec![],
            };
        }

        assert_eq!(
            ret.len(),
            num_to_find,
            "Expected to find {num_to_find} Inserters, found {}",
            ret.len()
        );

        self.lookup.extend(
            ret.iter()
                .map(|(to_find, state)| (*to_find, (current_time, *state))),
        );

        ret
    }
}

// TODO: Make this faster
#[profiling::function]
pub(super) fn get_possible_new_states_after_n_ticks(
    starting_state: LargeInserterState,
    movetime: u16,
    n: u32,
) -> Result<Vec<LargeInserterState>, ()> {
    const MAX_SEARCH_SIZE: usize = 100;

    let mut current = vec![starting_state];

    for _ in 0..n {
        let this_tick = std::mem::take(&mut current);

        current.extend(
            this_tick
                .into_iter()
                .flat_map(|state| get_possible_new_states(state, movetime))
                .unique(),
        );

        if current.len() > MAX_SEARCH_SIZE {
            return Err(());
        }
    }

    Ok(current)
}

pub(super) fn get_possible_new_states(
    starting_state: LargeInserterState,
    movetime: u16,
) -> Vec<LargeInserterState> {
    match starting_state {
        LargeInserterState::WaitingForSourceItems(_) => {
            iter::once(LargeInserterState::FullAndMovingOut(movetime))
                .chain(iter::once(LargeInserterState::WaitingForSourceItems(0)))
                .collect()
        },
        LargeInserterState::WaitingForSpaceInDestination(_) => {
            iter::once(LargeInserterState::EmptyAndMovingBack(movetime))
                .chain(iter::once(
                    LargeInserterState::WaitingForSpaceInDestination(0),
                ))
                .collect()
        },
        LargeInserterState::FullAndMovingOut(time) => {
            if time > 0 {
                vec![LargeInserterState::FullAndMovingOut(time - 1)]
            } else {
                vec![LargeInserterState::WaitingForSpaceInDestination(0)]
            }
        },
        LargeInserterState::EmptyAndMovingBack(time) => {
            if time > 0 {
                vec![LargeInserterState::EmptyAndMovingBack(time - 1)]
            } else {
                vec![LargeInserterState::WaitingForSourceItems(0)]
            }
        },
    }
}

#[cfg_attr(feature = "client", derive(ShowInfo), derive(GetSize))]
#[derive(Debug, Clone, serde::Deserialize, serde::Serialize)]
pub struct BucketedStorageStorageInserterStore {
    pub dir: Dir,
    pub movetime: u16,

    id_lookup: HashMap<(FakeUnionStorage, u32, u16), u8>,

    current_tick: usize,
    waiting_for_item: Vec<UpdatingInserter>,
    waiting_for_space_in_destination: Vec<UpdatingInserter>,
    full_and_moving_out: Box<[Vec<MovingInserter>]>,
    empty_and_moving_back: Box<[Vec<MovingInserter>]>,
}

impl BucketedStorageStorageInserterStore {
    pub fn new(dir: Dir, movetime: u16) -> Self {
        assert!((movetime as usize) < MAX_MOVE_TIME);

        Self {
            dir,
            movetime,
            id_lookup: HashMap::new(),
            waiting_for_item: vec![],
            full_and_moving_out: vec![vec![]; movetime as usize + 1].into_boxed_slice(),
            waiting_for_space_in_destination: vec![],
            empty_and_moving_back: vec![vec![]; movetime as usize + 1].into_boxed_slice(),
            current_tick: 0,
        }
    }

    pub fn add_inserter(
        &mut self,
        storage: FakeUnionStorage,
        belt_id: u32,
        belt_pos: BeltLenType,
        max_hand_size: ITEMCOUNTTYPE,
    ) -> InserterId {
        let id = self
            .id_lookup
            .entry((storage, belt_id, belt_pos))
            .or_default();

        let new_id = InserterId(*id);

        self.waiting_for_item.push(UpdatingInserter {
            storage_id: storage,
            belt_id,
            belt_pos,
            max_hand_size,
            current_hand: 0,
            id: new_id,
        });

        match (*id).checked_add(1) {
            Some(next_id) => {
                *id = next_id;
            },
            None => todo!(
                "A pair of storages has u8::MAX inserter between them! (Or a lot of ids were lost due to removals)"
            ),
            // None => {
            //     // FIXME FIXME FIXME
            // },
        }

        new_id
    }

    fn list_len(&self) -> usize {
        self.movetime as usize + 1
    }

    pub fn remove_inserter(
        &mut self,
        storage: FakeUnionStorage,
        belt_id: u32,
        belt_pos: BeltLenType,
        id: InserterId,
    ) -> UpdatingInserter {
        // TODO: We do not decrease the id (since it would invalidate all higher ids), which means we "lose" possible ids.
        //       This means after a lot of additions and removals it might be impossible to connect a pair of storages

        let old_id = self
            .id_lookup
            .get_mut(&(storage, belt_id, belt_pos))
            .expect("No ids for (source, dest) were ever given out!");

        // Only decrease if the id is the highest, so we do not invalidate but a simple place -> remove loop does not cause problems
        if *old_id == id.0 + 1 {
            *old_id -= 1;
        }

        // We iterate backwards here, since when setting an inserters movetime, it is likely just been placed, so it is at the end of its vec
        if let Some(idx) = self
            .waiting_for_item
            .iter()
            .enumerate()
            .rev()
            .find(|(_, ins)| {
                ins.storage_id == storage
                    && ins.belt_id == belt_id
                    && ins.belt_pos == belt_pos
                    && ins.id == id
            })
            .map(|(i, _)| i)
        {
            let removed = self.waiting_for_item.swap_remove(idx);
            return removed;
        }

        if let Some(idx) = self
            .waiting_for_space_in_destination
            .iter()
            .enumerate()
            .rev()
            .find(|(_, ins)| {
                ins.storage_id == storage
                    && ins.belt_id == belt_id
                    && ins.belt_pos == belt_pos
                    && ins.id == id
            })
            .map(|(i, _)| i)
        {
            let removed = self.waiting_for_space_in_destination.swap_remove(idx);
            return removed;
        }

        for moving_out in &mut self.full_and_moving_out {
            if let Some(idx) = moving_out
                .iter()
                .enumerate()
                .rev()
                .find(|(_, ins)| {
                    ins.storage_id == storage
                        && ins.belt_id == belt_id
                        && ins.belt_pos == belt_pos
                        && ins.id == id
                })
                .map(|(i, _)| i)
            {
                let removed = moving_out.swap_remove(idx);
                return UpdatingInserter {
                    storage_id: removed.storage_id,
                    belt_id: removed.belt_id,
                    belt_pos: removed.belt_pos,
                    max_hand_size: removed.max_hand_size,
                    current_hand: removed.max_hand_size,
                    id,
                };
            }
        }

        for moving_in in &mut self.empty_and_moving_back {
            if let Some(idx) = moving_in
                .iter()
                .enumerate()
                .rev()
                .find(|(_, ins)| {
                    ins.storage_id == storage
                        && ins.belt_id == belt_id
                        && ins.belt_pos == belt_pos
                        && ins.id == id
                })
                .map(|(i, _)| i)
            {
                let removed = moving_in.swap_remove(idx);
                return UpdatingInserter {
                    storage_id: removed.storage_id,
                    belt_id: removed.belt_id,
                    belt_pos: removed.belt_pos,
                    max_hand_size: removed.max_hand_size,
                    current_hand: removed.max_hand_size,
                    id,
                };
            }
        }

        unreachable!("Tried to remove an inserter that does not exist!");
    }

    pub fn get_num_inserters(&self) -> usize {
        self.waiting_for_item.len()
            + self.waiting_for_space_in_destination.len()
            + self
                .empty_and_moving_back
                .iter()
                .map(|v| v.len())
                .sum::<usize>()
            + self
                .full_and_moving_out
                .iter()
                .map(|v| v.len())
                .sum::<usize>()
    }

    fn handle_waiting_for_item_ins<ItemIdxType: IdxTrait, const DIR: Dir>(
        item_id: usize,
        inserter: &mut UpdatingInserter,
        frontend: &mut BucketedStorageStorageInserterStoreFrontend,
        storages: SingleItemStorages,
        belts: &mut [SmartBelt<ItemIdxType>],
        grid_size: usize,
        current_tick: u32,
        _movetime: u16,
    ) -> bool {
        match DIR {
            Dir::BeltToStorage => {
                let belt = &mut belts[inserter.belt_id as usize];
                let item = belt.remove_item(inserter.belt_pos);
                match item {
                    Some(_) => {
                        inserter.current_hand += 1;
                    },
                    None => {},
                }
            },
            Dir::StorageToBelt => {
                let (_max_insert, old, _) =
                    index_fake_union(Some(item_id), storages, inserter.storage_id, grid_size);

                let to_extract = min(inserter.max_hand_size - inserter.current_hand, *old);

                if to_extract > 0 {
                    *old -= to_extract;

                    inserter.current_hand += to_extract;
                }
            },
        }

        let extract = inserter.current_hand == inserter.max_hand_size;

        if !extract
            && frontend.next_tick.time == current_tick
            && frontend.next_tick.waiting_for_item.iter().any(|v| {
                v.storage == inserter.storage_id
                    && v.belt_id == inserter.belt_id
                    && v.belt_pos == inserter.belt_pos
                    && v.id == inserter.id
            })
        {
            frontend.next_tick.waiting_for_item_result.push(*inserter);
        }

        extract
    }

    fn handle_waiting_for_space_ins<ItemIdxType: IdxTrait, const DIR: Dir>(
        item_id: usize,
        inserter: &mut UpdatingInserter,
        frontend: &mut BucketedStorageStorageInserterStoreFrontend,
        storages: SingleItemStorages,
        belts: &mut [SmartBelt<ItemIdxType>],
        grid_size: usize,
        current_tick: u32,
        _movetime: u16,
    ) -> bool {
        match DIR {
            Dir::BeltToStorage => {
                let (max_insert, old, _) =
                    index_fake_union(Some(item_id), storages, inserter.storage_id, grid_size);

                let to_insert = min(inserter.current_hand, *max_insert - *old);

                if to_insert > 0 {
                    *old += to_insert;

                    inserter.current_hand -= to_insert;
                }
            },
            Dir::StorageToBelt => {
                let belt = &mut belts[inserter.belt_id as usize];
                let item = belt.try_insert_correct_item(inserter.belt_pos);

                if item.is_ok() {
                    inserter.current_hand -= 1;
                }
            },
        }
        let extract = inserter.current_hand == 0;

        if !extract
            && frontend.next_tick.time == current_tick
            && frontend.next_tick.waiting_for_item.iter().any(|v| {
                v.storage == inserter.storage_id
                    && v.belt_id == inserter.belt_id
                    && v.belt_pos == inserter.belt_pos
                    && v.id == inserter.id
            })
        {
            frontend.next_tick.waiting_for_space_result.push(*inserter);
        }

        extract
    }

    pub fn get_load_info(&self) -> (usize, usize, usize, usize) {
        let num_inserter_struct_loads = self.waiting_for_item.len();

        let num_inserter_struct_loads_space = self.waiting_for_space_in_destination.len();

        let get_id = |inserter: &UpdatingInserter| Some(inserter.storage_id);

        let storage_loads: HashMap<FakeUnionStorage, usize> = match self.dir {
            Dir::BeltToStorage => self
                .waiting_for_space_in_destination
                .iter()
                .filter_map(get_id),
            Dir::StorageToBelt => self.waiting_for_item.iter().filter_map(get_id),
        }
        .map(|id| FakeUnionStorage {
            // Every 64 consecutive indices will map to the same cacheline, so will only load a single cacheline
            index: id.index / u32::try_from(64 / std::mem::size_of::<ITEMCOUNTTYPE>()).unwrap(),
            grid_or_static_flag: id.grid_or_static_flag,
            recipe_idx_with_this_item: id.recipe_idx_with_this_item,
        })
        .counts();

        let num_loads: usize = storage_loads.values().copied().sum();
        let num_cacheline_reuses: usize = storage_loads.values().copied().map(|v| v - 1).sum();
        let num_cachelines = storage_loads.len();

        let num_cachelines_for_inserter_struct = (num_inserter_struct_loads
            + num_inserter_struct_loads_space)
            * std::mem::size_of::<UpdatingInserter>()
            / 64;

        (
            num_loads,
            num_cacheline_reuses,
            num_cachelines,
            num_cachelines_for_inserter_struct,
        )
    }

    #[profiling::function]
    pub fn update<ItemIdxType: IdxTrait>(
        &mut self,
        item_id: usize,
        frontend: &mut BucketedStorageStorageInserterStoreFrontend,
        storages: SingleItemStorages,
        belts: &mut [SmartBelt<ItemIdxType>],
        grid_size: usize,
        current_tick: u32,
    ) {
        let old_len: usize = self.get_list_sizes().iter().sum();

        assert!(self.current_tick < self.list_len());

        let len = self.list_len();

        // {
        //     let end = min(
        //         self.waiting_for_item.len(),
        //         // This depends on the cache size of the current cpu
        //         NUM_BYTES_FOR_SORTING / std::mem::size_of::<UpdatingInserter>(),
        //     );
        //     profiling::scope!(
        //         "sort waiting_for_item",
        //         format!("Sorting {} inserters", end)
        //     );
        //     // TODO: Maybe a sorting algorithm that likes fully sorted lists might be better
        //     self.waiting_for_item[0..end].sort_unstable_by(
        //         |UpdatingInserter {
        //              storage_id_in: a,
        //              storage_id_out: _,
        //              max_hand_size: _,
        //              current_hand: _,
        //              remaining_loops: _,
        //              id: _,
        //          },
        //          UpdatingInserter {
        //              storage_id_in: b,
        //              storage_id_out: _,
        //              max_hand_size: _,
        //              current_hand: _,
        //              remaining_loops: _,
        //              id: _,
        //          }| {
        //             a.grid_or_static_flag
        //                 .cmp(&b.grid_or_static_flag)
        //                 .then(
        //                     a.recipe_idx_with_this_item
        //                         .cmp(&b.recipe_idx_with_this_item),
        //                 )
        //                 .then(a.index.cmp(&b.index))
        //         },
        //     );
        // }

        {
            profiling::scope!("Try taking Items from inventories");
            match self.dir {
                Dir::BeltToStorage => {
                    let now_moving = self.waiting_for_item.extract_if(.., |inserter| {
                        Self::handle_waiting_for_item_ins::<ItemIdxType, { Dir::BeltToStorage }>(
                            item_id,
                            inserter,
                            frontend,
                            storages,
                            belts,
                            grid_size,
                            current_tick,
                            self.movetime,
                        )
                    });

                    for ins in now_moving {
                        // FIXME: I am sure there are some off by one errors in here
                        debug_assert_eq!(ins.current_hand, ins.max_hand_size);
                        self.full_and_moving_out
                            [(self.current_tick + usize::from(self.movetime)) % len]
                            .push(MovingInserter {
                                storage_id: ins.storage_id,
                                belt_id: ins.belt_id,
                                belt_pos: ins.belt_pos,
                                max_hand_size: ins.max_hand_size,
                                id: ins.id,
                            });
                    }
                },
                Dir::StorageToBelt => {
                    let now_moving = self.waiting_for_item.extract_if(.., |inserter| {
                        Self::handle_waiting_for_item_ins::<ItemIdxType, { Dir::StorageToBelt }>(
                            item_id,
                            inserter,
                            frontend,
                            storages,
                            belts,
                            grid_size,
                            current_tick,
                            self.movetime,
                        )
                    });

                    for ins in now_moving {
                        // FIXME: I am sure there are some off by one errors in here
                        debug_assert_eq!(ins.current_hand, ins.max_hand_size);
                        self.full_and_moving_out
                            [(self.current_tick + usize::from(self.movetime)) % len]
                            .push(MovingInserter {
                                storage_id: ins.storage_id,
                                belt_id: ins.belt_id,
                                belt_pos: ins.belt_pos,
                                max_hand_size: ins.max_hand_size,
                                id: ins.id,
                            });
                    }
                },
            }
        }

        // {
        //     let end = min(
        //         self.waiting_for_space_in_destination.len(),
        //         // This depends on the cache size of the current cpu
        //         NUM_BYTES_FOR_SORTING / std::mem::size_of::<UpdatingInserter>(),
        //     );
        //     profiling::scope!(
        //         "sort waiting_for_space_in_destination",
        //         format!("Sorting {} inserters", end)
        //     );
        //     // TODO: Maybe a sorting algorithm that likes fully sorted lists might be better
        //     self.waiting_for_space_in_destination[0..end].sort_unstable_by(
        //         |UpdatingInserter {
        //              storage_id_in: _,
        //              storage_id_out: a,
        //              max_hand_size: _,
        //              current_hand: _,
        //              remaining_loops: _,
        //              id: _,
        //          },
        //          UpdatingInserter {
        //              storage_id_in: _,
        //              storage_id_out: b,
        //              max_hand_size: _,
        //              current_hand: _,
        //              remaining_loops: _,
        //              id: _,
        //          }| {
        //             a.grid_or_static_flag
        //                 .cmp(&b.grid_or_static_flag)
        //                 .then(
        //                     a.recipe_idx_with_this_item
        //                         .cmp(&b.recipe_idx_with_this_item),
        //                 )z
        //                 .then(a.index.cmp(&b.index))
        //         },
        //     );
        // }

        {
            profiling::scope!("Try putting Items into inventories");
            match self.dir {
                Dir::BeltToStorage => {
                    let now_moving_back =
                        self.waiting_for_space_in_destination
                            .extract_if(.., |inserter| {
                                Self::handle_waiting_for_space_ins::<
                                    ItemIdxType,
                                    { Dir::BeltToStorage },
                                >(
                                    item_id,
                                    inserter,
                                    frontend,
                                    storages,
                                    belts,
                                    grid_size,
                                    current_tick,
                                    self.movetime,
                                )
                            });

                    for ins in now_moving_back {
                        // FIXME: I am sure there are some off by one errors in here
                        debug_assert_eq!(ins.current_hand, 0);
                        self.empty_and_moving_back
                            [(self.current_tick + usize::from(self.movetime)) % len]
                            .push(MovingInserter {
                                storage_id: ins.storage_id,
                                belt_id: ins.belt_id,
                                belt_pos: ins.belt_pos,
                                max_hand_size: ins.max_hand_size,
                                id: ins.id,
                            });
                    }
                },
                Dir::StorageToBelt => {
                    let now_moving_back =
                        self.waiting_for_space_in_destination
                            .extract_if(.., |inserter| {
                                Self::handle_waiting_for_space_ins::<
                                    ItemIdxType,
                                    { Dir::StorageToBelt },
                                >(
                                    item_id,
                                    inserter,
                                    frontend,
                                    storages,
                                    belts,
                                    grid_size,
                                    current_tick,
                                    self.movetime,
                                )
                            });

                    for ins in now_moving_back {
                        // FIXME: I am sure there are some off by one errors in here
                        debug_assert_eq!(ins.current_hand, 0);
                        self.empty_and_moving_back
                            [(self.current_tick + usize::from(self.movetime)) % len]
                            .push(MovingInserter {
                                storage_id: ins.storage_id,
                                belt_id: ins.belt_id,
                                belt_pos: ins.belt_pos,
                                max_hand_size: ins.max_hand_size,
                                id: ins.id,
                            });
                    }
                },
            }
        }

        {
            profiling::scope!("Advance time moving back");
            // if self.empty_and_moving_back[self.current_tick].len() < self.waiting_for_item.len() {
            self.waiting_for_item
                .extend(
                    self.empty_and_moving_back[self.current_tick]
                        .iter()
                        .map(|ins| UpdatingInserter {
                            storage_id: ins.storage_id,
                            belt_id: ins.belt_id,
                            belt_pos: ins.belt_pos,
                            max_hand_size: ins.max_hand_size,
                            current_hand: 0,
                            id: ins.id,
                        }),
                );
            self.empty_and_moving_back[self.current_tick].clear();
            // } else {
            //     // self.empty_and_moving_back[self.current_tick]
            //     //     .splice(0..0, self.waiting_for_item.drain(..));
            //     self.empty_and_moving_back[self.current_tick]
            //         .extend_from_slice(&self.waiting_for_item);
            //     self.waiting_for_item.clear();
            //     mem::swap(
            //         &mut self.empty_and_moving_back[self.current_tick],
            //         &mut self.waiting_for_item,
            //     );
            // }
        }

        {
            profiling::scope!("Advance time moving out");
            // if self.full_and_moving_out[self.current_tick].len()
            //     < self.waiting_for_space_in_destination.len()
            // {
            self.waiting_for_space_in_destination.extend(
                self.full_and_moving_out[self.current_tick]
                    .iter()
                    .map(|ins| UpdatingInserter {
                        storage_id: ins.storage_id,
                        belt_id: ins.belt_id,
                        belt_pos: ins.belt_pos,
                        max_hand_size: ins.max_hand_size,
                        current_hand: ins.max_hand_size,
                        id: ins.id,
                    }),
            );
            self.full_and_moving_out[self.current_tick].clear();
            // } else {
            //     // self.full_and_moving_out[self.current_tick].splice(0..0, self.waiting_for_space_in_destination.drain(..));
            //     self.full_and_moving_out[self.current_tick]
            //         .extend_from_slice(&self.waiting_for_space_in_destination);
            //     self.waiting_for_space_in_destination.clear();
            //     mem::swap(
            //         &mut self.full_and_moving_out[self.current_tick],
            //         &mut self.waiting_for_space_in_destination,
            //     );
            // }
        }

        self.current_tick = (self.current_tick + 1) % self.list_len();

        assert_eq!(old_len, self.get_list_sizes().iter().sum::<usize>());
    }

    fn get_list_sizes(&self) -> Vec<usize> {
        let mut ret = vec![];

        ret.push(self.waiting_for_item.len());
        ret.extend(self.full_and_moving_out.iter().map(|v| v.len()));
        ret.push(self.waiting_for_space_in_destination.len());
        ret.extend(self.empty_and_moving_back.iter().map(|v| v.len()));

        ret
    }

    #[profiling::function]
    fn find_in(
        &self,
        to_find: &mut Vec<InserterIdentifier>,
        search_in: usize,
    ) -> HashMap<InserterIdentifier, UpdatingInserter> {
        let moving_out_end: usize = self.list_len() + 1;
        let wating_for_space: usize = moving_out_end;
        let moving_in: usize = moving_out_end + 1;
        let moving_in_end: usize = moving_out_end + 1 + self.list_len();

        let mut ret = HashMap::new();

        let search_list: Box<dyn Iterator<Item = UpdatingInserter>> = match search_in {
            0 => Box::new(self.waiting_for_item.iter().copied()),
            n if (1..moving_out_end).contains(&n) => Box::new(
                self.full_and_moving_out[(n - 1 + self.current_tick) % self.list_len()]
                    .iter()
                    .map(|ins| UpdatingInserter {
                        storage_id: ins.storage_id,
                        belt_id: ins.belt_id,
                        belt_pos: ins.belt_pos,
                        max_hand_size: ins.max_hand_size,
                        current_hand: ins.max_hand_size,
                        id: ins.id,
                    }),
            ),
            x if x == wating_for_space => {
                Box::new(self.waiting_for_space_in_destination.iter().copied())
            },
            n if (moving_in..moving_in_end).contains(&n) => Box::new(
                self.empty_and_moving_back[(n - moving_in + self.current_tick) % self.list_len()]
                    .iter()
                    .map(|ins| UpdatingInserter {
                        storage_id: ins.storage_id,
                        belt_id: ins.belt_id,
                        belt_pos: ins.belt_pos,
                        max_hand_size: ins.max_hand_size,
                        current_hand: 0,
                        id: ins.id,
                    }),
            ),

            _ => unreachable!(),
        };

        for inserter in search_list {
            to_find.retain(|to_find| {
                if to_find.storage == inserter.storage_id
                    && to_find.belt_id == inserter.belt_id
                    && to_find.belt_pos == inserter.belt_pos
                    && to_find.id == inserter.id
                {
                    ret.insert(*to_find, inserter);
                    // Remove this search value for every future inserter
                    false
                } else {
                    true
                }
            });

            if to_find.is_empty() {
                return ret;
            }
        }

        ret
    }

    #[profiling::function]
    #[must_use]
    pub fn update_inserter_storage_id(
        &mut self,
        id: InserterIdentifier,
        new_storage: FakeUnionStorage,
    ) -> InserterIdentifier {
        if id.storage == new_storage {
            warn!("Tried to update src to the same id!");
            return id;
        }

        let old_id = self
            .id_lookup
            .get_mut(&(id.storage, id.belt_id, id.belt_pos))
            .expect("No ids for (source, dest) were ever given out!");

        // Only decrease if the id is the highest, so we do not invalidate but a simple place -> remove loop does not cause problems
        if *old_id == id.id.0 + 1 {
            *old_id -= 1;
        }

        if let Some(idx) = self.waiting_for_item.iter().position(|i| {
            i.storage_id == id.storage
                && i.belt_id == id.belt_id
                && i.belt_pos == id.belt_pos
                && i.id == id.id
        }) {
            self.waiting_for_item[idx].storage_id = new_storage;
            let new_id = self
                .id_lookup
                .entry((
                    self.waiting_for_item[idx].storage_id,
                    self.waiting_for_item[idx].belt_id,
                    self.waiting_for_item[idx].belt_pos,
                ))
                .or_insert(0);

            self.waiting_for_item[idx].id = InserterId(*new_id);

            return InserterIdentifier {
                storage: self.waiting_for_item[idx].storage_id,
                belt_id: self.waiting_for_item[idx].belt_id,
                belt_pos: self.waiting_for_item[idx].belt_pos,
                id: self.waiting_for_item[idx].id,
            };
        }

        if let Some(idx) = self.waiting_for_space_in_destination.iter().position(|i| {
            i.storage_id == id.storage
                && i.belt_id == id.belt_id
                && i.belt_pos == id.belt_pos
                && i.id == id.id
        }) {
            self.waiting_for_space_in_destination[idx].storage_id = new_storage;
            let new_id = self
                .id_lookup
                .entry((
                    self.waiting_for_space_in_destination[idx].storage_id,
                    self.waiting_for_space_in_destination[idx].belt_id,
                    self.waiting_for_space_in_destination[idx].belt_pos,
                ))
                .or_insert(0);

            self.waiting_for_space_in_destination[idx].id = InserterId(*new_id);

            return InserterIdentifier {
                storage: self.waiting_for_space_in_destination[idx].storage_id,
                belt_id: self.waiting_for_space_in_destination[idx].belt_id,
                belt_pos: self.waiting_for_space_in_destination[idx].belt_pos,
                id: self.waiting_for_space_in_destination[idx].id,
            };
        }

        for moving_out in &mut self.full_and_moving_out {
            if let Some(idx) = moving_out.iter().position(|i| {
                i.storage_id == id.storage
                    && i.belt_id == id.belt_id
                    && i.belt_pos == id.belt_pos
                    && i.id == id.id
            }) {
                moving_out[idx].storage_id = new_storage;
                let new_id = self
                    .id_lookup
                    .entry((
                        moving_out[idx].storage_id,
                        moving_out[idx].belt_id,
                        moving_out[idx].belt_pos,
                    ))
                    .or_insert(0);

                moving_out[idx].id = InserterId(*new_id);

                return InserterIdentifier {
                    storage: moving_out[idx].storage_id,
                    belt_id: moving_out[idx].belt_id,
                    belt_pos: moving_out[idx].belt_pos,
                    id: moving_out[idx].id,
                };
            }
        }

        for moving_in in &mut self.empty_and_moving_back {
            if let Some(idx) = moving_in.iter().position(|i| {
                i.storage_id == id.storage
                    && i.belt_id == id.belt_id
                    && i.belt_pos == id.belt_pos
                    && i.id == id.id
            }) {
                moving_in[idx].storage_id = new_storage;
                let new_id = self
                    .id_lookup
                    .entry((
                        moving_in[idx].storage_id,
                        moving_in[idx].belt_id,
                        moving_in[idx].belt_pos,
                    ))
                    .or_insert(0);

                moving_in[idx].id = InserterId(*new_id);

                return InserterIdentifier {
                    storage: moving_in[idx].storage_id,
                    belt_id: moving_in[idx].belt_id,
                    belt_pos: moving_in[idx].belt_pos,
                    id: moving_in[idx].id,
                };
            }
        }

        unreachable!("Tried to update_inserter_src an inserter that does not exist!");
    }
}

#[cfg(test)]
mod test {
    const MOVETIME: u16 = 120;
    const NUM_INSERTERS: usize = 2_000_000;
    const NUM_ITEMS: usize = 5;

    const NUM_BELTS: usize = 100_000;
    const LEN_BELTS: usize = 10_000;

    use std::array;

    use ::test::Bencher;
    use itertools::Itertools;
    use rand::{random, seq::SliceRandom};
    use rayon::iter::{IndexedParallelIterator, IntoParallelRefMutIterator, ParallelIterator};

    use crate::{
        belt::smart::SmartBelt,
        inserter::{FakeUnionStorage, belt_storage_inserter::Dir},
        storage_list::{InserterWaitLists, MaxInsertionLimit},
    };

    use super::*;

    #[bench]
    fn bench_update_storage_storage_inserter_store_buckets(b: &mut Bencher) {
        let mut store: [_; NUM_ITEMS] = array::from_fn(|_| {
            (
                BucketedStorageStorageInserterStore::new(Dir::BeltToStorage, MOVETIME),
                BucketedStorageStorageInserterStore::new(Dir::StorageToBelt, MOVETIME),
            )
        });
        let mut frontend: [_; NUM_ITEMS] =
            array::from_fn(|_| BucketedStorageStorageInserterStoreFrontend::new());

        let max_insert = vec![200u8; NUM_INSERTERS];
        let mut storages_in: [_; NUM_ITEMS] = array::from_fn(|_| vec![200u8; NUM_INSERTERS]);
        let mut storages_out: [_; NUM_ITEMS] = array::from_fn(|_| vec![0u8; NUM_INSERTERS]);
        let mut belts: [_; NUM_ITEMS] = array::from_fn(|i| {
            vec![
                SmartBelt::<u8>::new(
                    0,
                    LEN_BELTS as u16,
                    crate::item::Item {
                        id: i.try_into().unwrap()
                    }
                );
                NUM_BELTS
            ]
        });

        let mut current_tick = 0;

        for item in 0..NUM_ITEMS {
            let mut values = (0..(NUM_INSERTERS as u32)).collect_vec();
            let mut belt_ids = (0..(NUM_INSERTERS as u32))
                .map(|v| v % NUM_BELTS as u32)
                .collect_vec();
            values.shuffle(&mut rand::rng());
            belt_ids.shuffle(&mut rand::rng());
            for (storage, belt) in values.into_iter().zip(belt_ids) {
                if random::<u16>() < 1 {
                    store[item].0.update(
                        item,
                        &mut frontend[item],
                        &mut [
                            (
                                MaxInsertionLimit::PerMachine(max_insert.as_slice()),
                                storages_in[item].as_mut_slice(),
                                InserterWaitLists::None,
                            ),
                            (
                                MaxInsertionLimit::PerMachine(max_insert.as_slice()),
                                storages_out[item].as_mut_slice(),
                                InserterWaitLists::None,
                            ),
                        ],
                        &mut belts[item],
                        10,
                        current_tick,
                    );
                    store[item].1.update(
                        item,
                        &mut frontend[item],
                        &mut [
                            (
                                MaxInsertionLimit::PerMachine(max_insert.as_slice()),
                                storages_in[item].as_mut_slice(),
                                InserterWaitLists::None,
                            ),
                            (
                                MaxInsertionLimit::PerMachine(max_insert.as_slice()),
                                storages_out[item].as_mut_slice(),
                                InserterWaitLists::None,
                            ),
                        ],
                        &mut belts[item],
                        10,
                        current_tick,
                    );

                    if storages_in[item][0] < 20 {
                        for v in &mut storages_in[item] {
                            *v = 200u8;
                        }
                        for v in &mut storages_out[item] {
                            *v = 0u8;
                        }
                    }
                }

                if random() {
                    store[item].0.add_inserter(
                        FakeUnionStorage {
                            index: storage,
                            grid_or_static_flag: 0,
                            recipe_idx_with_this_item: 0,
                        },
                        belt,
                        rand::random(),
                        1,
                    );
                } else {
                    store[item].1.add_inserter(
                        FakeUnionStorage {
                            index: storage,
                            grid_or_static_flag: 0,
                            recipe_idx_with_this_item: 0,
                        },
                        belt,
                        rand::random(),
                        1,
                    );
                }
            }
        }

        storages_in = array::from_fn(|_| vec![200u8; NUM_INSERTERS]);
        storages_out = array::from_fn(|_| vec![0u8; NUM_INSERTERS]);

        let mut num_iter = 0;

        b.iter(|| {
            // for _ in 0..10 {
            storages_in
                .par_iter_mut()
                .zip(
                    storages_out.par_iter_mut().zip(
                        store
                            .par_iter_mut()
                            .zip(frontend.par_iter_mut().zip(belts.par_iter_mut())),
                    ),
                )
                .for_each(|(storage_in, (storage_out, (store, (frontend, belts))))| {
                    if storage_in[0] < 20 {
                        for v in storage_in.iter_mut() {
                            *v = 200u8;
                        }
                        for v in storage_out.iter_mut() {
                            *v = 0u8;
                        }
                    }
                    store.0.update(
                        0,
                        frontend,
                        &mut [
                            (
                                MaxInsertionLimit::PerMachine(max_insert.as_slice()),
                                storage_in.as_mut_slice(),
                                InserterWaitLists::None,
                            ),
                            (
                                MaxInsertionLimit::PerMachine(max_insert.as_slice()),
                                storage_out.as_mut_slice(),
                                InserterWaitLists::None,
                            ),
                        ],
                        belts,
                        10,
                        current_tick,
                    );
                    store.1.update(
                        0,
                        frontend,
                        &mut [
                            (
                                MaxInsertionLimit::PerMachine(max_insert.as_slice()),
                                storage_in.as_mut_slice(),
                                InserterWaitLists::None,
                            ),
                            (
                                MaxInsertionLimit::PerMachine(max_insert.as_slice()),
                                storage_out.as_mut_slice(),
                                InserterWaitLists::None,
                            ),
                        ],
                        belts,
                        10,
                        current_tick,
                    );
                });

            // }
            num_iter += 1;
            current_tick += 1;
        });

        dbg!(&storages_in[0][0..10], &storages_out[0][0..10], num_iter);
    }
}
