use itertools::Itertools;
use std::mem;
use std::{array, collections::HashMap};

use super::{FakeUnionStorage, InserterState, HAND_SIZE, MOVETIME};
use crate::assembler::arrays;
use crate::{
    item::ITEMCOUNTTYPE,
    storage_list::{index_fake_union, SingleItemStorages},
};
use log::{info, trace};
use std::{cmp::min, iter};

const MAX_MOVE_TIME: usize = 120;

#[derive(Debug, Clone, Copy, serde::Deserialize, serde::Serialize)]
pub struct Inserter {
    pub storage_id_in: FakeUnionStorage,
    pub storage_id_out: FakeUnionStorage,
    max_hand_size: ITEMCOUNTTYPE,
    current_hand: ITEMCOUNTTYPE,
    id: InserterId,
}

// This means at most u16::MAX inserters connecting any pair of Storages, that seems plenty
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, serde::Deserialize, serde::Serialize)]
pub struct InserterId(u16);

#[derive(Debug, Clone, serde::Deserialize, serde::Serialize)]
pub struct BucketedStorageStorageInserterStoreFrontend {
    lookup: HashMap<InserterIdentifier, (u32, InserterState)>,
    next_tick: NextTick,
}

#[derive(Debug, Clone, serde::Deserialize, serde::Serialize)]
struct NextTick {
    time: u32,
    waiting_for_item: Vec<InserterIdentifier>,
    waiting_for_item_result: Vec<Inserter>,
    waiting_for_space: Vec<InserterIdentifier>,
    waiting_for_space_result: Vec<Inserter>,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, serde::Deserialize, serde::Serialize)]
pub struct InserterIdentifier {
    pub source: FakeUnionStorage,
    pub dest: FakeUnionStorage,
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

    fn get_info_naive(&mut self, id: InserterIdentifier, current_time: u32) -> InserterState {
        match self.lookup.entry(id) {
            std::collections::hash_map::Entry::Occupied(mut occupied_entry) => {
                let (old_time, old_state) = occupied_entry.get();

                let possible_states = get_possible_new_states_after_n_ticks(
                    (*old_state).into(),
                    MOVETIME,
                    HAND_SIZE,
                    current_time - *old_time,
                );

                if let Ok(possible_states) = possible_states {
                    if possible_states.len() == 1 {
                        occupied_entry.insert((current_time, possible_states[0]));
                        possible_states[0]
                    } else {
                        todo!("We need to search in the possible states");
                    }
                } else {
                    todo!("We need to search everywhere");
                }
            },
            std::collections::hash_map::Entry::Vacant(vacant_entry) => {
                todo!("We need to search everywhere")
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
    ) -> HashMap<InserterIdentifier, InserterState> {
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
        let mut moving_out: [_; MAX_MOVE_TIME] = array::from_fn(|_| vec![]);
        let mut moving_in: [_; MAX_MOVE_TIME] = array::from_fn(|_| vec![]);

        {
            profiling::scope!("Build Search Lists");
            for to_find in to_find {
                num_to_find += 1;

                match self.lookup.get(&to_find) {
                    Some((old_time, old_state)) => {
                        let possible_states = get_possible_new_states_after_n_ticks(
                            (*old_state).into(),
                            MOVETIME,
                            HAND_SIZE,
                            current_time - *old_time,
                        );

                        if let Ok(possible_states) = possible_states {
                            if possible_states.len() == 1 {
                                ret.insert(to_find, possible_states[0]);
                            } else {
                                assert!(possible_states.iter().all_unique());
                                for possible in possible_states {
                                    match possible {
                                        InserterState::WaitingForSourceItems(_) => {
                                            waiting_for_item.push(to_find)
                                        },
                                        InserterState::WaitingForSpaceInDestination(_) => {
                                            waiting_for_space.push(to_find)
                                        },
                                        InserterState::FullAndMovingOut(time) => {
                                            moving_out[usize::from(time) - 1].push(to_find)
                                        },
                                        InserterState::EmptyAndMovingBack(time) => {
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
                        ins.storage_id_in == to_find.source
                            && ins.storage_id_out == to_find.dest
                            && ins.id == to_find.id
                    }) {
                        // TODO: Do I want to remove this inserter from next_tick result list?
                        ret.insert(
                            *to_find,
                            InserterState::WaitingForSourceItems(ins.current_hand),
                        );
                    } else {
                        let to_remove = moving_out[MOVETIME as usize - 1].iter().position(|v| v == to_find).expect("Inserter with last state being WaitingForSourceItems not in moving_out list?");
                        moving_out[MOVETIME as usize - 1].swap_remove(to_remove);
                        ret.insert(*to_find, InserterState::FullAndMovingOut(MOVETIME));
                    }
                    false
                } else {
                    true
                }
            });

            waiting_for_space.retain(|to_find| {
                if self.next_tick.waiting_for_space.contains(to_find) {
                    if let Some(ins) = self.next_tick.waiting_for_space_result.iter().find(|ins| {
                        ins.storage_id_in == to_find.source
                            && ins.storage_id_out == to_find.dest
                            && ins.id == to_find.id
                    }) {
                        // TODO: Do I want to remove this inserter from next_tick result list?
                        ret.insert(
                            *to_find,
                            InserterState::WaitingForSpaceInDestination(ins.current_hand),
                        );
                    } else {
                        let to_remove = moving_in[MOVETIME as usize - 1].iter().position(|v| v == to_find).expect("Inserter with last state being WaitingForSpaceInDestination not in moving_in list?");
                        moving_in[MOVETIME as usize - 1].swap_remove(to_remove);
                        ret.insert(*to_find, InserterState::EmptyAndMovingBack(MOVETIME));
                    }
                    false
                } else {
                    true
                }
            });
        }

        for (i, _) in sizes.into_iter().enumerate().sorted_by_key(|v| v.0) {
            const MOVING_OUT_END: usize = MAX_MOVE_TIME + 1;
            const WATING_FOR_SPACE: usize = MOVING_OUT_END;
            const MOVING_IN: usize = MOVING_OUT_END + 1;
            const MOVING_IN_END: usize = MOVING_OUT_END + 1 + MAX_MOVE_TIME;

            let search_list = match i {
                0 => &mut waiting_for_item,
                n @ 1..MOVING_OUT_END => &mut moving_out[n - 1],
                WATING_FOR_SPACE => &mut waiting_for_space,
                n @ MOVING_IN..MOVING_IN_END => &mut moving_in[n - MOVING_IN],

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
                        n @ 1..MOVING_OUT_END => &mut moving_out[n - 1],
                        WATING_FOR_SPACE => &mut waiting_for_space,
                        n @ MOVING_IN..MOVING_IN_END => &mut moving_in[n - MOVING_IN],

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
                        0 => InserterState::WaitingForSourceItems(inserter.current_hand),
                        n @ 1..MOVING_OUT_END => {
                            InserterState::FullAndMovingOut((n - 1).try_into().unwrap())
                        },
                        WATING_FOR_SPACE => {
                            InserterState::WaitingForSpaceInDestination(inserter.current_hand)
                        },
                        n @ MOVING_IN..MOVING_IN_END => {
                            InserterState::EmptyAndMovingBack((n - MOVING_IN).try_into().unwrap())
                        },

                        _ => unreachable!(),
                    },
                )
            }));
        }

        let mut waiting_for_item = vec![];
        let mut waiting_for_space = vec![];

        for (k, v) in ret.iter() {
            let next_possible_states = get_possible_new_states(*v, MOVETIME, HAND_SIZE);

            if matches!(*v, InserterState::WaitingForSourceItems(_)) {
                waiting_for_item.push(*k);
            } else if matches!(*v, InserterState::WaitingForSpaceInDestination(_)) {
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
fn get_possible_new_states_after_n_ticks(
    starting_state: InserterState,
    movetime: u8,
    max_hand_size: ITEMCOUNTTYPE,
    n: u32,
) -> Result<Vec<InserterState>, ()> {
    const MAX_SEARCH_SIZE: usize = 100;

    let mut current = vec![starting_state];

    for _ in 0..n {
        let this_tick = std::mem::take(&mut current);

        current.extend(
            this_tick
                .into_iter()
                .flat_map(|state| get_possible_new_states(state, movetime, max_hand_size))
                .unique(),
        );

        if current.len() > MAX_SEARCH_SIZE {
            return Err(());
        }
    }

    Ok(current)
}

fn get_possible_new_states(
    starting_state: InserterState,
    movetime: u8,
    max_hand_size: ITEMCOUNTTYPE,
) -> Vec<InserterState> {
    match starting_state {
        InserterState::WaitingForSourceItems(_) => {
            iter::once(InserterState::FullAndMovingOut(movetime))
                .chain(iter::once(InserterState::WaitingForSourceItems(0)))
                .collect()
        },
        InserterState::WaitingForSpaceInDestination(_) => {
            iter::once(InserterState::EmptyAndMovingBack(movetime))
                .chain(iter::once(InserterState::WaitingForSpaceInDestination(0)))
                .collect()
        },
        InserterState::FullAndMovingOut(time) => {
            if time > 0 {
                vec![InserterState::FullAndMovingOut(time - 1)]
            } else {
                vec![InserterState::WaitingForSpaceInDestination(0)]
            }
        },
        InserterState::EmptyAndMovingBack(time) => {
            if time > 0 {
                vec![InserterState::EmptyAndMovingBack(time - 1)]
            } else {
                vec![InserterState::WaitingForSourceItems(0)]
            }
        },
    }
}

#[derive(Debug, Clone, serde::Deserialize, serde::Serialize)]
pub struct BucketedStorageStorageInserterStore {
    id_lookup: HashMap<(FakeUnionStorage, FakeUnionStorage), u16>,

    current_tick: usize,
    waiting_for_item: Vec<Inserter>,
    waiting_for_space_in_destination: Vec<Inserter>,
    #[serde(with = "arrays")]
    full_and_moving_out: [Vec<Inserter>; MAX_MOVE_TIME],
    #[serde(with = "arrays")]
    empty_and_moving_back: [Vec<Inserter>; MAX_MOVE_TIME],
}

impl BucketedStorageStorageInserterStore {
    pub fn new() -> Self {
        Self {
            id_lookup: HashMap::new(),
            waiting_for_item: vec![],
            full_and_moving_out: array::from_fn(|_| vec![]),
            waiting_for_space_in_destination: vec![],
            empty_and_moving_back: array::from_fn(|_| vec![]),
            current_tick: 0,
        }
    }

    pub fn add_inserter(
        &mut self,
        source: FakeUnionStorage,
        dest: FakeUnionStorage,
        max_hand_size: ITEMCOUNTTYPE,
    ) -> InserterId {
        let id = self.id_lookup.entry((source, dest)).or_default();

        let new_id = InserterId(*id);

        self.waiting_for_item.push(Inserter {
            storage_id_in: source,
            storage_id_out: dest,
            max_hand_size,
            current_hand: 0,
            id: new_id,
        });

        match (*id).checked_add(1) {
            Some(next_id) => {
                *id = next_id;
            },
            None => todo!("A pair of storages has u16::MAX inserter between them! (Or a lot of ids were lost due to removals)"),
            // None => {
            //     // FIXME FIXME FIXME
            // },
        }

        new_id
    }

    pub fn remove_inserter(
        &mut self,
        source: FakeUnionStorage,
        dest: FakeUnionStorage,
        id: InserterId,
    ) -> Inserter {
        // TODO: We do not decrease the id (since it would invalidate all higher ids), which means we "lose" possible ids.
        //       This means after a lot of additions and removals it might be impossible to connect a pair of storages

        let old_id = self
            .id_lookup
            .get_mut(&(source, dest))
            .expect("No ids for (source, dest) were ever given out!");

        // Only decrease if the id is the highest, so we do not invalidate but a simple place -> remove loop does not cause problems
        if *old_id - 1 == id.0 {
            *old_id -= 1;
        }

        if let Some(idx) = self
            .waiting_for_item
            .iter()
            .position(|i| i.storage_id_in == source && i.storage_id_out == dest && i.id == id)
        {
            let removed = self.waiting_for_item.swap_remove(idx);
            return removed;
        }

        if let Some(idx) = self
            .waiting_for_space_in_destination
            .iter()
            .position(|i| i.storage_id_in == source && i.storage_id_out == dest && i.id == id)
        {
            let removed = self.waiting_for_space_in_destination.swap_remove(idx);
            return removed;
        }

        for moving_out in &mut self.full_and_moving_out {
            if let Some(idx) = moving_out
                .iter()
                .position(|i| i.storage_id_in == source && i.storage_id_out == dest && i.id == id)
            {
                let removed = moving_out.swap_remove(idx);
                return removed;
            }
        }

        for moving_in in &mut self.empty_and_moving_back {
            if let Some(idx) = moving_in
                .iter()
                .position(|i| i.storage_id_in == source && i.storage_id_out == dest && i.id == id)
            {
                let removed = moving_in.swap_remove(idx);
                return removed;
            }
        }

        unreachable!("Tried to remove an inserter that does not exist!");
    }

    #[profiling::function]
    pub fn update(
        &mut self,
        frontend: &mut BucketedStorageStorageInserterStoreFrontend,
        storages: SingleItemStorages,
        grid_size: usize,
        current_tick: u32,
    ) {
        let old_len: usize = self.get_list_sizes().iter().sum();

        assert!(self.current_tick < MAX_MOVE_TIME);

        {
            profiling::scope!("Try taking Items from inventories");
            let now_moving = self.waiting_for_item.extract_if(.., |inserter| {
                let (_max_insert, old) =
                    index_fake_union(storages, inserter.storage_id_in, grid_size);

                let to_extract = min(inserter.max_hand_size - inserter.current_hand, *old);

                *old -= to_extract;

                inserter.current_hand += to_extract;

                let extract = inserter.current_hand == inserter.max_hand_size;

                if !extract
                    && frontend.next_tick.time == current_tick
                    && frontend.next_tick.waiting_for_item.iter().any(|v| {
                        v.source == inserter.storage_id_in
                            && v.dest == inserter.storage_id_out
                            && v.id == inserter.id
                    })
                {
                    frontend.next_tick.waiting_for_item_result.push(*inserter);
                }

                extract
            });

            // TODO: constant movetime
            self.full_and_moving_out[(self.current_tick + MOVETIME as usize) % MAX_MOVE_TIME]
                .extend(now_moving);
        }

        {
            profiling::scope!("Try puttin Items into inventories");
            let now_moving_back =
                self.waiting_for_space_in_destination
                    .extract_if(.., |inserter| {
                        let (max_insert, old) =
                            index_fake_union(storages, inserter.storage_id_out, grid_size);

                        let to_insert = min(inserter.current_hand, *max_insert - *old);

                        *old += to_insert;

                        inserter.current_hand -= to_insert;

                        let extract = inserter.current_hand == 0;

                        if !extract
                            && frontend.next_tick.time == current_tick
                            && frontend.next_tick.waiting_for_space.iter().any(|v| {
                                v.source == inserter.storage_id_in
                                    && v.dest == inserter.storage_id_out
                                    && v.id == inserter.id
                            })
                        {
                            frontend.next_tick.waiting_for_space_result.push(*inserter);
                        }
                        extract
                    });

            // TODO: constant movetime
            self.empty_and_moving_back[(self.current_tick + MOVETIME as usize) % MAX_MOVE_TIME]
                .extend(now_moving_back);
        }

        {
            profiling::scope!("Advance time moving back");
            if dbg!(self.empty_and_moving_back[self.current_tick].len())
                < dbg!(self.waiting_for_item.len())
            {
                self.waiting_for_item
                    .extend_from_slice(&self.empty_and_moving_back[self.current_tick]);
                self.empty_and_moving_back[self.current_tick].clear();
            } else {
                // self.empty_and_moving_back[self.current_tick]
                //     .splice(0..0, self.waiting_for_item.drain(..));
                self.empty_and_moving_back[self.current_tick]
                    .extend_from_slice(&self.waiting_for_item);
                self.waiting_for_item.clear();
                mem::swap(
                    &mut self.empty_and_moving_back[self.current_tick],
                    &mut self.waiting_for_item,
                );
            }
        }

        {
            profiling::scope!("Advance time moving out");
            if self.full_and_moving_out[self.current_tick].len()
                < self.waiting_for_space_in_destination.len()
            {
                self.waiting_for_space_in_destination
                    .extend_from_slice(&self.full_and_moving_out[self.current_tick]);
                self.full_and_moving_out[self.current_tick].clear();
            } else {
                // self.full_and_moving_out[self.current_tick].splice(0..0, self.waiting_for_space_in_destination.drain(..));
                self.full_and_moving_out[self.current_tick]
                    .extend_from_slice(&self.waiting_for_space_in_destination);
                self.waiting_for_space_in_destination.clear();
                mem::swap(
                    &mut self.full_and_moving_out[self.current_tick],
                    &mut self.waiting_for_space_in_destination,
                );
            }
        }

        self.current_tick = (self.current_tick + 1) % MAX_MOVE_TIME;

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
    ) -> HashMap<InserterIdentifier, Inserter> {
        const MOVING_OUT_END: usize = MAX_MOVE_TIME + 1;
        const WATING_FOR_SPACE: usize = MOVING_OUT_END;
        const MOVING_IN: usize = MOVING_OUT_END + 1;
        const MOVING_IN_END: usize = MOVING_OUT_END + 1 + MAX_MOVE_TIME;

        let mut ret = HashMap::new();

        let search_list = match search_in {
            0 => &self.waiting_for_item,
            n @ 1..MOVING_OUT_END => {
                &self.full_and_moving_out[(n - 1 + self.current_tick) % MAX_MOVE_TIME]
            },
            WATING_FOR_SPACE => &self.waiting_for_space_in_destination,
            n @ MOVING_IN..MOVING_IN_END => {
                &self.empty_and_moving_back[(n - MOVING_IN + self.current_tick) % MAX_MOVE_TIME]
            },

            _ => unreachable!(),
        };

        trace!(
            "Having to search {} inserters for rendering",
            search_list.len()
        );

        for inserter in search_list {
            to_find.retain(|to_find| {
                if to_find.source == inserter.storage_id_in
                    && to_find.dest == inserter.storage_id_out
                    && to_find.id == inserter.id
                {
                    ret.insert(*to_find, *inserter);
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
    pub fn update_inserter_src(&mut self, id: InserterIdentifier, new_src: FakeUnionStorage) {
        if let Some(idx) = self.waiting_for_item.iter().position(|i| {
            i.storage_id_in == id.source && i.storage_id_out == id.dest && i.id == id.id
        }) {
            self.waiting_for_item[idx].storage_id_in = new_src;
            return;
        }

        if let Some(idx) = self.waiting_for_space_in_destination.iter().position(|i| {
            i.storage_id_in == id.source && i.storage_id_out == id.dest && i.id == id.id
        }) {
            self.waiting_for_space_in_destination[idx].storage_id_in = new_src;
            return;
        }

        for moving_out in &mut self.full_and_moving_out {
            if let Some(idx) = moving_out.iter().position(|i| {
                i.storage_id_in == id.source && i.storage_id_out == id.dest && i.id == id.id
            }) {
                moving_out[idx].storage_id_in = new_src;
                return;
            }
        }

        for moving_in in &mut self.empty_and_moving_back {
            if let Some(idx) = moving_in.iter().position(|i| {
                i.storage_id_in == id.source && i.storage_id_out == id.dest && i.id == id.id
            }) {
                moving_in[idx].storage_id_in = new_src;
                return;
            }
        }

        unreachable!("Tried to update_inserter_src an inserter that does not exist!");
    }

    #[profiling::function]
    pub fn update_inserter_dest(&mut self, id: InserterIdentifier, new_dest: FakeUnionStorage) {
        if let Some(idx) = self.waiting_for_item.iter().position(|i| {
            i.storage_id_in == id.source && i.storage_id_out == id.dest && i.id == id.id
        }) {
            self.waiting_for_item[idx].storage_id_out = new_dest;
            return;
        }

        if let Some(idx) = self.waiting_for_space_in_destination.iter().position(|i| {
            i.storage_id_in == id.source && i.storage_id_out == id.dest && i.id == id.id
        }) {
            self.waiting_for_space_in_destination[idx].storage_id_out = new_dest;
            return;
        }

        for moving_out in &mut self.full_and_moving_out {
            if let Some(idx) = moving_out.iter().position(|i| {
                i.storage_id_in == id.source && i.storage_id_out == id.dest && i.id == id.id
            }) {
                moving_out[idx].storage_id_out = new_dest;
                return;
            }
        }

        for moving_in in &mut self.empty_and_moving_back {
            if let Some(idx) = moving_in.iter().position(|i| {
                i.storage_id_in == id.source && i.storage_id_out == id.dest && i.id == id.id
            }) {
                moving_in[idx].storage_id_out = new_dest;
                return;
            }
        }

        unreachable!("Tried to update_inserter_dest an inserter that does not exist!");
    }
}

#[cfg(test)]
mod test {
    const NUM_INSERTERS: usize = 2_000_000;
    const NUM_ITEMS: usize = 5;
    const NUM_VISIBLE: usize = 1000;

    use std::array;

    use itertools::Itertools;
    use rand::{random, seq::SliceRandom};
    use rayon::iter::{IndexedParallelIterator, IntoParallelRefMutIterator, ParallelIterator};
    use test::{black_box, Bencher};

    use crate::inserter::{
        storage_storage_with_buckets::{
            BucketedStorageStorageInserterStoreFrontend, InserterId, InserterIdentifier,
        },
        FakeUnionStorage,
    };

    use super::BucketedStorageStorageInserterStore;

    #[bench]
    fn bench_update_storage_storage_inserter_store_buckets(b: &mut Bencher) {
        let mut store: [_; NUM_ITEMS] =
            array::from_fn(|_| BucketedStorageStorageInserterStore::new());
        let mut frontend: [_; NUM_ITEMS] =
            array::from_fn(|_| BucketedStorageStorageInserterStoreFrontend::new());

        let max_insert = vec![200u8; NUM_INSERTERS];
        let mut storages_in: [_; NUM_ITEMS] = array::from_fn(|_| vec![200u8; NUM_INSERTERS]);
        let mut storages_out: [_; NUM_ITEMS] = array::from_fn(|_| vec![0u8; NUM_INSERTERS]);

        let mut current_tick = 0;

        for item in 0..NUM_ITEMS {
            let mut values = (0..(NUM_INSERTERS as u32)).collect_vec();
            values.shuffle(&mut rand::thread_rng());
            for i in values {
                if random::<u16>() < 1 {
                    store[item].update(
                        &mut frontend[item],
                        &mut [
                            (max_insert.as_slice(), storages_in[item].as_mut_slice()),
                            (max_insert.as_slice(), storages_out[item].as_mut_slice()),
                        ],
                        10,
                        current_tick,
                    );

                    if storages_in[item][0] < 20 {
                        storages_in[item] = vec![200u8; NUM_INSERTERS];
                        storages_out[item] = vec![0u8; NUM_INSERTERS];
                    }
                }

                store[item].add_inserter(
                    FakeUnionStorage {
                        index: i,
                        grid_or_static_flag: 0,
                        recipe_idx_with_this_item: 0,
                    },
                    FakeUnionStorage {
                        index: i,
                        grid_or_static_flag: 0,
                        recipe_idx_with_this_item: 1,
                    },
                    1,
                );
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
                    storages_out
                        .par_iter_mut()
                        .zip(store.par_iter_mut().zip(frontend.par_iter_mut())),
                )
                .for_each(|(storage_in, (storage_out, (store, frontend)))| {
                    if storage_in[0] < 20 {
                        *storage_in = vec![200u8; NUM_INSERTERS];
                        *storage_out = vec![0u8; NUM_INSERTERS];
                    }
                    store.update(
                        frontend,
                        &mut [
                            (max_insert.as_slice(), storage_in.as_mut_slice()),
                            (max_insert.as_slice(), storage_out.as_mut_slice()),
                        ],
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

    #[bench]
    fn bench_storage_storage_inserter_store_find_batched_with_next_tick_optimization(
        b: &mut Bencher,
    ) {
        let mut store = BucketedStorageStorageInserterStore::new();

        let mut frontend = BucketedStorageStorageInserterStoreFrontend::new();

        let max_insert = vec![200u8; NUM_INSERTERS];
        let mut storages_in = vec![200u8; NUM_INSERTERS];
        let mut storages_out = vec![0u8; NUM_INSERTERS];

        let mut values = (0..(NUM_INSERTERS as u32)).collect_vec();
        values.shuffle(&mut rand::thread_rng());

        let mut current_time: u32 = 0;

        for i in values {
            if random::<u16>() < 10 {
                store.update(
                    &mut frontend,
                    &mut [
                        (max_insert.as_slice(), storages_in.as_mut_slice()),
                        (max_insert.as_slice(), storages_out.as_mut_slice()),
                    ],
                    10,
                    current_time,
                );

                if storages_in[0] < 20 {
                    storages_in = vec![200u8; NUM_INSERTERS];
                    storages_out = vec![0u8; NUM_INSERTERS];
                }
            }

            let id = store.add_inserter(
                FakeUnionStorage {
                    index: i,
                    grid_or_static_flag: 0,
                    recipe_idx_with_this_item: 0,
                },
                FakeUnionStorage {
                    index: i,
                    grid_or_static_flag: 0,
                    recipe_idx_with_this_item: 1,
                },
                1,
            );

            assert_eq!(id, InserterId(0));
        }

        storages_in = vec![200u8; NUM_INSERTERS];
        storages_out = vec![0u8; NUM_INSERTERS];

        let to_find: Vec<u32> = (0..(NUM_VISIBLE as u32)).collect();

        // let to_find = vec![0u16];

        let to_find: Vec<_> = to_find
            .into_iter()
            .map(|i| InserterIdentifier {
                source: FakeUnionStorage {
                    index: i.into(),
                    grid_or_static_flag: 0,
                    recipe_idx_with_this_item: 0,
                },
                dest: FakeUnionStorage {
                    index: i.into(),
                    grid_or_static_flag: 0,
                    recipe_idx_with_this_item: 1,
                },
                id: InserterId(0),
            })
            .collect();

        b.iter(|| {
            let ret =
                frontend.get_info_batched(to_find.iter().copied(), &store, true, current_time);

            if storages_in[0] < 20 {
                storages_in = vec![200u8; NUM_INSERTERS];
                storages_out = vec![0u8; NUM_INSERTERS];
            }
            store.update(
                &mut frontend,
                &mut [
                    (max_insert.as_slice(), storages_in.as_mut_slice()),
                    (max_insert.as_slice(), storages_out.as_mut_slice()),
                ],
                10,
                current_time,
            );

            current_time += 1;
        });

        dbg!(&storages_in[0..10], &storages_out[0..10], current_time);
    }

    #[bench]
    fn bench_storage_storage_inserter_store_find_batched_without_next_tick_optimization(
        b: &mut Bencher,
    ) {
        let mut store = BucketedStorageStorageInserterStore::new();

        let mut frontend = BucketedStorageStorageInserterStoreFrontend::new();

        let max_insert = vec![200u8; NUM_INSERTERS];
        let mut storages_in = vec![200u8; NUM_INSERTERS];
        let mut storages_out = vec![0u8; NUM_INSERTERS];

        let mut values = (0..(NUM_INSERTERS as u32)).collect_vec();
        values.shuffle(&mut rand::thread_rng());

        let mut current_time: u32 = 0;

        for i in values {
            if random::<u16>() < 10 {
                store.update(
                    &mut frontend,
                    &mut [
                        (max_insert.as_slice(), storages_in.as_mut_slice()),
                        (max_insert.as_slice(), storages_out.as_mut_slice()),
                    ],
                    10,
                    current_time,
                );

                if storages_in[0] < 20 {
                    storages_in = vec![200u8; NUM_INSERTERS];
                    storages_out = vec![0u8; NUM_INSERTERS];
                }
            }

            let id = store.add_inserter(
                FakeUnionStorage {
                    index: i,
                    grid_or_static_flag: 0,
                    recipe_idx_with_this_item: 0,
                },
                FakeUnionStorage {
                    index: i,
                    grid_or_static_flag: 0,
                    recipe_idx_with_this_item: 1,
                },
                1,
            );

            assert_eq!(id, InserterId(0));
        }

        storages_in = vec![200u8; NUM_INSERTERS];
        storages_out = vec![0u8; NUM_INSERTERS];

        let to_find: Vec<u32> = (0..(NUM_VISIBLE as u32)).collect();

        // let to_find = vec![0u16];

        let to_find: Vec<_> = to_find
            .into_iter()
            .map(|i| InserterIdentifier {
                source: FakeUnionStorage {
                    index: i.into(),
                    grid_or_static_flag: 0,
                    recipe_idx_with_this_item: 0,
                },
                dest: FakeUnionStorage {
                    index: i.into(),
                    grid_or_static_flag: 0,
                    recipe_idx_with_this_item: 1,
                },
                id: InserterId(0),
            })
            .collect();

        b.iter(|| {
            let ret =
                frontend.get_info_batched(to_find.iter().copied(), &store, false, current_time);

            if storages_in[0] < 20 {
                storages_in = vec![200u8; NUM_INSERTERS];
                storages_out = vec![0u8; NUM_INSERTERS];
            }
            store.update(
                &mut frontend,
                &mut [
                    (max_insert.as_slice(), storages_in.as_mut_slice()),
                    (max_insert.as_slice(), storages_out.as_mut_slice()),
                ],
                10,
                current_time,
            );

            current_time += 1;
        });

        dbg!(&storages_in[0..10], &storages_out[0..10], current_time);
    }

    const LEN: usize = 10_000_000;

    #[bench]
    fn bench_extract_if_low_extraction_rate(b: &mut Bencher) {
        let mut source = vec![0; LEN];
        let mut dest = vec![];

        let mut num_iterations = 0;
        b.iter(|| {
            let extracted = source.extract_if(.., |_| rand::random::<u8>() < 20);

            dest.extend(extracted);

            source.resize(LEN, 0);
            // dest.clear();

            num_iterations += 1;
        });

        dbg!(source.len());
        dbg!(dest.len());

        dbg!(num_iterations);
    }

    #[bench]
    fn bench_extract_if_high_extraction_rate(b: &mut Bencher) {
        let mut source = vec![0; LEN];
        let mut dest = vec![];

        let mut num_iterations = 0;
        b.iter(|| {
            let extracted = source.extract_if(.., |_| rand::random::<u8>() >= 20);

            dest.extend(extracted);

            source.resize(LEN, 0);

            num_iterations += 1;
        });

        dbg!(source.len());
        dbg!(dest.len());

        dbg!(num_iterations);
    }
}
