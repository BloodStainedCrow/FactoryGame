use rayon::iter::{IntoParallelRefMutIterator, ParallelIterator};
use std::{u16, u32};

use super::{
    FakeUnionStorage, InserterStateInfo, storage_storage_with_buckets::LargeInserterState,
};
use crate::{
    item::ITEMCOUNTTYPE,
    join_many::join,
    storage_list::{SingleItemStorages, index_fake_union},
};
use std::cmp::min;

#[cfg(feature = "client")]
use egui_show_info_derive::ShowInfo;
#[cfg(feature = "client")]
use get_size::GetSize;

const PERFORMANCE_CHEAT_INSERTER_UPDATE_MODULE: usize = 1;

#[allow(unused)]
struct IdealInserter {
    pub storage_id_in: u32,
    pub storage_id_out: u32,
    pub last_update_time: u16,
    state: u8,
    pub max_hand_size: ITEMCOUNTTYPE,
}

#[cfg_attr(feature = "client", derive(ShowInfo), derive(GetSize))]
#[derive(Debug, Clone, Copy, serde::Deserialize, serde::Serialize)]
pub struct InserterState {
    pub last_update_time: u16,
    state: ImplicitState,
}

#[cfg_attr(feature = "client", derive(ShowInfo), derive(GetSize))]
#[derive(Debug, Clone, Copy, serde::Deserialize, serde::Serialize)]
pub struct Inserter {
    pub storage_id_in: FakeUnionStorage,
    pub storage_id_out: FakeUnionStorage,
    pub last_update_time: u16,
    pub max_hand_size: ITEMCOUNTTYPE,
    state: ImplicitState,
}

#[cfg_attr(feature = "client", derive(ShowInfo), derive(GetSize))]
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, serde::Deserialize, serde::Serialize)]
pub enum ImplicitState {
    WaitingForSourceItems(ITEMCOUNTTYPE),
    WaitingForSpaceInDestination(ITEMCOUNTTYPE),
    FullAndMovingOut,
    EmptyAndMovingBack,
}

#[cfg_attr(feature = "client", derive(ShowInfo), derive(GetSize))]
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, serde::Deserialize, serde::Serialize)]
pub struct InserterIdentifier {
    id: InserterId,
}

#[cfg_attr(feature = "client", derive(ShowInfo), derive(GetSize))]
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, serde::Deserialize, serde::Serialize)]
struct InserterId {
    index: u32,
}

#[cfg_attr(feature = "client", derive(ShowInfo), derive(GetSize))]
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, serde::Deserialize, serde::Serialize)]
struct InserterBucketData {
    pub storage_id_in: FakeUnionStorage,
    pub storage_id_out: FakeUnionStorage,
    index: InserterId,
    current_hand: ITEMCOUNTTYPE,
    max_hand_size: ITEMCOUNTTYPE,
}

#[cfg_attr(feature = "client", derive(ShowInfo), derive(GetSize))]
#[derive(Debug, Clone, serde::Deserialize, serde::Serialize)]
pub struct BucketedStorageStorageInserterStore {
    pub movetime: u16,

    holes: Vec<u32>,
    inserters: Vec<InserterState>,

    current_tick: usize,
    waiting_for_item: Vec<InserterBucketData>,
    waiting_for_space_in_destination: Vec<InserterBucketData>,
    full_and_moving_out: Box<[Vec<InserterBucketData>]>,
    empty_and_moving_back: Box<[Vec<InserterBucketData>]>,
}

const PLACEHOLDER: InserterState = InserterState {
    last_update_time: 0,
    state: ImplicitState::WaitingForSourceItems(0),
};

impl BucketedStorageStorageInserterStore {
    pub fn new(movetime: u16) -> Self {
        Self {
            movetime,
            holes: vec![],
            inserters: vec![],

            waiting_for_item: vec![],
            full_and_moving_out: vec![vec![]; movetime as usize + 1].into_boxed_slice(),
            waiting_for_space_in_destination: vec![],
            empty_and_moving_back: vec![vec![]; movetime as usize + 1].into_boxed_slice(),
            current_tick: 0,
        }
    }

    pub fn add_inserter(
        &mut self,
        source: FakeUnionStorage,
        dest: FakeUnionStorage,
        max_hand_size: ITEMCOUNTTYPE,
    ) -> InserterIdentifier {
        let new_id = if let Some(hole) = self.holes.pop() {
            self.inserters[hole as usize] = InserterState {
                state: ImplicitState::WaitingForSourceItems(0),
                last_update_time: 0,
            };

            InserterId { index: hole }
        } else {
            self.inserters.push(InserterState {
                state: ImplicitState::WaitingForSourceItems(0),
                last_update_time: 0,
            });

            InserterId {
                index: (self.inserters.len() - 1) as u32,
            }
        };

        self.waiting_for_item.push(InserterBucketData {
            storage_id_in: source,
            storage_id_out: dest,
            index: new_id,
            max_hand_size,
            current_hand: 0,
        });

        InserterIdentifier { id: new_id }
    }

    fn list_len(&self) -> usize {
        self.movetime as usize + 1
    }

    pub fn remove_inserter(&mut self, id: InserterIdentifier) -> Inserter {
        let state = self.inserters[id.id.index as usize];

        let ret = |storage_id_in, storage_id_out, max_hand_size| Inserter {
            storage_id_in,
            storage_id_out,
            last_update_time: state.last_update_time,
            max_hand_size,
            state: state.state,
        };

        self.inserters[id.id.index as usize] = PLACEHOLDER;

        if let Some(pos) = self
            .waiting_for_item
            .iter()
            .position(|ins| id.id == ins.index)
        {
            let removed = self.waiting_for_item.remove(pos);

            return ret(
                removed.storage_id_in,
                removed.storage_id_out,
                removed.max_hand_size,
            );
        }

        if let Some(pos) = self
            .waiting_for_space_in_destination
            .iter()
            .position(|ins| id.id == ins.index)
        {
            let removed = self.waiting_for_space_in_destination.remove(pos);

            return ret(
                removed.storage_id_in,
                removed.storage_id_out,
                removed.max_hand_size,
            );
        }

        for full in &mut self.full_and_moving_out {
            if let Some(pos) = full.iter().position(|ins| id.id == ins.index) {
                let removed = full.remove(pos);

                return ret(
                    removed.storage_id_in,
                    removed.storage_id_out,
                    removed.max_hand_size,
                );
            }
        }

        for empty in &mut self.empty_and_moving_back {
            if let Some(pos) = empty.iter().position(|ins| id.id == ins.index) {
                let removed = empty.remove(pos);

                return ret(
                    removed.storage_id_in,
                    removed.storage_id_out,
                    removed.max_hand_size,
                );
            }
        }

        unreachable!()
    }

    pub fn remove_inserters_with_connection(&mut self, conn: FakeUnionStorage) {
        join!(
            || {
                self.waiting_for_item
                    .retain(|v| v.storage_id_in != conn && v.storage_id_out != conn);
            },
            || {
                self.waiting_for_space_in_destination
                    .retain(|v| v.storage_id_in != conn && v.storage_id_out != conn);
            },
            || {
                self.full_and_moving_out.par_iter_mut().for_each(|full| {
                    full.retain(|v| v.storage_id_in != conn && v.storage_id_out != conn);
                });
            },
            || {
                self.empty_and_moving_back.par_iter_mut().for_each(|empty| {
                    empty.retain(|v| v.storage_id_in != conn && v.storage_id_out != conn);
                });
            }
        );

        // let removed_list = self
        //     .inserters
        //     .iter_mut()
        //     .enumerate()
        //     .filter_map(move |(idx, ins)| {
        //         if ins.storage_id_in == conn || ins.storage_id_out == conn {
        //             let ret = *ins;
        //             *ins = PLACEHOLDER;
        //             Some((
        //                 InserterIdentifier {
        //                     id: InserterId { index: idx as u32 },
        //                 },
        //                 Inserter {
        //                     storage_id_in: todo!(),
        //                     storage_id_out: todo!(),
        //                     last_update_time: ret.last_update_time,
        //                     max_hand_size: ret.max_hand_size,
        //                     state: ret.state,
        //                 },
        //             ))
        //         } else {
        //             None
        //         }
        //     });

        // removed_list
    }

    pub fn get_num_inserters(&self) -> usize {
        self.inserters.len() - self.holes.len()
    }

    fn handle_waiting_for_item_ins(
        inserter: &mut InserterState,
        bucket_data: &mut InserterBucketData,

        storages: SingleItemStorages,
        grid_size: usize,
        current_tick: u32,
        _movetime: u16,
    ) -> bool {
        let storage_id = bucket_data.storage_id_in;

        let (_max_insert, old) = index_fake_union(storages, storage_id, grid_size);

        let old_val = *old;
        let max_hand_size = bucket_data.max_hand_size;
        let current_hand_val = bucket_data.current_hand;

        let to_extract = min(max_hand_size - current_hand_val, old_val);

        if to_extract > 0 {
            *old -= to_extract;

            bucket_data.current_hand += to_extract;
            inserter.state = ImplicitState::WaitingForSourceItems(bucket_data.current_hand);
        }

        let extract = bucket_data.current_hand == max_hand_size;

        if extract {
            inserter.state = ImplicitState::FullAndMovingOut;
            // Only use the lower 2 bytes
            inserter.last_update_time = current_tick as u16;
        }

        extract
    }

    fn handle_waiting_for_space_ins(
        inserter: &mut InserterState,
        bucket_data: &mut InserterBucketData,

        storages: SingleItemStorages,
        grid_size: usize,
        current_tick: u32,
        _movetime: u16,
    ) -> bool {
        let storage_id = bucket_data.storage_id_out;

        let (max_insert, old) = index_fake_union(storages, storage_id, grid_size);

        let old_val = *old;
        let max_insert = *max_insert;

        let to_insert = min(bucket_data.current_hand, max_insert - old_val);

        if to_insert > 0 {
            *old += to_insert;

            bucket_data.current_hand -= to_insert;
            inserter.state = ImplicitState::WaitingForSpaceInDestination(bucket_data.current_hand);
        }

        let extract = bucket_data.current_hand == 0;

        if extract {
            inserter.state = ImplicitState::EmptyAndMovingBack;
            // Only use the lower 2 bytes
            inserter.last_update_time = current_tick as u16;
        }

        extract
    }

    pub fn get_load_info(&self) -> (usize, usize, usize, usize) {
        //     let num_inserter_struct_loads = self.waiting_for_item.len();

        //     let num_inserter_struct_loads_space = self.waiting_for_space_in_destination.len();

        //     let storage_loads: HashMap<FakeUnionStorage, usize> = self
        //         .waiting_for_item
        //         .iter()
        //         .filter_map(|inserter| Some(inserter.storage_id_in))
        //         .chain(
        //             self.waiting_for_space_in_destination
        //                 .iter()
        //                 .filter_map(|inserter| Some(inserter.storage_id_out)),
        //         )
        //         .map(|id| FakeUnionStorage {
        //             // Every 64 consecutive indices will map to the same cacheline, so will only load a single cacheline
        //             index: id.index / u32::try_from(64 / std::mem::size_of::<ITEMCOUNTTYPE>()).unwrap(),
        //             grid_or_static_flag: id.grid_or_static_flag,
        //             recipe_idx_with_this_item: id.recipe_idx_with_this_item,
        //         })
        //         .counts();

        //     let num_loads: usize = storage_loads.values().copied().sum();
        //     let num_cacheline_reuses: usize = storage_loads.values().copied().map(|v| v - 1).sum();
        //     let num_cachelines = storage_loads.len();

        //     let num_cachelines_for_inserter_struct = (num_inserter_struct_loads
        //         + num_inserter_struct_loads_space)
        //         * std::mem::size_of::<UpdatingInserter>()
        //         / 64;

        //     (
        //         num_loads,
        //         num_cacheline_reuses,
        //         num_cachelines,
        //         num_cachelines_for_inserter_struct,
        //     )
        todo!()
    }

    #[profiling::function]
    pub fn update(
        &mut self,
        item_id: usize,
        storages: SingleItemStorages,
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
            let cheat_idx = current_tick as usize % PERFORMANCE_CHEAT_INSERTER_UPDATE_MODULE;

            let chunk_size = self
                .waiting_for_item
                .len()
                .div_ceil(PERFORMANCE_CHEAT_INSERTER_UPDATE_MODULE);

            let start = chunk_size * cheat_idx;
            let start = min(start, self.waiting_for_item.len());
            let end = chunk_size * (cheat_idx + 1);
            let end = min(end, self.waiting_for_item.len());

            profiling::scope!(
                "Try taking Items from inventories",
                format!("count: {}", self.waiting_for_item.len())
            );
            let now_moving = self.waiting_for_item.extract_if(start..end, |inserter| {
                Self::handle_waiting_for_item_ins(
                    &mut self.inserters[inserter.index.index as usize],
                    inserter,
                    storages,
                    grid_size,
                    current_tick,
                    self.movetime,
                )
            });

            self.full_and_moving_out[(self.current_tick + usize::from(self.movetime)) % len]
                .extend(now_moving);
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
        //                 )
        //                 .then(a.index.cmp(&b.index))
        //         },
        //     );
        // }

        {
            let cheat_idx = current_tick as usize % PERFORMANCE_CHEAT_INSERTER_UPDATE_MODULE;

            let chunk_size = self
                .waiting_for_space_in_destination
                .len()
                .div_ceil(PERFORMANCE_CHEAT_INSERTER_UPDATE_MODULE);

            let start = chunk_size * cheat_idx;
            let start = min(start, self.waiting_for_space_in_destination.len());
            let end = chunk_size * (cheat_idx + 1);
            let end = min(end, self.waiting_for_space_in_destination.len());

            profiling::scope!(
                "Try putting Items into inventories",
                format!("count: {}", self.waiting_for_space_in_destination.len())
            );
            let now_moving_back =
                self.waiting_for_space_in_destination
                    .extract_if(start..end, |inserter| {
                        Self::handle_waiting_for_space_ins(
                            &mut self.inserters[inserter.index.index as usize],
                            inserter,
                            storages,
                            grid_size,
                            current_tick,
                            self.movetime,
                        )
                    });

            self.empty_and_moving_back[(self.current_tick + usize::from(self.movetime)) % len]
                .extend(now_moving_back);
        }

        {
            profiling::scope!("Advance time moving back");
            // if self.empty_and_moving_back[self.current_tick].len() < self.waiting_for_item.len() {
            self.waiting_for_item.extend(
                self.empty_and_moving_back[self.current_tick]
                    .drain(..)
                    .map(|ins| {
                        self.inserters[ins.index.index as usize].state =
                            ImplicitState::WaitingForSourceItems(0);
                        // TODO: This is technically not needed
                        // Only use the lower 2 bytes
                        self.inserters[ins.index.index as usize].last_update_time =
                            current_tick as u16;
                        ins
                    }),
            );
            // self.empty_and_moving_back[self.current_tick].clear();
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
                    .drain(..)
                    .map(|ins| {
                        self.inserters[ins.index.index as usize].state =
                            ImplicitState::WaitingForSpaceInDestination(ins.max_hand_size);
                        // TODO: This is technically not needed
                        // Only use the lower 2 bytes
                        self.inserters[ins.index.index as usize].last_update_time =
                            current_tick as u16;
                        ins
                    }),
            );
            // self.full_and_moving_out[self.current_tick].clear();
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

    pub fn get_inserter(&self, id: InserterIdentifier, current_tick: u32) -> InserterStateInfo {
        let ins = &self.inserters[id.id.index as usize];

        /// This value is only correct if the timer between current_tick and the old tick is less than u16::MAX
        fn first_tick_value_with_this_lower_part(lower: u16, current_tick: u32) -> u32 {
            let current_lower = current_tick as u16;
            // Only take the upper 2 bytes
            let current_upper: u32 = current_tick >> 16;

            // We know that the original tick which produced lower must be lower than current_tick:
            let ret = if current_lower < lower {
                // This means we wrapped
                let old_upper = (current_upper - 1) << 16;

                old_upper | lower as u32
            } else {
                let old_upper = current_upper << 16;

                old_upper | lower as u32
            };

            assert!(ret <= current_tick);

            ret
        }

        InserterStateInfo {
            state: match ins.state {
                ImplicitState::WaitingForSourceItems(count) => {
                    LargeInserterState::WaitingForSourceItems(count)
                },
                ImplicitState::WaitingForSpaceInDestination(count) => {
                    LargeInserterState::WaitingForSpaceInDestination(count)
                },
                ImplicitState::FullAndMovingOut => LargeInserterState::FullAndMovingOut({
                    let time_passed = current_tick.strict_sub(
                        first_tick_value_with_this_lower_part(ins.last_update_time, current_tick),
                    );

                    u16::try_from(u32::from(self.movetime).strict_sub(time_passed)).expect(
                        &format!(
                            "Inserter has been moving for more than u16::MAX ticks: {}",
                            u32::from(self.movetime).strict_sub(current_tick.strict_sub(
                                first_tick_value_with_this_lower_part(
                                    ins.last_update_time,
                                    current_tick
                                )
                            ))
                        ),
                    )
                }),
                ImplicitState::EmptyAndMovingBack => LargeInserterState::EmptyAndMovingBack(
                    u16::try_from(u32::from(self.movetime).strict_sub(current_tick.strict_sub(
                        first_tick_value_with_this_lower_part(ins.last_update_time, current_tick),
                    )))
                    .expect(&format!(
                        "Inserter has been moving for more than u16::MAX ticks: {}",
                        u32::from(self.movetime).strict_sub(current_tick.strict_sub(
                            first_tick_value_with_this_lower_part(
                                ins.last_update_time,
                                current_tick
                            )
                        ))
                    )),
                ),
            },
        }
    }

    #[profiling::function]
    #[must_use]
    pub fn update_inserter_src(
        &mut self,
        id: InserterIdentifier,
        new_src: FakeUnionStorage,
    ) -> InserterIdentifier {
        match self.inserters[id.id.index as usize].state {
            ImplicitState::WaitingForSourceItems(_) => {
                for ins in &mut self.waiting_for_item {
                    if ins.index == id.id {
                        ins.storage_id_in = new_src;
                        return InserterIdentifier { id: id.id };
                    }
                }
            },
            ImplicitState::WaitingForSpaceInDestination(_) => {
                for ins in &mut self.waiting_for_space_in_destination {
                    if ins.index == id.id {
                        ins.storage_id_in = new_src;
                        return InserterIdentifier { id: id.id };
                    }
                }
            },
            ImplicitState::FullAndMovingOut => {
                // TODO: use current tick to get the bucket to search
                for bucket in &mut self.full_and_moving_out {
                    for ins in bucket {
                        if ins.index == id.id {
                            ins.storage_id_in = new_src;
                            return InserterIdentifier { id: id.id };
                        }
                    }
                }
            },
            ImplicitState::EmptyAndMovingBack => {
                // TODO: use current tick to get the bucket to search
                for bucket in &mut self.empty_and_moving_back {
                    for ins in bucket {
                        if ins.index == id.id {
                            ins.storage_id_in = new_src;
                            return InserterIdentifier { id: id.id };
                        }
                    }
                }
            },
        }

        unreachable!()
    }

    #[profiling::function]
    #[must_use]
    pub fn update_inserter_dest(
        &mut self,
        id: InserterIdentifier,
        new_dest: FakeUnionStorage,
    ) -> InserterIdentifier {
        match self.inserters[id.id.index as usize].state {
            ImplicitState::WaitingForSourceItems(_) => {
                for ins in &mut self.waiting_for_item {
                    if ins.index == id.id {
                        ins.storage_id_out = new_dest;
                        return InserterIdentifier { id: id.id };
                    }
                }
            },
            ImplicitState::WaitingForSpaceInDestination(_) => {
                for ins in &mut self.waiting_for_space_in_destination {
                    if ins.index == id.id {
                        ins.storage_id_out = new_dest;
                        return InserterIdentifier { id: id.id };
                    }
                }
            },
            ImplicitState::FullAndMovingOut => {
                // TODO: use current tick to get the bucket to search
                for bucket in &mut self.full_and_moving_out {
                    for ins in bucket {
                        if ins.index == id.id {
                            ins.storage_id_out = new_dest;
                            return InserterIdentifier { id: id.id };
                        }
                    }
                }
            },
            ImplicitState::EmptyAndMovingBack => {
                // TODO: use current tick to get the bucket to search
                for bucket in &mut self.empty_and_moving_back {
                    for ins in bucket {
                        if ins.index == id.id {
                            ins.storage_id_out = new_dest;
                            return InserterIdentifier { id: id.id };
                        }
                    }
                }
            },
        }

        unreachable!()
    }
}

#[cfg(test)]
mod test {
    const MOVETIME: u16 = 120;
    const NUM_INSERTERS: usize = 20_000_000;
    const NUM_ITEMS: usize = 5;

    use std::array;

    use ::test::Bencher;
    use itertools::Itertools;
    use rand::{random, seq::SliceRandom};
    use rayon::iter::{IndexedParallelIterator, IntoParallelRefMutIterator, ParallelIterator};

    use super::*;

    #[bench]
    fn bench_update_storage_storage_inserter_store_buckets(b: &mut Bencher) {
        let mut store: [_; NUM_ITEMS] =
            array::from_fn(|_| BucketedStorageStorageInserterStore::new(MOVETIME));

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
                        0,
                        &mut [
                            (max_insert.as_slice(), storages_in[item].as_mut_slice()),
                            (max_insert.as_slice(), storages_out[item].as_mut_slice()),
                        ],
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
                .zip(storages_out.par_iter_mut().zip(store.par_iter_mut()))
                .for_each(|(storage_in, (storage_out, store))| {
                    if storage_in[0] < 20 {
                        for v in storage_in.iter_mut() {
                            *v = 200u8;
                        }
                        for v in storage_out.iter_mut() {
                            *v = 0u8;
                        }
                    }
                    store.update(
                        0,
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
}
