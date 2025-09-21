use std::{cmp::max, cmp::min, collections::BTreeMap, marker::PhantomData};

// I will render bots as "particles" with a fixed size instanced gpu buffer
// This is fine as long as we do not override bots, which are still flying
const GOAL_ITEMS_PER_MINUTE: usize = 1_000_000_000;
const GOAL_ITEMS_PER_TICK: usize = GOAL_ITEMS_PER_MINUTE / 60 / 60;
const BOT_HAND_SIZE: usize = 4;
const BOT_SPEED: usize = 120 * 1000 / 60 / 60; // tiles / s
const MAX_BASE_SIZE: usize = 3000;
const BOT_BATTERY_LIFE_KJ: usize = 1500;
const BOT_KJ_PER_TILE: usize = 5;
const BOT_KJ_PER_SEC: usize = 3;
const BOT_FLIGHT_COST_PER_SEC: usize = BOT_SPEED * BOT_KJ_PER_TILE + BOT_KJ_PER_SEC;
const BOT_BATTERY_LIFE_TICKS: usize = 1500 * 60 / BOT_FLIGHT_COST_PER_SEC;
const AVG_TRIP_LENGTH: usize = 600;
const MAX_BOT_TRAVEL_TIME_TICKS: usize = 2 * BOT_BATTERY_LIFE_TICKS;
const AVG_BOT_TRAVEL_TIME_TICKS: usize = AVG_TRIP_LENGTH * 60 / BOT_SPEED;
const AVG_NEW_BOTS_PER_TICK: usize = GOAL_ITEMS_PER_TICK / (BOT_HAND_SIZE);
const REQUIRED_BOTS: usize = GOAL_ITEMS_PER_TICK * AVG_BOT_TRAVEL_TIME_TICKS / BOT_HAND_SIZE;
const BOT_UPDATE_COUNT_CPU_PER_TICK: usize = REQUIRED_BOTS / BOT_BATTERY_LIFE_TICKS;
const REQUIRED_DRAW_SLOTS: usize = MAX_BOT_TRAVEL_TIME_TICKS * AVG_NEW_BOTS_PER_TICK;
const REQUIRED_MEMORY_SEND_TO_GPU_PER_TICK: usize = AVG_NEW_BOTS_PER_TICK * (2 * 3 * (2 + 1)) * 4;
const REQUIRED_PCI_BANDWIDTH_MBS: usize = REQUIRED_MEMORY_SEND_TO_GPU_PER_TICK * 60 / 1_000_000;
const REQUIRED_VRAM: usize = REQUIRED_DRAW_SLOTS * (2 * 3 * (2 + 1));
const REQUIRED_VRAM_MB: usize = REQUIRED_VRAM / 1_000_000;

use log::info;

use crate::{
    app_state::SimulationState,
    frontend::world::{Position, tile::World},
    item::{ITEMCOUNTTYPE, IdxTrait, Item, WeakIdxTrait, usize_from},
    network_graph::{Network, WeakIndex},
    power::{Joule, Watt},
};

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, PartialOrd, Ord)]
struct RoboportId {
    index: u32,
}

enum NetworkEntity<ItemIdxType: WeakIdxTrait> {
    Requester {
        item: Item<ItemIdxType>,
        index: usize,
    },
    Provider {
        item: Item<ItemIdxType>,
        index: usize,
    },
    Storage,
}

struct BotSystem<ItemIdxType: WeakIdxTrait, RecipeIdxType: WeakIdxTrait> {
    networks: Vec<BotNetwork<ItemIdxType, RecipeIdxType>>,
}

struct BotNetwork<ItemIdxType: WeakIdxTrait, RecipeIdxType: WeakIdxTrait> {
    // TODO: This might need another graph to handle removal of roboports
    current_tick: u32,
    roboports: Vec<Roboport>,
    network: Network<RoboportId, (), NetworkEntity<ItemIdxType>>,
    bot_jobs: BTreeMap<u32, Vec<BotUpdate<ItemIdxType>>>,

    providers: Box<[MultiChestInfo]>,
    requesters: Box<[MultiRequesterInfo]>,
    storages: Box<[MultiStorageInfo]>,

    ty: PhantomData<RecipeIdxType>,
}

enum BotRenderInfo {
    StraightLine {
        sprite: u8,
        start_time: f32,
        end_time: f32,
        start_pos: (f32, f32),
        end_pos: (f32, f32),
    },
    VShape {
        sprite: u8,
        start_time: f32,
        mid_time: f32,
        end_time: f32,
        start_pos: (f32, f32),
        mid_pos: (f32, f32),
        end_pos: (f32, f32),
    },
    WaitThenVShape {
        sprite: u8,
        wait_start_time: f32,
        start_time: f32,
        mid_time: f32,
        end_time: f32,
        start_pos: (f32, f32),
        mid_pos: (f32, f32),
        end_pos: (f32, f32),
    },
}

impl BotRenderInfo {
    fn prepend_movement(self, new_start_pos: (f32, f32), time: f32) -> Self {
        match self {
            BotRenderInfo::StraightLine {
                sprite,
                start_time,
                end_time,
                start_pos,
                end_pos,
            } => Self::VShape {
                sprite,
                start_time: start_time - time,
                mid_time: start_time,
                end_time,
                start_pos: new_start_pos,
                mid_pos: start_pos,
                end_pos,
            },
            BotRenderInfo::VShape { .. } => unimplemented!("We currently cannot handle Z shapes"),
            BotRenderInfo::WaitThenVShape { .. } => {
                unimplemented!("We currently cannot handle Z shapes")
            },
        }
    }

    fn prepend_waiting(self, time: f32) -> Self {
        match self {
            BotRenderInfo::StraightLine {
                sprite,
                start_time,
                end_time,
                start_pos,
                end_pos,
            } => Self::VShape {
                sprite,
                start_time: start_time - time,
                mid_time: start_time,
                end_time,
                start_pos,
                mid_pos: start_pos,
                end_pos,
            },
            BotRenderInfo::VShape {
                sprite,
                start_time,
                mid_time,
                end_time,
                start_pos,
                mid_pos,
                end_pos,
            } => Self::WaitThenVShape {
                sprite,
                wait_start_time: start_time - time,
                start_time,
                mid_time,
                end_time,
                start_pos,
                mid_pos,
                end_pos,
            },
            BotRenderInfo::WaitThenVShape {
                sprite,
                wait_start_time,
                start_time,
                mid_time,
                end_time,
                start_pos,
                mid_pos,
                end_pos,
            } => BotRenderInfo::WaitThenVShape {
                sprite,
                wait_start_time: wait_start_time - time,
                start_time,
                mid_time,
                end_time,
                start_pos,
                mid_pos,
                end_pos,
            },
        }
    }
}

struct MultiChestInfo {
    in_out: Vec<ITEMCOUNTTYPE>,
    storages: Vec<u16>,
    max_items: Vec<u16>,
    holes: Vec<usize>,
}

impl MultiChestInfo {
    pub fn new() -> Self {
        Self {
            in_out: vec![],
            storages: vec![],
            max_items: vec![],
            holes: vec![],
        }
    }

    pub fn add_chest(&mut self, items: u16, max_items: u16) -> usize {
        assert!(items <= max_items);

        let free_spots = max_items - items;

        let inout = min(
            ITEMCOUNTTYPE::try_from(items).unwrap_or(ITEMCOUNTTYPE::MAX),
            max(
                ITEMCOUNTTYPE::MAX / 2,
                ITEMCOUNTTYPE::MAX
                    .saturating_sub(free_spots.try_into().unwrap_or(ITEMCOUNTTYPE::MAX)),
            ),
        );

        let storage = items.saturating_sub(inout.into());

        assert_eq!(items, inout as u16 + storage);

        if let Some(hole) = self.holes.pop() {
            self.in_out[hole] = inout;

            self.max_items[hole] = max_items;
            self.storages[hole] = storage;

            hole
        } else {
            self.in_out.push(inout);
            self.max_items.push(max_items);
            self.storages.push(storage);

            self.storages.len()
        }
    }

    pub fn remove_chest(&mut self, index: usize) -> u16 {
        self.holes.push(index);
        let items = self.storages[index] + self.in_out[index] as u16;

        self.storages[index] = 0;
        self.max_items[index] = 0;
        self.in_out[index] = 0;

        items
    }
}

struct MultiRequesterInfo {
    in_out: Vec<ITEMCOUNTTYPE>,
    storages: Vec<u16>,
    requests: Vec<u16>,
    max_items: Vec<u16>,
    holes: Vec<usize>,
}

impl MultiRequesterInfo {
    pub fn new() -> Self {
        Self {
            in_out: vec![],
            storages: vec![],
            requests: vec![],
            max_items: vec![],
            holes: vec![],
        }
    }

    pub fn add_chest(&mut self, items: u16, request: u16, max_items: u16) -> usize {
        assert!(items <= max_items);
        assert!(request <= max_items);

        let free_spots = max_items - items;

        let inout = min(
            ITEMCOUNTTYPE::try_from(items).unwrap_or(ITEMCOUNTTYPE::MAX),
            max(
                ITEMCOUNTTYPE::MAX / 2,
                ITEMCOUNTTYPE::MAX
                    .saturating_sub(free_spots.try_into().unwrap_or(ITEMCOUNTTYPE::MAX)),
            ),
        );

        let storage = items.saturating_sub(inout.into());

        assert_eq!(items, inout as u16 + storage);

        if let Some(hole) = self.holes.pop() {
            self.in_out[hole] = inout;

            self.max_items[hole] = max_items;
            self.storages[hole] = storage;
            self.requests[hole] = request;

            hole
        } else {
            self.in_out.push(inout);
            self.max_items.push(max_items);
            self.storages.push(storage);
            self.requests.push(request);

            self.storages.len()
        }
    }

    pub fn remove_chest(&mut self, index: usize) -> u16 {
        self.holes.push(index);
        let items = self.storages[index] + self.in_out[index] as u16;

        self.storages[index] = 0;
        self.max_items[index] = 0;
        self.in_out[index] = 0;

        items
    }
}

struct MultiStorageInfo {}

enum BotUpdate<ItemIdxType: WeakIdxTrait> {
    DoGoal(BotGoal<ItemIdxType>),
    // TODO: To allow removing of roboports I will need generational indices!
    TryStartChargingAndContinue(u32, (f32, f32), BotGoal<ItemIdxType>),
    TryStartChargingAndEnter(u32, Joule, (f32, f32)),
    Enter(u32),
}

#[derive(Debug)]
enum BotGoal<ItemIdxType: WeakIdxTrait> {
    TryDepositItem {
        current_charge: Joule,
        item: Item<ItemIdxType>,
        amount: ITEMCOUNTTYPE,
        dest_id: u16,
    },
    TryRetrieveItem {
        current_charge: Joule,
        item: Item<ItemIdxType>,
        amount: ITEMCOUNTTYPE,
        source_id: u16,
        dest_id: u16,
    },
}

// TODO: Make this dynamic
const MAX_ROBOPORT_CHARGE: Joule = Joule(2_000_000);
const MAX_ROBOPORT_CHARGE_RATE: Watt = Watt(2_000_000);

const BOT_CHARGING_RATE: Watt = Watt(500_000);
const BOT_MAX_CHARGE: Joule = Joule(3_000_000);

const LOGIBOT_SPEED_CHARGED: f32 = 3.0 / 60.0;
const LOGIBOT_SPEED_EMPTY: f32 = LOGIBOT_SPEED_CHARGED / 5.0;

const LOGIBOT_POWER_CONSUMPTION: Watt = Watt(63_750);

struct Roboport {
    pos: Position,
    current_charge: Joule,
    construction_bots_idle: u8,
    logibots_idle: u8,
}

struct ConstructionBotWorldUpdate {}

const BOT_ENTER_TIME: u32 = 20;

// TODO: Maybe this should be dynamic
static BOT_RETRY_CHARGE_TIME: u32 =
    (MAX_ROBOPORT_CHARGE.0 / MAX_ROBOPORT_CHARGE_RATE.joules_per_tick().0) as u32;

enum DepositError {
    NoSpaceWait,
    NoSpaceDump,
    StorageRemoved,
}

enum WithdrawError {
    NoItemsWait,
    NoItemsCancel,
    StorageRemoved,
}

impl<ItemIdxType: IdxTrait, RecipeIdxType: IdxTrait> BotNetwork<ItemIdxType, RecipeIdxType> {
    fn path_result_keep_going_when_empty(
        current_bot_pos: (f32, f32),
        current_bot_charge: Joule,
        goal: (f32, f32),
        bot_movement_per_tick_charged: f32,
        bot_movement_per_tick_empty: f32,
        bot_power_consumption: Watt,
    ) -> (f32, Joule) {
        let path_len = ((goal.0 - current_bot_pos.0) * (goal.0 - current_bot_pos.0)
            + (goal.1 - current_bot_pos.1) * (goal.1 - current_bot_pos.1))
            .sqrt();
        let path_time_assuming_infinite_power = path_len / bot_movement_per_tick_charged;

        let out_of_power_time =
            (current_bot_charge / bot_power_consumption.joules_per_tick()) as f32;

        if out_of_power_time > path_time_assuming_infinite_power {
            (
                path_time_assuming_infinite_power,
                current_bot_charge
                    - bot_power_consumption.joules_per_tick()
                        * (path_time_assuming_infinite_power as u64),
            )
        } else {
            (
                out_of_power_time
                    + (path_time_assuming_infinite_power - out_of_power_time)
                        * (bot_movement_per_tick_charged / bot_movement_per_tick_empty),
                Joule(0),
            )
        }
    }

    fn path_result(
        current_bot_pos: (f32, f32),
        current_bot_charge: Joule,
        bot_movement_per_tick: f32,
        bot_power_consumption: Watt,
        goal: (f32, f32),
    ) -> (f32, Result<Joule, (f32, f32)>) {
        let path_len = ((goal.0 - current_bot_pos.0) * (goal.0 - current_bot_pos.0)
            + (goal.1 - current_bot_pos.1) * (goal.1 - current_bot_pos.1))
            .sqrt();
        let path_time_assuming_infinite_power = path_len / bot_movement_per_tick;

        let out_of_power_time =
            (current_bot_charge / bot_power_consumption.joules_per_tick()) as f32;

        if out_of_power_time > path_time_assuming_infinite_power {
            (
                path_time_assuming_infinite_power,
                Ok(current_bot_charge
                    - bot_power_consumption.joules_per_tick()
                        * (path_time_assuming_infinite_power as u64)),
            )
        } else {
            (
                out_of_power_time,
                Err((
                    current_bot_pos.0
                        + (goal.0 - current_bot_pos.0)
                            * (path_time_assuming_infinite_power / out_of_power_time),
                    current_bot_pos.1
                        + (goal.1 - current_bot_pos.1)
                            * (path_time_assuming_infinite_power / out_of_power_time),
                )),
            )
        }
    }

    fn go_deposit(
        &self,
        current_bot_pos: (f32, f32),
        current_bot_charge: Joule,
        bot_movement_per_tick_charged: f32,
        bot_movement_per_tick_empty: f32,
        bot_power_consumption: Watt,
        item: Item<ItemIdxType>,
        amount: ITEMCOUNTTYPE,
        dest_id: u16,
    ) -> ((u32, BotUpdate<ItemIdxType>), BotRenderInfo) {
        let storage_pos = self.get_storage_pos(item, dest_id);
        self.go_and_possibly_recharge(
            current_bot_pos,
            current_bot_charge,
            bot_movement_per_tick_charged,
            bot_movement_per_tick_empty,
            bot_power_consumption,
            storage_pos,
            |charge| BotGoal::TryDepositItem {
                current_charge: charge,
                item,
                amount,
                dest_id,
            },
        )
    }

    fn go_withdraw(
        &self,
        current_bot_pos: (f32, f32),
        current_bot_charge: Joule,
        bot_movement_per_tick_charged: f32,
        bot_movement_per_tick_empty: f32,
        bot_power_consumption: Watt,
        item: Item<ItemIdxType>,
        amount: ITEMCOUNTTYPE,
        source_id: u16,
        dest_id: u16,
    ) -> ((u32, BotUpdate<ItemIdxType>), BotRenderInfo) {
        let storage_pos = self.get_storage_pos(item, source_id);
        self.go_and_possibly_recharge(
            current_bot_pos,
            current_bot_charge,
            bot_movement_per_tick_charged,
            bot_movement_per_tick_empty,
            bot_power_consumption,
            storage_pos,
            |charge| BotGoal::TryRetrieveItem {
                current_charge: charge,
                item,
                amount,
                source_id,
                dest_id,
            },
        )
    }

    fn go_and_possibly_recharge(
        &self,
        current_bot_pos: (f32, f32),
        current_bot_charge: Joule,
        bot_movement_per_tick_charged: f32,
        bot_movement_per_tick_empty: f32,
        bot_power_consumption: Watt,
        goal: (f32, f32),
        goal_fn: impl Fn(Joule) -> BotGoal<ItemIdxType>,
    ) -> ((u32, BotUpdate<ItemIdxType>), BotRenderInfo) {
        match Self::path_result(
            current_bot_pos,
            current_bot_charge,
            bot_movement_per_tick_charged,
            bot_power_consumption,
            goal,
        ) {
            (arrival_time, Ok(rest_charge)) => (
                (arrival_time as u32, BotUpdate::DoGoal(goal_fn(rest_charge))),
                BotRenderInfo::StraightLine {
                    sprite: 0,
                    start_time: self.current_tick as f32,
                    end_time: self.current_tick as f32 + arrival_time,
                    start_pos: current_bot_pos,
                    end_pos: goal,
                },
            ),
            (out_of_power_time, Err(out_of_power_position)) => {
                let recharge_roboport = self.get_closest_roboport(out_of_power_position);
                // TODO: I might want the bots to charge not perfectly on the spot, but some distance away
                let recharge_position = self.get_roboport_position(recharge_roboport);

                let (arrival_time_at_roboport_from_out_of_power, Joule(0)) =
                    Self::path_result_keep_going_when_empty(
                        out_of_power_position,
                        Joule(0),
                        recharge_position,
                        bot_movement_per_tick_charged,
                        bot_movement_per_tick_empty,
                        bot_power_consumption,
                    )
                else {
                    unreachable!(
                        "We started with an empty charge, we should not have gained charge"
                    )
                };

                (
                    (
                        (out_of_power_time + arrival_time_at_roboport_from_out_of_power) as u32,
                        BotUpdate::TryStartChargingAndContinue(
                            recharge_roboport,
                            recharge_position,
                            goal_fn(BOT_MAX_CHARGE),
                        ),
                    ),
                    BotRenderInfo::VShape {
                        sprite: 0,
                        start_time: self.current_tick as f32,
                        mid_time: self.current_tick as f32 + out_of_power_time,
                        end_time: self.current_tick as f32
                            + out_of_power_time
                            + arrival_time_at_roboport_from_out_of_power,
                        start_pos: current_bot_pos,
                        mid_pos: out_of_power_position,
                        end_pos: recharge_position,
                    },
                )
            },
        }
    }

    // TODO: This should return a result and error if the storage was removed (But still return the position)
    fn get_storage_pos(&self, item: Item<ItemIdxType>, id: u16) -> (f32, f32) {
        todo!()
    }

    // TODO: Handle Logibot types
    fn get_closest_roboport_with_free_slot_and_reserve_it(
        &mut self,
        pos: (f32, f32),
    ) -> Option<(u32, (f32, f32))> {
        todo!()
    }

    // TODO: Handle Logibot types
    fn get_closest_roboport_with_idle_bot_and_take_it(
        &mut self,
        pos: (f32, f32),
    ) -> Option<(u32, (f32, f32))> {
        todo!()
    }

    // This is effectively a lookup into a voronoi texture!
    // Except it is not, since this would lead to all robots flying to the same roboport :/
    fn get_closest_roboport(&self, pos: (f32, f32)) -> u32 {
        todo!()
    }

    fn get_roboport_position(&self, roboport_id: u32) -> (f32, f32) {
        let Position { x, y } = self.roboports[roboport_id as usize].pos;

        (x as f32, y as f32)
    }

    fn try_deposit_item(
        &mut self,
        item: Item<ItemIdxType>,
        id: u16,
        amount: ITEMCOUNTTYPE,
        sim_state: &mut SimulationState<ItemIdxType, RecipeIdxType>,
    ) -> Result<(), DepositError> {
        todo!()
    }

    fn try_withdraw(
        &mut self,
        item: Item<ItemIdxType>,
        id: u16,
        amount: ITEMCOUNTTYPE,
        sim_state: &mut SimulationState<ItemIdxType, RecipeIdxType>,
    ) -> Result<(), WithdrawError> {
        todo!()
    }

    fn try_find_storage_to_get_rid_of_and_reserve(
        &mut self,
        item: Item<ItemIdxType>,
        count: ITEMCOUNTTYPE,
    ) -> Result<u16, ()> {
        todo!()
    }

    fn get_logibot_jobs(
        &mut self,
    ) -> impl Iterator<Item = (Item<ItemIdxType>, ITEMCOUNTTYPE, u16, u16)>
    + use<ItemIdxType, RecipeIdxType> {
        vec![todo!()].into_iter()
    }

    fn add_requester_chest(
        &mut self,
        roboport_id: RoboportId,
        item: Item<ItemIdxType>,
        num_items: u16,
        requested_items: u16,
        max_items: u16,
    ) -> WeakIndex {
        let requester_idx =
            self.requesters[usize_from(item.id)].add_chest(num_items, requested_items, max_items);

        let weak_idx = self.network.add_weak_element(
            roboport_id,
            NetworkEntity::Requester {
                item,
                index: requester_idx,
            },
        );

        weak_idx
    }

    // TODO: In order to ensure we never add/remove from a removed chest that was replaced in the meantime,
    // only add removed slots after the last currently queued timer has elapsed.
    // FIXME: This does not work for repeating jobs unless we check if the destination still exists when repushing.
    // Instead we could use a "ref-counting" solution, and keep track of how many jobs target any requester_chest/provider or maybe even roboports
    fn remove_requester_chest(
        &mut self,
        roboport_id: RoboportId,
        weak_idx: WeakIndex,
    ) -> (Item<ItemIdxType>, u16) {
        let entity = self.network.remove_weak_element(roboport_id, weak_idx);

        let NetworkEntity::Provider { item, index } = entity else {
            unreachable!()
        };

        let item_count = self.providers[usize_from(item.id)].remove_chest(index);

        (item, item_count)
    }

    fn add_provider_chest(
        &mut self,
        roboport_id: RoboportId,
        item: Item<ItemIdxType>,
        num_items: u16,
        max_items: u16,
    ) -> WeakIndex {
        let provider_idx = self.providers[usize_from(item.id)].add_chest(num_items, max_items);

        let weak_idx = self.network.add_weak_element(
            roboport_id,
            NetworkEntity::Provider {
                item,
                index: provider_idx,
            },
        );

        weak_idx
    }

    fn remove_provider_chest(
        &mut self,
        roboport_id: RoboportId,
        weak_idx: WeakIndex,
    ) -> (Item<ItemIdxType>, u16) {
        let entity = self.network.remove_weak_element(roboport_id, weak_idx);

        let NetworkEntity::Provider { item, index } = entity else {
            unreachable!()
        };

        let item_count = self.providers[usize_from(item.id)].remove_chest(index);

        (item, item_count)
    }

    fn update_logibots(
        &mut self,
        sim_state: &mut SimulationState<ItemIdxType, RecipeIdxType>,
    ) -> impl Iterator<Item = BotRenderInfo> + use<ItemIdxType, RecipeIdxType> {
        let mut render_infos = vec![];

        // Handle logibots, which are now done with their job
        let done = self.bot_jobs.remove(&self.current_tick).unwrap_or(vec![]);

        debug_assert!(self.bot_jobs.get(&(self.current_tick - 1)).is_none());

        for update in done {
            match update {
                BotUpdate::DoGoal(BotGoal::TryDepositItem {
                    current_charge,
                    item,
                    amount,
                    dest_id,
                }) => {
                    let current_bot_pos = self.get_storage_pos(item, dest_id);
                    match self.try_deposit_item(item, dest_id, amount, sim_state) {
                        Ok(()) => {
                            match self
                                .get_closest_roboport_with_free_slot_and_reserve_it(current_bot_pos)
                            {
                                Some((roboport_id, robotport_pos)) => {
                                    let (arrival_time, remaining_charge) =
                                        Self::path_result_keep_going_when_empty(
                                            current_bot_pos,
                                            current_charge,
                                            robotport_pos,
                                            LOGIBOT_SPEED_CHARGED,
                                            LOGIBOT_SPEED_EMPTY,
                                            LOGIBOT_POWER_CONSUMPTION,
                                        );

                                    self.bot_jobs
                                        .entry(self.current_tick + arrival_time as u32)
                                        .or_default()
                                        .push(BotUpdate::TryStartChargingAndEnter(
                                            roboport_id,
                                            remaining_charge,
                                            // TODO: I might want the bots to charge not perfectly on the spot, but some distance away
                                            robotport_pos,
                                        ));
                                },
                                None => todo!("Handle not having any free roboports!"),
                            }
                        },
                        Err(DepositError::NoSpaceWait) => {
                            todo!("Reinsert the job and add bot render of it staying in place.");
                        },
                        Err(DepositError::NoSpaceDump) | Err(DepositError::StorageRemoved) => {
                            match self.try_find_storage_to_get_rid_of_and_reserve(item, amount) {
                                Ok(id) => {
                                    let ((time, update), render) = self.go_deposit(
                                        current_bot_pos,
                                        current_charge,
                                        LOGIBOT_SPEED_CHARGED,
                                        LOGIBOT_SPEED_EMPTY,
                                        LOGIBOT_POWER_CONSUMPTION,
                                        item,
                                        amount,
                                        id,
                                    );

                                    self.bot_jobs
                                        .entry(self.current_tick + time)
                                        .or_default()
                                        .push(update);
                                    render_infos.push(render);
                                },
                                Err(()) => todo!(
                                    "Handle not having anywhere to dump items (i.e.) all storage chests are full"
                                ),
                            }
                        },
                    }
                },
                BotUpdate::DoGoal(BotGoal::TryRetrieveItem {
                    current_charge,
                    item,
                    amount,
                    source_id,
                    dest_id,
                }) => {
                    let current_bot_pos = self.get_storage_pos(item, dest_id);
                    let dest_pos = self.get_storage_pos(item, dest_id);
                    match self.try_withdraw(item, source_id, amount, sim_state) {
                        Ok(()) => {
                            match Self::path_result(
                                current_bot_pos,
                                current_charge,
                                LOGIBOT_SPEED_CHARGED,
                                LOGIBOT_POWER_CONSUMPTION,
                                dest_pos,
                            ) {
                                (arrival_time, Ok(rest_charge)) => {
                                    self.bot_jobs
                                        .entry(self.current_tick + arrival_time as u32)
                                        .or_default()
                                        .push(BotUpdate::DoGoal(BotGoal::TryDepositItem {
                                            current_charge: rest_charge,
                                            item,
                                            amount,
                                            dest_id,
                                        }));

                                    render_infos.push(BotRenderInfo::StraightLine {
                                        sprite: 0,
                                        start_time: self.current_tick as f32,
                                        end_time: self.current_tick as f32 + arrival_time,
                                        start_pos: current_bot_pos,
                                        end_pos: dest_pos,
                                    });
                                },
                                (out_of_power_time, Err(out_of_power_position)) => {
                                    let recharge_roboport =
                                        self.get_closest_roboport(out_of_power_position);
                                    // TODO: I might want the bots to charge not perfectly on the spot, but some distance away
                                    let recharge_position =
                                        self.get_roboport_position(recharge_roboport);

                                    let (arrival_time_at_roboport_from_out_of_power, Joule(0)) =
                                        Self::path_result_keep_going_when_empty(
                                            out_of_power_position,
                                            Joule(0),
                                            recharge_position,
                                            LOGIBOT_SPEED_CHARGED,
                                            LOGIBOT_SPEED_EMPTY,
                                            LOGIBOT_POWER_CONSUMPTION,
                                        )
                                    else {
                                        unreachable!(
                                            "We started with an empty charge, we should not have gained charge"
                                        )
                                    };

                                    self.bot_jobs
                                        .entry(
                                            self.current_tick
                                                + (out_of_power_time
                                                    + arrival_time_at_roboport_from_out_of_power)
                                                    as u32,
                                        )
                                        .or_default()
                                        .push(BotUpdate::TryStartChargingAndContinue(
                                            recharge_roboport,
                                            recharge_position,
                                            BotGoal::TryDepositItem {
                                                current_charge: BOT_MAX_CHARGE,
                                                item,
                                                amount,
                                                dest_id,
                                            },
                                        ));

                                    render_infos.push(BotRenderInfo::VShape {
                                        sprite: 0,
                                        start_time: self.current_tick as f32,
                                        mid_time: self.current_tick as f32 + out_of_power_time,
                                        end_time: self.current_tick as f32
                                            + out_of_power_time
                                            + arrival_time_at_roboport_from_out_of_power,
                                        start_pos: current_bot_pos,
                                        mid_pos: out_of_power_position,
                                        end_pos: recharge_position,
                                    });
                                },
                            }
                        },
                        Err(WithdrawError::NoItemsWait) => todo!("Requeue the withdraw"),
                        Err(WithdrawError::NoItemsCancel | WithdrawError::StorageRemoved) => {
                            todo!("Send the bot home and remove the reservation on the destination")
                        },
                    }
                },
                BotUpdate::TryStartChargingAndEnter(idx, current_charge, charge_pos) => {
                    // TODO: Handle the case where the roboport was removed?
                    // Or do I want to loop through all jobs to avoid having to check on the hot path?
                    let charge_needed = BOT_MAX_CHARGE - current_charge;
                    if self.roboports[idx as usize].current_charge < charge_needed {
                        // Requeue charging later
                        self.bot_jobs
                            .entry(self.current_tick + BOT_RETRY_CHARGE_TIME)
                            .or_default()
                            .push(BotUpdate::TryStartChargingAndEnter(
                                idx,
                                current_charge,
                                charge_pos,
                            ));

                        // Add the new flying (and waiting) logibots to the particle draw buffer
                        render_infos.push(BotRenderInfo::StraightLine {
                            // TODO: Correct values
                            sprite: 0,
                            start_time: self.current_tick as f32,
                            end_time: (self.current_tick + BOT_RETRY_CHARGE_TIME) as f32,
                            start_pos: charge_pos,
                            end_pos: charge_pos,
                        });
                    } else {
                        // Get the energy from the roboport
                        self.roboports[idx as usize].current_charge =
                            self.roboports[idx as usize].current_charge - charge_needed;

                        // Enter the roboport
                        // FIXME: Check that the roboport has not been deconstructed
                        self.bot_jobs
                            .entry(self.current_tick + BOT_ENTER_TIME)
                            .or_default()
                            .push(BotUpdate::Enter(idx));

                        let roboport_pos = self.roboports[idx as usize].pos;

                        let charge_time =
                            charge_needed.0 as f32 / BOT_CHARGING_RATE.joules_per_tick().0 as f32;

                        // Add the logibot, which is now entering to the particle draw buffer
                        render_infos.push(BotRenderInfo::VShape {
                            // TODO: Correct values
                            sprite: 0,
                            start_time: self.current_tick as f32,
                            mid_time: (self.current_tick as f32) + charge_time,
                            end_time: (self.current_tick + BOT_ENTER_TIME) as f32 + charge_time,
                            start_pos: charge_pos,
                            mid_pos: charge_pos,
                            end_pos: (roboport_pos.x as f32, roboport_pos.y as f32),
                        });
                    }
                },
                BotUpdate::TryStartChargingAndContinue(idx, charge_pos, next_goal) => {
                    let charge_needed = BOT_MAX_CHARGE;
                    if self.roboports[idx as usize].current_charge < charge_needed {
                        // Requeue charging later
                        self.bot_jobs
                            .entry(self.current_tick + BOT_RETRY_CHARGE_TIME)
                            .or_default()
                            .push(BotUpdate::TryStartChargingAndContinue(
                                idx, charge_pos, next_goal,
                            ));

                        // Add the new flying (and waiting) logibots to the particle draw buffer
                        render_infos.push(BotRenderInfo::StraightLine {
                            // TODO: Correct values
                            sprite: 0,
                            start_time: self.current_tick as f32,
                            end_time: (self.current_tick + BOT_RETRY_CHARGE_TIME) as f32,
                            start_pos: charge_pos,
                            end_pos: charge_pos,
                        });
                    } else {
                        // Get the energy from the roboport
                        self.roboports[idx as usize].current_charge =
                            self.roboports[idx as usize].current_charge - charge_needed;

                        let charge_time =
                            charge_needed.0 as f32 / BOT_CHARGING_RATE.joules_per_tick().0 as f32;

                        let ((time, update), render) = match next_goal {
                            BotGoal::TryDepositItem {
                                current_charge: BOT_MAX_CHARGE,
                                item,
                                amount,
                                dest_id,
                            } => self.go_deposit(
                                charge_pos,
                                BOT_MAX_CHARGE,
                                LOGIBOT_SPEED_CHARGED,
                                LOGIBOT_SPEED_EMPTY,
                                LOGIBOT_POWER_CONSUMPTION,
                                item,
                                amount,
                                dest_id,
                            ),
                            BotGoal::TryRetrieveItem {
                                current_charge: BOT_MAX_CHARGE,
                                item,
                                amount,
                                source_id,
                                dest_id,
                            } => self.go_withdraw(
                                charge_pos,
                                BOT_MAX_CHARGE,
                                LOGIBOT_SPEED_CHARGED,
                                LOGIBOT_SPEED_EMPTY,
                                LOGIBOT_POWER_CONSUMPTION,
                                item,
                                amount,
                                source_id,
                                dest_id,
                            ),
                            goal => unreachable!(
                                "Bot Goal after recharging with set current_charge, this indicates an error somewhere {:?}",
                                goal
                            ),
                        };
                        self.bot_jobs
                            .entry(self.current_tick + time)
                            .or_default()
                            .push(update);
                        render_infos.push(render.prepend_waiting(charge_time));
                    }
                },
                BotUpdate::Enter(idx) => {
                    // FIXME: This will break if an inserter fills up a roboport before the bot arrives and will panic!!
                    self.roboports[idx as usize].logibots_idle = self.roboports[idx as usize]
                        .logibots_idle
                        .checked_add(1)
                        .expect("Too many bots in single robotport");

                    // Since the bot is no longer flying around, there is no need to push a render obj
                },
            }
        }

        // Queue new logistic bots flying (and the effect they have when done)
        // TODO: How to efficiently figure out the jobs in the logi network

        for (item, amount, source, destination) in self.get_logibot_jobs().into_iter() {
            let source_pos = self.get_storage_pos(item, source);
            let roboport = self.get_closest_roboport_with_idle_bot_and_take_it(source_pos);

            if let Some((roboport_id, roboport_pos)) = roboport {
                let ((time, update), render) = self.go_withdraw(
                    roboport_pos,
                    BOT_MAX_CHARGE,
                    LOGIBOT_SPEED_CHARGED,
                    LOGIBOT_SPEED_EMPTY,
                    LOGIBOT_POWER_CONSUMPTION,
                    item,
                    amount,
                    source,
                    destination,
                );
                self.bot_jobs
                    .entry(self.current_tick + time)
                    .or_default()
                    .push(update);
                render_infos.push(render);
            } else {
                break;
            }
        }

        // Add the new flying logibots to the particle draw buffer

        info!(
            "Adding {} new logibot draw commands to the draw queue",
            render_infos.len()
        );

        self.current_tick += 1;

        render_infos.into_iter()
    }

    fn update_construction_bots(
        &mut self,
        sim_state: &mut SimulationState<ItemIdxType, RecipeIdxType>,
        world: &mut World<ItemIdxType, RecipeIdxType>,
    ) -> (
        impl IntoIterator<Item = ConstructionBotWorldUpdate> + use<ItemIdxType, RecipeIdxType>,
        impl IntoIterator<Item = BotRenderInfo> + use<ItemIdxType, RecipeIdxType>,
    ) {
        // self.bot_jobs.
        (vec![todo!()], vec![todo!()])
    }

    // FIXME: This assumes all roboports are connected to a single power network.
    // TODO: This might be a case where the single tick delay of the power grid power_mult could lead to problems
    fn update_roboports(&mut self, last_power_mult: u8) -> Joule {
        let mut power_needed = Joule(0);

        for roboport in &mut self.roboports {
            power_needed = power_needed
                + min(
                    MAX_ROBOPORT_CHARGE_RATE.joules_per_tick(),
                    MAX_ROBOPORT_CHARGE - roboport.current_charge,
                );
            roboport.current_charge = min(
                MAX_ROBOPORT_CHARGE,
                roboport.current_charge
                    + (MAX_ROBOPORT_CHARGE_RATE.joules_per_tick() * 64
                        / u64::from(last_power_mult)),
            );

            debug_assert!(roboport.current_charge <= MAX_ROBOPORT_CHARGE);
        }

        power_needed
    }
}
