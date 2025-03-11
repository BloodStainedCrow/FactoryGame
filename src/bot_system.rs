use std::{cmp::min, collections::BTreeMap, marker::PhantomData};

// I will render bots as "particles" with a fixed size instanced gpu buffer
// This is fine as long as we do not override bots, which are still flying
const GOAL_ITEMS_PER_TICK: usize = 400;
const BOT_HAND_SIZE: usize = 4;
const BOT_SPEED: usize = 120 * 1000 / 60 / 60; // tiles / s
const MAX_BASE_SIZE: usize = 3000;
const AVG_TRIP_LENGTH: usize = 300;
const MAX_BOT_TRAVEL_TIME_TICKS: usize = MAX_BASE_SIZE * 60 / BOT_SPEED;
const AVG_BOT_TRAVEL_TIME_TICKS: usize = AVG_TRIP_LENGTH * 60 / BOT_SPEED;
const AVG_NEW_BOTS_PER_TICK: usize = GOAL_ITEMS_PER_TICK / (BOT_HAND_SIZE);
const REQUIRED_BOTS: usize = GOAL_ITEMS_PER_TICK * AVG_BOT_TRAVEL_TIME_TICKS / BOT_HAND_SIZE;
const REQUIRED_DRAW_SLOTS: usize = MAX_BOT_TRAVEL_TIME_TICKS * AVG_NEW_BOTS_PER_TICK;
const OVERDRAW_RATIO: usize = MAX_BASE_SIZE / AVG_TRIP_LENGTH;

use crate::{
    frontend::world::Position,
    item::{IdxTrait, Item, WeakIdxTrait, ITEMCOUNTTYPE},
    power::{Joule, Watt},
    rendering::app_state::SimulationState,
};

struct BotNetwork<ItemIdxType: WeakIdxTrait, RecipeIdxType: WeakIdxTrait> {
    // TODO: This might need another graph to handle removal of roboports
    current_tick: u32,
    roboports: Vec<Roboport>,
    bot_jobs: BTreeMap<u32, Vec<BotUpdate<ItemIdxType>>>,

    providers: Box<[MultiProviderInfo]>,
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
}

struct MultiProviderInfo {}

struct MultiRequesterInfo {}

struct MultiStorageInfo {}

enum BotUpdate<ItemIdxType: WeakIdxTrait> {
    // TODO: Name the fields
    DepositItem(Joule, Item<ItemIdxType>, u16, ITEMCOUNTTYPE),
    TryRetrieveItem(Joule, Item<ItemIdxType>, u16, ITEMCOUNTTYPE),
    // TODO: To allow removing of robotports I will need generational indices!
    TryStartCharging(u32, Joule, (f32, f32)),
    EnterRoboport(u32),
}

// TODO: Make this dynamic
const MAX_ROBOPORT_CHARGE: Joule = Joule(2_000_000);
const MAX_ROBOPORT_CHARGE_RATE: Watt = Watt(2_000_000);

const BOT_CHARGING_RATE: Watt = Watt(500);
const BOT_MAX_CHARGE: Joule = Joule(3_000_000);

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

impl<ItemIdxType: IdxTrait, RecipeIdxType: IdxTrait> BotNetwork<ItemIdxType, RecipeIdxType> {
    fn update_logibots(
        &mut self,
        sim_state: &mut SimulationState<RecipeIdxType>,
    ) -> impl IntoIterator<Item = BotRenderInfo> {
        let mut render_infos = vec![];

        // Handle logibots, which are now done with their job
        let done = self
            .bot_jobs
            .remove(&self.current_tick)
            .into_iter()
            .flatten();

        for update in done {
            match update {
                BotUpdate::DepositItem(current_charge, item, storage_id, _) => todo!(),
                BotUpdate::TryRetrieveItem(current_charge, item, storage_id, _) => todo!(),
                BotUpdate::TryStartCharging(idx, current_charge, charge_pos) => {
                    let charge_needed = BOT_MAX_CHARGE - current_charge;
                    if self.roboports[idx as usize].current_charge < charge_needed {
                        // Requeue charging later
                        self.bot_jobs
                            .entry(self.current_tick + BOT_RETRY_CHARGE_TIME)
                            .or_default()
                            .push(BotUpdate::TryStartCharging(idx, charge_needed, charge_pos));

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
                        // TODO: Check that the roboport has not been deconstructed
                        self.bot_jobs
                            .entry(self.current_tick + BOT_ENTER_TIME)
                            .or_default()
                            .push(BotUpdate::EnterRoboport(idx));

                        let roboport_pos = self.roboports[idx as usize].pos;

                        let charge_time =
                            (charge_needed.0 as f32 / BOT_CHARGING_RATE.joules_per_tick().0 as f32);

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
                BotUpdate::EnterRoboport(idx) => {
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

        // Add the new flying logibots to the particle draw buffer

        self.current_tick += 1;

        render_infos
    }

    fn update_construction_bots(&mut self) -> impl IntoIterator<Item = ConstructionBotWorldUpdate> {
        // self.bot_jobs.
        vec![todo!()]
    }

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
