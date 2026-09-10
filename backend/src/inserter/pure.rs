use std::num::NonZero;

use data::item::{Item, ItemCountType};
use itertools::Either;
use stable_vec::StableVec;
use static_assertions::const_assert_eq;

use crate::{
    inserter::{bucket::Bucket, pure::State::Incoming},
    power_grid::{
        inserter::InserterBackendID,
        power_mult::{AdvanceResult, PowerMult, PowerMultTimer},
    },
    slot_arenas::item_arena::{SingleItemSlice, SingleItemSlotIndex},
};

#[derive(Debug, Clone)]
pub(crate) struct PureOneToOneInserterStore {
    // TODO: This might not be kept since it can be inferred from the vec location?
    item: Item,
    /// The inserter movetime in power_adjusted_time
    // FIXME: Typing
    movetime: u16,

    /// The percentage we are done with advancing a single tick
    timer: PowerMultTimer,

    inserters: StableVec<InserterState>,

    incoming_buckets: Bucket<InserterBucketInfo>,
    outgoing_buckets: Bucket<InserterBucketInfo>,
}

#[derive(Debug, Clone, Copy)]
struct InserterState {
    /// This is used to calculate how far the inserter has moved (implicitly)
    /// Since the maximum inserter move time is u16::MAX we can use a u16 here
    /// After an inserter is done moving, its information is added to the waitlist and MUST be read first from there. The info here WILL become incorrect after `movetime`
    /// This is in power_adjusted_time
    time_updated: u16,

    state: State,
}

pub enum InserterRenderState {
    WaitingForItems(ItemCountType),
    FullAndMovingOut(ItemCountType, f32),
    WaitingForSpaceInDestination(ItemCountType),
    EmptyAndMovingBack(f32),
}

impl From<(InserterState, u16, u16)> for InserterRenderState {
    fn from((state, current_time, movetime): (InserterState, u16, u16)) -> Self {
        let time_passed = state.time_updated.wrapping_sub(current_time);

        let move_perc = f32::from(time_passed) / f32::from(movetime);

        match state.state {
            State::Outgoing(hand) => Self::FullAndMovingOut(hand.into(), move_perc),
            State::Incoming => Self::EmptyAndMovingBack(move_perc),
        }
    }
}

const_assert_eq!(std::mem::size_of::<InserterState>(), 4);

#[derive(Debug, Clone, Copy)]
enum State {
    Outgoing(NonZero<ItemCountType>),
    Incoming,
}

#[derive(Debug, Clone)]
struct InserterBucketInfo {
    id: InserterBackendID,
    source: SingleItemSlotIndex,
    sink: SingleItemSlotIndex,
    hand_size: ItemCountType,
    hand_count: ItemCountType,
}
const_assert_eq!(std::mem::size_of::<InserterBucketInfo>(), 16);

pub(crate) struct InserterRemovalInfoMoving {}
pub(crate) struct InserterRemovalInfoStatic {}

impl PureOneToOneInserterStore {
    pub fn new(item: Item, movetime: u16) -> Self {
        Self {
            item,
            movetime,
            timer: PowerMultTimer(0),
            inserters: vec![].into(),
            incoming_buckets: Bucket::new(usize::from(movetime)),
            outgoing_buckets: Bucket::new(usize::from(movetime)),
        }
    }

    pub fn add_inserter(&mut self) -> InserterBackendID {
        let index = self.inserters.push(InserterState {
            time_updated: self.timer.0 as u16,
            state: Incoming,
        });

        InserterBackendID(index.try_into().expect("More than u32::MAX inserters"))
    }

    pub fn remove_inserter(
        &mut self,
        id: InserterBackendID,
        token_already_removed: bool,
    ) -> Either<InserterRemovalInfoMoving, InserterRemovalInfoStatic> {
        let inserter = self
            .inserters
            .remove(id.0 as usize)
            .expect("Tried to remove inserter that did not exist");

        if !token_already_removed {
            let state = self.get_state_after_checking_waitlist(id);
            match state {
                // TODO: We should be able to calculate the bucket
                InserterRenderState::FullAndMovingOut(_, _) => {
                    let bucket_info = self
                        .outgoing_buckets
                        .remove_first(|b| b.id == id)
                        .expect("Where else would it be?");

                    Either::Left(InserterRemovalInfoMoving {})
                },
                InserterRenderState::EmptyAndMovingBack(_) => todo!(),
                _ => unreachable!(),
            }
        } else {
            Either::Right(InserterRemovalInfoStatic {})
        }
    }

    pub fn update(&mut self, power_mult: PowerMult, single_item_slice: &mut SingleItemSlice) {
        match self.timer.advance(power_mult) {
            AdvanceResult::Tick => {},
            AdvanceResult::NoTick => {
                // Nothing changed
                return;
            },
        }

        for outgoing in self.outgoing_buckets.advance() {
            // TODO: Interact with lists
        }

        for incoming in self.incoming_buckets.advance() {
            // TODO: Interact with lists
        }
    }

    /// If this is called without checking the waitlists, this will result in incorrect information!!!
    pub fn get_state_after_checking_waitlist(
        &self,
        inserter: InserterBackendID,
    ) -> InserterRenderState {
        let state = self.inserters[inserter.0 as usize];

        (state, self.timer.0 as u16, self.movetime).into()
    }
}
