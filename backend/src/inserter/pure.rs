use std::num::NonZero;

use data::item::{Item, ItemCountType};
use static_assertions::const_assert_eq;

use crate::{
    inserter::bucket::Bucket,
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

    inserters: Vec<InserterState>,

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

pub enum MovingInserterRenderState {
    FullAndMovingOut(ItemCountType, f32),
    EmptyAndMovingBack(f32),
}

impl From<(InserterState, u16, u16)> for MovingInserterRenderState {
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
    // TODO: We still have 2 bytes of room
    hand_count: ItemCountType,
}
const_assert_eq!(std::mem::size_of::<InserterBucketInfo>(), 16);

impl PureOneToOneInserterStore {
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
    ) -> MovingInserterRenderState {
        let state = self.inserters[inserter.0 as usize];

        (state, self.timer.0 as u16, self.movetime).into()
    }
}
