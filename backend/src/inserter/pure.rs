use std::num::NonZero;

use data::item::{Item, ItemCountType};

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

    /// The percentage we are done with advancing a single tick
    timer: PowerMultTimer,

    inserters: Vec<PureOneToOneInserter>,

    incoming_buckets: Bucket<InserterBucketInfo>,
    outgoing_buckets: Bucket<InserterBucketInfo>,
}

#[derive(Debug, Clone)]
struct PureOneToOneInserter {
    state: InserterState,
}

#[derive(Debug, Clone)]
struct InserterState {
    // FIXME: Typing
    time_updated: u32,

    state: Option<State>,
}

#[derive(Debug, Clone)]
enum State {
    Outgoing(NonZero<ItemCountType>),
    Incoming,
}

#[derive(Debug, Clone)]
struct InserterBucketInfo {
    id: InserterBackendID,
    source: SingleItemSlotIndex,
    sink: SingleItemSlotIndex,
    hand_count: ItemCountType,
}

impl PureOneToOneInserterStore {
    pub fn update(
        &mut self,
        power_mult: PowerMult,
        time: u32,
        single_item_slice: &mut SingleItemSlice,
    ) {
        match self.timer.advance(power_mult) {
            AdvanceResult::Tick => {},
            AdvanceResult::NoTick => {
                // Nothing changed
                return;
            },
        }

        for outgoing in self.outgoing_buckets.advance() {
            self.inserters[outgoing.id.0 as usize].state.time_updated = time;
            self.inserters[outgoing.id.0 as usize].state.state = None;

            // TODO: Interact with lists
        }

        for incoming in self.incoming_buckets.advance() {
            self.inserters[incoming.id.0 as usize].state.time_updated = time;
            self.inserters[incoming.id.0 as usize].state.state = None;

            // TODO: Interact with lists
        }
    }
}
