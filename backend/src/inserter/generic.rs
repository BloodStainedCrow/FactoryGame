use std::num::NonZero;

use data::item::{
    Item, ItemCountType, ItemStack,
    item_set::{ItemSet, ItemSetTrait},
};
use middle_indices::InserterMiddleID;
use stable_vec::StableVec;

use crate::{
    chests::sushi::SushiChest,
    inserter::{
        bucket::Bucket,
        power_tick::{MonotonicPowerTick, NoTick, PowerMult, RealTick, Tick},
    },
};

#[derive(Debug, Clone)]
struct InserterStore<I: InserterKindTrait> {
    /// The inserter movetime in real_ticks
    // FIXME: Typing
    movetime: u16,

    power_timer: MonotonicPowerTick,

    inserters: StableVec<InserterState<I::Hand>>,
    middle_ids: StableVec<InserterMiddleID>,

    incoming_buckets: Bucket<I::EmptyAndMovingBack>,
    outgoing_buckets: Bucket<I::FullAndGoingOut>,

    kind: I,
}

#[derive(Debug, Clone, Copy)]
struct InserterState<H: Copy> {
    /// This is used to calculate how far the inserter has moved (implicitly)
    /// Since the maximum inserter move time is `u16::MAX` we can use a u16 here
    /// After an inserter is done moving, its information is added to the waitlist and MUST be read first from there. The info here WILL become incorrect after `movetime`
    time_updated: RealTick,

    state: State<H>,
}

#[derive(Debug, Clone, Copy)]
enum State<H: Copy> {
    Outgoing(H),
    Incoming,
}

pub enum InserterRenderState<H> {
    WaitingForItems(H),
    FullAndMovingOut(H, f32),
    WaitingForSpaceInDestination(H),
    EmptyAndMovingBack(f32),
}

impl<H: Copy> From<(InserterState<H>, &MonotonicPowerTick, u16)> for InserterRenderState<H> {
    fn from((state, current_time, movetime): (InserterState<H>, &MonotonicPowerTick, u16)) -> Self {
        let time_passed = current_time.time_since(state.time_updated);

        debug_assert!(time_passed <= movetime);

        let move_perc = f32::from(time_passed) / f32::from(movetime);

        match state.state {
            State::Outgoing(hand) => Self::FullAndMovingOut(hand.into(), move_perc),
            State::Incoming => Self::EmptyAndMovingBack(move_perc),
        }
    }
}

impl<I: InserterKindTrait> InserterStore<I> {
    pub fn new(kind: I, movetime: u16) -> Self {
        Self {
            movetime,
            power_timer: MonotonicPowerTick::default(),
            inserters: StableVec::new(),
            middle_ids: StableVec::new(),
            incoming_buckets: Bucket::new(movetime.into()),
            outgoing_buckets: Bucket::new(movetime.into()),
            kind,
        }
    }

    fn update(&mut self, power_mult: PowerMult, source: &mut I::Source, dest: &mut I::Dest) {
        match self.power_timer.advance(power_mult) {
            Ok(Tick) => {},
            Err(NoTick) => {
                return;
            },
        }

        let now_waiting_for_items = self.incoming_buckets.advance();

        for arrived in now_waiting_for_items {
            match self.kind.try_take_item(arrived) {
                Ok(going_out) => {
                    self.outgoing_buckets.add(going_out);
                },
                Err(()) => {
                    // Was added to waitlist
                },
            }
        }

        let now_going_back = self.outgoing_buckets.advance();

        for arrived in now_going_back {
            match self.kind.try_insert_item(arrived) {
                Ok(going_out) => {
                    self.incoming_buckets.add(going_out);
                },
                Err(()) => {
                    // Was added to waitlist
                },
            }
        }
    }

    /// The caller must first check the waitlists this inserter is attached to, to prevent long waiting inserters from misreporting!
    fn get_state(&self, id: InserterMiddleID) -> InserterRenderState<I::Hand> {
        let ins = self.inserters[id.0 as usize];

        (ins, &self.power_timer, self.movetime).into()
    }
}

trait InserterKindTrait {
    type Source: ItemSourceProvider;
    type Dest: ItemDestProvider;

    type Hand: Copy;

    type EmptyAndMovingBack;
    type FullAndGoingOut;
    type WaitingForItems;
    type WaitingForSpaceInDestination;

    // TODO: What if something does not have a waitlist?
    fn try_take_item(&self, arrived: Self::EmptyAndMovingBack)
    -> Result<Self::FullAndGoingOut, ()>;
    // TODO: What if something does not have a waitlist?
    fn try_insert_item(
        &self,
        arrived: Self::FullAndGoingOut,
    ) -> Result<Self::EmptyAndMovingBack, ()>;
}

trait ItemSourceProvider {
    type Index;
    type Single: ItemSourceTrait;

    fn get_source(&mut self, index: Self::Index) -> &mut Self::Single;
}

trait ItemDestProvider {
    type Index;
    type Single: ItemDestTrait;

    fn get_dest(&mut self, index: Self::Index) -> &mut Self::Single;
}

trait ItemSourceTrait {
    fn take_single_item(
        &mut self,
        item: Item,
        max_count: NonZero<ItemCountType>,
    ) -> Result<NonZero<ItemCountType>, ()>;
    fn take_one_of_many_items(
        &mut self,
        items: &ItemSet,
        max_count: NonZero<ItemCountType>,
    ) -> Option<ItemStack>;
}

impl<T: ItemSourceTrait> ItemSourceTrait for [T] {
    fn take_single_item(
        &mut self,
        item: Item,
        mut max_count: NonZero<ItemCountType>,
    ) -> Result<NonZero<ItemCountType>, ()> {
        let mut taken = 0;
        for source in self {
            let new_taken = source
                .take_single_item(item, max_count)
                .map_or(0, Into::into);
            taken += new_taken;
            max_count = match NonZero::new(ItemCountType::from(max_count) - taken) {
                Some(new_max) => new_max,
                None => {
                    debug_assert_eq!(ItemCountType::from(max_count), new_taken);
                    return Ok(NonZero::new(taken).unwrap());
                },
            };
        }

        match NonZero::new(taken) {
            Some(v) => Ok(v),
            None => Err(()),
        }
    }

    fn take_one_of_many_items(
        &mut self,
        items: &ItemSet,
        max_count: NonZero<ItemCountType>,
    ) -> Option<ItemStack> {
        for i in 0..self.len() {
            let source = &mut self[i];
            let new_taken = source.take_one_of_many_items(items, max_count);
            match new_taken {
                Some(stack) => {
                    let new_max = match NonZero::new(
                        ItemCountType::from(max_count) - ItemCountType::from(stack.count),
                    ) {
                        Some(new_max) => new_max,
                        None => return Some(stack),
                    };

                    return self[(i + 1)..]
                        .take_single_item(stack.item, new_max)
                        .map_or(Some(stack), |add_count| {
                            Some(stack.increase_count(add_count.into()))
                        });
                },
                None => {},
            }
        }
        for source in self {}

        None
    }
}

trait ItemDestTrait {
    fn try_put_item_stack(&mut self, stack: ItemStack) -> Result<(), ItemStack>;
}

impl<T: ItemDestTrait> ItemDestTrait for [T] {
    fn try_put_item_stack(&mut self, mut stack: ItemStack) -> Result<(), ItemStack> {
        for dest in self {
            match dest.try_put_item_stack(stack) {
                Ok(()) => return Ok(()),
                Err(remaining) => stack = remaining,
            }
        }

        Err(stack)
    }
}

// Sushi Chest
impl ItemSourceTrait for SushiChest {
    fn take_single_item(
        &mut self,
        item: Item,
        count: NonZero<ItemCountType>,
    ) -> Result<NonZero<ItemCountType>, ()> {
        self.try_remove_item(count, |i| i == item)
            .ok_or(())
            .map(|stack| stack.count.into())
    }

    fn take_one_of_many_items(
        &mut self,
        items: &ItemSet,
        count: NonZero<ItemCountType>,
    ) -> Option<ItemStack> {
        self.try_remove_item(count, |i| items.contains(i))
    }
}
impl ItemDestTrait for SushiChest {
    fn try_put_item_stack(&mut self, stack: ItemStack) -> Result<(), ItemStack> {
        self.try_add_item_stack(stack)
    }
}
