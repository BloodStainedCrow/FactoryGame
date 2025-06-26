use crate::item::{IdxTrait, Item, WeakIdxTrait};

use super::{InserterState, SushiInserterState};

use crate::item::ITEMCOUNTTYPE;

#[derive(Debug, Clone, Copy, serde::Deserialize, serde::Serialize)]
pub struct BeltBeltInserter {
    state: InserterState,
}

impl Default for BeltBeltInserter {
    fn default() -> Self {
        Self::new()
    }
}

pub trait Loc {
    type Item;

    fn has_item(&self) -> bool;

    fn peek_item(&self) -> Option<Self::Item>;

    fn try_take_item(&mut self) -> Option<Self::Item>;

    /// Calling this on a Loc without an item is considered an error
    fn force_take_item(&mut self) -> Self::Item;

    /// Calling this on a Loc without an item is considered an error
    fn force_put_loc_value(&mut self, to_put: Option<Self::Item>);

    /// Calling this on a Loc with an item is considered an error
    fn force_put_item(&mut self, to_put: Self::Item);
}

impl Loc for bool {
    type Item = ();

    fn has_item(&self) -> bool {
        *self
    }

    fn peek_item(&self) -> Option<Self::Item> {
        self.then_some(())
    }

    fn try_take_item(&mut self) -> Option<Self::Item> {
        let ret = self.then_some(());
        *self = false;
        ret
    }

    fn force_take_item(&mut self) -> Self::Item {
        debug_assert!(*self);
        *self = false;
        ()
    }

    fn force_put_loc_value(&mut self, to_put: Option<Self::Item>) {
        *self = to_put.is_some();
    }

    fn force_put_item(&mut self, _to_put: Self::Item) {
        *self = true;
    }
}

impl<ItemIdxType: IdxTrait> Loc for Option<Item<ItemIdxType>> {
    type Item = Item<ItemIdxType>;

    fn has_item(&self) -> bool {
        self.is_some()
    }

    fn peek_item(&self) -> Option<Self::Item> {
        *self
    }

    fn try_take_item(&mut self) -> Option<Self::Item> {
        self.take()
    }

    fn force_take_item(&mut self) -> Self::Item {
        self.take().unwrap()
    }

    fn force_put_loc_value(&mut self, to_put: Option<Self::Item>) {
        *self = to_put;
    }

    fn force_put_item(&mut self, to_put: Self::Item) {
        debug_assert!(self.is_none());
        *self = Some(to_put);
    }
}

impl BeltBeltInserter {
    #[must_use]
    pub const fn new() -> Self {
        Self {
            state: InserterState::WaitingForSourceItems(0),
        }
    }

    #[profiling::function]
    pub fn update<LocType: Loc>(
        &mut self,
        loc_in: &mut LocType,
        loc_out: &mut LocType,
        movetime: u8,
        max_hand_size: ITEMCOUNTTYPE,
        filter: LocType::Item,
        filter_test: impl Fn(LocType::Item) -> bool,
    ) {
        // TODO: I just added InserterStates and it is a lot slower (unsurprisingly),
        // Try and find a faster implementation of similar logic

        match self.state {
            InserterState::WaitingForSourceItems(count) => {
                if let Some(item) = loc_in.peek_item() {
                    if filter_test(item) {
                        loc_in.force_take_item();

                        if count + 1 == max_hand_size {
                            self.state = InserterState::FullAndMovingOut(movetime);
                        } else {
                            self.state = InserterState::WaitingForSourceItems(count + 1);
                        }
                    }
                }
            },
            InserterState::WaitingForSpaceInDestination(count) => {
                if !loc_out.has_item() {
                    loc_out.force_put_item(filter);

                    if count == 1 {
                        self.state = InserterState::EmptyAndMovingBack(movetime);
                    } else {
                        self.state = InserterState::WaitingForSpaceInDestination(count - 1);
                    }
                }
            },
            InserterState::FullAndMovingOut(time) => {
                if time > 0 {
                    self.state = InserterState::FullAndMovingOut(time - 1);
                } else {
                    // TODO: Do I want to try inserting immediately?
                    self.state = InserterState::WaitingForSpaceInDestination(max_hand_size);
                }
            },
            InserterState::EmptyAndMovingBack(time) => {
                if time > 0 {
                    self.state = InserterState::EmptyAndMovingBack(time - 1);
                } else {
                    // TODO: Do I want to try getting a new item immediately?
                    self.state = InserterState::WaitingForSourceItems(0);
                }
            },
        }
    }

    #[profiling::function]
    pub fn update_instant<LocType: Loc>(&mut self, loc_in: &mut LocType, loc_out: &mut LocType) {
        if !loc_out.has_item() {
            let item = loc_in.try_take_item();
            loc_out.force_put_loc_value(item);
        }
    }
}

#[derive(Debug, Clone, serde::Deserialize, serde::Serialize)]
pub struct SushiBeltBeltInserter<ItemIdxType: WeakIdxTrait> {
    state: SushiInserterState<ItemIdxType>,
}

impl<ItemIdxType: IdxTrait> Default for SushiBeltBeltInserter<ItemIdxType> {
    fn default() -> Self {
        Self::new()
    }
}

impl<ItemIdxType: IdxTrait> SushiBeltBeltInserter<ItemIdxType> {
    #[must_use]
    pub const fn new() -> Self {
        Self {
            state: SushiInserterState::Empty,
        }
    }

    pub fn update(
        &mut self,
        item_in: &mut Option<Item<ItemIdxType>>,
        item_out: &mut Option<Item<ItemIdxType>>,
        movetime: u8,
    ) {
        // TODO: I just added InserterStates and it is a lot slower (unsurprisingly),
        // Try and find a faster implementation of similar logic

        match self.state {
            SushiInserterState::Empty => {
                if let Some(item) = *item_in {
                    *item_in = None;

                    self.state = SushiInserterState::FullAndMovingOut(movetime, item);
                }
            },
            SushiInserterState::FullAndWaitingForSlot(item) => {
                if item_out.is_none() {
                    *item_out = Some(item);

                    self.state = SushiInserterState::EmptyAndMovingBack(movetime);
                }
            },
            SushiInserterState::FullAndMovingOut(time, item) => {
                if time > 0 {
                    self.state = SushiInserterState::FullAndMovingOut(time - 1, item);
                } else {
                    // TODO: Do I want to try inserting immediately?
                    self.state = SushiInserterState::FullAndWaitingForSlot(item);
                }
            },
            SushiInserterState::EmptyAndMovingBack(time) => {
                if time > 0 {
                    self.state = SushiInserterState::EmptyAndMovingBack(time - 1);
                } else {
                    // TODO: Do I want to try getting a new item immediately?
                    self.state = SushiInserterState::Empty;
                }
            },
        }
    }
}
