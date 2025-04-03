use crate::item::{IdxTrait, Item, WeakIdxTrait};

use super::{InserterState, SushiInserterState};

#[derive(Debug, Clone, serde::Deserialize, serde::Serialize)]
pub struct BeltBeltInserter {
    state: InserterState,
}

impl Default for BeltBeltInserter {
    fn default() -> Self {
        Self::new()
    }
}

impl BeltBeltInserter {
    #[must_use]
    pub const fn new() -> Self {
        Self {
            state: InserterState::Empty,
        }
    }

    pub fn update(&mut self, loc_in: &mut bool, loc_out: &mut bool, movetime: u8) {
        // TODO: I just added InserterStates and it is a lot slower (unsurprisingly),
        // Try and find a faster implementation of similar logic

        match self.state {
            InserterState::Empty => {
                if *loc_in {
                    *loc_in = false;

                    self.state = InserterState::FullAndMovingOut(movetime);
                }
            },
            InserterState::FullAndWaitingForSlot => {
                if !*loc_out {
                    *loc_out = true;

                    self.state = InserterState::EmptyAndMovingBack(movetime);
                }
            },
            InserterState::FullAndMovingOut(time) => {
                if time > 0 {
                    self.state = InserterState::FullAndMovingOut(time - 1);
                } else {
                    // TODO: Do I want to try inserting immediately?
                    self.state = InserterState::FullAndWaitingForSlot;
                }
            },
            InserterState::EmptyAndMovingBack(time) => {
                if time > 0 {
                    self.state = InserterState::EmptyAndMovingBack(time - 1);
                } else {
                    // TODO: Do I want to try getting a new item immediately?
                    self.state = InserterState::Empty;
                }
            },
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
