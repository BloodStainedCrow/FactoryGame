use std::marker::PhantomData;

use crate::item::ItemTrait;

use super::{InserterState, MOVETIME};

#[derive(Debug, Clone)]
pub struct BeltBeltInserter<T: ItemTrait> {
    marker: PhantomData<T>,
    state: InserterState,
}

impl<T: ItemTrait> Default for BeltBeltInserter<T> {
    fn default() -> Self {
        Self::new()
    }
}

impl<T: ItemTrait> BeltBeltInserter<T> {
    #[must_use]
    pub const fn new() -> Self {
        Self {
            marker: PhantomData,
            state: InserterState::Empty,
        }
    }

    pub fn update(&mut self, loc_in: &mut bool, loc_out: &mut bool) {
        // TODO: I just added InserterStates and it is a lot slower (unsurprisingly),
        // Try and find a faster implementation of similar logic

        match self.state {
            InserterState::Empty => {
                if *loc_in {
                    *loc_in = false;

                    self.state = InserterState::FullAndMovingOut(MOVETIME);
                }
            },
            InserterState::FullAndWaitingForSlot => {
                if !*loc_out {
                    // There is space in the machine
                    *loc_out = true;

                    self.state = InserterState::EmptyAndMovingBack(MOVETIME);
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
