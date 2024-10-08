use std::{marker::PhantomData, num::NonZero};

use bytemuck::TransparentWrapper;

use crate::item::{ItemStorage, ItemTrait};
use std::marker::ConstParamTy;

#[derive(ConstParamTy, PartialEq, Eq)]
pub enum Dir {
    BeltToStorage,
    StorageToBelt,
}

#[derive(Debug, Clone, Copy)]
pub struct BeltStorageInserter<T: ItemTrait, const DIR: Dir> {
    marker: PhantomData<T>,
    pub storage_id: NonZero<u16>,
    state: InserterState,
}

// TODO: This could be minified using a union or similar,
// But since Inserters are the same sice, whether this is 2 or 1 byte (atleast in a Vec of Structs)
// I will leave this be for now.
#[derive(Debug, Clone, Copy)]
enum InserterState {
    Empty,
    FullAndWaitingForSlot,
    FullAndMovingOut(u8),
    EmptyAndMovingBack(u8),
}

impl<T: ItemTrait, const DIR: Dir> BeltStorageInserter<T, DIR> {
    #[must_use]
    pub const fn new(id: NonZero<u16>) -> Self {
        Self {
            marker: PhantomData,
            storage_id: id,
            state: InserterState::Empty,
        }
    }
}

impl<T: ItemTrait> BeltStorageInserter<T, { Dir::BeltToStorage }> {
    // #[inline(never)]
    /// This function assumes that it can remove an item from somewhere (a belt)
    /// The caller is responsible for doing the removal and ensuring before calling this,
    /// that there is an item to remove
    /// This Inserter removes Items from belts and adds them to a Machine
    pub fn update(&mut self, loc: &mut bool, storages: &mut [ItemStorage<T>]) {
        // TODO: I just added InserterStates and it is a lot slower (unsurprisingly),
        // Try and find a faster implementation of similar logic
        const MOVETIME: u8 = 10;

        match self.state {
            InserterState::Empty => {
                if *loc {
                    *loc = false;
                    self.state = InserterState::FullAndMovingOut(MOVETIME);
                }
            },
            InserterState::FullAndWaitingForSlot => {
                if *TransparentWrapper::peel_ref(
                    &storages[usize::from(Into::<u16>::into(self.storage_id))],
                ) < T::MAX_STACK_SIZE
                {
                    // There is space in the machine
                    *TransparentWrapper::peel_mut(
                        &mut storages[usize::from(Into::<u16>::into(self.storage_id))],
                    ) += 1;

                    self.state = InserterState::EmptyAndMovingBack(MOVETIME);
                }
            },
            InserterState::FullAndMovingOut(time) => {
                if time > 0 {
                    self.state = InserterState::FullAndMovingOut(time - 1);
                } else {
                    // TODO: Do I want to try inserting immediately
                    self.state = InserterState::FullAndWaitingForSlot;
                }
            },
            InserterState::EmptyAndMovingBack(time) => {
                if time > 0 {
                    self.state = InserterState::EmptyAndMovingBack(time - 1);
                } else {
                    // TODO: Do I want to try getting a new item immediately
                    self.state = InserterState::Empty;
                }
            },
        }
    }
}
