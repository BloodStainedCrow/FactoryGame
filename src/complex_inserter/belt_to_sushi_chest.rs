use std::num::NonZero;

use crate::{
    belt::{belt::BeltLenType, smart::SmartBelt},
    frontend::world::tile::BeltId,
    item::{ITEMCOUNTTYPE, Item},
};

struct BeltToSushiChestInserter {
    movetime: u16,
    max_hand_size: ITEMCOUNTTYPE,
    state: BeltToSushiInserterState,

    sources: [(BeltId<u8>, BeltLenType); 2],
}

enum BeltToSushiInserterState {
    WaitingForSourceItems { current_hand: Option<PickupInfo> },
    MovingOut { current_hand: HandInfo, timer: u16 },
    WaitingForSpaceInDestination { current_hand: HandInfo },
    EmptyAndMovingBack { timer: u16 },
}

struct HandInfo {
    item: Item<u8>,
    count: ITEMCOUNTTYPE,
}

struct PickupInfo {
    hand: HandInfo,
    patience_timer: NonZero<u8>,
}

struct SushiBelt;

impl BeltToSushiChestInserter {
    pub fn update(&mut self, sushi_belts: &mut [SushiBelt], pure_belts: &mut [SmartBelt<u8>]) {
        match &mut self.state {
            BeltToSushiInserterState::WaitingForSourceItems { current_hand: None } => todo!(),
            BeltToSushiInserterState::WaitingForSourceItems {
                current_hand:
                    Some(PickupInfo {
                        hand,
                        patience_timer,
                    }),
            } => todo!(),
            BeltToSushiInserterState::MovingOut {
                current_hand,
                timer,
            } => todo!(),
            BeltToSushiInserterState::WaitingForSpaceInDestination { current_hand } => todo!(),
            BeltToSushiInserterState::EmptyAndMovingBack { timer } => todo!(),
        }
    }
}
