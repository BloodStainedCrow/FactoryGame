use std::num::NonZero;

use enum_map::Enum;

use crate::{bucket_store::Reinsertion, item::ITEMCOUNTTYPE};

struct StorageStorageInserterStore {}

struct Info {
    last_ticked: u16,
    state_at_last_tick: ImplicitState,
}

enum ImplicitState {
    WaitingForSourceItems(ITEMCOUNTTYPE),
    WaitingForSpaceInDestination(ITEMCOUNTTYPE),
}

struct StorageStorageInserterReinsertion;

#[derive(Debug, Enum)]
enum State {
    PickingUpItems,
    DroppingOffItems,
}

#[derive(Debug, Clone, Copy)]
struct MovingInserter {
    id: u32,
    todo: !,
}

#[derive(Debug, Clone, Copy)]
struct TickingInserter {
    id: u32,
    hand: ITEMCOUNTTYPE,
    todo: !,
}

struct WorldState<'a> {
    max_hand_size: ITEMCOUNTTYPE,

    current_tick: u32,
    infos: &'a mut [Info],
}

impl Reinsertion for StorageStorageInserterReinsertion {
    const STATE_COUNT: usize = State::LENGTH;

    type MovingValue = MovingInserter;

    type TickingValue = TickingInserter;

    type WorldState<'a> = WorldState<'a>;

    #[inline]
    fn tick<'a>(
        state: usize,
        value: &mut Self::TickingValue,
        world_state: &mut Self::WorldState<'a>,
    ) -> bool {
        let WorldState {
            current_tick,
            infos,
            max_hand_size,
        } = world_state;

        infos[value.id as usize].last_ticked = *current_tick as u16;

        match State::from_usize(state) {
            State::PickingUpItems => {
                let amount_picked_up: ITEMCOUNTTYPE = todo!("Pick up actually");

                if amount_picked_up > 0 {
                    value.hand += amount_picked_up;
                    infos[value.id as usize].state_at_last_tick =
                        ImplicitState::WaitingForSourceItems(value.hand);
                    if value.hand == *max_hand_size {
                        todo!("Try to put it in waitlist");
                        true
                    } else {
                        false
                    }
                } else {
                    false
                }
            },
            State::DroppingOffItems => {
                let amount_dropped_off: ITEMCOUNTTYPE = todo!("Drop off actually");

                if amount_dropped_off > 0 {
                    value.hand -= amount_dropped_off;
                    infos[value.id as usize].state_at_last_tick =
                        ImplicitState::WaitingForSpaceInDestination(value.hand);
                    if value.hand == 0 {
                        todo!("Try to put it in waitlist");
                        true
                    } else {
                        false
                    }
                } else {
                    false
                }
            },
        }
    }

    fn ticking_to_moving(value: Self::TickingValue) -> Option<Self::MovingValue> {
        todo!()
    }

    fn moving_to_ticking(value: Self::MovingValue) -> Self::TickingValue {
        todo!()
    }
}
