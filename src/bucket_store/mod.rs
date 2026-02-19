pub(crate) mod const_reinsertion;

mod storage_storage_inserter_store;

pub(crate) trait Reinsertion {
    const STATE_COUNT: usize;

    type MovingValue: Copy;
    type TickingValue: Copy;

    type WorldState<'a>;

    fn tick<'a>(
        state: usize,
        value: &mut Self::TickingValue,
        world_state: &mut Self::WorldState<'a>,
    ) -> bool;
    fn ticking_to_moving(value: Self::TickingValue) -> Option<Self::MovingValue>;

    fn moving_to_ticking(value: Self::MovingValue) -> Self::TickingValue;
}
