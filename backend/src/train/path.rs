use std::ops::Add;

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
struct SegmentId(u32);

struct Path {
    segments: Vec<PathSegment>,
}
struct PathSegment {
    segment_id: SegmentId,
    length: DistanceUnit,
}

#[derive(Debug, Clone, Copy)]
struct TrainID(u32);

#[derive(Debug, Clone, Copy)]
struct TrainState {
    train_id: TrainID,

    // units per tick
    speed: u32,

    acceleration: u32,
    braking_force: u32,
    weight: u32,

    length: DistanceUnit,
}

type SegmentReservationList<'a> = &'a mut [bool];

// This is such that we can still represent distances up to 2_000_000 tiles. Maybe thats not enough? For a train ride around the world that does not suffice
const UNITS_PER_TILE: u32 = 2048;

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
struct DistanceUnit(u32);

impl Add for DistanceUnit {
    type Output = Self;

    fn add(self, rhs: Self) -> Self::Output {
        Self(self.0 + rhs.0)
    }
}

impl DistanceUnit {
    fn to_tiles_f64(self) -> f64 {
        f64::from(self.0) / f64::from(UNITS_PER_TILE)
    }
}

impl TrainState {
    fn new(
        id: TrainID,
        speed: u32,
        acc: u32,
        braking: u32,
        weight: u32,
        length: DistanceUnit,
    ) -> Self {
        assert!(weight > 0, "This would mean infinite acc and brake");
        assert!(braking > 0, "This means infinite time till stop");
        assert!(acc > 0, "This would mean infinite time till arrival");

        Self {
            train_id: id,
            speed,
            acceleration: acc,
            braking_force: braking,
            weight,
            length,
        }
    }

    fn new_standing(
        id: TrainID,
        acc: u32,
        braking: u32,
        weight: u32,
        length: DistanceUnit,
    ) -> Self {
        assert!(weight > 0, "This would mean infinite acc and brake");
        assert!(braking > 0, "This means infinite time till stop");
        assert!(acc > 0, "This would mean infinite time till arrival");

        Self {
            train_id: id,
            speed: 0,
            acceleration: acc,
            braking_force: braking,
            weight,
            length,
        }
    }

    const fn units_till_stopped_when_full_brake(self) -> DistanceUnit {
        // This is equivalent to the area under a right triangle with sidelengths speed and time_till_stopped

        // TODO: Roundings
        // TODO: Div_ceil
        let time_till_stopped = self.speed * self.weight / self.braking_force;

        DistanceUnit(self.speed * time_till_stopped / 2)
    }

    const fn accelerate(self) -> Self {
        let Self {
            train_id,
            speed,
            acceleration,
            braking_force,
            weight,
            length,
        } = self;

        // TODO: Max speed, maybe wind, friction depending on what I want
        Self {
            train_id,
            speed: speed + (acceleration / weight),
            acceleration,
            braking_force,
            weight,
            length,
        }
    }

    const fn brake(self) -> Self {
        let Self {
            train_id,
            speed,
            acceleration,
            braking_force,
            weight,
            length,
        } = self;

        // TODO: Max speed, maybe wind, friction depending on what I want
        Self {
            train_id,
            speed: speed - (braking_force / weight),
            acceleration,
            braking_force,
            weight,
            length,
        }
    }
}

impl Path {
    fn needed_reservations(&self, state: TrainState) -> impl Iterator<Item = SegmentId> {
        let mut distance = state.units_till_stopped_when_full_brake() + state.length;

        self.segments
            .iter()
            .take_while(move |segment| {
                distance.0 -= segment.length.0;

                distance.0 > 0
            })
            .map(|segment| segment.segment_id)
    }

    fn new_reservations_for_acceleration(
        &self,
        state: TrainState,
    ) -> impl Iterator<Item = SegmentId> {
        let acc_state = state.accelerate();

        // TODO: Assert we actually own these reservations
        let current_reservations = self.needed_reservations(state);

        let faster_reservations = self.needed_reservations(acc_state);

        #[cfg(debug_assertions)]
        {
            // Current reservations should be a prefix of faster_reservations
            let current_reservations = self.needed_reservations(state);
            let current_reservations_count = self.needed_reservations(state).count();

            let faster_reservations = self.needed_reservations(acc_state);

            itertools::assert_equal(
                current_reservations,
                faster_reservations.take(current_reservations_count),
            );
        }

        faster_reservations.skip(current_reservations.count())
    }
}

#[cfg(test)]
mod test {
    use super::*;

    use proptest::{prop_assert, prop_assume, proptest};

    #[test]
    fn accelerate() {
        let mut state =
            TrainState::new_standing(TrainID(0), 10 * UNITS_PER_TILE, 10, 2_000, DistanceUnit(10));

        for _ in 0..60 {
            let acc = state.accelerate();

            let kmh = DistanceUnit(acc.speed).to_tiles_f64() / 1000.0 * 60.0 * 60.0 * 60.0;

            dbg!(kmh);

            state = acc;
        }
    }

    proptest! {
        #[test]
        fn accelerate_increases_speed(acc in 10u32..100, weight in 2_000u32..20_000) {
            let state = TrainState::new_standing(TrainID(0), acc * UNITS_PER_TILE, 10 * UNITS_PER_TILE, weight, DistanceUnit(10));

            let acc = state.accelerate();

            prop_assert!(acc.speed > state.speed);
        }

        #[test]
        fn brake_distance_does_not_crash(brake in 10u32..100, weight in 2_000u32..20_000, speed in 0u32..4) {
            let state = TrainState::new(TrainID(0), speed, 10 * UNITS_PER_TILE, brake * UNITS_PER_TILE, weight, DistanceUnit(10));

            let _distance = state.units_till_stopped_when_full_brake();
        }

        #[test]
        fn brake_distance_increases_with_speed(brake in 10u32..100, weight in 2_000u32..20_000, speed in 0u32..4, higher_speed in 0u32..4) {
            prop_assume!(speed < higher_speed);
            let state = TrainState::new(TrainID(0), speed * UNITS_PER_TILE, 10 * UNITS_PER_TILE, brake * UNITS_PER_TILE, weight, DistanceUnit(10));

            let distance_slow = state.units_till_stopped_when_full_brake();

            let state_fast = TrainState::new(TrainID(0), higher_speed * UNITS_PER_TILE, 10 * UNITS_PER_TILE, brake * UNITS_PER_TILE, weight, DistanceUnit(10));

            let distance_fast = state_fast.units_till_stopped_when_full_brake();

            prop_assert!(distance_slow < distance_fast);
        }
    }
}
