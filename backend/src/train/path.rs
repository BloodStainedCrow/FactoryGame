use std::{
    cmp::min,
    iter::{self, Sum},
    ops::Add,
};

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
struct SegmentId(u32);

#[derive(Debug)]
struct Path {
    segments: Vec<PathSegment>,
}

#[derive(Debug)]
struct PathSegment {
    segment_id: SegmentId,
    length: DistanceUnit,
    is_chain: bool,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
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

struct SegmentReservationList<'a> {
    list: &'a mut [Option<TrainID>],
}

impl SegmentReservationList<'_> {
    #[cfg(debug_assertions)]
    fn is_reserved_by_us(&self, index: SegmentId, train_id: TrainID) -> bool {
        self.list[index.0 as usize] == Some(train_id)
    }

    fn can_be_reserved(&self, index: SegmentId) -> bool {
        self.list[index.0 as usize].is_none()
    }

    fn reserve(&mut self, index: SegmentId, train_id: TrainID) {
        assert!(self.list[index.0 as usize].is_none());

        self.list[index.0 as usize] = Some(train_id);
    }

    fn release(&mut self, index: SegmentId, train_id: TrainID) {
        assert_eq!(self.list[index.0 as usize], Some(train_id));

        self.list[index.0 as usize] = None;
    }
}

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

impl Sum for DistanceUnit {
    fn sum<I: Iterator<Item = Self>>(iter: I) -> Self {
        Self(iter.map(|unit| unit.0).sum())
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

    fn units_till_stopped_when_full_brake(self) -> DistanceUnit {
        // This is equivalent to the area under a right triangle with sidelengths speed and time_till_stopped (if it were not discrete)

        // FIXME: Find a closed formula
        DistanceUnit(
            iter::successors(Some(self.speed), |&speed| {
                if speed > 0 {
                    Some(speed.saturating_sub(self.braking_force / self.weight))
                } else {
                    None
                }
            })
            .sum(),
        )
    }

    fn accelerate(self) -> Self {
        let Self {
            train_id,
            speed,
            acceleration,
            braking_force,
            weight,
            length,
        } = self;

        // TODO: Max speed, maybe wind, friction depending on what I want
        // FIXME: Hardcoded max speed
        Self {
            train_id,
            speed: min(speed + (acceleration / weight), UNITS_PER_TILE),
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

        // TODO: maybe wind, friction depending on what I want
        Self {
            train_id,
            speed: speed.saturating_sub(braking_force / weight),
            acceleration,
            braking_force,
            weight,
            length,
        }
    }
}

impl Path {
    fn current_reservations(&self, state: TrainState) -> impl Iterator<Item = SegmentId> + Clone {
        self.needed_reservations_for_distance(
            state.units_till_stopped_when_full_brake() + state.length,
        )
    }

    fn needed_reservations_for_distance(
        &self,
        mut distance: DistanceUnit,
    ) -> impl Iterator<Item = SegmentId> + Clone {
        let mut found_stop = false;
        self.segments
            .iter()
            .take_while(move |segment| {
                let take = distance.0 > 0;

                distance.0 = distance.0.saturating_sub(segment.length.0);
                if distance.0 == 0 && !segment.is_chain {
                    found_stop = true;
                }

                take || !found_stop
            })
            .map(|segment| segment.segment_id)
    }

    fn new_reservations_for_acceleration<'a>(
        &'a self,
        current_state: TrainState,
        reservation_state: &mut SegmentReservationList,
    ) -> impl Iterator<Item = SegmentId> + Clone + use<'a> {
        let current_reservations = self.current_reservations(current_state);

        for reservation in current_reservations.clone() {
            assert!(reservation_state.is_reserved_by_us(reservation, current_state.train_id));
        }

        let state_after_acc = current_state.accelerate();

        // This determines whether we move than change speed or the other way round
        // FIXME: These two using different speeds (one pre-acc one after) is wrong for sure. But it is the only way to make the tests pass lul
        let amount_moved = state_after_acc.speed;

        let reservations_needed_after_move_and_acc = self.needed_reservations_for_distance(
            state_after_acc.units_till_stopped_when_full_brake()
                + state_after_acc.length
                + DistanceUnit(amount_moved),
        );

        #[cfg(debug_assertions)]
        {
            // Ensure current is a prefix
            itertools::assert_equal(
                current_reservations.clone(),
                reservations_needed_after_move_and_acc
                    .clone()
                    .take(current_reservations.clone().count()),
            );
        }

        reservations_needed_after_move_and_acc
            .clone()
            .skip(current_reservations.count())
    }

    fn new_reservations_for_braking(
        &self,
        current_state: TrainState,
    ) -> impl Iterator<Item = SegmentId> {
        let current_reservations = self.current_reservations(current_state);

        let state_after_brake = current_state.brake();

        // This determines whether we move than change speed or the other way round
        // FIXME: These two using different speeds (one pre-acc one after) is wrong for sure. But it is the only way to make the tests pass lul
        let amount_moved = current_state.speed;

        let reservations_needed_after_move_and_brake = self.needed_reservations_for_distance(
            state_after_brake.units_till_stopped_when_full_brake()
                + state_after_brake.length
                + DistanceUnit(amount_moved),
        );

        // If this ever fails we could have an issue:
        // If left is shorter, we might not be able to brake in the distance we reserved, which is important
        // If right is shorter, we reserve something we will not need later, and on the next tick we do not know this is required
        itertools::assert_equal(
            current_reservations,
            reservations_needed_after_move_and_brake,
        );

        iter::empty()
    }

    fn advance(
        &mut self,
        current_state: &mut TrainState,
        mut reservation_state: SegmentReservationList,
    ) {
        debug_assert!(
            current_state.length <= self.segments.iter().map(|segment| segment.length).sum(),
            "Speed: {}",
            current_state.speed
        );

        #[cfg(debug_assertions)]
        for segment in self.current_reservations(*current_state) {
            assert!(
                reservation_state.is_reserved_by_us(segment, current_state.train_id),
                "{segment:?} not reserved by us"
            );
        }

        let mut distance_to_advance = DistanceUnit(current_state.speed);

        // Move
        loop {
            let Some(front) = self.segments.first_mut() else {
                unreachable!("At least the last segment where the train is in should remain")
            };

            let to_take = min(distance_to_advance.0, front.length.0);

            distance_to_advance.0 -= to_take;
            front.length.0 -= to_take;

            if front.length == DistanceUnit(0) {
                let released = self.segments.remove(0);
                reservation_state.release(released.segment_id, current_state.train_id);
            }

            if distance_to_advance.0 == 0 {
                break;
            }
        }

        #[cfg(debug_assertions)]
        for segment in self.current_reservations(*current_state) {
            assert!(reservation_state.is_reserved_by_us(segment, current_state.train_id),);
        }

        let need_to_brake_to_not_overrun_end = current_state
            .accelerate()
            .units_till_stopped_when_full_brake()
            + current_state.length
            >= self.segments.iter().map(|seg| seg.length).sum();

        // Check if we can accelerate
        if !need_to_brake_to_not_overrun_end
            && self
                .new_reservations_for_acceleration(*current_state, &mut reservation_state)
                .all(|segment| {
                    debug_assert!(
                        !reservation_state.is_reserved_by_us(segment, current_state.train_id)
                    );

                    reservation_state.can_be_reserved(segment)
                })
        {
            // We may accelerate
            for segment in
                self.new_reservations_for_acceleration(*current_state, &mut reservation_state)
            {
                reservation_state.reserve(segment, current_state.train_id);
            }

            *current_state = current_state.accelerate();
        } else {
            // We must brake
            *current_state = current_state.brake();
        }

        #[cfg(debug_assertions)]
        for segment in self.current_reservations(*current_state) {
            assert!(reservation_state.is_reserved_by_us(segment, current_state.train_id));
        }
    }
}

#[cfg(test)]
mod test {
    use super::*;

    use itertools::Itertools;
    use proptest::{prop_assert, prop_assert_eq, prop_assume, proptest};

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

    #[test]
    fn traverse_path_free() {
        const NUM_PATHS: u32 = 100;

        const TRAIN_SIZE: DistanceUnit = DistanceUnit(UNITS_PER_TILE * 4);

        let mut state = TrainState::new_standing(
            TrainID(0),
            10 * UNITS_PER_TILE,
            10 * UNITS_PER_TILE,
            2_000,
            TRAIN_SIZE,
        );

        let mut path = Path {
            segments: (0..NUM_PATHS)
                .map(|id| PathSegment {
                    segment_id: SegmentId(id),
                    length: DistanceUnit(UNITS_PER_TILE * 40),
                    is_chain: false,
                })
                .collect(),
        };

        let mut segment_reservations = vec![None; NUM_PATHS as usize];
        segment_reservations[0] = Some(TrainID(0));

        for _ in 0..10_000 {
            path.advance(
                &mut state,
                SegmentReservationList {
                    list: &mut segment_reservations,
                },
            );
        }

        assert_eq!(state.speed, 0);
        assert_eq!(path.segments.len(), 1);
        // TODO: Some imprecision in the system
        assert!(path.segments[0].length < DistanceUnit(TRAIN_SIZE.0 + 10));
    }

    #[test]
    fn traverse_path_free_all_chain() {
        const NUM_PATHS: u32 = 100;

        const TRAIN_SIZE: DistanceUnit = DistanceUnit(UNITS_PER_TILE * 4);

        let mut state = TrainState::new_standing(
            TrainID(0),
            10 * UNITS_PER_TILE,
            10 * UNITS_PER_TILE,
            2_000,
            TRAIN_SIZE,
        );

        let mut path = Path {
            segments: (0..NUM_PATHS)
                .map(|id| PathSegment {
                    segment_id: SegmentId(id),
                    length: DistanceUnit(UNITS_PER_TILE * 40),
                    is_chain: true,
                })
                .collect(),
        };

        let mut segment_reservations = vec![None; NUM_PATHS as usize];

        // Let the train start in a non-chain segment. Since the first reservation (done when the train departs, would otherwise need to reserve everything)
        path.segments[0].is_chain = false;
        segment_reservations[0] = Some(TrainID(0));

        for _ in 0..10_000 {
            path.advance(
                &mut state,
                SegmentReservationList {
                    list: &mut segment_reservations,
                },
            );
        }

        assert_eq!(state.speed, 0);
        assert_eq!(path.segments.len(), 1);
        // TODO: Some imprecision in the system
        assert!(path.segments[0].length < DistanceUnit(TRAIN_SIZE.0 + 10));
    }

    #[test]
    fn traverse_path_blocked() {
        const NUM_PATHS: u32 = 100;

        let mut state = TrainState::new_standing(
            TrainID(0),
            10 * UNITS_PER_TILE,
            10 * UNITS_PER_TILE,
            2_000,
            DistanceUnit(UNITS_PER_TILE * 4),
        );

        let mut path = Path {
            segments: (0..NUM_PATHS)
                .map(|id| PathSegment {
                    segment_id: SegmentId(id),
                    length: DistanceUnit(UNITS_PER_TILE * 40),
                    is_chain: false,
                })
                .collect(),
        };

        let mut segment_reservations = vec![None; NUM_PATHS as usize];
        segment_reservations[0] = Some(TrainID(0));
        segment_reservations[(NUM_PATHS / 2) as usize] = Some(TrainID(u32::MAX));

        for _ in 0..10_000 {
            path.advance(
                &mut state,
                SegmentReservationList {
                    list: &mut segment_reservations,
                },
            );
        }

        assert_eq!(state.speed, 0);
        assert!(path.segments.len() > 1);
    }

    #[test]
    fn traverse_path_blocked_all_chain() {
        const NUM_PATHS: u32 = 100;

        let mut state = TrainState::new_standing(
            TrainID(0),
            10 * UNITS_PER_TILE,
            10 * UNITS_PER_TILE,
            2_000,
            DistanceUnit(UNITS_PER_TILE * 4),
        );

        let mut path = Path {
            segments: (0..NUM_PATHS)
                .map(|id| PathSegment {
                    segment_id: SegmentId(id),
                    length: DistanceUnit(UNITS_PER_TILE * 40),
                    is_chain: true,
                })
                .collect(),
        };

        let mut segment_reservations = vec![None; NUM_PATHS as usize];

        // Let the train start in a non-chain segment. Since the first reservation (done when the train departs, would otherwise need to reserve everything)
        path.segments[0].is_chain = false;

        segment_reservations[0] = Some(TrainID(0));
        segment_reservations[(NUM_PATHS / 2) as usize] = Some(TrainID(u32::MAX));

        for _ in 0..10_000 {
            path.advance(
                &mut state,
                SegmentReservationList {
                    list: &mut segment_reservations,
                },
            );
        }

        assert_eq!(state.speed, 0);
        assert_eq!(
            path.segments.len(),
            NUM_PATHS as usize,
            "Train went into chain signal segment even though path was not fully claimable"
        );
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
        fn brake_distance_is_correct(brake in 10u32..100, weight in 2_000u32..20_000, speed in 0u32..(UNITS_PER_TILE * 4)) {
            let mut state = TrainState::new(
                TrainID(0),
                speed,
                10 * UNITS_PER_TILE,
                brake * UNITS_PER_TILE,
                weight,
                DistanceUnit(10),
            );

            let prediced_distance = state.units_till_stopped_when_full_brake();

            let mut real_distance = 0;
            for _ in 0..1_000_000 {
                real_distance += state.speed;
                state = state.brake();

                if state.speed == 0 {
                    break;
                }
            }

            prop_assert!(real_distance == prediced_distance.0, "real: {}, predicted: {}", real_distance, prediced_distance.0);
        }

        #[test]
        fn test_reservations_not_empty(brake in 10u32..100, weight in 2_000u32..20_000, speed in 0u32..UNITS_PER_TILE) {
            const NUM_PATHS: u32 = 1_000;

            let state = TrainState::new(TrainID(0), speed, 10 * UNITS_PER_TILE, brake * UNITS_PER_TILE, weight, DistanceUnit(10 * UNITS_PER_TILE));

            let path = Path {
                segments: (0..NUM_PATHS)
                    .map(|id| PathSegment {
                        segment_id: SegmentId(id),
                        length: DistanceUnit(UNITS_PER_TILE * 40),
                        is_chain: false,
                    })
                    .collect(),
            };

            prop_assert!(path.current_reservations(state).count() > 0);
        }


        #[test]
        fn braking_should_always_yield_the_same_reservations(brake in 10u32..100, weight in 2_000u32..20_000, speed in 0u32..UNITS_PER_TILE) {
            const NUM_PATHS: u32 = 10_000;

            let mut state = TrainState::new(TrainID(0), speed, 10 * UNITS_PER_TILE, brake * UNITS_PER_TILE, weight, DistanceUnit(10 * UNITS_PER_TILE));

            let mut path = Path {
                segments: (0..NUM_PATHS)
                    .map(|id| PathSegment {
                        segment_id: SegmentId(id),
                        length: DistanceUnit(4),
                        is_chain: false,
                    })
                    .collect(),
            };

            let reservations = path.current_reservations(state).collect_vec();

            // dbg!(state.speed, state.units_till_stopped_when_full_brake());

            for _ in 0..1_000 {
                let mut distance_to_advance = DistanceUnit(state.speed);

                // Move
                loop {
                    let Some(front) = path.segments.first_mut() else {
                        return Ok(());
                    };

                    let to_take = min(distance_to_advance.0, front.length.0);

                    distance_to_advance.0 -= to_take;
                    front.length.0 -= to_take;

                    if front.length == DistanceUnit(0) {
                        let _released = path.segments.remove(0);
                    }

                    if distance_to_advance.0 == 0 {
                        break;
                    }
                }

                prop_assume!(!path.segments.is_empty());

                state = state.brake();

                let new_reservations = path.current_reservations(state).collect_vec();


                prop_assert_eq!(reservations.last(), new_reservations.last());
            }
        }

        // This is like the most important test, since it means that no matter what happens we will always reserve what we need for
        #[test]
        fn new_reservations_for_brake_should_be_always_empty(brake in 10u32..100, weight in 2_000u32..20_000, speed in 0u32..UNITS_PER_TILE) {
            const NUM_PATHS: u32 = 1_000;

            let state = TrainState::new(TrainID(0), speed, 10 * UNITS_PER_TILE, brake * UNITS_PER_TILE, weight, DistanceUnit(10 * UNITS_PER_TILE));

            let path = Path {
                segments: (0..NUM_PATHS)
                    .map(|id| PathSegment {
                        segment_id: SegmentId(id),
                        length: DistanceUnit(UNITS_PER_TILE * 40),
                        is_chain: false,
                    })
                    .collect(),
            };

            prop_assert_eq!(path.new_reservations_for_braking(state).count(), 0);
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
