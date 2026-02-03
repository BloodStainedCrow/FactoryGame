pub(crate) struct ConstReinsertionBucketStore<
    const STATE_COUNT: usize,
    Reinsertion: super::Reinsertion,
> {
    pub reinsertion_time: u32,

    states: [State<Reinsertion>; STATE_COUNT],
}

struct State<Reinsertion: super::Reinsertion> {
    moving_time_counts: Box<[u32]>,
    moving_values: Vec<Reinsertion::MovingValue>,
    ticking_values: Vec<Reinsertion::TickingValue>,
}

impl<const STATE_COUNT: usize, Reinsertion: super::Reinsertion>
    ConstReinsertionBucketStore<STATE_COUNT, Reinsertion>
{
    pub fn new(reinsertion_time: u32) -> Self {
        assert_eq!(Reinsertion::STATE_COUNT, STATE_COUNT);
        assert!(Reinsertion::STATE_COUNT > 0);
        assert!(reinsertion_time > 0);
        Self {
            reinsertion_time,
            states: std::array::from_fn(|_| State {
                moving_time_counts: vec![0; reinsertion_time as usize].into_boxed_slice(),
                moving_values: vec![],
                ticking_values: vec![],
            }),
        }
    }

    pub fn update<'a>(&mut self, world_state: &mut Reinsertion::WorldState<'a>) {
        for state in &mut self.states {
            // Done moving
            let (ticking, (moving, moving_time_counts)) = {
                (
                    &mut state.ticking_values,
                    (&mut state.moving_values, &mut state.moving_time_counts),
                )
            };

            let amount_done_moving = moving_time_counts[0];
            let extract_range = 0..(amount_done_moving as usize);

            let removed_moving = moving.drain(extract_range);
            let added_ticking = removed_moving.map(|moving| Reinsertion::moving_to_ticking(moving));

            ticking.extend(added_ticking);

            moving_time_counts.rotate_left(1);
            assert_eq!(
                amount_done_moving,
                moving_time_counts[self.reinsertion_time as usize - 1]
            );
            moving_time_counts[self.reinsertion_time as usize - 1] = 0;
        }

        for state in 0..STATE_COUNT {
            let next_state = (state + 1) % STATE_COUNT;

            // Start Moving
            let (ticking, (moving, moving_time_counts)) = if state == next_state {
                assert!(STATE_COUNT == 1);
                let state = &mut self.states[state];
                (
                    &mut state.ticking_values,
                    (&mut state.moving_values, &mut state.moving_time_counts),
                )
            } else {
                assert!(STATE_COUNT > 1);
                let [this, next] = self.states.get_disjoint_mut([state, next_state]).unwrap();

                (
                    &mut this.ticking_values,
                    (&mut next.moving_values, &mut next.moving_time_counts),
                )
            };

            let extracted_ticking =
                ticking.extract_if(.., |ticking| Reinsertion::tick(state, ticking, world_state));

            let reinserted_moving =
                extracted_ticking.filter_map(|ticking| Reinsertion::ticking_to_moving(ticking));

            let moving_old_len = moving.len();
            moving.extend(reinserted_moving);
            let added = moving.len() - moving_old_len;

            moving_time_counts[self.reinsertion_time as usize - 1] += added as u32;
        }
    }
}
