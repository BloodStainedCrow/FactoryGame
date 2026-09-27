use std::collections::VecDeque;

#[derive(Debug, Clone)]
pub struct Bucket<T> {
    sizes: VecDeque<u32>,
    values: VecDeque<T>,
}

impl<T> Bucket<T> {
    pub fn new(timer: usize) -> Self {
        assert!(timer > 0);
        Self {
            sizes: vec![0; timer].into(),
            values: VecDeque::new(),
        }
    }

    pub fn add(&mut self, value: T) {
        *self.sizes.back_mut().expect("Timer was 0") += 1;
        self.values.push_back(value);
    }

    pub fn remove_first(&mut self, filter: impl Fn(&T) -> bool) -> Option<T> {
        let pos: u32 = self
            .values
            .iter()
            .position(filter)?
            .try_into()
            .expect("More than u32::MAX things in bucket");

        // Adjust sizes
        let mut current = 0;
        for sizes in &mut self.sizes {
            current += *sizes;
            if current > pos {
                *sizes -= 1;
                break;
            }
        }

        Some(self.values.remove(pos as usize).expect("Checked before"))
    }

    pub fn advance(&mut self) -> impl Iterator<Item = T> {
        let count = self.sizes[0] as usize;

        let values = self.values.drain(0..count);

        // Advance
        self.sizes[0] = 0;
        self.sizes.rotate_left(1);

        values
    }
}

#[cfg(test)]
mod test {
    use super::*;

    use proptest::{prelude::*, prop_assert_eq, prop_assert_ne, proptest};

    #[test]
    fn remove_first_shrinks_only_the_bucket_of_the_removed_value() {
        let mut bucket: Bucket<&str> = Bucket::new(3);
        bucket.add("a");
        bucket.add("b");
        bucket.add("c");

        assert_eq!(bucket.remove_first(|v| *v == "b"), Some("b"));

        // "b" sat in the last bucket, so only that bucket shrank: "a" comes
        // out on the third advance, followed by "c", and nothing else.
        assert_eq!(bucket.advance().count(), 0);
        assert_eq!(bucket.advance().count(), 0);
        let out: Vec<_> = bucket.advance().collect();
        assert_eq!(out, vec!["a", "c"]);
    }

    #[test]
    fn remove_first_from_the_front_bucket() {
        let mut bucket: Bucket<&str> = Bucket::new(3);
        bucket.add("a");
        bucket.add("b");
        bucket.add("c");

        assert_eq!(bucket.remove_first(|v| *v == "a"), Some("a"));

        assert_eq!(bucket.advance().count(), 0);
        assert_eq!(bucket.advance().count(), 0);
        let out: Vec<_> = bucket.advance().collect();
        assert_eq!(out, vec!["b", "c"]);
    }

    #[test]
    fn remove_first_with_empty_leading_buckets_does_not_underflow() {
        // Values added right after creation all sit in the last bucket, so the
        // leading slots are empty. Removing a value must not decrement them
        // (this used to panic with a subtract overflow).
        let mut bucket: Bucket<&str> = Bucket::new(4);
        bucket.add("a");
        bucket.add("b");

        assert_eq!(bucket.remove_first(|v| *v == "a"), Some("a"));

        for _ in 0..3 {
            assert_eq!(bucket.advance().count(), 0);
        }
        let out: Vec<_> = bucket.advance().collect();
        assert_eq!(out, vec!["b"]);
    }

    proptest! {
        #[test]
        fn remove_first_keeps_values_and_timing_consistent(
            ticks in 1usize..8,
            removal_positions in prop::collection::vec(0usize..8, 0..4),
        ) {
            let mut bucket: Bucket<usize> = Bucket::new(ticks);
            let mut expected: Vec<usize> = (0..4).collect();

            for value in &expected {
                bucket.add(*value);
            }

            for &pos in &removal_positions {
                if expected.is_empty() {
                    break;
                }
                let index = pos % expected.len();
                let target = expected[index];

                prop_assert_eq!(bucket.remove_first(|v| *v == target), Some(target));
                expected.remove(index);
            }

            // Everything left comes out in order on the final tick, and the
            // earlier ticks release nothing.
            let mut released = Vec::new();
            for _ in 0..ticks {
                released.extend(bucket.advance());
            }
            prop_assert_eq!(released, expected);
        }

        #[test]
        fn comes_out_after_x_ticks(ticks in 1usize..1000) {
            let mut bucket: Bucket<()> = Bucket::new(ticks);

            bucket.add(());

            for i in 1..=ticks {
                let res = bucket.advance();

                if i == ticks {
                    prop_assert_ne!(res.count(), 0);
                } else {
                    prop_assert_eq!(res.count(), 0);
                }
            }
        }

        #[test]
        fn only_one_at_a_time(ticks in 1usize..1000) {
            let mut bucket: Bucket<()> = Bucket::new(ticks);


            for i in 1..=(ticks * 2) {
                bucket.add(());
                let res = bucket.advance();

                if i < ticks {
                    prop_assert_eq!(res.count(), 0);
                } else {
                    prop_assert_eq!(res.count(), 1);
                }
            }
        }
    }
}
