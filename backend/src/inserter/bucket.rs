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
        // TODO: TEST THIS
        let mut current = 0;
        for sizes in &mut self.sizes {
            current += *sizes;
            if current < pos {
                *sizes -= 1;
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

    use proptest::{prop_assert_eq, prop_assert_ne, proptest};

    proptest! {
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
