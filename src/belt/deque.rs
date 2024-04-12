use std::collections::VecDeque;

use crate::item::Item;

#[allow(clippy::module_name_repetitions)]
#[derive(Debug)]
pub struct DequeBelt {
    compressed: VecDeque<Item>,
    // moving ALWAYS is either Empty, or starts with a None value
    moving: VecDeque<Option<Item>>,
    has_free_spot_at_front: bool,
    len: usize,
}

impl DequeBelt {
    #[must_use]
    pub fn new(len: usize) -> Self {
        let moving = VecDeque::from(vec![None; len - 1]);

        Self {
            compressed: VecDeque::<Item>::with_capacity(len),
            moving,
            len,
            has_free_spot_at_front: true,
        }
    }

    pub fn update(&mut self) {
        let starting_free_spot = usize::from(self.has_free_spot_at_front);
        debug_assert_eq!(
            self.compressed.len() + self.moving.len() + starting_free_spot,
            self.len
        );

        if self.has_free_spot_at_front && !self.compressed.is_empty() {
            self.has_free_spot_at_front = false;
            self.moving.push_back(None);
        } else {
            // We are stuck.

            if !self.moving.is_empty() {
                // The first item in self.moving is a None, by definition
                // Otherwise the following items would be in self.compressed
                let removed_none = self.moving.pop_front();
                debug_assert_eq!(removed_none, Some(None));

                // Complexity: This is amortized constant IFF , since every item will only pass through this loop ONCE
                // And we only output a constant amount of items each tick.
                let to_remove = self.moving.drain(
                    ..self
                        .moving
                        .iter()
                        .position(Option::is_none)
                        .unwrap_or(self.moving.len()),
                );

                self.compressed
                    .extend(to_remove.map(|item| item.unwrap_or_else(|| unreachable!())));

                // Either we stopped because self.moving is now empty
                // OR we stopped because we found a None value.

                // In any case, we now either have a None value at the front of self.moving
                // Or it is empty
                self.moving.push_back(None);

                // So we readd the None value we removed at the start
                // Recreating our invariants.
            }
        }
    }

    #[must_use]
    pub fn query_item(&self, pos: usize) -> Option<Item> {
        if pos == 0 && self.has_free_spot_at_front {
            return None;
        }

        let starting_free_spot = usize::from(self.has_free_spot_at_front);

        if pos < starting_free_spot + self.compressed.len() {
            Some(self.compressed[pos - starting_free_spot])
        } else if pos < starting_free_spot + self.compressed.len() + self.moving.len() {
            self.moving[pos - starting_free_spot - self.compressed.len()]
        } else {
            unreachable!("pos was out of range for the belt")
        }
    }

    pub fn get_and_remove(&mut self, pos: usize) -> Option<Item> {
        let ret = self.query_item(pos);

        self.remove(pos);

        ret
    }

    fn remove(&mut self, pos: usize) {
        if pos == 0 {
            if !self.has_free_spot_at_front {
                let _ = self.compressed.pop_front();
                self.has_free_spot_at_front = true;
            }
            return;
        }

        let starting_free_spot = usize::from(self.has_free_spot_at_front);

        if pos < starting_free_spot + self.compressed.len() {
            // This is not O(1).
            // In fact in the worst case this means we are O(2n) per tick.

            // TODO: It is possible to end up moving items from moving to compressed and back. Check when this could happen and what a potential fix could be

            let items_to_remove = self.compressed.drain(pos..);

            for item in items_to_remove {
                self.moving.push_front(Some(item));
            }

            // Since we pushed at least one element, we know self.moving is not empty and we will never panic here
            self.moving[0] = None;
        } else if pos < starting_free_spot + self.compressed.len() + self.moving.len() {
            self.moving[pos - starting_free_spot - self.compressed.len()] = None;
        } else {
            unreachable!("pos was out of range for the belt");
        }
    }

    pub fn try_insert(&mut self, pos: usize, item: Item) -> bool {
        if pos == 0 && self.has_free_spot_at_front {
            self.has_free_spot_at_front = false;
            self.compressed.push_front(item);
            return true;
        }

        let starting_free_spot = usize::from(self.has_free_spot_at_front);

        if pos < starting_free_spot + self.compressed.len() {
            false
        } else if pos < starting_free_spot + self.compressed.len() + self.moving.len() {
            let ret = self.moving[pos - starting_free_spot - self.compressed.len()].is_none();

            if ret {
                if pos - starting_free_spot - self.compressed.len() == 0 {
                    self.moving[0] = Some(item);
                    // Rebuild the invariant since now self.moving[0] != None

                    let to_remove = self.moving.drain(
                        ..self
                            .moving
                            .iter()
                            .position(Option::is_none)
                            .unwrap_or(self.moving.len()),
                    );

                    self.compressed
                        .extend(to_remove.map(|item| item.unwrap_or_else(|| unreachable!())));

                    // Either we stopped because self.moving is now empty
                    // OR we stopped because we found a None value.
                } else {
                    self.moving[pos - starting_free_spot - self.compressed.len()] = Some(item);
                }
            }

            ret
        } else {
            unreachable!("pos was out of range for the belt");
        }
    }
}

#[cfg(test)]
mod test {
    extern crate test;
    use test::Bencher;

    use proptest::{prelude::prop, prop_assert_eq, proptest};

    use crate::{
        belt::{deque::DequeBelt, do_update_test},
        item::{option, Item},
    };

    const MAX_LEN: usize = 10_000;

    proptest! {

        #[test]
        fn test_deque_belt_moves_item_forward(item_pos in 0..MAX_LEN) {
            let mut belt = DequeBelt::new(MAX_LEN);

            let ret = belt.try_insert(item_pos, Item::Iron);

            // Since the whole belt is empty, it should not fail to put an item in
            assert!(ret);

            belt.update();

            if item_pos > 0 {
                // The item should have moved
                for i in 0..MAX_LEN {
                    if i == item_pos - 1 {
                        prop_assert_eq!(belt.query_item(i), Some(Item::Iron));
                    } else {
                        prop_assert_eq!(belt.query_item(i), None);
                    }
                }
            } else {
                // The item should NOT have moved
                for i in 0..MAX_LEN {
                    if i == item_pos {
                        prop_assert_eq!(belt.query_item(i), Some(Item::Iron));
                    } else {
                        prop_assert_eq!(belt.query_item(i), None);
                    }
                }
            }
        }

        #[test]
        fn test_deque_belt_agrees_with_functional(mut items in prop::collection::vec(option(), 1..100)) {
            let mut belt = DequeBelt::new(items.len());

            for (i, item_opt) in items.iter().enumerate() {
                match item_opt {
                    Some(item) => {
                        belt.try_insert(i, *item);
                    },
                    None => {
                        belt.get_and_remove(i);
                    },
                };
            }

            for _update_count in 0..items.len() * 2 {
                belt.update();

                do_update_test(&mut items);

                for (i, item) in items.iter().enumerate() {
                    prop_assert_eq!(belt.query_item(i), *item);
                }
            }
        }
    }

    #[test]
    fn test_deque_belt_update_stuck() {
        let mut belt = DequeBelt::new(MAX_LEN);
        belt.try_insert(MAX_LEN - 1, Item::Iron);

        belt.try_insert(0, Item::Iron);

        for _ in 0..1_000 {
            let bb = test::black_box(&mut belt);
            for _ in 0..1_000 {
                bb.update();
                // println!("{bb}");
            }
        }

        // println!("{belt}");
    }

    #[bench]
    fn bench_deque_belt_update_free_flowing(b: &mut Bencher) {
        let mut belt = DequeBelt::new(MAX_LEN);

        b.iter(|| {
            let bb = test::black_box(&mut belt);
            for _ in 0..1_000 {
                bb.try_insert(MAX_LEN - 1, Item::Iron);
                bb.update();
                // println!("{bb}");
            }
        });

        println!("{belt:?}");
    }

    #[bench]
    fn bench_deque_belt_update_stuck(b: &mut Bencher) {
        let mut belt = DequeBelt::new(MAX_LEN);
        belt.try_insert(MAX_LEN - 1, Item::Iron);

        belt.try_insert(0, Item::Iron);

        b.iter(|| {
            let bb = test::black_box(&mut belt);
            for _ in 0..1_000 {
                bb.update();
                // println!("{bb}");
            }
        });

        // println!("{belt}");
    }

    #[bench]
    fn bench_deque_belt_half_full(b: &mut Bencher) {
        let mut belt = DequeBelt::new(MAX_LEN);

        for i in 0..MAX_LEN / 2 {
            assert!(belt.try_insert(i, Item::Iron));
        }

        for _ in MAX_LEN / 2..MAX_LEN {
            // This spot is empty
        }

        b.iter(|| {
            for _ in 0..1_000 {
                let bb = test::black_box(&mut belt);
                bb.update();
            }
        });
    }

    #[bench]
    fn bench_deque_belt_worst_case_inserter(b: &mut Bencher) {
        let mut belt = DequeBelt::new(MAX_LEN);

        let mut num_updates = 0;

        b.iter(|| {
            num_updates += 1;
            let bb = test::black_box(&mut belt);
            bb.try_insert(MAX_LEN - 1, Item::Iron);
            bb.update();
            bb.get_and_remove(1);
        });

        // println!("{belt:?}");

        println!("Done {num_updates} updates.");
    }
}
