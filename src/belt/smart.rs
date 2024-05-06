use std::{marker::PhantomData, mem};

use crate::{
    inserter::Inserter,
    item::{Item, ItemStorage, ItemTrait},
};

use super::belt::{Belt, NoSpaceError};

#[allow(clippy::module_name_repetitions)]
#[derive(Debug)]
pub struct SmartBelt<T: ItemTrait> {
    marker: PhantomData<T>,
    first_free_index: FreeIndex,
    zero_index: usize,
    locs: Vec<bool>,
    inserters: Vec<(u16, Inserter<T>)>,
}

#[derive(Debug, PartialEq)]
enum FreeIndex {
    FreeIndex(usize),
    OldFreeIndex(usize),
}

impl<T: ItemTrait> SmartBelt<T> {
    #[must_use]
    pub fn new(len: usize) -> Self {
        Self {
            marker: PhantomData {},
            first_free_index: FreeIndex::FreeIndex(0),
            zero_index: 0,
            locs: vec![false; len],
            inserters: vec![],
        }
    }

    #[inline(never)]
    pub fn update_inserters(&mut self, storages: &mut [ItemStorage<T>]) {
        // TODO: Update first_free_index

        let mut items_mut_iter = Self::items_mut(&mut self.locs, self.zero_index).into_iter();

        for (index, inserter) in &self.inserters {
            let loc = items_mut_iter.nth(usize::from(*index));

            match loc {
                Some(loc) => {
                    if *loc {
                        *loc = inserter.update(storages);
                    }
                },
                None => unreachable!(),
            }
        }

        coz::progress!("Inserter");
    }

    fn update_first_free_pos(&mut self, now_empty_pos: usize) {
        match self.first_free_index {
            FreeIndex::OldFreeIndex(index) | FreeIndex::FreeIndex(index) => {
                if now_empty_pos <= index {
                    self.first_free_index = FreeIndex::FreeIndex(now_empty_pos);
                }
            },
        }
    }

    #[inline(never)]
    fn find_and_update_real_first_free_index(&mut self) -> usize {
        let new_free_index = match self.first_free_index {
            FreeIndex::FreeIndex(index) => index,
            FreeIndex::OldFreeIndex(index) => {
                #[cfg(debug)]
                println!("HAD TO SEARCH FOR FIRST FREE INDEX!");

                let search_start_index = index;

                let mut iter = self
                    .locs
                    .iter()
                    .skip(self.zero_index)
                    .chain(self.locs.iter().take(self.zero_index))
                    .skip(search_start_index);

                debug_assert_eq!(iter.clone().count(), self.locs.len() - search_start_index);

                // We now have an iterator which is effectively the belt in the correct order,
                // starting at search_start_index

                iter.position(|x| !(*x))
                    .unwrap_or(self.locs.len() - search_start_index)
                    + search_start_index
            },
        };

        self.first_free_index = FreeIndex::FreeIndex(new_free_index);

        new_free_index
    }

    fn items_mut(locs: &mut [bool], zero_index: usize) -> impl IntoIterator<Item = &mut bool> {
        let len = locs.len();
        let (start, end) = locs.split_at_mut(zero_index % locs.len());

        end.iter_mut().chain(start.iter_mut()).take(len)
    }
}

impl<T: ItemTrait> Belt<T> for SmartBelt<T> {
    #[inline(never)]
    fn query_item(&self, pos: usize) -> Option<Item> {
        if self.locs[(self.zero_index + pos) % self.locs.len()] {
            Some(T::get_item())
        } else {
            None
        }
    }

    fn remove_item(&mut self, pos: usize) -> Option<Item> {
        self.update_first_free_pos(pos);
        self.query_item(pos).map(|item| {
            let len = self.locs.len();
            self.locs[(self.zero_index + pos) % len] = false;

            item
        })
    }

    fn try_insert_item(&mut self, pos: usize) -> Result<(), super::belt::NoSpaceError> {
        if self.query_item(pos).is_none() {
            let len = self.locs.len();
            self.locs[(self.zero_index + pos) % len] = true;

            // TODO: Check that the compiler realizes these are the same
            // Update first_free_index to show that it is old
            match self.first_free_index {
                FreeIndex::OldFreeIndex(free_index) | FreeIndex::FreeIndex(free_index) => {
                    if free_index == pos {
                        self.first_free_index = FreeIndex::OldFreeIndex(free_index);
                    }
                },
            }

            Ok(())
        } else {
            Err(NoSpaceError {})
        }
    }

    fn update(&mut self) {
        if self.query_item(0).is_none() {
            // Correctness: Since we always % len whenever we access using self.zero_index, we do not need to % len here
            // TODO: This could overflow after usize::MAX ticks which is 9749040289 Years. Should be fine!
            self.zero_index += 1;
            match self.first_free_index {
                FreeIndex::FreeIndex(0) | FreeIndex::OldFreeIndex(0) => {
                    if self.query_item(0).is_none() {
                        self.first_free_index = FreeIndex::FreeIndex(0);
                    } else {
                        self.first_free_index = FreeIndex::OldFreeIndex(0);
                    }
                },
                FreeIndex::FreeIndex(_) => {
                    unreachable!("FreeIndex should always point at the earliest known empty spot and we know that index 0 WAS an empty spot")
                },
                FreeIndex::OldFreeIndex(_) => {
                    unreachable!("OldFreeIndex should always point at the earliest potential empty spot and we know that index 0 WAS an empty spot")
                },
            }
            return;
        }

        self.zero_index %= self.locs.len();
        // println!("SLOW path");

        let first_free_index_real = self.find_and_update_real_first_free_index();

        let len = self.locs.len();

        let slice = self.locs.as_mut_slice();

        let (end_slice, start_slice) = slice.split_at_mut(self.zero_index);

        if self.zero_index + first_free_index_real >= len {
            // We have two stuck and one moving slice
            let (middle_stuck_slice, moving_slice) =
                end_slice.split_at_mut((self.zero_index + first_free_index_real) % len);

            // We can now either move the "moving_slice" to the left or the "stuck_slice" to the right and update the zero index
            // We choose whichever is shorter

            // TODO: Here it might sometimes be useful not to split at self.zero_index?

            if first_free_index_real < len / 2 {
                // Move the stuck_slice to the right
                // TODO: Zero length checking!

                if !start_slice.is_empty() && !middle_stuck_slice.is_empty() {
                    // TODO: Do I need to check if moving_slice.len() > 0?

                    // Step 1: Copy the empty spot into the stuck start_slice
                    mem::swap(
                        &mut middle_stuck_slice[middle_stuck_slice.len() - 1],
                        &mut moving_slice[0],
                    );

                    // Step 2: Rotate the middle_stuck_slice to the right moving the empty spot to the start
                    middle_stuck_slice.rotate_right(1);

                    debug_assert!(!middle_stuck_slice[0]);

                    // Step 3: Copy the empty spot into the stuck start_slice
                    mem::swap(
                        &mut start_slice[start_slice.len() - 1],
                        &mut middle_stuck_slice[0],
                    );

                    // Step 4: Rotate the start_slice to the right moving the empty spot to the start of the belt
                    start_slice.rotate_right(1);
                } else {
                    let non_empty_stuck_slice = if middle_stuck_slice.is_empty() {
                        start_slice
                    } else {
                        middle_stuck_slice
                    };

                    // Step 1: Copy the first empty spot into the stuck slice
                    mem::swap(
                        &mut non_empty_stuck_slice[non_empty_stuck_slice.len() - 1],
                        &mut moving_slice[0],
                    );

                    // Steps 2,3 are not necessary since we only have one stuck slice

                    // Step 4: Rotate the start_slice to the right moving the empty spot to the start of the belt
                    non_empty_stuck_slice.rotate_right(1);
                }

                // Step 5: Update zero_index (which currently points at the new empty spot at the start of the array) to now point one further,
                // effectively moving the empty spot to the end of the belt
                // Correctness: Since we always % len whenever we access using self.zero_index, we do not need to % len here
                // TODO: This could overflow after usize::MAX ticks which is 9749040289 Years. Should be fine!
                self.zero_index += 1;
            } else {
                // Move the moving_slice to the left
                if !moving_slice.is_empty() {
                    // This will rotate the first item to the end.
                    moving_slice.rotate_left(1);
                    // The first item of moving_slice is the first empty slot, therefore we automatically have the empty slot at the end
                    // moving_slice[moving_slice.len() - 1].item = None;
                }
            }
        } else {
            let (starting_stuck_slice, middle_moving_slice) =
                start_slice.split_at_mut(first_free_index_real);

            assert!(!middle_moving_slice.is_empty());
            assert!(!starting_stuck_slice.is_empty());

            if first_free_index_real < len / 2 {
                mem::swap(
                    &mut middle_moving_slice[0],
                    &mut starting_stuck_slice[starting_stuck_slice.len() - 1],
                );

                starting_stuck_slice.rotate_right(1);

                self.zero_index += 1;
            } else {
                middle_moving_slice.rotate_left(1);

                if !end_slice.is_empty() {
                    mem::swap(
                        &mut middle_moving_slice[middle_moving_slice.len() - 1],
                        &mut end_slice[0],
                    );

                    end_slice.rotate_left(1);
                }
            }
        }

        // Instead of finding the real first_free_index after the update, we just use OldFreeIndex since most likely an inserter
        // Will update it for us before the next update
        self.first_free_index = FreeIndex::OldFreeIndex(first_free_index_real);
    }

    fn get_len(&self) -> usize {
        self.locs.len()
    }
}

#[cfg(test)]
mod tests {

    extern crate test;

    use std::num::NonZeroU16;

    use proptest::{prelude::prop, prop_assert_eq, proptest};
    use test::Bencher;

    use crate::{belt::do_update_test_bools, item::Iron};

    use super::*;

    const MAX_LEN: usize = 5_000_000;
    proptest! {

        #[test]
        fn test_belt_moves_item_forward(item_pos in 0..MAX_LEN) {
            let mut belt = SmartBelt::<Iron>::new(MAX_LEN);

            let ret = belt.try_insert_item(item_pos);

            // Since the whole belt is empty, it should not fail to put an item in
            assert!(ret.is_ok());

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
        fn test_smart_belt_agrees_with_functional(mut items in prop::collection::vec(prop::bool::ANY, 1..100)) {
            let mut belt = SmartBelt::<Iron>::new(items.len());

            for (i, item_opt) in items.iter().enumerate() {

                if *item_opt {
                    belt.try_insert_item(i).expect("Since the belt starts empty this should never fail");
                } else {
                    belt.remove_item(i);
                }
            }

            for _update_count in 0..items.len() * 2 {
                belt.update();

                do_update_test_bools(&mut items);

                for (i, should_be_filled) in items.iter().enumerate() {
                    let correct = if *should_be_filled {
                        Some(Item::Iron)
                    } else {
                        None
                    };
                    prop_assert_eq!(belt.query_item(i), correct);
                }
            }
        }
    }

    #[test]
    fn test_smart_belt_does_not_set_free_index_wrong() {
        let mut belt = SmartBelt::<Iron>::new(MAX_LEN);

        // OutInserter::create_and_add(Arc::downgrade(&storage.storage), &mut belt, MAX_LEN - 1);

        belt.try_insert_item(0).expect("Expected insert to work");

        for _ in 0..1_000 {
            belt.update();
            belt.remove_item(1);
        }
    }

    #[test]
    fn test_smart_belt_with_inserters() {
        let mut belt = SmartBelt::<Iron>::new(10);

        let storage_unused = ItemStorage::<Iron>::default();

        let storage_source = ItemStorage::<Iron>::new(30);
        let storage_dest = ItemStorage::<Iron>::default();

        belt.inserters.push((
            5,
            Inserter::<Iron>::new(NonZeroU16::new(2).expect("Hardcoded")),
        ));

        let mut storages = [storage_unused, storage_source, storage_dest];

        for _ in 0..20 {
            belt.update();
            belt.update_inserters(&mut storages);
            // let _ = belt.remove_item(5);

            let _ = belt.try_insert_item(9);

            println!("{}", &belt as &dyn Belt<Iron>);
            println!("{:?}", storages[2]);
        }
    }

    #[bench]
    fn bench_smart_belt_with_inserters(b: &mut Bencher) {
        let mut belt = SmartBelt::<Iron>::new(MAX_LEN);

        let storage_unused = ItemStorage::<Iron>::default();

        let storage_source = ItemStorage::<Iron>::new(30);
        let storage_dest = ItemStorage::<Iron>::default();

        belt.inserters.push((
            5,
            Inserter::<Iron>::new(NonZeroU16::new(2).expect("Hardcoded")),
        ));

        let mut storages = [storage_unused, storage_source, storage_dest];

        b.iter(|| {
            belt.update();
            belt.update_inserters(&mut storages);

            let _ = belt.try_insert_item(9);
        });
    }

    #[bench]
    fn bench_smart_belt_with_10000_inserters(b: &mut Bencher) {
        let mut belt = SmartBelt::<Iron>::new(MAX_LEN);

        let storage_unused = ItemStorage::<Iron>::default();

        let storage_source = ItemStorage::<Iron>::new(30);
        let storage_dest = ItemStorage::<Iron>::default();

        for i in 0..MAX_LEN {
            belt.inserters.push((
                0,
                Inserter::<Iron>::new(NonZeroU16::new(2).expect("Hardcoded")),
            ));
        }

        let mut storages = [storage_unused, storage_source, storage_dest];

        b.iter(|| {
            for _ in 0..100 {
                belt.update();
                belt.update_inserters(&mut storages);

                let _ = belt.try_insert_item(9);
            }
        });
    }

    #[test]
    fn test_debug() {
        let mut belt = SmartBelt::<Iron>::new(MAX_LEN);

        for _ in 0..100_000 {
            let bb = test::black_box(&mut belt);
            bb.update();
        }
    }

    #[bench]
    fn bench_smart_belt_update_free_flowing(b: &mut Bencher) {
        let mut belt = SmartBelt::<Iron>::new(MAX_LEN);
        // belt.belt_storage
        //     .try_put_item_in_pos(Item::Iron, MAX_LEN - 1);

        b.iter(|| {
            let bb = test::black_box(&mut belt);
            bb.update();
        });

        // println!("{belt}");
    }

    #[bench]
    fn bench_smart_belt_update_stuck(b: &mut Bencher) {
        let mut belt = SmartBelt::<Iron>::new(MAX_LEN);
        belt.try_insert_item(MAX_LEN - 1)
            .expect("Expected insert to work");

        belt.try_insert_item(0).expect("Expected insert to work");

        b.iter(|| {
            let bb = test::black_box(&mut belt);
            bb.update();
        });

        // println!("{belt}");
    }

    #[bench]
    fn bench_smart_belt_worst_case(b: &mut Bencher) {
        let mut belt = SmartBelt::<Iron>::new(MAX_LEN);

        for i in 0..MAX_LEN / 2 {
            assert!(belt.try_insert_item(i).is_ok());
        }

        for _ in (MAX_LEN / 2)..MAX_LEN {
            // This spot is empty
        }

        b.iter(|| {
            let bb = test::black_box(&mut belt);
            bb.update();
        });

        // println!("{belt:?}");
    }
}
