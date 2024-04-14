use std::{fmt::Display, mem};

use crate::item::Item;

use super::{
    in_inserter::InInserter,
    out_inserter::OutInserter,
    splitter::{SplitterInserterIn, SplitterInserterOut},
};

#[allow(clippy::module_name_repetitions)]
#[derive(Debug)]
pub struct SmartBelt {
    belt_storage: SmartBeltStorage,
    connected_out_inserters: Vec<OutInserter>,
    connected_in_inserters: Vec<InInserter>,
    splitter_inserter_in: Option<SplitterInserterIn>,
    splitter_inserter_out: Option<SplitterInserterOut>,
}

impl Display for SmartBelt {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let mut s = String::new();

        for i in 0..self.belt_storage.locs.len() {
            if self
                .belt_storage
                .get_item_from_pos(i.try_into().expect("Expected usize to fit into u32"))
                .is_some()
            {
                s.push('I');
            } else {
                s.push('.');
            }
        }

        write!(f, "{s}")
    }
}

#[derive(Debug)]
pub(super) struct SmartBeltStorage {
    first_free_index: FreeIndex,
    zero_index: usize,
    locs: Vec<Option<Item>>,
}

#[derive(Debug, PartialEq)]
enum FreeIndex {
    FreeIndex(usize),
    OldFreeIndex(Option<usize>),
}

impl SmartBeltStorage {
    #[must_use]
    pub fn new(len: u32) -> Self {
        Self {
            first_free_index: FreeIndex::FreeIndex(0),
            zero_index: 0,
            locs: vec![None; len as usize],
        }
    }

    pub fn update(&mut self) {
        if self.query_item(0).is_none() {
            // Correctness: Since we always % len whenever we access using self.zero_index, we do not need to % len here
            // TODO: This could overflow after usize::MAX ticks which is 9749040289 Years. Should be fine!
            self.zero_index += 1;
            match self.first_free_index {
                FreeIndex::FreeIndex(0) => self.first_free_index = FreeIndex::OldFreeIndex(None),
                FreeIndex::FreeIndex(index) => {
                    self.first_free_index = FreeIndex::FreeIndex(index - 1);
                },
                FreeIndex::OldFreeIndex(Some(index)) => {
                    self.first_free_index = FreeIndex::OldFreeIndex(Some(index - 1));
                },
                FreeIndex::OldFreeIndex(None) => {},
            }
            // println!("Fast path");
            return;
        }

        self.zero_index %= self.locs.len();
        // println!("SLOW path");

        let first_free_index_real = match self.first_free_index {
            FreeIndex::FreeIndex(index) => index,
            FreeIndex::OldFreeIndex(index) => {
                #[cfg(debug)]
                println!("HAD TO SEARCH FOR FIRST FREE INDEX!");

                let search_start_index = index.unwrap_or(0);

                let mut iter = self
                    .locs
                    .iter()
                    .skip(self.zero_index)
                    .chain(self.locs.iter().take(self.zero_index))
                    .skip(search_start_index);

                debug_assert_eq!(iter.clone().count(), self.locs.len() - search_start_index);

                // We now have an iterator which is effectively the belt in the correct order,
                // starting at search_start_index

                iter.position(Option::is_none)
                    .unwrap_or(self.locs.len() - search_start_index)
                    + search_start_index
            },
        };

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

                    debug_assert!(middle_stuck_slice[0].is_none());

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
        self.first_free_index = FreeIndex::OldFreeIndex(Some(first_free_index_real));
    }

    fn query_item(&self, pos: u32) -> Option<Item> {
        self.locs[(self.zero_index + pos as usize) % self.locs.len()]
    }

    fn set_item(&mut self, pos: u32, item: Option<Item>) {
        let len = self.locs.len();
        self.locs[(self.zero_index + pos as usize) % len] = item;
    }

    pub fn try_put_item_in_pos(&mut self, item: Item, pos: u32) -> bool {
        if self.query_item(pos).is_none() {
            self.set_item(pos, Some(item));

            // Update first_free_index to show that it is old
            match self.first_free_index {
                FreeIndex::FreeIndex(free_index) => {
                    if free_index == pos.try_into().expect("Expected u32 to fit into usize") {
                        self.first_free_index = FreeIndex::OldFreeIndex(
                            free_index
                                .try_into()
                                .expect("Expected usize to fit into isize"),
                        );
                    }
                },
                FreeIndex::OldFreeIndex(Some(old_free_index)) => {
                    if old_free_index == pos.try_into().expect("Expected u32 to fit into usize") {
                        unreachable!("If some spot is free, it should always be FreeIndex(index) and never OldFreeIndex(index)")
                    }
                },
                FreeIndex::OldFreeIndex(None) => {},
            }

            true
        } else {
            false
        }
    }

    pub fn try_take_item_from_pos(&mut self, item: Item, pos: u32) -> bool {
        if Some(item) == self.query_item(pos) {
            self.set_item(pos, None);

            self.update_first_free_pos(pos);

            true
        } else {
            false
        }
    }

    fn update_first_free_pos(&mut self, now_empty_pos: u32) {
        match self.first_free_index {
            FreeIndex::FreeIndex(index) =>
            {
                #[allow(clippy::cast_possible_truncation)]
                if now_empty_pos <= index as u32 {
                    self.first_free_index = FreeIndex::FreeIndex(
                        now_empty_pos
                            .try_into()
                            .expect("Expected u32 to fit into usize"),
                    );
                }
            },
            FreeIndex::OldFreeIndex(Some(index)) => {
                if now_empty_pos
                    <= index
                        .try_into()
                        .expect("Expected free_index to fit into u32")
                {
                    self.first_free_index = FreeIndex::FreeIndex(
                        now_empty_pos
                            .try_into()
                            .expect("Expected u32 to fit into usize"),
                    );
                }
            },
            FreeIndex::OldFreeIndex(None) => {
                if now_empty_pos == 0 {
                    self.first_free_index = FreeIndex::FreeIndex(0);
                }
            },
        }
    }

    pub fn get_item_from_pos(&self, pos: u32) -> Option<Item> {
        self.query_item(pos)
    }

    pub fn get_item_from_pos_and_remove(&mut self, pos: u32) -> Option<Item> {
        self.update_first_free_pos(pos);
        self.query_item(pos).map(|item| {
            self.set_item(pos, None);

            item
        })
    }

    pub fn check_for_space(&self, pos: u32) -> bool {
        self.query_item(pos).is_none()
    }

    #[allow(clippy::cast_possible_truncation)]
    pub fn get_belt_len(&self) -> u32 {
        self.locs.len() as u32
    }
}

impl SmartBelt {
    #[must_use]
    pub fn new(len: u32) -> Self {
        Self {
            belt_storage: SmartBeltStorage::new(len),
            connected_out_inserters: vec![],
            connected_in_inserters: vec![],
            splitter_inserter_in: None,
            splitter_inserter_out: None,
        }
    }

    pub fn update(&mut self) {
        // if let Some(inserter) = &mut self.splitter_inserter_in {
        //     inserter.update_simple(&mut self.belt_storage);
        // }

        self.belt_storage.update();

        // if let Some(inserter) = &mut self.splitter_inserter_out {
        //     inserter.update_simple(&mut self.belt_storage);
        // }

        // for inserter in &mut self.connected_out_inserters {
        //     inserter.update_belt_smart(&mut self.belt_storage);
        // }

        // for inserter in &mut self.connected_in_inserters {
        //     inserter.update_belt(&mut self.belt_storage);
        // }
    }

    pub fn add_out_inserter(&mut self, inserter: OutInserter) {
        self.connected_out_inserters.push(inserter);
    }

    pub fn add_in_inserter(&mut self, inserter: InInserter) {
        self.connected_in_inserters.push(inserter);
    }

    pub(super) fn add_splitter_inserter_in(&mut self, inserter: SplitterInserterIn) {
        self.splitter_inserter_in = Some(inserter);
    }

    pub(super) fn add_splitter_inserter_out(&mut self, inserter: SplitterInserterOut) {
        self.splitter_inserter_out = Some(inserter);
    }

    #[must_use]
    pub fn get_item_at(&self, pos: u32) -> Option<Item> {
        self.belt_storage.query_item(pos)
    }
}

#[cfg(test)]
mod tests {

    extern crate test;
    use std::{array, sync::Arc};

    use proptest::{prelude::prop, prop_assert_eq, proptest};
    use test::Bencher;

    use crate::{
        belt::do_update_test,
        item::{option, Iron},
        specialized_storage::SpecializedStorage,
    };

    use super::*;

    const MAX_LEN: u32 = 10_000;
    proptest! {

        #[test]
        fn test_belt_moves_item_forward(item_pos in 0..MAX_LEN) {
            let mut belt = SmartBelt::new(MAX_LEN);

            let ret = belt.belt_storage.try_put_item_in_pos(Item::Iron, item_pos);

            // Since the whole belt is empty, it should not fail to put an item in
            assert!(ret);

            belt.update();

            if item_pos > 0 {
                // The item should have moved
                for i in 0..MAX_LEN {
                    if i == item_pos - 1 {
                        prop_assert_eq!(belt.get_item_at(i), Some(Item::Iron));
                    } else {
                        prop_assert_eq!(belt.get_item_at(i), None);
                    }
                }
            } else {
                // The item should NOT have moved
                for i in 0..MAX_LEN {
                    if i == item_pos {
                        prop_assert_eq!(belt.get_item_at(i), Some(Item::Iron));
                    } else {
                        prop_assert_eq!(belt.get_item_at(i), None);
                    }
                }
            }
        }

        #[test]
        fn test_smart_belt_agrees_with_functional(mut items in prop::collection::vec(option(), 1..100)) {
            let mut belt = SmartBelt::new(items.len().try_into().expect("Size does not fit into usize"));

            for (i, item_opt) in items.iter().enumerate() {
                match item_opt {
                    Some(item) => {
                        belt.belt_storage.try_put_item_in_pos(*item, i.try_into().expect("Size does not fit into usize"));
                    },
                    None => {
                        belt.belt_storage.get_item_from_pos_and_remove(i.try_into().expect("Size does not fit into usize"));
                    },
                };
            }

            for _update_count in 0..items.len() * 2 {
                belt.update();

                do_update_test(&mut items);

                for (i, item) in items.iter().enumerate() {
                    prop_assert_eq!(belt.belt_storage.get_item_from_pos(i.try_into().expect("Size does not fit into usize")), *item);
                }
            }
        }
    }

    #[test]
    fn test_smart_belt_does_not_set_free_index_wrong() {
        let mut belt = SmartBelt::new(MAX_LEN);

        let storage = SpecializedStorage::<Iron>::new();

        storage
            .storage
            .count
            .store(1_000_000_000, std::sync::atomic::Ordering::Relaxed);

        OutInserter::create_and_add_strict_smart(
            Arc::downgrade(&storage.storage),
            &mut belt,
            MAX_LEN - 1,
        );

        belt.belt_storage.try_put_item_in_pos(Item::Iron, 0);

        for _ in 0..1_000 {
            belt.update();
            belt.belt_storage.get_item_from_pos_and_remove(1);
        }
    }

    #[test]
    fn test_debug() {
        let mut belt = SmartBelt::new(MAX_LEN);

        for _ in 0..100_000 {
            let bb = test::black_box(&mut belt);
            // for _ in 0..1_000 {
            bb.update();
            // println!("{bb}");
            // }
        }
    }

    #[bench]
    fn bench_smart_belt_update_free_flowing(b: &mut Bencher) {
        let mut belt = SmartBelt::new(MAX_LEN);
        // belt.belt_storage
        //     .try_put_item_in_pos(Item::Iron, MAX_LEN - 1);

        b.iter(|| {
            let bb = test::black_box(&mut belt);
            // for _ in 0..1_000 {
            bb.update();
            // println!("{bb}");
            // }
        });

        // println!("{belt}");
    }

    #[bench]
    fn bench_smart_belt_update_stuck(b: &mut Bencher) {
        let mut belt = SmartBelt::new(MAX_LEN);
        belt.belt_storage
            .try_put_item_in_pos(Item::Iron, MAX_LEN - 1);

        belt.belt_storage.try_put_item_in_pos(Item::Iron, 0);

        b.iter(|| {
            let bb = test::black_box(&mut belt);
            // for _ in 0..1_000 {
            bb.update();
            // println!("{bb}");
            // }
        });

        // println!("{belt}");
    }

    #[bench]
    fn bench_smart_belt_worst_case(b: &mut Bencher) {
        let mut belt = SmartBelt::new(MAX_LEN);

        for i in 0..MAX_LEN / 2 {
            assert!(belt.belt_storage.try_put_item_in_pos(Item::Iron, i));
        }

        for _ in MAX_LEN / 2..MAX_LEN {
            // This spot is empty
        }

        b.iter(|| {
            let bb = test::black_box(&mut belt);
            bb.update();
        });

        // println!("{belt:?}");
    }
}
