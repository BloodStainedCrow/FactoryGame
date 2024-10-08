use std::{marker::PhantomData, mem, num::NonZero};

use crate::{
    inserter::BeltStorageInserter,
    item::{Item, ItemStorage, ItemTrait},
};

use super::belt::{Belt, NoSpaceError};
use crate::inserter::Dir;

#[allow(clippy::module_name_repetitions)]
#[derive(Debug)]
pub struct SmartBelt<T: ItemTrait> {
    marker: PhantomData<T>,
    first_free_index: FreeIndex,
    zero_index: usize,
    locs: Box<[bool]>,
    pub inserters: InserterStore<T>,
}

#[derive(Debug)]
pub struct InserterStore<T: ItemTrait> {
    pub offsets: Vec<u16>,
    pub out_inserters: Vec<BeltStorageInserter<T, { Dir::BeltToStorage }>>,
}

#[derive(Debug, PartialEq, Clone, Copy)]
enum FreeIndex {
    FreeIndex(usize),
    OldFreeIndex(usize),
}

const MIN_INSERTER_SPACING: usize = 8;

pub struct SpaceOccupiedError;

impl<T: ItemTrait> SmartBelt<T> {
    #[must_use]
    pub fn new(len: usize) -> Self {
        Self {
            marker: PhantomData {},
            first_free_index: FreeIndex::FreeIndex(0),
            zero_index: 0,
            locs: vec![false; len].into_boxed_slice(),
            inserters: InserterStore {
                offsets: vec![],
                out_inserters: vec![],
            },
        }
    }

    /// # Errors
    /// If the index is already used by another inserter
    /// # Panics
    /// If the index is greater or equal to the length of the belt
    pub fn add_out_inserter(
        &mut self,
        index: u16,
        storage_id: NonZero<u16>,
    ) -> Result<(), SpaceOccupiedError> {
        assert!(usize::from(index) < self.locs.len(), "Bounds check");

        let mut pos_after_last_inserter = 0;
        let mut i = 0;

        for offset in &self.inserters.offsets {
            let next_inserter_pos = pos_after_last_inserter + offset;

            match next_inserter_pos.cmp(&index) {
                std::cmp::Ordering::Greater => break, // This is the index to insert at
                std::cmp::Ordering::Equal => return Err(SpaceOccupiedError),

                std::cmp::Ordering::Less => {
                    pos_after_last_inserter = next_inserter_pos + 1;
                    i += 1;
                },
            }
        }

        // Insert at i
        let new_inserter_offset = index - pos_after_last_inserter;
        self.inserters.offsets.insert(i, new_inserter_offset);
        self.inserters
            .out_inserters
            .insert(i, BeltStorageInserter::new(storage_id));

        let next = self.inserters.offsets.get_mut(i + 1);

        if let Some(next_offs) = next {
            *next_offs -= new_inserter_offset + 1;
        }

        Ok(())
    }

    fn get_out_inserter(
        &self,
        index: u16,
    ) -> Option<&BeltStorageInserter<T, { Dir::BeltToStorage }>> {
        let mut pos_after_last_inserter = 0;

        // FIXME: There is a off by one here somewhere I am sure
        for (i, offset) in self.inserters.offsets.iter().enumerate() {
            let next_inserter_pos = pos_after_last_inserter + offset;

            match next_inserter_pos.cmp(&index) {
                std::cmp::Ordering::Equal => return Some(&self.inserters.out_inserters[i]),
                std::cmp::Ordering::Greater => return None,

                std::cmp::Ordering::Less => pos_after_last_inserter = next_inserter_pos + 1,
            }
        }

        None
    }

    #[inline(never)]
    pub fn update_inserters(&mut self, storages: &mut [ItemStorage<T>]) {
        // FIXME: This has a critical bug. FreeIndex does not get set correctly,
        // which could result in parts of the belt not working correctly
        debug_assert_eq!(
            self.inserters.out_inserters.len(),
            self.inserters.offsets.len()
        );
        let len = self.get_len();
        let mut items_mut_iter = Self::items_mut(&mut self.locs, self.zero_index).rev();

        // TODO: SENTINEL VALUE
        let mut new_empty_pos = len;
        let mut i = 0;
        let mut has_changed = false;

        for (offset, inserter) in self
            .inserters
            .offsets
            .iter()
            .zip(self.inserters.out_inserters.iter_mut())
        {
            i += usize::from(*offset);
            let loc = items_mut_iter.nth(usize::from(*offset));

            match loc {
                Some(loc) => {
                    let old = *loc;
                    inserter.update(loc, storages);

                    // FIXME: This is wrong, if we iterate inserters forward.
                    if !*loc {
                        new_empty_pos = i;
                    }
                    has_changed |= old != *loc;
                },
                None => unreachable!(
                    "Adding the offsets of the inserters is bigger than the length of the belt."
                ),
            }

            i += 1;
        }

        // Needed for the borrowchecker to not complain, since drop could modify self
        mem::drop(items_mut_iter);

        if new_empty_pos < usize::MAX {
            self.update_first_free_pos(new_empty_pos);
        } else {
            debug_assert!(!has_changed);
        }
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

    fn find_and_update_real_first_free_index(&mut self) -> usize {
        let new_free_index = match self.first_free_index {
            FreeIndex::FreeIndex(index) => index,
            FreeIndex::OldFreeIndex(index) => {
                // println!("HAD TO SEARCH FOR FIRST FREE INDEX!");

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

    fn items_mut(
        locs: &mut [bool],
        zero_index: usize,
    ) -> impl DoubleEndedIterator<Item = &mut bool> {
        // TODO: I have another implementation of this:
        // TODO: Check which is faster (or simpler)
        // let mut iter = self
        //     .locs
        //     .iter()
        //     .skip(self.zero_index)
        //     .chain(self.locs.iter().take(self.zero_index));
        let len = locs.len();
        let (start, end) = locs.split_at_mut(zero_index % locs.len());

        debug_assert_eq!(end.iter().chain(start.iter()).count(), len);
        end.iter_mut().chain(start.iter_mut())
    }

    #[must_use]
    /// # Panics
    /// If one of the belts is longer than `u16::MAX`
    pub fn join(front: Self, back: Self) -> Self {
        // TODO: currently we always join the belt, this can potentially result in lag spikes when attaching new pieces of belt to a
        // very long belt, because of having to copy a lot of data around. This could be solved by implementing a maximum length of belt,
        // and attaching two belts back to back when we exceed this length. This does require a lot of testing to make sure this break is
        // invisible though, so I will leave this for now and test if it is actually a problem.

        let front_len = front.get_len();
        let _back_len = back.get_len();

        // TODO: Check if maybe choosing the shorter belt somewhere could make this faster?
        // My guess is that splicing the one with the shorter tail (see Vec::splice) will be best

        let Self {
            marker: _front_marker,
            first_free_index: front_first_free_index,
            zero_index: front_zero_index,
            locs: front_locs,
            inserters: front_inserters,
        } = front;

        let Self {
            marker: _back_marker,
            first_free_index: _back_first_free_index,
            zero_index: back_zero_index,
            locs: back_locs,
            inserters: mut back_inserters,
        } = back;

        let num_front_inserters = front_inserters.offsets.len();
        let _num_back_inserters = back_inserters.offsets.len();

        let free_spots_before_last_inserter_front: u16 = front_inserters.offsets.iter().sum();
        let length_after_last_inserter = TryInto::<u16>::try_into(front_len)
            .expect("Belt should be max u16::MAX long")
            - free_spots_before_last_inserter_front
            - TryInto::<u16>::try_into(num_front_inserters)
                .expect("Belt should be max u16::MAX long");

        if let Some(offs) = back_inserters.offsets.get_mut(0) {
            *offs += length_after_last_inserter;
        }

        let mut new_inserters = front_inserters;
        new_inserters
            .out_inserters
            .append(&mut back_inserters.out_inserters);
        new_inserters.offsets.append(&mut back_inserters.offsets);

        let new_first_free_index = front_first_free_index;

        let mut front_locs_vec = Vec::from(front_locs);

        let back_loc_iter = back_locs
            .iter()
            .skip(back_zero_index)
            .chain(back_locs.iter().take(back_zero_index));

        let insert_pos = (front_zero_index + front_len) % (front_len + 1);

        front_locs_vec.splice(insert_pos..insert_pos, back_loc_iter.copied());

        Self {
            marker: PhantomData,
            first_free_index: new_first_free_index,
            zero_index: front_zero_index,
            locs: front_locs_vec.into_boxed_slice(),
            inserters: new_inserters,
        }
    }
}

impl<T: ItemTrait> Belt<T> for SmartBelt<T> {
    fn query_item(&self, pos: usize) -> Option<Item> {
        if self.locs[(self.zero_index + pos) % self.locs.len()] {
            Some(T::ITEM_ENUM)
        } else {
            None
        }
    }

    fn remove_item(&mut self, pos: usize) -> Option<Item> {
        self.update_first_free_pos(pos);
        self.query_item(pos).inspect(|_| {
            let len = self.locs.len();
            self.locs[(self.zero_index + pos) % len] = false;
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
            Err(NoSpaceError)
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

        let slice = &mut self.locs;

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

    use proptest::{prelude::prop, prop_assert, prop_assert_eq, proptest};
    use test::Bencher;

    use crate::{belt::do_update_test_bools, item::Iron};

    use super::*;

    const MAX_LEN: usize = 5_000;
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
                    assert!(belt.remove_item(i).is_none());
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

        #[test]
        fn test_join_belt_length(front_len in 0..MAX_LEN, back_len in 0..MAX_LEN) {
            let back = SmartBelt::<Iron>::new(back_len);
            let front = SmartBelt::<Iron>::new(front_len);

            prop_assert_eq!(SmartBelt::join(front, back).get_len(), front_len + back_len);
        }

        #[test]
        fn test_join_belt_items(front_items in prop::collection::vec(prop::bool::ANY, 1..100), back_items in prop::collection::vec(prop::bool::ANY, 1..100)) {
            let mut back = SmartBelt::<Iron>::new(back_items.len());
            let mut front = SmartBelt::<Iron>::new(front_items.len());

            for (i, item) in front_items.iter().enumerate() {
                if *item {
                    assert!(front.try_insert_item(i).is_ok());
                }
            }

            for (i, item) in back_items.iter().enumerate() {
                if *item {
                    assert!(back.try_insert_item(i).is_ok());
                }
            }

            let new_belt = SmartBelt::join(front, back);

            for (i, item) in front_items.iter().chain(back_items.iter()).enumerate() {
                prop_assert_eq!(new_belt.query_item(i).is_some(), *item, "{:?}", new_belt);
            }
        }

        #[test]
        fn test_join_belt_first_free_index(front_items in prop::collection::vec(prop::bool::ANY, 1..100), back_items in prop::collection::vec(prop::bool::ANY, 1..100)) {
            let mut back = SmartBelt::<Iron>::new(back_items.len());
            let mut front = SmartBelt::<Iron>::new(front_items.len());

            for (i, item) in front_items.iter().enumerate() {
                if *item {
                    assert!(front.try_insert_item(i).is_ok());
                }
            }

            for (i, item) in back_items.iter().enumerate() {
                if *item {
                    assert!(back.try_insert_item(i).is_ok());
                }
            }

            let new_belt = SmartBelt::join(front, back);

            let Some((index, _)) = front_items.iter().chain(back_items.iter()).enumerate().find(|(_i, item)| !**item) else {
                return Ok(());
            };

            match new_belt.first_free_index {
                FreeIndex::FreeIndex(join_index) => prop_assert_eq!(join_index, index),
                FreeIndex::OldFreeIndex(join_index) => prop_assert!(join_index <= index),
            }
        }

        #[test]
        fn test_join_belt_inserters(front_inserters in prop::collection::vec(prop::bool::ANY, 1..100), back_inserters in prop::collection::vec(prop::bool::ANY, 1..100)) {
            let mut back = SmartBelt::<Iron>::new(back_inserters.len());
            let mut front = SmartBelt::<Iron>::new(front_inserters.len());

            for (i, inserter) in front_inserters.iter().enumerate() {
                let Some(i) = NonZeroU16::new(i.try_into().expect("Should fit")) else {
                    continue;
                };

                if *inserter {
                    assert!(front.add_out_inserter(i.into(), i).is_ok());
                }
            }

            for (i, inserter) in back_inserters.iter().enumerate() {
                let Some(i) = NonZeroU16::new(i.try_into().expect("Should fit")) else {
                    continue;
                };

                if *inserter {
                    dbg!(i);
                    assert!(back.add_inserter(i.into(), i).is_ok());
                }
            }

            let new_belt = SmartBelt::join(front, back);

            for (i, (storage_i, inserter)) in front_inserters.iter().enumerate().chain(back_inserters.iter().enumerate()).enumerate() {
                let Some(storage_i) = NonZeroU16::new(storage_i.try_into().expect("Should fit")) else {
                    continue;
                };

                match new_belt.get_inserter(i.try_into().expect("Hardcoded")) {
                    Some(ins) => prop_assert_eq!(ins.storage_id, storage_i, "{:?}", new_belt),
                    None => prop_assert!(!*inserter, "{:?}", new_belt),
                }
            }
        }

        #[test]
        fn test_add_inserter(inserters in prop::collection::vec(prop::bool::ANY, 1..100)) {
            let mut belt = SmartBelt::<Iron>::new(inserters.len());

            for (i, inserter) in inserters.iter().enumerate() {
                let Some(i) = NonZeroU16::new(i.try_into().expect("Should fit")) else {
                    continue;
                };

                if *inserter {
                    prop_assert!(belt.add_inserter(i.into(), i).is_ok());
                }
            }

            for (i, inserter) in inserters.iter().enumerate() {
                let Some(i) = NonZeroU16::new(i.try_into().expect("Should fit")) else {
                    continue;
                };

                match belt.get_inserter(i.into()) {
                    Some(ins) => prop_assert_eq!(ins.storage_id, i, "{:?}", belt),
                    None => prop_assert!(!*inserter, "{:?}", belt),
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
        let mut belt = SmartBelt::<Iron>::new(10_000);

        let storage_unused = ItemStorage::<Iron>::default();

        let storage_source = ItemStorage::<Iron>::new(30);
        let storage_dest = ItemStorage::<Iron>::default();

        belt.inserters.offsets.push(5);
        belt.inserters
            .out_inserters
            .push(BeltStorageInserter::<Iron>::new(
                NonZeroU16::new(2).expect("Hardcoded"),
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

        belt.inserters.offsets.push(5);
        belt.inserters
            .out_inserters
            .push(BeltStorageInserter::<Iron>::new(
                NonZeroU16::new(2).expect("Hardcoded"),
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

        for _ in 0..10_000 {
            belt.inserters.offsets.push(0);
            belt.inserters
                .out_inserters
                .push(BeltStorageInserter::<Iron>::new(
                    NonZeroU16::new(2).expect("Hardcoded"),
                ));
        }

        let mut storages: Vec<ItemStorage<Iron>> =
            [storage_unused.clone(), storage_source, storage_dest].into();

        for _ in 0..100_000 {
            storages.push(storage_unused.clone());
        }

        b.iter(|| {
            belt.update();
            belt.update_inserters(&mut storages);

            let _ = belt.try_insert_item(9);
        });
    }

    #[bench]
    fn bench_smart_belt_with_10000_inserters_belt_empty(b: &mut Bencher) {
        let mut belt = SmartBelt::<Iron>::new(MAX_LEN);

        let storage_unused = ItemStorage::<Iron>::default();

        let storage_source = ItemStorage::<Iron>::new(0);
        let storage_dest = ItemStorage::<Iron>::default();

        for _ in 0..10_000 {
            belt.inserters.offsets.push(0);
            belt.inserters
                .out_inserters
                .push(BeltStorageInserter::<Iron>::new(
                    NonZeroU16::new(2).expect("Hardcoded"),
                ));
        }

        let mut storages: Vec<ItemStorage<Iron>> =
            [storage_unused.clone(), storage_source, storage_dest].into();

        for _ in 0..100_000 {
            storages.push(storage_unused.clone());
        }

        b.iter(|| {
            belt.update();
            belt.update_inserters(&mut storages);
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
    fn bench_smart_belt_update_full(b: &mut Bencher) {
        let mut belt = SmartBelt::<Iron>::new(MAX_LEN);

        for i in 0..MAX_LEN {
            assert!(belt.try_insert_item(i).is_ok());
        }

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

        // Insert a single item at the end
        assert!(belt.try_insert_item(MAX_LEN - 1).is_ok());

        b.iter(|| {
            let bb = test::black_box(&mut belt);
            bb.update();
        });

        // println!("{belt:?}");
        let mut num_items = 0;
        let mut last_spot_with_items: Option<usize> = None;
        for i in 0..MAX_LEN {
            if belt.query_item(i).is_some() {
                num_items += 1;
                last_spot_with_items = Some(i);
            }
        }
        println!("{num_items}");
        println!("{last_spot_with_items:?}");
    }

    #[bench]
    fn bench_extend_belt_by_one_front_1_000(b: &mut Bencher) {
        let mut belt = Some(SmartBelt::<Iron>::new(1_000));

        b.iter(move || {
            let back = belt.take().expect("Hardcoded");
            let front = SmartBelt::new(1);

            belt = Some(SmartBelt::join(front, back));
        });
    }

    #[bench]
    fn bench_extend_belt_by_one_back_1_000(b: &mut Bencher) {
        let mut belt = Some(SmartBelt::<Iron>::new(1_000));

        b.iter(move || {
            let front = belt.take().expect("Hardcoded");
            let back = SmartBelt::new(1);

            belt = Some(SmartBelt::join(front, back));
        });
    }

    #[bench]
    fn bench_extend_belt_by_one_front_1_000_000(b: &mut Bencher) {
        let mut belt = Some(SmartBelt::<Iron>::new(1_000_000));

        b.iter(move || {
            let back = belt.take().expect("Hardcoded");
            let front = SmartBelt::new(1);

            belt = Some(SmartBelt::join(front, back));
        });
    }

    #[bench]
    fn bench_extend_belt_by_one_back_1_000_000(b: &mut Bencher) {
        let mut belt = Some(SmartBelt::<Iron>::new(1_000_000));

        b.iter(move || {
            let front = belt.take().expect("Hardcoded");
            let back = SmartBelt::new(1);

            belt = Some(SmartBelt::join(front, back));
        });
    }
}
