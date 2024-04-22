use std::fmt::Display;

use crate::item::Item;

use super::{
    belt::{Belt, NoSpaceError},
    in_inserter::InInserter,
    out_inserter::OutInserter,
    splitter::{SplitterInserterIn, SplitterInserterOut},
};

#[derive(Debug, Clone, Copy)]
struct ItemLocation {
    item: Option<Item>,
}

// TODO: Maybe add a first_full_index aswell?
#[allow(clippy::module_name_repetitions)]
pub struct SimpleBelt {
    pub(super) belt_storage: BeltStorage,
    connected_out_inserters: Vec<OutInserter>,
    connected_in_inserters: Vec<InInserter>,
    splitter_inserter_in: Option<SplitterInserterIn>,
    splitter_inserter_out: Option<SplitterInserterOut>,
}

impl Display for SimpleBelt {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let mut s = String::new();

        for i in 0..self.belt_storage.locs.len() {
            if self
                .belt_storage
                .get_item_from_pos(i.try_into().expect("Expect usize to fit into u32"))
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
pub(super) struct BeltStorage {
    first_free_index: usize,
    locs: Vec<ItemLocation>,
}

impl BeltStorage {
    #[must_use]
    pub fn new(len: u32) -> Self {
        Self {
            first_free_index: 0,
            locs: vec![ItemLocation { item: None }; len as usize],
        }
    }

    pub fn update(&mut self) {
        let slice = self.locs.as_mut_slice();

        let (_stuck_slice, moving_slice) = slice.split_at_mut(self.first_free_index);

        if !moving_slice.is_empty() {
            // This will rotate the first item to the end.
            moving_slice.rotate_left(1);
            // The first item of moving_slice is the first empty slot, therefore we automatically have the empty slot at the end
            // moving_slice[moving_slice.len() - 1].item = None;

            // We need to update first_free_index
            // TODO: This is a stupid way to do it
            let old = self.first_free_index;
            while self.first_free_index - old < moving_slice.len()
                && moving_slice[self.first_free_index - old].item.is_some()
            {
                self.first_free_index += 1;
            }
        }
    }

    pub fn try_put_item_in_pos(&mut self, item: Item, pos: usize) -> bool {
        if self.locs[pos as usize].item.is_none() {
            self.locs[pos as usize].item = Some(item);

            // TODO: Write a test for this!
            // Update first_free_index
            if self.first_free_index == pos as usize {
                let (_left, right) = self.locs.split_at(pos as usize);

                let index_in_right = right.iter().position(|elem| elem.item.is_none());

                let index_total = index_in_right.unwrap_or(right.len()) + pos as usize;

                self.first_free_index = index_total;
            }

            true
        } else {
            false
        }
    }

    pub fn try_take_item_from_pos(&mut self, item: Item, pos: usize) -> bool {
        if Some(item) == self.locs[pos as usize].item {
            self.locs[pos as usize].item = None;

            if self.first_free_index > pos as usize {
                self.first_free_index = pos as usize;
            }

            true
        } else {
            false
        }
    }

    pub fn get_item_from_pos(&self, pos: usize) -> Option<Item> {
        self.locs[pos as usize].item
    }

    pub fn get_item_from_pos_and_remove(&mut self, pos: usize) -> Option<Item> {
        if let Some(item) = self.locs[pos as usize].item {
            self.locs[pos as usize].item = None;

            if self.first_free_index > pos as usize {
                self.first_free_index = pos as usize;
            }

            Some(item)
        } else {
            None
        }
    }

    pub fn check_for_space(&self, pos: usize) -> bool {
        self.locs[pos].item.is_none()
    }

    pub fn get_belt_len(&self) -> usize {
        self.locs.len()
    }
}

impl SimpleBelt {
    #[must_use]
    pub fn new(len: u32) -> Self {
        Self {
            belt_storage: BeltStorage::new(len),
            connected_out_inserters: vec![],
            connected_in_inserters: vec![],
            splitter_inserter_in: None,
            splitter_inserter_out: None,
        }
    }
}

impl Belt for SimpleBelt {
    fn query_item(&self, pos: usize) -> Option<Item> {
        self.belt_storage.locs[pos].item
    }

    fn remove_item(&mut self, pos: usize) -> Option<Item> {
        self.belt_storage.get_item_from_pos_and_remove(pos)
    }

    fn try_insert_item(&mut self, pos: usize, item: Item) -> Result<(), super::belt::NoSpaceError> {
        if self.belt_storage.try_put_item_in_pos(item, pos) {
            Ok(())
        } else {
            Err(NoSpaceError {})
        }
    }

    fn add_in_inserter(&mut self, in_inserter: InInserter) {
        self.connected_in_inserters.push(in_inserter);
    }

    fn add_out_inserter(&mut self, out_inserter: OutInserter) {
        self.connected_out_inserters.push(out_inserter);
    }

    fn get_len(&self) -> usize {
        self.belt_storage.get_belt_len()
    }

    fn update(&mut self) {
        self.belt_storage.update();

        // for inserter in &self.connected_out_inserters {
        //     inserter.update_belt(self);
        // }

        for inserter in &mut self.connected_in_inserters {
            inserter.update_belt(&mut self.belt_storage);
        }
    }
}

#[cfg(test)]
mod tests {
    use crate::{belt::do_update_test, item::option};

    use super::*;
    use proptest::{prelude::prop, prop_assert_eq, proptest};
    extern crate test;
    use test::Bencher;

    proptest! {
        #[test]
        fn test_constructing_belt_does_not_panic(len in 0..10_000u32) {
            // TODO: This only checks a reasonable range since Belt is so inefficient with memory
            let _ = SimpleBelt::new(len);
        }

        #[test]
        fn test_simple_belt_agrees_with_functional(mut items in prop::collection::vec(option(), 1..1_000)) {
            let mut belt = SimpleBelt::new(items.len().try_into().expect("Size does not fit into usize"));

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

            for _ in 0..items.len() {
                belt.update();

                do_update_test(&mut items);

                for (i, item) in items.iter().enumerate() {
                    prop_assert_eq!(belt.belt_storage.get_item_from_pos(i.try_into().expect("Size does not fit into usize")), *item);
                }
            }
        }
    }

    #[bench]
    fn bench_empty_belt_update(b: &mut Bencher) {
        let mut belt = SimpleBelt::new(10_000);

        let mut i = 0;

        b.iter(|| {
            let bb = test::black_box(&mut belt);
            for _ in 0..1_000 {
                bb.update();
                i += 1;
            }
        });
    }

    #[bench]
    fn bench_belt_update(b: &mut Bencher) {
        let mut belt = SimpleBelt::new(10_000);
        belt.belt_storage
            .try_put_item_in_pos(Item::Iron, 10_000 - 1);

        let mut i = 0;

        b.iter(|| {
            let bb = test::black_box(&mut belt);
            bb.update();
            i += 1;
            // println!("{bb}");
        });

        println!("{belt}");
    }

    #[bench]
    fn bench_belt_update_minimum(b: &mut Bencher) {
        let mut belt = SimpleBelt::new(1);

        let mut i = 0;

        b.iter(|| {
            let bb = test::black_box(&mut belt);
            for _ in 0..1_000 {
                bb.update();
                bb.belt_storage.get_item_from_pos_and_remove(0);
                i += 1;
                // println!("{bb}");
            }
        });

        println!("{belt}");
    }
}
