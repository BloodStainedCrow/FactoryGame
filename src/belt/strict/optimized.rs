use std::{fmt::Display, marker::PhantomData};

use crate::item::{get_char, Item, ItemTrait};

use super::{in_inserter::InInserterStrict, out_inserter::OutInserterStrict};

// This type of Belt is used when it only holds one kind of item
#[derive(Debug)]
#[allow(clippy::module_name_repetitions)]
pub struct OptimizedBelt<T: ItemTrait> {
    belt_storage: OptimizedBeltStorage<T>,
    connected_out_inserters: Vec<OutInserterStrict<T>>,
    connected_in_inserters: Vec<InInserterStrict<T>>,
}

#[derive(Debug)]
pub(super) struct OptimizedBeltStorage<T: ItemTrait> {
    marker: PhantomData<T>,
    first_spot_has_item: bool,
    data: Vec<u32>,
}

impl<T: ItemTrait> Display for OptimizedBelt<T> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let mut s = String::new();

        let mut current_state = self.belt_storage.first_spot_has_item;

        for len in &self.belt_storage.data {
            for _ in 0..*len {
                if current_state {
                    s.push(get_char(T::get_item()));
                } else {
                    s.push('.');
                }
            }

            current_state = !current_state;
        }

        write!(f, "{s}")
    }
}

impl<T: ItemTrait> OptimizedBelt<T> {
    #[must_use]
    pub fn new(len: u32) -> Self {
        Self {
            belt_storage: OptimizedBeltStorage::<T>::new(len),
            connected_out_inserters: vec![],
            connected_in_inserters: vec![],
        }
    }

    pub fn update(&mut self) {
        self.belt_storage.update();

        for inserter in &mut self.connected_out_inserters {
            inserter.update_optimized_belt(&mut self.belt_storage);
        }

        for inserter in &mut self.connected_in_inserters {
            inserter.update_optimized_belt(&mut self.belt_storage);
        }
    }

    pub fn add_out_inserter(&mut self, inserter: OutInserterStrict<T>) {
        self.connected_out_inserters.push(inserter);
    }

    pub fn add_in_inserter(&mut self, inserter: InInserterStrict<T>) {
        self.connected_in_inserters.push(inserter);
    }

    #[must_use]
    pub fn get_item_at(&self, pos: u32) -> Option<Item> {
        let mut current = self.belt_storage.first_spot_has_item;
        let mut count = 0;

        for section_len in &self.belt_storage.data {
            count += section_len;
            if count > pos {
                break;
            }
            current = !current;
        }

        if current {
            Some(T::get_item())
        } else {
            None
        }
    }
}

impl<T: ItemTrait> OptimizedBeltStorage<T> {
    #[must_use]
    pub fn new(len: u32) -> Self {
        Self {
            first_spot_has_item: false,
            data: vec![len],
            marker: PhantomData,
        }
    }

    pub fn update(&mut self) {
        if self.first_spot_has_item {
            if self.data.len() <= 2 {
                // The Belt has layout like OOOOO....
                // or                       OOOOOOOOO
                // So nothing to do
            } else {
                // This cannot underflow because all elements of data must be at least one!
                self.data[1] -= 1;

                if self.data[1] == 0 {
                    // Merge the two groups
                    self.data[0] += self.data[2];
                    // Remove the now zero "empty" element and the old second group of items
                    self.data.drain(1..=2);
                }

                let end_section_has_items = (self.data.len() % 2 == 0) ^ self.first_spot_has_item;

                if end_section_has_items {
                    self.data.push(1);
                } else {
                    let len = self.data.len();
                    self.data[len - 1] += 1;
                }
            }
        } else if self.data.len() < 2 {
            // The Belt has layout like ........
            // So nothing to do
        } else {
            // This cannot underflow because all elements of data must be at least one!
            self.data[0] -= 1;

            if self.data[0] == 0 {
                // Merge the two groups
                self.data[0] += self.data[1];
                // Remove the now zero "empty" element and indicate that the first slot has items
                self.data.remove(1);
                self.first_spot_has_item = true;
            }

            let end_section_has_items = (self.data.len() % 2 == 0) ^ self.first_spot_has_item;

            if end_section_has_items {
                self.data.push(1);
            } else {
                let len = self.data.len();
                self.data[len - 1] += 1;
            }
        }

        debug_assert!(self.data.iter().all(|group_len| *group_len > 0));
    }

    // TODO: Write tests for this
    pub fn try_put_item_in_pos(&mut self, pos: u32) -> bool {
        let mut old_count = 0;
        let mut count = 0;
        let mut i = 0;

        while count <= pos {
            old_count = count;
            count += self.data[i];
            i += 1;
        }

        // i now stores the index into the data array of the chunk AFTER the one that contains the pos

        let spot_full: bool = (i % 2 == 0) ^ self.first_spot_has_item;

        if !spot_full {
            // data[i] is a section containing items

            // Three possibilities:
            // pos is at the start, middle or end of the section
            if self.data[i - 1] == 1 {
                // We are at the start and end of the section of length 1

                // FIXME: Check bounds
                if i < self.data.len() {
                    self.data[i - 2] += self.data[i] + 1;
                    self.data.remove(i);
                } else {
                    self.data[i - 2] += 1;
                }

                self.data.remove(i - 1);
            } else if old_count == pos {
                // We are at the start of the section
                self.data[i - 1] -= 1;

                // If i is not in bounds, we are at the start of the belt
                if i > 1 {
                    self.data[i - 2] += 1;
                } else {
                    self.data.insert(0, 1);
                    self.first_spot_has_item = true;
                }
            } else if pos == count - 1 {
                // We are at the end of the section
                self.data[i - 1] -= 1;

                // If i is not in bounds, this is the end of the belt
                if i < self.data.len() {
                    self.data[i] += 1;
                } else {
                    self.data.push(1);
                }
            } else {
                // TODO: Check for off by one errors
                self.data[i - 1] = pos - old_count;
                self.data.insert(i, 1);
                self.data.insert(i + 1, count - pos - 1);
            }
        }

        !spot_full
    }

    // FIXME: This is buggy
    pub fn try_take_item_from_pos(&mut self, pos: u32) -> bool {
        let mut old_count = 0;
        let mut count = 0;
        let mut i = 0;

        while count <= pos {
            old_count = count;
            count += self.data[i];
            i += 1;
        }

        // i now stores the index into the data array of the chunk AFTER the one that contains the pos

        let spot_full: bool = (i % 2 == 0) ^ self.first_spot_has_item;

        if spot_full {
            // data[i] is a section containing items

            // Three possibilities:
            // pos is at the start, middle or end of the section
            if self.data[i - 1] == 1 {
                // We are at the start and end of the section of length 1

                // FIXME: Check bounds
                if i < self.data.len() {
                    self.data[i - 2] += self.data[i] + 1;
                    self.data.remove(i);
                } else {
                    self.data[i - 2] += 1;
                }

                self.data.remove(i - 1);
            } else if old_count == pos {
                // We are at the start of the section
                self.data[i - 1] -= 1;

                // If i is not in bounds, we are at the start of the belt
                if i > 1 {
                    self.data[i - 2] += 1;
                } else {
                    self.data.insert(0, 1);
                    self.first_spot_has_item = false;
                }
            } else if pos == count - 1 {
                // We are at the end of the section
                self.data[i - 1] -= 1;

                // If i is not in bounds, this is the end of the belt
                if i < self.data.len() {
                    self.data[i] += 1;
                } else {
                    self.data.push(1);
                }
            } else {
                // TODO: Check for off by one errors
                self.data[i - 1] = pos - old_count;
                self.data.insert(i, 1);
                self.data.insert(i + 1, count - pos - 1);
            }
        }

        spot_full
    }
}

#[cfg(test)]
mod tests {
    use std::sync::Arc;

    use crate::{
        belt::do_update_test,
        item::{option, Iron},
        producer::Producer,
    };

    use super::*;
    extern crate test;
    use proptest::{prelude::prop, prop_assert_eq, proptest};
    use test::Bencher;

    proptest! {
        #[test]
        fn test_constructing_optimized_belt_does_not_panic(len: u32) {
            let _ = OptimizedBelt::<Iron>::new(len);
        }

        #[test]
        fn test_optimized_belt_agrees_with_functional(mut items in prop::collection::vec(option(), 1..1_000)) {
            let mut belt = OptimizedBelt::<Iron>::new(items.len().try_into().expect("Size does not fit into usize"));

            for (i, item_opt) in items.iter().enumerate() {
                match item_opt {
                    Some(_) => {
                        belt.belt_storage.try_put_item_in_pos(i.try_into().expect("Size does not fit into usize"));
                    },
                    None => {
                        belt.belt_storage.try_take_item_from_pos(i.try_into().expect("Size does not fit into usize"));
                    },
                };
            }

            belt.update();

            do_update_test(&mut items);

            for (i, item) in items.iter().enumerate() {
                prop_assert_eq!(belt.get_item_at(i.try_into().expect("Size does not fit into usize")), *item);
            }
        }
    }

    #[bench]
    fn bench_optimized_belt_update(b: &mut Bencher) {
        let producer1 = Producer::<Iron>::new();
        // let producer2 = Producer::<Iron>::new();
        // let producer3 = Producer::<Iron>::new();

        let mut belt = OptimizedBelt {
            belt_storage: OptimizedBeltStorage {
                first_spot_has_item: true,
                data: vec![2, 10, 3, 4_294_967_295, 23],
                marker: PhantomData,
            },
            connected_out_inserters: vec![],
            connected_in_inserters: vec![],
        };

        OutInserterStrict::create_and_add(
            Arc::downgrade(&producer1.storage),
            &mut belt,
            1_000_000_000,
        );

        let mut i: i64 = 0;

        b.iter(|| {
            let bb = test::black_box(&mut belt);
            for _ in 0..1_000 {
                bb.update();
                // bb.belt_storage.try_take_item_from_pos(0);
                i += 1;
            }
        });

        println!("{belt:?}");

        println!("{i} updates done.");
    }
}
