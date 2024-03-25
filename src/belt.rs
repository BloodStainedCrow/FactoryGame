use std::fmt::Display;
use std::sync::atomic::{AtomicU64, Ordering};

use proptest::{
    prop_oneof,
    strategy::{Just, Strategy},
};

#[derive(Debug, Clone, Copy)]
struct ItemLocation {
    item: Option<Item>,
}

// TODO: Maybe add a first_full_index aswell?
#[derive(Debug)]
pub struct Belt<'a> {
    belt_storage: BeltStorage,
    connected_inserters: Vec<Inserter<'a>>,
}

impl Display for Belt<'_> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let mut s = String::new();

        for item_loc in &self.belt_storage.locs {
            if item_loc.item.is_some() {
                s.push('I');
            } else {
                s.push('.');
            }
        }

        write!(f, "{s}")
    }
}

#[derive(Debug)]
struct BeltStorage {
    first_free_index: usize,
    locs: Vec<ItemLocation>,
}

// This type of Belt is used when it only holds one kind of item
#[derive(Debug)]
struct OptimizedBelt<'a> {
    belt_storage: OptimizedBeltStorage,
    connected_inserters: Vec<Inserter<'a>>,
}

#[derive(Debug)]
struct OptimizedBeltStorage {
    item: Option<Item>,
    first_spot_has_item: bool,
    data: Vec<u32>,
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

    pub fn try_put_item_in_pos(&mut self, item: Item, pos: u32) -> bool {
        if self.locs[pos as usize].item.is_none() {
            self.locs[pos as usize].item = Some(item);

            // TODO: Write a test for this!
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
}

impl<'b> Belt<'b> {
    #[must_use]
    pub fn new(len: u32) -> Self {
        Self {
            belt_storage: BeltStorage::new(len),
            connected_inserters: vec![],
        }
    }

    pub fn update(&mut self) {
        self.belt_storage.update();

        for inserter in &mut self.connected_inserters {
            inserter.update_belt(&mut self.belt_storage);
        }
    }

    pub fn add_inserter<'a>(&mut self, inserter: Inserter<'a>)
    where
        'a: 'b,
    {
        self.connected_inserters.push(inserter);
    }
}

impl OptimizedBelt<'_> {
    #[must_use]
    pub fn new(len: u32) -> Self {
        Self {
            belt_storage: OptimizedBeltStorage::new(len),
            connected_inserters: vec![],
        }
    }

    pub fn update(&mut self) {
        self.belt_storage.update();
    }
}

impl OptimizedBeltStorage {
    #[must_use]
    pub fn new(len: u32) -> Self {
        Self {
            item: None,
            first_spot_has_item: false,
            data: vec![len],
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
        }

        debug_assert!(self.data.iter().all(|group_len| *group_len > 0));
    }

    pub fn try_put_item_in_pos(&mut self, pos: u32) -> bool {
        todo!();

        let mut count = 0;
        let mut i = 0;

        while count <= pos {
            count += self.data[i];
            i += 1;
        }

        // i now stores the index into the data array of the chunk AFTER the one that contains the pos

        let spot_full: bool = (i % 2 == 1) ^ self.first_spot_has_item;

        if !spot_full {
            // self.data[i - 1]
        }

        spot_full
    }
}

#[derive(Debug, Clone, Copy)]
pub enum Item {
    Iron,
}

pub fn my_enum_strategy() -> impl Strategy<Value = Item> {
    prop_oneof![
        // For cases without data, `Just` is all you need
        Just(Item::Iron),
    ]
}

#[derive(Debug)]
pub struct Inserter<'a> {
    pub connected_producer_count: &'a AtomicU64,
    pub belt_pos: u32,
}

impl Inserter<'_> {
    fn update_belt(&mut self, belt_storage: &mut BeltStorage) {
        // TODO: get Item type from producer
        if self.connected_producer_count.load(Ordering::Acquire) > 0
            && belt_storage.try_put_item_in_pos(Item::Iron, self.belt_pos)
        {
            self.connected_producer_count
                .fetch_sub(1, Ordering::Release);
        }
    }
}

#[cfg(test)]
mod tests {
    use crate::producer::Producer;

    use super::*;
    extern crate test;
    use proptest::proptest;
    use test::Bencher;

    proptest! {
        #[test]
        fn test_constructing_optimized_belt_does_not_panic(len: u32) {
            let _ = OptimizedBelt::new(len);
        }

        #[test]
        fn test_constructing_belt_does_not_panic(len in 0..10_000u32) {
            // TODO: This only checks a reasonable range since Belt is so inefficient with memory
            let _ = Belt::new(len);
        }
    }

    #[bench]
    fn bench_belt_update(b: &mut Bencher) {
        let producer1 = Producer::new(Item::Iron);
        let producer2 = Producer::new(Item::Iron);
        let producer3 = Producer::new(Item::Iron);

        let inserters = vec![
            Inserter {
                connected_producer_count: &producer1.count,
                belt_pos: 3,
            },
            Inserter {
                connected_producer_count: &producer2.count,
                belt_pos: 12,
            },
            Inserter {
                connected_producer_count: &producer3.count,
                belt_pos: 127,
            },
        ];

        let mut items: Vec<ItemLocation> = vec![ItemLocation { item: None }; 5_00];
        items.push(ItemLocation {
            item: Some(Item::Iron),
        });

        let mut belt = Belt {
            belt_storage: BeltStorage {
                first_free_index: 0,
                locs: items,
            },
            connected_inserters: inserters,
        };

        let mut i = 0;

        b.iter(|| {
            let bb = test::black_box(&mut belt);
            for _ in 0..1_000 {
                bb.update();
                i += 1;
            }
        });

        println!("Item at front: {:?}", belt.belt_storage.locs);

        println!("{i} updates done.");
    }

    #[bench]
    fn bench_optimized_belt_update(b: &mut Bencher) {
        let producer1 = Producer::new(Item::Iron);
        let producer2 = Producer::new(Item::Iron);
        let producer3 = Producer::new(Item::Iron);

        let inserters = vec![
            Inserter {
                connected_producer_count: &producer1.count,
                belt_pos: 3,
            },
            Inserter {
                connected_producer_count: &producer2.count,
                belt_pos: 12,
            },
            Inserter {
                connected_producer_count: &producer3.count,
                belt_pos: 127,
            },
        ];

        let mut belt = OptimizedBelt {
            belt_storage: OptimizedBeltStorage {
                item: Some(Item::Iron),
                first_spot_has_item: true,
                data: vec![2, 10, 3, 4_294_967_295, 23],
            },
            connected_inserters: inserters,
        };

        let mut i: i64 = 0;

        b.iter(|| {
            let bb = test::black_box(&mut belt);
            for _ in 0..1_000 {
                bb.update();
                i += 1;
            }
        });

        println!("{belt:?}");

        println!("{i} updates done.");
    }
}
