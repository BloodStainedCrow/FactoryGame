use std::fmt::Display;

use crate::item::Item;

use super::inserter::Inserter;

#[derive(Debug, Clone, Copy)]
struct ItemLocation {
    item: Option<Item>,
}

// TODO: Maybe add a first_full_index aswell?
#[derive(Debug)]
#[allow(clippy::module_name_repetitions)]
pub struct SimpleBelt<'a> {
    belt_storage: BeltStorage,
    connected_inserters: Vec<Inserter<'a>>,
}

impl Display for SimpleBelt<'_> {
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

    pub fn try_put_item_in_pos(&mut self, item: Item, pos: u32) -> bool {
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
}

impl<'b> SimpleBelt<'b> {
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

#[cfg(test)]
mod tests {
    use crate::producer::Producer;

    use super::*;
    extern crate test;
    use proptest::proptest;
    use test::Bencher;

    proptest! {
        #[test]
        fn test_constructing_belt_does_not_panic(len in 0..10_000u32) {
            // TODO: This only checks a reasonable range since Belt is so inefficient with memory
            let _ = SimpleBelt::new(len);
        }
    }

    #[bench]
    fn bench_belt_update(b: &mut Bencher) {
        let producer1 = Producer::new(Item::Iron);
        let producer2 = Producer::new(Item::Iron);
        let producer3 = Producer::new(Item::Iron);

        let inserters = vec![
            Inserter {
                connected_producer_count: producer1.count,
                belt_pos: 3,
            },
            Inserter {
                connected_producer_count: producer2.count,
                belt_pos: 12,
            },
            Inserter {
                connected_producer_count: producer3.count,
                belt_pos: 127,
            },
        ];

        let mut items: Vec<ItemLocation> = vec![ItemLocation { item: None }; 5_00];
        items.push(ItemLocation {
            item: Some(Item::Iron),
        });

        let mut belt = SimpleBelt {
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
}
