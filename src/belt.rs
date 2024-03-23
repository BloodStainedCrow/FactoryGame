#[derive(Debug, Clone, Copy)]
struct ItemLocation {
    item: Option<Item>,
}

// TODO: Maybe add a first_full_index aswell?
#[derive(Debug, Clone)]
pub struct Belt {
    first_free_index: usize,
    locs: Vec<ItemLocation>,
    connected_inserters: Vec<Inserter>,
}

// This type of Belt is used when it only holds one kind of item
#[derive(Debug, Clone)]
pub struct OptimizedBelt {
    item: Option<Item>,
    first_spot_has_item: bool,
    data: Vec<u32>,
    connected_inserters: Vec<Inserter>,
}

impl Belt {
    #[must_use]
    pub fn new(len: u32) -> Self {
        Self {
            first_free_index: 0,
            locs: vec![ItemLocation { item: None }; len as usize],
            connected_inserters: vec![],
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
}

impl OptimizedBelt {
    #[must_use]
    pub fn new(len: u32) -> Self {
        Self {
            item: None,
            first_spot_has_item: false,
            data: vec![len],
            connected_inserters: vec![],
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
            // The Belt has layout like ....OOOOO
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
    }
}

#[derive(Debug, Clone, Copy)]
pub enum Item {
    Iron,
}

#[derive(Debug, Default, Clone)]
pub struct Inserter {}

impl Inserter {
    // fn get_replacing_item(&self) -> Option<Item> {
    //     None
    // }
}

#[cfg(test)]
mod tests {
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
        let inserters = vec![Inserter {}, Inserter {}, Inserter {}];

        let mut items: Vec<ItemLocation> = vec![ItemLocation { item: None }; 5_000];
        items.push(ItemLocation {
            item: Some(Item::Iron),
        });

        let mut belt = Belt {
            first_free_index: 0,
            locs: items,
            connected_inserters: inserters,
        };

        let mut i = 0;

        b.iter(|| {
            let mut bb = test::black_box(belt.clone());
            for _ in 0..1_000 {
                bb.update();
                i += 1;
            }
        });

        println!("Item at front: {:?}", belt.locs[0].item);

        println!("{i} updates done.");
    }

    #[bench]
    fn bench_optimized_belt_update(b: &mut Bencher) {
        let inserters = vec![Inserter {}, Inserter {}, Inserter {}];

        let mut belt = OptimizedBelt {
            item: Some(Item::Iron),
            first_spot_has_item: true,
            data: vec![2, 10, 3, 4_294_967_295, 23],
            connected_inserters: inserters,
        };

        let mut i: i64 = 0;

        b.iter(|| {
            // Fixme: this will now also benchmark the clone
            let mut bb = test::black_box(belt.clone());
            // let bb = test::black_box(&mut belt);
            for _ in 0..1_000 {
                bb.update();
                i += 1;
            }
        });

        println!("{belt:?}");

        println!("{i} updates done.");
    }
}
