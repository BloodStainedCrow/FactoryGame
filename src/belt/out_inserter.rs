use std::sync::{atomic::Ordering, Weak};

use crate::item::{Item, ItemStorage};

use super::belt::Belt;

#[derive(Debug)]
pub struct OutInserter {
    pub item: Item,
    connected_storage: Weak<ItemStorage>,
    pub belt_pos: usize,
}

// #[derive(Debug)]
// enum ConnectedStorage {
//     Strict(Box<Weak<dyn ItemStorer + Sync + Send>>),
//     Lenient(),
// }

impl OutInserter {
    pub fn create_and_add<B: Belt>(storage: Weak<ItemStorage>, belt: &mut B, pos: usize) {
        let ins = Self {
            item: storage.upgrade().expect("Storage dropped").item,
            connected_storage: storage,
            belt_pos: pos,
        };

        belt.add_out_inserter(ins);
    }

    // pub fn create_and_add_strict<T: ItemTrait + Sync + Send + 'static, B: StrictBelt<T>>(
    //     storage: Weak<ItemStorageStrict<T>>,
    //     belt: &mut B,
    //     pos: usize,
    // ) {
    //     let ins = Self {
    //         item: T::get_item(),
    //         connected_storage: ConnectedStorage::Strict(Box::new(storage)),
    //         belt_pos: pos,
    //     };

    //     belt.add_out_inserter(ins);
    // }

    pub(super) fn update_belt<B: Belt>(&self, belt_storage: &mut B) {
        // match &self.connected_storage {
        //     ConnectedStorage::Strict(store) => {
        //         let connected_storage_option = store.upgrade();

        //         if let Some(storage) = connected_storage_option {
        //             if storage.get_count().load(Ordering::Relaxed) > 0
        //                 && belt_storage
        //                     .try_insert_item(self.belt_pos, self.item)
        //                     .is_ok()
        //             {
        //                 storage.get_count().fetch_sub(1, Ordering::Relaxed);
        //             }
        //         }
        //     },
        //     ConnectedStorage::Lenient(store) => {
        //         let connected_storage_option = store.upgrade();

        //         if let Some(storage) = connected_storage_option {
        //             if storage.count.load(Ordering::Relaxed) > 0
        //                 && belt_storage.try_put_item_in_pos(self.item, self.belt_pos)
        //             {
        //                 storage.count.fetch_sub(1, Ordering::Relaxed);
        //             }
        //         }
        //     },
        // }

        let connected_storage_option = self.connected_storage.upgrade();

        // TODO: Race condition!
        if let Some(storage) = connected_storage_option {
            if storage.count.load(Ordering::Relaxed) > 0
                && belt_storage
                    .try_insert_item(self.belt_pos, self.item)
                    .is_ok()
            {
                storage.count.fetch_sub(1, Ordering::Relaxed);
            }
        }
    }
}

#[cfg(test)]
mod tests {
    use std::sync::{atomic::AtomicU64, Arc};

    use crate::belt::smart::SmartBelt;

    use super::*;
    extern crate test;
    use test::Bencher;

    // #[bench]
    // fn bench_out_inserter_strict(b: &mut Bencher) {
    //     let storage = Arc::new(SpecializedStorage::<Iron>::new());
    //     let mut belt = SimpleBelt::new(1);

    //     OutInserter::create_and_add_strict(Arc::downgrade(&storage.storage), &mut belt, 0);

    //     b.iter(|| {
    //         let bb = test::black_box(&mut belt);

    //         for _ in 0..1_000 {
    //             bb.update();
    //             bb.belt_storage.get_item_from_pos_and_remove(0);
    //         }
    //     });

    //     println!("{belt}");
    // }

    #[bench]
    fn bench_out_inserter(b: &mut Bencher) {
        let storage = Arc::new(ItemStorage {
            count: AtomicU64::new(1_000_000),
            item: Item::Iron,
        });
        let mut belt = SmartBelt::new(1);

        OutInserter::create_and_add(Arc::downgrade(&storage), &mut belt, 0);

        b.iter(|| {
            let bb = test::black_box(&mut belt);

            for _ in 0..1_000 {
                bb.update();
                bb.remove_item(0);
            }
        });

        println!("{belt}");
    }
}
