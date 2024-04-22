use std::sync::{atomic::Ordering, Weak};

use crate::item::{
    get_max_stack_size, Item, ItemStorage, ItemStorageStrict, ItemStorer, ItemTrait,
};

use super::{
    simple::{BeltStorage, SimpleBelt},
    smart::SmartBelt,
};

#[derive(Debug)]
pub struct InInserter {
    pub item: Item,
    connected_storage: ConnectedStorage,
    pub belt_pos: u32,
}

#[derive(Debug)]
enum ConnectedStorage {
    Strict(Box<Weak<dyn ItemStorer + Sync + Send + 'static>>),
    Lenient(Weak<ItemStorage>),
}

impl InInserter {
    pub(super) fn update_belt(&mut self, belt_storage: &mut BeltStorage) {
        // match &self.connected_storage {
        //     ConnectedStorage::Strict(store) => {
        //         let connected_storage = store.upgrade();

        //         if let Some(storage) = connected_storage {
        //             if belt_storage.get_item_from_pos(self.belt_pos) != Some(storage.get_item()) {
        //                 return;
        //             }

        //             let mut current_count = storage.get_count().load(Ordering::Relaxed);
        //             let stack_size = get_max_stack_size(storage.get_item());

        //             loop {
        //                 if current_count >= stack_size {
        //                     break;
        //                 }

        //                 match storage.get_count().compare_exchange_weak(
        //                     current_count,
        //                     current_count + 1,
        //                     Ordering::Relaxed,
        //                     Ordering::Relaxed,
        //                 ) {
        //                     Ok(_) => {
        //                         break;
        //                     },
        //                     Err(real_value) => current_count = real_value,
        //                 }
        //             }

        //             // We already checked that it is the correct item
        //             belt_storage.get_item_from_pos_and_remove(self.belt_pos);
        //         }
        //     },
        //     ConnectedStorage::Lenient(store) => {
        //         let connected_storage = store.upgrade();

        //         if let Some(storage) = connected_storage {
        //             if belt_storage.get_item_from_pos(self.belt_pos) != Some(storage.item) {
        //                 return;
        //             }

        //             let mut current_count = storage.count.load(Ordering::Relaxed);
        //             let stack_size = get_max_stack_size(storage.item);

        //             loop {
        //                 if current_count >= stack_size {
        //                     break;
        //                 }

        //                 match storage.count.compare_exchange_weak(
        //                     current_count,
        //                     current_count + 1,
        //                     Ordering::Relaxed,
        //                     Ordering::Relaxed,
        //                 ) {
        //                     Ok(_) => {
        //                         break;
        //                     },
        //                     Err(real_value) => current_count = real_value,
        //                 }
        //             }
        //         }
        //     },
        // }

        // // We already checked that it is the correct item
        // belt_storage.get_item_from_pos_and_remove(self.belt_pos);
    }

    // pub(super) fn update_optimized_belt(&mut self, belt_storage: &mut OptimizedBeltStorage) {
    //     let connected_count = self.connected_count.upgrade();

    //     if let Some(count) = connected_count {
    //         // TODO: get Item type from producer for stack size
    //         if count.load(Ordering::Relaxed) < 10
    //             && belt_storage.try_take_item_from_pos(self.belt_pos)
    //         {
    //             count.fetch_add(1, Ordering::Relaxed);
    //         }
    //     }
    // }

    pub fn create_and_add(storage: Weak<ItemStorage>, belt: &mut SimpleBelt, pos: u32) {
        let ins = Self {
            item: storage.upgrade().expect("Storage dropped").item,
            connected_storage: ConnectedStorage::Lenient(storage),
            belt_pos: pos,
        };

        // belt.add_in_inserter(ins);
    }

    pub fn create_and_add_strict<T: ItemTrait + Sync + Send + 'static>(
        storage: Weak<ItemStorageStrict<T>>,
        belt: &mut SimpleBelt,
        pos: u32,
    ) {
        let ins = Self {
            item: T::get_item(),
            connected_storage: ConnectedStorage::Strict(Box::new(storage)),
            belt_pos: pos,
        };

        // belt.add_in_inserter(ins);
    }

    pub fn create_and_add_strict_smart<T: ItemTrait + Sync + Send + 'static>(
        storage: Weak<ItemStorageStrict<T>>,
        belt: &mut SmartBelt,
        pos: u32,
    ) {
        let ins = Self {
            item: T::get_item(),
            connected_storage: ConnectedStorage::Strict(Box::new(storage)),
            belt_pos: pos,
        };

        // belt.add_in_inserter(ins);
    }
}
