use std::{marker::PhantomData, sync::Arc};

use crate::item::{Item, ItemStorageStrict, ItemTrait};

#[derive(Debug, Default)]
pub struct SpecializedStorage<T: ItemTrait> {
    marker: PhantomData<T>,
    pub storage: Arc<ItemStorageStrict<T>>,
}

impl<T: ItemTrait> SpecializedStorage<T> {
    #[must_use]
    pub fn new() -> Self {
        Self {
            storage: Arc::new(ItemStorageStrict::default()),
            marker: PhantomData,
        }
    }

    #[must_use]
    pub const fn get_item() -> Item
    where
        T: ~const ItemTrait,
    {
        T::get_item()
    }
}
