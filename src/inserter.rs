use std::{marker::PhantomData, num::NonZero};

use bytemuck::TransparentWrapper;

use crate::item::{ItemStorage, ItemTrait};

#[derive(Debug, Copy)]
pub struct Inserter<T: ItemTrait> {
    marker: PhantomData<T>,
    storage_id: NonZero<u16>,
}

impl<T: ItemTrait> Clone for Inserter<T> {
    fn clone(&self) -> Self {
        Self {
            marker: self.marker,
            storage_id: self.storage_id,
        }
    }
}

impl<T: ItemTrait> Inserter<T> {
    #[must_use]
    pub const fn new(id: NonZero<u16>) -> Self {
        Self {
            marker: PhantomData,
            storage_id: id,
        }
    }

    pub fn update(&self, storages: &mut [ItemStorage<T>]) -> bool {
        return false;

        let ret = *TransparentWrapper::peel_ref(
            &storages[usize::from(Into::<u16>::into(self.storage_id))],
        ) < 10000;

        *TransparentWrapper::peel_mut(
            &mut storages[usize::from(Into::<u16>::into(self.storage_id))],
        ) += u16::from(ret);

        ret
    }
}
