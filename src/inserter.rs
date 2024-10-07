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

    /// This function assumes that it can remove an item from somewhere (a belt)
    /// The caller is responsible for doing the removal and ensuring before calling this,
    /// that there is an item to remove
    pub fn update(&self, storages: &mut [ItemStorage<T>]) -> bool {
        let ret = *TransparentWrapper::peel_ref(
            &storages[usize::from(Into::<u16>::into(self.storage_id))],
        ) < T::MAX_STACK_SIZE;

        *TransparentWrapper::peel_mut(
            &mut storages[usize::from(Into::<u16>::into(self.storage_id))],
        ) += u16::from(ret);

        ret
    }

    /// This function assumes that it can remove an item from somewhere (a belt)
    /// The caller is responsible for doing the removal and ensuring before calling this,
    /// that there is an item to remove
    pub fn update_branched(&self, storages: &mut [ItemStorage<T>]) -> bool {
        if *TransparentWrapper::peel_ref(&storages[usize::from(Into::<u16>::into(self.storage_id))])
            < T::MAX_STACK_SIZE
        {
            *TransparentWrapper::peel_mut(
                &mut storages[usize::from(Into::<u16>::into(self.storage_id))],
            ) += 1;
            true
        } else {
            false
        }
    }
}
