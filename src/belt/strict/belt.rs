use std::{fmt::Display, marker::PhantomData};

use crate::{
    belt::{
        in_inserter::InInserter,
        splitter::{SplitterInserterIn, SplitterInserterOut},
    },
    item::{get_char, Item, ItemTrait},
};

use super::out_inserter::OutInserterStrict;

#[allow(clippy::module_name_repetitions)]
pub struct StrictBelt<T: ItemTrait> {
    belt_storage: StrictBeltStorage<T>,
    connected_out_inserters: Vec<OutInserterStrict<T>>,
    connected_in_inserters: Vec<InInserter>,
    splitter_inserter_in: Option<SplitterInserterIn>,
    splitter_inserter_out: Option<SplitterInserterOut>,
}

#[derive(Debug)]
pub(super) struct StrictBeltStorage<T: ItemTrait> {
    marker: PhantomData<T>,
    first_free_index: usize,
    data: Vec<bool>,
}

impl<T: ItemTrait> Display for StrictBelt<T> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let mut s = String::new();

        for item in &self.belt_storage.data {
            if *item {
                s.push(get_char(T::get_item()));
            } else {
                s.push('.');
            }
        }

        write!(f, "{s}")
    }
}

impl<T: ItemTrait> StrictBeltStorage<T> {
    #[must_use]
    pub fn new(len: u32) -> Self {
        Self {
            data: vec![false; len as usize],
            first_free_index: 0,
            marker: PhantomData,
        }
    }

    pub fn update(&mut self) {
        let slice = self.data.as_mut_slice();

        let (_stuck_slice, moving_slice) = slice.split_at_mut(self.first_free_index);

        if !moving_slice.is_empty() {
            // This will rotate the first item to the end.
            moving_slice.rotate_left(1);
            // The first item of moving_slice is the first empty slot, therefore we automatically have the empty slot at the end
            // moving_slice[moving_slice.len() - 1].item = None;

            // We need to update first_free_index
            self.first_free_index += moving_slice
                .iter()
                .position(|loc| *loc == false)
                .unwrap_or(moving_slice.len());
        }
    }

    pub fn try_put_item_in_pos(&mut self, pos: u32) -> bool {
        if !self.data[pos as usize] {
            self.data[pos as usize] = true;

            // TODO: Write a test for this!
            // Update first_free_index
            if self.first_free_index == pos as usize {
                let (_left, right) = self.data.split_at(pos as usize);

                let index_in_right = right.iter().position(|elem| !elem);

                let index_total = index_in_right.unwrap_or(right.len()) + pos as usize;

                self.first_free_index = index_total;
            }

            true
        } else {
            false
        }
    }

    pub fn try_take_item_from_pos(&mut self, pos: u32) -> bool {
        if self.data[pos as usize] {
            self.data[pos as usize] = false;

            if self.first_free_index > pos as usize {
                self.first_free_index = pos as usize;
            }

            true
        } else {
            false
        }
    }

    pub fn get_item_from_pos(&mut self, pos: u32) -> Option<Item> {
        if self.data[pos as usize] {
            Some(T::get_item())
        } else {
            None
        }
    }

    pub fn get_item_from_pos_and_remove(&mut self, pos: u32) -> Option<Item> {
        if self.data[pos as usize] {
            self.data[pos as usize] = false;

            if self.first_free_index > pos as usize {
                self.first_free_index = pos as usize;
            }

            Some(T::get_item())
        } else {
            None
        }
    }

    pub fn check_for_space(&self, pos: u32) -> bool {
        !self.data[pos as usize]
    }

    #[allow(clippy::cast_possible_truncation)]
    pub fn get_belt_len(&self) -> u32 {
        self.data.len() as u32
    }
}

impl<T: ItemTrait> StrictBelt<T> {
    #[must_use]
    pub fn new(len: u32) -> Self {
        Self {
            belt_storage: StrictBeltStorage::<T>::new(len),
            connected_out_inserters: vec![],
            connected_in_inserters: vec![],
            splitter_inserter_in: None,
            splitter_inserter_out: None,
        }
    }

    pub fn update(&mut self) {
        // if let Some(inserter) = &mut self.splitter_inserter_in {
        //     inserter.update_simple(&mut self.belt_storage);
        // }

        // self.belt_storage.update();

        // if let Some(inserter) = &mut self.splitter_inserter_out {
        //     inserter.update_simple(&mut self.belt_storage);
        // }

        // for inserter in &mut self.connected_out_inserters {
        //     inserter.update_belt(&mut self.belt_storage);
        // }

        // for inserter in &mut self.connected_in_inserters {
        //     inserter.update_belt(&mut self.belt_storage);
        // }
    }

    pub fn add_out_inserter(&mut self, inserter: OutInserterStrict<T>) {
        self.connected_out_inserters.push(inserter);
    }

    pub fn add_in_inserter(&mut self, inserter: InInserter) {
        self.connected_in_inserters.push(inserter);
    }

    pub(super) fn add_splitter_inserter_in(&mut self, inserter: SplitterInserterIn) {
        self.splitter_inserter_in = Some(inserter);
    }

    pub(super) fn add_splitter_inserter_out(&mut self, inserter: SplitterInserterOut) {
        self.splitter_inserter_out = Some(inserter);
    }

    #[must_use]
    pub fn get_item_at(&self, pos: u32) -> Option<Item> {
        if self.belt_storage.data[pos as usize] {
            Some(T::get_item())
        } else {
            None
        }
    }
}
