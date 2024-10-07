use crate::item::{get_char, Item, ItemTrait};
use std::{error::Error, fmt::Display, slice::IterMut};

pub trait Belt<T: ItemTrait> {
    fn query_item(&self, pos: usize) -> Option<Item>;
    fn remove_item(&mut self, pos: usize) -> Option<Item>;
    /// # Errors
    /// When there is no space at `pos`
    fn try_insert_item(&mut self, pos: usize) -> Result<(), NoSpaceError>;

    // fn items_mut(&mut self) -> IterMut<bool>;

    fn get_len(&self) -> usize;

    fn update(&mut self);
}

impl<T: ItemTrait> Display for dyn Belt<T> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let mut s = String::new();

        for i in 0..self.get_len() {
            match self.query_item(i) {
                Some(item) => s.push(get_char(item)),
                None => s.push('.'),
            }
        }

        write!(f, "{s}")
    }
}

#[derive(Debug)]
pub struct NoSpaceError;

impl Display for NoSpaceError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.write_str("No space in Belt!")
    }
}

impl Error for NoSpaceError {}
