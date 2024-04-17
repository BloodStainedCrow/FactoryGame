use crate::item::Item;
use std::{error::Error, fmt::Display};

pub trait Belt {
    fn query_item(pos: usize) -> Option<Item>;
    fn remove_item(pos: usize) -> Option<Item>;
    /// # Errors
    /// When there is no space at `pos`
    fn try_insert_item(pos: usize, item: Item) -> Result<(), NoSpaceError>;
}

#[derive(Debug)]
pub struct NoSpaceError {}

impl Display for NoSpaceError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.write_str("No space in Belt!")
    }
}

impl Error for NoSpaceError {}
