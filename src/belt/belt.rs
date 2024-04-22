use crate::item::{Item, ItemTrait};
use std::{error::Error, fmt::Display};

use super::{
    in_inserter::InInserter,
    out_inserter::OutInserter,
    strict::{in_inserter::InInserterStrict, out_inserter::OutInserterStrict},
};

pub(super) trait Belt {
    fn query_item(&self, pos: usize) -> Option<Item>;
    fn remove_item(&mut self, pos: usize) -> Option<Item>;
    /// # Errors
    /// When there is no space at `pos`
    fn try_insert_item(&mut self, pos: usize, item: Item) -> Result<(), NoSpaceError>;

    fn add_in_inserter(&mut self, in_inserter: InInserter);
    fn add_out_inserter(&mut self, out_inserter: OutInserter);

    fn get_len(&self) -> usize;

    fn update(&mut self);
}

pub(super) trait StrictBelt<T: ItemTrait> {
    fn query_item(&self, pos: usize) -> Option<Item>;
    fn remove_item(&mut self, pos: usize) -> Option<Item>;
    /// # Errors
    /// When there is no space at `pos`
    fn try_insert_item(&mut self, pos: usize) -> Result<(), NoSpaceError>;

    fn add_in_inserter(&mut self, in_inserter: InInserterStrict<T>);
    fn add_out_inserter(&mut self, in_inserter: OutInserterStrict<T>);

    fn update(&mut self);
}

#[derive(Debug)]
pub struct NoSpaceError {}

impl Display for NoSpaceError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.write_str("No space in Belt!")
    }
}

impl Error for NoSpaceError {}
