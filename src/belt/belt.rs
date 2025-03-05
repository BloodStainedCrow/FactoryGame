use std::{
    error::Error,
    fmt::{Display, Write},
};

pub trait Belt {
    // fn query_item(&self, pos: usize) -> Option<Item<ItemIdxType>>;
    // fn remove_item(&mut self, pos: usize) -> Option<Item<ItemIdxType>>;
    /// # Errors
    /// When there is no space at `pos`
    fn try_insert_item(&mut self, pos: u16) -> Result<(), NoSpaceError>;

    // fn items_mut(&mut self) -> IterMut<bool>;

    fn get_len(&self) -> u16;

    fn update(&mut self);
}

impl Display for dyn Belt {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let mut s = String::new();

        for i in 0..self.get_len() {
            // match self.query_item(i) {
            //     Some(item) => f.write_str(&item.print()),
            //     None => f.write_char("."),
            // }
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
