pub mod deque;
pub mod in_inserter;
pub mod out_inserter;
pub mod simple;
pub mod smart;
pub mod splitter;
pub mod strict;

#[cfg(test)]
use crate::item::Item;

#[cfg(test)]
fn do_update_test(items: &mut [Option<Item>]) {
    match items {
        [] => {},
        [Some(_), rest @ ..] => do_update_test(rest),
        [None, _rest @ ..] => {
            items.rotate_left(1);
        },
    }
}
