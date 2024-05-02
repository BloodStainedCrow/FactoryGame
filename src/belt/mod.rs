#[allow(clippy::module_inception)]
pub mod belt;
pub mod smart;

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

#[cfg(test)]
fn do_update_test_bools(items: &mut [bool]) {
    match items {
        [] => {},
        [true, rest @ ..] => do_update_test_bools(rest),
        [false, _rest @ ..] => {
            items.rotate_left(1);
        },
    }
}
