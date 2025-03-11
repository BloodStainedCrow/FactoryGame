#[allow(clippy::module_inception)]
pub mod belt;
pub mod smart;

use smart::{EmptyBelt, SmartBelt};

use crate::item::{IdxTrait, WeakIdxTrait};
#[cfg(test)]
use crate::item::Item;

#[cfg(test)]
fn do_update_test(items: &mut [Option<Item<u8>>]) {
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

#[derive(Debug, Clone, serde::Deserialize, serde::Serialize)]
pub struct BeltStore<RecipeIdxType: WeakIdxTrait> {
    pub empty_belts: Vec<EmptyBelt>,
    pub empty_belt_holes: Vec<usize>,

    pub belts: Box<[MultiBeltStore<RecipeIdxType>]>,
}

#[derive(Debug, Clone, serde::Deserialize, serde::Serialize)]
pub struct MultiBeltStore<RecipeIdxType: WeakIdxTrait> {
    pub belts: Vec<SmartBelt<RecipeIdxType>>,

    pub holes: Vec<usize>,
}

impl<RecipeIdxType: IdxTrait> Default for MultiBeltStore<RecipeIdxType> {
    fn default() -> Self {
        Self {
            belts: vec![],
            holes: vec![],
        }
    }
}

impl<RecipeIdxType: IdxTrait> MultiBeltStore<RecipeIdxType> {
    pub fn belts_mut(&mut self) -> impl IntoIterator<Item = &mut SmartBelt<RecipeIdxType>> {
        self.belts
            .iter_mut()
            .enumerate()
            .filter_map(|(i, b)| (!self.holes.contains(&i)).then_some(b))
    }
}
