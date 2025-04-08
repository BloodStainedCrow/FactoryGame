#[allow(clippy::module_inception)]
pub mod belt;
pub mod smart;

pub mod splitter;

use std::mem;

use crate::item::{usize_from, Item};
use belt::Belt;
use smart::{EmptyBelt, Side, SmartBelt};

use crate::{
    frontend::world::tile::{BeltId, BeltTileId},
    item::{IdxTrait, WeakIdxTrait},
};

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

pub struct BreakBeltResultInfo<ItemIdxType: WeakIdxTrait> {
    pub kept_id: BeltTileId<ItemIdxType>,
    pub new_id: BeltTileId<ItemIdxType>,
    pub kept_side: Side,
}

impl<RecipeIdxType: IdxTrait> BeltStore<RecipeIdxType> {
    pub fn get_mut_and_instantiate<ItemIdxType: IdxTrait>(
        &mut self,
        item: Item<ItemIdxType>,
        id: BeltTileId<ItemIdxType>,
    ) -> (&mut SmartBelt<RecipeIdxType>, BeltId<ItemIdxType>) {
        match id {
            BeltTileId::EmptyBeltId(index) => {
                self.empty_belt_holes.push(index);

                let empty_belt = mem::take(&mut self.empty_belts[index]);

                let smart_belt = empty_belt.into_smart_belt();

                let id = self.add_belt(item, smart_belt);

                (self.get_belt_mut(id), id)
            },
            BeltTileId::BeltId(belt_id) => {
                assert_eq!(item, belt_id.item);
                (
                    &mut self.belts[usize_from(item.id)].belts[belt_id.index],
                    belt_id,
                )
            },
        }
    }

    pub fn add_length<ItemIdxType: IdxTrait>(
        &mut self,
        belt: BeltTileId<ItemIdxType>,
        amount: u16,
        side: Side,
    ) -> u16 {
        match belt {
            BeltTileId::EmptyBeltId(idx) => self.empty_belts[idx].add_length(amount, side),
            BeltTileId::BeltId(BeltId { item, index }) => {
                self.belts[Into::<usize>::into(item.id)].belts[index].add_length(amount, side)
            },
        }
    }

    pub fn get_belt<ItemIdxType: IdxTrait>(
        &self,
        id: BeltId<ItemIdxType>,
    ) -> &SmartBelt<RecipeIdxType> {
        &self.belts[Into::<usize>::into(id.item.id)].belts[id.index]
    }

    pub fn get_belt_mut<ItemIdxType: IdxTrait>(
        &mut self,
        id: BeltId<ItemIdxType>,
    ) -> &mut SmartBelt<RecipeIdxType> {
        &mut self.belts[Into::<usize>::into(id.item.id)].belts[id.index]
    }

    pub fn add_belt<ItemIdxType: IdxTrait>(
        &mut self,
        item: Item<ItemIdxType>,
        belt: SmartBelt<RecipeIdxType>,
    ) -> BeltId<ItemIdxType> {
        if let Some(id) = self.belts[Into::<usize>::into(item.id)].holes.pop() {
            self.belts[Into::<usize>::into(item.id)].belts[id] = belt;
            BeltId { item, index: id }
        } else {
            self.belts[Into::<usize>::into(item.id)].belts.push(belt);
            BeltId {
                item,
                index: self.belts[Into::<usize>::into(item.id)].belts.len() - 1,
            }
        }
    }

    pub fn add_empty_belt<ItemIdxType: IdxTrait>(&mut self, len: u16) -> BeltTileId<ItemIdxType> {
        if let Some(id) = self.empty_belt_holes.pop() {
            self.empty_belts[id] = EmptyBelt::new(len);
            BeltTileId::EmptyBeltId(id)
        } else {
            self.empty_belts.push(EmptyBelt::new(len));
            BeltTileId::EmptyBeltId(self.empty_belts.len() - 1)
        }
    }

    pub fn break_belt_at<ItemIdxType: IdxTrait>(
        &mut self,
        id: BeltTileId<ItemIdxType>,
        belt_pos_to_break_at: u16,
    ) -> BreakBeltResultInfo<ItemIdxType> {
        let ret = match id {
            BeltTileId::EmptyBeltId(idx) => {
                let new_empty_belt = self.empty_belts[idx].break_belt_at(belt_pos_to_break_at);
                let new_id = self.add_empty_belt(new_empty_belt.len);
                BreakBeltResultInfo {
                    kept_id: id,
                    new_id,
                    kept_side: Side::FRONT,
                }
            },
            BeltTileId::BeltId(BeltId { item, index }) => {
                dbg!(belt_pos_to_break_at);
                let new_belt: SmartBelt<RecipeIdxType> = self.belts[Into::<usize>::into(item.id)]
                    .belts[index]
                    .break_belt_at(belt_pos_to_break_at);
                let new_id = BeltTileId::BeltId(self.add_belt(item, new_belt));
                BreakBeltResultInfo {
                    kept_id: id,
                    new_id,
                    kept_side: Side::FRONT,
                }
            },
        };

        dbg!(self);
        ret
    }

    pub fn get_len<ItemIdxType: IdxTrait>(&self, id: BeltTileId<ItemIdxType>) -> u16 {
        match id {
            BeltTileId::EmptyBeltId(idx) => self.empty_belts[idx].len,
            BeltTileId::BeltId(BeltId { item, index }) => {
                self.belts[Into::<usize>::into(item.id)].belts[index].get_len()
            },
        }
    }
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
