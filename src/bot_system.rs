use std::collections::BTreeMap;

use crate::{
    frontend::world::Position,
    inserter::StorageID,
    item::{IdxTrait, Item, ITEMCOUNTTYPE},
};

struct BotSystem<ItemIdxType: IdxTrait, RecipeIdxType: IdxTrait> {
    roboports: Vec<Roboport>,
    bot_jobs: BTreeMap<u32, Vec<BotUpdate<ItemIdxType, RecipeIdxType>>>,
}

enum BotUpdate<ItemIdxType: IdxTrait, RecipeIdxType: IdxTrait> {
    DepositItem(Item<ItemIdxType>, ITEMCOUNTTYPE, StorageID<RecipeIdxType>),
    RetrieveItem(Item<ItemIdxType>, ITEMCOUNTTYPE, StorageID<RecipeIdxType>),
    RunOutOfPower,
    EnterRoboport(u32),
}

struct Roboport {
    pos: Position,
    construction_bots_idle: u8,
    logibots_idle: u8,
}

impl<ItemIdxType: IdxTrait, RecipeIdxType: IdxTrait> BotSystem<ItemIdxType, RecipeIdxType> {
    fn update(&mut self) {
        // self.bot_jobs.
        todo!()
    }
}
