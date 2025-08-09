use std::sync::{
    Arc,
    atomic::{AtomicBool, AtomicU64},
    mpsc::Sender,
};

use parking_lot::Mutex;

use crate::{
    app_state::GameState,
    data::DataStore,
    frontend::action::{ActionType, action_state_machine::ActionStateMachine},
    item::WeakIdxTrait,
};

pub struct LoadedGameInfo {
    pub state: LoadedGame,
    pub tick: Arc<AtomicU64>,
}

pub enum LoadedGame {
    ItemU8RecipeU8(LoadedGameSized<u8, u8>),
    ItemU8RecipeU16(LoadedGameSized<u8, u16>),
    ItemU16RecipeU8(LoadedGameSized<u16, u8>),
    ItemU16RecipeU16(LoadedGameSized<u16, u16>),
}

pub struct LoadedGameSized<ItemIdxType: WeakIdxTrait, RecipeIdxType: WeakIdxTrait> {
    pub state: Arc<Mutex<GameState<ItemIdxType, RecipeIdxType>>>,
    pub state_machine: Arc<Mutex<ActionStateMachine<ItemIdxType, RecipeIdxType>>>,
    pub data_store: Arc<Mutex<DataStore<ItemIdxType, RecipeIdxType>>>,
    pub ui_action_sender: Sender<ActionType<ItemIdxType, RecipeIdxType>>,

    pub stop_update_thread: Arc<AtomicBool>,
}

impl<ItemIdxType: WeakIdxTrait, RecipeIdxType: WeakIdxTrait> Drop
    for LoadedGameSized<ItemIdxType, RecipeIdxType>
{
    fn drop(&mut self) {
        self.stop_update_thread
            .store(true, std::sync::atomic::Ordering::Relaxed);
    }
}
