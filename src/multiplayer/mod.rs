use std::sync::{mpsc::Receiver, Arc, Mutex};

use plumbing::{Client, IntegratedServer, Server};
use server::{ActionSource, GameStateUpdateHandler, HandledActionConsumer};

use crate::{
    data::DataStore, frontend::action::ActionType, item::IdxTrait, rendering::app_state::GameState,
};

mod plumbing;
mod protocol;
mod server;

enum Game<ItemIdxType: IdxTrait, RecipeIdxType: IdxTrait> {
    Client(
        Arc<Mutex<GameState<ItemIdxType, RecipeIdxType>>>,
        GameStateUpdateHandler<ItemIdxType, RecipeIdxType, Client>,
    ),
    DedicatedServer(
        GameState<ItemIdxType, RecipeIdxType>,
        GameStateUpdateHandler<ItemIdxType, RecipeIdxType, Server>,
    ),
    /// Integrated Server is also how Singleplayer works
    IntegratedServer(
        Arc<Mutex<GameState<ItemIdxType, RecipeIdxType>>>,
        GameStateUpdateHandler<
            ItemIdxType,
            RecipeIdxType,
            IntegratedServer<ItemIdxType, RecipeIdxType>,
        >,
    ),
}

enum GameInitData<ItemIdxType: IdxTrait, RecipeIdxType: IdxTrait> {
    Client {
        game_state: Arc<Mutex<GameState<ItemIdxType, RecipeIdxType>>>,
        action_reciever: Receiver<ActionType<ItemIdxType, RecipeIdxType>>,
    },
    DedicatedServer(GameState<ItemIdxType, RecipeIdxType>),
    IntegratedServer {
        game_state: Arc<Mutex<GameState<ItemIdxType, RecipeIdxType>>>,
        action_reciever: Receiver<ActionType<ItemIdxType, RecipeIdxType>>,
    },
}

impl<ItemIdxType: IdxTrait, RecipeIdxType: IdxTrait> Game<ItemIdxType, RecipeIdxType> {
    pub fn new(init: GameInitData<ItemIdxType, RecipeIdxType>) -> Self {
        todo!()
    }

    pub fn run(&mut self) {
        todo!()
    }

    fn do_tick(&mut self, data_store: &DataStore<ItemIdxType, RecipeIdxType>) {
        match self {
            Game::Client(game_state, game_state_update_handler) => game_state_update_handler
                .update(
                    &mut game_state.lock().expect("Lock poison for update"),
                    data_store,
                ),
            Game::DedicatedServer(game_state, game_state_update_handler) => {
                game_state_update_handler.update(game_state, data_store)
            },
            Game::IntegratedServer(game_state, game_state_update_handler) => {
                game_state_update_handler.update(
                    &mut game_state.lock().expect("Lock poison for update"),
                    data_store,
                )
            },
        }
    }
}

impl<ItemIdxType: IdxTrait, RecipeIdxType: IdxTrait> ActionSource<ItemIdxType, RecipeIdxType>
    for Receiver<ActionType<ItemIdxType, RecipeIdxType>>
{
    fn get(
        &mut self,
        current_tick: u64,
    ) -> impl IntoIterator<Item = ActionType<ItemIdxType, RecipeIdxType>, IntoIter: Clone>
           + Clone
           + use<ItemIdxType, RecipeIdxType> {
        self.try_recv()
    }
}

impl<ItemIdxType: IdxTrait, RecipeIdxType: IdxTrait>
    HandledActionConsumer<ItemIdxType, RecipeIdxType> for ()
{
    fn consume(
        &mut self,
        current_tick: u64,
        actions: impl IntoIterator<Item = ActionType<ItemIdxType, RecipeIdxType>>,
    ) {
        // Do nothing
    }
}
