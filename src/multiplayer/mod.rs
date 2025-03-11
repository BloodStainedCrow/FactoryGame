use std::{
    net::{IpAddr, TcpStream},
    sync::{mpsc::Receiver, Arc, Mutex},
};

use plumbing::{Client, IntegratedServer, Server};
use server::{ActionSource, GameStateUpdateHandler, HandledActionConsumer};

use crate::{
    data::DataStore,
    frontend::action::ActionType,
    item::{IdxTrait, WeakIdxTrait},
    rendering::app_state::GameState,
};

mod plumbing;
mod protocol;
mod server;

mod connection_reciever;

pub enum Game<ItemIdxType: IdxTrait, RecipeIdxType: IdxTrait> {
    Client(
        Arc<Mutex<GameState<ItemIdxType, RecipeIdxType>>>,
        GameStateUpdateHandler<ItemIdxType, RecipeIdxType, Client<ItemIdxType, RecipeIdxType>>,
    ),
    DedicatedServer(
        GameState<ItemIdxType, RecipeIdxType>,
        GameStateUpdateHandler<ItemIdxType, RecipeIdxType, Server<ItemIdxType, RecipeIdxType>>,
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

pub struct ClientConnectionInfo {
    ip: IpAddr,
    port: u16,
}

pub struct ServerInfo {
    connections: Arc<Mutex<Vec<TcpStream>>>,
}

pub enum GameInitData<ItemIdxType: WeakIdxTrait, RecipeIdxType: WeakIdxTrait> {
    Client {
        game_state: Arc<Mutex<GameState<ItemIdxType, RecipeIdxType>>>,
        action_reciever: Receiver<ActionType<ItemIdxType, RecipeIdxType>>,
        info: ClientConnectionInfo,
    },
    DedicatedServer(GameState<ItemIdxType, RecipeIdxType>, ServerInfo),
    IntegratedServer {
        game_state: Arc<Mutex<GameState<ItemIdxType, RecipeIdxType>>>,
        action_reciever: Receiver<ActionType<ItemIdxType, RecipeIdxType>>,
        info: ServerInfo,
    },
}

impl<ItemIdxType: IdxTrait, RecipeIdxType: IdxTrait> Game<ItemIdxType, RecipeIdxType> {
    pub fn new(init: GameInitData<ItemIdxType, RecipeIdxType>) -> Result<Self, std::io::Error> {
        match init {
            GameInitData::Client {
                game_state,
                action_reciever,
                info,
            } => {
                let stream = std::net::TcpStream::connect((info.ip, info.port))?;
                Ok(Self::Client(
                    game_state,
                    GameStateUpdateHandler::new(Client {
                        local_actions: action_reciever,
                        server_connection: stream,
                    }),
                ))
            },
            GameInitData::DedicatedServer(game_state, info) => Ok(Self::DedicatedServer(
                game_state,
                GameStateUpdateHandler::new(Server::new(info)),
            )),
            GameInitData::IntegratedServer {
                game_state,
                action_reciever,
                info,
            } => Ok(Self::IntegratedServer(
                game_state,
                GameStateUpdateHandler::new(IntegratedServer {
                    local_actions: action_reciever,
                    server: Server::new(info),
                }),
            )),
        }
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

impl<ItemIdxType: WeakIdxTrait, RecipeIdxType: WeakIdxTrait>
    ActionSource<ItemIdxType, RecipeIdxType> for Receiver<ActionType<ItemIdxType, RecipeIdxType>>
{
    fn get(
        &self,
        current_tick: u64,
    ) -> impl IntoIterator<Item = ActionType<ItemIdxType, RecipeIdxType>, IntoIter: Clone>
           + Clone
           + use<ItemIdxType, RecipeIdxType> {
        self.try_recv()
    }
}

impl<ItemIdxType: WeakIdxTrait, RecipeIdxType: WeakIdxTrait>
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
