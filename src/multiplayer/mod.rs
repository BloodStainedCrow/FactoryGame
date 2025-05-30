use std::{
    net::{IpAddr, TcpStream},
    ops::ControlFlow,
    sync::{atomic::AtomicU64, mpsc::Receiver, Arc},
    time::{Duration, Instant},
};

use parking_lot::Mutex;

use log::trace;
use plumbing::{Client, IntegratedServer, Server};
use server::{ActionSource, GameStateUpdateHandler, HandledActionConsumer};

use crate::{
    data::DataStore,
    frontend::{
        action::{action_state_machine::ActionStateMachine, ActionType},
        input::Input,
        world::tile::World,
    },
    item::{IdxTrait, WeakIdxTrait},
    rendering::app_state::GameState,
    replays::Replay,
    TICKS_PER_SECOND_RUNSPEED,
};

mod plumbing;
mod protocol;
mod server;

pub mod connection_reciever;

pub enum Game<ItemIdxType: IdxTrait, RecipeIdxType: IdxTrait> {
    Client(
        Arc<Mutex<GameState<ItemIdxType, RecipeIdxType>>>,
        GameStateUpdateHandler<ItemIdxType, RecipeIdxType, Client<ItemIdxType, RecipeIdxType>>,
        Arc<AtomicU64>,
    ),
    DedicatedServer(
        GameState<ItemIdxType, RecipeIdxType>,
        Replay<ItemIdxType, RecipeIdxType, DataStore<ItemIdxType, RecipeIdxType>>,
        GameStateUpdateHandler<ItemIdxType, RecipeIdxType, Server<ItemIdxType, RecipeIdxType>>,
    ),
    /// Integrated Server is also how Singleplayer works
    IntegratedServer(
        Arc<Mutex<GameState<ItemIdxType, RecipeIdxType>>>,
        Replay<ItemIdxType, RecipeIdxType, DataStore<ItemIdxType, RecipeIdxType>>,
        GameStateUpdateHandler<
            ItemIdxType,
            RecipeIdxType,
            IntegratedServer<ItemIdxType, RecipeIdxType>,
        >,
        Arc<AtomicU64>,
    ),
}

pub struct ClientConnectionInfo {
    pub ip: IpAddr,
    pub port: u16,
}

pub struct ServerInfo {
    pub connections: Arc<Mutex<Vec<TcpStream>>>,
}

pub enum GameInitData<ItemIdxType: WeakIdxTrait, RecipeIdxType: WeakIdxTrait> {
    Client {
        game_state: Arc<Mutex<GameState<ItemIdxType, RecipeIdxType>>>,
        action_state_machine: Arc<Mutex<ActionStateMachine<ItemIdxType, RecipeIdxType>>>,
        inputs: Receiver<Input>,
        ui_actions: Receiver<ActionType<ItemIdxType, RecipeIdxType>>,
        tick_counter: Arc<AtomicU64>,
        info: ClientConnectionInfo,
    },
    DedicatedServer(GameState<ItemIdxType, RecipeIdxType>, ServerInfo),
    IntegratedServer {
        game_state: Arc<Mutex<GameState<ItemIdxType, RecipeIdxType>>>,
        action_state_machine: Arc<Mutex<ActionStateMachine<ItemIdxType, RecipeIdxType>>>,
        inputs: Receiver<Input>,
        ui_actions: Receiver<ActionType<ItemIdxType, RecipeIdxType>>,
        tick_counter: Arc<AtomicU64>,
        info: ServerInfo,
    },
}

pub enum ExitReason {
    UserQuit,
    ConnectionDropped,
}

impl<ItemIdxType: IdxTrait, RecipeIdxType: IdxTrait> Game<ItemIdxType, RecipeIdxType> {
    pub fn new(
        init: GameInitData<ItemIdxType, RecipeIdxType>,
        data_store: &DataStore<ItemIdxType, RecipeIdxType>,
    ) -> Result<Self, std::io::Error> {
        match init {
            GameInitData::Client {
                game_state,
                action_state_machine,
                inputs,
                tick_counter,
                info,
                ui_actions,
            } => {
                let stream = std::net::TcpStream::connect((info.ip, info.port))?;
                Ok(Self::Client(
                    game_state,
                    GameStateUpdateHandler::new(Client {
                        local_actions: action_state_machine,
                        local_input: inputs,
                        server_connection: stream,
                        ui_actions,
                    }),
                    tick_counter,
                ))
            },
            GameInitData::DedicatedServer(game_state, info) => {
                let replay = Replay::new(&game_state, None, data_store.clone());
                Ok(Self::DedicatedServer(
                    game_state,
                    replay,
                    GameStateUpdateHandler::new(Server::new(info)),
                ))
            },
            GameInitData::IntegratedServer {
                game_state,
                tick_counter,
                info,
                action_state_machine,
                inputs,
                ui_actions,
            } => {
                let replay = Replay::new(&*game_state.lock(), None, data_store.clone());
                Ok(Self::IntegratedServer(
                    game_state,
                    replay,
                    GameStateUpdateHandler::new(IntegratedServer {
                        local_actions: action_state_machine,
                        local_input: inputs,
                        server: Server::new(info),
                        ui_actions,
                    }),
                    tick_counter,
                ))
            },
        }
    }

    pub fn run(&mut self, data_store: &DataStore<ItemIdxType, RecipeIdxType>) -> ExitReason {
        let mut update_interval =
            spin_sleep_util::interval(Duration::from_secs(1) / TICKS_PER_SECOND_RUNSPEED as u32);

        loop {
            profiling::finish_frame!();
            profiling::scope!("Update Loop");

            {
                profiling::scope!("Wait");
                update_interval.tick();
            }

            match self.do_tick(data_store) {
                ControlFlow::Continue(_) => {},
                ControlFlow::Break(e) => return e,
            }
        }
    }

    fn do_tick(
        &mut self,
        data_store: &DataStore<ItemIdxType, RecipeIdxType>,
    ) -> ControlFlow<ExitReason> {
        match self {
            Game::Client(game_state, game_state_update_handler, tick_counter) => {
                game_state_update_handler.update::<&DataStore<ItemIdxType, RecipeIdxType>>(
                    &mut game_state.lock(),
                    None,
                    data_store,
                );
                tick_counter.fetch_add(1, std::sync::atomic::Ordering::Relaxed);
            },
            Game::DedicatedServer(game_state, replay, game_state_update_handler) => {
                game_state_update_handler.update(game_state, Some(replay), data_store)
            },
            Game::IntegratedServer(game_state, replay, game_state_update_handler, tick_counter) => {
                game_state_update_handler.update(
                    {
                        profiling::scope!("Wait for GameState Lock");
                        &mut game_state.lock()
                    },
                    Some(replay),
                    data_store,
                );
                tick_counter.fetch_add(1, std::sync::atomic::Ordering::Relaxed);
            },
        }

        ControlFlow::Continue(())
    }
}

impl<ItemIdxType: WeakIdxTrait, RecipeIdxType: WeakIdxTrait>
    ActionSource<ItemIdxType, RecipeIdxType> for Receiver<ActionType<ItemIdxType, RecipeIdxType>>
{
    fn get<'a>(
        &'a self,
        current_tick: u64,
        _: &World<ItemIdxType, RecipeIdxType>,
        _: &DataStore<ItemIdxType, RecipeIdxType>,
    ) -> impl IntoIterator<Item = ActionType<ItemIdxType, RecipeIdxType>>
           + use<'a, ItemIdxType, RecipeIdxType> {
        self.try_iter()
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
