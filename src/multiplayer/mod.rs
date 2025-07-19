use std::{
    env,
    net::{SocketAddr, TcpStream},
    ops::ControlFlow,
    sync::{
        Arc,
        atomic::{AtomicBool, AtomicU64},
        mpsc::Receiver,
    },
    time::Duration,
};

use parking_lot::Mutex;

use plumbing::{Client, IntegratedServer, Server};
use server::{ActionSource, GameStateUpdateHandler, HandledActionConsumer};

use crate::{
    TICKS_PER_SECOND_RUNSPEED,
    data::DataStore,
    frontend::{
        action::{ActionType, action_state_machine::ActionStateMachine},
        input::Input,
        world::tile::World,
    },
    item::{IdxTrait, WeakIdxTrait},
    rendering::app_state::GameState,
    replays::Replay,
    saving::save,
};

mod plumbing;
mod protocol;
mod server;

pub mod connection_reciever;

pub(super) enum Game<ItemIdxType: IdxTrait, RecipeIdxType: IdxTrait> {
    Client(
        Arc<Mutex<GameState<ItemIdxType, RecipeIdxType>>>,
        GameStateUpdateHandler<ItemIdxType, RecipeIdxType, Client<ItemIdxType, RecipeIdxType>>,
        Arc<AtomicU64>,
    ),
    DedicatedServer(
        GameState<ItemIdxType, RecipeIdxType>,
        Replay<ItemIdxType, RecipeIdxType, DataStore<ItemIdxType, RecipeIdxType>>,
        GameStateUpdateHandler<ItemIdxType, RecipeIdxType, Server<ItemIdxType, RecipeIdxType>>,
        Box<dyn FnMut() + Send + Sync>,
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
        Box<dyn FnMut() + Send + Sync>,
    ),
}

impl<ItemIdxType: IdxTrait, RecipeIdxType: IdxTrait> Drop for Game<ItemIdxType, RecipeIdxType> {
    fn drop(&mut self) {
        match self {
            Game::Client(mutex, game_state_update_handler, atomic_u64) => {},
            Game::DedicatedServer(game_state, replay, game_state_update_handler, cancel_socket) => {
                cancel_socket()
            },
            Game::IntegratedServer(
                mutex,
                replay,
                game_state_update_handler,
                atomic_u64,
                cancel_socket,
            ) => cancel_socket(),
        }
    }
}

pub struct ClientConnectionInfo {
    pub addr: SocketAddr,
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
    DedicatedServer(
        GameState<ItemIdxType, RecipeIdxType>,
        ServerInfo,
        Box<dyn FnMut() + Send + Sync>,
    ),
    IntegratedServer {
        game_state: Arc<Mutex<GameState<ItemIdxType, RecipeIdxType>>>,
        action_state_machine: Arc<Mutex<ActionStateMachine<ItemIdxType, RecipeIdxType>>>,
        inputs: Receiver<Input>,
        ui_actions: Receiver<ActionType<ItemIdxType, RecipeIdxType>>,
        tick_counter: Arc<AtomicU64>,
        info: ServerInfo,
        cancel_socket: Box<dyn FnMut() + Send + Sync>,
    },
}

pub enum ExitReason {
    LoopStopped,
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
                let stream = std::net::TcpStream::connect(info.addr)?;
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
            GameInitData::DedicatedServer(game_state, info, cancel_socket) => {
                #[cfg(debug_assertions)]
                let replay = Replay::new(&game_state, None, data_store.clone());
                #[cfg(not(debug_assertions))]
                let replay = Replay::new_dummy(data_store.clone());
                Ok(Self::DedicatedServer(
                    game_state,
                    replay,
                    GameStateUpdateHandler::new(Server::new(info)),
                    cancel_socket,
                ))
            },
            GameInitData::IntegratedServer {
                game_state,
                tick_counter,
                info,
                action_state_machine,
                inputs,
                ui_actions,
                cancel_socket,
            } => {
                #[cfg(debug_assertions)]
                let replay = Replay::new(&*game_state.lock(), None, data_store.clone());
                #[cfg(not(debug_assertions))]
                let replay = Replay::new_dummy(data_store.clone());
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
                    cancel_socket,
                ))
            },
        }
    }

    pub fn run(
        &mut self,
        stop: Arc<AtomicBool>,
        data_store: &DataStore<ItemIdxType, RecipeIdxType>,
    ) -> ExitReason {
        let mut update_interval =
            spin_sleep_util::interval(Duration::from_secs(1) / TICKS_PER_SECOND_RUNSPEED as u32);

        while stop.load(std::sync::atomic::Ordering::Relaxed) == false {
            profiling::finish_frame!();
            profiling::scope!("Update Loop");

            match self.do_tick(data_store) {
                ControlFlow::Continue(_) => {},
                ControlFlow::Break(e) => return e,
            }

            if !env::var("ZOOM").is_ok() {
                profiling::scope!("Wait");
                update_interval.tick();
            }
        }

        ExitReason::LoopStopped
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
            Game::DedicatedServer(
                game_state,
                replay,
                game_state_update_handler,
                _cancel_socket,
            ) => game_state_update_handler.update(game_state, Some(replay), data_store),
            Game::IntegratedServer(
                game_state,
                replay,
                game_state_update_handler,
                tick_counter,
                _cancel_socket,
            ) => {
                #[cfg(debug_assertions)]
                {
                    profiling::scope!("Crash anticipation save to disk");
                    save(&game_state.lock(), data_store, data_store.checksum.clone());
                }
                game_state_update_handler.update(
                    &mut *{
                        profiling::scope!("Wait for GameState Lock");
                        game_state.lock()
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
    ) -> impl Iterator<Item = ActionType<ItemIdxType, RecipeIdxType>> + use<'a, ItemIdxType, RecipeIdxType>
    {
        self.try_iter()
    }
}

impl<ItemIdxType: WeakIdxTrait, RecipeIdxType: WeakIdxTrait>
    HandledActionConsumer<ItemIdxType, RecipeIdxType> for ()
{
    fn consume(
        &mut self,
        _current_tick: u64,
        _actions: impl IntoIterator<Item = ActionType<ItemIdxType, RecipeIdxType>>,
    ) {
        // Do nothing
    }
}
