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

use plumbing::Server;
#[cfg(feature = "client")]
use plumbing::{Client, IntegratedServer};
use server::{ActionSource, GameStateUpdateHandler, HandledActionConsumer};

#[cfg(feature = "client")]
use crate::frontend::action::action_state_machine::ActionStateMachine;
use crate::{
    TICKS_PER_SECOND_RUNSPEED,
    app_state::{AuxillaryData, GameState, SimulationState},
    data::DataStore,
    frontend::{action::ActionType, input::Input, world::tile::World},
    item::{IdxTrait, WeakIdxTrait},
    replays::Replay,
};

mod plumbing;
// mod protocol;
mod bad_tcp;
mod server;

pub mod connection_reciever_tcp;

pub(crate) enum Game<ItemIdxType: IdxTrait, RecipeIdxType: IdxTrait> {
    #[cfg(feature = "client")]
    Client(
        Arc<GameState<ItemIdxType, RecipeIdxType>>,
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
    #[cfg(feature = "client")]
    IntegratedServer(
        Arc<GameState<ItemIdxType, RecipeIdxType>>,
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
            #[cfg(feature = "client")]
            Game::Client(..) => {},
            Game::DedicatedServer(.., cancel_socket) => cancel_socket(),
            #[cfg(feature = "client")]
            Game::IntegratedServer(.., cancel_socket) => cancel_socket(),
        }
    }
}

pub struct ClientConnectionInfo {
    pub addr: SocketAddr,
}

pub struct ServerInfo {
    pub connections: Vec<TcpStream>,
    pub new_connection_recv: Receiver<TcpStream>,
}

pub enum GameInitData<ItemIdxType: WeakIdxTrait, RecipeIdxType: WeakIdxTrait> {
    #[cfg(feature = "client")]
    Client {
        game_state_start_fun: Box<
            dyn FnOnce(
                Arc<GameState<ItemIdxType, RecipeIdxType>>,
                Arc<Mutex<ActionStateMachine<ItemIdxType, RecipeIdxType>>>,
            ),
        >,
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
    #[cfg(feature = "client")]
    IntegratedServer {
        game_state: Arc<GameState<ItemIdxType, RecipeIdxType>>,
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

#[derive(Debug, serde::Serialize, serde::Deserialize)]
struct PlayerIDInformation {
    player_id: u16,
}

impl<ItemIdxType: IdxTrait, RecipeIdxType: IdxTrait> Game<ItemIdxType, RecipeIdxType> {
    pub fn new(
        init: GameInitData<ItemIdxType, RecipeIdxType>,
        data_store: &DataStore<ItemIdxType, RecipeIdxType>,
    ) -> Result<Self, std::io::Error> {
        match init {
            #[cfg(feature = "client")]
            GameInitData::Client {
                game_state_start_fun,
                inputs,
                tick_counter,
                info,
                ui_actions,
            } => {
                let stream = std::net::TcpStream::connect(info.addr)?;

                let mut buffer = vec![0; 1_000_000];
                let decoder = flate2::read::ZlibDecoder::new(stream);

                log::info!("Get player_id");
                let (PlayerIDInformation { player_id }, (decoder, _)) =
                    postcard::from_io((decoder, &mut buffer))
                        .expect("Could not recieve PlayerIDInformation from server!");
                log::info!("Get simulation_state");
                let (simulation_state, (decoder, _)): (
                    crate::get_size::Mutex<SimulationState<_, _>>,
                    _,
                ) = postcard::from_io((decoder, &mut buffer))
                    .expect("Could not recieve Game State from server!");
                log::info!("Get world");
                let (world, (decoder, _)): (crate::get_size::Mutex<World<_, _>>, _) =
                    postcard::from_io((decoder, &mut buffer))
                        .expect("Could not recieve Game State from server!");
                log::info!("Get aux_data");
                let (aux_data, (decoder, _)): (crate::get_size::Mutex<AuxillaryData>, _) =
                    postcard::from_io((decoder, &mut buffer))
                        .expect("Could not recieve Game State from server!");

                let game_state = Arc::new(GameState {
                    world,
                    simulation_state,
                    aux_data,
                });

                let action_state_machine =
                    Arc::new(Mutex::new(ActionStateMachine::new_from_gamestate(
                        player_id,
                        &*game_state.world.lock(),
                        &*game_state.simulation_state.lock(),
                        data_store,
                    )));

                let stream = decoder.into_inner();

                game_state_start_fun(game_state.clone(), action_state_machine.clone());

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
            #[cfg(feature = "client")]
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
                let replay = Replay::new(&game_state, None, data_store.clone());
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

            let is_client = {
                #[cfg(feature = "client")]
                {
                    matches!(self, Game::Client(_, _, _))
                }
                #[cfg(not(feature = "client"))]
                {
                    false
                }
            };
            if !env::var("ZOOM").is_ok() && !is_client {
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
            #[cfg(feature = "client")]
            Game::Client(game_state, game_state_update_handler, tick_counter) => {
                game_state_update_handler.update::<&DataStore<ItemIdxType, RecipeIdxType>>(
                    &game_state,
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
            ) => {
                {
                    profiling::scope!("GameState dedicated server autosave");
                    let simulation_state = game_state.simulation_state.lock();
                    let world = game_state.world.lock();
                    let aux_data = &mut *game_state.aux_data.lock();

                    // TODO: Autosave interval
                    if aux_data.current_tick % (60 * 60 * 1) == 0 {
                        // Autosave
                        #[cfg(not(all(target_arch = "wasm32", target_os = "unknown")))]
                        {
                            use crate::saving::save_with_fork;

                            profiling::scope!("Autosave");
                            // TODO: Handle overlapping saves
                            let _ = save_with_fork(
                                "dedicated_server_save",
                                Some("dedicated_server_save.save"),
                                &world,
                                &simulation_state,
                                &aux_data,
                                data_store,
                            );
                        }
                    }
                }
                log::trace!("Post Autosave");

                game_state_update_handler.update(game_state, Some(replay), data_store);
            },
            #[cfg(feature = "client")]
            Game::IntegratedServer(
                game_state,
                replay,
                game_state_update_handler,
                tick_counter,
                _cancel_socket,
            ) => {
                // #[cfg(debug_assertions)]
                // {
                //     profiling::scope!("Crash anticipation save to disk");
                //     save(&game_state, data_store);
                // }
                game_state_update_handler.update(game_state, Some(replay), data_store);
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
        _current_tick: u64,
        _: &World<ItemIdxType, RecipeIdxType>,
        _: &SimulationState<ItemIdxType, RecipeIdxType>,
        _: &AuxillaryData,
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
