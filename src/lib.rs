#![feature(test)]
#![feature(portable_simd)]
#![feature(adt_const_params)]
#![feature(array_try_map)]
#![feature(never_type)]
#![feature(int_roundings)]
#![feature(vec_push_within_capacity)]
#![feature(iterator_try_collect)]
// the vec recycle crate will collide with Vec::recycle at some point. Once that happens I want to switch over to std anyway
#![allow(unstable_name_collisions)]

extern crate test;

// See https://docs.rs/built/latest/built/
pub mod built_info {
    // The file has been placed there by the build script.
    include!(concat!(env!("OUT_DIR"), "/built.rs"));
}

#[cfg(all(target_arch = "wasm32", target_os = "unknown"))]
use eframe::web_sys;

use std::{
    borrow::Borrow,
    net::{SocketAddr, TcpStream},
    process::exit,
    simd::Mask,
    sync::{
        Arc,
        atomic::{AtomicBool, AtomicU64, Ordering},
        mpsc::{Sender, channel},
    },
    thread,
};

use parking_lot::Mutex;

use app_state::GameState;
use data::{DataStore, factorio_1_1::get_raw_data_test};
#[cfg(feature = "client")]
#[cfg(not(target_arch = "wasm32"))]
use eframe::NativeOptions;
use frontend::world::Position;
#[cfg(feature = "client")]
use frontend::{action::action_state_machine::ActionStateMachine, input::Input};
use item::{IdxTrait, WeakIdxTrait};
use multiplayer::{
    ClientConnectionInfo, Game, GameInitData, ServerInfo,
    connection_reciever_tcp::accept_continously,
};
use power::Watt;
#[cfg(feature = "client")]
use rendering::{
    eframe_app,
    window::{LoadedGame, LoadedGameSized},
};

use saving::load;
use std::path::PathBuf;

use crate::item::Indexable;
#[cfg(feature = "client")]
use crate::{progress_info::ProgressInfo, replays::GenerationInformation};

const TICKS_PER_SECOND_LOGIC: u64 = 60;

const TICKS_PER_SECOND_RUNSPEED: u64 = 60;

pub mod get_size;

pub mod assembler;
pub mod belt;
pub mod inserter;
pub mod item;
pub mod lab;
pub mod mining_drill;
pub mod power;
pub mod progress_info;
pub mod research;

// For future modding capabilities
// pub mod scenario;

#[cfg(test)]
pub mod test_world_harness;

#[cfg(feature = "client")]
mod example_worlds;

mod shopping_list_arena;
mod temp_vec;

// This is an experiment. Before I can use it, I need to run it through a miri gauntlet
// mod small_box_slice;

pub mod data;
pub mod mod_manager;

pub mod frontend;

pub mod app_state;

#[cfg(feature = "client")]
pub mod rendering;

pub mod bot_system;

mod statistics;

pub mod replays;

mod saving;

mod multiplayer;

mod storage_list;

pub mod split_arbitrary;

pub mod join_many;

mod chest;

pub mod blueprint;

mod network_graph;

pub mod liquid;

mod par_generation;

// In progress
// mod bucket_store;
mod lockfile;

impl WeakIdxTrait for u8 {}
impl WeakIdxTrait for u16 {}
impl IdxTrait for u8 {}
impl IdxTrait for u16 {}

impl Indexable for u8 {
    fn into_usize(self) -> usize {
        self.into()
    }
}
impl Indexable for u16 {
    fn into_usize(self) -> usize {
        self.into()
    }
}

pub static DATA_STORE: std::sync::LazyLock<DataStore<u8, u8>> =
    std::sync::LazyLock::new(|| get_raw_data_test().turn::<u8, u8>());

pub trait NewWithDataStore {
    fn new<ItemIdxType: IdxTrait, RecipeIdxType: IdxTrait>(
        data_store: impl Borrow<DataStore<ItemIdxType, RecipeIdxType>>,
    ) -> Self;
}

impl<T: Default> NewWithDataStore for T {
    fn new<ItemIdxType: IdxTrait, RecipeIdxType: IdxTrait>(
        _data_store: impl Borrow<DataStore<ItemIdxType, RecipeIdxType>>,
    ) -> Self {
        T::default()
    }
}

fn get_version() -> &'static str {
    if crate::built_info::GIT_HEAD_REF == Some("refs/head/master") {
        crate::built_info::PKG_VERSION
    } else {
        let version = crate::built_info::GIT_VERSION.unwrap_or(
            crate::built_info::GIT_COMMIT_HASH_SHORT.unwrap_or("Could not get git version"),
        );
        version
    }
}

#[cfg(not(target_arch = "wasm32"))]
#[allow(unused)]
pub fn main(input: &Vec<String>) -> Result<(), args::ArgsError> {
    // use ron::ser::PrettyConfig;

    // let raw = crate::data::factorio_1_1::get_raw_data_fn();

    // let str = ron::ser::to_string_pretty(&raw, PrettyConfig::new()).unwrap();
    // println!("{}", str);
    // return Ok(());

    puffin::set_scopes_on(true);

    #[cfg(feature = "logging")]
    simple_logger::SimpleLogger::new()
        .with_level(log::LevelFilter::Warn)
        .env()
        .init()
        .unwrap();
    log::info!("Welcome to main on native");

    #[cfg(feature = "client")]
    {
        eframe::run_native(
            "FactoryGame",
            NativeOptions {
                // depth_buffer: 32,
                ..Default::default()
            },
            Box::new(|cc| {
                let app = eframe_app::App::new(cc);

                Ok(Box::new(app))
            }),
        )
        .unwrap();

        Ok(())
    }

    #[cfg(not(feature = "client"))]
    {
        use crate::saving::save_folder;

        log::info!("This is the dedicated server");
        let mut args = args::Args::new("factory", "FactoryGame dedicated server");

        args.flag("h", "help", "Print the usage menu");
        args.flag("c", "create", "Create a new world");
        args.flag(
            "o",
            "override",
            "Allows overriding an existing world when creating world",
        );

        args.parse(input)?;
        log::trace!("Parsed input");

        let help = args.value_of("help")?;
        if help {
            println!("{}", args.full_usage());
            return Ok(());
        }

        let create = args.value_of("create")?;
        let overwrite = args.value_of("override")?;

        log::info!("Running Dedicated server");
        let save_path = save_folder().join("dedicated_server_save.save");
        log::info!("Loading Save game from {:?}", &save_path);

        let start_game = if create {
            StartGameInfo::Create {
                name: "dedicated_server_save.save".to_string(),
                info: GameCreationInfo::Empty,
                allow_overwrite: overwrite,
            }
        } else {
            StartGameInfo::Load(save_path)
        };
        run_dedicated_server(start_game);
    }
}

#[cfg(target_arch = "wasm32")]
pub fn main(_input: &Vec<String>) -> Result<(), args::ArgsError> {
    console_error_panic_hook::set_once();

    puffin::set_scopes_on(true);
    use eframe::wasm_bindgen::JsCast as _;

    // Redirect `log` message to `console.log` and friends:
    eframe::WebLogger::init(log::LevelFilter::Warn).ok();
    log::info!("Welcome to main on wasm");

    let web_options = eframe::WebOptions::default();

    wasm_bindgen_futures::spawn_local(async {
        let document = web_sys::window()
            .expect("No window")
            .document()
            .expect("No document");

        let canvas = document
            .get_element_by_id("the_canvas_id")
            .expect("Failed to find the_canvas_id")
            .dyn_into::<web_sys::HtmlCanvasElement>()
            .expect("the_canvas_id was not a HtmlCanvasElement");

        let start_result = eframe::WebRunner::new()
            .start(
                canvas,
                web_options,
                Box::new(|cc| Ok(Box::new(eframe_app::App::new(cc)))),
            )
            .await;

        // Remove the loading text and spinner:
        if let Some(loading_text) = document.get_element_by_id("loading_text") {
            match start_result {
                Ok(_) => {
                    loading_text.remove();
                },
                Err(e) => {
                    loading_text.set_inner_html(
                        "<p> The app has crashed. See the developer console for details. </p>",
                    );
                    panic!("Failed to start eframe: {e:?}");
                },
            }
        }
    });

    Ok(())
}

enum StartGameInfo {
    Load(PathBuf),
    LoadReadable(PathBuf),
    Create {
        name: String,
        gen_info: GenerationInformation,
        info: GameCreationInfo,
        allow_overwrite: bool,
    },
}

enum GameCreationInfo {
    Empty,
    RedGreen,
    RedGreenBelts,

    RedWithLabs,

    Megabase(bool),

    Gigabase(u16),

    LotsOfBelts,

    SolarField(Watt, Position),

    TrainRide,

    FromBP(PathBuf),
}

#[cfg(feature = "client")]
fn run_integrated_server(
    progress: ProgressInfo,
    game_creation_fn: impl FnOnce(ProgressInfo, &DataStore<u8, u8>) -> GameState<u8, u8>,

    // FIXME: This type is wrong
    listen_addr: Option<&'static str>,
) -> (LoadedGame, Arc<AtomicU64>, Sender<Input>) {
    // TODO: Do mod loading here
    let raw_data = get_raw_data_test();
    let data_store = raw_data.process();

    let tick_counter: Arc<AtomicU64> = Arc::new(AtomicU64::new(0));

    let (new_conn_send, new_conn_recv) = channel();
    let cancel: Arc<AtomicBool> = Default::default();
    if let Some(listen_addr) = listen_addr {
        accept_continously(listen_addr, new_conn_send, cancel.clone()).unwrap();
    }

    match data_store {
        data::DataStoreOptions::ItemU8RecipeU8(data_store) => {
            let (send, recv) = channel();

            let game_state = game_creation_fn(progress, &data_store);
            let game_state = Arc::new(game_state);

            let state_machine: Arc<Mutex<ActionStateMachine<_, _>>> =
                Arc::new(Mutex::new(ActionStateMachine::new_from_gamestate(
                    0,
                    &*game_state.world.lock(),
                    &*game_state.simulation_state.lock(),
                    &data_store,
                )));

            let (ui_sender, ui_recv) = channel();

            let mut game = Game::new(
                GameInitData::IntegratedServer {
                    game_state: game_state.clone(),
                    tick_counter: tick_counter.clone(),
                    info: ServerInfo {
                        connections: vec![],
                        new_connection_recv: new_conn_recv,
                    },
                    action_state_machine: state_machine.clone(),
                    inputs: recv,
                    ui_actions: ui_recv,
                    cancel_socket: Box::new(move || {
                        cancel.store(true, Ordering::SeqCst);
                        // This is a little hack. Our connection accept thread is stuck waiting for connections and will only exit if anything connects.
                        // So we just connect to ourselves :)
                        // See https://stackoverflow.com/questions/56692961/how-do-i-gracefully-exit-tcplistener-incoming
                        if let Some(local_addr) = listen_addr {
                            let _ = TcpStream::connect(local_addr);
                        }
                    }),
                },
                &data_store,
            )
            .unwrap();

            let stop = Arc::new(AtomicBool::new(false));

            let m_data_store = data_store.clone();
            let m_stop: Arc<AtomicBool> = stop.clone();

            #[cfg(not(target_arch = "wasm32"))]
            thread::spawn(move || {
                profiling::register_thread!("Game Update Thread");
                game.run(m_stop, &m_data_store);
            });

            let data_store = Arc::new(Mutex::new(data_store));
            return (
                LoadedGame::ItemU8RecipeU8(LoadedGameSized {
                    state: game_state,
                    state_machine,
                    data_store,
                    ui_action_sender: ui_sender,

                    stop_update_thread: stop,
                }),
                tick_counter,
                send,
            );
        },
        _ => todo!(),
    }
}

fn run_dedicated_server(start_game_info: StartGameInfo) -> ! {
    // TODO: Do mod loading here
    let raw_data = get_raw_data_test();
    let data_store = raw_data.process();

    // let progress = ProgressInfo::new();

    let local_addr = "0.0.0.0:42069";
    let cancel: Arc<AtomicBool> = Default::default();

    log::warn!("Hosting on {}", &local_addr);

    let (new_conn_send, new_conn_recv) = channel();

    accept_continously(local_addr, new_conn_send, cancel.clone()).unwrap();

    match data_store {
        data::DataStoreOptions::ItemU8RecipeU8(data_store) => {
            let game_state = match start_game_info {
                StartGameInfo::Load(path_buf) => load(path_buf)
                    .map(|save| save.game_state)
                    .expect("Could not load game"),
                StartGameInfo::LoadReadable(_path_buf) => unimplemented!(),
                StartGameInfo::Create {
                    name,
                    gen_info,
                    info: _info,
                    allow_overwrite,
                } => {
                    if !allow_overwrite {
                        log::error!(
                            "Currently allow_overwrite is ignored and overwriting is always allowed!!!!"
                        );
                    }
                    GameState::new(name, gen_info, &data_store)
                },
            };

            let mut game = Game::new(
                GameInitData::DedicatedServer(
                    game_state,
                    ServerInfo {
                        connections: vec![],
                        new_connection_recv: new_conn_recv,
                    },
                    Box::new(move || {
                        cancel.store(true, Ordering::Relaxed);
                        // This is a little hack. Our connection accept thread is stuck waiting for connections and will only exit if anything connects.
                        // So we just connect to ourselves :)
                        // See https://stackoverflow.com/questions/56692961/how-do-i-gracefully-exit-tcplistener-incoming
                        let _ = TcpStream::connect(local_addr);
                    }),
                ),
                &data_store,
            )
            .unwrap();

            let stop = Arc::new(AtomicBool::new(false));

            let data_store = Arc::new(data_store);
            match game.run(stop, &data_store) {
                multiplayer::ExitReason::LoopStopped => exit(0),
            }
        },
        _ => todo!(),
    }
}

#[cfg(feature = "client")]
fn run_client(
    remote_addr: SocketAddr,
    game_state_sender: Sender<(LoadedGame, Arc<AtomicU64>, Sender<Input>)>,
) {
    // TODO: Do mod loading here
    let raw_data = get_raw_data_test();
    let data_store = raw_data.process();

    let tick_counter: Arc<AtomicU64> = Arc::new(AtomicU64::new(0));

    match data_store {
        data::DataStoreOptions::ItemU8RecipeU8(data_store) => {
            let (send, recv) = channel();

            let (ui_sender, ui_recv) = channel();
            let stop = Arc::new(AtomicBool::new(false));
            let m_stop = stop.clone();

            let m_data_store = data_store.clone();
            let data_store = Arc::new(Mutex::new(data_store));
            let m_tick_counter = tick_counter.clone();
            let mut game = Game::new(
                GameInitData::Client {
                    game_state_start_fun: Box::new(move |game_state, state_machine| {
                        log::info!("GameState Recieved Successfully");

                        game_state_sender
                            .send((
                                LoadedGame::ItemU8RecipeU8(LoadedGameSized {
                                    state: game_state,
                                    state_machine,
                                    data_store,
                                    ui_action_sender: ui_sender,
                                    stop_update_thread: stop,
                                }),
                                tick_counter,
                                send,
                            ))
                            .unwrap();
                    }),
                    inputs: recv,
                    tick_counter: m_tick_counter,
                    info: ClientConnectionInfo { addr: remote_addr },
                    ui_actions: ui_recv,
                },
                &m_data_store,
            )
            .expect("Could not start Game");

            thread::spawn(move || {
                game.run(m_stop, &m_data_store);
            });
        },
        _ => todo!(),
    }
}

use std::simd::Simd;

// TODO: Increase if possible
type MASKTYPE = Mask<i16, 16>;
type SIMDTYPE = Simd<u16, 16>;

pub struct InserterInfo {
    num_items: u8,
}

pub fn simple(
    locs: &mut [bool],
    inserter_info: &mut [InserterInfo],
    inserter_ouput_idx: &[u8],
    outputs: &mut [u16],
) {
    const HANDSIZE: u8 = 8;

    assert!(locs.len() == inserter_info.len());
    assert!(locs.len() % 64 == 0);
    assert!(inserter_info.len() % 64 == 0);

    locs.iter_mut()
        .zip(inserter_info.iter_mut().zip(inserter_ouput_idx.iter()))
        .for_each(|(l, (i, idx))| {
            let item = *l;
            *l = *l && i.num_items >= 8;
            let did_increase = item;
            i.num_items = u8::from(item) + i.num_items % HANDSIZE;
            outputs[*idx as usize] += u16::from(did_increase && (i.num_items == 0));
        });
}

#[cfg(test)]
mod tests {
    use std::sync::Arc;

    use test::Bencher;

    use crate::{
        app_state::GameState, data::factorio_1_1::get_raw_data_test, progress_info::ProgressInfo,
        replays::GenerationInformation,
    };

    #[bench]
    fn clone_empty_simulation(b: &mut Bencher) {
        let data_store = get_raw_data_test().process().assume_simple();

        let game_state = GameState::new(
            "Test World".to_string(),
            GenerationInformation::default(),
            &data_store,
        );

        b.iter(|| game_state.clone());
    }

    #[bench]
    fn empty_simulation(b: &mut Bencher) {
        let data_store = get_raw_data_test().process().assume_simple();

        let game_state = Arc::new(GameState::new(
            "Test World".to_string(),
            GenerationInformation::default(),
            &data_store,
        ));

        b.iter(|| {
            GameState::update(
                &mut *game_state.simulation_state.lock(),
                &mut *game_state.aux_data.lock(),
                &data_store,
            )
        });
    }

    #[bench]
    fn bench_megabase(b: &mut Bencher) {
        let data_store = get_raw_data_test().process().assume_simple();

        let game_state = GameState::new_with_megabase(
            "Test World".to_string(),
            GenerationInformation::default(),
            false,
            ProgressInfo::new(),
            &data_store,
        );

        b.iter(|| {
            GameState::update(
                &mut *game_state.simulation_state.lock(),
                &mut *game_state.aux_data.lock(),
                &data_store,
            );
        })
    }
}
