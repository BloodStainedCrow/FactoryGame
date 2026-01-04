#![feature(test)]
#![feature(portable_simd)]
#![feature(adt_const_params)]
#![feature(array_try_map)]
#![feature(never_type)]
#![feature(mixed_integer_ops_unsigned_sub)]
#![feature(int_roundings)]
#![feature(strict_overflow_ops)]
#![feature(vec_push_within_capacity)]

extern crate test;

// See https://docs.rs/built/latest/built/
pub mod built_info {
    // The file has been placed there by the build script.
    include!(concat!(env!("OUT_DIR"), "/built.rs"));
}

#[cfg(all(target_arch = "wasm32", target_os = "unknown"))]
use eframe::web_sys;

#[cfg(feature = "client")]
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
#[cfg(not(all(target_arch = "wasm32", target_os = "unknown")))]
use eframe::NativeOptions;
use frontend::world::{Position, tile::CHUNK_SIZE_FLOAT};
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

#[cfg(not(feature = "client"))]
use directories::ProjectDirs;

use saving::{load, load_readable};
use std::path::PathBuf;

use crate::item::Indexable;

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
pub mod research;

pub mod scenario;

mod shopping_list_arena;

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

#[cfg(not(all(target_arch = "wasm32", target_os = "unknown")))]
pub fn main() -> Result<(), ()> {
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
        info!("Running Dedicated server!");
        // let dir = ProjectDirs::from("de", "aschhoff", "factory_game").expect("No Home path found");
        // let save_file_dir = dir.data_dir().join("save.save");
        // run_dedicated_server(StartGameInfo::Load(save_file_dir));
        run_dedicated_server(StartGameInfo::Load(
            "/home/tim/.local/share/factory_game/save.save"
                .try_into()
                .unwrap(),
        ));
    }
}

#[cfg(target_arch = "wasm32")]
pub fn main() {
    puffin::set_scopes_on(true);
    use eframe::wasm_bindgen::JsCast as _;

    // Redirect `log` message to `console.log` and friends:
    eframe::WebLogger::init(log::LevelFilter::Error).ok();

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
}

enum StartGameInfo {
    Load(PathBuf),
    LoadReadable(PathBuf),
    Create {
        name: String,
        info: GameCreationInfo,
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
    progress: Arc<AtomicU64>,
    start_game_info: StartGameInfo,

    // FIXME: This type is wrong
    listen_addr: Option<&'static str>,
) -> (LoadedGame, Arc<AtomicU64>, Sender<Input>) {
    // TODO: Do mod loading here
    let raw_data = get_raw_data_test();
    let data_store = raw_data.process();

    let tick_counter: Arc<AtomicU64> = Arc::new(AtomicU64::new(0));

    let connections: Arc<Mutex<Vec<std::net::TcpStream>>> = Arc::default();

    let cancel: Arc<AtomicBool> = Default::default();
    if let Some(listen_addr) = listen_addr {
        accept_continously(listen_addr, connections.clone(), cancel.clone()).unwrap();
    }

    match data_store {
        data::DataStoreOptions::ItemU8RecipeU8(data_store) => {
            let (send, recv) = channel();
            let state_machine: Arc<Mutex<ActionStateMachine<_, _>>> =
                Arc::new(Mutex::new(ActionStateMachine::new(
                    0,
                    (100.0 * CHUNK_SIZE_FLOAT, 100.0 * CHUNK_SIZE_FLOAT),
                    &data_store,
                )));

            let game_state = Arc::new(match start_game_info {
                StartGameInfo::Load(path) => load(path)
                    .map(|sg| {
                        assert_eq!(
                            sg.checksum, data_store.checksum,
                            "A savegame can only be loaded with the EXACT same mods!"
                        );
                        sg.game_state
                    })
                    .unwrap(),
                StartGameInfo::LoadReadable(path) => load_readable(path)
                    .map(|sg| {
                        assert_eq!(
                            sg.checksum, data_store.checksum,
                            "A savegame can only be loaded with the EXACT same mods!"
                        );
                        sg.game_state
                    })
                    .unwrap(),
                StartGameInfo::Create { name, info } => {
                    assert!(
                        name.is_ascii(),
                        "For now only ASCII game names are allowed, since they are used as the filename"
                    );
                    match info {
                        GameCreationInfo::Empty => GameState::new(name, &data_store),
                        GameCreationInfo::Megabase(use_solar_field) => {
                            GameState::new_with_megabase(
                                name,
                                use_solar_field,
                                progress,
                                &data_store,
                            )
                        },
                        GameCreationInfo::Gigabase(count) => {
                            GameState::new_with_gigabase(name, count, progress, &data_store)
                        },
                        GameCreationInfo::SolarField(wattage, base_pos) => {
                            GameState::new_with_tons_of_solar(
                                name,
                                wattage,
                                base_pos,
                                None,
                                progress,
                                &data_store,
                            )
                        },
                        GameCreationInfo::LotsOfBelts => {
                            GameState::new_with_lots_of_belts(name, progress, &data_store)
                        },
                        GameCreationInfo::TrainRide => {
                            GameState::new_with_world_train_ride(name, progress, &data_store)
                        },

                        GameCreationInfo::FromBP(path) => {
                            GameState::new_with_bp(name, &data_store, path)
                        },

                        _ => unimplemented!(),
                    }
                },
            });

            let (ui_sender, ui_recv) = channel();

            let mut game = Game::new(
                GameInitData::IntegratedServer {
                    game_state: game_state.clone(),
                    tick_counter: tick_counter.clone(),
                    info: ServerInfo { connections },
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

    // let progress = Default::default();

    let connections: Arc<Mutex<Vec<std::net::TcpStream>>> = Arc::default();

    let local_addr = "127.0.0.1:8080";
    let cancel: Arc<AtomicBool> = Default::default();

    accept_continously(local_addr, connections.clone(), cancel.clone()).unwrap();

    match data_store {
        data::DataStoreOptions::ItemU8RecipeU8(data_store) => {
            let game_state = load(todo!("Add a console argument for the save file path"))
                .map(|save| save.game_state)
                .unwrap_or_else(|| GameState::new("Server Save".to_string(), &data_store));

            let mut game = Game::new(
                GameInitData::DedicatedServer(
                    game_state,
                    ServerInfo { connections },
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
                multiplayer::ExitReason::UserQuit => exit(0),
                multiplayer::ExitReason::ConnectionDropped => exit(1),
                multiplayer::ExitReason::LoopStopped => exit(0),
            }
        },
        _ => todo!(),
    }
}

#[cfg(feature = "client")]
fn run_client(remote_addr: SocketAddr) -> (LoadedGame, Arc<AtomicU64>, Sender<Input>) {
    // TODO: Do mod loading here
    let raw_data = get_raw_data_test();
    let data_store = raw_data.process();

    let tick_counter: Arc<AtomicU64> = Arc::new(AtomicU64::new(0));

    match data_store {
        data::DataStoreOptions::ItemU8RecipeU8(data_store) => {
            let (send, recv) = channel();
            let state_machine: Arc<Mutex<ActionStateMachine<_, _>>> =
                Arc::new(Mutex::new(ActionStateMachine::new(
                    1,
                    (100.0 * CHUNK_SIZE_FLOAT, 100.0 * CHUNK_SIZE_FLOAT),
                    &data_store,
                )));

            let game_state = Arc::new(
                // FIXME: When running in client mode, we should download the gamestate from the server instead of loading it from disk
                GameState::new("FIXME".to_string(), &data_store),
            );

            let (ui_sender, ui_recv) = channel();

            let mut game = Game::new(
                GameInitData::Client {
                    game_state: game_state.clone(),
                    action_state_machine: state_machine.clone(),
                    inputs: recv,
                    tick_counter: tick_counter.clone(),
                    info: ClientConnectionInfo { addr: remote_addr },
                    ui_actions: ui_recv,
                },
                &data_store,
            )
            .expect("Could not start Game");

            let stop = Arc::new(AtomicBool::new(false));

            let m_data_store = data_store.clone();
            let m_stop = stop.clone();
            thread::spawn(move || {
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
    use std::{iter, rc::Rc};

    use test::{Bencher, black_box};

    use crate::{
        TICKS_PER_SECOND_LOGIC,
        app_state::GameState,
        data::factorio_1_1::get_raw_data_test,
        frontend::{action::ActionType, world::Position},
        replays::{Replay, run_till_finished},
    };

    #[bench]
    fn clone_empty_simulation(b: &mut Bencher) {
        let data_store = get_raw_data_test().process().assume_simple();

        let game_state = GameState::new("Test World".to_string(), &data_store);

        let replay = Replay::new(&game_state, None, Rc::new(data_store));

        b.iter(|| replay.clone());
    }

    #[bench]
    fn empty_simulation(b: &mut Bencher) {
        // 1 hour
        const NUM_TICKS: u64 = TICKS_PER_SECOND_LOGIC * 60 * 60;

        let data_store = get_raw_data_test().process().assume_simple();

        let game_state = GameState::new("Test World".to_string(), &data_store);

        let mut replay = Replay::new(&game_state, None, Rc::new(data_store));

        for _ in 0..NUM_TICKS {
            replay.tick();
        }

        replay.finish();

        b.iter(|| black_box(replay.clone().run().with(run_till_finished)));
    }

    #[bench]
    fn noop_actions_simulation(b: &mut Bencher) {
        // 1 hour
        const NUM_TICKS: u64 = TICKS_PER_SECOND_LOGIC * 60 * 60;

        let data_store = get_raw_data_test().process().assume_simple();

        let game_state = GameState::new("Test World".to_string(), &data_store);

        let mut replay = Replay::new(&game_state, None, Rc::new(data_store));

        for _ in 0..NUM_TICKS {
            replay.append_actions(
                iter::repeat(ActionType::Ping(Position { x: 100, y: 100 })).take(5),
            );
            replay.tick();
        }

        replay.finish();

        b.iter(|| replay.clone().run().with(run_till_finished));
    }

    // #[rstest]
    // fn crashing_replays(#[files("crash_replays/*.rep")] path: PathBuf) {
    //     use std::io::Read;

    //     // Keep running for 30 seconds
    //     const RUNTIME_AFTER_PRESUMED_CRASH: u64 = 30 * 60;

    //     let mut file = File::open(&path).unwrap();

    //     let mut v = Vec::with_capacity(file.metadata().unwrap().len() as usize);

    //     file.read_to_end(&mut v).unwrap();

    //     // TODO: For non u8 IdxTypes this will fail
    //     let mut replay: Replay<u8, u8, DataStore<u8, u8>> = bitcode::deserialize(v.as_slice())
    //         .expect(
    //             format!("Test replay {path:?} did not deserialize, consider removing it.").as_str(),
    //         );

    //     replay.finish();

    //     let running_replay = replay.run();

    //     let (mut game_state_before_crash, data_store) = running_replay.with(run_till_finished);

    //     for _ in 0..RUNTIME_AFTER_PRESUMED_CRASH {
    //         game_state_before_crash.update(&data_store);
    //     }
    // }

    // #[bench]
    // fn bench_huge_red_green_sci(b: &mut Bencher) {
    //     let game_state = GameState::new_with_beacon_red_green_production_many_grids(
    //         Default::default(),
    //         &DATA_STORE,
    //     );

    //     let mut game_state = game_state.clone();

    //     b.iter(|| {
    //         game_state.update(&DATA_STORE);
    //     })
    // }

    // #[bench]
    // fn bench_12_beacon_red(b: &mut Bencher) {
    //     let game_state =
    //         GameState::new_with_beacon_belt_production(Default::default(), &DATA_STORE);

    //     let mut game_state = game_state.clone();

    //     b.iter(|| {
    //         game_state.update(&DATA_STORE);
    //     })
    // }
}
