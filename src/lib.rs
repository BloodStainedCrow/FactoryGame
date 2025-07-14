#![feature(test)]
#![feature(portable_simd)]
#![feature(adt_const_params)]
#![feature(array_try_map)]
#![feature(never_type)]
#![feature(mixed_integer_ops_unsigned_sub)]

extern crate test;

use std::{
    array,
    borrow::Borrow,
    env,
    net::{IpAddr, Ipv4Addr},
    process::exit,
    simd::cmp::SimdPartialEq,
    sync::{
        atomic::AtomicU64,
        mpsc::{channel, Sender},
        Arc,
    },
    thread,
};

use parking_lot::Mutex;

use data::{factorio_1_1::get_raw_data_test, DataStore};
use eframe::NativeOptions;
use frontend::{
    action::action_state_machine::ActionStateMachine, input::Input, world::tile::CHUNK_SIZE_FLOAT,
};
use item::{IdxTrait, WeakIdxTrait};
use multiplayer::{
    connection_reciever::accept_continously, ClientConnectionInfo, Game, GameInitData, ServerInfo,
};
use rendering::{
    app_state::GameState,
    eframe_app,
    window::{LoadedGame, LoadedGameSized},
};
use saving::load;
use simple_logger::SimpleLogger;
use std::path::PathBuf;

use crate::item::Indexable;

const TICKS_PER_SECOND_LOGIC: u64 = 60;

const TICKS_PER_SECOND_RUNSPEED: u64 = 60;

pub mod assembler;
pub mod belt;
pub mod inserter;
pub mod item;
pub mod lab;
pub mod mining_drill;
pub mod power;
pub mod research;

pub mod data;
pub mod mod_manager;

pub mod frontend;

pub mod rendering;

pub mod bot_system;

mod statistics;

pub mod replays;

mod saving;

mod multiplayer;

mod storage_list;

pub mod split_arbitrary;

mod chest;

pub mod blueprint;

mod network_graph;

mod canonical;

pub mod liquid;

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

pub fn main() -> Result<(), ()> {
    puffin::set_scopes_on(true);

    SimpleLogger::new()
        .with_level(log::LevelFilter::Warn)
        .env()
        .init()
        .unwrap();

    let mode = env::args().nth(1);

    if Some("--dedicated") == mode.as_deref() {
        run_dedicated_server(StartGameInfo::Load("".try_into().unwrap()));
    } else {
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
}

enum StartGameInfo {
    Load(PathBuf),
    Create(GameCreationInfo),
}

enum GameCreationInfo {
    Empty,
    RedGreen,
    RedGreenBelts,

    RedWithLabs,
}

fn run_integrated_server(
    progress: Arc<AtomicU64>,
    start_game_info: StartGameInfo,
) -> (LoadedGame, Arc<AtomicU64>, Sender<Input>) {
    // TODO: Do mod loading here
    let raw_data = get_raw_data_test();
    let data_store = raw_data.process();

    let tick_counter: Arc<AtomicU64> = Arc::new(AtomicU64::new(0));

    let connections: Arc<Mutex<Vec<std::net::TcpStream>>> = Arc::default();

    accept_continously(connections.clone()).unwrap();

    match data_store {
        data::DataStoreOptions::ItemU8RecipeU8(data_store) => {
            let (send, recv) = channel();
            let state_machine: Arc<Mutex<ActionStateMachine<_, _>>> =
                Arc::new(Mutex::new(ActionStateMachine::new(
                    0,
                    (100.0 * CHUNK_SIZE_FLOAT, 100.0 * CHUNK_SIZE_FLOAT),
                    &data_store,
                )));

            let game_state = Arc::new(Mutex::new(match start_game_info {
                StartGameInfo::Load(path) => load(path)
                    .map(|sg| {
                        assert_eq!(
                            sg.checksum, data_store.checksum,
                            "A savegame can only be loaded with the EXACT same mods!"
                        );
                        sg.game_state
                    })
                    .expect("Loading from disk failed!"),
                StartGameInfo::Create(info) => match info {
                    GameCreationInfo::Empty => GameState::new(&data_store),
                    GameCreationInfo::RedGreen => {
                        GameState::new_with_beacon_production(progress, &data_store)
                    },
                    GameCreationInfo::RedGreenBelts => {
                        GameState::new_with_beacon_belt_production(progress, &data_store)
                    },
                    GameCreationInfo::RedWithLabs => {
                        GameState::new_with_production(progress, &data_store)
                    },
                },
            }));

            let (ui_sender, ui_recv) = channel();

            let mut game = Game::new(
                GameInitData::IntegratedServer {
                    game_state: game_state.clone(),
                    tick_counter: tick_counter.clone(),
                    info: ServerInfo { connections },
                    action_state_machine: state_machine.clone(),
                    inputs: recv,
                    ui_actions: ui_recv,
                },
                &data_store,
            )
            .unwrap();

            let m_data_store = data_store.clone();
            thread::spawn(move || {
                profiling::register_thread!("Game Update Thread");
                game.run(&m_data_store);
            });

            let data_store = Arc::new(Mutex::new(data_store));
            return (
                LoadedGame::ItemU8RecipeU8(LoadedGameSized {
                    state: game_state,
                    state_machine,
                    data_store,
                    ui_action_sender: ui_sender,
                }),
                tick_counter,
                send,
            );
        },
        data::DataStoreOptions::ItemU8RecipeU16(data_store) => todo!(),
        data::DataStoreOptions::ItemU16RecipeU8(data_store) => todo!(),
        data::DataStoreOptions::ItemU16RecipeU16(data_store) => todo!(),
    }
}

fn run_dedicated_server(start_game_info: StartGameInfo) -> ! {
    // TODO: Do mod loading here
    let raw_data = get_raw_data_test();
    let data_store = raw_data.process();

    let progress = Default::default();

    let connections: Arc<Mutex<Vec<std::net::TcpStream>>> = Arc::default();

    accept_continously(connections.clone()).unwrap();

    match data_store {
        data::DataStoreOptions::ItemU8RecipeU8(data_store) => {
            let game_state = load(todo!("Add a console argument for the save file path"))
                .map(|save| save.game_state)
                .unwrap_or_else(|| {
                    // GameState::new(&data_store)
                    GameState::new_with_beacon_production(progress, &data_store)
                });

            let mut game = Game::new(
                GameInitData::DedicatedServer(game_state, ServerInfo { connections }),
                &data_store,
            )
            .unwrap();

            let data_store = Arc::new(data_store);
            match game.run(&data_store) {
                multiplayer::ExitReason::UserQuit => exit(0),
                multiplayer::ExitReason::ConnectionDropped => exit(1),
            }
        },
        data::DataStoreOptions::ItemU8RecipeU16(data_store) => todo!(),
        data::DataStoreOptions::ItemU16RecipeU8(data_store) => todo!(),
        data::DataStoreOptions::ItemU16RecipeU16(data_store) => todo!(),
    }
}

fn run_client(start_game_info: StartGameInfo) -> (LoadedGame, Arc<AtomicU64>, Sender<Input>) {
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

            let game_state = Arc::new(Mutex::new(
                load(todo!("When running in client mode, we should download the gamestate from the server instead of loading it from disk"))
                    .map(|save| save.game_state)
                    .unwrap_or_else(|| GameState::new(&data_store)),
            ));

            let (ui_sender, ui_recv) = channel();

            let mut game = Game::new(
                GameInitData::Client {
                    game_state: game_state.clone(),
                    action_state_machine: state_machine.clone(),
                    inputs: recv,
                    tick_counter: tick_counter.clone(),
                    info: ClientConnectionInfo {
                        ip: IpAddr::V4(Ipv4Addr::LOCALHOST),
                        port: 8080,
                    },
                    ui_actions: ui_recv,
                },
                &data_store,
            )
            .unwrap();

            let m_data_store = data_store.clone();
            thread::spawn(move || {
                game.run(&m_data_store);
            });

            let data_store = Arc::new(Mutex::new(data_store));
            return (
                LoadedGame::ItemU8RecipeU8(LoadedGameSized {
                    state: game_state,
                    state_machine,
                    data_store,
                    ui_action_sender: ui_sender,
                }),
                tick_counter,
                send,
            );
        },
        data::DataStoreOptions::ItemU8RecipeU16(data_store) => todo!(),
        data::DataStoreOptions::ItemU16RecipeU8(data_store) => todo!(),
        data::DataStoreOptions::ItemU16RecipeU16(data_store) => todo!(),
    }
}

use std::simd::Simd;

// TODO: Increase if possible
type BOOLSIMDTYPE = Simd<u8, 4>;
type SIMDTYPE = Simd<u8, 4>;

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

pub fn simd(
    locs: &mut [u8],
    inserter_info: &mut [u8],
    inserter_ouput_idx: &[usize],
    outputs: &mut [u16; 5],
) {
    const HANDSIZE: u16 = 8;

    let HAS_ITEM_TEST: SIMDTYPE = SIMDTYPE::splat(1);

    let mut local_accs = [0; 5 * SIMDTYPE::LEN];
    let LOCAL_IDX: Simd<usize, 4> =
        Simd::<usize, 4>::from_array(array::from_fn(|i| (i * local_accs.len() / SIMDTYPE::LEN)));

    assert!(locs.len() == inserter_info.len());
    assert!(locs.len() % 64 == 0);
    assert!(inserter_info.len() % 64 == 0);

    for i in (0..locs.len().min(locs.len() - (locs.len() % SIMDTYPE::LEN))).step_by(SIMDTYPE::LEN) {
        let input = BOOLSIMDTYPE::from_slice(locs.split_at(i).1);

        let input_has_items = input.simd_eq(HAS_ITEM_TEST);

        let num_items_in_hand = SIMDTYPE::from_slice(inserter_info.split_at(i).1);

        let post_items_in_hand =
            input_has_items.select(num_items_in_hand + SIMDTYPE::splat(1), num_items_in_hand);

        let full_hand_mask = post_items_in_hand.simd_eq(SIMDTYPE::splat(8));

        let final_items_in_hand = full_hand_mask.select(SIMDTYPE::splat(0), post_items_in_hand);

        final_items_in_hand.copy_to_slice(inserter_info.split_at_mut(i).1);

        let output_idx = Simd::<usize, 4>::from_slice(inserter_ouput_idx.split_at(i).1);

        let old_vals = SIMDTYPE::gather_or(&local_accs, LOCAL_IDX + output_idx, SIMDTYPE::splat(0));

        let new_vals = full_hand_mask.select(old_vals + SIMDTYPE::splat(8), old_vals);

        new_vals.scatter(&mut local_accs, LOCAL_IDX + output_idx);
    }

    for (i, local) in local_accs.iter().enumerate() {
        let real_index = i % 5;

        outputs[real_index] += u16::from(*local);
    }
}

#[cfg(test)]
mod tests {
    use std::{fs::File, iter, path::PathBuf, rc::Rc};

    use rstest::rstest;
    use test::{black_box, Bencher};

    use crate::{
        data::{factorio_1_1::get_raw_data_test, DataStore},
        frontend::{action::ActionType, world::Position},
        rendering::app_state::GameState,
        replays::{run_till_finished, Replay},
        DATA_STORE, TICKS_PER_SECOND_LOGIC,
    };

    #[bench]
    fn clone_empty_simulation(b: &mut Bencher) {
        let data_store = get_raw_data_test().process().assume_simple();

        let game_state = GameState::new(&data_store);

        let replay = Replay::new(&game_state, None, Rc::new(data_store));

        b.iter(|| replay.clone());
    }

    #[bench]
    fn empty_simulation(b: &mut Bencher) {
        // 1 hour
        const NUM_TICKS: u64 = TICKS_PER_SECOND_LOGIC * 60 * 60;

        let data_store = get_raw_data_test().process().assume_simple();

        let game_state = GameState::new(&data_store);

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

        let game_state = GameState::new(&data_store);

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

    #[rstest]
    fn crashing_replays(#[files("crash_replays/*.rep")] path: PathBuf) {
        use std::io::Read;

        // Keep running for 30 seconds
        const RUNTIME_AFTER_PRESUMED_CRASH: u64 = 30 * 60;

        let mut file = File::open(&path).unwrap();

        let mut v = Vec::with_capacity(file.metadata().unwrap().len() as usize);

        file.read_to_end(&mut v).unwrap();

        // TODO: For non u8 IdxTypes this will fail
        let mut replay: Replay<u8, u8, DataStore<u8, u8>> = bitcode::deserialize(v.as_slice())
            .expect(
                format!("Test replay {path:?} did not deserialize, consider removing it.").as_str(),
            );

        replay.finish();

        let running_replay = replay.run();

        let (mut game_state_before_crash, data_store) = running_replay.with(run_till_finished);

        for _ in 0..RUNTIME_AFTER_PRESUMED_CRASH {
            game_state_before_crash.update(&data_store);
        }
    }

    #[bench]
    fn bench_huge_red_green_sci(b: &mut Bencher) {
        let game_state = GameState::new_with_beacon_red_green_production_many_grids(
            Default::default(),
            &DATA_STORE,
        );

        let mut game_state = game_state.clone();

        b.iter(|| {
            game_state.update(&DATA_STORE);
        })
    }

    #[bench]
    fn bench_12_beacon_red(b: &mut Bencher) {
        let game_state =
            GameState::new_with_beacon_belt_production(Default::default(), &DATA_STORE);

        let mut game_state = game_state.clone();

        b.iter(|| {
            game_state.update(&DATA_STORE);
        })
    }
}
