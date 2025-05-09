#![feature(test)]
#![feature(portable_simd)]
#![feature(adt_const_params)]
#![feature(array_try_map)]
#![feature(never_type)]

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
        Arc, Mutex,
    },
    thread,
};

use data::{get_raw_data_test, DataStore};
use eframe::NativeOptions;
use frontend::{
    action::action_state_machine::ActionStateMachine, input::Input, world::tile::CHUNK_SIZE_FLOAT,
};
use item::{IdxTrait, WeakIdxTrait};
use multiplayer::{
    connection_reciever::accept_continously, ClientConnectionInfo, Game, GameInitData, ServerInfo,
};
use rendering::{
    app_state::{AppState, GameState},
    eframe_app,
    window::{App, LoadedGame, LoadedGameInfo, LoadedGameSized},
};
use saving::load;
use simple_logger::SimpleLogger;
use winit::event_loop::EventLoop;

const TICKS_PER_SECOND_LOGIC: u64 = 60;

const TICKS_PER_SECOND_RUNSPEED: u64 = 60;

pub mod assembler;
pub mod belt;
pub mod inserter;
pub mod item;
pub mod lab;
pub mod power;
pub mod research;

pub mod data;
pub mod mod_manager;

pub mod frontend;

pub mod rendering;

pub mod bot_system;

mod statistics;

mod replays;

mod saving;

mod multiplayer;

mod storage_list;

pub mod split_arbitrary;

mod chest;

pub mod blueprint;

mod network_graph;

impl WeakIdxTrait for u8 {}
impl WeakIdxTrait for u16 {}
impl IdxTrait for u8 {}
impl IdxTrait for u16 {}

#[cfg(test)]
static DATA_STORE: std::sync::LazyLock<DataStore<u8, u8>> =
    std::sync::LazyLock::new(|| get_raw_data_test().turn::<u8, u8>());

pub trait NewWithDataStore {
    fn new<ItemIdxType: IdxTrait, RecipeIdxType: IdxTrait>(
        data_store: impl Borrow<DataStore<ItemIdxType, RecipeIdxType>>,
    ) -> Self;
}

impl NewWithDataStore for u32 {
    fn new<ItemIdxType: IdxTrait, RecipeIdxType: IdxTrait>(
        _data_store: impl Borrow<DataStore<ItemIdxType, RecipeIdxType>>,
    ) -> Self {
        0
    }
}

pub fn main() {
    SimpleLogger::new()
        .with_level(log::LevelFilter::Warn)
        .init()
        .unwrap();

    let mode = env::args().nth(1);

    dbg!(&mode);

    let (loaded, tick, sender) = if Some("--client") == mode.as_deref() {
        run_client(StartGameInfo {})
    } else if Some("--dedicated") == mode.as_deref() {
        run_dedicated_server(StartGameInfo {});
    } else {
        run_integrated_server(StartGameInfo {})
    };
    // let mut app = App::new(sender);

    // app.currently_loaded_game = Some(LoadedGameInfo {
    //     state: loaded,
    //     tick: tick,
    // });
    // app.state = AppState::Ingame;
    // let event_loop = EventLoop::new().unwrap();

    // event_loop.set_control_flow(winit::event_loop::ControlFlow::Poll);

    // let _ = event_loop.run_app(&mut app);

    eframe::run_native(
        "FactoryGame",
        NativeOptions::default(),
        Box::new(|cc| {
            let mut app = eframe_app::App::new(cc, sender);

            app.state = AppState::Ingame;
            app.currently_loaded_game = Some(LoadedGameInfo {
                state: loaded,
                tick,
            });

            Ok(Box::new(app))
        }),
    )
    .unwrap();
}

struct StartGameInfo {}

fn run_integrated_server(
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
            let data_store = Arc::new(data_store);

            let (send, recv) = channel();
            let state_machine: Arc<Mutex<ActionStateMachine<_, _>>> = Arc::new(Mutex::new(
                ActionStateMachine::new(0, (100.0 * CHUNK_SIZE_FLOAT, 100.0 * CHUNK_SIZE_FLOAT)),
            ));

            let game_state = Arc::new(Mutex::new(
                load()
                    .map(|save| save.game_state)
                    .unwrap_or_else(|| GameState::new(&data_store)),
            ));

            let (ui_sender, ui_recv) = channel();

            let mut game = Game::new(GameInitData::IntegratedServer {
                game_state: game_state.clone(),
                tick_counter: tick_counter.clone(),
                info: ServerInfo { connections },
                action_state_machine: state_machine.clone(),
                inputs: recv,
                ui_actions: ui_recv,
            })
            .unwrap();

            let m_data_store = data_store.clone();
            thread::spawn(move || {
                game.run(&m_data_store);
            });

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

    let connections: Arc<Mutex<Vec<std::net::TcpStream>>> = Arc::default();

    accept_continously(connections.clone()).unwrap();

    match data_store {
        data::DataStoreOptions::ItemU8RecipeU8(data_store) => {
            let game_state = load()
                .map(|save| save.game_state)
                .unwrap_or_else(|| GameState::new(&data_store));

            let mut game = Game::new(GameInitData::DedicatedServer(
                game_state,
                ServerInfo { connections },
            ))
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
            let data_store = Arc::new(data_store);

            let (send, recv) = channel();
            let state_machine: Arc<Mutex<ActionStateMachine<_, _>>> = Arc::new(Mutex::new(
                ActionStateMachine::new(1, (100.0 * CHUNK_SIZE_FLOAT, 100.0 * CHUNK_SIZE_FLOAT)),
            ));

            let game_state = Arc::new(Mutex::new(
                load()
                    .map(|save| save.game_state)
                    .unwrap_or_else(|| GameState::new(&data_store)),
            ));

            let (ui_sender, ui_recv) = channel();

            let mut game = Game::new(GameInitData::Client {
                game_state: game_state.clone(),
                action_state_machine: state_machine.clone(),
                inputs: recv,
                tick_counter: tick_counter.clone(),
                info: ClientConnectionInfo {
                    ip: IpAddr::V4(Ipv4Addr::LOCALHOST),
                    port: 8080,
                },
                ui_actions: ui_recv,
            })
            .unwrap();

            let m_data_store = data_store.clone();
            thread::spawn(move || {
                game.run(&m_data_store);
            });

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

// fn main_loop<ItemIdxType: IdxTrait, RecipeIdxType: IdxTrait>(
//     current_tick: Arc<AtomicU64>,
//     input_reciever: Receiver<Input>,
//     app_state: Arc<Mutex<AppState<ItemIdxType, RecipeIdxType>>>,
//     state_machine: Arc<Mutex<ActionStateMachine<ItemIdxType>>>,
//     data_store: Arc<DataStore<ItemIdxType, RecipeIdxType>>,
// ) -> ! {
//     let mut update_interval =
//         spin_sleep_util::interval(Duration::from_secs(1) / TICKS_PER_SECOND as u32);

//     loop {
//         update_interval.tick();
//         match &mut *app_state.lock().unwrap() {
//             AppState::Ingame(game_state) => {
//                 // TODO: For now I collect the actions here.

//                 let actions: Vec<ActionType<ItemIdxType, RecipeIdxType>> = {
//                     let mut state_machine = state_machine.lock().unwrap();
//                     let mut ret: Vec<ActionType<ItemIdxType, RecipeIdxType>> = input_reciever
//                         .try_iter()
//                         .flat_map(|input| {
//                             state_machine.handle_input(input, &game_state.world, &data_store)
//                         })
//                         .collect();

//                     ret.extend(state_machine.once_per_update_actions());

//                     ret
//                 };

//                 let start = Instant::now();
//                 game_state.apply_actions(actions, &data_store);

//                 info!("Apply Actions Time: {:?}", start.elapsed());
//                 let start = Instant::now();

//                 game_state.update(&data_store);
//                 info!("Update Time: {:?}", start.elapsed());
//             },
//         }
//         current_tick.fetch_add(1, std::sync::atomic::Ordering::Relaxed);
//     }
// }

// #[cfg(not(debug_assertions))]

// Type your code here, or load an example.

use std::simd::Simd;

// TODO: Increase if possible
type BOOLSIMDTYPE = Simd<u8, 4>;
type SIMDTYPE = Simd<u8, 4>;

// As of Rust 1.75, small functions are automatically
// marked as `#[inline]` so they will not show up in
// the output when compiling with optimisations. Use
// `#[no_mangle]` or `#[inline(never)]` to work around
// this issue.
// See https://github.com/compiler-explorer/compiler-explorer/issues/5939
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

// #[cfg(test)]
// mod tests {
//     use std::{iter, rc::Rc};

//     use test::{black_box, Bencher};

//     use crate::{
//         data::get_raw_data_test,
//         frontend::{action::ActionType, world::tile::World},
//         rendering::app_state::{GameState, SimulationState},
//         replays::{run_till_finished, Replay},
//         TICKS_PER_SECOND_LOGIC,
//     };

//     #[bench]
//     fn clone_empty_simulation(b: &mut Bencher) {
//         let data_store = get_raw_data_test().process().assume_simple();

//         let game_state = GameState::new(&data_store);

//         let replay = Replay::new(game_state, Rc::new(data_store));

//         b.iter(|| replay.clone());
//     }

//     #[bench]
//     fn empty_simulation(b: &mut Bencher) {
//         // 1 hour
//         const NUM_TICKS: u64 = TICKS_PER_SECOND_LOGIC * 60 * 60;

//         let data_store = get_raw_data_test().process().assume_simple();

//         let game_state = GameState::new(&data_store);

//         let mut replay = Replay::new(game_state, Rc::new(data_store));

//         for _ in 0..NUM_TICKS {
//             replay.tick();
//         }

//         replay.finish();

//         b.iter(|| black_box(replay.clone().run().with(run_till_finished)));
//     }

//     #[bench]
//     fn noop_actions_simulation(b: &mut Bencher) {
//         // 1 hour
//         const NUM_TICKS: u64 = TICKS_PER_SECOND_LOGIC * 60 * 60;

//         let data_store = get_raw_data_test().process().assume_simple();

//         let game_state = GameState::new(&data_store);

//         let mut replay = Replay::new(game_state, Rc::new(data_store));

//         for _ in 0..NUM_TICKS {
//             replay.append_actions(iter::repeat(ActionType::Ping((100, 100))).take(5));
//             replay.tick();
//         }

//         replay.finish();

//         b.iter(|| replay.clone().run().with(run_till_finished));
//     }
// }
