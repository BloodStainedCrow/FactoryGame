#![feature(test)]
#![feature(portable_simd)]
#![feature(adt_const_params)]

use std::{
    array,
    fs::{create_dir_all, File},
    io::Write,
    simd::cmp::SimdPartialEq,
    sync::{
        atomic::AtomicU64,
        mpsc::{channel, Receiver},
        Arc, Mutex,
    },
    thread,
    time::{Duration, Instant},
};

use directories::ProjectDirs;
use frontend::{
    action::{action_state_machine::ActionStateMachine, ActionType},
    input::Input,
};
use log::{error, info};
use rendering::{
    app_state::{AppState, GameState},
    window::App,
};
use simple_logger::SimpleLogger;
use winit::event_loop::EventLoop;

const TICKS_PER_SECOND: u64 = 60;

pub mod assembler;
pub mod belt;
pub mod inserter;
pub mod item;
pub mod lab;
pub mod power;
pub mod producer;
pub mod research;

pub mod frontend;

mod rendering;

pub fn main() {
    SimpleLogger::new()
        .with_level(log::LevelFilter::Info)
        .init()
        .unwrap();

    let (send, recv) = channel();

    let mut app = App::new(send);

    let tick = app.current_tick.clone();
    let state = app.state.clone();
    let state_machine = app.state_machine.clone();
    thread::spawn(|| {
        main_loop(tick, recv, state, state_machine);
    });

    let event_loop = EventLoop::new().unwrap();

    event_loop.set_control_flow(winit::event_loop::ControlFlow::Poll);

    let _ = event_loop.run_app(&mut app);
}

fn main_loop(
    current_tick: Arc<AtomicU64>,
    input_reciever: Receiver<Input>,
    app_state: Arc<Mutex<AppState>>,
    state_machine: Arc<Mutex<ActionStateMachine>>,
) -> ! {
    let mut update_interval = spin_sleep_util::interval(Duration::from_secs(1) / 60);

    loop {
        update_interval.tick();
        let mut state = app_state.lock().unwrap();
        match &mut *state {
            AppState::Ingame(game_state) => {
                // TODO: For now I collect the actions here.

                let actions: Vec<ActionType> = {
                    let mut state_machine = state_machine.lock().unwrap();
                    let mut ret: Vec<ActionType> = input_reciever
                        .try_iter()
                        .flat_map(|input| state_machine.handle_input(input, &game_state.world))
                        .collect();

                    ret.extend(state_machine.once_per_update_actions());

                    ret
                };

                dbg!(actions.len());

                let start = Instant::now();
                game_state.apply_actions(actions);

                info!("Apply Actions Time: {:?}", start.elapsed());
                let start = Instant::now();

                game_state.update();
                info!("Update Time: {:?}", start.elapsed());
            },
        }
        current_tick.fetch_add(1, std::sync::atomic::Ordering::Relaxed);
    }
}

/// # Panics
/// If File system stuff fails
pub fn save(game_state: &GameState) {
    let dir = ProjectDirs::from("de", "aschhoff", "factory_game").expect("No Home path found");

    create_dir_all(dir.data_dir()).expect("Could not create data dir");

    let save_file_dir = dir.data_dir().join("save.save");

    let mut file = File::create(save_file_dir).expect("Could not open file");

    file.write_all(
        ron::ser::to_string_pretty(game_state, ron::ser::PrettyConfig::default())
            .unwrap()
            .as_bytes(),
    )
    .expect("Could not write to file");
}

/// # Panics
/// If File system stuff fails
#[must_use]
pub fn load() -> Option<GameState> {
    let dir = ProjectDirs::from("de", "aschhoff", "factory_game").expect("No Home path found");

    let save_file_dir = dir.data_dir().join("save.save");

    let file = File::open(save_file_dir);

    file.map_or(None, |file| match ron::de::from_reader(file) {
        Ok(val) => Some(val),
        Err(err) => {
            error!("Found save, but was unable to deserialize it!!!! \n{}", err);
            None
        },
    })
}

// #[cfg(not(debug_assertions))]
#[cfg(test)]
mod tests {
    use rand::random;
    use test::Bencher;

    use crate::{
        assembler::MultiAssemblerStoreOne,
        belt::{belt::Belt, smart::SmartBelt},
        inserter::StorageID,
        item::{Iron, IronOre, ItemStorage},
    };
    extern crate test;

    // multi_stores is a placeholder for assemblers for all recipes
    const NUM_RECIPES: usize = 250;
    const NUM_ASSEMBLERS_PER_RECIPE: usize = 10_000;

    const NUM_BELTS: usize = NUM_RECIPES * 40;
    const BELT_LEN: usize = NUM_ASSEMBLERS_PER_RECIPE * 4;
    const NUM_INSERTERS_PER_BELT: usize = NUM_ASSEMBLERS_PER_RECIPE / 10;

    #[bench]
    fn bench_factory_single_threaded(b: &mut Bencher) {
        // Do a single update "step"
        let mut multi_stores = vec![];

        for _ in 0..NUM_RECIPES {
            multi_stores.push(MultiAssemblerStoreOne::<Iron>::new());
        }

        for store in &mut multi_stores {
            for i in 0..NUM_ASSEMBLERS_PER_RECIPE {
                store.add_assembler();
                *store.get_input_1_mut(i).expect("Hardcoded") =
                    ItemStorage::<IronOre>::new(random());
            }
        }

        let mut belts: Vec<SmartBelt<Iron>> = vec![];

        for _ in 0..NUM_BELTS {
            let mut belt = SmartBelt::<Iron>::new(BELT_LEN);

            for i in 0..NUM_INSERTERS_PER_BELT {
                belt.add_out_inserter(
                    i.try_into().unwrap(),
                    StorageID {
                        recipe: 0,
                        grid: 0,
                        storage: random::<u8>() as u16,
                    },
                );
            }

            belts.push(belt);
        }

        b.iter(|| {
            for store in &mut multi_stores {
                store.update_branchless(64);
            }

            for belt in &mut belts {
                let _ = belt.try_insert_item(belt.get_len() - 1);
                let _ = belt.try_insert_item(belt.get_len() / 2);
                let _ = belt.try_insert_item(belt.get_len() / 4);

                belt.update_inserters(&mut [&mut [multi_stores[0].get_outputs_mut()]]);

                belt.update();
            }
        });
    }

    #[bench]
    fn bench_factory_single_threaded_assembler(b: &mut Bencher) {
        // Do a single update "step"
        let mut multi_stores = vec![];

        for _ in 0..NUM_RECIPES {
            multi_stores.push(MultiAssemblerStoreOne::<Iron>::new());
        }

        for store in &mut multi_stores {
            for i in 0..NUM_ASSEMBLERS_PER_RECIPE {
                store.add_assembler();
                *store.get_input_1_mut(i).expect("Hardcoded") =
                    ItemStorage::<IronOre>::new(random());
            }
        }

        #[allow(clippy::collection_is_never_read)]
        let mut belts: Vec<SmartBelt<Iron>> = vec![];

        for _ in 0..NUM_BELTS {
            let mut belt = SmartBelt::<Iron>::new(BELT_LEN);

            for i in 0..NUM_INSERTERS_PER_BELT {
                belt.add_out_inserter(
                    i.try_into().unwrap(),
                    StorageID {
                        recipe: 0,
                        grid: 0,
                        storage: random::<u8>() as u16,
                    },
                );
            }

            belts.push(belt);
        }

        b.iter(|| {
            for store in &mut multi_stores {
                store.update_branchless(64);
            }
        });
    }

    #[bench]
    fn bench_factory_single_threaded_belt(b: &mut Bencher) {
        // Do a single update "step"
        let mut multi_stores = vec![];

        for _ in 0..NUM_RECIPES {
            multi_stores.push(MultiAssemblerStoreOne::<Iron>::new());
        }

        for store in &mut multi_stores {
            for i in 0..NUM_ASSEMBLERS_PER_RECIPE {
                store.add_assembler();
                *store.get_input_1_mut(i).expect("Hardcoded") =
                    ItemStorage::<IronOre>::new(random());
            }
        }

        let mut belts: Vec<SmartBelt<Iron>> = vec![];

        for _ in 0..NUM_BELTS {
            let mut belt = SmartBelt::<Iron>::new(BELT_LEN);

            for i in 0..NUM_INSERTERS_PER_BELT {
                belt.add_out_inserter(
                    i.try_into().unwrap(),
                    StorageID {
                        recipe: 0,
                        grid: 0,
                        storage: random::<u8>() as u16,
                    },
                );
            }

            belts.push(belt);
        }

        let mut i = 0;

        b.iter(|| {
            for belt in &mut belts {
                let _ = belt.try_insert_item(belt.get_len() - 1);
                let _ = belt.try_insert_item(belt.get_len() / 2);
                let _ = belt.try_insert_item(belt.get_len() / 4);
                belt.update();
            }
            i += 1;
        });
    }

    #[bench]
    fn bench_factory_single_threaded_inserter(b: &mut Bencher) {
        // Do a single update "step"
        let mut multi_stores = vec![];

        for _ in 0..NUM_RECIPES {
            multi_stores.push(MultiAssemblerStoreOne::<Iron>::new());
        }

        for store in &mut multi_stores {
            for i in 0..NUM_ASSEMBLERS_PER_RECIPE {
                store.add_assembler();
                *store.get_input_1_mut(i).expect("Hardcoded") =
                    ItemStorage::<IronOre>::new(random());
            }
        }

        let mut belts: Vec<SmartBelt<Iron>> = vec![];

        for _ in 0..NUM_BELTS {
            let mut belt = SmartBelt::<Iron>::new(BELT_LEN);

            for i in 0..NUM_INSERTERS_PER_BELT {
                belt.add_out_inserter(
                    i.try_into().unwrap(),
                    StorageID {
                        recipe: 0,
                        grid: 0,
                        storage: random::<u8>() as u16,
                    },
                );
            }

            belts.push(belt);
        }

        b.iter(|| {
            for belt in &mut belts {
                let _ = belt.try_insert_item(belt.get_len() - 1);
                let _ = belt.try_insert_item(belt.get_len() / 2);
                let _ = belt.try_insert_item(belt.get_len() / 4);
                belt.update_inserters(&mut [&mut [multi_stores[0].get_outputs_mut()]]);
            }
        });
    }
}

// Type your code here, or load an example.

use std::simd::Mask;
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

// If you use `main()`, declare it as `pub` to see it in the output:
// pub fn main() { ... }
