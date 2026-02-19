use eframe::EventLoopBuilderHook;
use factory::app_state::AppState;
use factory::rendering::eframe_app;
use factory::rendering::window::LoadedGameSized;

use factory::DATA_STORE;
use factory::replays::GenerationInformation;
use parking_lot::Mutex;
use rstest::fixture;
use rstest::rstest;
use std::path::PathBuf;
use std::sync::Arc;
use std::sync::atomic::AtomicU64;
use std::sync::mpsc::channel;
use std::thread::sleep;
use std::thread::spawn;
use std::time::Duration;
use winit::platform::wayland::EventLoopBuilderExtWayland;

use factory::app_state::GameState;
use factory::frontend::action::action_state_machine::ActionStateMachine;
use factory::rendering::window::LoadedGame;
use factory::rendering::window::LoadedGameInfo;
use factory::replays::Replay;

use egui::Context;

#[fixture]
#[once]
fn start_ui() -> (Mutex<Context>, Arc<GameState<u8, u8>>) {
    simple_logger::SimpleLogger::new()
        .with_level(log::LevelFilter::Error)
        .env()
        .init()
        .unwrap();
    let (ctx_send, ctx_recv) = channel();

    let gs = Arc::new(GameState::new(
        "TEST_GAMESTATE".to_string(),
        GenerationInformation::default(),
        &DATA_STORE,
    ));

    let gs_move = gs.clone();
    spawn(move || {
        let (send_input, recv) = channel();
        let sm = {
            let sim_state = gs_move.simulation_state.lock();
            Arc::new(Mutex::new(ActionStateMachine::new_from_gamestate(
                0,
                &*gs_move.world.lock(),
                &*sim_state,
                &DATA_STORE,
            )))
        };

        let sm_move = sm.clone();
        let gs_move_move = gs_move.clone();
        spawn(move || {
            loop {
                {
                    let sim_state = gs_move_move.simulation_state.lock();
                    let world = gs_move_move.world.lock();
                    for _action in sm_move.lock().handle_inputs(
                        recv.try_iter(),
                        &*world,
                        &*sim_state,
                        &DATA_STORE,
                    ) {
                        // dbg!(_action);
                    }
                    for _action in sm_move.lock().once_per_update_actions(&*world, &DATA_STORE) {
                        // dbg!(_action);
                    }
                }
                sleep(Duration::from_millis(16));
            }
        });

        let event_loop_builder: Option<EventLoopBuilderHook> =
            Some(Box::new(|event_loop_builder| {
                event_loop_builder.with_any_thread(true);
            }));
        let native_options: eframe::NativeOptions = eframe::NativeOptions {
            event_loop_builder,

            ..Default::default()
        };

        eprintln!("eframe::run_native");
        eframe::run_native(
            format!("FactoryGame Test Runner").as_str(),
            native_options,
            Box::new(move |cc| {
                let mut app = eframe_app::App::new(cc);

                ctx_send.send(cc.egui_ctx.clone()).unwrap();

                let (send, _recv) = channel();

                app.currently_loaded_game = Some(LoadedGameInfo {
                    state: LoadedGame::ItemU8RecipeU8(LoadedGameSized {
                        state: gs_move,
                        state_machine: sm,
                        // FIXME: This test was the only reason this was in a mutex and now I no longer use it
                        data_store: Arc::new(Mutex::new(DATA_STORE.clone())),
                        ui_action_sender: send,
                        stop_update_thread: Default::default(),
                    }),
                    tick: Arc::new(AtomicU64::new(0)),
                });

                app.input_sender = Some(send_input);
                app.state = AppState::Ingame;

                Ok(Box::new(app))
            }),
        )
        .expect("failed to run app");
        eprintln!("eframe::run_native returned??");
    });

    let ctx_lock = Mutex::new(ctx_recv.recv().unwrap());

    // FIXME: When the last test finishes we SEGV?!?
    (ctx_lock, gs)
}

#[rstest]
fn crashing_replays_visual(
    #[files("crash_replays/*.rep.ron")] path: PathBuf,
    start_ui: &(Mutex<Context>, Arc<GameState<u8, u8>>),
) {
    use std::{fs::File, io::Read};

    let _im_running = start_ui.0.lock();
    eprintln!("{path:?}");

    // Keep running for 30 seconds
    const RUNTIME_AFTER_PRESUMED_CRASH: u64 = 30 * 60;

    let mut file = File::open(&path).unwrap();

    let mut v = Vec::with_capacity(file.metadata().unwrap().len() as usize);

    file.read_to_end(&mut v).unwrap();
    let str = String::try_from(v).expect("File content not UTF-8");

    let mut replay: Replay = ron::de::from_str(&str).expect(
        format!("Test replay {path:?} did not deserialize, consider removing it.").as_str(),
    );
    replay.finish();

    let game_state = replay
        .run(
            start_ui.1.clone(),
            |_current_gs| {
                // sleep(Duration::from_millis(1));
            },
            &DATA_STORE,
        )
        .expect("Replay ran into an error");

    for _ in 0..RUNTIME_AFTER_PRESUMED_CRASH {
        GameState::update(
            &mut *game_state.simulation_state.lock(),
            &mut *game_state.aux_data.lock(),
            &DATA_STORE,
        );
    }
}
