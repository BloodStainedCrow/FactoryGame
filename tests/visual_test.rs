use eframe::EventLoopBuilderHook;
use factory::rendering::app_state::AppState;
use factory::rendering::eframe_app;
use factory::rendering::window::LoadedGameSized;

use factory::DATA_STORE;
use parking_lot::Mutex;
use rstest::fixture;
use rstest::rstest;
use std::path::PathBuf;
use std::sync::atomic::AtomicU64;
use std::sync::mpsc::channel;
use std::sync::Arc;
use std::thread::sleep;
use std::thread::spawn;
use std::time::Duration;
use winit::platform::wayland::EventLoopBuilderExtWayland;

use factory::data::DataStore;
use factory::frontend::action::action_state_machine::ActionStateMachine;
use factory::rendering::app_state::GameState;
use factory::rendering::window::LoadedGame;
use factory::rendering::window::LoadedGameInfo;
use factory::replays::Replay;

use egui::Context;

#[fixture]
#[once]
fn start_ui() -> (
    Mutex<Context>,
    Arc<Mutex<DataStore<u8, u8>>>,
    Arc<Mutex<GameState<u8, u8>>>,
) {
    let (ctx_send, ctx_recv) = channel();

    let ds = Arc::new(Mutex::new(DATA_STORE.clone()));
    let gs = Arc::new(Mutex::new(GameState::new(&DATA_STORE)));

    let gs_move = gs.clone();
    let ds_move = ds.clone();
    spawn(move || {
        let (send, recv) = channel();
        let sm = Arc::new(Mutex::new(ActionStateMachine::new(
            0,
            (1600.0, 1600.0),
            &DATA_STORE,
        )));

        let sm_move = sm.clone();
        let gs_move_move = gs_move.clone();
        let ds_move_move = ds_move.clone();
        spawn(move || loop {
            {
                let gs = gs_move_move.lock();
                for action in sm_move
                    .lock()
                    .handle_inputs(&recv, &gs.world, &ds_move_move.lock())
                {
                    dbg!(action);
                }
            }
            sleep(Duration::from_millis(16));
        });

        let event_loop_builder: Option<EventLoopBuilderHook> =
            Some(Box::new(|event_loop_builder| {
                event_loop_builder.with_any_thread(true);
            }));
        let native_options: eframe::NativeOptions = eframe::NativeOptions {
            event_loop_builder,

            ..Default::default()
        };

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
                        data_store: ds_move,
                        ui_action_sender: send,
                        stop_update_thread: Default::default(),
                    }),
                    tick: Arc::new(AtomicU64::new(0)),
                });

                let (send, _recv) = channel();

                app.input_sender = Some(send);
                app.state = AppState::Ingame;

                Ok(Box::new(app))
            }),
        )
        .expect("failed to run app");
    });

    let ctx_lock = Mutex::new(ctx_recv.recv().unwrap());

    // FIXME: When the last test finishes we SEGV?!?
    (ctx_lock, ds, gs)
}

#[rstest]
fn crashing_replays_visual(
    #[files("crash_replays/*.rep")] path: PathBuf,
    start_ui: &(
        Mutex<Context>,
        Arc<Mutex<DataStore<u8, u8>>>,
        Arc<Mutex<GameState<u8, u8>>>,
    ),
) {
    use std::{fs::File, io::Read};

    let _im_running = start_ui.0.lock();
    let gs = start_ui.2.clone();

    // Keep running for 30 seconds
    const RUNTIME_AFTER_PRESUMED_CRASH: u64 = 30 * 60;

    let mut file = File::open(&path).unwrap();

    let mut v = Vec::with_capacity(file.metadata().unwrap().len() as usize);

    file.read_to_end(&mut v).unwrap();

    // TODO: For non u8 IdxTypes this will fail
    let mut replay: Replay<u8, u8, DataStore<u8, u8>> = bitcode::deserialize(v.as_slice()).expect(
        format!("Test replay {path:?} did not deserialize, consider removing it.").as_str(),
    );
    replay.finish();

    *start_ui.1.lock() = replay.data_store.clone();

    let gs_move = gs.clone();
    let ds_move = start_ui.1.clone();

    replay.run_with(gs_move.clone(), || {
        sleep(Duration::from_millis(1));
    });

    for _ in 0..RUNTIME_AFTER_PRESUMED_CRASH {
        gs_move.lock().update(&ds_move.lock());
    }
}
