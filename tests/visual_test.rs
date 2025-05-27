use rstest::rstest;
use std::path::PathBuf;
use std::thread::sleep;
use std::thread::spawn;
use std::time::Duration;

use factory::data::DataStore;
use factory::frontend::action::action_state_machine::ActionStateMachine;
use factory::rendering::app_state::GameState;
use factory::rendering::window::LoadedGame;
use factory::rendering::window::LoadedGameInfo;
use factory::replays::Replay;

#[rstest]
fn crashing_replays_visual(#[files("crash_replays/*.rep")] path: PathBuf) {
    use std::{
        fs::File,
        io::Read,
        path::PathBuf,
        sync::{atomic::AtomicU64, mpsc::channel, Arc, Mutex},
    };

    use eframe::EventLoopBuilderHook;
    use winit::platform::wayland::EventLoopBuilderExtWayland;

    use factory::rendering::{eframe_app, window::LoadedGameSized};
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

    let gs = Arc::new(Mutex::new(GameState::new(&replay.data_store)));
    let ds = Arc::new(replay.data_store.clone());

    let gs_move = gs.clone();
    let ds_move = ds.clone();

    let (ctx_send, ctx_recv) = channel();

    let t = spawn(move || {
        let (send, recv) = channel();
        let sm = Arc::new(Mutex::new(ActionStateMachine::new(0, (1600.0, 1600.0))));

        let sm_move = sm.clone();
        let gs_move = gs.clone();
        let ds_move = ds.clone();
        spawn(move || loop {
            {
                let gs = gs_move.lock().unwrap();
                for action in sm_move
                    .lock()
                    .unwrap()
                    .handle_inputs(&recv, &gs.world, &ds_move)
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
            format!("FactoryGame Test {path:?}").as_str(),
            native_options,
            Box::new(move |cc| {
                let mut app = eframe_app::App::new(cc, send);

                ctx_send.send(cc.egui_ctx.clone()).unwrap();

                let (send, _recv) = channel();

                app.currently_loaded_game = Some(LoadedGameInfo {
                    state: LoadedGame::ItemU8RecipeU8(LoadedGameSized {
                        state: gs,
                        state_machine: sm,
                        data_store: ds,
                        ui_action_sender: send,
                    }),
                    tick: Arc::new(AtomicU64::new(0)),
                });

                Ok(Box::new(app))
            }),
        )
        .expect("failed to run app");
    });

    replay.run_with(gs_move.clone(), || {
        sleep(Duration::from_millis(1));
    });

    for _ in 0..RUNTIME_AFTER_PRESUMED_CRASH {
        gs_move.lock().unwrap().update(&ds_move);
    }

    ctx_recv
        .recv()
        .unwrap()
        .send_viewport_cmd(egui::ViewportCommand::Close);

    t.join().expect("Failed to join");
}
