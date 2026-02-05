use std::{
    fs::remove_dir_all,
    net::ToSocketAddrs,
    sync::{
        Arc,
        atomic::{AtomicU64, Ordering},
        mpsc::{Sender, channel},
    },
    thread,
    time::Duration,
};

use chrono::Local;
use egui_extras::{Column, TableBuilder};
use egui_graphs::LayoutStateTree;
use url::Url;
use wasm_timer::Instant;

use parking_lot::Mutex;

use crate::{
    GameCreationInfo, example_worlds, run_client,
    saving::{load, loading::SaveFileList, save_folder},
};
use crate::{StartGameInfo, frontend::world::Position};
use crate::{rendering::render_world::EscapeMenuOptions, run_integrated_server};
use eframe::{
    egui::{CentralPanel, Event, PaintCallbackInfo, Shape},
    egui_wgpu::{self, CallbackTrait},
};
use egui::{
    Align2, Button, Color32, CursorIcon, Grid, Modal, ProgressBar, RichText, Slider, TextBuffer,
    TextEdit, Window,
};
use log::{error, warn};
use tilelib::types::RawRenderer;

use crate::{
    data::DataStore,
    frontend::{action::action_state_machine::ActionStateMachine, input::Input},
    item::{IdxTrait, WeakIdxTrait},
};

use super::{
    TextureAtlas,
    render_world::{render_ui, render_world},
    texture_atlas,
    window::{LoadedGame, LoadedGameInfo},
};
use crate::app_state::{AppState, GameState};

use crate::saving::save;

pub struct App {
    raw_renderer: Option<RawRenderer>,
    pub(crate) state: AppState,
    pub currently_loaded_game: Option<LoadedGameInfo>,

    last_rendered_update: u64,

    world_creation_state: example_worlds::WorldValueStore,

    pub input_sender: Option<Sender<Input>>,

    texture_atlas: Arc<TextureAtlas>,
}

impl App {
    #[must_use]
    pub fn new(cc: &eframe::CreationContext) -> Self {
        let render_state = cc.wgpu_render_state.as_ref();
        let atlas = Arc::new(texture_atlas());

        Self {
            raw_renderer: render_state.map(|render_state| {
                RawRenderer::new(
                    &render_state.device,
                    &render_state.queue,
                    render_state.target_format,
                )
            }),
            input_sender: None,
            state: AppState::MainMenu { in_ip_box: None },
            world_creation_state: Default::default(),
            texture_atlas: atlas,
            currently_loaded_game: None,
            last_rendered_update: 0,
        }
    }
}

impl eframe::App for App {
    fn update(&mut self, ctx: &eframe::egui::Context, frame: &mut eframe::Frame) {
        if self.raw_renderer.is_none() {
            if let Some(render_state) = frame.wgpu_render_state() {
                self.raw_renderer = Some(RawRenderer::new(
                    &render_state.device,
                    &render_state.queue,
                    render_state.target_format,
                ));
                warn!("Was finally able to set a renderer.");
            } else {
                error!("Unable to set up renderer!!!!!")
            }
        }

        match &self.state {
            AppState::MainMenu { .. } => {},
            AppState::LoadSaveMenu { .. } => {},
            AppState::NewGameMenu { .. } => {},
            AppState::Loading { .. } => {
                ctx.request_repaint_after(Duration::from_secs_f32(1.0 / 60.0));
            },
            AppState::Ingame => {
                // Only request a repaint when we are ingame (otherwise depending on the os we might render like 1k FPS for no reason)
                ctx.request_repaint();
            },
        }

        match &mut self.state {
            AppState::Ingame => {},
            _ => {
                if let Some(raw_renderer) = self.raw_renderer.as_mut() {
                    // Reset the runtime textures (i.e. map textures), since the user might have come from a game and have some old data floating around on the GPU
                    raw_renderer.reset_runtime_textures();
                } else {
                    warn!("Tried to reset_runtime_textures, but we are missing a renderer!")
                }
            },
        }

        ctx.set_cursor_icon(egui::CursorIcon::Default);

        match &mut self.state {
            AppState::Ingame => {
                self.update_ingame(ctx, frame);
            },

            AppState::LoadSaveMenu { save_files } => {
                let mut new_state = None;
                egui::CentralPanel::default().show(ctx, |ui| {
                    if ui.button("Back to Main Menu").clicked() {
                        new_state = Some(AppState::MainMenu { in_ip_box: None });
                    }

                    if ui
                        .add_enabled(
                            cfg!(not(target_arch = "wasm32")),
                            Button::new("Open Save File Folder"),
                        )
                        .clicked()
                    {
                        #[cfg(not(target_arch = "wasm32"))]
                        {
                            let uri =
                                Url::from_file_path(save_folder()).expect("Could not generate URI");
                            open::that(uri.as_str()).expect("Failed to Open Folder");
                        }
                    }

                    let mut dirty = false;

                    TableBuilder::new(ui)
                        .columns(Column::remainder(), 5)
                        .header(1.0, |mut header| {
                            header.col(|ui| {
                                ui.label("Save File Name");
                            });
                            header.col(|ui| {
                                ui.label("Saved At");
                            });
                            header.col(|ui| {
                                ui.label("Playtime");
                            });
                            header.col(|ui| {
                                ui.label("");
                            });
                        })
                        .body(|ui| {
                            ui.rows(1.0, save_files.save_files.len(), |mut row| {
                                let idx = row.index();
                                let file = &save_files.save_files[idx];

                                match file {
                                    Ok(file) => {
                                        row.col(|ui| {
                                            ui.label(&file.stored.name);
                                        });
                                        row.col(|ui| {
                                            ui.label(format!(
                                                "{}",
                                                file.stored
                                                    .saved_at
                                                    .with_timezone(&Local)
                                                    .format("%v %R")
                                            ));
                                        });
                                        row.col(|ui| {
                                            let dur = chrono::Duration::from_std(
                                                file.stored.playtime,
                                            )
                                            .expect(
                                                "Could not transform playtime to chrono duration",
                                            );
                                            ui.label(format!(
                                                "{:02}:{:02}:{:02}",
                                                dur.num_hours(),
                                                dur.num_minutes() % 60,
                                                dur.num_seconds() % 60,
                                            ));
                                        });
                                        row.col(|ui| {
                                            if ui
                                                .add_enabled(
                                                    true,
                                                    Button::new(
                                                        RichText::new("Delete").color(Color32::RED),
                                                    ),
                                                )
                                                .clicked()
                                            {
                                                remove_dir_all(&file.path)
                                                    .expect("Unable to delete save game");
                                                dirty = true;
                                            }
                                        });
                                        row.col(|ui| {
                                            if ui.add_enabled(true, Button::new("Load")).on_disabled_hover_text("Currently WASM does not support saving or loading").clicked() {
                                                let path = file.path.clone();
                                                let progress =
                                                    Arc::new(AtomicU64::new(0f64.to_bits()));
                                                let (send, recv) = channel();

                                                let progress_send = progress.clone();
                                                thread::spawn(move || {
                                                    send.send(run_integrated_server(
                                                        progress_send,
                                                        |progress, data_store| {
                                                            load(path).map(|sg| {
                                                                assert_eq!(
                                                                    sg.checksum, data_store.checksum,
                                                                    "A savegame can only be loaded with the EXACT same mods!"
                                                                );
                                                                sg.game_state
                                                            })
                                                            .unwrap()
                                                        },
                                                        None,
                                                    ))
                                                    .expect("Channel send failed");
                                                });
                                                new_state = Some(AppState::Loading {
                                                    start_time: Instant::now(),
                                                    progress,
                                                    game_state_receiver: recv,
                                                });
                                            }
                                        });
                                    },
                                    Err((p, e)) => {
                                        row.col(|ui| {
                                            ui.label("Save File corrupt").on_hover_text(&format!(
                                                "Path: {}, Error: {e:?}",
                                                p.display()
                                            ));
                                        });
                                        row.col(|ui| {
                                            ui.label("Save File corrupt").on_hover_text(&format!(
                                                "Path: {}, Error: {e:?}",
                                                p.display()
                                            ));
                                        });
                                        row.col(|ui| {
                                            ui.label("Save File corrupt").on_hover_text(&format!(
                                                "Path: {}, Error: {e:?}",
                                                p.display()
                                            ));
                                        });
                                        row.col(|ui| {
                                            ui.add_enabled(false, Button::new("Load"));
                                        });
                                        row.col(|ui| {
                                            if ui.add_enabled(true, Button::new("Delete")).clicked()
                                            {
                                                remove_dir_all(p)
                                                    .expect("Unable to delete save game");
                                                dirty = true;
                                            }
                                        });
                                    },
                                }
                            });
                        });

                    if dirty {
                        *save_files = SaveFileList::generate_from_save_folder(&save_folder())
                    }
                });
                // Borrow checker issue
                if let Some(new_state) = new_state {
                    self.state = new_state;
                }
            },

            AppState::Loading {
                start_time,
                progress,
                game_state_receiver,
            } => {
                let progress = f64::from_bits(progress.load(Ordering::Relaxed));

                CentralPanel::default().show(ctx, |ui| {
                    #[cfg(all(target_arch = "wasm32", target_os = "unknown"))]
                    ui.vertical_centered(|ui|{
                        ui.label(
                            egui::RichText::new("Detected running in a browser(WASM). Performance might be significantly degraded, and/or features might not work correctly. Support is on a best effort basis.")
                                .heading()
                                .color(egui::Color32::RED),
                        );
                        ui.label(
                            egui::RichText::new("For the best experience run on native.")
                                .heading()
                                .color(egui::Color32::RED),
                        );
                    });

                    Window::new("Loading")
                        .default_pos((0.5, 0.5))
                        .show(ctx, |ui| {
                            let mul: f64 = 1.0 / progress;
                            let text = if mul.is_infinite() {
                                format!("Calculating Remaining Time...")
                            } else {
                                if mul >= 1.0 {
                                    format!(
                                        "Est Remaining: {:?} min",
                                        start_time
                                            .elapsed()
                                            .mul_f64(mul - 1.0)
                                            .as_secs()
                                            .div_ceil(60)
                                    )
                                } else {
                                    error!("mul out of range 1.0..: {}", mul);
                                    format!("Calculating Remaining Time...")
                                }
                            };
                            ui.add(
                                ProgressBar::new(progress as f32)
                                    .corner_radius(0)
                                    .text(text),
                            );
                            if mul.is_finite() {
                                ui.label(format!(
                                    "Est Full Time: {:?} min",
                                    start_time.elapsed().mul_f64(mul).as_secs().div_ceil(60)
                                ));
                            }
                        });

                    // Window::new("Actions per second").show(ctx, |ui| {
                    //     ui.add(ProgressBar::new(progress as f32).corner_radius(0));
                    // });
                });

                if let Ok((new_state, current_tick, input_sender)) = game_state_receiver.try_recv()
                {
                    self.input_sender = Some(input_sender);
                    self.currently_loaded_game = Some(LoadedGameInfo {
                        state: new_state,
                        tick: current_tick,
                    });
                    // FIXME: This is needed to prevent the tech tree from collapsing?
                    // TODO: Make an issue to investigae why this is needed
                    Window::new("FIXME").show(ctx, |ui| {
                        egui_graphs::reset_layout::<LayoutStateTree>(
                            ui,
                            Some("Tech Tree".to_string()),
                        );
                    });
                    self.state = AppState::Ingame;
                }
            },

            AppState::MainMenu { in_ip_box } => {
                Window::new("Version")
                    .default_pos(Align2::RIGHT_TOP.pos_in_rect(&ctx.screen_rect()))
                    .show(ctx, |ui| {
                        Grid::new("version_grid").num_columns(2).show(ui, |ui| {
                            ui.label("Version:");
                            if crate::built_info::GIT_HEAD_REF == Some("refs/head/master") {
                                ui.label(crate::built_info::PKG_VERSION);
                            } else {
                                let version = crate::built_info::GIT_VERSION
                                    .unwrap_or("Could not get git version");
                                ui.label(version);
                            }
                            ui.end_row();

                            // TODO: This does not work because of nixos :/
                            // ui.label("Built at:");
                            // ui.label(crate::built_info::BUILT_TIME_UTC);
                            // ui.end_row();
                        })
                    });

                if let Some((current_text, error_pupup)) = in_ip_box {
                    if ctx.input(|input| input.key_pressed(egui::Key::Escape)) {
                        *in_ip_box = None;
                        return;
                    }

                    let popup = Modal::new("Ip Box".into());

                    if let Some(ip) = popup
                        .show(ctx, |ui| {
                            let text = TextEdit::singleline(current_text)
                                .char_limit(100)
                                .hint_text("ip:port");

                            let ret = if (text.show(ui).response.lost_focus()
                                && ui.input(|i| i.key_pressed(egui::Key::Enter)))
                                || ui.button("Connect").clicked()
                            {
                                match current_text.to_socket_addrs() {
                                    Ok(mut addr) => match addr.next() {
                                        Some(addr) => Some(addr),
                                        None => {
                                            *error_pupup = true;
                                            None
                                        },
                                    },
                                    Err(_) => {
                                        *error_pupup = true;
                                        None
                                    },
                                }
                            } else {
                                None
                            };

                            if *error_pupup {
                                ui.label(
                                    RichText::new("Invalid IP. Example: 127.0.0.1:4242")
                                        .color(Color32::RED),
                                );
                            }

                            ret
                        })
                        .inner
                    {
                        let progress = Arc::new(AtomicU64::new(0f64.to_bits()));
                        let (send, recv) = channel();

                        run_client(ip, send);

                        self.state = AppState::Loading {
                            start_time: Instant::now(),
                            progress,
                            game_state_receiver: recv,
                        };

                        return;
                    }
                }

                CentralPanel::default().show(ctx, |ui| {
                    #[cfg(all(target_arch = "wasm32", target_os = "unknown"))]
                        ui.vertical_centered(|ui|{
                            ui.label(
                                egui::RichText::new("Detected running in a browser(WASM). Performance might be significantly degraded, and/or features might not work correctly. Support is on a best effort basis.")
                                    .heading()
                                    .color(egui::Color32::RED),
                            );
                            ui.label(
                                egui::RichText::new("For the best experience run on native.")
                                    .heading()
                                    .color(egui::Color32::RED),
                            );
                        });

                    if ui.add_enabled(cfg!(not(target_arch = "wasm32")), Button::new("Load")).on_disabled_hover_text("Saving/Loading not yet supported when running the browser").clicked() {
                        self.state = AppState::LoadSaveMenu {
                            save_files: SaveFileList::generate_from_save_folder(
                                &save_folder()
                            ),
                        }
                    }
                    // else if ui.button("Load Debug Save").clicked() {
                    //     #[cfg(not(target_arch = "wasm32"))]
                    //     if let Some(path) = rfd::FileDialog::new()
                    //         .set_directory(
                    //             save_folder(),
                    //         )
                    //         .pick_file()
                    //     {
                    //         let progress = Arc::new(AtomicU64::new(0f64.to_bits()));
                    //         let (send, recv) = channel();

                    //         let progress_send = progress.clone();
                    //         thread::spawn(move || {
                    //             send.send(run_integrated_server(
                    //                 progress_send,
                    //                 StartGameInfo::LoadReadable(path),
                    //             )).expect("Channel send failed");
                    //         });

                    //         self.state = AppState::Loading {
                    //             start_time: Instant::now(),progress,
                    //             game_state_receiver: recv,
                    //         };
                    //     }
                    // }
                    else if ui
                        .button(
                            "Create New Game"
                        )
                        .clicked()
                    {
                        self.state = AppState::NewGameMenu { gigabase_size: 40, new_game_name: "My World".to_string()};
                    }
                    else if ui
                        .add_enabled(
                            cfg!(not(target_arch = "wasm32")),
                            egui::Button::new("Connect over network"),
                        )
                        .on_disabled_hover_text("Disabled on WASM")
                        .clicked()
                    {
                        let AppState::MainMenu { in_ip_box, .. } = &mut self.state else {
                            unreachable!()
                        };
                        assert!(in_ip_box.is_none());
                        *in_ip_box = Some((String::new(), false));
                    }
                });
            },

            AppState::NewGameMenu {
                gigabase_size,
                new_game_name,
            } => {
                CentralPanel::default().show(ctx, |ui| {
                    #[cfg(all(target_arch = "wasm32", target_os = "unknown"))]
                    ui.vertical_centered(|ui|{
                        ui.label(
                            egui::RichText::new("Detected running in a browser(WASM). Performance might be significantly degraded, and/or features might not work correctly. Support is on a best effort basis.")
                                .heading()
                                .color(egui::Color32::RED),
                        );
                        ui.label(
                            egui::RichText::new("For the best experience run on native.")
                                .heading()
                                .color(egui::Color32::RED),
                        );
                    });

                    #[cfg(all(target_arch = "wasm32", target_os = "unknown"))]
                    ui.vertical_centered(|ui|{
                        ui.label(
                            egui::RichText::new("Creating a game on WASM might freeze the browser tab for some time. Do not worry if after clicking \'Create\' the tab freezes and your browser warns that \"This page is slowing down your browser\"")
                                .heading()
                                .color(egui::Color32::YELLOW),
                        );
                    });

                    if ui.button("Back to Main Menu").clicked() {
                        self.state = AppState::MainMenu { in_ip_box: None };
                        return;
                    }
                    let ret =
                        example_worlds::list_example_worlds(&mut self.world_creation_state, ui);

                    if let Some(creation_fn) = ret {
                        let progress = Arc::new(AtomicU64::new(0f64.to_bits()));
                        let (send, recv) = channel();

                        let progress_send = progress.clone();
                        #[cfg(not(target_arch = "wasm32"))]
                        thread::spawn(move || {
                            send.send(run_integrated_server(
                                progress_send.clone(),
                                creation_fn,
                                None,
                            ))
                            .expect("Channel send failed");
                        });

                        #[cfg(target_arch = "wasm32")]
                        send.send(run_integrated_server(progress_send, creation_fn, None));

                        self.state = AppState::Loading {
                            start_time: Instant::now(),
                            progress,
                            game_state_receiver: recv,
                        };
                    }
                });
            },
        }
    }

    fn on_exit(&mut self) {
        if let Some(state) = &self.currently_loaded_game {
            match &state.state {
                LoadedGame::ItemU8RecipeU8(state) => {
                    save(
                        "Last Exit",
                        Some("last_exit.save"),
                        &state.state,
                        &state.data_store.lock(),
                    );
                },
                LoadedGame::ItemU8RecipeU16(state) => {
                    save(
                        "Last Exit",
                        Some("last_exit.save"),
                        &state.state,
                        &state.data_store.lock(),
                    );
                },
                LoadedGame::ItemU16RecipeU8(state) => {
                    save(
                        "Last Exit",
                        Some("last_exit.save"),
                        &state.state,
                        &state.data_store.lock(),
                    );
                },
                LoadedGame::ItemU16RecipeU16(state) => {
                    save(
                        "Last Exit",
                        Some("last_exit.save"),
                        &state.state,
                        &state.data_store.lock(),
                    );
                },
            }
        }
    }
}

impl App {
    fn update_ingame(&mut self, ctx: &eframe::egui::Context, _frame: &mut eframe::Frame) {
        let size = ctx.available_rect();

        #[cfg(target_arch = "wasm32")]
        let mut render_action_vec = vec![];

        CentralPanel::default().show(ctx, |ui| {
            if ui.ui_contains_pointer() {
                ctx.set_cursor_icon(CursorIcon::Default);
            }
            let painter = ui.painter();

            if let Some(game) = &self.currently_loaded_game {
                // Only create game input actions if the ui does not currently want input
                let wants_pointer = ctx.wants_pointer_input();
                let wants_keyboard = ctx.wants_keyboard_input();

                {
                    profiling::scope!("Inputs");
                    #[cfg(not(target_arch = "wasm32"))]
                    ui.input(|input_state| {
                        for event in &input_state.events {
                            match event {
                                Event::Copy
                                | Event::Cut
                                | Event::Paste(_)
                                | Event::Text(_)
                                | Event::Key { .. } => {
                                    if wants_keyboard {
                                        continue;
                                    }
                                },
                                Event::PointerMoved(_)
                                | Event::MouseMoved(_)
                                | Event::PointerButton { .. }
                                | Event::PointerGone
                                | Event::Zoom(_)
                                | Event::Touch { .. }
                                | Event::MouseWheel { .. } => {
                                    if wants_pointer {
                                        continue;
                                    }
                                },
                                _ => {},
                            }
                            let input = if let Event::PointerMoved(dest) = event {
                                let pos_normalized =
                                    [dest.x / size.width(), dest.y / size.height()];

                                let ar = size.width() / size.height();

                                if pos_normalized[0] < 0.0
                                    || pos_normalized[0] > 1.0
                                    || pos_normalized[1] < 0.0
                                    || pos_normalized[1] > 1.0
                                {
                                    continue;
                                }

                                Ok(Input::MouseMove(
                                    pos_normalized[0] - 0.5,
                                    (pos_normalized[1] - 0.5) / ar,
                                ))
                            } else {
                                event.clone().try_into()
                            };

                            if let Ok(input) = input {
                                if self.input_sender.as_mut().unwrap().send(input).is_err() {
                                    #[cfg(not(test))]
                                    panic!("Could not send input");
                                    #[allow(unreachable_code)]
                                    {
                                        error!("Could not send input");
                                    }
                                }
                            }
                        }
                    });
                }

                {
                    profiling::scope!("Render UI");
                    match &game.state {
                        LoadedGame::ItemU8RecipeU8(loaded_game_sized) => {
                            let cb = Callback {
                                raw_renderer: self
                                    .raw_renderer
                                    .clone()
                                    .expect("Tried to Load a game without a renderer ready"),
                                texture_atlas: self.texture_atlas.clone(),
                                state_machine: loaded_game_sized.state_machine.clone(),
                                game_state: loaded_game_sized.state.clone(),
                                data_store: loaded_game_sized.data_store.clone(),
                            };
                            painter.add(Shape::Callback(egui_wgpu::Callback::new_paint_callback(
                                size, cb,
                            )));

                            let simulation_state = loaded_game_sized.state.simulation_state.lock();
                            let world = loaded_game_sized.state.world.lock();
                            let aux_data = loaded_game_sized.state.aux_data.lock();
                            let state_machine = loaded_game_sized.state_machine.lock();
                            let data_store = loaded_game_sized.data_store.lock();

                            let tick = game.tick.load(std::sync::atomic::Ordering::Relaxed);

                            self.last_rendered_update = tick;

                            match render_ui(
                                ctx,
                                ui,
                                state_machine,
                                simulation_state,
                                world,
                                aux_data,
                                data_store,
                            ) {
                                Ok(render_actions) => {
                                    for action in render_actions {
                                        #[cfg(not(target_arch = "wasm32"))]
                                        loaded_game_sized
                                            .ui_action_sender
                                            .send(action)
                                            .expect("Ui action channel died");

                                        #[cfg(target_arch = "wasm32")]
                                        render_action_vec.push(action);
                                    }
                                },
                                Err(escape) => match escape {
                                    EscapeMenuOptions::BackToMainMenu => {
                                        #[cfg(not(target_arch = "wasm32"))]
                                        save(
                                            "Last Exit",
                                            Some("last_exit.save"),
                                            &loaded_game_sized.state,
                                            &loaded_game_sized.data_store.lock(),
                                        );

                                        self.state = AppState::MainMenu { in_ip_box: None };
                                        self.last_rendered_update = 0;
                                        self.input_sender = None;

                                        self.currently_loaded_game = None;
                                    },
                                },
                            }
                        },
                        LoadedGame::ItemU8RecipeU16(_loaded_game_sized) => {
                            todo!("Handle bigger item/recipe counts")
                        },
                        LoadedGame::ItemU16RecipeU8(_loaded_game_sized) => {
                            todo!("Handle bigger item/recipe counts")
                        },
                        LoadedGame::ItemU16RecipeU16(_loaded_game_sized) => {
                            todo!("Handle bigger item/recipe counts")
                        },
                    };
                }
            } else {
                warn!("No Game loaded!");
            }
        });

        #[cfg(target_arch = "wasm32")]
        {
            // TODO:
            // let wants_pointer = ctx.wants_pointer_input();
            // let wants_keyboard = ctx.wants_keyboard_input();

            let wants_pointer = false;
            let wants_keyboard = false;

            let mut inputs = vec![];
            ctx.input(|input_state| {
                for event in &input_state.events {
                    match event {
                        Event::Copy
                        | Event::Cut
                        | Event::Paste(_)
                        | Event::Text(_)
                        | Event::Key { .. } => {
                            if wants_keyboard {
                                continue;
                            }
                        },
                        Event::PointerMoved(_)
                        | Event::MouseMoved(_)
                        | Event::PointerButton { .. }
                        | Event::PointerGone
                        | Event::Zoom(_)
                        | Event::Touch { .. }
                        | Event::MouseWheel { .. } => {
                            if wants_pointer {
                                continue;
                            }
                        },
                        _ => {},
                    }
                    let input = if let Event::PointerMoved(dest) = event {
                        let pos_normalized = [dest.x / size.width(), dest.y / size.height()];

                        let ar = size.width() / size.height();

                        if pos_normalized[0] < 0.0
                            || pos_normalized[0] > 1.0
                            || pos_normalized[1] < 0.0
                            || pos_normalized[1] > 1.0
                        {
                            continue;
                        }

                        Ok(Input::MouseMove(
                            pos_normalized[0] - 0.5,
                            (pos_normalized[1] - 0.5) / ar,
                        ))
                    } else {
                        event.clone().try_into()
                    };

                    if let Ok(input) = input {
                        inputs.push(input);
                    }
                }
            });

            match self.currently_loaded_game.as_ref() {
                Some(game) => match &game.state {
                    LoadedGame::ItemU8RecipeU8(state) => {
                        let mut world_lock: parking_lot::lock_api::MutexGuard<
                            '_,
                            parking_lot::RawMutex,
                            crate::frontend::world::tile::World<u8, u8>,
                        > = state.state.world.lock();
                        let world = &mut *world_lock;

                        let data_store_lock = state.data_store.lock();
                        let data_store = &*data_store_lock;

                        let mut sim_state_lock = state.state.simulation_state.lock();
                        let sim_state = &mut *sim_state_lock;

                        let mut actions: Vec<crate::frontend::action::ActionType<_, _>> = state
                            .state_machine
                            .lock()
                            .handle_inputs(inputs, world, sim_state, data_store)
                            .collect();

                        actions.extend(
                            state
                                .state_machine
                                .lock()
                                .once_per_update_actions(world, data_store),
                        );

                        actions.extend(render_action_vec);

                        GameState::apply_actions(sim_state, world, actions, data_store);

                        // TODO: Do not run at 1 million UPS
                        GameState::update(sim_state, &mut *state.state.aux_data.lock(), data_store);
                    },
                    LoadedGame::ItemU8RecipeU16(_state) => todo!(),
                    LoadedGame::ItemU16RecipeU8(_state) => todo!(),
                    LoadedGame::ItemU16RecipeU16(_state) => todo!(),
                },
                None => {
                    warn!("No Game loaded!");
                },
            }
        }
    }
}

#[derive(Debug, Clone)]
struct Callback<ItemIdxType: WeakIdxTrait, RecipeIdxType: WeakIdxTrait> {
    raw_renderer: RawRenderer,
    texture_atlas: Arc<TextureAtlas>,
    state_machine: Arc<Mutex<ActionStateMachine<ItemIdxType, RecipeIdxType>>>,
    game_state: Arc<GameState<ItemIdxType, RecipeIdxType>>,
    data_store: Arc<Mutex<DataStore<ItemIdxType, RecipeIdxType>>>,
}

impl<ItemIdxType: IdxTrait, RecipeIdxType: IdxTrait> CallbackTrait
    for Callback<ItemIdxType, RecipeIdxType>
{
    fn paint(
        &self,
        info: PaintCallbackInfo,
        render_pass: &mut eframe::wgpu::RenderPass<'static>,
        _callback_resources: &egui_wgpu::CallbackResources,
    ) {
        let mut rend = self.raw_renderer.start_draw(
            render_pass,
            [
                info.viewport_in_pixels().width_px as f32,
                info.viewport_in_pixels().height_px as f32,
            ],
        );

        let sim_state = self.game_state.simulation_state.lock();
        let world = self.game_state.world.lock();
        let aux_data = self.game_state.aux_data.lock();
        let state_machine = self.state_machine.lock();

        render_world(
            &mut rend,
            sim_state,
            world,
            aux_data,
            &self.texture_atlas,
            state_machine,
            &self.data_store.lock(),
        );
    }
}
