use std::{
    mem,
    net::ToSocketAddrs,
    sync::{
        Arc, LazyLock,
        atomic::{AtomicU64, Ordering},
        mpsc::{Sender, channel},
    },
    thread,
    time::Instant,
};

use directories::ProjectDirs;
use parking_lot::Mutex;

use crate::StartGameInfo;
use crate::{GameCreationInfo, run_client};
use crate::{rendering::render_world::EscapeMenuOptions, run_integrated_server};
use eframe::{
    egui::{CentralPanel, Event, PaintCallbackInfo, Shape},
    egui_wgpu::{self, CallbackTrait},
};
use egui::{Color32, CursorIcon, Modal, ProgressBar, RichText, TextEdit, Window};
use log::{error, warn};
use tilelib::types::RawRenderer;

use crate::{
    data::DataStore,
    frontend::{action::action_state_machine::ActionStateMachine, input::Input},
    item::{IdxTrait, WeakIdxTrait},
};

use super::{
    TextureAtlas,
    app_state::{AppState, GameState},
    render_world::{render_ui, render_world},
    texture_atlas,
    window::{LoadedGame, LoadedGameInfo},
};

use crate::saving::save;

pub struct App {
    raw_renderer: RawRenderer,
    pub state: AppState,
    pub currently_loaded_game: Option<LoadedGameInfo>,

    last_rendered_update: u64,

    pub input_sender: Option<Sender<Input>>,

    texture_atlas: Arc<TextureAtlas>,
}

impl App {
    #[must_use]
    pub fn new(cc: &eframe::CreationContext) -> Self {
        let render_state = cc.wgpu_render_state.as_ref().unwrap();
        let atlas = Arc::new(texture_atlas());

        Self {
            raw_renderer: RawRenderer::new(
                &render_state.device,
                &render_state.queue,
                render_state.target_format,
            ),
            input_sender: None,
            state: AppState::MainMenu { in_ip_box: None },
            texture_atlas: atlas,
            currently_loaded_game: None,
            last_rendered_update: 0,
        }
    }
}

impl eframe::App for App {
    fn update(&mut self, ctx: &eframe::egui::Context, frame: &mut eframe::Frame) {
        ctx.request_repaint();

        match &mut self.state {
            AppState::Ingame => {
                self.update_ingame(ctx, frame);
            },

            AppState::Loading {
                progress,
                game_state_receiver,
            } => {
                let progress = f64::from_bits(progress.load(Ordering::Relaxed));

                CentralPanel::default().show(ctx, |ui| {
                    Window::new("Loading")
                        .default_pos((0.5, 0.5))
                        .show(ctx, |ui| {
                            ui.add(ProgressBar::new(progress as f32).corner_radius(0));
                        });
                });

                if let Ok((new_state, current_tick, input_sender)) = game_state_receiver.try_recv()
                {
                    self.input_sender = Some(input_sender);
                    self.currently_loaded_game = Some(LoadedGameInfo {
                        state: new_state,
                        tick: current_tick,
                    });
                    self.state = AppState::Ingame;
                }
            },

            AppState::MainMenu { in_ip_box } => {
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
                                    RichText::new("Invalid IP. Example: 127.0.0.1:8080")
                                        .color(Color32::RED),
                                );
                            }

                            ret
                        })
                        .inner
                    {
                        let progress = Arc::new(AtomicU64::new(0f64.to_bits()));
                        let (send, recv) = channel();

                        send.send(run_client(ip));

                        self.state = AppState::Loading {
                            progress,
                            game_state_receiver: recv,
                        };

                        return;
                    }
                }

                CentralPanel::default().show(ctx, |ui| {
                    if ui.button("Load").clicked() {
                        if let Some(path) = rfd::FileDialog::new()
                            .set_directory(
                                ProjectDirs::from("de", "aschhoff", "factory_game")
                                    .expect("No Home path found")
                                    .data_dir(),
                            )
                            .pick_file()
                        {
                            let progress = Arc::new(AtomicU64::new(0f64.to_bits()));
                            let (send, recv) = channel();

                            let progress_send = progress.clone();
                            thread::spawn(move || {
                                send.send(run_integrated_server(
                                    progress_send,
                                    StartGameInfo::Load(path),
                                ));
                            });

                            self.state = AppState::Loading {
                                progress,
                                game_state_receiver: recv,
                            };
                        }
                    } else if ui.button("Load Debug Save").clicked() {
                        if let Some(path) = rfd::FileDialog::new()
                            .set_directory(
                                ProjectDirs::from("de", "aschhoff", "factory_game")
                                    .expect("No Home path found")
                                    .data_dir(),
                            )
                            .pick_file()
                        {
                            let progress = Arc::new(AtomicU64::new(0f64.to_bits()));
                            let (send, recv) = channel();

                            let progress_send = progress.clone();
                            thread::spawn(move || {
                                send.send(run_integrated_server(
                                    progress_send,
                                    StartGameInfo::LoadReadable(path),
                                ));
                            });

                            self.state = AppState::Loading {
                                progress,
                                game_state_receiver: recv,
                            };
                        }
                    } else if ui.button("Connect over network").clicked() {
                        let AppState::MainMenu { in_ip_box } = &mut self.state else {
                            unreachable!()
                        };
                        assert!(in_ip_box.is_none());
                        *in_ip_box = Some((String::new(), false));
                    } else if ui.button("Empty World").clicked() {
                        let progress = Arc::new(AtomicU64::new(0f64.to_bits()));
                        let (send, recv) = channel();

                        let progress_send = progress.clone();
                        thread::spawn(move || {
                            send.send(run_integrated_server(
                                progress_send,
                                StartGameInfo::Create(GameCreationInfo::Empty),
                            ));
                        });

                        self.state = AppState::Loading {
                            progress,
                            game_state_receiver: recv,
                        };
                    } else if ui.button("Red Green Chest Insertion").clicked() {
                        let progress = Arc::new(AtomicU64::new(0f64.to_bits()));
                        let (send, recv) = channel();

                        let progress_send = progress.clone();
                        thread::spawn(move || {
                            send.send(run_integrated_server(
                                progress_send,
                                StartGameInfo::Create(GameCreationInfo::RedGreen),
                            ));
                        });

                        self.state = AppState::Loading {
                            progress,
                            game_state_receiver: recv,
                        };
                    } else if ui.button("Red Green 1 to 1 belts").clicked() {
                        let progress = Arc::new(AtomicU64::new(0f64.to_bits()));
                        let (send, recv) = channel();

                        let progress_send = progress.clone();
                        thread::spawn(move || {
                            send.send(run_integrated_server(
                                progress_send,
                                StartGameInfo::Create(GameCreationInfo::RedGreenBelts),
                            ));
                        });

                        self.state = AppState::Loading {
                            progress,
                            game_state_receiver: recv,
                        };
                    } else if ui.button("Red with labs").clicked() {
                        let progress = Arc::new(AtomicU64::new(0f64.to_bits()));
                        let (send, recv) = channel();

                        let progress_send = progress.clone();
                        thread::spawn(move || {
                            send.send(run_integrated_server(
                                progress_send,
                                StartGameInfo::Create(GameCreationInfo::RedWithLabs),
                            ));
                        });

                        self.state = AppState::Loading {
                            progress,
                            game_state_receiver: recv,
                        };
                    } else if ui.button("With bp file").clicked() {
                        if let Some(path) = rfd::FileDialog::new().pick_file() {
                            let progress = Arc::new(AtomicU64::new(0f64.to_bits()));
                            let (send, recv) = channel();

                            let progress_send = progress.clone();
                            thread::spawn(move || {
                                send.send(run_integrated_server(
                                    progress_send,
                                    StartGameInfo::Create(GameCreationInfo::FromBP(path)),
                                ));
                            });

                            self.state = AppState::Loading {
                                progress,
                                game_state_receiver: recv,
                            };
                        }
                    }
                });
            },
        }
    }

    fn on_exit(&mut self, _gl: Option<&eframe::glow::Context>) {
        if let Some(state) = &self.currently_loaded_game {
            match &state.state {
                LoadedGame::ItemU8RecipeU8(state) => {
                    save(&state.state.lock(), &state.data_store.lock())
                },
                LoadedGame::ItemU8RecipeU16(state) => {
                    save(&state.state.lock(), &state.data_store.lock())
                },
                LoadedGame::ItemU16RecipeU8(state) => {
                    save(&state.state.lock(), &state.data_store.lock())
                },
                LoadedGame::ItemU16RecipeU16(state) => {
                    save(&state.state.lock(), &state.data_store.lock())
                },
            }
        }
    }
}

impl App {
    fn update_ingame(&mut self, ctx: &eframe::egui::Context, _frame: &mut eframe::Frame) {
        let size = ctx.available_rect();

        CentralPanel::default().show(ctx, |ui| {
            if ui.ui_contains_pointer() {
                ctx.set_cursor_icon(CursorIcon::Default);
            }
            let painter = ui.painter();

            if let Some(game) = &self.currently_loaded_game {
                // Only create game input actions if the ui does not currently want input
                let wants_pointer = ctx.wants_pointer_input();
                let wants_keyboard = ctx.wants_keyboard_input();

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
                            if self.input_sender.as_mut().unwrap().send(input).is_err() {
                                #[cfg(not(test))]
                                panic!("Could not send input");
                                error!("Could not send input");
                            }
                        }
                    }
                });

                match &game.state {
                    LoadedGame::ItemU8RecipeU8(loaded_game_sized) => {
                        let cb = Callback {
                            raw_renderer: self.raw_renderer.clone(),
                            texture_atlas: self.texture_atlas.clone(),
                            state_machine: loaded_game_sized.state_machine.clone(),
                            game_state: loaded_game_sized.state.clone(),
                            data_store: loaded_game_sized.data_store.clone(),
                        };
                        painter.add(Shape::Callback(egui_wgpu::Callback::new_paint_callback(
                            size, cb,
                        )));

                        let mut game_state = loaded_game_sized.state.lock();
                        let mut state_machine = loaded_game_sized.state_machine.lock();
                        let data_store = loaded_game_sized.data_store.lock();

                        let tick = game.tick.load(std::sync::atomic::Ordering::Relaxed);

                        static LAST_DRAW: LazyLock<Mutex<Instant>> =
                            LazyLock::new(|| Mutex::new(Instant::now()));

                        let mut now = Instant::now();

                        mem::swap(&mut now, &mut *LAST_DRAW.lock());

                        self.last_rendered_update = tick;

                        match render_ui(ctx, ui, &mut state_machine, &mut game_state, &data_store) {
                            Ok(render_actions) => {
                                for action in render_actions {
                                    loaded_game_sized
                                        .ui_action_sender
                                        .send(action)
                                        .expect("Ui action channel died");
                                }
                            },
                            Err(escape) => match escape {
                                EscapeMenuOptions::BackToMainMenu => {
                                    self.state = AppState::MainMenu { in_ip_box: None };
                                    self.last_rendered_update = 0;
                                    self.input_sender = None;

                                    // Needed to make borrows happy
                                    mem::drop(game_state);
                                    mem::drop(state_machine);
                                    mem::drop(data_store);
                                    self.currently_loaded_game = None;
                                },
                            },
                        }
                    },
                    LoadedGame::ItemU8RecipeU16(loaded_game_sized) => todo!(),
                    LoadedGame::ItemU16RecipeU8(loaded_game_sized) => todo!(),
                    LoadedGame::ItemU16RecipeU16(loaded_game_sized) => todo!(),
                };
            } else {
                warn!("No Game loaded!");
            }
        });
    }
}

#[derive(Debug, Clone)]
struct Callback<ItemIdxType: WeakIdxTrait, RecipeIdxType: WeakIdxTrait> {
    raw_renderer: RawRenderer,
    texture_atlas: Arc<TextureAtlas>,
    state_machine: Arc<Mutex<ActionStateMachine<ItemIdxType, RecipeIdxType>>>,
    game_state: Arc<Mutex<GameState<ItemIdxType, RecipeIdxType>>>,
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

        let gamestate = self.game_state.lock();

        let state_machine = self.state_machine.lock();

        render_world(
            &mut rend,
            gamestate,
            &self.texture_atlas,
            state_machine,
            &self.data_store.lock(),
        );
    }
}
