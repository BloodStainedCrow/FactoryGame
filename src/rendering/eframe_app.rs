use std::{
    mem,
    sync::{mpsc::Sender, Arc, LazyLock},
    time::Instant,
};

use parking_lot::Mutex;

use eframe::{
    egui::{CentralPanel, Event, PaintCallbackInfo, Shape},
    egui_wgpu::{self, CallbackTrait},
};
use log::warn;
use tilelib::types::RawRenderer;

use crate::{
    data::DataStore,
    frontend::{action::action_state_machine::ActionStateMachine, input::Input},
    item::{IdxTrait, WeakIdxTrait},
};

use super::{
    app_state::{AppState, GameState},
    render_world::{render_ui, render_world},
    texture_atlas,
    window::{LoadedGame, LoadedGameInfo},
    TextureAtlas,
};

use crate::saving::save;

pub struct App {
    raw_renderer: RawRenderer,
    pub state: AppState,
    pub currently_loaded_game: Option<LoadedGameInfo>,

    last_rendered_update: u64,

    input_sender: Sender<Input>,

    texture_atlas: Arc<TextureAtlas>,
}

impl App {
    #[must_use]
    pub fn new(cc: &eframe::CreationContext, input_sender: Sender<Input>) -> Self {
        let render_state = cc.wgpu_render_state.as_ref().unwrap();
        let atlas = Arc::new(texture_atlas());

        Self {
            raw_renderer: RawRenderer::new(
                &render_state.device,
                &render_state.queue,
                render_state.target_format,
            ),
            input_sender,
            state: AppState::Ingame,
            texture_atlas: atlas,
            currently_loaded_game: None,
            last_rendered_update: 0,
        }
    }
}

impl eframe::App for App {
    fn update(&mut self, ctx: &eframe::egui::Context, frame: &mut eframe::Frame) {
        ctx.request_repaint();

        let size = ctx.available_rect();

        CentralPanel::default().show(ctx, |ui| {
            let painter = ui.painter();

            let game_graphics_area = painter.clip_rect();

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
                            self.input_sender.send(input).expect("Could not send input");
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

                        let game_state = loaded_game_sized.state.lock();
                        let mut state_machine = loaded_game_sized.state_machine.lock();

                        let tick = game.tick.load(std::sync::atomic::Ordering::Relaxed);

                        static LAST_DRAW: LazyLock<Mutex<Instant>> =
                            LazyLock::new(|| Mutex::new(Instant::now()));

                        let mut now = Instant::now();

                        mem::swap(&mut now, &mut *LAST_DRAW.lock());

                        let time_since_last_update = now.elapsed();

                        // dbg!(tick - self.last_rendered_update);

                        self.last_rendered_update = tick;

                        let render_actions = render_ui(
                            ctx,
                            &ui,
                            &mut state_machine,
                            &game_state,
                            &loaded_game_sized.data_store.lock(),
                        );

                        for action in render_actions {
                            loaded_game_sized
                                .ui_action_sender
                                .send(action)
                                .expect("Ui action channel died");
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

    fn on_exit(&mut self, _gl: Option<&eframe::glow::Context>) {
        if let Some(state) = &self.currently_loaded_game {
            match &state.state {
                LoadedGame::ItemU8RecipeU8(state) => save(
                    &state.state.lock(),
                    state.data_store.lock().checksum.clone(),
                ),
                LoadedGame::ItemU8RecipeU16(state) => save(
                    &state.state.lock(),
                    state.data_store.lock().checksum.clone(),
                ),
                LoadedGame::ItemU16RecipeU8(state) => save(
                    &state.state.lock(),
                    state.data_store.lock().checksum.clone(),
                ),
                LoadedGame::ItemU16RecipeU16(state) => save(
                    &state.state.lock(),
                    state.data_store.lock().checksum.clone(),
                ),
            }
        }
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
        callback_resources: &egui_wgpu::CallbackResources,
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
            &gamestate,
            &self.texture_atlas,
            &state_machine,
            &self.data_store.lock(),
        );
    }
}
