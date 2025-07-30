use std::{
    sync::{
        Arc,
        atomic::{AtomicBool, AtomicU64},
        mpsc::Sender,
    },
    time::{Duration, Instant},
};

use parking_lot::Mutex;

use crate::{
    app_state::{AppState, GameState},
    data::DataStore,
    frontend::{
        action::{ActionType, action_state_machine::ActionStateMachine},
        input::Input,
    },
    item::WeakIdxTrait,
    rendering::render_world::render_world,
    saving::save,
};
use log::{info, warn};
use tilelib::types::Display;
use winit::{
    event::{ElementState, MouseButton, WindowEvent},
    window::WindowAttributes,
};

use super::{TextureAtlas, texture_atlas};

pub struct App {
    window: Window,
    pub state: AppState,
    pub currently_loaded_game: Option<LoadedGameInfo>,

    last_rendered_update: u64,

    input_sender: Sender<Input>,

    texture_atlas: TextureAtlas,
}

pub struct LoadedGameInfo {
    pub state: LoadedGame,
    pub tick: Arc<AtomicU64>,
}

pub enum LoadedGame {
    ItemU8RecipeU8(LoadedGameSized<u8, u8>),
    ItemU8RecipeU16(LoadedGameSized<u8, u16>),
    ItemU16RecipeU8(LoadedGameSized<u16, u8>),
    ItemU16RecipeU16(LoadedGameSized<u16, u16>),
}

pub struct LoadedGameSized<ItemIdxType: WeakIdxTrait, RecipeIdxType: WeakIdxTrait> {
    pub state: Arc<Mutex<GameState<ItemIdxType, RecipeIdxType>>>,
    pub state_machine: Arc<Mutex<ActionStateMachine<ItemIdxType, RecipeIdxType>>>,
    pub data_store: Arc<Mutex<DataStore<ItemIdxType, RecipeIdxType>>>,
    pub ui_action_sender: Sender<ActionType<ItemIdxType, RecipeIdxType>>,

    pub stop_update_thread: Arc<AtomicBool>,
}

impl<ItemIdxType: WeakIdxTrait, RecipeIdxType: WeakIdxTrait> Drop
    for LoadedGameSized<ItemIdxType, RecipeIdxType>
{
    fn drop(&mut self) {
        self.stop_update_thread
            .store(true, std::sync::atomic::Ordering::Relaxed);
    }
}

pub struct Window {
    winit_handle: Option<Arc<winit::window::Window>>,
    display: Option<tilelib::types::Display>,

    last_frame_time: Instant,
}

impl winit::application::ApplicationHandler for App {
    fn resumed(&mut self, event_loop: &winit::event_loop::ActiveEventLoop) {
        let window = Arc::new(
            event_loop
                .create_window(WindowAttributes::default().with_title("FactoryGame"))
                .expect("Unable to create window"),
        );

        // window.set_cursor_grab(winit::window::CursorGrabMode::Confined);

        self.window.display = Some(Display::from_winit(window.clone()));
        self.window.winit_handle = Some(window);
    }

    fn window_event(
        &mut self,
        event_loop: &winit::event_loop::ActiveEventLoop,
        _window_id: winit::window::WindowId,
        event: winit::event::WindowEvent,
    ) {
        assert!(self.window.display.is_some());
        assert!(self.window.display.as_ref().unwrap().renderer.is_some());

        match &event {
            // Exit the event loop when a close is requested (e.g. window's close button is pressed)
            WindowEvent::CloseRequested => {
                info!("EXITING");
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

                event_loop.exit();
            },

            // Resize the surface when the window is resized
            WindowEvent::Resized(size) => {
                self.window.display.as_mut().unwrap().resize(*size);
            },

            WindowEvent::KeyboardInput {
                device_id: _,
                event,
                is_synthetic,
            } => {
                // TODO: Maybe don't ignore synthetic events
                if *is_synthetic {
                    warn!("Synthic event: {:?}", event);
                } else {
                    let input = event.clone().try_into();

                    if let Ok(input) = input {
                        self.input_sender.send(input);
                    }
                }
            },

            WindowEvent::MouseWheel {
                device_id: _,
                delta,
                phase,
            } => {
                let input = Input::MouseScoll(todo!());
                self.input_sender.send(input);
            },

            WindowEvent::CursorMoved {
                device_id,
                position,
            } => {
                let inner_size = self.window.winit_handle.as_ref().unwrap().inner_size();
                let input: Input = Input::MouseMove(
                    position.x as f32 / inner_size.width as f32,
                    position.y as f32 / inner_size.height as f32,
                );
                self.input_sender.send(input);
            },

            WindowEvent::MouseInput {
                device_id: _,
                state,
                button,
            } => {
                let input = match (state, button) {
                    (ElementState::Pressed, MouseButton::Left) => {
                        Input::LeftClickPressed { shift: false }
                    },
                    (ElementState::Released, MouseButton::Left) => Input::LeftClickReleased,
                    (ElementState::Pressed, MouseButton::Right) => {
                        Input::RightClickPressed { shift: false }
                    },
                    (ElementState::Released, MouseButton::Right) => Input::RightClickReleased,
                    v => todo!("{:?}", v),
                };

                self.input_sender.send(input).unwrap();
            },

            // This is where all the rendering happens
            WindowEvent::RedrawRequested => {
                let start_render_time = Instant::now();

                let renderer = self.window.display.as_mut().unwrap().get_renderer();

                let fps =
                    Duration::from_secs(1).div_duration_f32(self.window.last_frame_time.elapsed());

                match &self.state {
                    AppState::MainMenu { in_ip_box } => todo!(),

                    AppState::Ingame => {
                        if let Some(loaded) = &self.currently_loaded_game {
                            let current_tick =
                                loaded.tick.load(std::sync::atomic::Ordering::Relaxed);

                            // TODO: This way we kind of spin in the render loop, only redrawing when there is another update.
                            // This is wrong according to the winit docs, which mention ALWAYS redrawing when recieving WindowEvent::RedrawRequested
                            if current_tick > self.last_rendered_update {
                                self.window.last_frame_time = Instant::now();
                                info!("fps: {}", fps);

                                match &loaded.state {
                                    LoadedGame::ItemU8RecipeU8(loaded_game_sized) => {
                                        let game_state = loaded_game_sized.state.lock();
                                        let state_machine = loaded_game_sized.state_machine.lock();
                                        render_world(
                                            renderer,
                                            game_state,
                                            &self.texture_atlas,
                                            state_machine,
                                            &loaded_game_sized.data_store.lock(),
                                        )
                                    },
                                    LoadedGame::ItemU8RecipeU16(loaded_game_sized) => todo!(),
                                    LoadedGame::ItemU16RecipeU8(loaded_game_sized) => todo!(),
                                    LoadedGame::ItemU16RecipeU16(loaded_game_sized) => todo!(),
                                }

                                // self.window_handle.as_ref().unwrap().pre_present_notify();

                                self.window
                                    .display
                                    .as_mut()
                                    .unwrap()
                                    .finish_frame()
                                    .expect("Could not finish frame");

                                info!("render_cpu_time: {:?}", start_render_time.elapsed());
                                self.last_rendered_update = current_tick;
                            }
                        } else {
                            warn!("No Game loaded");
                        }
                    },
                    AppState::Loading { .. } => {
                        // TODO:
                    },
                }

                self.window.winit_handle.as_ref().unwrap().request_redraw();
            },
            e => {
                info!("Ignoring event {:?}", e);
            },
        }

        assert!(self.window.display.is_some());
        assert!(
            self.window.display.as_ref().unwrap().renderer.is_some(),
            "Renderer lost"
        );
    }
}

impl Window {
    pub fn new() -> Self {
        Self {
            winit_handle: None,
            display: None,
            last_frame_time: Instant::now(),
        }
    }
}

impl App {
    pub fn new(input_sender: Sender<Input>) -> Self {
        Self {
            state: AppState::MainMenu { in_ip_box: None },
            window: Window::new(),
            last_rendered_update: 0,
            currently_loaded_game: None,
            input_sender,

            texture_atlas: texture_atlas(),
        }
    }
}
