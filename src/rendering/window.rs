use std::{
    sync::{atomic::AtomicU64, mpsc::Sender, Arc, Mutex},
    time::{Duration, Instant},
};

use crate::{
    data::DataStore,
    frontend::{
        action::action_state_machine::ActionStateMachine, input::Input, world::tile::World,
    },
    item::IdxTrait,
};
use log::{info, warn};
use tilelib::types::{Display, Sprite, Texture};
use winit::{
    event::{ElementState, MouseButton, WindowEvent},
    window::WindowAttributes,
};

use crate::{load, save};

use super::{
    app_state::{AppState, GameState, SimulationState},
    TextureAtlas,
};
use image::GenericImageView;

pub struct App<ItemIdxType: IdxTrait, RecipeIdxType: IdxTrait> {
    pub state: Arc<Mutex<AppState<ItemIdxType, RecipeIdxType>>>,
    pub state_machine: Arc<Mutex<ActionStateMachine<ItemIdxType>>>,
    window: Window,

    last_rendered_update: u64,
    pub current_tick: Arc<AtomicU64>,

    input_sender: Sender<Input>,

    texture_atlas: TextureAtlas,

    data_store: Arc<DataStore<ItemIdxType, RecipeIdxType>>,
}

pub struct Window {
    winit_handle: Option<Arc<winit::window::Window>>,
    display: Option<tilelib::types::Display>,

    last_frame_time: Instant,
}

impl<ItemIdxType: IdxTrait, RecipeIdxType: IdxTrait> winit::application::ApplicationHandler
    for App<ItemIdxType, RecipeIdxType>
{
    fn resumed(&mut self, event_loop: &winit::event_loop::ActiveEventLoop) {
        dbg!("RESUMED");
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
        window_id: winit::window::WindowId,
        event: winit::event::WindowEvent,
    ) {
        assert!(self.window.display.is_some());
        assert!(self.window.display.as_ref().unwrap().renderer.is_some());

        match &event {
            // Exit the event loop when a close is requested (e.g. window's close button is pressed)
            WindowEvent::CloseRequested => {
                info!("EXITING");
                match &*self.state.lock().unwrap() {
                    AppState::Ingame(game_state) => save(game_state),
                }

                event_loop.exit();
            },

            // Resize the surface when the window is resized
            WindowEvent::Resized(size) => {
                self.window.display.as_mut().unwrap().resize(*size);
            },

            WindowEvent::KeyboardInput {
                device_id,
                event,
                is_synthetic,
            } => {
                // TODO: Maybe don't ignore synthetic events
                if *is_synthetic {
                    warn!("Synthic event: {:?}", event);
                } else {
                    let input: Input = event.clone().into();
                    self.input_sender.send(input).expect("Channel died");
                }
            },

            WindowEvent::MouseWheel {
                device_id,
                delta,
                phase,
            } => {
                let input = Input::MouseScoll(*delta);
                self.input_sender.send(input).expect("Channel died");
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
                self.input_sender.send(input).expect("Channel died");
            },

            WindowEvent::MouseInput {
                device_id,
                state,
                button,
            } => {
                let input = match (state, button) {
                    (ElementState::Pressed, MouseButton::Left) => Input::LeftClickPressed,
                    (ElementState::Released, MouseButton::Left) => Input::LeftClickReleased,
                    (ElementState::Pressed, MouseButton::Right) => Input::RightClickPressed,
                    (ElementState::Released, MouseButton::Right) => Input::RightClickReleased,
                    (_, _) => todo!(),
                };

                self.input_sender.send(input).expect("Channel died");
            },

            // This is where all the rendering happens
            WindowEvent::RedrawRequested => {
                let start_render_time = Instant::now();

                let renderer = self.window.display.as_mut().unwrap().get_renderer();

                let fps =
                    Duration::from_secs(1).div_duration_f32(self.window.last_frame_time.elapsed());

                let current_tick = self.current_tick.load(std::sync::atomic::Ordering::Relaxed);

                // TODO: This way we kind of spin in the render loop, only redrawing when there is another update.
                // This is wrong according to the winit docs, which mention ALWAYS redrawing when recieving WindowEvent::RedrawRequested
                if current_tick > self.last_rendered_update {
                    self.window.last_frame_time = Instant::now();

                    info!("fps: {}", fps);

                    self.state.lock().unwrap().render(
                        self.window.display.as_mut().unwrap().get_renderer(),
                        &self.texture_atlas,
                        &self.state_machine.lock().unwrap(),
                        &self.data_store,
                    );

                    // self.window_handle.as_ref().unwrap().pre_present_notify();

                    self.window.display.as_mut().unwrap().finish_frame();

                    info!("render_cpu_time: {:?}", start_render_time.elapsed());
                    self.last_rendered_update = current_tick;
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

impl<
        ItemIdxType: IdxTrait + for<'a> serde::Deserialize<'a>,
        RecipeIdxType: IdxTrait + for<'a> serde::Deserialize<'a>,
    > App<ItemIdxType, RecipeIdxType>
{
    pub fn new(
        input_sender: Sender<Input>,
        data_store: Arc<DataStore<ItemIdxType, RecipeIdxType>>,
    ) -> Self {
        let black = include_bytes!("temp_assets/outside_world.png");
        let black = image::load_from_memory(black).unwrap();

        let black_dimensions = black.dimensions();
        let black = black.to_rgba8().into_vec();

        let blue = include_bytes!("temp_assets/blue.png");
        let blue = image::load_from_memory(blue).unwrap();

        let blue_dimensions = blue.dimensions();
        let blue = blue.to_rgba8().into_vec();

        let assembler = include_bytes!("temp_assets/assembler.png");
        let assembler = image::load_from_memory(assembler).unwrap();

        let assembler_dimensions = assembler.dimensions();
        let assembler = assembler.to_rgba8().into_vec();

        let belt_north = include_bytes!("temp_assets/belt_north.png");
        let belt_north = image::load_from_memory(belt_north).unwrap();

        let belt_north_dimensions = belt_north.dimensions();
        let belt_north = belt_north.to_rgba8().into_vec();

        let belt_south = include_bytes!("temp_assets/belt_south.png");
        let belt_south = image::load_from_memory(belt_south).unwrap();

        let belt_south_dimensions = belt_south.dimensions();
        let belt_south = belt_south.to_rgba8().into_vec();

        let belt_west = include_bytes!("temp_assets/belt_west.png");
        let belt_west = image::load_from_memory(belt_west).unwrap();

        let belt_west_dimensions = belt_west.dimensions();
        let belt_west = belt_west.to_rgba8().into_vec();

        let belt_east = include_bytes!("temp_assets/belt_east.png");
        let belt_east = image::load_from_memory(belt_east).unwrap();

        let belt_east_dimensions = belt_east.dimensions();
        let belt_east = belt_east.to_rgba8().into_vec();

        let inserter_north = include_bytes!("temp_assets/inserter_north.png");
        let inserter_north = image::load_from_memory(inserter_north).unwrap();

        let inserter_north_dimensions = inserter_north.dimensions();
        let inserter_north = inserter_north.to_rgba8().into_vec();

        let inserter_south = include_bytes!("temp_assets/inserter_south.png");
        let inserter_south = image::load_from_memory(inserter_south).unwrap();

        let inserter_south_dimensions = inserter_south.dimensions();
        let inserter_south = inserter_south.to_rgba8().into_vec();

        let inserter_west = include_bytes!("temp_assets/inserter_west.png");
        let inserter_west = image::load_from_memory(inserter_west).unwrap();

        let inserter_west_dimensions = inserter_west.dimensions();
        let inserter_west = inserter_west.to_rgba8().into_vec();

        let inserter_east = include_bytes!("temp_assets/inserter_east.png");
        let inserter_east = image::load_from_memory(inserter_east).unwrap();

        let inserter_east_dimensions = inserter_east.dimensions();
        let inserter_east = inserter_east.to_rgba8().into_vec();

        let plate = include_bytes!("temp_assets/plate.png");
        let plate = image::load_from_memory(plate).unwrap();

        let plate_dimensions = plate.dimensions();
        let plate = plate.to_rgba8().into_vec();

        let state = load().unwrap_or_else(|| GameState {
            world: World::new(),
            simulation_state: SimulationState::new(&data_store),
        });

        Self {
            state: Arc::new(Mutex::new(AppState::Ingame(state))),
            state_machine: Arc::new(Mutex::new(ActionStateMachine::new())),
            window: Window::new(),
            last_rendered_update: 0,
            current_tick: Arc::new(AtomicU64::new(0)),
            input_sender,

            texture_atlas: TextureAtlas {
                outside_world: Sprite::new(Texture::new(1, black, black_dimensions)),
                assembler: Sprite::new(Texture::new(1, assembler, assembler_dimensions)),
                blue: Sprite::new(Texture::new(1, blue, blue_dimensions)),

                plate: Sprite::new(Texture::new(1, plate, plate_dimensions)),
                belt: enum_map::EnumMap::from_array([
                    Sprite::new(Texture::new(1, belt_north, belt_north_dimensions)),
                    Sprite::new(Texture::new(1, belt_east, belt_east_dimensions)),
                    Sprite::new(Texture::new(1, belt_south, belt_south_dimensions)),
                    Sprite::new(Texture::new(1, belt_west, belt_west_dimensions)),
                ]),

                inserter: enum_map::EnumMap::from_array([
                    Sprite::new(Texture::new(1, inserter_north, inserter_north_dimensions)),
                    Sprite::new(Texture::new(1, inserter_east, inserter_east_dimensions)),
                    Sprite::new(Texture::new(1, inserter_south, inserter_south_dimensions)),
                    Sprite::new(Texture::new(1, inserter_west, inserter_west_dimensions)),
                ]),

                default: Sprite::new(Texture::default()),
            },
            data_store,
        }
    }
}
