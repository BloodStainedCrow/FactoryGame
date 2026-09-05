use std::{sync::Arc, time::Duration};

use eframe::{
    egui::{self, PaintCallbackInfo},
    egui_wgpu::{self, CallbackTrait},
};
use frontend::{GameState, SurfaceId};
use log::{error, warn};
use tilelib::types::RawRenderer;

use crate::{app_state::AppState, render_game::RenderInfo};

pub struct App {
    app_state: AppState,
    tilelib_renderer: Option<RawRenderer>,
    // TODO: This will need to be in a different thread
}

impl App {
    pub fn new(cc: &eframe::CreationContext<'_>) -> Self {
        let render_state = cc.wgpu_render_state.as_ref();
        // Customize egui here with cc.egui_ctx.set_fonts and cc.egui_ctx.set_global_style.
        // Restore app state using cc.storage (requires the "persistence" feature).
        // Use the cc.gl (a glow::Context) to create graphics shaders and buffers that you can use
        // for e.g. egui::PaintCallback.
        Self {
            app_state: AppState::Ingame {
                game_state: Arc::new(GameState::default()),
            },
            tilelib_renderer: render_state.map(|render_state| {
                RawRenderer::new(
                    &render_state.device,
                    &render_state.queue,
                    render_state.target_format,
                )
            }),
        }
    }
}

impl eframe::App for App {
    fn ui(&mut self, ui: &mut egui::Ui, frame: &mut eframe::Frame) {
        if self.tilelib_renderer.is_none() {
            if let Some(render_state) = frame.wgpu_render_state() {
                self.tilelib_renderer = Some(RawRenderer::new(
                    &render_state.device,
                    &render_state.queue,
                    render_state.target_format,
                ));
                warn!("Was finally able to set a renderer.");
            } else {
                error!("Unable to set up renderer!!!!!");
            }
        }

        self.app_state.draw(ui, self.tilelib_renderer.as_ref());

        ui.ctx()
            .request_repaint_after(Duration::from_secs_f32(1.0 / 60.0));
    }
}

#[derive(Debug, Clone)]
pub struct Callback {
    pub raw_renderer: RawRenderer,

    pub state: Arc<GameState>,
}

impl CallbackTrait for Callback {
    fn paint(
        &self,
        info: PaintCallbackInfo,
        render_pass: &mut eframe::wgpu::RenderPass<'static>,
        _callback_resources: &egui_wgpu::CallbackResources,
    ) {
        let mut renderer = self.raw_renderer.start_draw(
            render_pass,
            [
                info.viewport_in_pixels()
                    .width_px
                    .try_into()
                    .expect("Negative canvas size????"),
                info.viewport_in_pixels()
                    .height_px
                    .try_into()
                    .expect("Negative canvas size????"),
            ],
        );

        log::trace!("Paint");
        crate::render_game::render_game(
            &mut renderer,
            self.state.as_ref(),
            RenderInfo {
                surface: SurfaceId::default(),
                map_view: false,
                zoom_level: 19.5,
                center: [0.0, 0.0],
            },
        );
    }
}
