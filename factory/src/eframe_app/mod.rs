use eframe::{
    egui::{self, PaintCallbackInfo},
    egui_wgpu::{self, CallbackTrait},
};
use tilelib::types::RawRenderer;

use crate::app_state::AppState;

pub struct App {
    app_state: AppState,
}

impl App {
    pub const fn new(_cc: &eframe::CreationContext<'_>) -> Self {
        // Customize egui here with cc.egui_ctx.set_fonts and cc.egui_ctx.set_global_style.
        // Restore app state using cc.storage (requires the "persistence" feature).
        // Use the cc.gl (a glow::Context) to create graphics shaders and buffers that you can use
        // for e.g. egui::PaintCallback.
        Self {
            app_state: AppState::MainMenu {},
        }
    }
}

impl eframe::App for App {
    fn ui(&mut self, ui: &mut egui::Ui, frame: &mut eframe::Frame) {}
}

#[derive(Debug, Clone)]
pub struct Callback {
    pub raw_renderer: RawRenderer,
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

        // render_game(&mut renderer, query_engine, render_info);
    }
}
