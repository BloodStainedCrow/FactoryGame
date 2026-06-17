use std::sync::Arc;

use eframe::{
    egui::{CentralPanel, CursorIcon, Shape, Ui, Window},
    egui_wgpu,
};
use frontend::GameState;
use tilelib::types::RawRenderer;

use crate::eframe_app::Callback;

pub enum AppState {
    Ingame { game_state: Arc<GameState> },
    MainMenu {},
    Loading {},
}

impl AppState {
    pub fn draw(&self, ui: &mut Ui, raw_renderer: Option<&RawRenderer>) {
        match self {
            Self::MainMenu {} => {
                let ctx = ui.ctx();

                Window::new("Main Menu").show(ctx, |ui| {
                    ui.label("Hello World");
                });
            },
            Self::Ingame { game_state } => {
                let size = ui.ctx().content_rect();

                CentralPanel::default().show_inside(ui, |ui| {
                    if ui.ui_contains_pointer() {
                        ui.ctx().set_cursor_icon(CursorIcon::Default);
                    }
                    let painter = ui.painter();

                    let cb = Callback {
                        state: game_state.clone(),
                        raw_renderer: raw_renderer
                            .expect("Cannot draw game without a renderer")
                            .clone(),
                    };
                    painter.add(Shape::Callback(egui_wgpu::Callback::new_paint_callback(
                        size, cb,
                    )));
                });
            },
            Self::Loading {} => todo!(),
        }
    }
}
