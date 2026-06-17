use eframe::{
    egui::{CentralPanel, CursorIcon, Shape, Ui, Window},
    egui_wgpu,
};

use crate::eframe_app::Callback;

pub enum AppState {
    Ingame {},
    MainMenu {},
    Loading {},
}

impl AppState {
    pub fn draw(&mut self, ui: &mut Ui) {
        match self {
            Self::MainMenu {} => {
                let ctx = ui.ctx();

                Window::new("Main Menu").show(ctx, |ui| {
                    ui.label("Hello World");
                });
            },
            Self::Ingame {} => {
                let size = ui.ctx().content_rect();

                CentralPanel::default().show_inside(ui, |ui| {
                    if ui.ui_contains_pointer() {
                        ui.ctx().set_cursor_icon(CursorIcon::Default);
                    }
                    let painter = ui.painter();

                    let cb = Callback {
                        // raw_renderer: self
                        //     .raw_renderer
                        //     .clone()
                        //     .expect("Tried to Load a game without a renderer ready"),
                        raw_renderer: todo!(),
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
