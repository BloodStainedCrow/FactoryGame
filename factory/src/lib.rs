use eframe::NativeOptions;

use crate::eframe_app::App;

mod app_state;
mod eframe_app;
mod render_game;

pub fn run_window() {
    puffin::set_scopes_on(true);

    simple_logger::SimpleLogger::new()
        .with_level(log::LevelFilter::Warn)
        .env()
        .init()
        .expect("Initializing simple_logger failed");
    log::info!("Welcome to main on native");

    let native_options = NativeOptions {
        vsync: false,
        ..NativeOptions::default()
    };
    eframe::run_native(
        "FactoryGame",
        native_options,
        Box::new(|cc| Ok(Box::new(App::new(cc)))),
    )
    .expect("Error from run_native");
}
