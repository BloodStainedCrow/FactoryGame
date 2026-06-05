use std::{num::NonZero, sync::LazyLock};

use parking_lot::Mutex;

use crate::{
    TICKS_PER_SECOND_LOGIC,
    saving::{settings_file, try_load_at},
};

#[derive(Debug, serde::Serialize, serde::Deserialize)]
pub struct GlobalSettings {
    pub mouse_wheel_sensitivity: f32,
    pub autosave_interval: Option<NonZero<u32>>,
    pub use_non_blocking_save: bool,
}

impl Default for GlobalSettings {
    fn default() -> Self {
        Self {
            mouse_wheel_sensitivity: 1.0,
            autosave_interval: Some((60 * TICKS_PER_SECOND_LOGIC as u32).try_into().unwrap()),
            use_non_blocking_save: true,
        }
    }
}

pub static GLOBAL_SETTINGS: LazyLock<Mutex<GlobalSettings>> = LazyLock::new(|| {
    let global_settings = try_load_at(settings_file()).unwrap_or_else(|e| {
        log::error!("Could not load settings: {e:?}. Loading default settings");
        Default::default()
    });

    Mutex::new(global_settings)
});
