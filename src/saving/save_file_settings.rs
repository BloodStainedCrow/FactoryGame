use std::{path::PathBuf, time::Duration};

use chrono::Utc;

#[derive(Debug, Clone)]
pub(crate) struct SaveFileInfo {
    pub(crate) path: PathBuf,
    pub(crate) stored: StoredSaveFileInfo,
}

#[derive(Debug, Clone, serde::Serialize, serde::Deserialize)]
pub(crate) struct StoredSaveFileInfo {
    pub(crate) name: String,

    pub(crate) saved_at: chrono::DateTime<Utc>,
    pub(crate) playtime: Duration,
    pub(super) is_autosave: bool,

    pub(super) includes_replay: bool,

    // scenario: ScenarioInfo,

    // TODO: Do I want a preview?
    pub(super) preview: Option<()>,
}

#[derive(Debug, Clone, serde::Serialize, serde::Deserialize)]
struct SaveFileSettings {
    save_location: PathBuf,
}
