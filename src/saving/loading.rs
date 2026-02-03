use std::path::{Path, PathBuf};

use crate::saving::{
    LoadError,
    save_file_settings::{SaveFileInfo, StoredSaveFileInfo},
    try_load_at,
};

#[derive(Debug)]
pub(crate) struct SaveFileList {
    pub(crate) save_files: Vec<Result<SaveFileInfo, (PathBuf, SaveFileError)>>,
}

#[derive(Debug)]
pub(crate) enum SaveFileError {
    CouldNotOpenSaveFile(std::io::Error),
    LoadError(LoadError),
}

impl SaveFileList {
    pub(crate) fn generate_from_save_folder(save_folder: &Path) -> Self {
        let folder = std::fs::read_dir(save_folder).expect("Could not read save folder");

        let mut saves: Vec<_> = folder
            .filter_map(|save_folder| {
                let save = save_folder.unwrap();

                if save.file_name() == "save_in_progress.lockfile" {
                    return None;
                }

                let info_path = save.path().join("save_file_info");
                let info: Result<StoredSaveFileInfo, LoadError> = try_load_at(info_path);

                Some(
                    info.map_err(|err| (save.path(), SaveFileError::LoadError(err)))
                        .map(|stored| SaveFileInfo {
                            path: save.path(),
                            stored,
                        }),
                )
            })
            .collect();

        saves.sort_by(|a, b| match (a, b) {
            (Ok(a), Ok(b)) => a.stored.saved_at.cmp(&b.stored.saved_at).reverse(),
            (Ok(_), Err(_)) => std::cmp::Ordering::Greater,
            (Err(_), Ok(_)) => std::cmp::Ordering::Less,
            (Err((a, _)), Err((b, _))) => a.cmp(b),
        });

        SaveFileList { save_files: saves }
    }
}
