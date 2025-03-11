use std::{
    borrow::Borrow,
    fs::{create_dir_all, File},
    io::Write,
    marker::PhantomData,
};

use directories::ProjectDirs;
use log::error;

use crate::{item::{IdxTrait, WeakIdxTrait}, rendering::app_state::GameState};

#[derive(Debug, serde::Deserialize, serde::Serialize)]
pub struct SaveGame<
    ItemIdxType: IdxTrait,
    RecipeIdxType: IdxTrait,
    G: Borrow<GameState<ItemIdxType, RecipeIdxType>>,
> {
    pub checksum: String,
    pub game_state: G,
    pub item: PhantomData<ItemIdxType>,
    pub recipe: PhantomData<RecipeIdxType>,
}

/// # Panics
/// If File system stuff fails
pub fn save<ItemIdxType: IdxTrait, RecipeIdxType: IdxTrait>(
    game_state: &GameState<ItemIdxType, RecipeIdxType>,
    checksum: String,
) {
    let dir = ProjectDirs::from("de", "aschhoff", "factory_game").expect("No Home path found");

    create_dir_all(dir.data_dir()).expect("Could not create data dir");

    let save_file_dir = dir.data_dir().join("save.save");

    let mut file = File::create(save_file_dir).expect("Could not open file");

    file.write_all(
        ron::ser::to_string_pretty(
            &SaveGame {
                checksum,
                game_state,
                item: PhantomData,
                recipe: PhantomData,
            },
            ron::ser::PrettyConfig::default(),
        )
        .unwrap()
        .as_bytes(),
    )
    .expect("Could not write to file");
}

/// # Panics
/// If File system stuff fails
#[must_use]
pub fn load<
    ItemIdxType: IdxTrait + for<'a> serde::Deserialize<'a>,
    RecipeIdxType: IdxTrait + for<'a> serde::Deserialize<'a>,
>() -> Option<SaveGame<ItemIdxType, RecipeIdxType, GameState<ItemIdxType, RecipeIdxType>>> {
    let dir = ProjectDirs::from("de", "aschhoff", "factory_game").expect("No Home path found");

    let save_file_dir = dir.data_dir().join("save.save");

    let file = File::open(save_file_dir);

    file.map_or(None, |file| match ron::de::from_reader(file) {
        Ok(val) => Some(val),
        Err(err) => {
            error!("Found save, but was unable to deserialize it!!!! \n{}", err);
            None
        },
    })
}
