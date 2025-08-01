use std::{
    borrow::Borrow,
    env,
    fs::{File, create_dir_all},
    io::{Read, Write},
    marker::PhantomData,
    path::PathBuf,
};

use bitcode::Encode;
use directories::ProjectDirs;
use log::error;
use ron::ser::PrettyConfig;

use crate::{data::DataStore, item::IdxTrait, rendering::app_state::GameState};

#[derive(Debug, Encode, serde::Deserialize, serde::Serialize)]
pub struct SaveGame<
    ItemIdxType: IdxTrait,
    RecipeIdxType: IdxTrait,
    G: Borrow<GameState<ItemIdxType, RecipeIdxType>>,
> {
    pub checksum: String,
    pub game_state: G,
    // pub data_store: D,
    pub item: PhantomData<ItemIdxType>,
    pub recipe: PhantomData<RecipeIdxType>,
}

/// # Panics
/// If File system stuff fails
pub fn save<ItemIdxType: IdxTrait, RecipeIdxType: IdxTrait>(
    game_state: &GameState<ItemIdxType, RecipeIdxType>,
    data_store: &DataStore<ItemIdxType, RecipeIdxType>,
) {
    let checksum = data_store.checksum.clone();
    let dir = ProjectDirs::from("de", "aschhoff", "factory_game").expect("No Home path found");

    create_dir_all(dir.data_dir()).expect("Could not create data dir");

    if let Ok(s) = env::var("FACTORY_SAVE_READABLE") {
        if s == "true" {
            let readable_save_file_dir = dir.data_dir().join("readable.save");

            let mut file = File::create(readable_save_file_dir).expect("Could not open file");

            let res = ron::ser::to_string_pretty(
                &SaveGame {
                    checksum: checksum.clone(),
                    game_state,
                    item: PhantomData,
                    recipe: PhantomData,
                },
                PrettyConfig::default(),
            )
            .unwrap();

            file.write_all(res.as_bytes())
                .expect("Could not write to file");
        }
    }

    let temp_file_dir = dir.data_dir().join("tmp.save");
    let save_file_dir = dir.data_dir().join("save.save");

    let mut file = File::create(&temp_file_dir).expect("Could not open file");

    // FIXME: What to do, if the size of the Save in memory + on disk exceeds RAM?
    let res = bitcode::serialize(&SaveGame {
        checksum,
        game_state,
        item: PhantomData,
        recipe: PhantomData,
    })
    .unwrap();

    file.write_all(&res).expect("Could not write to file");

    std::fs::rename(temp_file_dir, save_file_dir).expect("Could not rename tmp save file!");
}

/// # Panics
/// If File system stuff fails
#[must_use]
pub fn load<
    ItemIdxType: IdxTrait + for<'a> serde::Deserialize<'a>,
    RecipeIdxType: IdxTrait + for<'a> serde::Deserialize<'a>,
>(
    path: PathBuf,
) -> Option<SaveGame<ItemIdxType, RecipeIdxType, GameState<ItemIdxType, RecipeIdxType>>> {
    let file = File::open(path);

    file.map_or(None, |mut file| {
        let mut v = Vec::with_capacity(file.metadata().unwrap().len() as usize);

        file.read_to_end(&mut v).unwrap();

        match bitcode::deserialize(&mut v) {
            Ok(val) => Some(val),
            Err(err) => {
                error!("Found save, but was unable to deserialize it!!!! \n{}", err);
                None
            },
        }
    })
}

/// # Panics
/// If File system stuff fails
#[must_use]
pub fn load_readable<
    ItemIdxType: IdxTrait + for<'a> serde::Deserialize<'a>,
    RecipeIdxType: IdxTrait + for<'a> serde::Deserialize<'a>,
>(
    path: PathBuf,
) -> Option<SaveGame<ItemIdxType, RecipeIdxType, GameState<ItemIdxType, RecipeIdxType>>> {
    let file = File::open(path);

    file.map_or(None, |mut file| {
        let mut v = Vec::with_capacity(file.metadata().unwrap().len() as usize);

        file.read_to_end(&mut v).unwrap();

        let mut de = ron::Deserializer::from_bytes(&v).unwrap();

        match serde_path_to_error::deserialize(&mut de) {
            Ok(val) => Some(val),
            Err(err) => {
                error!("Found save, but was unable to deserialize it!!!! \n{}", err);
                None
            },
        }
    })
}
