use std::{
    borrow::Borrow,
    fs::{File, create_dir_all},
    io::{Read, Write},
    marker::PhantomData,
    path::PathBuf,
};

use bitcode::Encode;
use directories::ProjectDirs;
use flate2::{Compression, read::ZlibDecoder, write::ZlibEncoder};
use log::error;

use crate::{
    app_state::{AuxillaryData, Factory, GameState, SimulationState},
    data::DataStore,
    frontend::world::tile::World,
    get_size::Mutex,
    item::IdxTrait,
    join_many::join,
};

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

pub fn save_at<V: serde::Serialize + ?Sized>(value: &V, path: PathBuf) {
    profiling::scope!("Save at", format!("path: {}", path.display()));
    let file = {
        profiling::scope!("Create file");
        File::create(path).expect("could not create file")
    };
    let data = {
        profiling::scope!("Serialize");
        bitcode::serialize(value).unwrap()
    };

    {
        profiling::scope!("Compressing and Writing to file");
        let mut e = ZlibEncoder::new(file, Compression::fast());
        e.write_all(&data).expect("write_all failed");
        e.finish().expect("Compression failed");
    }
}

pub fn load_at<V: for<'a> serde::Deserialize<'a>>(path: PathBuf) -> V {
    profiling::scope!("Load at", format!("path: {}", path.display()));
    let file = {
        profiling::scope!("Open file");
        File::open(&path).expect(&format!("could not open file {:?}", &path))
    };

    {
        profiling::scope!("Decompressing and deserializing");
        let mut data = vec![];
        let mut e = ZlibDecoder::new(file);
        e.read_to_end(&mut data).expect("Read to end failed");
        bitcode::deserialize(&data).expect("Deserialization failed")
    }
}

/// # Panics
/// If File system stuff fails
pub fn save_components<ItemIdxType: IdxTrait, RecipeIdxType: IdxTrait>(
    world: &World<ItemIdxType, RecipeIdxType>,
    simulation_state: &SimulationState<ItemIdxType, RecipeIdxType>,
    aux_data: &AuxillaryData,
    data_store: &DataStore<ItemIdxType, RecipeIdxType>,
) {
    let checksum = data_store.checksum.clone();
    let dir = ProjectDirs::from("de", "aschhoff", "factory_game").expect("No Home path found");

    create_dir_all(dir.data_dir()).expect("Could not create data dir");

    // if let Ok(s) = env::var("FACTORY_SAVE_READABLE") {
    //     if s == "true" {
    //         let readable_save_file_dir = dir.data_dir().join("readable.save");

    //         let mut file = File::create(readable_save_file_dir).expect("Could not open file");

    //         let res = ron::ser::to_string_pretty(
    //             &SaveGame {
    //                 checksum: checksum.clone(),
    //                 game_state,
    //                 item: PhantomData,
    //                 recipe: PhantomData,
    //             },
    //             PrettyConfig::default(),
    //         )
    //         .unwrap();

    //         file.write_all(res.as_bytes())
    //             .expect("Could not write to file");
    //     }
    // }

    let temp_file_dir = dir.data_dir().join("tmp.save");
    let save_file_dir = dir.data_dir().join("save.save");

    create_dir_all(&temp_file_dir).expect("Could not create temp dir");

    // FIXME: What to do, if the size of the Save in memory + on disk exceeds RAM?

    {
        let SimulationState {
            tech_state,
            factory:
                Factory {
                    power_grids,
                    belts,
                    storage_storage_inserters,
                    belt_storage_inserters,
                    chests,
                    fluid_store,
                    item_times,
                },
        } = simulation_state;

        join!(
            || {
                save_at(&checksum, temp_file_dir.join("checksum"));
            },
            || {
                save_at(
                    tech_state,
                    temp_file_dir.join("simulation_state.tech_state"),
                );
            },
            || {
                save_at(
                    power_grids,
                    temp_file_dir.join("simulation_state.factory.power_grids"),
                );
            },
            || {
                save_at(belts, temp_file_dir.join("simulation_state.factory.belts"));
            },
            || {
                save_at(
                    storage_storage_inserters,
                    temp_file_dir.join("simulation_state.factory.storage_storage_inserters"),
                );
            },
            || {
                save_at(
                    belt_storage_inserters,
                    temp_file_dir.join("simulation_state.factory.belt_storage_inserters"),
                );
            },
            || {
                save_at(
                    chests,
                    temp_file_dir.join("simulation_state.factory.chests"),
                );
            },
            || {
                save_at(
                    fluid_store,
                    temp_file_dir.join("simulation_state.factory.fluid_store"),
                );
            },
            || {
                save_at(
                    item_times,
                    temp_file_dir.join("simulation_state.factory.item_times"),
                );
            },
            || {
                profiling::scope!("Serialize world");
                world.par_save(temp_file_dir.join("world"));
            },
            || {
                save_at(aux_data, temp_file_dir.join("aux_data"));
            }
        );
    }

    // bitcode::file
    //     .write_all(&res)
    //     .expect("Could not write to file");

    // Remove old save if it exists
    let _ = std::fs::remove_dir_all(&save_file_dir);
    std::fs::rename(temp_file_dir, save_file_dir).expect("Could not rename tmp save dir!");
}

/// # Panics
/// If File system stuff fails
pub fn save<ItemIdxType: IdxTrait, RecipeIdxType: IdxTrait>(
    game_state: &GameState<ItemIdxType, RecipeIdxType>,
    data_store: &DataStore<ItemIdxType, RecipeIdxType>,
) {
    let GameState {
        world,
        simulation_state,
        aux_data,
    } = game_state;

    profiling::scope!("Save");
    let simulation_state = {
        profiling::scope!("Lock simulation_state");
        &*simulation_state.lock()
    };
    let world = {
        profiling::scope!("Lock world");
        &*world.lock()
    };
    let aux_data = {
        profiling::scope!("Lock aux_data");
        &*aux_data.lock()
    };

    save_components(world, simulation_state, aux_data, data_store);
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
    let (
        checksum,
        tech_state,
        power_grids,
        belts,
        storage_storage_inserters,
        belt_storage_inserters,
        chests,
        fluid_store,
        item_times,
        world,
        aux_data,
    ) = join!(
        || {
            profiling::scope!("Deserialize checksum");
            load_at(path.join("checksum"))
        },
        || {
            profiling::scope!("Deserialize simulation_state.tech_state");
            load_at(path.join("simulation_state.tech_state"))
        },
        || {
            profiling::scope!("Deserialize simulation_state.factory.power_grids");
            load_at(path.join("simulation_state.factory.power_grids"))
        },
        || {
            profiling::scope!("Deserialize simulation_state.factory.belts");
            load_at(path.join("simulation_state.factory.belts"))
        },
        || {
            profiling::scope!("Deserialize simulation_state.factory.storage_storage_inserters");
            load_at(path.join("simulation_state.factory.storage_storage_inserters"))
        },
        || {
            profiling::scope!("Deserialize simulation_state.factory.belt_storage_inserters");
            load_at(path.join("simulation_state.factory.belt_storage_inserters"))
        },
        || {
            profiling::scope!("Deserialize simulation_state.factory.chests");
            load_at(path.join("simulation_state.factory.chests"))
        },
        || {
            profiling::scope!("Deserialize simulation_state.factory.fluid_store");
            load_at(path.join("simulation_state.factory.fluid_store"))
        },
        || {
            profiling::scope!("Deserialize simulation_state.factory.item_times");
            load_at(path.join("simulation_state.factory.item_times"))
        },
        || {
            profiling::scope!("Deserialize world");
            Mutex::new(World::par_load(path.join("world")))
        },
        || {
            profiling::scope!("Deserialize aux_data");
            load_at(path.join("aux_data"))
        }
    );

    let game_state = GameState {
        world,
        simulation_state: Mutex::new(SimulationState {
            tech_state,
            factory: Factory {
                power_grids,
                belts,
                storage_storage_inserters,
                belt_storage_inserters,
                chests,
                fluid_store,
                item_times,
            },
        }),
        aux_data,
    };

    Some(SaveGame {
        checksum,
        game_state,
        item: PhantomData,
        recipe: PhantomData,
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
