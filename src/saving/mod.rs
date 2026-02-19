use std::{
    borrow::Borrow,
    fs::{File, create_dir_all},
    io::{Read, Write},
    marker::PhantomData,
    path::PathBuf,
    time::Duration,
};

use bitcode::Encode;
use directories::ProjectDirs;
use log::error;

use std::io::{BufReader, BufWriter};

use crate::{
    app_state::{AuxillaryData, Factory, GameState, SimulationState},
    data::DataStore,
    frontend::world::tile::World,
    get_size::Mutex,
    item::IdxTrait,
    join_many::join,
    par_generation::Timer,
    saving::save_file_settings::StoredSaveFileInfo,
};

pub mod loading;
mod save_file_settings;

pub(crate) fn save_folder() -> PathBuf {
    ProjectDirs::from("de", "aschhoff", "factory_game")
        .expect("No Home path found")
        .data_dir()
        .join("saves")
}

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

    let mut buf_writer = BufWriter::new(file);
    {
        profiling::scope!("Compressing and Writing to file");
        bincode::serde::encode_into_std_write(value, &mut buf_writer, bincode::config::standard())
            .unwrap();
    }
    buf_writer.flush().unwrap();
}

pub fn save_at_fork<V: serde::Serialize + ?Sized>(value: &V, path: PathBuf) {
    let file = {
        File::create(&path).expect(&format!(
            "could not create file, tried to create {}",
            path.display()
        ))
    };

    // FIXME: It is technically not okay to allocate here.
    let mut buf_writer = BufWriter::new(file);

    bincode::serde::encode_into_std_write(value, &mut buf_writer, bincode::config::standard())
        .unwrap();
    buf_writer.flush().unwrap();
}

pub fn load_at<V: for<'a> serde::Deserialize<'a>>(path: PathBuf) -> V {
    try_load_at(path).expect("Failed to load file")
}

#[derive(Debug)]
pub(crate) enum LoadError {
    CouldNotOpenFile(std::io::Error),
    DeserializationFailed(bincode::error::DecodeError),
}

pub fn try_load_at<V: for<'a> serde::Deserialize<'a>>(path: PathBuf) -> Result<V, LoadError> {
    profiling::scope!("Load at", format!("path: {}", path.display()));
    let file = {
        profiling::scope!("Open file");
        File::open(&path).map_err(|err| LoadError::CouldNotOpenFile(err))?
    };

    let mut buf_reader = BufReader::new(file);
    let loaded = {
        profiling::scope!("Decompressing and deserializing");
        bincode::serde::decode_from_std_read(&mut buf_reader, bincode::config::standard())
            .map_err(|err| LoadError::DeserializationFailed(err))?
    };

    Ok(loaded)
}

/// # Panics
/// If File system stuff fails
pub fn save_components<ItemIdxType: IdxTrait, RecipeIdxType: IdxTrait>(
    name: &str,
    save_name: Option<&str>,

    world: &World<ItemIdxType, RecipeIdxType>,
    simulation_state: &SimulationState<ItemIdxType, RecipeIdxType>,
    aux_data: &AuxillaryData,
    data_store: &DataStore<ItemIdxType, RecipeIdxType>,
) {
    let checksum = data_store.checksum.clone();
    let save_dir = save_folder();

    let lockfile = crate::lockfile::LockfileUnique::create_blocking(
        save_dir.join("save_in_progress.lockfile"),
    )
    .expect("Locking lockfile failed");

    create_dir_all(&save_dir).expect("Could not create save dir");

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

    let temp_file_dir = save_dir.join("tmp.save");
    let save_file_dir = if let Some(name) = save_name {
        save_dir.join(&name)
    } else {
        save_dir.join("autosave.save")
    };

    let info = StoredSaveFileInfo {
        name: if save_name.is_some() {
            name.to_string()
        } else {
            format!("[Autosave] {}", name)
        },
        saved_at: chrono::offset::Utc::now(),
        playtime: Duration::from_secs(1) / 60 * aux_data.current_tick as u32,
        is_autosave: save_name.is_none(),
        includes_replay: false,
        preview: None,
    };

    create_dir_all(&temp_file_dir).expect("Could not create temp dir");
    dbg!(&temp_file_dir);

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
                    ore_store,
                },
        } = simulation_state;

        join!(
            || {
                save_at(&info, temp_file_dir.join("save_file_info"));
            },
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
                save_at(
                    ore_store,
                    temp_file_dir.join("simulation_state.factory.ore_store"),
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

    // Remove old save if it exists
    let _ = std::fs::remove_dir_all(&save_file_dir);
    std::fs::rename(&temp_file_dir, save_file_dir).expect(&format!(
        "Could not rename tmp save dir: {}",
        temp_file_dir.display()
    ));

    lockfile.release().expect("Failed to remove lockfile");
}

pub const FORK_SAVE_STAGES: usize = 13;
/// # Panics
/// If File system stuff fails
#[cfg(not(target_arch = "wasm32"))]
pub fn save_components_fork_safe<ItemIdxType: IdxTrait, RecipeIdxType: IdxTrait>(
    name: &str,
    save_name: Option<&str>,

    world: &World<ItemIdxType, RecipeIdxType>,
    simulation_state: &SimulationState<ItemIdxType, RecipeIdxType>,
    aux_data: &AuxillaryData,
    data_store: &DataStore<ItemIdxType, RecipeIdxType>,
    mut send: interprocess::unnamed_pipe::Sender,
) {
    let checksum = &data_store.checksum;
    let save_dir = save_folder();

    let lockfile = crate::lockfile::LockfileUnique::create_blocking(
        save_dir.join("save_in_progress.lockfile"),
    )
    .expect("Locking lockfile failed");

    create_dir_all(&save_dir).expect("Could not create data dir");

    // FIXME: Allocation and chrono is prob illegal after a fork
    let info = StoredSaveFileInfo {
        name: if save_name.is_some() {
            name.to_string()
        } else {
            format!("[Autosave] {}", name)
        },
        saved_at: chrono::offset::Utc::now(),
        playtime: Duration::from_secs(1) / 60 * aux_data.current_tick as u32,

        is_autosave: save_name.is_none(),
        includes_replay: false,
        preview: None,
    };

    let temp_file_dir = save_dir.join("tmp.save");
    let save_file_dir = if let Some(name) = save_name {
        save_dir.join(&name)
    } else {
        save_dir.join("autosave.save")
    };

    assert_ne!(temp_file_dir, save_dir);

    create_dir_all(&temp_file_dir).expect("Could not create temp dir");
    dbg!(&temp_file_dir);

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
                    ore_store,
                },
        } = simulation_state;

        save_at_fork(&info, temp_file_dir.join("save_file_info"));
        send.write(&[0]).expect("Write to pipe failed");
        // send.flush().expect("Flushing pipe failed");
        save_at_fork(checksum, temp_file_dir.join("checksum"));
        send.write(&[1]).expect("Write to pipe failed");
        // send.flush().expect("Flushing pipe failed");
        save_at_fork(
            tech_state,
            temp_file_dir.join("simulation_state.tech_state"),
        );
        send.write(&[2]).expect("Write to pipe failed");
        // send.flush().expect("Flushing pipe failed");
        save_at_fork(
            power_grids,
            temp_file_dir.join("simulation_state.factory.power_grids"),
        );
        send.write(&[3]).expect("Write to pipe failed");
        // send.flush().expect("Flushing pipe failed");
        save_at_fork(belts, temp_file_dir.join("simulation_state.factory.belts"));
        send.write(&[4]).expect("Write to pipe failed");
        // send.flush().expect("Flushing pipe failed");
        save_at_fork(
            storage_storage_inserters,
            temp_file_dir.join("simulation_state.factory.storage_storage_inserters"),
        );
        send.write(&[5]).expect("Write to pipe failed");
        // send.flush().expect("Flushing pipe failed");
        save_at_fork(
            belt_storage_inserters,
            temp_file_dir.join("simulation_state.factory.belt_storage_inserters"),
        );
        send.write(&[6]).expect("Write to pipe failed");
        // send.flush().expect("Flushing pipe failed");
        save_at_fork(
            chests,
            temp_file_dir.join("simulation_state.factory.chests"),
        );
        send.write(&[7]).expect("Write to pipe failed");
        // send.flush().expect("Flushing pipe failed");
        save_at_fork(
            fluid_store,
            temp_file_dir.join("simulation_state.factory.fluid_store"),
        );
        send.write(&[8]).expect("Write to pipe failed");
        // send.flush().expect("Flushing pipe failed");
        save_at_fork(
            item_times,
            temp_file_dir.join("simulation_state.factory.item_times"),
        );
        send.write(&[9]).expect("Write to pipe failed");
        // send.flush().expect("Flushing pipe failed");
        save_at_fork(
            ore_store,
            temp_file_dir.join("simulation_state.factory.ore_store"),
        );
        send.write(&[10]).expect("Write to pipe failed");
        // send.flush().expect("Flushing pipe failed");
        world.save_fork(temp_file_dir.join("world"));
        send.write(&[11]).expect("Write to pipe failed");
        // send.flush().expect("Flushing pipe failed");
        save_at_fork(aux_data, temp_file_dir.join("aux_data"));
        send.write(&[12]).expect("Write to pipe failed");
        // send.flush().expect("Flushing pipe failed");
    }

    // Remove old save if it exists
    let _ = std::fs::remove_dir_all(&save_file_dir);
    std::fs::rename(temp_file_dir, save_file_dir).expect("Could not rename tmp save dir!");

    lockfile.release().expect("Failed to remove lockfile");
}

/// # Panics
/// If File system stuff fails
#[cfg(not(target_arch = "wasm32"))]
pub fn save_with_fork<ItemIdxType: IdxTrait, RecipeIdxType: IdxTrait>(
    name: &str,
    save_name: Option<&str>,

    world: &World<ItemIdxType, RecipeIdxType>,
    simulation_state: &SimulationState<ItemIdxType, RecipeIdxType>,
    aux_data: &AuxillaryData,
    data_store: &DataStore<ItemIdxType, RecipeIdxType>,
) -> Option<interprocess::unnamed_pipe::Recver> {
    #[cfg(target_os = "linux")]
    {
        let (send, recv) =
            interprocess::unnamed_pipe::pipe().expect("Failed to create unnamed pipe");
        match fork::fork() {
            Ok(fork::Fork::Parent(child_pid)) => {
                log::info!("Started saving fork with pid {}", child_pid);
                return Some(recv);
            },
            Ok(fork::Fork::Child) => {},
            Err(e) => {
                log::error!("Saving with fork failed: Unable to create fork: {}", e);
                save_components(
                    name,
                    save_name,
                    world,
                    simulation_state,
                    aux_data,
                    data_store,
                );
                return None;
            },
        }

        save_components_fork_safe(
            name,
            save_name,
            world,
            simulation_state,
            aux_data,
            data_store,
            send,
        );

        unsafe { libc::_exit(0) }
    }

    #[cfg(not(target_os = "linux"))]
    {
        save_components(world, simulation_state, aux_data, data_store);
        None
    }
}

/// # Panics
/// If File system stuff fails
pub fn save<ItemIdxType: IdxTrait, RecipeIdxType: IdxTrait>(
    name: &str,
    save_name: Option<&str>,

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

    save_components(
        name,
        save_name,
        world,
        simulation_state,
        aux_data,
        data_store,
    );
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
    let _timer = Timer::new("Load Save File from disk");

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
        ore_store,
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
            profiling::scope!("Deserialize simulation_state.factory.ore_store");
            load_at(path.join("simulation_state.factory.ore_store"))
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
                ore_store,
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
