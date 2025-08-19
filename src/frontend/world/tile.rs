#[cfg(feature = "client")]
use egui::Color32;
#[cfg(feature = "client")]
use egui_show_info::{EguiDisplayable, InfoExtractor, ShowInfo};
use log::error;
use rayon::iter::IndexedParallelIterator;
use rayon::iter::ParallelIterator;
use rayon::prelude::ParallelSliceMut;
use std::{
    cmp::min,
    collections::{BTreeMap, BTreeSet, HashMap},
    marker::PhantomData,
    mem,
    num::NonZero,
    ops::{Add, ControlFlow, Range},
};

use crate::frontend::world::sparse_grid::GetGridIndex;

use crate::{frontend::action::belt_placement, get_size::EnumMap};
#[cfg(feature = "client")]
use egui_show_info_derive::ShowInfo;
use enum_map::Enum;
#[cfg(feature = "client")]
use get_size::GetSize;
use log::{info, warn};
use strum::EnumIter;

use itertools::Itertools;

use noise::{NoiseFn, Simplex};

use crate::{
    TICKS_PER_SECOND_LOGIC,
    app_state::{InstantiateInserterError, SimulationState},
    belt::{
        BeltBeltInserterAdditionInfo, BeltTileId, SplitterTileId,
        splitter::{SPLITTER_BELT_LEN, SplitterDistributionMode, SplitterSide},
    },
    data::{AllowedFluidDirection, DataStore, ItemRecipeDir},
    inserter::{
        HAND_SIZE, MOVETIME, StaticID, Storage, storage_storage_with_buckets::InserterIdentifier,
    },
    item::{IdxTrait, Item, Recipe, WeakIdxTrait, usize_from},
    liquid::{
        CannotMixFluidsError, FluidConnectionDir,
        connection_logic::can_fluid_tanks_connect_to_single_connection,
    },
    network_graph::WeakIndex,
    power::power_grid::{BeaconAffectedEntity, PowerGridEntity, PowerGridIdentifier},
};
use crate::{inserter::FakeUnionStorage, item::Indexable};
use static_assertions::const_assert;

use std::fmt::Debug;
use std::ops::{Deref, DerefMut};

use serde::Deserializer;
use serde::Serializer;

use std::iter;

use noise::Seedable;
use petgraph::prelude::Bfs;

use super::{Position, sparse_grid::perfect_grid::PerfectGrid};
use crate::liquid::FluidSystemId;

pub const BELT_LEN_PER_TILE: u16 = 4;

pub const CHUNK_SIZE: u16 = 16;
pub const CHUNK_SIZE_FLOAT: f32 = 16.0;

#[cfg_attr(feature = "client", derive(ShowInfo), derive(GetSize))]
#[derive(Debug, Default, Clone, Copy, PartialEq, serde::Serialize, serde::Deserialize)]
pub enum FloorTile {
    #[default]
    Empty,
    Concrete,
    Water,
}

pub enum InserterInstantiationNewOptions<ItemIdxType: WeakIdxTrait> {
    Positions(Vec<Position>),
    Belts(Vec<BeltTileId<ItemIdxType>>),
    PositionsAndBelts(Vec<Position>, Vec<BeltTileId<ItemIdxType>>),
    All,
}

// We rely on this, by storing entity indices as u8 in the chunk
const_assert!(CHUNK_SIZE * CHUNK_SIZE - 1 <= u8::MAX as u16);

#[cfg_attr(feature = "client", derive(ShowInfo), derive(GetSize))]
#[derive(Debug, Clone, PartialEq, serde::Deserialize, serde::Serialize)]
pub struct Chunk<ItemIdxType: WeakIdxTrait, RecipeIdxType: WeakIdxTrait> {
    base_pos: (i32, i32),
    pub floor_tiles: Option<Box<[[FloorTile; CHUNK_SIZE as usize]; CHUNK_SIZE as usize]>>,
    chunk_tile_to_entity_into: Option<Box<[[u8; CHUNK_SIZE as usize]; CHUNK_SIZE as usize]>>,
    entities: Vec<Entity<ItemIdxType, RecipeIdxType>>,
}

#[cfg_attr(feature = "client", derive(ShowInfo), derive(GetSize))]
#[derive(Debug, Clone, serde::Deserialize, serde::Serialize)]
enum FloorOre<ItemIdxType: WeakIdxTrait> {
    AllSame {
        ore: Item<ItemIdxType>,
        amounts: [[u32; CHUNK_SIZE as usize]; CHUNK_SIZE as usize],
    },
    Mixed {
        // We Box this here to not increase the RAM consumption of chunks which only contain a single type of ore (which are most chunks with ore)
        mixed_ores: Box<[[(Item<ItemIdxType>, u32); CHUNK_SIZE as usize]; CHUNK_SIZE as usize]>,
    },
}

fn is_default<T: Default + PartialEq>(val: &T) -> bool {
    *val == T::default()
}

#[cfg_attr(feature = "client", derive(ShowInfo), derive(GetSize))]
#[derive(Debug, Clone, serde::Deserialize, serde::Serialize)]
pub struct PlayerInfo {
    pub pos: (f32, f32),
    pub visible: bool,
    pub movement_speed: f32,

    inventory: (),
}

impl Default for PlayerInfo {
    fn default() -> Self {
        Self {
            pos: (100.0 * CHUNK_SIZE_FLOAT, 100.0 * CHUNK_SIZE_FLOAT),
            visible: false,
            movement_speed: 10.0 / (TICKS_PER_SECOND_LOGIC as f32),
            inventory: Default::default(),
        }
    }
}

#[cfg_attr(feature = "client", derive(ShowInfo), derive(GetSize))]
#[derive(Debug, Clone, serde::Deserialize, serde::Serialize)]
struct Assembler<RecipeIdxType: WeakIdxTrait> {
    /// List of all the module slots of this assembler
    modules: ModuleSlots,
    info: AssemblerInfo<RecipeIdxType>,
}

#[cfg_attr(feature = "client", derive(ShowInfo), derive(GetSize))]
#[derive(Debug, Clone, serde::Deserialize, serde::Serialize)]
pub struct World<ItemIdxType: WeakIdxTrait, RecipeIdxType: WeakIdxTrait> {
    noise: SerializableSimplex,

    // TODO: I don´t think I want FP
    pub players: Vec<PlayerInfo>,
    chunks: PerfectGrid<i32, Chunk<ItemIdxType, RecipeIdxType>>,

    belt_lookup: BeltIdLookup<ItemIdxType>,
    belt_recieving_input_directions: HashMap<Position, EnumMap<Dir, bool>>,
    power_grid_lookup: PowerGridConnectedDevicesLookup,

    remaining_updates: Vec<WorldUpdate>,

    pub to_instantiate: BTreeSet<Position>,
    pub to_instantiate_by_belt: HashMap<BeltTileId<ItemIdxType>, BTreeSet<Position>>,

    #[serde(skip)]
    pub map_updates: Option<Vec<Position>>,
}

#[derive(Debug, Clone)]
struct SerializableSimplex {
    inner: Simplex,
}

#[cfg(feature = "client")]
impl GetSize for SerializableSimplex {}

#[cfg(feature = "client")]
impl<E: InfoExtractor<Self, Info>, Info: EguiDisplayable> ShowInfo<E, Info>
    for SerializableSimplex
{
    fn show_fields<C: egui_show_info::Cache<String, Info>>(
        &self,
        _extractor: &mut E,
        _ui: &mut egui::Ui,
        _path: String,
        _cache: &mut C,
    ) {
    }
}

impl Deref for SerializableSimplex {
    type Target = Simplex;
    fn deref(&self) -> &Self::Target {
        &self.inner
    }
}

impl DerefMut for SerializableSimplex {
    // type Target = Simplex;
    fn deref_mut(&mut self) -> &mut Self::Target {
        &mut self.inner
    }
}

impl serde::Serialize for SerializableSimplex {
    fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
    where
        S: Serializer,
    {
        serializer.serialize_u32(self.inner.seed())
    }
}

impl<'de> serde::Deserialize<'de> for SerializableSimplex {
    fn deserialize<D>(deserializer: D) -> Result<Self, D::Error>
    where
        D: Deserializer<'de>,
    {
        let seed = u32::deserialize(deserializer)?;
        Ok(Self {
            inner: Simplex::new(seed),
        })
    }
}

#[cfg_attr(feature = "client", derive(ShowInfo), derive(GetSize))]
#[derive(Debug, Clone, serde::Deserialize, serde::Serialize)]
enum WorldUpdate {
    EntityNewlyPowered { pos: Position },
    NewEntity { pos: Position },
}

#[cfg_attr(feature = "client", derive(ShowInfo), derive(GetSize))]
#[derive(Debug, Clone, serde::Deserialize, serde::Serialize)]
struct PowerGridConnectedDevicesLookup {
    grid_to_chunks: BTreeMap<PowerGridIdentifier, BTreeSet<(i32, i32)>>,
}

#[cfg_attr(feature = "client", derive(ShowInfo), derive(GetSize))]
#[derive(Debug, Clone, serde::Deserialize, serde::Serialize)]
struct BeltIdLookup<ItemIdxType: WeakIdxTrait> {
    belt_id_to_chunks: BTreeMap<BeltTileId<ItemIdxType>, BTreeSet<(i32, i32)>>,
}

#[derive(Debug, Clone, Copy)]
pub struct ChunkMissingError;

#[derive(Debug, Clone, Copy)]
enum AddEntityError {
    ChunkMissingError(ChunkMissingError),
}

struct CascadingUpdate<ItemIdxType: WeakIdxTrait, RecipeIdxType: WeakIdxTrait> {
    update: Box<
        dyn FnOnce(
            &mut World<ItemIdxType, RecipeIdxType>,
            &mut SimulationState<ItemIdxType, RecipeIdxType>,
            &mut Vec<CascadingUpdate<ItemIdxType, RecipeIdxType>>,
            &DataStore<ItemIdxType, RecipeIdxType>,
        ) -> (),
    >,
}

fn try_attaching_fluids<ItemIdxType: IdxTrait, RecipeIdxType: IdxTrait>(
    new_assembler_pos: Position,
) -> CascadingUpdate<ItemIdxType, RecipeIdxType> {
    CascadingUpdate {
        update: Box::new(move |world, sim_state, updates, data_store| {
            profiling::scope!("try_attaching_fluids");
            let Some(
                e @ Entity::Assembler {
                    ty: assembler_ty,
                    pos: assembler_pos,
                    info:
                        AssemblerInfo::Powered {
                            id,
                            pole_position,
                            weak_index,
                        },
                    rotation: assembler_rotation,
                    ..
                },
            ) = world.get_entity_at(new_assembler_pos, data_store)
            else {
                return;
            };

            assert_eq!(new_assembler_pos, *assembler_pos);

            let e_size: [u16; 2] = e.get_entity_size(data_store).into();

            world
                .get_entities_colliding_with(
                    Position {
                        x: assembler_pos.x - 1,
                        y: assembler_pos.y - 1,
                    },
                    (e_size[0] + 2, e_size[1] + 2),
                    data_store,
                )
                .into_iter()
                .for_each(|e| match e {
                    Entity::FluidTank {
                        ty, pos, rotation, ..
                    } => {
                        let assembler_size = e_size.into();

                        let recipe_fluid_inputs: Vec<_> = data_store.recipe_to_items[&id.recipe]
                            .iter()
                            .filter_map(|(dir, item)| {
                                (*dir == ItemRecipeDir::Ing
                                    && data_store.item_is_fluid[item.into_usize()])
                                .then_some(*item)
                            })
                            .collect();
                        let recipe_fluid_outputs: Vec<_> = data_store.recipe_to_items[&id.recipe]
                            .iter()
                            .filter_map(|(dir, item)| {
                                (*dir == ItemRecipeDir::Out
                                    && data_store.item_is_fluid[item.into_usize()])
                                .then_some(*item)
                            })
                            .collect();

                        let fluid_pure_outputs: Vec<_> = data_store.assembler_info
                            [usize::from(*assembler_ty)]
                        .fluid_connections
                        .iter()
                        .filter(|(_conn, allowed)| {
                            *allowed == AllowedFluidDirection::Single(ItemRecipeDir::Out)
                                || matches!(*allowed, AllowedFluidDirection::Both { .. })
                        })
                        .collect();

                        let fluid_pure_inputs: Vec<_> = data_store.assembler_info
                            [usize::from(*assembler_ty)]
                        .fluid_connections
                        .iter()
                        .filter(|(_conn, allowed)| {
                            *allowed == AllowedFluidDirection::Single(ItemRecipeDir::Ing)
                                || matches!(*allowed, AllowedFluidDirection::Both { .. })
                        })
                        .collect();

                        // FIXME: FINISH IMPLEMENTING THIS

                        let all_connections_with_items = recipe_fluid_inputs
                            .into_iter()
                            .cycle()
                            .zip(fluid_pure_inputs)
                            .zip(iter::repeat(FluidConnectionDir::Output))
                            .chain(
                                recipe_fluid_outputs
                                    .into_iter()
                                    .cycle()
                                    .zip(fluid_pure_outputs)
                                    .zip(iter::repeat(FluidConnectionDir::Input)),
                            );

                        let in_out_connections = all_connections_with_items.filter_map(
                            move |((item, (fluid_conn, _allowed)), fluid_dir)| {
                                can_fluid_tanks_connect_to_single_connection(
                                    *pos,
                                    *ty,
                                    *rotation,
                                    *assembler_pos,
                                    *fluid_conn,
                                    *assembler_rotation,
                                    assembler_size,
                                    data_store,
                                )
                                .map(
                                    |(dest_conn, dest_conn_dir)| {
                                        (
                                            fluid_dir,
                                            item,
                                            Storage::Assembler {
                                                grid: id.grid,
                                                index: id.assembler_index,
                                                recipe_idx_with_this_item: data_store
                                                    .recipe_to_translated_index[&(id.recipe, item)],
                                            },
                                            dest_conn,
                                            Box::new(|_weak_index: WeakIndex| {})
                                                as Box<dyn FnOnce(WeakIndex) -> ()>,
                                        )
                                    },
                                )
                            },
                        );
                        for (dir, conn_fluid, conn_storage, conn_pos, cb) in in_out_connections {
                            let res = match dir {
                                FluidConnectionDir::Output => {
                                    sim_state.factory.fluid_store.try_add_output(
                                        *pos,
                                        conn_fluid,
                                        conn_storage,
                                        conn_pos,
                                        &mut sim_state.factory.chests,
                                        &mut sim_state.factory.storage_storage_inserters,
                                        data_store,
                                    )
                                },
                                FluidConnectionDir::Input => {
                                    sim_state.factory.fluid_store.try_add_input(
                                        *pos,
                                        conn_fluid,
                                        conn_storage,
                                        conn_pos,
                                        &mut sim_state.factory.chests,
                                        &mut sim_state.factory.storage_storage_inserters,
                                        data_store,
                                    )
                                },
                            };
                            match res {
                                Ok(weak_index) => cb(weak_index),
                                Err(CannotMixFluidsError { items: fluids }) => {
                                    let fluids = fluids.map(|item| {
                                        &data_store.item_display_names[item.into_usize()]
                                    });
                                    error!("Cannot mix {} and {}", fluids[0], fluids[1]);
                                },
                            }
                        }
                    },
                    _ => {},
                });
        }),
    }
}

fn try_instantiating_inserters_for_belt_cascade<ItemIdxType: IdxTrait, RecipeIdxType: IdxTrait>(
    belt_id: BeltTileId<ItemIdxType>,
) -> CascadingUpdate<ItemIdxType, RecipeIdxType> {
    CascadingUpdate {
        update: Box::new(move |world, sim_state, updates, data_store| {
            profiling::scope!("try_instantiating_inserters_for_belt_cascade");
            let mut reachable = Bfs::new(
                &*sim_state.factory.belts.belt_graph,
                *sim_state.factory.belts.belt_graph_lookup[&belt_id],
            );
            while let Some(idx) = reachable.next(&*sim_state.factory.belts.belt_graph) {
                let belt = sim_state.factory.belts.belt_graph.node_weight(idx).unwrap();
                // FIXME: What if the graph contains cycles???
                updates.push(try_instantiating_inserters_for_belt(*belt));
            }
            // // In order to avoid problems wit
            // updates.push(try_instantiating_all_inserters_cascade());
        }),
    }
}

fn try_instantiating_inserters_for_belt<ItemIdxType: IdxTrait, RecipeIdxType: IdxTrait>(
    belt_id: BeltTileId<ItemIdxType>,
) -> CascadingUpdate<ItemIdxType, RecipeIdxType> {
    CascadingUpdate {
        update: Box::new(move |world, sim_state, updates, data_store| {
            profiling::scope!("try_instantiating_inserters_for_belt");
            if !world.to_instantiate_by_belt.contains_key(&belt_id) {
                return;
            }

            let by_belt = world.to_instantiate_by_belt.get_mut(&belt_id).unwrap();

            if by_belt.len() > 1_000 {
                warn!(
                    "More than 1_000 inserters waiting to be instantiated on belt {:?}: {}. This will cause lag!",
                    belt_id,
                    by_belt.len()
                );
            }

            let mut tmp = BTreeSet::default();

            mem::swap(&mut tmp, &mut *by_belt);

            tmp.retain(|pos| {
                match world.try_instantiate_inserter(sim_state, *pos, data_store) {
                    Ok(newly_instantiated) => {
                        match newly_instantiated {
                            InserterInstantiationNewOptions::Positions(positions) => updates
                                .extend(positions.into_iter().map(|new_pos| {
                                    // FIXME: Size hardcoded
                                    new_possible_inserter_connection(new_pos, (10, 10))
                                })),
                            InserterInstantiationNewOptions::Belts(belts) => {
                                updates.extend(belts.into_iter().map(|new_belt| {
                                    try_instantiating_inserters_for_belt_cascade(new_belt)
                                }))
                            },
                            InserterInstantiationNewOptions::PositionsAndBelts(
                                positions,
                                belts,
                            ) => {
                                updates.extend(positions.into_iter().map(|new_pos| {
                                    // FIXME: Size hardcoded
                                    new_possible_inserter_connection(new_pos, (10, 10))
                                }));
                                updates.extend(belts.into_iter().map(|new_belt| {
                                    try_instantiating_inserters_for_belt_cascade(new_belt)
                                }));
                            },
                            InserterInstantiationNewOptions::All => {
                                updates.push(try_instantiating_all_inserters_cascade())
                            },
                        }

                        false
                    },
                    Err(InstantiateInserterError::NotUnattachedInserter) => false,
                    Err(
                        InstantiateInserterError::ItemConflict {
                            belts_which_could_help,
                        }
                        | InstantiateInserterError::PleaseSpecifyFilter {
                            belts_which_could_help,
                        },
                    ) => {
                        for belt in belts_which_could_help {
                            if belt_id != belt {
                                let by_belt = world.to_instantiate_by_belt.entry(belt).or_default();
                                if !by_belt.contains(pos) {
                                    by_belt.insert(*pos);
                                }
                            }
                        }
                        true
                    },
                    Err(
                        InstantiateInserterError::SourceMissing
                        | InstantiateInserterError::DestMissing,
                    ) => false,
                }
            });

            let by_belt = world.to_instantiate_by_belt.get_mut(&belt_id).unwrap();

            assert!(by_belt.is_empty());

            mem::swap(&mut tmp, &mut *by_belt);
        }),
    }
}

fn try_instantiating_all_inserters_cascade<ItemIdxType: IdxTrait, RecipeIdxType: IdxTrait>()
-> CascadingUpdate<ItemIdxType, RecipeIdxType> {
    CascadingUpdate {
        update: Box::new(|world, sim_state, updates, data_store| {
            profiling::scope!("try_instantiating_all_inserters_cascade");
            if world.to_instantiate.len() > 1_000 {
                warn!(
                    "More than 1_000 inserters waiting to be instantiated: {}. This will cause lag!",
                    world.to_instantiate.len()
                );
            }

            let mut tmp = BTreeSet::default();

            mem::swap(&mut tmp, &mut world.to_instantiate);

            tmp.retain(|pos| {
                match world.try_instantiate_inserter(sim_state, *pos, data_store) {
                    Ok(newly_instantiated) => {
                        match newly_instantiated {
                            InserterInstantiationNewOptions::Positions(positions) => updates
                                .extend(positions.into_iter().map(|new_pos| {
                                    // FIXME: Size hardcoded
                                    new_possible_inserter_connection(new_pos, (10, 10))
                                })),
                            InserterInstantiationNewOptions::Belts(belts) => {
                                updates.extend(belts.into_iter().map(|new_belt| {
                                    try_instantiating_inserters_for_belt_cascade(new_belt)
                                }))
                            },
                            InserterInstantiationNewOptions::PositionsAndBelts(
                                positions,
                                belts,
                            ) => {
                                updates.extend(positions.into_iter().map(|new_pos| {
                                    // FIXME: Size hardcoded
                                    new_possible_inserter_connection(new_pos, (10, 10))
                                }));
                                updates.extend(belts.into_iter().map(|new_belt| {
                                    try_instantiating_inserters_for_belt_cascade(new_belt)
                                }));
                            },
                            InserterInstantiationNewOptions::All => {
                                updates.push(try_instantiating_all_inserters_cascade())
                            },
                        }

                        false
                    },
                    Err(InstantiateInserterError::NotUnattachedInserter) => {
                        warn!("We seem to have instantiated the same inserter twice?!?");
                        false
                    },
                    Err(
                        InstantiateInserterError::ItemConflict {
                            belts_which_could_help,
                        }
                        | InstantiateInserterError::PleaseSpecifyFilter {
                            belts_which_could_help,
                        },
                    ) => {
                        for belt in belts_which_could_help {
                            let by_belt = world.to_instantiate_by_belt.entry(belt).or_default();
                            if !by_belt.contains(pos) {
                                by_belt.insert(*pos);
                            }
                        }
                        true
                    },
                    Err(e) => {
                        info!("try_instantiate_inserter failed at {:?}, with {e:?}", pos);
                        false
                    },
                }
            });

            mem::swap(&mut tmp, &mut world.to_instantiate);
        }),
    }
}

fn instantiate_inserter_cascade<ItemIdxType: IdxTrait, RecipeIdxType: IdxTrait>(
    new_instantiate_pos: Position,
    data_store: &DataStore<ItemIdxType, RecipeIdxType>,
) -> CascadingUpdate<ItemIdxType, RecipeIdxType> {
    CascadingUpdate {
        update: Box::new(move |world, sim_state, updates, data_store| {
            match world.try_instantiate_inserter(sim_state, new_instantiate_pos, data_store) {
                Ok(newly_instantiated) => {
                    match newly_instantiated {
                        InserterInstantiationNewOptions::Positions(positions) => {
                            updates.extend(positions.into_iter().map(|new_pos| {
                                // FIXME: Size hardcoded
                                new_possible_inserter_connection(new_pos, (10, 10))
                            }))
                        },
                        InserterInstantiationNewOptions::Belts(belts) => {
                            updates.extend(belts.into_iter().map(|new_belt| {
                                try_instantiating_inserters_for_belt_cascade(new_belt)
                            }))
                        },
                        InserterInstantiationNewOptions::PositionsAndBelts(positions, belts) => {
                            updates.extend(positions.into_iter().map(|new_pos| {
                                // FIXME: Size hardcoded
                                new_possible_inserter_connection(new_pos, (10, 10))
                            }));
                            updates.extend(belts.into_iter().map(|new_belt| {
                                try_instantiating_inserters_for_belt_cascade(new_belt)
                            }));
                        },
                        InserterInstantiationNewOptions::All => {
                            updates.push(try_instantiating_all_inserters_cascade())
                        },
                    }
                },
                Err(InstantiateInserterError::NotUnattachedInserter) => {
                    warn!("We seem to have instantiated the same inserter twice?!?");
                },
                Err(
                    InstantiateInserterError::ItemConflict {
                        belts_which_could_help,
                    }
                    | InstantiateInserterError::PleaseSpecifyFilter {
                        belts_which_could_help,
                    },
                ) => {
                    world.to_instantiate.insert(new_instantiate_pos);
                    for belt in belts_which_could_help {
                        let by_belt = world.to_instantiate_by_belt.entry(belt).or_default();
                        if !by_belt.contains(&new_instantiate_pos) {
                            by_belt.insert(new_instantiate_pos);
                        }
                    }
                },
                Err(e) => {
                    info!(
                        "try_instantiate_inserter failed at {:?}, with {e:?}",
                        new_instantiate_pos
                    );
                    world.to_instantiate.insert(new_instantiate_pos);
                },
            }
        }),
    }
}

fn new_possible_inserter_connection<ItemIdxType: IdxTrait, RecipeIdxType: IdxTrait>(
    pos: Position,
    size: (u16, u16),
) -> CascadingUpdate<ItemIdxType, RecipeIdxType> {
    CascadingUpdate {
        update: Box::new(move |world, _sim_state, updates, data_store| {
            profiling::scope!("new_possible_inserter_connection");
            let inserter_search_start_pos = Position {
                x: pos.x - data_store.max_inserter_search_range as i32,
                y: pos.y - data_store.max_inserter_search_range as i32,
            };

            let inserter_search_size = (
                2 * data_store.max_inserter_search_range as u16 + size.0,
                2 * data_store.max_inserter_search_range as u16 + size.1,
            );

            world.mutate_entities_colliding_with(
                inserter_search_start_pos,
                inserter_search_size,
                data_store,
                |e| {
                    match e {
                        Entity::Inserter {
                            pos: inserter_pos,
                            direction: _inserter_dir,
                            filter: _inserter_filter,
                            info: InserterInfo::NotAttached { start_pos, end_pos },
                            ..
                        } => {
                            if start_pos.contained_in(pos, size) || end_pos.contained_in(pos, size)
                            {
                                updates
                                    .push(instantiate_inserter_cascade(*inserter_pos, data_store));
                            }
                        },
                        _ => {},
                    }
                    ControlFlow::Continue(())
                },
            );
        }),
    }
}

fn new_lab_cascade<ItemIdxType: IdxTrait, RecipeIdxType: IdxTrait>(
    pos: Position,
    _data_store: &DataStore<ItemIdxType, RecipeIdxType>,
) -> CascadingUpdate<ItemIdxType, RecipeIdxType> {
    CascadingUpdate {
        update: Box::new(move |world, sim_state, updates, data_store| {
            profiling::scope!("new_lab_cascade");
            let Some(Entity::Lab {
                pos,
                ty,
                modules,
                pole_position: Some((pole_pos, weak_index, index)),
            }) = world.get_entity_at(pos, data_store)
            else {
                warn!("Lab missing in new lab cascade");
                return;
            };

            let pos = *pos;
            let size = data_store.lab_info[usize::from(*ty)].size;
            let pole_pos = *pole_pos;
            let index = *index;

            updates.push(new_possible_inserter_connection(pos, size));

            let beacon_search_start_pos = Position {
                x: pos.x - data_store.max_beacon_range.0 as i32,
                y: pos.y - data_store.max_beacon_range.1 as i32,
            };

            let beacon_search_size = (
                2 * data_store.max_beacon_range.0 as u16 + size.0,
                2 * data_store.max_beacon_range.1 as u16 + size.1,
            );

            for entity in world.get_entities_colliding_with(
                beacon_search_start_pos,
                beacon_search_size,
                data_store,
            ) {
                match entity {
                    Entity::Beacon {
                        ty: beacon_ty,
                        pos: beacon_pos,
                        modules: beacon_modules,
                        pole_position: Some((beacon_pole_pos, beacon_weak_idx)),
                    } => {
                        let (beacon_range_x, beacon_range_y) =
                            data_store.beacon_info[usize::from(*beacon_ty)].effect_range;

                        let (beacon_offs_x, beacon_offs_y) =
                            ((beacon_range_x - size.0) / 2, (beacon_range_y - size.1) / 2);

                        if pos.overlap(
                            size,
                            Position {
                                x: beacon_pos.x - beacon_offs_x as i32,
                                y: beacon_pos.y - beacon_offs_y as i32,
                            },
                            (beacon_range_x, beacon_range_y),
                        ) {
                            sim_state.factory.power_grids.add_beacon_affected_entity(
                                *beacon_pole_pos,
                                *beacon_weak_idx,
                                BeaconAffectedEntity::Lab {
                                    grid: sim_state.factory.power_grids.pole_pos_to_grid_id
                                        [&pole_pos],
                                    index: index.try_into().unwrap(),
                                },
                                data_store,
                            );
                        }
                    },
                    _ => {},
                }
            }
        }),
    }
}

fn new_power_pole<ItemIdxType: IdxTrait, RecipeIdxType: IdxTrait>(
    pos: Position,
    data_store: &DataStore<ItemIdxType, RecipeIdxType>,
) -> CascadingUpdate<ItemIdxType, RecipeIdxType> {
    CascadingUpdate {
        update: Box::new(move |world, sim_state, updates, data_store| {
            profiling::scope!("new_power_pole");
            let pole = world.get_entity_at(pos, data_store);

            if let Some(Entity::PowerPole {
                ty, pos: pole_pos, ..
            }) = pole
            {
                let pole_pos = *pole_pos;

                let grid_id = sim_state.factory.power_grids.pole_pos_to_grid_id[&pole_pos];

                let pole_size = data_store.power_pole_data[usize::from(*ty)].size;
                let pole_power_range = data_store.power_pole_data[usize::from(*ty)].power_range;

                world.mutate_entities_colliding_with(
                    Position {
                        x: pole_pos.x - pole_power_range as i32,
                        y: pole_pos.y - pole_power_range as i32,
                    },
                    (
                        (pole_power_range as u16) * 2 + pole_size.0,
                        (pole_power_range as u16) * 2 + pole_size.1,
                    ),
                    data_store,
                    |e| {
                        match e {
                            Entity::Assembler {
                                ty,
                                pos,
                                modules,
                                info: info @ AssemblerInfo::Unpowered(_),
                                ..
                            } => {
                                let AssemblerInfo::Unpowered(recipe) = info else {
                                    unreachable!();
                                };

                                let (new_id, weak_index) =
                                    sim_state.factory.power_grids.power_grids[usize::from(grid_id)]
                                        .add_assembler(
                                            *ty, grid_id, *recipe, &**modules, pole_pos, *pos,
                                            data_store,
                                        );

                                *info = AssemblerInfo::Powered {
                                    id: new_id,
                                    pole_position: pole_pos,
                                    weak_index,
                                };

                                updates.push(newly_working_assembler(*pos, data_store));
                            },
                            Entity::Assembler {
                                info: info @ AssemblerInfo::UnpoweredNoRecipe,
                                ..
                            } => {
                                *info = AssemblerInfo::PoweredNoRecipe(pole_pos);
                            },
                            Entity::Roboport {
                                ty,
                                pos,
                                power_grid,
                                network,
                                id,
                            } => todo!(),
                            Entity::SolarPanel {
                                pos,
                                ty,
                                pole_position: pole_position @ None,
                            } => {
                                let weak_index = sim_state.factory.power_grids.power_grids
                                    [usize::from(grid_id)]
                                .add_solar_panel(*pos, *ty, pole_pos, data_store);

                                *pole_position = Some((pole_pos, weak_index));
                            },
                            Entity::Lab {
                                pos,
                                ty,
                                modules,
                                pole_position: pole_position @ None,
                            } => {
                                let (weak_index, index) = sim_state.factory.power_grids.power_grids
                                    [usize::from(grid_id)]
                                .add_lab(*pos, *ty, &**modules, pole_pos, data_store);

                                *pole_position = Some((pole_pos, weak_index, index));

                                updates.push(new_lab_cascade(*pos, data_store));
                            },
                            Entity::Beacon {
                                ty,
                                pos,
                                modules,
                                pole_position: pole_position @ None,
                            } => {
                                let weak_index = sim_state.factory.power_grids.power_grids
                                    [usize::from(grid_id)]
                                .add_beacon(
                                    *ty,
                                    *pos,
                                    pole_pos,
                                    modules.clone(),
                                    vec![],
                                    data_store,
                                );

                                *pole_position = Some((pole_pos, weak_index));

                                updates.push(new_powered_beacon_cascade(*pos, data_store));
                            },
                            e => {
                                // warn!("Entity {e:?} cannot accept power in start_powering_entity")
                            },
                        }
                        ControlFlow::Continue(())
                    },
                );
            } else {
                warn!("Power pole disappeared, while new_power_pole was in the queue");
            }
        }),
    }
}

fn new_chest_cascade<ItemIdxType: IdxTrait, RecipeIdxType: IdxTrait>(
    pos: Position,
) -> CascadingUpdate<ItemIdxType, RecipeIdxType> {
    CascadingUpdate {
        update: Box::new(move |world, sim_state, updates, data_store| {
            profiling::scope!("new_chest_cascade");
            let Some(Entity::Chest {
                ty,
                pos,
                item: _,
                slot_limit: _,
            }) = world.get_entity_at(pos, data_store)
            else {
                return;
            };

            let size = data_store.chest_tile_sizes[usize::from(*ty)];

            updates.push(new_possible_inserter_connection(*pos, size));
        }),
    }
}

fn new_powered_beacon_cascade<ItemIdxType: IdxTrait, RecipeIdxType: IdxTrait>(
    pos: Position,
    data_store: &DataStore<ItemIdxType, RecipeIdxType>,
) -> CascadingUpdate<ItemIdxType, RecipeIdxType> {
    CascadingUpdate {
        update: Box::new(move |world, sim_state, _updates, data_store| {
            profiling::scope!("new_powered_beacon_cascade");
            let Some(Entity::Beacon {
                ty,
                pos,
                modules: _,
                pole_position: Some((pole_pos, weak_idx)),
            }) = world.get_entity_at(pos, data_store)
            else {
                return;
            };

            let size = data_store.beacon_info[usize::from(*ty)].size;
            let range = data_store.beacon_info[usize::from(*ty)].effect_range;

            let beacon_search_size = range;

            for affected_entity in world.get_entities_colliding_with(
                Position {
                    x: pos.x - (range.0 as i32 - size.0 as i32) / 2,
                    y: pos.y - (range.1 as i32 - size.1 as i32) / 2,
                },
                beacon_search_size,
                data_store,
            ) {
                match affected_entity {
                    Entity::Assembler {
                        info:
                            AssemblerInfo::Powered {
                                id,
                                pole_position: _,
                                weak_index: _,
                            },
                        ..
                    } => {
                        sim_state.factory.power_grids.add_beacon_affected_entity(
                            *pole_pos,
                            *weak_idx,
                            BeaconAffectedEntity::Assembler { id: *id },
                            data_store,
                        );
                    },
                    Entity::Lab {
                        pos: _,
                        ty: _,
                        modules: _,
                        pole_position: Some((lab_pole_pos, _lab_weak_idx, lab_idx)),
                    } => {
                        sim_state.factory.power_grids.add_beacon_affected_entity(
                            *pole_pos,
                            *weak_idx,
                            BeaconAffectedEntity::Lab {
                                grid: sim_state.factory.power_grids.pole_pos_to_grid_id
                                    [lab_pole_pos],
                                index: (*lab_idx).into(),
                            },
                            data_store,
                        );
                    },
                    _ => {},
                }
            }
        }),
    }
}

fn newly_working_assembler<ItemIdxType: IdxTrait, RecipeIdxType: IdxTrait>(
    pos: Position,
    data_store: &DataStore<ItemIdxType, RecipeIdxType>,
) -> CascadingUpdate<ItemIdxType, RecipeIdxType> {
    CascadingUpdate {
        update: Box::new(move |world, sim_state, updates, data_store| {
            profiling::scope!("newly_working_assembler");
            let Some(
                e @ Entity::Assembler {
                    ty,
                    pos,
                    modules: _,
                    info:
                        AssemblerInfo::Powered {
                            id,
                            pole_position: _,
                            weak_index: _,
                        },
                    rotation,
                },
            ) = world.get_entity_at(pos, data_store)
            else {
                warn!("Assembler missing in new assembler cascade");
                return;
            };

            let pos = *pos;
            let size = e.get_entity_size(data_store);
            let id = *id;

            updates.push(new_possible_inserter_connection(pos, size));
            updates.push(try_attaching_fluids(pos));

            let beacon_search_start_pos = Position {
                x: pos.x - data_store.max_beacon_range.0 as i32,
                y: pos.y - data_store.max_beacon_range.1 as i32,
            };

            let beacon_search_size = (
                2 * data_store.max_beacon_range.0 as u16 + size.0,
                2 * data_store.max_beacon_range.1 as u16 + size.1,
            );

            for entity in world.get_entities_colliding_with(
                beacon_search_start_pos,
                beacon_search_size,
                data_store,
            ) {
                match entity {
                    Entity::Beacon {
                        ty: beacon_ty,
                        pos: beacon_pos,
                        modules: _,
                        pole_position: Some((beacon_pole_pos, beacon_weak_idx)),
                    } => {
                        let (beacon_range_x, beacon_range_y) =
                            data_store.beacon_info[usize::from(*beacon_ty)].effect_range;

                        let (beacon_offs_x, beacon_offs_y) =
                            ((beacon_range_x - size.0) / 2, (beacon_range_y - size.1) / 2);

                        if pos.overlap(
                            size,
                            Position {
                                x: beacon_pos.x - beacon_offs_x as i32,
                                y: beacon_pos.y - beacon_offs_y as i32,
                            },
                            (beacon_range_x, beacon_range_y),
                        ) {
                            sim_state.factory.power_grids.add_beacon_affected_entity(
                                *beacon_pole_pos,
                                *beacon_weak_idx,
                                BeaconAffectedEntity::Assembler { id },
                                data_store,
                            );
                        }
                    },
                    _ => {},
                }
            }
        }),
    }
}

fn removal_of_possible_inserter_connection<ItemIdxType: IdxTrait, RecipeIdxType: IdxTrait>(
    pos: Position,
    size: (u16, u16),
    data_store: &DataStore<ItemIdxType, RecipeIdxType>,
) -> CascadingUpdate<ItemIdxType, RecipeIdxType> {
    CascadingUpdate {
        update: Box::new(move |world, sim_state, updates, data_store| {
            profiling::scope!("removal_of_possible_inserter_connection");
            let inserter_search_start_pos = Position {
                x: pos.x - data_store.max_inserter_search_range as i32,
                y: pos.y - data_store.max_inserter_search_range as i32,
            };

            let inserter_search_size = (
                2 * data_store.max_inserter_search_range as u16 + size.0,
                2 * data_store.max_inserter_search_range as u16 + size.1,
            );

            world.mutate_entities_colliding_with(
                inserter_search_start_pos,
                inserter_search_size,
                data_store,
                |e| {
                    match e {
                        Entity::Inserter {
                            ty,
                            user_movetime,
                            type_movetime,

                            pos: inserter_pos,
                            direction: _inserter_dir,
                            filter: _inserter_filter,
                            info,
                        } => {
                            if let InserterInfo::Attached {
                                start_pos,
                                end_pos,
                                info: attached_inserter,
                            } = info
                            {
                                if start_pos.contained_in(pos, size)
                                    || end_pos.contained_in(pos, size)
                                {
                                    match attached_inserter {
                                        AttachedInserter::BeltStorage { id, belt_pos } => {
                                            sim_state.factory.belts.remove_inserter(*id, *belt_pos);

                                            *info = InserterInfo::NotAttached {
                                                start_pos: *start_pos,
                                                end_pos: *end_pos,
                                            };
                                        },
                                        AttachedInserter::BeltBelt { item, inserter } => {
                                            todo!("Remove BeltBelt inserter");
                                        },
                                        AttachedInserter::StorageStorage { item, inserter } => {
                                            let movetime = user_movetime
                                                .map(|v| v.into())
                                                .unwrap_or(*type_movetime);

                                            // This might return something at some point, and this will be a compiler error
                                            let () = sim_state
                                                .factory
                                                .storage_storage_inserters
                                                .remove_ins(*item, movetime.into(), *inserter);

                                            *info = InserterInfo::NotAttached {
                                                start_pos: *start_pos,
                                                end_pos: *end_pos,
                                            };
                                        },
                                    }
                                }
                            }
                        },
                        _ => {},
                    }
                    ControlFlow::Continue(())
                },
            );
        }),
    }
}

const ORE_THRESHHOLD: f64 = 0.95;
const ORE_DISTANCE_MULT: f64 = 1.0 / 80.0;

enum ChunkTileState {
    Index(usize),
    Empty,
    OtherChunk,
    EmptyOrOtherChunk,
}

const EMPTY: u8 = 254;
const OTHER_CHUNK: u8 = 255;
const EMPTY_OR_OTHER_CHUNK: u8 = 255;

fn arr_val_to_state(entities_len: usize, value: u8) -> ChunkTileState {
    match value {
        v if usize::from(v) < entities_len => ChunkTileState::Index(usize::from(v)),
        EMPTY_OR_OTHER_CHUNK if entities_len == usize::from(min(EMPTY, OTHER_CHUNK) + 1) => {
            ChunkTileState::EmptyOrOtherChunk
        },

        OTHER_CHUNK => ChunkTileState::OtherChunk,
        EMPTY => ChunkTileState::Empty,
        _ => unreachable!(),
    }
}

impl<ItemIdxType: WeakIdxTrait, RecipeIdxType: WeakIdxTrait> GetGridIndex<i32>
    for Chunk<ItemIdxType, RecipeIdxType>
{
    fn get_grid_index(&self) -> (i32, i32) {
        (
            self.base_pos.0 / i32::from(CHUNK_SIZE),
            self.base_pos.1 / i32::from(CHUNK_SIZE),
        )
    }
}

impl<ItemIdxType: IdxTrait, RecipeIdxType: IdxTrait> World<ItemIdxType, RecipeIdxType> {
    #[must_use]
    pub fn new_with_starting_area(center: Position, width_chunks: u16, height_chunks: u16) -> Self {
        let mut grid = PerfectGrid::new();

        let noise = Simplex::new(1);

        let positions = (0..i32::from(width_chunks))
            .cartesian_product(0..i32::from(height_chunks))
            .map(|(x, y)| {
                (
                    x - i32::from(width_chunks) / 2,
                    y - i32::from(height_chunks) / 2,
                )
            })
            .map(|(x, y)| {
                (
                    center.x / i32::from(CHUNK_SIZE) + x,
                    center.y / i32::from(CHUNK_SIZE) + y,
                )
            })
            .collect_vec();
        let chunks = positions.iter().copied().map(|(x, y)| Chunk {
            base_pos: (x * i32::from(CHUNK_SIZE), y * i32::from(CHUNK_SIZE)),
            floor_tiles: None,
            chunk_tile_to_entity_into: None,
            entities: vec![],
        });
        grid.insert_many(positions.iter().copied(), chunks);

        Self {
            noise: SerializableSimplex { inner: noise },
            chunks: grid,
            players: vec![PlayerInfo::default(), PlayerInfo::default()],

            belt_lookup: BeltIdLookup {
                belt_id_to_chunks: BTreeMap::new(),
            },
            belt_recieving_input_directions: HashMap::new(),
            power_grid_lookup: PowerGridConnectedDevicesLookup {
                grid_to_chunks: BTreeMap::new(),
            },

            remaining_updates: vec![],

            to_instantiate: BTreeSet::default(),
            to_instantiate_by_belt: HashMap::default(),

            map_updates: None,
        }
    }

    pub fn new_with_area(top_left: Position, bottom_right: Position) -> Self {
        Self::new_with_starting_area(
            Position {
                x: top_left.x + (bottom_right.x - top_left.x) / 2,
                y: top_left.y + (bottom_right.y - top_left.y) / 2,
            },
            (top_left
                .x
                .abs_diff(bottom_right.x)
                .div_ceil(u32::from(CHUNK_SIZE)))
            .try_into()
            .unwrap(),
            (top_left
                .y
                .abs_diff(bottom_right.y)
                .div_ceil(u32::from(CHUNK_SIZE)))
            .try_into()
            .unwrap(),
        )
    }

    pub fn get_original_ore_at_pos(&self, pos: Position) -> Option<(Item<ItemIdxType>, u32)> {
        let v = self.noise.get([
            pos.x as f64 * ORE_DISTANCE_MULT,
            pos.y as f64 * ORE_DISTANCE_MULT,
        ]);

        (v > ORE_THRESHHOLD).then_some((
            Item {
                id: ItemIdxType::try_from(0).unwrap(),
            },
            (v * pos.x.abs() as f64 * pos.y.abs() as f64 / 1000.0) as u32,
        ))
    }

    pub fn get_original_ore_in_area(
        &self,
        pos: Position,
        size: [u16; 2],
    ) -> Vec<(Item<ItemIdxType>, u32)> {
        (pos.x..(pos.x + size[0] as i32))
            .cartesian_product(pos.y..(pos.y + size[1] as i32))
            .flat_map(|(x, y)| self.get_original_ore_at_pos(Position { x, y }))
            .into_group_map()
            .into_iter()
            // FIXME: This will panic if ore patches are too rich
            .flat_map(|(item, amounts)| {
                let sum = amounts.iter().sum();
                (sum > 0).then_some((item, sum))
            })
            // CORRECTNESS: We need to sort in some way, since HashMap::into_iter() does not guarantee any order, which is needed for this to be deterministic
            .sorted_by_key(|(_, amount)| *amount)
            .collect()
    }

    pub fn get_belt_possible_inputs(&mut self, pos: Position) -> &EnumMap<Dir, bool> {
        self.belt_recieving_input_directions.entry(pos).or_default()
    }

    pub fn get_belt_possible_inputs_no_cache(&self, pos: Position) -> EnumMap<Dir, bool> {
        self.belt_recieving_input_directions
            .get(&pos)
            .copied()
            .unwrap_or_default()
    }

    pub fn get_chunks(&self) -> impl Iterator<Item = &Chunk<ItemIdxType, RecipeIdxType>> + Send {
        self.chunks.occupied_entries().map(|(_, chunk)| chunk)
    }

    pub fn get_chunk(
        &self,
        chunk_x: i32,
        chunk_y: i32,
    ) -> Option<&Chunk<ItemIdxType, RecipeIdxType>> {
        self.chunks.get(chunk_x, chunk_y)
    }

    pub fn extent_chunk_positions(&self) -> [Range<i32>; 2] {
        match self.chunks.get_extent() {
            Some(ranges) => ranges.map(|range| *range.start()..(*range.end() + 1)),
            None => Default::default(),
        }
    }

    pub fn set_floor_tile(&mut self, pos: Position, floor_tile: FloorTile) -> Result<(), ()> {
        if let Some(chunk) = self.get_chunk_for_tile_mut(pos) {
            chunk.floor_tiles.get_or_insert_default()
                [usize::try_from(pos.x.rem_euclid(i32::from(CHUNK_SIZE))).unwrap()]
                [usize::try_from(pos.y.rem_euclid(i32::from(CHUNK_SIZE))).unwrap()] = floor_tile;
            Ok(())
        } else {
            Err(())
        }
    }

    #[profiling::function]
    pub fn change_assembler_recipe(
        &mut self,
        sim_state: &mut SimulationState<ItemIdxType, RecipeIdxType>,
        pos: Position,
        new_recipe: Recipe<RecipeIdxType>,
        data_store: &DataStore<ItemIdxType, RecipeIdxType>,
    ) {
        let can_accept_prod = data_store.recipe_is_intermediate[new_recipe.into_usize()];

        // FIXME: Remove Cheated modules
        let cheated_modules = if can_accept_prod { Some(1) } else { Some(0) };

        let mut cascading_updates = vec![];

        self.get_entity_at_mut(pos, data_store).map(|e| {
            match e {
                Entity::Assembler {
                    ty,
                    pos,
                    modules,
                    info:
                        AssemblerInfo::Powered {
                            id,
                            pole_position,
                            weak_index,
                        },
                    rotation,
                } => {
                    let assembler_size = data_store.assembler_info[*ty as usize].size(*rotation);

                    for module_slot in modules.iter_mut() {
                        *module_slot = cheated_modules;
                    }

                    if id.recipe == new_recipe {
                        return;
                    }

                    // Change assembler recipe
                    let (removal_info, new_id) = sim_state.factory.power_grids.power_grids
                        [usize::from(id.grid)]
                    .change_assembler_recipe(
                        *id,
                        *pole_position,
                        *weak_index,
                        new_recipe,
                        data_store,
                    );

                    *id = new_id;

                    // CORRECTNESS: Since we process updates in a LIFO order, and we push the reconnection first, we will disconnect first

                    // Push trying to reconnect all connected inserters (if possible)
                    cascading_updates.push(new_possible_inserter_connection(*pos, assembler_size));

                    // Push disconnecting all connected inserters
                    cascading_updates.push(removal_of_possible_inserter_connection(
                        *pos,
                        assembler_size,
                        data_store,
                    ));
                },
                Entity::Assembler {
                    ty,
                    pos,
                    modules,
                    info: info @ AssemblerInfo::PoweredNoRecipe(_),
                    rotation,
                } => {
                    for module_slot in modules.iter_mut() {
                        *module_slot = cheated_modules;
                    }

                    let AssemblerInfo::PoweredNoRecipe(pole_position) = info else {
                        unreachable!();
                    };
                    let grid_id = sim_state.factory.power_grids.pole_pos_to_grid_id[pole_position];
                    let (new_id, new_weak_idx) = sim_state.factory.power_grids.power_grids
                        [usize::from(grid_id)]
                    .add_assembler(
                        *ty,
                        grid_id,
                        new_recipe,
                        &**modules,
                        *pole_position,
                        *pos,
                        data_store,
                    );
                    *info = AssemblerInfo::Powered {
                        id: new_id,
                        pole_position: *pole_position,
                        weak_index: new_weak_idx,
                    };
                    cascading_updates.push(newly_working_assembler(*pos, data_store));
                },
                Entity::Assembler {
                    modules,
                    info: AssemblerInfo::Unpowered(recipe),
                    ..
                } => {
                    for module_slot in modules.iter_mut() {
                        *module_slot = cheated_modules;
                    }
                    *recipe = new_recipe;
                },
                Entity::Assembler {
                    modules,
                    info: info @ AssemblerInfo::UnpoweredNoRecipe,
                    ..
                } => {
                    for module_slot in modules.iter_mut() {
                        *module_slot = cheated_modules;
                    }
                    *info = AssemblerInfo::Unpowered(new_recipe);
                },
                e => unreachable!("Called change recipe on non assembler: {e:?}"),
            }
        });

        // CORRECTNESS: We rely on the updates being processed in a LIFO order!
        while let Some(update) = cascading_updates.pop() {
            (update.update)(self, sim_state, &mut cascading_updates, data_store);
        }
    }

    #[profiling::function]
    pub fn add_entity(
        &mut self,
        entity: Entity<ItemIdxType, RecipeIdxType>,
        sim_state: &mut SimulationState<ItemIdxType, RecipeIdxType>,
        data_store: &DataStore<ItemIdxType, RecipeIdxType>,
    ) -> Result<(), ()> {
        if !self.can_fit(
            entity.get_pos(),
            entity.get_entity_size(data_store),
            data_store,
        ) {
            return Err(());
        }

        profiling::scope!("add_entity {}", entity.get_type_name());

        let pos = entity.get_pos();
        let size = entity.get_entity_size(data_store);

        let chunk_pos = self.get_chunk_pos_for_tile(pos);

        if self.get_chunk_for_tile_mut(pos).is_none() {
            todo!("Chunk missing");
            return Err(());
        }

        let mut cascading_updates = vec![];

        match entity {
            Entity::Lab {
                pos,
                ty,
                ref modules,
                pole_position,
            } => {
                if let Some((pole_pos, _, _)) = pole_position {
                    let grid = sim_state.factory.power_grids.pole_pos_to_grid_id[&pole_pos];
                    self.power_grid_lookup
                        .grid_to_chunks
                        .entry(grid)
                        .or_default()
                        .insert(chunk_pos);
                }

                cascading_updates.push(new_lab_cascade(pos, data_store));
            },
            Entity::SolarPanel {
                pos,
                ty,
                pole_position,
            } => {
                if let Some((pole_pos, _)) = pole_position {
                    let grid = sim_state.factory.power_grids.pole_pos_to_grid_id[&pole_pos];
                    self.power_grid_lookup
                        .grid_to_chunks
                        .entry(grid)
                        .or_default()
                        .insert(chunk_pos);
                }
            },
            Entity::Assembler { info, .. } => match info {
                AssemblerInfo::UnpoweredNoRecipe | AssemblerInfo::Unpowered(_) => {},
                AssemblerInfo::PoweredNoRecipe(pole_position) => {
                    let grid = sim_state.factory.power_grids.pole_pos_to_grid_id[&pole_position];
                    self.power_grid_lookup
                        .grid_to_chunks
                        .entry(grid)
                        .or_default()
                        .insert(chunk_pos);
                },
                AssemblerInfo::Powered {
                    id: AssemblerID { grid, .. },
                    pole_position,
                    weak_index,
                } => {
                    let lookup_grid =
                        sim_state.factory.power_grids.pole_pos_to_grid_id[&pole_position];
                    assert_eq!(grid, lookup_grid);
                    self.power_grid_lookup
                        .grid_to_chunks
                        .entry(grid)
                        .or_default()
                        .insert(chunk_pos);

                    cascading_updates.push(newly_working_assembler(pos, data_store));
                },
            },
            Entity::PowerPole {
                ty, pos: pole_pos, ..
            } => {
                let grid = sim_state.factory.power_grids.pole_pos_to_grid_id[&pole_pos];
                self.power_grid_lookup
                    .grid_to_chunks
                    .entry(grid)
                    .or_default()
                    .insert(chunk_pos);

                // Handle Entities that are newly powered
                cascading_updates.push(new_power_pole(pole_pos, data_store));
            },
            Entity::Belt { id, direction, .. } => {
                self.belt_lookup
                    .belt_id_to_chunks
                    .entry(id)
                    .or_default()
                    .insert(chunk_pos);

                self.belt_recieving_input_directions
                    .entry(pos + direction)
                    .or_default()[direction.reverse()] = true;

                cascading_updates.push(new_possible_inserter_connection(pos, (1, 1)));
                cascading_updates.push(try_instantiating_inserters_for_belt_cascade(id));
            },
            Entity::Underground {
                pos,
                direction,
                id,
                underground_dir,
                ..
            } => {
                self.belt_lookup
                    .belt_id_to_chunks
                    .entry(id)
                    .or_default()
                    .insert(chunk_pos);

                if underground_dir == UndergroundDir::Exit {
                    self.belt_recieving_input_directions
                        .entry(pos + direction)
                        .or_default()[direction.reverse()] = true;
                }

                cascading_updates.push(new_possible_inserter_connection(pos, (1, 1)));
                cascading_updates.push(try_instantiating_inserters_for_belt_cascade(id));
            },
            Entity::Splitter {
                pos,
                direction,
                id: splitter_id,
            } => {
                self.belt_recieving_input_directions
                    .entry(pos + direction)
                    .or_default()[direction.reverse()] = true;
                self.belt_recieving_input_directions
                    .entry(pos + direction.turn_right() + direction)
                    .or_default()[direction.reverse()] = true;

                cascading_updates.push(try_instantiating_all_inserters_cascade());
            },
            Entity::Inserter { info, .. } => match info {
                InserterInfo::NotAttached { .. } => {
                    cascading_updates.push(instantiate_inserter_cascade(pos, data_store));
                },
                InserterInfo::Attached {
                    info: attached_inserter,
                    ..
                } => match attached_inserter {
                    AttachedInserter::BeltStorage { id, .. } => {
                        self.belt_lookup
                            .belt_id_to_chunks
                            .entry(id)
                            .or_default()
                            .insert(chunk_pos);
                        cascading_updates.push(instantiate_inserter_cascade(pos, data_store));
                    },
                    AttachedInserter::BeltBelt { item, inserter } => {
                        todo!("We need to store the position in the belt_id_lookup");
                    },
                    AttachedInserter::StorageStorage { .. } => todo!(),
                },
            },
            Entity::Chest {
                ty,
                pos,
                item: None,
                slot_limit: _,
            } => {
                cascading_updates.push(new_chest_cascade(pos));
            },
            Entity::Chest {
                ty,
                pos,
                item: Some(_),
                slot_limit: _,
            } => {
                cascading_updates.push(new_chest_cascade(pos));
            },
            Entity::Roboport {
                ty,
                pos,
                power_grid,
                network,
                id,
            } => {},
            Entity::Beacon {
                ty,
                pos,
                ref modules,
                pole_position: Some((pole_pos, weak_idx)),
            } => {
                cascading_updates.push(new_powered_beacon_cascade(pos, data_store));
            },
            Entity::Beacon {
                ty,
                pos,
                ref modules,
                pole_position: None,
            } => {},
            Entity::FluidTank { .. } => {},
        };

        if let Some(map_updates) = &mut self.map_updates {
            for x_offs in 0..entity.get_entity_size(data_store).0 {
                for y_offs in 0..entity.get_entity_size(data_store).1 {
                    let e_pos = entity.get_pos();

                    map_updates.push(Position {
                        x: e_pos.x + i32::from(x_offs),
                        y: e_pos.y + i32::from(y_offs),
                    });
                }
            }
        }

        let chunk_x = pos.x / i32::from(CHUNK_SIZE);
        let chunk_y = pos.y / i32::from(CHUNK_SIZE);
        let chunk = self
            .get_chunk_mut((chunk_x, chunk_y))
            .expect("Chunk outside the world!");

        let map = if let Some(map) = &mut chunk.chunk_tile_to_entity_into {
            map
        } else {
            chunk.chunk_tile_to_entity_into = Some(Box::new(
                [[EMPTY; CHUNK_SIZE as usize]; CHUNK_SIZE as usize],
            ));
            chunk.chunk_tile_to_entity_into.as_mut().unwrap()
        };

        let e_pos: Position = entity.get_pos();
        let e_size = entity.get_entity_size(data_store);

        chunk.entities.push(entity);

        let index = u8::try_from(chunk.entities.len() - 1).expect("Into a chunk of size 16 x 16 we can fit at most 256 entitites. This assumes all entities are at least 1x1");

        if index == min(EMPTY, OTHER_CHUNK) {
            // This could cause confusion with tiles marked empty
            // if map[x][y] == min(EMPTY, OTHER_CHUNK) {
            //     map[x][y] = EMPTY_OR_OTHER_CHUNK;
            // }
            todo!()
        }

        let x_in_chunk = usize::try_from((e_pos.x).rem_euclid(CHUNK_SIZE as i32)).unwrap();
        let y_in_chunk = usize::try_from((e_pos.y).rem_euclid(CHUNK_SIZE as i32)).unwrap();
        for x_offs in 0..e_size.0 {
            for y_offs in 0..e_size.1 {
                let x = min(x_in_chunk + usize::from(x_offs), CHUNK_SIZE as usize - 1);
                let y = min(y_in_chunk + usize::from(y_offs), CHUNK_SIZE as usize - 1);

                // TODO: We write the same index multiple times here
                assert!(
                    map[x][y] == EMPTY || map[x][y] == EMPTY_OR_OTHER_CHUNK || map[x][y] == index,
                    "Tile was not empty, but was {}",
                    map[x][y]
                );
                map[x][y] = index;
            }
        }

        for x_offs in 0..e_size.0 {
            for y_offs in 0..e_size.1 {
                let x_rel = x_in_chunk + usize::from(x_offs);
                let y_rel = y_in_chunk + usize::from(y_offs);

                match (
                    x_rel >= usize::from(CHUNK_SIZE),
                    y_rel >= usize::from(CHUNK_SIZE),
                ) {
                    (false, false) => {},
                    (x, y) => {
                        let chunk = self
                            .get_chunk_mut((chunk_x + i32::from(x), chunk_y + i32::from(y)))
                            .expect("Trying to place entitty outside the world");

                        let arr = chunk.chunk_tile_to_entity_into.get_or_insert(Box::new(
                            [[EMPTY; CHUNK_SIZE as usize]; CHUNK_SIZE as usize],
                        ));

                        arr[x_rel - (usize::from(x) * usize::from(CHUNK_SIZE))]
                            [y_rel - (usize::from(y) * usize::from(CHUNK_SIZE))] = OTHER_CHUNK;
                    },
                }
            }
        }

        #[cfg(debug_assertions)]
        for x_offs in 0..e_size.0 {
            for y_offs in 0..e_size.1 {
                assert!(
                    self.get_entity_at(
                        Position {
                            x: e_pos.x + i32::from(x_offs),
                            y: e_pos.y + i32::from(y_offs)
                        },
                        data_store
                    )
                    .is_some()
                );
                assert!(
                    self.get_entity_at(
                        Position {
                            x: e_pos.x + i32::from(x_offs),
                            y: e_pos.y + i32::from(y_offs)
                        },
                        data_store
                    )
                    .unwrap()
                    .get_pos()
                        == e_pos
                );
            }
        }

        {
            profiling::scope!("Cascading updates");
            while let Some(update) = cascading_updates.pop() {
                (update.update)(self, sim_state, &mut cascading_updates, data_store);
            }
        }

        Ok(())
    }

    pub fn try_instantiate_inserter(
        &mut self,
        simulation_state: &mut SimulationState<ItemIdxType, RecipeIdxType>,
        pos: Position,
        data_store: &DataStore<ItemIdxType, RecipeIdxType>,
    ) -> Result<InserterInstantiationNewOptions<ItemIdxType>, InstantiateInserterError<ItemIdxType>>
    {
        enum InserterConnection<ItemIdxType: IdxTrait, RecipeIdxType: IdxTrait> {
            Belt(BeltTileId<ItemIdxType>, u16),
            Storage(Static<RecipeIdxType>),
        }

        enum Static<RecipeIdxType: IdxTrait> {
            Done(Storage<RecipeIdxType>),
            ToInstantiate,
        }

        #[derive(Debug, Clone, PartialEq)]
        enum PossibleItem<ItemIdxType: IdxTrait> {
            All,
            List(Vec<Item<ItemIdxType>>),
            None,
        }

        impl<ItemIdxType: IdxTrait> PossibleItem<ItemIdxType> {
            fn contains(&self, item: Item<ItemIdxType>) -> bool {
                match self {
                    PossibleItem::All => true,
                    PossibleItem::List(items) => items.contains(&item),
                    PossibleItem::None => false,
                }
            }
        }

        struct InserterConnectionPossibility<ItemIdxType: IdxTrait, RecipeIdxType: IdxTrait> {
            conn: InserterConnection<ItemIdxType, RecipeIdxType>,
            inserter_item_hint: Option<Vec<Item<ItemIdxType>>>,
            possible_item_list: PossibleItem<ItemIdxType>,
        }

        let Some(Entity::Inserter {
            ty,
            user_movetime,
            type_movetime,

            pos: _pos,
            direction,
            info: InserterInfo::NotAttached { start_pos, end_pos },
            filter,
        }) = self.get_entity_at(pos, data_store)
        else {
            return Err(InstantiateInserterError::NotUnattachedInserter);
        };

        let movetime = user_movetime.map(|v| v.into()).unwrap_or(*type_movetime);

        let start_conn: Option<InserterConnectionPossibility<ItemIdxType, RecipeIdxType>> = self
            .get_entity_at(*start_pos, data_store)
            .map(|e| match e {
                Entity::Inserter { .. } | Entity::PowerPole { .. }| Entity::SolarPanel { .. }| Entity::Beacon { .. }| Entity::FluidTank { .. }  => None,

                Entity::Roboport { ty, pos, power_grid, network, id } => {
                    // TODO:
                    warn!("It is currently not possible to add or remove bots from roboports using inserters");
                    None
                }

                Entity::Assembler {
                    ty: _,
                    pos: _,
                    info: AssemblerInfo::Powered {
                        id,
                        pole_position: _,
                        weak_index: _
                    },
                    modules: _,
                    rotation: _
                    // FIXME: Translate the recipe_idx to
                } => Some(InserterConnectionPossibility {
                    conn: InserterConnection::Storage(Static::Done(Storage::Assembler {
                        grid: id.grid,
                        recipe_idx_with_this_item: id.recipe.id,
                        index: id.assembler_index,
                    })),
                    inserter_item_hint: None,
                    possible_item_list: PossibleItem::List(
                        data_store.recipe_to_items[&id.recipe]
                            .iter()
                            .filter_map(|(dir, item)| (*dir == ItemRecipeDir::Out).then_some(*item))
                            .collect(),
                    ),
                }),
                Entity::Assembler { .. } => None,
                Entity::Belt {
                    id: BeltTileId::AnyBelt(id, _),
                    belt_pos,
                    ..
                }
                | Entity::Underground {
                    id: BeltTileId::AnyBelt(id, _),
                    belt_pos,
                    ..
                } => Some(InserterConnectionPossibility {
                    conn: InserterConnection::Belt(BeltTileId::AnyBelt(*id, PhantomData), *belt_pos),
                    inserter_item_hint: match simulation_state
                        .factory
                        .belts
                        .get_pure_item(BeltTileId::AnyBelt(*id, PhantomData))
                    {
                        Some(item) => Some(vec![item]),
                        None => None,
                    },
                    possible_item_list: match simulation_state
                        .factory
                        .belts
                        .get_pure_item(BeltTileId::AnyBelt(*id, PhantomData))
                    {
                        Some(item) => PossibleItem::List(vec![item]),
                        None => PossibleItem::All,
                    },
                }),
                Entity::Splitter { pos, id, direction: splitter_dir, .. } => {
                    let mut side = if *pos == *end_pos {
                        SplitterSide::Left
                    } else {
                        SplitterSide::Right
                    };

                    match splitter_dir {
                        Dir::North | Dir::East => {},
                        Dir::South | Dir::West => side = side.switch(),
                    }

                    let [_, outputs] = simulation_state
                    .factory
                    .belts.get_splitter_belt_ids(*id);

                    let id = outputs[usize::from(bool::from(side))];

                    Some(InserterConnectionPossibility { conn: InserterConnection::Belt(id, SPLITTER_BELT_LEN), inserter_item_hint: simulation_state
                        .factory
                        .belts
                        .get_pure_item(id).map(|item| vec![item]), possible_item_list: PossibleItem::All })
                },
                Entity::Chest {
                    ty: _,
                    pos: _,
                    item: Some((item, index)),
                    slot_limit: _
                } => Some(InserterConnectionPossibility {
                    conn:  InserterConnection::Storage(Static::Done(Storage::Static {
                        static_id: StaticID::Chest as u16,
                        index: *index,
                    })),
                    inserter_item_hint: None,
                    possible_item_list: PossibleItem::List(vec![*item])
                }),
                Entity::Chest {
                    ty,
                    pos,
                    item: None,
                    slot_limit: _
                } => Some(InserterConnectionPossibility {
                    conn:  InserterConnection::Storage(Static::ToInstantiate),
                    inserter_item_hint: None,
                    possible_item_list: PossibleItem::All
                }),
                Entity::Lab { pos, ty, pole_position, modules } => {
                    // No removing items from Labs!
                    None
                }
            })
            .flatten();

        let Some(start_conn) = start_conn else {
            return Err(InstantiateInserterError::SourceMissing);
        };

        let dest_conn: Option<InserterConnectionPossibility<ItemIdxType, RecipeIdxType>> = self
            .get_entity_at(*end_pos, data_store)
            .map(|e| match e {
                Entity::Inserter { .. } | Entity::PowerPole { .. }| Entity::SolarPanel { .. }| Entity::Beacon { .. }| Entity::FluidTank { .. }  => None,

                Entity::Roboport { ty, pos, power_grid, network, id } => {
                    // TODO:
                    warn!("It is currently not possible to add or remove bots from roboports using inserters");
                    None
                }

                Entity::Assembler {
                    ty: _,
                    pos: _,
                    info: AssemblerInfo::Powered {
                        id,
                        pole_position: _,
                        weak_index: _
                    },
                    modules: _,
                    rotation: _,
                    // FIXME: Translate the recipe_idx to
                } => Some(InserterConnectionPossibility {
                    conn: InserterConnection::Storage(Static::Done(Storage::Assembler {
                        grid: id.grid,
                        recipe_idx_with_this_item: id.recipe.id,
                        index: id.assembler_index,
                    })),
                    inserter_item_hint: None,
                    possible_item_list: PossibleItem::List(
                        data_store.recipe_to_items[&id.recipe]
                            .iter()
                            .filter_map(|(dir, item)| (*dir == ItemRecipeDir::Ing).then_some(*item))
                            .collect(),
                    ),
                }),
                Entity::Assembler { .. } => None,
                Entity::Belt {
                    id: BeltTileId::AnyBelt(id, _),
                    belt_pos,
                    ..
                }
                | Entity::Underground {
                    id: BeltTileId::AnyBelt(id, _),
                    belt_pos,
                    ..
                } => Some(InserterConnectionPossibility {
                    conn: InserterConnection::Belt(BeltTileId::AnyBelt(*id, PhantomData), *belt_pos),
                    inserter_item_hint: simulation_state
                        .factory
                        .belts
                        .get_pure_item(BeltTileId::AnyBelt(*id, PhantomData)).map(|item| vec![item]),
                    possible_item_list: PossibleItem::All,
                }),
                Entity::Splitter { pos, id, direction: splitter_dir, .. } => {
                    let mut side = if *pos == *end_pos {
                        SplitterSide::Left
                    } else {
                        SplitterSide::Right
                    };

                    match splitter_dir {
                        Dir::North | Dir::East => {},
                        Dir::South | Dir::West => side = side.switch(),
                    }

                    let [inputs, _] = simulation_state
                    .factory
                    .belts.get_splitter_belt_ids(*id);

                    let id = inputs[usize::from(bool::from(side))];

                    Some(InserterConnectionPossibility { conn: InserterConnection::Belt(id, SPLITTER_BELT_LEN), inserter_item_hint: simulation_state
                        .factory
                        .belts
                        .get_pure_item(id).map(|item| vec![item]), possible_item_list: PossibleItem::All })
                },
                Entity::Chest {
                    ty,
                    pos,
                    item: Some((item, index)),
                    slot_limit: _
                } => Some(InserterConnectionPossibility {
                    conn:  InserterConnection::Storage(Static::Done(Storage::Static {
                        static_id: StaticID::Chest as u16,
                        index: *index,
                    })),
                    inserter_item_hint: None,
                    possible_item_list: PossibleItem::List(vec![*item])
                }),
                Entity::Chest {
                    ty: _,
                    pos: _,
                    item: None,
                    slot_limit: _
                } => Some(InserterConnectionPossibility {
                    conn:  InserterConnection::Storage(Static::ToInstantiate),
                    inserter_item_hint: None,
                    possible_item_list: PossibleItem::All
                }),
                Entity::Lab { pos, ty, pole_position, modules } => {
                    if let Some((pole_pos, idx, lab_store_index)) = pole_position {
                        Some(InserterConnectionPossibility { conn: InserterConnection::Storage(Static::Done(Storage::Lab { grid: simulation_state.factory.power_grids.pole_pos_to_grid_id[pole_pos], index: *lab_store_index })), inserter_item_hint: None, possible_item_list: PossibleItem::List(data_store.science_bottle_items.iter().copied().collect()) })
                    } else {
                        None
                    }
                }
            })
            .flatten();

        let Some(dest_conn) = dest_conn else {
            return Err(InstantiateInserterError::DestMissing);
        };

        let possible_items: PossibleItem<_> = match (
            &start_conn.possible_item_list,
            &dest_conn.possible_item_list,
        ) {
            (PossibleItem::All, i) | (i, PossibleItem::All) => i.clone(),
            (PossibleItem::List(a), PossibleItem::List(b)) => {
                PossibleItem::List(a.iter().copied().filter(|v| b.contains(v)).collect())
            },
            (PossibleItem::None, _) => PossibleItem::None,
            (_, PossibleItem::None) => PossibleItem::None,
        };

        if possible_items == PossibleItem::None {
            match (start_conn.conn, dest_conn.conn) {
                (
                    InserterConnection::Belt(start_belt_id, _),
                    InserterConnection::Belt(dest_belt_id, _),
                ) => {
                    return Err(InstantiateInserterError::ItemConflict {
                        belts_which_could_help: vec![start_belt_id, dest_belt_id],
                    });
                },
                (InserterConnection::Belt(start_belt_id, _), InserterConnection::Storage(_)) => {
                    return Err(InstantiateInserterError::ItemConflict {
                        belts_which_could_help: vec![start_belt_id],
                    });
                },
                (InserterConnection::Storage(_), InserterConnection::Belt(dest_belt_id, _)) => {
                    return Err(InstantiateInserterError::ItemConflict {
                        belts_which_could_help: vec![dest_belt_id],
                    });
                },
                (InserterConnection::Storage(_), InserterConnection::Storage(_)) => {
                    return Err(InstantiateInserterError::ItemConflict {
                        belts_which_could_help: vec![],
                    });
                },
            }
        }

        // For determining the filter we use this plan:
        // If a filter is specified, use that
        // If we can determine a single source item use that,
        // If we can determine a single destination item use that
        // Else make the user do it
        let determined_filter = match filter {
            Some(filter) => {
                if possible_items.contains(*filter) {
                    *filter
                } else {
                    match (start_conn.conn, dest_conn.conn) {
                        (
                            InserterConnection::Belt(start_belt_id, _),
                            InserterConnection::Belt(dest_belt_id, _),
                        ) => {
                            return Err(InstantiateInserterError::ItemConflict {
                                belts_which_could_help: vec![start_belt_id, dest_belt_id],
                            });
                        },
                        (
                            InserterConnection::Belt(start_belt_id, _),
                            InserterConnection::Storage(_),
                        ) => {
                            return Err(InstantiateInserterError::ItemConflict {
                                belts_which_could_help: vec![start_belt_id],
                            });
                        },
                        (
                            InserterConnection::Storage(_),
                            InserterConnection::Belt(dest_belt_id, _),
                        ) => {
                            return Err(InstantiateInserterError::ItemConflict {
                                belts_which_could_help: vec![dest_belt_id],
                            });
                        },
                        (InserterConnection::Storage(_), InserterConnection::Storage(_)) => {
                            return Err(InstantiateInserterError::ItemConflict {
                                belts_which_could_help: vec![],
                            });
                        },
                    }
                }
            },
            None => {
                // The user/game has not specified a filter, try and infer it

                let belts = match (&start_conn.conn, &dest_conn.conn) {
                    (
                        InserterConnection::Belt(start_belt_id, _),
                        InserterConnection::Belt(dest_belt_id, _),
                    ) => vec![*start_belt_id, *dest_belt_id],
                    (
                        InserterConnection::Belt(start_belt_id, _),
                        InserterConnection::Storage(_),
                    ) => vec![*start_belt_id],
                    (InserterConnection::Storage(_), InserterConnection::Belt(dest_belt_id, _)) => {
                        vec![*dest_belt_id]
                    },
                    (InserterConnection::Storage(_), InserterConnection::Storage(_)) => vec![],
                };

                // TODO: Figure out what is most intuitive here, for now just use the only possible item otherwise error
                match possible_items {
                    PossibleItem::All => {
                        return Err(InstantiateInserterError::PleaseSpecifyFilter {
                            belts_which_could_help: belts,
                        });
                    },
                    PossibleItem::List(items) => match items.len().cmp(&1) {
                        std::cmp::Ordering::Less => {
                            return Err(InstantiateInserterError::ItemConflict {
                                belts_which_could_help: belts,
                            });
                        },
                        std::cmp::Ordering::Equal => items[0],
                        std::cmp::Ordering::Greater => {
                            return Err(InstantiateInserterError::PleaseSpecifyFilter {
                                belts_which_could_help: belts,
                            });
                        },
                    },
                    PossibleItem::None => unreachable!(),
                }
            },
        };

        let mut instantiated = InserterInstantiationNewOptions::Positions(vec![]);

        match (start_conn.conn, dest_conn.conn) {
            (
                InserterConnection::Belt(start_belt_id, start_belt_pos),
                InserterConnection::Belt(dest_belt_id, dest_belt_pos),
            ) => {
                let start_pos = *start_pos;
                let end_pos = *end_pos;
                // FIXME: The movetime should be dependent on the inserter type!
                let index = simulation_state.factory.belts.add_belt_belt_inserter(
                    (start_belt_id, start_belt_pos),
                    (dest_belt_id, dest_belt_pos),
                    BeltBeltInserterAdditionInfo {
                        cooldown: MOVETIME,
                        filter: determined_filter,
                    },
                );
                instantiated =
                    InserterInstantiationNewOptions::Belts(vec![start_belt_id, dest_belt_id]);

                let Entity::Inserter { info, .. } = self
                    .get_chunk_for_tile_mut(pos)
                    .unwrap()
                    .get_entity_at_mut(pos, data_store)
                    .unwrap()
                else {
                    unreachable!("We already checked it was an unattached inserter before")
                };

                *info = InserterInfo::Attached {
                    info: AttachedInserter::BeltBelt {
                        item: determined_filter,
                        inserter: index,
                    },
                    start_pos,
                    end_pos,
                }
            },
            (
                InserterConnection::Belt(start_belt_id, start_belt_pos),
                InserterConnection::Storage(dest_storage_untranslated),
            ) => {
                let start_pos = *start_pos;
                let end_pos = *end_pos;
                let dest_storage_untranslated = match dest_storage_untranslated {
                    Static::Done(storage) => storage,
                    Static::ToInstantiate => {
                        let mut storage = None;
                        self.get_entity_at_mut(end_pos, data_store)
                            .map(|e| match e {
                                Entity::Chest {
                                    ty,
                                    pos: chest_pos,
                                    item,
                                    slot_limit,
                                } => {
                                    let index = simulation_state.factory.chests.stores
                                        [usize_from(determined_filter.id)]
                                    .add_chest(*ty, *slot_limit, data_store);
                                    *item = Some((determined_filter, index));
                                    storage = Some(Storage::Static {
                                        index,
                                        static_id: StaticID::Chest as u16,
                                    })
                                },
                                _ => unreachable!(),
                            });
                        storage.unwrap()
                    },
                };

                let dest_storage =
                    dest_storage_untranslated.translate(determined_filter, data_store);

                match simulation_state.factory.belts.add_belt_storage_inserter(
                    determined_filter,
                    start_belt_id,
                    start_belt_pos - 1,
                    FakeUnionStorage::from_storage_with_statics_at_zero(
                        determined_filter,
                        dest_storage,
                        data_store,
                    ),
                ) {
                    Ok(()) => {},
                    Err(_) => {
                        todo!()
                    },
                };
                instantiated = InserterInstantiationNewOptions::PositionsAndBelts(
                    vec![end_pos],
                    vec![start_belt_id],
                );

                let Entity::Inserter { info, .. } = self
                    .get_chunk_for_tile_mut(pos)
                    .unwrap()
                    .get_entity_at_mut(pos, data_store)
                    .unwrap()
                else {
                    unreachable!("We already checked it was an unattached inserter before")
                };

                *info = InserterInfo::Attached {
                    info: AttachedInserter::BeltStorage {
                        id: start_belt_id,
                        belt_pos: start_belt_pos - 1,
                    },
                    start_pos,
                    end_pos,
                };

                self.belt_lookup
                    .belt_id_to_chunks
                    .entry(start_belt_id)
                    .or_default()
                    .insert((pos.x / CHUNK_SIZE as i32, pos.y / CHUNK_SIZE as i32));
            },
            (
                InserterConnection::Storage(start_storage_untranslated),
                InserterConnection::Belt(dest_belt_id, dest_belt_pos),
            ) => {
                let start_pos = *start_pos;
                let end_pos = *end_pos;
                let start_storage_untranslated = match start_storage_untranslated {
                    Static::Done(storage) => storage,
                    Static::ToInstantiate => {
                        let mut storage = None;
                        self.get_entity_at_mut(start_pos, data_store)
                            .map(|e| match e {
                                Entity::Chest {
                                    ty,
                                    pos: chest_pos,
                                    item,
                                    slot_limit,
                                } => {
                                    let index = simulation_state.factory.chests.stores
                                        [usize_from(determined_filter.id)]
                                    .add_chest(*ty, *slot_limit, data_store);
                                    *item = Some((determined_filter, index));
                                    storage = Some(Storage::Static {
                                        index,
                                        static_id: StaticID::Chest as u16,
                                    })
                                },
                                _ => unreachable!(),
                            });
                        storage.unwrap()
                    },
                };
                instantiated = InserterInstantiationNewOptions::PositionsAndBelts(
                    vec![start_pos],
                    vec![dest_belt_id],
                );

                let start_storage =
                    start_storage_untranslated.translate(determined_filter, data_store);

                match simulation_state.factory.belts.add_storage_belt_inserter(
                    determined_filter,
                    dest_belt_id,
                    dest_belt_pos - 1,
                    FakeUnionStorage::from_storage_with_statics_at_zero(
                        determined_filter,
                        start_storage,
                        data_store,
                    ),
                ) {
                    Ok(()) => {},
                    Err(_) => {
                        todo!()
                    },
                };

                let Entity::Inserter { info, .. } = self
                    .get_chunk_for_tile_mut(pos)
                    .unwrap()
                    .get_entity_at_mut(pos, data_store)
                    .unwrap()
                else {
                    unreachable!("We already checked it was an unattached inserter before")
                };

                *info = InserterInfo::Attached {
                    info: AttachedInserter::BeltStorage {
                        id: dest_belt_id,
                        belt_pos: dest_belt_pos - 1,
                    },
                    start_pos,
                    end_pos,
                };

                self.belt_lookup
                    .belt_id_to_chunks
                    .entry(dest_belt_id)
                    .or_default()
                    .insert((pos.x / CHUNK_SIZE as i32, pos.y / CHUNK_SIZE as i32));
            },
            (
                InserterConnection::Storage(start_storage_untranslated),
                InserterConnection::Storage(dest_storage_untranslated),
            ) => {
                let start_pos = *start_pos;
                let end_pos = *end_pos;

                let start_storage_untranslated = match start_storage_untranslated {
                    Static::Done(storage) => storage,
                    Static::ToInstantiate => {
                        let mut storage = None;
                        self.get_entity_at_mut(start_pos, data_store)
                            .map(|e| match e {
                                Entity::Chest {
                                    ty,
                                    pos: chest_pos,
                                    item,
                                    slot_limit,
                                } => {
                                    let index = simulation_state.factory.chests.stores
                                        [usize_from(determined_filter.id)]
                                    .add_chest(*ty, *slot_limit, data_store);
                                    *item = Some((determined_filter, index));
                                    match &mut instantiated {
                                        InserterInstantiationNewOptions::Positions(positions) => {
                                            positions.push(*chest_pos)
                                        },
                                        _ => todo!(),
                                    }
                                    storage = Some(Storage::Static {
                                        index,
                                        static_id: StaticID::Chest as u16,
                                    });
                                },
                                _ => unreachable!(),
                            });
                        storage.unwrap()
                    },
                };

                let dest_storage_untranslated = match dest_storage_untranslated {
                    Static::Done(storage) => storage,
                    Static::ToInstantiate => {
                        let mut storage = None;
                        self.get_entity_at_mut(end_pos, data_store)
                            .map(|e| match e {
                                Entity::Chest {
                                    ty,
                                    pos: chest_pos,
                                    item: item @ None,
                                    slot_limit,
                                } => {
                                    let index = simulation_state.factory.chests.stores
                                        [usize_from(determined_filter.id)]
                                    .add_chest(*ty, *slot_limit, data_store);
                                    *item = Some((determined_filter, index));
                                    match &mut instantiated {
                                        InserterInstantiationNewOptions::Positions(positions) => {
                                            positions.push(*chest_pos)
                                        },
                                        _ => todo!(),
                                    }
                                    storage = Some(Storage::Static {
                                        index,
                                        static_id: StaticID::Chest as u16,
                                    });
                                },
                                _ => unreachable!(),
                            });
                        storage.unwrap()
                    },
                };

                let start_storage =
                    start_storage_untranslated.translate(determined_filter, data_store);
                let dest_storage =
                    dest_storage_untranslated.translate(determined_filter, data_store);

                let index = simulation_state.factory.storage_storage_inserters.add_ins(
                    determined_filter,
                    movetime.into(),
                    start_storage,
                    dest_storage,
                    HAND_SIZE,
                    data_store,
                );

                let Entity::Inserter { info, .. } = self
                    .get_chunk_for_tile_mut(pos)
                    .unwrap()
                    .get_entity_at_mut(pos, data_store)
                    .unwrap()
                else {
                    unreachable!("We already checked it was an unattached inserter before")
                };

                *info = InserterInfo::Attached {
                    info: AttachedInserter::StorageStorage {
                        item: determined_filter,
                        inserter: index,
                    },
                    start_pos,
                    end_pos,
                };
            },
        }

        Ok(instantiated)
    }

    pub fn update_power_grid_id(
        &mut self,
        sim_state: &mut SimulationState<ItemIdxType, RecipeIdxType>,
        old_id: PowerGridIdentifier,
        new_id: PowerGridIdentifier,
    ) {
        let old_chunks = self.power_grid_lookup.grid_to_chunks.remove(&old_id);

        for chunk_pos in old_chunks.iter().flatten() {
            let chunk = self
                .chunks
                .get_mut(chunk_pos.0, chunk_pos.1)
                .expect("Ungenerated chunk in belt map!");

            for entity in &mut chunk.entities {
                match entity {
                    Entity::SolarPanel {
                        pole_position: None,
                        ..
                    } => {},
                    Entity::SolarPanel {
                        pole_position: Some(_),
                        ..
                    } => todo!(),
                    Entity::Lab {
                        pole_position: None,
                        ..
                    } => {},
                    Entity::Lab {
                        pole_position: Some(_),
                        ..
                    } => todo!(),
                    Entity::Assembler { info, .. } => match info {
                        AssemblerInfo::UnpoweredNoRecipe | AssemblerInfo::Unpowered(_) => {},
                        AssemblerInfo::PoweredNoRecipe(pole_position) => {},
                        AssemblerInfo::Powered {
                            id:
                                AssemblerID {
                                    grid: grid_in_id, ..
                                },
                            pole_position,
                            weak_index,
                        } => {
                            let grid =
                                sim_state.factory.power_grids.pole_pos_to_grid_id[pole_position];
                            assert_eq!(grid, new_id);
                            if *grid_in_id == old_id {
                                *grid_in_id = new_id;
                            }
                        },
                    },
                    Entity::PowerPole { .. } => {},
                    Entity::Inserter { info, .. } => match info {
                        InserterInfo::NotAttached { .. } => {},
                        InserterInfo::Attached {
                            info: attached_inserter,
                            ..
                        } => match attached_inserter {
                            AttachedInserter::BeltStorage { id, belt_pos } => {
                                // TODO
                            },
                            AttachedInserter::BeltBelt { item, inserter } => {
                                // TODO
                            },
                            AttachedInserter::StorageStorage { .. } => todo!(),
                        },
                    },
                    Entity::Roboport { power_grid, .. } => {
                        if *power_grid == Some(old_id) {
                            *power_grid = Some(new_id);
                        }
                    },
                    Entity::Belt { .. }
                    | Entity::Underground { .. }
                    | Entity::Splitter { .. }
                    | Entity::Chest { .. }
                    | Entity::Beacon { .. }
                    | Entity::FluidTank { .. } => {},
                }
            }
        }

        self.power_grid_lookup
            .grid_to_chunks
            .entry(new_id)
            .or_default()
            .extend(old_chunks.into_iter().flatten());
    }

    pub fn update_belt_id_after(
        &mut self,
        sim_state: &mut SimulationState<ItemIdxType, RecipeIdxType>,
        old_id: BeltTileId<ItemIdxType>,
        new_id: BeltTileId<ItemIdxType>,
        belt_pos_earliest: u16,
        data_store: &DataStore<ItemIdxType, RecipeIdxType>,
    ) {
        if old_id == new_id {
            return;
        }
        info!(
            "Change belt_id at >= {} {:?} to {:?}",
            belt_pos_earliest, old_id, new_id
        );
        if belt_pos_earliest != 0 {
            if let Some(waiting) = self.to_instantiate_by_belt.get(&old_id) {
                let waiting = waiting.clone();
                self.to_instantiate_by_belt
                    .entry(new_id)
                    .or_default()
                    .extend(waiting);
            }
        }

        let old_chunks: BTreeSet<_> = if belt_pos_earliest == 0 {
            let Some(old_chunks) = self.belt_lookup.belt_id_to_chunks.remove(&old_id) else {
                debug_assert!(
                    self.get_chunks()
                        .flat_map(|chunk| chunk.get_entities())
                        .all(|e| match e {
                            Entity::Belt { id, .. } => *id != old_id,
                            Entity::Underground { id, .. } => *id != old_id,
                            Entity::Splitter { .. } => {
                                // Nothing, since a splitter does not stores its belt_ids
                                true
                            },
                            Entity::Inserter { info, .. } => match info {
                                InserterInfo::NotAttached { .. } => true,
                                InserterInfo::Attached { info, .. } => match info {
                                    AttachedInserter::BeltStorage { id, belt_pos } => *id != old_id,
                                    AttachedInserter::BeltBelt { item, inserter } => {
                                        // TODO:
                                        true
                                    },
                                    AttachedInserter::StorageStorage { item, inserter } => true,
                                },
                            },
                            _ => true,
                        })
                );
                return;
            };
            old_chunks
        } else {
            self.belt_lookup
                .belt_id_to_chunks
                .get(&old_id)
                .iter()
                .flat_map(|v| v.iter())
                .copied()
                .collect()
        };

        info!("Checking {} chunks to change belt_id", old_chunks.len());
        if old_chunks.len() > 150 {
            warn!("Having to check a lot of chunks: {}", old_chunks.len());
        }

        let old_chunks_filtered = old_chunks.iter().copied().filter(|&chunk_pos| {
            let chunk = self
                .chunks
                .get_mut(chunk_pos.0, chunk_pos.1)
                .expect("Ungenerated chunk in belt map!");

            let mut found_anything = false;

            for entity in &mut chunk.entities {
                match entity {
                    Entity::Beacon { .. } => {},
                    Entity::Lab { .. } => {},
                    Entity::SolarPanel { .. } => {},
                    Entity::Assembler { .. } => {},
                    Entity::PowerPole { .. } => {},
                    Entity::Chest { .. } => {},
                    Entity::Roboport { .. } => {},
                    Entity::FluidTank { .. } => {},
                    Entity::Belt { id, belt_pos, .. }
                    | Entity::Underground { id, belt_pos, .. } => {
                        if *id == old_id && belt_pos_earliest <= *belt_pos {
                            *id = new_id;
                            found_anything = true;
                        }
                    },
                    Entity::Splitter { .. } => {},
                    Entity::Inserter { info, .. } => match info {
                        InserterInfo::NotAttached { .. } => {},
                        InserterInfo::Attached {
                            info: attached_inserter,
                            ..
                        } => match attached_inserter {
                            AttachedInserter::BeltStorage { id, belt_pos } => {
                                if *id == old_id && belt_pos_earliest <= *belt_pos {
                                    *id = new_id;
                                    found_anything = true;
                                }
                            },
                            AttachedInserter::BeltBelt { item, inserter } => todo!(),
                            AttachedInserter::StorageStorage { .. } => {},
                        },
                    },
                }
            }

            found_anything
        });

        self.belt_lookup
            .belt_id_to_chunks
            .entry(new_id)
            .or_default()
            .extend(old_chunks_filtered);

        let mut cascading_updates = vec![try_instantiating_inserters_for_belt_cascade(new_id)];
        while let Some(update) = cascading_updates.pop() {
            (update.update)(self, sim_state, &mut cascading_updates, data_store);
        }
    }

    pub fn update_belt_id(
        &mut self,
        sim_state: &mut SimulationState<ItemIdxType, RecipeIdxType>,
        old_id: BeltTileId<ItemIdxType>,
        new_id: BeltTileId<ItemIdxType>,
        data_store: &DataStore<ItemIdxType, RecipeIdxType>,
    ) {
        info!("Change belt_id {:?} to {:?}", old_id, new_id);
        if let Some(waiting) = self.to_instantiate_by_belt.remove(&old_id) {
            self.to_instantiate_by_belt
                .entry(new_id)
                .or_default()
                .extend(waiting);
        }
        // Do it for ALL belt_pos
        self.update_belt_id_after(sim_state, old_id, new_id, 0, data_store);
    }

    pub fn modify_belt_pos(&mut self, id_to_change: BeltTileId<ItemIdxType>, sub: bool, offs: u16) {
        let chunks = self.belt_lookup.belt_id_to_chunks.get_mut(&id_to_change);

        let num_chunks = chunks.as_ref().map(|v| v.len()).unwrap_or(0);
        info!("Checking {} chunks to modify pos", num_chunks);
        if num_chunks > 150 {
            warn!("Having to check a lot of chunks: {}", num_chunks);
        }

        if let Some(chunks) = chunks {
            chunks.retain(|chunk_pos| {
                let chunk = self
                    .chunks
                    .get_mut(chunk_pos.0, chunk_pos.1)
                    .expect("Ungenerated chunk in belt map!");

                let mut found_anything = false;
                for entity in &mut chunk.entities {
                    match entity {
                        Entity::Beacon { .. } => {},
                        Entity::Lab { .. } => {},
                        Entity::SolarPanel { .. } => {},
                        Entity::Assembler { .. } => {},
                        Entity::PowerPole { .. } => {},
                        Entity::Chest { .. } => {},
                        Entity::Roboport { .. } => {},
                        Entity::FluidTank { .. } => {},
                        Entity::Belt { id, belt_pos, .. }
                        | Entity::Underground { id, belt_pos, .. } => {
                            if *id == id_to_change {
                                found_anything = true;
                                if sub {
                                    *belt_pos =
                                        belt_pos.checked_sub(offs).expect("belt_pos wrapped!");
                                } else {
                                    *belt_pos =
                                        belt_pos.checked_add(offs).expect("belt_pos wrapped!");
                                }
                            }
                        },
                        Entity::Splitter { .. } => {},
                        Entity::Inserter { info, .. } => match info {
                            InserterInfo::NotAttached { .. } => {},
                            InserterInfo::Attached {
                                info: attached_inserter,
                                ..
                            } => match attached_inserter {
                                AttachedInserter::BeltStorage { id, belt_pos, .. } => {
                                    if *id == id_to_change {
                                        found_anything = true;
                                        if sub {
                                            *belt_pos = belt_pos
                                                .checked_sub(offs)
                                                .expect("belt_pos wrapped!");
                                        } else {
                                            *belt_pos = belt_pos
                                                .checked_add(offs)
                                                .expect("belt_pos wrapped!");
                                        }
                                    }
                                },
                                AttachedInserter::BeltBelt { item, inserter } => todo!(),
                                AttachedInserter::StorageStorage { .. } => {},
                            },
                        },
                    }
                }
                found_anything
            });
        }
    }

    pub fn check_inserters(&self, sim_state: &SimulationState<ItemIdxType, RecipeIdxType>) {
        #[cfg(debug_assertions)]
        {
            self.get_chunks()
                .flat_map(|chunk| chunk.entities.iter())
                .filter_map(|e| match e {
                    Entity::Inserter {
                        info: InserterInfo::Attached { info, .. },
                        ..
                    } => Some(*info),
                    _ => None,
                })
                .all(|info| {
                    match info {
                        AttachedInserter::BeltStorage { id, belt_pos } => {
                            let Some(_) = sim_state
                                .factory
                                .belts
                                .get_inserter_info_at(id, belt_pos) else {
                                    panic!("No inserter at pos {} for belt {:?}, which has inserters at {:?}", belt_pos, id, sim_state
                                    .factory
                                    .belts.get_inserter_positions(id));
                                };
                        },
                        AttachedInserter::BeltBelt { item, inserter } => {
                            // TODO:
                        },
                        AttachedInserter::StorageStorage { item, inserter } => {
                            // TODO:
                        },
                    }
                    true
                });
        }
    }

    #[must_use]
    pub fn get_chunk_for_tile(&self, pos: Position) -> Option<&Chunk<ItemIdxType, RecipeIdxType>> {
        self.chunks
            .get(pos.x / i32::from(CHUNK_SIZE), pos.y / i32::from(CHUNK_SIZE))
    }

    fn get_chunk_for_tile_mut(
        &mut self,
        pos: Position,
    ) -> Option<&mut Chunk<ItemIdxType, RecipeIdxType>> {
        self.chunks
            .get_mut(pos.x / i32::from(CHUNK_SIZE), pos.y / i32::from(CHUNK_SIZE))
    }

    fn get_chunk_pos_for_tile(&self, pos: Position) -> (i32, i32) {
        (pos.x / i32::from(CHUNK_SIZE), pos.y / i32::from(CHUNK_SIZE))
    }

    fn get_chunk_mut(
        &mut self,
        chunk: (i32, i32),
    ) -> Option<&mut Chunk<ItemIdxType, RecipeIdxType>> {
        self.chunks.get_mut(chunk.0, chunk.1)
    }

    pub fn is_powered_by(
        &self,
        sim_state: &SimulationState<ItemIdxType, RecipeIdxType>,
        entity_pos: Position,
        entity_size: (u16, u16),
        data_store: &DataStore<ItemIdxType, RecipeIdxType>,
    ) -> Option<Position> {
        self.get_entities_colliding_with(
            Position {
                x: entity_pos.x - i32::from(data_store.max_power_search_range),
                y: entity_pos.y - i32::from(data_store.max_power_search_range),
            },
            (
                2 * data_store.max_power_search_range as u16 + entity_size.0,
                2 * data_store.max_power_search_range as u16 + entity_size.1,
            ),
            data_store,
        )
        .into_iter()
        .find_map(|e| match e {
            Entity::PowerPole { ty, pos, .. } => {
                if sim_state
                    .factory
                    .power_grids
                    .pole_pos_to_grid_id
                    .get(pos)
                    .is_none()
                {
                    // This is a power pole that does not actually exist anymore
                    // TODO: This is a hack :/
                    return None;
                }

                let power_range = data_store.power_pole_data[usize::from(*ty)].power_range as u16;
                let size = data_store.power_pole_data[usize::from(*ty)].size;
                if entity_pos.overlap(
                    entity_size,
                    Position {
                        x: pos.x - i32::from(power_range),
                        y: pos.y - i32::from(power_range),
                    },
                    (
                        2 * power_range + size.0 as u16,
                        2 * power_range + size.1 as u16,
                    ),
                ) {
                    Some(*pos)
                } else {
                    None
                }
            },
            _ => None,
        })
    }

    #[cfg(feature = "client")]
    pub fn get_area_colors(
        &self,
        x_range_tiles: Range<i32>,
        y_range_tiles: Range<i32>,
        pixel_to_tile: usize,
        data: &mut [Color32],
        data_store: &DataStore<ItemIdxType, RecipeIdxType>,
    ) {
        use std::cmp::max;

        let full_len = data.len();

        let width_tiles = x_range_tiles.len();

        assert!(x_range_tiles.start % pixel_to_tile as i32 == 0);
        assert!(x_range_tiles.end % pixel_to_tile as i32 == 0);
        assert!(y_range_tiles.start % pixel_to_tile as i32 == 0);
        assert!(y_range_tiles.end % pixel_to_tile as i32 == 0);
        assert!(width_tiles % pixel_to_tile == 0);

        let width_pixels = width_tiles / pixel_to_tile;

        let height_tiles = y_range_tiles.len();
        let height_pixels = height_tiles / pixel_to_tile;

        assert_eq!(width_pixels * height_pixels, full_len);

        // More chunks than threads to allow work stealing, to avoid having all the work end up on a single thread
        let num_threads = rayon::current_num_threads() * 8;
        let chunk_size = height_pixels.div_ceil(num_threads) * width_pixels;

        // We use the extent of the world here, to skip looking for chunks that can neveer exist in the first place
        let [x_extent, y_extent] = self.extent_chunk_positions();

        // Since building can span chunk boundries, we also need to check chunks outside the simple chunk area
        // TODO: This depends on the size of the largest building
        const OVERLAP: i32 = 1;
        let chunk_x_start = max(
            x_extent.start,
            (x_range_tiles.start / i32::from(CHUNK_SIZE)) - OVERLAP,
        );
        let chunk_x_end = min(
            x_extent.end - 1,
            (x_range_tiles.end / i32::from(CHUNK_SIZE)) + OVERLAP,
        );

        data.par_chunks_mut(chunk_size)
            .enumerate()
            .for_each(|(i, data)| {
                profiling::scope!("par_chunks_mut", format!("Index: {i}"));
                let start_index = i * chunk_size;
                let end_index = start_index + data.len();

                assert_eq!(start_index % width_pixels, 0);
                assert_eq!(end_index % width_pixels, 0);
                let my_y_start_pixel = start_index / width_pixels;
                let my_y_end_pixel = end_index / width_pixels;

                let my_y_start_tile =
                    (my_y_start_pixel * pixel_to_tile) as i32 + y_range_tiles.start;
                let my_y_end_tile = (my_y_end_pixel * pixel_to_tile) as i32 + y_range_tiles.start;

                let chunk_y_start = my_y_start_tile / i32::from(CHUNK_SIZE);
                let chunk_y_end = my_y_end_tile / i32::from(CHUNK_SIZE);


                {
                    profiling::scope!("Get Floor");
                    for x in x_range_tiles.clone().step_by(pixel_to_tile) {
                        for y in (my_y_start_tile..my_y_end_tile).step_by(pixel_to_tile) {
                            let pixel_index: usize = (usize::try_from(y - my_y_start_tile)
                                .expect(
                                    "Since y is in range, this should always be positive",
                                )
                                / pixel_to_tile)
                                * width_pixels
                                + usize::try_from(x - x_range_tiles.start).expect(
                                    "Since x is in range, this should always be positive",
                                ) / pixel_to_tile;

                            let floor_color = self.get_floor_color(Position { x, y }, data_store);

                            data[pixel_index] = floor_color;
                        }
                    }
                }

                {
                    profiling::scope!("Handle Entities by chunk");
                    // Since building can span chunk boundries, we also need to check chunks outside the simple chunk area
                    let y_range = max(y_extent.start, chunk_y_start - OVERLAP)..=min(y_extent.end - 1, chunk_y_end + OVERLAP);
                    for chunk_x in chunk_x_start..=chunk_x_end {
                        for chunk_y in y_range.clone() {
                            let chunk = {
                                self.get_chunk(chunk_x, chunk_y)
                            };
                            let Some(chunk) = chunk else {
                                continue;
                            };

                            let base_pos = [
                                chunk_x * i32::from(CHUNK_SIZE),
                                chunk_y * i32::from(CHUNK_SIZE),
                            ];

                            if let Some(arr) = &chunk.chunk_tile_to_entity_into {
                                for x in 0..CHUNK_SIZE {
                                    let world_x = i32::from(x) + base_pos[0];
                                    if world_x % i32::try_from(pixel_to_tile).unwrap() != 0 {
                                        continue;
                                    }
                                    for y in 0..CHUNK_SIZE {
                                        let world_y = i32::from(y) + base_pos[1];
                                        if world_y % i32::try_from(pixel_to_tile).unwrap() != 0 {
                                            continue;
                                        }

                                        if !x_range_tiles.contains(&world_x) || !(my_y_start_tile..my_y_end_tile).contains(&world_y) {
                                            continue;
                                        }


                                        let idx = arr[usize::from(x)][usize::from(y)];
                                        if usize::from(idx) < chunk.entities.len() {
                                            let color = chunk.entities[usize::from(idx)].get_map_color(data_store);
                                            let pixel_index: usize = (usize::try_from(world_y - my_y_start_tile)
                                                .expect(
                                                    "Since y is in range, this should always be positive",
                                                )
                                                / pixel_to_tile)
                                                * width_pixels
                                                + usize::try_from(world_x - x_range_tiles.start).expect(
                                                    "Since x is in range, this should always be positive",
                                                ) / pixel_to_tile;
                                            if pixel_index >= data.len() {
                                                // FIXME: This should not be needed :/
                                                continue;
                                            }
                                            data[pixel_index] = color;
                                        } else {
                                            // No entity here, nothing to do
                                        }
                                    }
                                }
                            } else {
                                // This chunk does not contain any entities
                                assert!(chunk.entities.is_empty());
                            }

                            for entity in chunk.get_entities() {
                                let e_pos = entity.get_pos();
                                let e_size = entity.get_entity_size(data_store);

                                if e_pos.x + i32::from(e_size.0) <= base_pos[0] + i32::from(CHUNK_SIZE) {
                                    // Handled by the array loop
                                    continue;
                                }
                                if e_pos.y + i32::from(e_size.1) <= base_pos[1] + i32::from(CHUNK_SIZE) {
                                    // Handled by the array loop
                                    continue;
                                }

                                let color = entity.get_map_color(data_store);

                                for x in base_pos[0] + i32::from(CHUNK_SIZE)..(e_pos.x + i32::from(e_size.0)) {
                                    if x % i32::try_from(pixel_to_tile).unwrap() != 0 {
                                        continue;
                                    }
                                    for y in base_pos[1] + i32::from(CHUNK_SIZE)..(e_pos.y + i32::from(e_size.1)) {
                                        if y % i32::try_from(pixel_to_tile).unwrap() != 0 {
                                            continue;
                                        }

                                        if !x_range_tiles.contains(&x)
                                            || !(my_y_start_tile..my_y_end_tile).contains(&y)
                                        {
                                            continue;
                                        }

                                        assert!((my_y_start_pixel..my_y_end_pixel).contains(
                                            &(usize::try_from(y - my_y_start_tile).expect(
                                                "Since y is in range, this should always be positive",
                                            ) / pixel_to_tile + my_y_start_pixel)
                                        ), "{:?} does not contain {:?}", (my_y_start_pixel..my_y_end_pixel), usize::try_from(y - my_y_start_tile).expect(
                                                "Since y is in range, this should always be positive",
                                            ) / pixel_to_tile + my_y_start_pixel);

                                        let pixel_index: usize = (usize::try_from(y - my_y_start_tile)
                                            .expect(
                                                "Since y is in range, this should always be positive",
                                            )
                                            / pixel_to_tile)
                                            * width_pixels
                                            + usize::try_from(x - x_range_tiles.start).expect(
                                                "Since x is in range, this should always be positive",
                                            ) / pixel_to_tile;


                                        if pixel_index >= data.len() {
                                            // FIXME: This should not be needed :/
                                            continue;
                                        }

                                        data[pixel_index] = color;
                                    }
                                }
                            }
                        }
                    }
                }
            });
    }

    #[cfg(feature = "client")]
    pub fn get_entity_color(
        &self,
        pos: Position,
        data_store: &DataStore<ItemIdxType, RecipeIdxType>,
    ) -> Color32 {
        // self.get_tile_arr_debug_color(pos)
        self.get_entity_at(pos, data_store)
            .map(|e| e.get_map_color(data_store))
            .unwrap_or(self.get_floor_color(pos, data_store))
    }

    #[cfg(feature = "client")]
    fn get_tile_arr_debug_color(&self, pos: Position) -> Color32 {
        self.get_chunk_for_tile(pos)
            .map(|chunk| {
                let arr_val = chunk
                    .chunk_tile_to_entity_into
                    .as_ref()
                    .map(|arr| {
                        arr[pos.x.rem_euclid(i32::from(CHUNK_SIZE)) as usize]
                            [pos.y.rem_euclid(i32::from(CHUNK_SIZE)) as usize]
                    })
                    .unwrap_or(EMPTY);

                match arr_val {
                    OTHER_CHUNK => Color32::PURPLE,
                    EMPTY => Color32::BLACK,
                    index => Color32::from_gray(u8::MAX - index),
                }
            })
            .unwrap_or(Color32::RED)
    }

    #[cfg(feature = "client")]
    pub fn get_floor_color(
        &self,
        pos: Position,
        data_store: &DataStore<ItemIdxType, RecipeIdxType>,
    ) -> Color32 {
        let chunk = self.get_chunk_for_tile(pos);

        chunk
            .map(|chunk| {
                if let Some(ore) = self.get_original_ore_at_pos(pos) {
                    if ore.1 > 0 {
                        // TODO ORE COLOR
                        Color32::LIGHT_BLUE
                    } else {
                        // TODO: Get floor color
                        Color32::from_hex("#3f3f3f").unwrap()
                    }
                } else {
                    // TODO: Get floor color
                    Color32::from_hex("#3f3f3f").unwrap()
                }
            })
            .unwrap_or(Color32::BLACK)
    }

    pub fn get_entities_colliding_with<'a, 'b>(
        &'a self,
        pos: Position,
        size: (u16, u16),
        data_store: &'b DataStore<ItemIdxType, RecipeIdxType>,
    ) -> impl IntoIterator<Item = &'a Entity<ItemIdxType, RecipeIdxType>, IntoIter: Clone>
    + use<'a, 'b, ItemIdxType, RecipeIdxType> {
        let max_size = data_store.max_entity_size;

        let bb_top_left = (pos.x - i32::from(max_size.0), pos.y - i32::from(max_size.1));

        let bb_bottom_right = (pos.x + i32::from(size.0), pos.y + i32::from(size.1));

        let chunk_range_x =
            (bb_top_left.0 / i32::from(CHUNK_SIZE))..=(bb_bottom_right.0 / i32::from(CHUNK_SIZE));
        let chunk_range_y =
            (bb_top_left.1 / i32::from(CHUNK_SIZE))..=(bb_bottom_right.1 / i32::from(CHUNK_SIZE));

        debug_assert!(chunk_range_x.clone().count() >= 1);
        debug_assert!(chunk_range_y.clone().count() >= 1);

        chunk_range_x
            .cartesian_product(chunk_range_y)
            .filter_map(|(chunk_x, chunk_y)| self.chunks.get(chunk_x, chunk_y))
            .flat_map(move |chunk| chunk.entities.iter())
            .filter(move |e| {
                let e_pos = e.get_pos();
                let e_size = e.get_entity_size(data_store);

                pos.overlap(size, e_pos, (e_size.0.into(), e_size.1.into()))
            })
    }

    pub fn get_entity_at_mut(
        &mut self,
        pos: Position,
        data_store: &DataStore<ItemIdxType, RecipeIdxType>,
    ) -> Option<&mut Entity<ItemIdxType, RecipeIdxType>> {
        let max_size = data_store.max_entity_size;

        let chunk = self.get_chunk_for_tile_mut(pos);
        let state = match &chunk {
            Some(chunk) => arr_val_to_state(
                chunk.entities.len(),
                chunk
                    .chunk_tile_to_entity_into
                    .as_ref()
                    .map(|arr| {
                        arr[usize::try_from(pos.x.rem_euclid(i32::from(CHUNK_SIZE))).unwrap()]
                            [usize::try_from(pos.y.rem_euclid(i32::from(CHUNK_SIZE))).unwrap()]
                    })
                    .unwrap_or(EMPTY),
            ),
            None => return None,
        };

        match state {
            ChunkTileState::Index(idx) => {
                return Some(&mut self.get_chunk_for_tile_mut(pos).unwrap().entities[idx]);
            },
            ChunkTileState::Empty => return None,
            ChunkTileState::OtherChunk | ChunkTileState::EmptyOrOtherChunk => {
                // x axis
                let x_positions = iter::once(pos.x)
                    .chain(iter::once(pos.x - 1))
                    .chain(
                        iter::successors(
                            Some(
                                pos.x.next_multiple_of(i32::from(CHUNK_SIZE))
                                    - i32::from(CHUNK_SIZE),
                            ),
                            |x| Some(x - i32::from(CHUNK_SIZE)),
                        )
                        .map(|v| v - 1),
                    )
                    // .take(5);
                    .take_while(|v| v.abs_diff(pos.x) <= u32::from(max_size.0));
                let y_positions = iter::once(pos.y)
                    .chain(iter::once(pos.y - 1))
                    .chain(
                        iter::successors(
                            Some(
                                pos.y.next_multiple_of(i32::from(CHUNK_SIZE))
                                    - i32::from(CHUNK_SIZE),
                            ),
                            |y| Some(y - i32::from(CHUNK_SIZE)),
                        )
                        .map(|v| v - 1),
                    )
                    // .take(5);
                    .take_while(|v| v.abs_diff(pos.y) <= u32::from(max_size.1));

                assert!(x_positions.clone().all(|x| x <= pos.x));
                assert!(y_positions.clone().all(|y| y <= pos.y));
                for x in x_positions {
                    for y in y_positions.clone() {
                        let next_chunk_pos = Position { x, y };

                        if next_chunk_pos == pos {
                            continue;
                        }

                        info!("Checking other chunk pos: {:?}", next_chunk_pos);
                        // FIXME: This sucks, but the borrow checker is unhappy otherwise
                        match self.get_entity_at_mut_no_recursion(next_chunk_pos) {
                            Some(e) => {
                                if pos.contained_in(e.get_pos(), e.get_entity_size(data_store)) {
                                    return self.get_entity_at_mut_no_recursion(next_chunk_pos);
                                }
                            },
                            None => {},
                        }
                    }
                }

                // assert!(
                //     chunk.is_none(),
                //     "We indicated there to be an entity in another chunk, but there wasn't"
                // );
                return None;
            },
        };

        unreachable!()
    }

    pub fn get_entity_at(
        &self,
        pos: Position,
        data_store: &DataStore<ItemIdxType, RecipeIdxType>,
    ) -> Option<&Entity<ItemIdxType, RecipeIdxType>> {
        let max_size = data_store.max_entity_size;

        let chunk = self.get_chunk_for_tile(pos);
        let state = match chunk {
            Some(chunk) => arr_val_to_state(
                chunk.entities.len(),
                chunk
                    .chunk_tile_to_entity_into
                    .as_ref()
                    .map(|arr| {
                        arr[usize::try_from(pos.x.rem_euclid(i32::from(CHUNK_SIZE))).unwrap()]
                            [usize::try_from(pos.y.rem_euclid(i32::from(CHUNK_SIZE))).unwrap()]
                    })
                    .unwrap_or(EMPTY),
            ),
            None => return None,
        };

        match state {
            ChunkTileState::Index(idx) => Some(&chunk.unwrap().entities[idx]),
            ChunkTileState::Empty => None,
            ChunkTileState::OtherChunk | ChunkTileState::EmptyOrOtherChunk => {
                // x axis
                let x_positions = iter::once(pos.x)
                    .chain(iter::once(pos.x - 1))
                    .chain(
                        iter::successors(
                            Some(
                                pos.x.next_multiple_of(i32::from(CHUNK_SIZE))
                                    - i32::from(CHUNK_SIZE),
                            ),
                            |x| Some(x - i32::from(CHUNK_SIZE)),
                        )
                        .map(|v| v - 1),
                    )
                    // .take(5);
                    .take_while(|v| v.abs_diff(pos.x) <= u32::from(max_size.0));
                let y_positions = iter::once(pos.y)
                    .chain(iter::once(pos.y - 1))
                    .chain(
                        iter::successors(
                            Some(
                                pos.y.next_multiple_of(i32::from(CHUNK_SIZE))
                                    - i32::from(CHUNK_SIZE),
                            ),
                            |y| Some(y - i32::from(CHUNK_SIZE)),
                        )
                        .map(|v| v - 1),
                    )
                    // .take(5);
                    .take_while(|v| v.abs_diff(pos.y) <= u32::from(max_size.1));

                assert!(x_positions.clone().all(|x| x <= pos.x));
                assert!(y_positions.clone().all(|y| y <= pos.y));
                for x in x_positions {
                    for y in y_positions.clone() {
                        let next_chunk_pos = Position { x, y };

                        if next_chunk_pos == pos {
                            continue;
                        }

                        info!("Checking other chunk pos: {:?}", next_chunk_pos);
                        match self.get_entity_at_no_recursion(next_chunk_pos) {
                            Some(e) => {
                                if pos.contained_in(e.get_pos(), e.get_entity_size(data_store)) {
                                    return Some(e);
                                }
                            },
                            None => {},
                        }
                    }
                }

                assert!(
                    chunk.is_none(),
                    "We indicated there to be an entity in another chunk, but there wasn't"
                );
                return None;
            },
        }
    }

    fn get_entity_at_no_recursion(
        &self,
        pos: Position,
    ) -> Option<&Entity<ItemIdxType, RecipeIdxType>> {
        let chunk = self.get_chunk_for_tile(pos);
        let state = match chunk {
            Some(chunk) => arr_val_to_state(
                chunk.entities.len(),
                chunk
                    .chunk_tile_to_entity_into
                    .as_ref()
                    .map(|arr| {
                        arr[usize::try_from(pos.x.rem_euclid(i32::from(CHUNK_SIZE))).unwrap()]
                            [usize::try_from(pos.y.rem_euclid(i32::from(CHUNK_SIZE))).unwrap()]
                    })
                    .unwrap_or(EMPTY),
            ),
            None => ChunkTileState::EmptyOrOtherChunk,
        };

        match state {
            ChunkTileState::Index(idx) => Some(&chunk.unwrap().entities[idx]),
            ChunkTileState::Empty => None,
            ChunkTileState::OtherChunk | ChunkTileState::EmptyOrOtherChunk => {
                return None;
            },
        }
    }

    fn get_entity_at_mut_no_recursion(
        &mut self,
        pos: Position,
    ) -> Option<&mut Entity<ItemIdxType, RecipeIdxType>> {
        let chunk = self.get_chunk_for_tile_mut(pos);
        let state = match &chunk {
            Some(chunk) => arr_val_to_state(
                chunk.entities.len(),
                chunk
                    .chunk_tile_to_entity_into
                    .as_ref()
                    .map(|arr| {
                        arr[usize::try_from(pos.x.rem_euclid(i32::from(CHUNK_SIZE))).unwrap()]
                            [usize::try_from(pos.y.rem_euclid(i32::from(CHUNK_SIZE))).unwrap()]
                    })
                    .unwrap_or(EMPTY),
            ),
            None => ChunkTileState::EmptyOrOtherChunk,
        };

        match state {
            ChunkTileState::Index(idx) => Some(&mut chunk.unwrap().entities[idx]),
            ChunkTileState::Empty => None,
            ChunkTileState::OtherChunk | ChunkTileState::EmptyOrOtherChunk => {
                return None;
            },
        }
    }

    pub fn mutate_entities_colliding_with<'a, 'b>(
        &'a mut self,
        pos: Position,
        size: (u16, u16),
        data_store: &'b DataStore<ItemIdxType, RecipeIdxType>,
        mut f: impl FnMut(&mut Entity<ItemIdxType, RecipeIdxType>) -> ControlFlow<(), ()>,
    ) {
        let max_size = data_store.max_entity_size;

        let bb_top_left = (pos.x - i32::from(max_size.0), pos.y - i32::from(max_size.1));

        let bb_bottom_right = (pos.x + i32::from(size.0), pos.y + i32::from(size.1));

        let chunk_range_x =
            (bb_top_left.0 / i32::from(CHUNK_SIZE))..=(bb_bottom_right.0 / i32::from(CHUNK_SIZE));
        let chunk_range_y =
            (bb_top_left.1 / i32::from(CHUNK_SIZE))..=(bb_bottom_right.1 / i32::from(CHUNK_SIZE));

        debug_assert!(chunk_range_x.clone().count() >= 1);
        debug_assert!(chunk_range_y.clone().count() >= 1);

        for chunk_x in chunk_range_x {
            for chunk_y in chunk_range_y.clone() {
                let Some(chunk) = self.chunks.get_mut(chunk_x, chunk_y) else {
                    continue;
                };

                for e in chunk.entities.iter_mut() {
                    let e_pos = e.get_pos();
                    let e_size = e.get_entity_size(data_store);

                    if (pos.x + i32::from(size.0)) <= e_pos.x
                        || (pos.y + i32::from(size.1)) <= e_pos.y
                        || (pos.x) >= (e_pos.x + i32::from(e_size.0))
                        || (pos.y) >= (e_pos.y + i32::from(e_size.1))
                    {
                        continue;
                    }

                    match f(e) {
                        ControlFlow::Continue(_) => continue,
                        ControlFlow::Break(_) => break,
                    }
                }
            }
        }
    }

    // TODO: What does this return?
    #[profiling::function]
    pub fn remove_entity_at(
        &mut self,
        pos: Position,
        sim_state: &mut SimulationState<ItemIdxType, RecipeIdxType>,
        data_store: &DataStore<ItemIdxType, RecipeIdxType>,
    ) {
        let entity = self.get_entity_at(pos, data_store);

        let mut cascading_updates = vec![];

        if let Some(entity) = entity {
            let e_pos = entity.get_pos();
            let e_size = entity.get_entity_size(data_store);
            let max_inserter_range = data_store.max_inserter_search_range;

            match entity {
                Entity::FluidTank { ty, pos, rotation } => {
                    sim_state.factory.fluid_store.remove_fluid_box(
                        *pos,
                        &mut sim_state.factory.chests,
                        &mut sim_state.factory.storage_storage_inserters,
                        data_store,
                    );
                },
                Entity::Beacon {
                    pos,
                    ty,
                    pole_position,
                    modules,
                } => {
                    if let Some((pole_pos, idx)) = *pole_position {
                        sim_state
                            .factory
                            .power_grids
                            .remove_beacon(pole_pos, idx, data_store);
                    } else {
                        // This was not connected, nothing to do
                    }
                },
                Entity::Lab {
                    pos,
                    ty,
                    modules,
                    pole_position,
                } => {
                    if let Some((pole_pos, idx, store_idx)) = *pole_position {
                        sim_state.factory.power_grids.power_grids[usize::from(
                            sim_state.factory.power_grids.pole_pos_to_grid_id[&pole_pos],
                        )]
                        .remove_lab(pole_pos, idx, data_store);

                        let lab_size = data_store.lab_info[usize::from(*ty)].size;

                        cascading_updates.push(removal_of_possible_inserter_connection(
                            *pos, lab_size, data_store,
                        ));
                    } else {
                        // This was not connected, nothing to do
                    }
                },
                Entity::SolarPanel {
                    pos,
                    ty,
                    pole_position,
                } => {
                    if let Some((pole_pos, idx)) = *pole_position {
                        sim_state.factory.power_grids.power_grids[usize::from(
                            sim_state.factory.power_grids.pole_pos_to_grid_id[&pole_pos],
                        )]
                        .remove_solar_panel(pole_pos, idx, data_store);
                    } else {
                        // This was not connected, nothing to do
                    }
                },
                Entity::Assembler {
                    ty,
                    pos,
                    info,
                    modules,
                    rotation,
                } => match info {
                    AssemblerInfo::UnpoweredNoRecipe
                    | AssemblerInfo::Unpowered(_)
                    | AssemblerInfo::PoweredNoRecipe(_) => {
                        // Nothing to do besides removing the entity
                    },
                    AssemblerInfo::Powered {
                        id: assembler_id,
                        pole_position,
                        weak_index,
                    } => {
                        // TODO:
                        let assembler_removal_info = sim_state.factory.power_grids.power_grids
                            [assembler_id.grid as usize]
                            .remove_assembler(
                                *assembler_id,
                                *pole_position,
                                *weak_index,
                                data_store,
                            );
                    },
                },
                Entity::PowerPole {
                    ty,
                    pos,
                    connected_power_poles,
                } => {
                    let ty = *ty;

                    let old_id = sim_state.factory.power_grids.pole_pos_to_grid_id[pos];

                    let (
                        _poles_which_changed,
                        machines_which_changed,
                        no_longer_connected_entity_positions,
                    ) = sim_state.factory.power_grids.remove_pole(*pos, data_store);

                    {
                        profiling::scope!("Apply Index updates");
                        for index_update in machines_which_changed {
                            self.get_entity_at_mut(index_update.position, data_store)
                                .map(|e| {
                                    match (e, index_update.new_pg_entity.clone()) {
                                        (
                                            Entity::Assembler {
                                                info:
                                                    AssemblerInfo::Powered {
                                                        id,
                                                        pole_position,
                                                        weak_index,
                                                    },
                                                ..
                                            },
                                            crate::power::power_grid::PowerGridEntity::Assembler {
                                                ty,
                                                recipe,
                                                index,
                                            },
                                        ) => {
                                            assert_eq!(id.recipe, recipe);

                                            assert_eq!(id.grid, old_id);

                                            id.assembler_index = index;
                                            id.grid = index_update.new_grid;
                                        },
                                        (Entity::Beacon { .. }, PowerGridEntity::Beacon { .. }) => {
                                            // Do nothing. The beacon only stores the pole_position, which has not changed
                                        },
                                        (entity, power_grid_entity) => {
                                            unreachable!(
                                                "Expected {power_grid_entity:?} found {entity:?}"
                                            )
                                        },
                                    }
                                });

                            // let assembler_size: (u16, u16) =
                            //     data_store.assembler_info[usize::from(*ty)].size(*rotation);

                            // let inserter_search_area = (
                            //     Position {
                            //         x: index_update.position.x - i32::from(max_inserter_range),
                            //         y: index_update.position.y - i32::from(max_inserter_range),
                            //     },
                            //     (
                            //         2 * max_inserter_range as u16 + assembler_size.0,
                            //         2 * max_inserter_range as u16 + assembler_size.1,
                            //     ),
                            // );

                            // let new_storages: Vec<_> = match index_update.new_pg_entity {
                            //     PowerGridEntity::Assembler { ty, recipe, index } => data_store
                            //         .recipe_to_items[&recipe]
                            //         .iter()
                            //         .map(|(_dir, item)| {
                            //             (
                            //                 item,
                            //                 Storage::Assembler {
                            //                     grid: index_update.new_grid,
                            //                     recipe_idx_with_this_item: data_store
                            //                         .recipe_to_translated_index[&(recipe, *item)],
                            //                     index,
                            //                 },
                            //             )
                            //         })
                            //         .collect(),
                            //     PowerGridEntity::Lab { index, ty } => todo!(),
                            //     PowerGridEntity::LazyPowerProducer { item, index } => {
                            //         todo!("Expand Storage type")
                            //     },
                            //     PowerGridEntity::SolarPanel { .. } => {
                            //         vec![]
                            //     },
                            //     PowerGridEntity::Accumulator { .. } => {
                            //         vec![]
                            //     },
                            //     PowerGridEntity::Beacon { .. } => {
                            //         vec![]
                            //     },
                            // };

                            // assert!(
                            //     new_storages
                            //         .iter()
                            //         .map(|(item, _storage)| item)
                            //         .all_unique()
                            // );

                            // self.mutate_entities_colliding_with(
                            //     inserter_search_area.0,
                            //     inserter_search_area.1,
                            //     data_store,
                            //     |e| {
                            //         match e {
                            //             Entity::Inserter {
                            //                 pos,
                            //                 direction,
                            //                 info: InserterInfo::NotAttached { .. },
                            //                 ..
                            //             } => {
                            //                 // Nothing to do
                            //             },
                            //             Entity::Inserter {
                            //                 pos,
                            //                 direction,
                            //                 info,
                            //                 ..
                            //             } => {
                            //                 let (start_pos, end_pos) =
                            //                     calculate_inserter_positions(*pos, *direction);

                            //                 if start_pos
                            //                     .contained_in(index_update.position, assembler_size)
                            //                     || end_pos.contained_in(
                            //                         index_update.position,
                            //                         assembler_size,
                            //                     )
                            //                 {
                            //                     // This Inserter is connected to the entity we are removing!
                            //                     match info {
                            //                         InserterInfo::NotAttached {
                            //                             start_pos,
                            //                             end_pos,
                            //                         } => {
                            //                             unreachable!()
                            //                         },
                            //                         InserterInfo::Attached {
                            //                             info: attached_inserter,
                            //                             ..
                            //                         } => {
                            //                             match attached_inserter {
                            //                                 AttachedInserter::BeltStorage {
                            //                                     id,
                            //                                     belt_pos,
                            //                                 } => sim_state
                            //                                     .factory
                            //                                     .belts
                            //                                     .remove_inserter(*id, *belt_pos),
                            //                                 AttachedInserter::BeltBelt {
                            //                                     item,
                            //                                     inserter,
                            //                                 } => todo!(),
                            //                                 AttachedInserter::StorageStorage {
                            //                                     ..
                            //                                 } => {
                            //                                     todo!()
                            //                                 },
                            //                             }
                            //                             todo!();
                            //                         },
                            //                     }
                            //                 }
                            //             },

                            //             _ => {},
                            //         }
                            //         ControlFlow::Continue(())
                            //     },
                            // );
                        }
                    }

                    for unconnected_position in no_longer_connected_entity_positions {
                        // FIXME: Hardcoded size
                        let pole_pos = self.is_powered_by(
                            &sim_state,
                            unconnected_position,
                            (3, 3),
                            data_store,
                        );

                        self.get_entity_at_mut(unconnected_position, data_store)
                            .map(|e| {
                                match e {
                                    Entity::Assembler {
                                        ty,
                                        pos,
                                        info,
                                        modules,
                                        rotation,
                                    } => match info {
                                        AssemblerInfo::UnpoweredNoRecipe => unreachable!(),
                                        AssemblerInfo::Unpowered(recipe) => unreachable!(),
                                        AssemblerInfo::PoweredNoRecipe(position) => unreachable!(),
                                        AssemblerInfo::Powered {
                                            id,
                                            pole_position,
                                            weak_index,
                                        } => {
                                            assert!(
                                                sim_state
                                                    .factory
                                                    .power_grids
                                                    .pole_pos_to_grid_id
                                                    .get(pole_position)
                                                    .is_none()
                                            );

                                            if let Some(new_pole_pos) = pole_pos {
                                                // FIXME: Items are lost here!
                                                let grid_id = sim_state
                                                    .factory
                                                    .power_grids
                                                    .pole_pos_to_grid_id[&new_pole_pos];

                                                let (new_id, new_weak_index) =
                                                    sim_state.factory.power_grids.power_grids
                                                        [usize::from(grid_id)]
                                                    .add_assembler(
                                                        *ty,
                                                        grid_id,
                                                        id.recipe,
                                                        &**modules,
                                                        new_pole_pos,
                                                        *pos,
                                                        data_store,
                                                    );

                                                *id = new_id;
                                                *weak_index = new_weak_index;
                                            } else {
                                                // FIXME: This will delete items!
                                                *info = AssemblerInfo::Unpowered(id.recipe)
                                            }
                                        },
                                    },
                                    Entity::PowerPole { .. } => unreachable!(),
                                    Entity::SolarPanel {
                                        pos,
                                        ty,
                                        pole_position,
                                    } => {
                                        *pole_position = None;
                                    },
                                    Entity::Beacon {
                                        ty,
                                        pos,
                                        modules,
                                        pole_position,
                                    } => {
                                        *pole_position = None;
                                    },

                                    e => unreachable!("Tried to unpower {e:?}"),
                                }
                            });
                    }
                },

                Entity::Belt {
                    pos,
                    direction,
                    ty,
                    id,
                    belt_pos,
                } => {
                    let pos = *pos;
                    let direction = *direction;

                    belt_placement::handle_belt_removal(
                        self, sim_state, *id, *belt_pos, pos, direction, data_store,
                    );
                    cascading_updates.push(removal_of_possible_inserter_connection(
                        pos,
                        (1, 1),
                        data_store,
                    ));

                    let front_pos = pos + direction;
                    if let Some(inputs) = self.belt_recieving_input_directions.get_mut(&front_pos) {
                        assert_eq!(inputs[direction.reverse()], true);
                        inputs[direction.reverse()] = false;
                    }
                },
                Entity::Underground {
                    pos,
                    underground_dir,
                    direction,
                    ty,
                    id,
                    belt_pos,
                } => {
                    let pos = *pos;
                    let direction = *direction;
                    let underground_dir = *underground_dir;

                    belt_placement::handle_underground_removal(
                        self,
                        sim_state,
                        *id,
                        *belt_pos,
                        pos,
                        direction,
                        underground_dir,
                        *ty,
                        data_store,
                    );
                    cascading_updates.push(removal_of_possible_inserter_connection(
                        pos,
                        (1, 1),
                        data_store,
                    ));

                    if underground_dir == UndergroundDir::Exit {
                        let front_pos = pos + direction;
                        if let Some(inputs) =
                            self.belt_recieving_input_directions.get_mut(&front_pos)
                        {
                            assert_eq!(inputs[direction.reverse()], true);
                            inputs[direction.reverse()] = false;
                        }
                    }
                },
                Entity::Splitter { pos, direction, id } => todo!(),

                Entity::Chest {
                    ty,
                    pos,
                    item,
                    slot_limit,
                } => {
                    if let Some((item, index)) = item {
                        let chest_removal_info = sim_state.factory.chests.stores
                            [usize_from(item.id)]
                        .remove_chest(*index);

                        let chest_size = data_store.chest_tile_sizes[usize::from(*ty)];

                        cascading_updates.push(removal_of_possible_inserter_connection(
                            *pos, chest_size, data_store,
                        ));
                    }
                },
                Entity::Roboport {
                    ty,
                    pos,
                    power_grid,
                    network,
                    id,
                } => todo!(),

                Entity::Inserter {
                    pos,
                    direction,
                    info: InserterInfo::NotAttached { .. },
                    ..
                } => {},
                Entity::Inserter {
                    ty,
                    user_movetime,
                    type_movetime,
                    pos,
                    direction,
                    info:
                        InserterInfo::Attached {
                            info: attached_inserter,
                            ..
                        },
                    ..
                } => match attached_inserter {
                    AttachedInserter::BeltStorage { id, belt_pos } => {
                        sim_state.factory.belts.remove_inserter(*id, *belt_pos);
                    },
                    AttachedInserter::BeltBelt { item, inserter } => {
                        sim_state.factory.belts.remove_belt_belt_inserter(*inserter);
                    },
                    AttachedInserter::StorageStorage { item, inserter } => {
                        sim_state.factory.storage_storage_inserters.remove_ins(
                            *item,
                            user_movetime.unwrap_or(*type_movetime).into(),
                            *inserter,
                        );
                    },
                },
            }

            let chunk = self.get_chunk_for_tile_mut(e_pos).unwrap();
            let old_idx = u8::try_from(
                chunk
                    .entities
                    .iter()
                    .position(|e| e.get_pos() == e_pos)
                    .unwrap(),
            )
            .unwrap();

            for outer in chunk.chunk_tile_to_entity_into.as_mut().unwrap().iter_mut() {
                for v in outer.iter_mut() {
                    match (*v).cmp(&old_idx) {
                        std::cmp::Ordering::Less => {
                            // We will not be moved by Vec::remove
                        },
                        std::cmp::Ordering::Equal => {
                            // Remove it
                            *v = EMPTY;
                        },
                        std::cmp::Ordering::Greater => {
                            if (chunk.entities.len() - 1) == usize::from(*v) {
                                *v = old_idx;
                            }
                        },
                    }
                }
            }

            // Actually remove the entity
            chunk.entities.swap_remove(old_idx as usize);
        } else {
            // Nothing to do
        }

        while let Some(update) = cascading_updates.pop() {
            (update.update)(self, sim_state, &mut cascading_updates, data_store);
        }
    }

    #[must_use]
    pub fn can_fit(
        &self,
        pos: Position,
        size: (u16, u16),
        data_store: &DataStore<ItemIdxType, RecipeIdxType>,
    ) -> bool {
        self.get_entities_colliding_with(pos, size, data_store)
            .into_iter()
            .next()
            .is_none()
        // let chunk_range_x = (pos.x / i32::from(CHUNK_SIZE))
        //     ..=((pos.x + i32::from(size.0) - 1) / i32::from(CHUNK_SIZE));
        // let chunk_range_y = (pos.y / i32::from(CHUNK_SIZE))
        //     ..=((pos.y + i32::from(size.1) - 1) / i32::from(CHUNK_SIZE));

        // chunk_range_x
        //     .cartesian_product(chunk_range_y)
        //     .all(
        //         |(chunk_x, chunk_y)| match self.get_chunk(chunk_x, chunk_y) {
        //             Some(chunk) => chunk.can_fit(pos, size, data_store),
        //             None => false,
        //         },
        //     )
    }

    pub fn get_power_poles_which_could_connect_to_pole_at<'a, 'b>(
        &'a self,
        pole_pos: Position,
        pole_size: (u16, u16),
        connection_range: u8,
        data_store: &'b DataStore<ItemIdxType, RecipeIdxType>,
    ) -> impl IntoIterator<Item = &'a Entity<ItemIdxType, RecipeIdxType>, IntoIter: Clone>
    + Clone
    + use<'a, 'b, ItemIdxType, RecipeIdxType> {
        self.get_entities_colliding_with(
            Position {
                x: pole_pos.x - i32::from(connection_range),
                y: pole_pos.y - i32::from(connection_range),
            },
            (
                2 * connection_range as u16 + pole_size.0 as u16,
                2 * connection_range as u16 + pole_size.1 as u16,
            ),
            data_store,
        )
        .into_iter()
        .filter(|e| matches!(e, Entity::PowerPole { .. }))
    }

    fn get_power_pole_range(
        &self,
        pole_pos: Position,
        data_store: &DataStore<ItemIdxType, RecipeIdxType>,
    ) -> (u8, (u16, u16)) {
        let Some(Entity::PowerPole { ty, .. }) = self.get_entity_at(pole_pos, data_store) else {
            unreachable!()
        };

        let range = data_store.power_pole_data[usize::from(*ty)].connection_range;
        let size = data_store.power_pole_data[usize::from(*ty)].size;

        (range, size)
    }
}

impl<ItemIdxType: IdxTrait, RecipeIdxType: IdxTrait> Default for World<ItemIdxType, RecipeIdxType> {
    fn default() -> Self {
        #[cfg(debug_assertions)]
        const WORLDSIZE_CHUNKS: u16 = 200;
        #[cfg(not(debug_assertions))]
        const WORLDSIZE_CHUNKS: u16 = 4000;
        Self::new_with_starting_area(
            Position { x: 1600, y: 1600 },
            WORLDSIZE_CHUNKS,
            WORLDSIZE_CHUNKS,
        )
    }
}

impl<ItemIdxType: IdxTrait, RecipeIdxType: IdxTrait> Chunk<ItemIdxType, RecipeIdxType> {
    #[must_use]
    pub fn get_entity_at(
        &self,
        pos: Position,
        data_store: &DataStore<ItemIdxType, RecipeIdxType>,
    ) -> Option<&Entity<ItemIdxType, RecipeIdxType>> {
        self.entities.iter().find(|e| {
            let e_pos = e.get_pos();
            let e_size = e.get_entity_size(data_store);

            pos.contained_in(e_pos, (e_size.0.into(), e_size.1.into()))
        })
    }

    pub fn get_entity_at_mut(
        &mut self,
        pos: Position,
        data_store: &DataStore<ItemIdxType, RecipeIdxType>,
    ) -> Option<&mut Entity<ItemIdxType, RecipeIdxType>> {
        self.entities.iter_mut().find(|e| {
            let e_pos = e.get_pos();
            let e_size = e.get_entity_size(data_store);

            pos.contained_in(e_pos, (e_size.0.into(), e_size.1.into()))
        })
    }

    #[must_use]
    fn can_fit(
        &self,
        pos: Position,
        size: (u16, u16),
        data_store: &DataStore<ItemIdxType, RecipeIdxType>,
    ) -> bool {
        if let Some(arr) = &self.chunk_tile_to_entity_into {
            let x_in_chunk = pos.x.rem_euclid(i32::from(CHUNK_SIZE)) as usize;
            let y_in_chunk = pos.y.rem_euclid(i32::from(CHUNK_SIZE)) as usize;
            for x in x_in_chunk
                ..min(
                    x_in_chunk + usize::from(size.0),
                    usize::from(CHUNK_SIZE) - 1,
                )
            {
                for y in y_in_chunk
                    ..min(
                        y_in_chunk + usize::from(size.1),
                        usize::from(CHUNK_SIZE) - 1,
                    )
                {
                    match arr_val_to_state(self.entities.len(), arr[x][y]) {
                        ChunkTileState::Index(_) => return false,
                        ChunkTileState::Empty => {},
                        ChunkTileState::OtherChunk => return false,
                        ChunkTileState::EmptyOrOtherChunk => return true,
                    }
                }
            }
            return true;
        } else {
            return true;
        }
    }

    #[must_use]
    pub fn get_entities(&self) -> impl Iterator<Item = &Entity<ItemIdxType, RecipeIdxType>> {
        self.entities.iter()
    }
}

#[cfg_attr(feature = "client", derive(ShowInfo), derive(GetSize))]
#[derive(Debug, Clone, Copy, serde::Deserialize, serde::Serialize, PartialEq)]
pub enum AssemblerInfo<RecipeIdxType: WeakIdxTrait = u8> {
    UnpoweredNoRecipe,
    Unpowered(Recipe<RecipeIdxType>),
    PoweredNoRecipe(Position),
    Powered {
        id: AssemblerID<RecipeIdxType>,
        pole_position: Position,
        weak_index: WeakIndex,
    },
}

#[cfg_attr(feature = "client", derive(ShowInfo), derive(GetSize))]
#[derive(Debug, Clone, Copy, serde::Deserialize, serde::Serialize, PartialEq)]
pub enum InserterInfo<ItemIdxType: WeakIdxTrait> {
    NotAttached {
        start_pos: Position,
        end_pos: Position,
    },
    Attached {
        start_pos: Position,
        end_pos: Position,
        info: AttachedInserter<ItemIdxType>,
    },
}

#[cfg_attr(feature = "client", derive(ShowInfo), derive(GetSize))]
#[derive(Debug, Clone, Copy, serde::Deserialize, serde::Serialize, PartialEq)]
pub enum InternalInserterInfo<ItemIdxType: WeakIdxTrait> {
    NotAttached {
        end_pos: Position,
    },
    Attached {
        end_pos: Position,
        info: AttachedInternalInserter<ItemIdxType>,
    },
}

#[cfg_attr(feature = "client", derive(ShowInfo), derive(GetSize))]
#[derive(Debug, Clone, Copy, serde::Deserialize, serde::Serialize, PartialEq)]
pub enum AttachedInserter<ItemIdxType: WeakIdxTrait> {
    BeltStorage {
        id: BeltTileId<ItemIdxType>,
        belt_pos: u16,
    },
    BeltBelt {
        item: Item<ItemIdxType>,
        inserter: usize,
    },
    StorageStorage {
        item: Item<ItemIdxType>,
        // TODO: Do I want to store this Identifier of calculate it on demand to save RAM?
        inserter: InserterIdentifier,
    },
}

#[cfg_attr(feature = "client", derive(ShowInfo), derive(GetSize))]
#[derive(Debug, Clone, Copy, serde::Deserialize, serde::Serialize, PartialEq)]
pub enum AttachedInternalInserter<ItemIdxType: WeakIdxTrait> {
    BeltStorage {
        id: BeltTileId<ItemIdxType>,
        belt_pos: u16,
    },
    StorageStorage {
        item: Item<ItemIdxType>,
        // TODO: Do I want to store this Identifier of calculate it on demand to save RAM?
        inserter: InserterIdentifier,
    },
}

#[cfg_attr(feature = "client", derive(ShowInfo), derive(GetSize))]
#[derive(Debug, Clone, Copy, PartialEq, Eq, serde::Deserialize, serde::Serialize, Enum)]
pub enum UndergroundDir {
    Entrance,
    Exit,
}

#[cfg_attr(feature = "client", derive(ShowInfo), derive(GetSize))]
#[derive(Debug, Clone, serde::Deserialize, serde::Serialize, PartialEq)]
struct PipeConnection {
    pipe_pos: Position,
    connection_weak_index: WeakIndex,
}

// TODO: Support more than 8 modules slots
type ModuleSlots = Box<[Option<usize>; 8]>;

#[cfg_attr(feature = "client", derive(ShowInfo), derive(GetSize))]
#[derive(Debug, Clone, serde::Deserialize, serde::Serialize, PartialEq)]
pub enum Entity<ItemIdxType: WeakIdxTrait = u8, RecipeIdxType: WeakIdxTrait = u8> {
    Assembler {
        ty: u8,
        pos: Position,

        /// List of all the module slots of this assembler
        modules: ModuleSlots,
        info: AssemblerInfo<RecipeIdxType>,

        #[serde(default = "Dir::default")]
        rotation: Dir,
    },
    PowerPole {
        // This means at most 256 different types of power poles can exist, should be fine :)
        ty: u8,
        pos: Position,
        connected_power_poles: Vec<Position>,
    },
    Belt {
        pos: Position,
        direction: Dir,
        ty: u8,
        id: BeltTileId<ItemIdxType>,
        belt_pos: u16,
    },
    Underground {
        pos: Position,
        underground_dir: UndergroundDir,
        direction: Dir,
        ty: u8,
        id: BeltTileId<ItemIdxType>,
        belt_pos: u16,
    },
    Splitter {
        pos: Position,
        direction: Dir,
        id: SplitterTileId,
    },
    Inserter {
        ty: u8,
        user_movetime: Option<NonZero<u16>>,
        type_movetime: NonZero<u16>,

        pos: Position,
        direction: Dir,
        filter: Option<Item<ItemIdxType>>,

        info: InserterInfo<ItemIdxType>,
    },
    Chest {
        // This means at most 256 different types of Chest can exist, should be fine :)
        ty: u8,
        pos: Position,
        item: Option<(Item<ItemIdxType>, u32)>,
        slot_limit: u8,
    },
    Roboport {
        // This means at most 256 different types of Roboports can exist, should be fine
        ty: u8,
        pos: Position,
        power_grid: Option<PowerGridIdentifier>,
        network: u16,
        id: u32,
    },
    SolarPanel {
        pos: Position,
        ty: u8,
        pole_position: Option<(Position, WeakIndex)>,
    },
    Lab {
        pos: Position,
        ty: u8,
        /// List of all the module slots of this lab
        modules: ModuleSlots,
        pole_position: Option<(Position, WeakIndex, u32)>,
    },
    Beacon {
        ty: u8,
        pos: Position,
        /// List of all the module slots of this beacon
        modules: ModuleSlots,
        pole_position: Option<(Position, WeakIndex)>,
    },
    // Pipes are coded as fluid tanks with connections on all sides
    FluidTank {
        ty: u8,
        pos: Position,
        rotation: Dir,
    },
    // TODO:
    // MiningDrill {
    //     ty: u8,
    //     pos: Position,
    //     rotation: Dir,
    //     drill_id: DrillID,
    //     internal_inserter: InternalInserterInfo<ItemIdxType>,
    // },
}

#[cfg_attr(feature = "client", derive(ShowInfo), derive(GetSize))]
#[derive(Debug, Clone, Copy, serde::Deserialize, serde::Serialize, PartialEq)]
enum DrillID {
    OnlySolo(u32),
    WithShared(u32),
}

#[cfg_attr(feature = "client", derive(ShowInfo), derive(GetSize))]
#[derive(Debug, Clone, Copy, serde::Deserialize, serde::Serialize, PartialEq)]
pub struct UndergroundPipeConnection<ItemIdxType: WeakIdxTrait> {
    connected_pipe_pos: Position,
    system_id: FluidSystemId<ItemIdxType>,
}

impl<ItemIdxType: IdxTrait, RecipeIdxType: IdxTrait> Entity<ItemIdxType, RecipeIdxType> {
    pub const fn get_pos(&self) -> Position {
        match self {
            Self::Assembler { pos, .. } => *pos,
            Self::PowerPole { pos, .. } => *pos,
            Self::Belt { pos, .. } => *pos,
            Self::Inserter { pos, .. } => *pos,
            Self::Underground { pos, .. } => *pos,
            Self::Splitter { pos, .. } => *pos,
            Self::Chest { pos, .. } => *pos,
            Self::Roboport { pos, .. } => *pos,
            Self::SolarPanel { pos, .. } => *pos,
            Self::Lab { pos, .. } => *pos,
            Self::Beacon { pos, .. } => *pos,
            Self::FluidTank { pos, .. } => *pos,
        }
    }

    pub fn get_entity_size(
        &self,
        data_store: &DataStore<ItemIdxType, RecipeIdxType>,
    ) -> (u16, u16) {
        // FIXME: Use data_store for everything
        match self {
            Self::Assembler { ty, rotation, .. } => {
                data_store.assembler_info[usize::from(*ty)].size(*rotation)
            },
            Self::PowerPole { ty, .. } => data_store.power_pole_data[usize::from(*ty)].size,
            Self::Belt { .. } => (1, 1),
            Self::Inserter { .. } => (1, 1),
            Self::Underground { .. } => (1, 1),
            Self::Splitter { direction, .. } => match direction {
                Dir::North => (2, 1),
                Dir::East => (1, 2),
                Dir::South => (2, 1),
                Dir::West => (1, 2),
            },
            Self::Chest { ty, .. } => data_store.chest_tile_sizes[usize::from(*ty)],
            Self::Roboport { .. } => (4, 4),
            Self::SolarPanel { ty, .. } => (
                data_store.solar_panel_info[usize::from(*ty)].size[0],
                data_store.solar_panel_info[usize::from(*ty)].size[1],
            ),
            Self::Lab { ty, .. } => data_store.lab_info[usize::from(*ty)].size,
            Self::Beacon { ty, .. } => data_store.beacon_info[usize::from(*ty)].size,
            Self::FluidTank { ty, .. } => data_store.fluid_tank_infos[usize::from(*ty)].size.into(),
        }
    }

    #[cfg(feature = "client")]
    pub fn get_map_color(&self, data_store: &DataStore<ItemIdxType, RecipeIdxType>) -> Color32 {
        match self {
            Self::Assembler { .. } => Color32::from_hex("#0086c9").unwrap(),
            Self::PowerPole { .. } => Color32::from_hex("#eeee29").unwrap(),
            Self::Belt { .. } => Color32::from_hex("#faba00").unwrap(),
            Self::Inserter { .. } => Color32::from_hex("#006192").unwrap(),
            Self::Underground { .. } => Color32::from_hex("#faba00").unwrap(),
            Self::Splitter { .. } => Color32::from_hex("#faba00").unwrap(),
            Self::Chest { .. } => Color32::from_hex("#ccd8cc").unwrap(),
            Self::Roboport { .. } => Color32::from_hex("#4888e8").unwrap(),
            Self::SolarPanel { .. } => Color32::from_hex("#1f2124").unwrap(),
            Self::Lab { .. } => Color32::from_hex("#ff90bd").unwrap(),
            Self::Beacon { .. } => Color32::from_hex("#008192").unwrap(),
            Self::FluidTank { .. } => Color32::from_hex("#b429ff").unwrap(),
        }
    }

    pub fn cares_about_power(&self) -> bool {
        match self {
            Self::Assembler { .. } => true,
            Self::PowerPole { .. } => true,
            Self::Belt { .. } => false,
            Self::Inserter { .. } => false,
            Self::Underground { .. } => false,
            Self::Splitter { .. } => false,
            Self::Chest { .. } => false,
            Self::Roboport { .. } => true,
            Self::SolarPanel { .. } => true,
            Self::Lab { .. } => true,
            Self::Beacon { .. } => true,
            Self::FluidTank { .. } => false,
        }
    }

    pub fn get_type_name(&self) -> &'static str {
        match self {
            Self::Assembler { .. } => "Assembler",
            Self::PowerPole { .. } => "PowerPole",
            Self::Belt { .. } => "Belt",
            Self::Inserter { .. } => "Inserter",
            Self::Underground { .. } => "Underground",
            Self::Splitter { .. } => "Splitter",
            Self::Chest { .. } => "Chest",
            Self::Roboport { .. } => "Roboport",
            Self::SolarPanel { .. } => "SolarPanel",
            Self::Lab { .. } => "Lab",
            Self::Beacon { .. } => "Beacon",
            Self::FluidTank { .. } => "FluidTank",
        }
    }
}

#[cfg_attr(feature = "client", derive(ShowInfo), derive(GetSize))]
#[derive(
    Debug, Clone, Copy, serde::Deserialize, serde::Serialize, PartialEq, Eq, Hash, PartialOrd, Ord,
)]
pub struct BeltId<ItemIdxType: WeakIdxTrait> {
    pub item: Item<ItemIdxType>,
    pub index: usize,
}

#[derive(Debug, Clone, Copy, serde::Serialize, serde::Deserialize)]
pub enum PlaceEntityType<ItemIdxType: WeakIdxTrait> {
    Assembler {
        pos: Position,
        ty: u8,

        #[serde(default = "Dir::default")]
        rotation: Dir,
    },
    Inserter {
        pos: Position,
        dir: Dir,
        /// The Item the inserter will move, must fit both the in and output side
        filter: Option<Item<ItemIdxType>>,
    },
    Belt {
        pos: Position,
        direction: Dir,
        ty: u8,
    },
    Underground {
        pos: Position,
        direction: Dir,
        ty: u8,
        underground_dir: UndergroundDir,
    },
    PowerPole {
        pos: Position,
        ty: u8,
    },
    Splitter {
        pos: Position,
        direction: Dir,
        ty: u8,

        in_mode: Option<SplitterDistributionMode>,
        out_mode: Option<SplitterDistributionMode>,
    },
    Chest {
        pos: Position,
        ty: u8,
    },
    SolarPanel {
        pos: Position,
        ty: u8,
    },
    Lab {
        pos: Position,
        ty: u8,
    },
    Beacon {
        ty: u8,
        pos: Position,
    },
    FluidTank {
        ty: u8,
        pos: Position,
        rotation: Dir,
    },
    MiningDrill {
        ty: u8,
        pos: Position,
        rotation: Dir,
    },
}

impl<ItemIdxType: WeakIdxTrait> PlaceEntityType<ItemIdxType> {
    pub fn order_for_optimization(&self, other: &Self) -> std::cmp::Ordering {
        todo!()
    }
}

impl<ItemIdxType: IdxTrait> PlaceEntityType<ItemIdxType> {
    pub fn cares_about_power(&self) -> bool {
        match self {
            Self::Assembler { .. } => true,
            Self::PowerPole { .. } => true,
            Self::Belt { .. } => false,
            Self::Underground { .. } => false,
            Self::Inserter { .. } => false,
            Self::Splitter { .. } => false,
            Self::Chest { .. } => false,
            // Self::Roboport { .. } => true,
            Self::SolarPanel { .. } => true,
            Self::Lab { .. } => true,
            Self::Beacon { .. } => true,
            Self::FluidTank { .. } => false,
            Self::MiningDrill { .. } => true,
        }
    }
}

#[cfg_attr(feature = "client", derive(ShowInfo), derive(GetSize))]
#[derive(
    Debug, Clone, Copy, Default, serde::Serialize, serde::Deserialize, PartialEq, Eq, Enum, EnumIter,
)]
pub enum Dir {
    #[default]
    North,
    East,
    South,
    West,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum DirRelative {
    SameDir,
    Turned,
    Opposite,
}

impl Dir {
    #[must_use]
    pub const fn reverse(self) -> Self {
        match self {
            Self::North => Self::South,
            Self::East => Self::West,
            Self::South => Self::North,
            Self::West => Self::East,
        }
    }

    #[must_use]
    pub const fn into_offset(self) -> (i8, i8) {
        match self {
            Self::North => (0, -1),
            Self::East => (1, 0),
            Self::South => (0, 1),
            Self::West => (-1, 0),
        }
    }

    #[must_use]
    pub const fn turn_right(self) -> Self {
        match self {
            Self::North => Self::East,
            Self::East => Self::South,
            Self::South => Self::West,
            Self::West => Self::North,
        }
    }

    #[must_use]
    pub fn compare(self, other: Self) -> DirRelative {
        if self == other {
            DirRelative::SameDir
        } else if self == other.reverse() {
            DirRelative::Opposite
        } else {
            DirRelative::Turned
        }
    }
}

#[cfg_attr(feature = "client", derive(ShowInfo), derive(GetSize))]
#[derive(
    Debug, Clone, Copy, serde::Deserialize, serde::Serialize, PartialEq, Eq, PartialOrd, Ord, Hash,
)]
pub struct AssemblerID<RecipeIdxType: WeakIdxTrait = u8> {
    pub recipe: Recipe<RecipeIdxType>,
    pub grid: PowerGridIdentifier,
    pub assembler_index: u32,
}

#[cfg_attr(feature = "client", derive(ShowInfo), derive(GetSize))]
#[derive(
    Debug, Clone, Copy, serde::Deserialize, serde::Serialize, PartialEq, Eq, PartialOrd, Ord, Hash,
)]
pub enum MachineID<RecipeIdxType: WeakIdxTrait> {
    Assembler(AssemblerID<RecipeIdxType>),
    SolarPanel { grid: PowerGridIdentifier },
}

impl<RecipeIdxType: IdxTrait> MachineID<RecipeIdxType> {
    pub fn get_grid(&self) -> PowerGridIdentifier {
        match self {
            MachineID::Assembler(assembler_id) => assembler_id.grid,
            MachineID::SolarPanel { grid } => *grid,
        }
    }
}

impl<RecipeIdxType: IdxTrait> Into<MachineID<RecipeIdxType>> for AssemblerID<RecipeIdxType> {
    fn into(self) -> MachineID<RecipeIdxType> {
        MachineID::Assembler(self)
    }
}

impl Add<Dir> for Position {
    type Output = Position;

    fn add(self, rhs: Dir) -> Self::Output {
        let offs = rhs.into_offset();
        Self {
            x: self.x.checked_add(offs.0.into()).unwrap(),
            y: self.y.checked_add(offs.1.into()).unwrap(),
        }
    }
}

#[cfg(test)]
mod test {

    use proptest::{prop_assert, prop_assert_eq, proptest};

    use crate::{
        DATA_STORE,
        app_state::GameState,
        blueprint::{Blueprint, random_entity_to_place, random_position},
        frontend::{
            action::{ActionType, place_entity::PlaceEntityInfo},
            world::Position,
        },
        replays::Replay,
    };

    proptest! {

        #[test]
        fn test_get_entity(position in random_position(), ent in random_entity_to_place(&DATA_STORE)) {
            let mut state = GameState::new(&DATA_STORE);

            let mut rep = Replay::new(&state, None, &*DATA_STORE);

            rep.append_actions([ActionType::PlaceEntity(PlaceEntityInfo { force: false, entities: crate::frontend::action::place_entity::EntityPlaceOptions::Single(ent) })]);

            let bp = Blueprint::from_replay(&rep);

            bp.apply(false, position, &mut state, &DATA_STORE);

            let mut e_pos = None;
            let mut e_size = None;
            state.world.get_entities_colliding_with(position, (100, 100), &DATA_STORE).into_iter().for_each(|v| {
                e_pos = Some(v.get_pos());
                e_size = Some(v.get_entity_size(&DATA_STORE));
            });

            prop_assert!(e_pos.is_some());
            prop_assert!(e_size.is_some());

            let e_pos = e_pos.unwrap();
            let e_size = e_size.unwrap();

            for x_pos in e_pos.x..(e_pos.x + (e_size.0 as i32)) {
                for y_pos in e_pos.y..(e_pos.y + (e_size.1 as i32)) {
                    prop_assert_eq!(state.world.get_entities_colliding_with(Position { x: x_pos, y: y_pos }, (1, 1), &DATA_STORE).into_iter().count(), 1,  "test_pos = {:?}, world + {:?}", Position {x: x_pos, y: y_pos}, state.world.get_chunk_for_tile(position));
                }
            }
        }

    }
}
