use std::{
    cmp::min,
    collections::{BTreeMap, BTreeSet, HashMap},
    marker::PhantomData,
    mem,
    num::NonZero,
    ops::{Add, ControlFlow},
};

use egui::Color32;

use enum_map::{Enum, EnumMap};
use log::{info, warn};
use strum::EnumIter;

use itertools::Itertools;

use noise::{NoiseFn, Simplex};

use crate::{
    belt::{
        splitter::{SplitterDistributionMode, SplitterSide, SPLITTER_BELT_LEN},
        BeltBeltInserterAdditionInfo, BeltTileId, SplitterTileId,
    },
    data::{DataStore, ItemRecipeDir},
    inserter::{storage_storage_with_buckets::InserterIdentifier, StaticID, Storage, MOVETIME},
    item::{usize_from, IdxTrait, Item, Recipe, WeakIdxTrait},
    network_graph::WeakIndex,
    power::power_grid::{BeaconAffectedEntity, PowerGridEntity, PowerGridIdentifier},
    rendering::app_state::{
        calculate_inserter_positions, InstantiateInserterError, SimulationState,
    },
    TICKS_PER_SECOND_LOGIC,
};
use crate::{inserter::FakeUnionStorage, item::Indexable};
use static_assertions::const_assert;

use std::fmt::Debug;

use std::ops::{Deref, DerefMut};

use serde::Deserializer;
use serde::Serializer;

use noise::Seedable;

use super::{sparse_grid::SparseGrid, Position};
use crate::liquid::FluidSystemId;

pub const BELT_LEN_PER_TILE: u16 = 4;

pub const CHUNK_SIZE: u16 = 16;
pub const CHUNK_SIZE_FLOAT: f32 = 16.0;

#[derive(Debug, Default, Clone, Copy, PartialEq, serde::Serialize, serde::Deserialize)]
pub enum FloorTile {
    #[default]
    Empty,
    Concrete,
    Water,
}

// We rely on this, by storing entity indices as u8 in the chunk
const_assert!(CHUNK_SIZE * CHUNK_SIZE - 1 <= u8::MAX as u16);

#[derive(Debug, Clone, serde::Deserialize, serde::Serialize)]
pub struct Chunk<ItemIdxType: WeakIdxTrait, RecipeIdxType: WeakIdxTrait> {
    pub floor_tiles: Option<Box<[[FloorTile; CHUNK_SIZE as usize]; CHUNK_SIZE as usize]>>,
    chunk_tile_to_entity_into: Option<Box<[[u8; CHUNK_SIZE as usize]; CHUNK_SIZE as usize]>>,
    entities: Vec<Entity<ItemIdxType, RecipeIdxType>>,
}

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

#[derive(Debug, Clone, serde::Deserialize, serde::Serialize)]
pub struct World<ItemIdxType: WeakIdxTrait, RecipeIdxType: WeakIdxTrait> {
    noise: SerializableSimplex,

    // TODO: I don´t think I want FP
    pub players: Vec<PlayerInfo>,
    chunks: SparseGrid<i32, Chunk<ItemIdxType, RecipeIdxType>>,

    belt_lookup: BeltIdLookup<ItemIdxType>,
    belt_recieving_input_directions: HashMap<Position, EnumMap<Dir, bool>>,
    power_grid_lookup: PowerGridConnectedDevicesLookup,

    remaining_updates: Vec<WorldUpdate>,

    to_instantiate: BTreeSet<Position>,

    #[serde(skip)]
    pub map_updates: Option<Vec<Position>>,
}

#[derive(Debug, Clone)]
struct SerializableSimplex {
    inner: Simplex,
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

#[derive(Debug, Clone, serde::Deserialize, serde::Serialize)]
enum WorldUpdate {
    EntityNewlyPowered { pos: Position },
    NewEntity { pos: Position },
}

#[derive(Debug, Clone, serde::Deserialize, serde::Serialize)]
struct PowerGridConnectedDevicesLookup {
    grid_to_chunks: BTreeMap<PowerGridIdentifier, BTreeSet<(i32, i32)>>,
}

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

#[profiling::function]
fn newly_instantiated_inserter_cascade<ItemIdxType: IdxTrait, RecipeIdxType: IdxTrait>(
    pos: Position,
) -> CascadingUpdate<ItemIdxType, RecipeIdxType> {
    CascadingUpdate {
        update: Box::new(|world, sim_state, updates, data_store| {
            // TODO:
        }),
    }
}

#[profiling::function]
fn instantiate_inserter_cascade<ItemIdxType: IdxTrait, RecipeIdxType: IdxTrait>(
    pos: Position,
    data_store: &DataStore<ItemIdxType, RecipeIdxType>,
) -> CascadingUpdate<ItemIdxType, RecipeIdxType> {
    CascadingUpdate {
        update: Box::new(move |world, sim_state, updates, data_store| {
            world.to_instantiate.insert(pos);

            let mut tmp = BTreeSet::default();

            mem::swap(&mut tmp, &mut world.to_instantiate);

            tmp.retain(|pos| {
                match world.try_instantiate_inserter(sim_state, *pos, data_store) {
                    Ok(newly_instantiated) => {
                        updates.push(newly_instantiated_inserter_cascade(*pos));
                        // FIXME: Size hardcoded
                        updates.extend(
                            newly_instantiated
                                .into_iter()
                                .map(|new_pos| new_possible_inserter_connection(new_pos, (10, 10))),
                        );

                        false
                    },
                    Err(InstantiateInserterError::NotUnattachedInserter) => {
                        warn!("We seem to have instantiated the same inserter twice?!?");
                        false
                    },
                    Err(
                        InstantiateInserterError::ItemConflict
                        | InstantiateInserterError::PleaseSpecifyFilter,
                    ) => true,
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

#[profiling::function]
fn new_possible_inserter_connection<ItemIdxType: IdxTrait, RecipeIdxType: IdxTrait>(
    pos: Position,
    size: (u16, u16),
) -> CascadingUpdate<ItemIdxType, RecipeIdxType> {
    CascadingUpdate {
        update: Box::new(move |world, sim_state, updates, data_store| {
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

#[profiling::function]
fn new_lab_cascade<ItemIdxType: IdxTrait, RecipeIdxType: IdxTrait>(
    pos: Position,
    data_store: &DataStore<ItemIdxType, RecipeIdxType>,
) -> CascadingUpdate<ItemIdxType, RecipeIdxType> {
    CascadingUpdate {
        update: Box::new(move |world, sim_state, updates, data_store| {
            let Some(Entity::Lab {
                pos,
                ty,
                modules,
                pole_position: Some((pole_pos, weak_index, index)),
            }) = world
                .get_entities_colliding_with(pos, (1, 1), data_store)
                .into_iter()
                .next()
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

#[profiling::function]
fn new_power_pole<ItemIdxType: IdxTrait, RecipeIdxType: IdxTrait>(
    pos: Position,
    data_store: &DataStore<ItemIdxType, RecipeIdxType>,
) -> CascadingUpdate<ItemIdxType, RecipeIdxType> {
    CascadingUpdate {
        update: Box::new(move |world, sim_state, updates, data_store| {
            let pole = world
                .get_entities_colliding_with(pos, (1, 1), data_store)
                .into_iter()
                .next();

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
                            } => {
                                let AssemblerInfo::Unpowered(recipe) = info else {
                                    unreachable!();
                                };

                                let (new_id, weak_index) =
                                    sim_state.factory.power_grids.power_grids[usize::from(grid_id)]
                                        .add_assembler(
                                            *ty, grid_id, *recipe, &modules, pole_pos, *pos,
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
                                .add_lab(*pos, *ty, &modules, pole_pos, data_store);

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

#[profiling::function]
fn new_chest_cascade<ItemIdxType: IdxTrait, RecipeIdxType: IdxTrait>(
    pos: Position,
) -> CascadingUpdate<ItemIdxType, RecipeIdxType> {
    CascadingUpdate {
        update: Box::new(move |world, sim_state, updates, data_store| {
            let Some(Entity::Chest {
                ty,
                pos,
                item: _,
                slot_limit: _,
            }) = world
                .get_entities_colliding_with(pos, (1, 1), data_store)
                .into_iter()
                .next()
            else {
                return;
            };

            let size = data_store.chest_tile_sizes[usize::from(*ty)];

            updates.push(new_possible_inserter_connection(*pos, size));
        }),
    }
}

#[profiling::function]
fn new_powered_beacon_cascade<ItemIdxType: IdxTrait, RecipeIdxType: IdxTrait>(
    pos: Position,
    data_store: &DataStore<ItemIdxType, RecipeIdxType>,
) -> CascadingUpdate<ItemIdxType, RecipeIdxType> {
    CascadingUpdate {
        update: Box::new(move |world, sim_state, updates, data_store| {
            let Some(Entity::Beacon {
                ty,
                pos,
                modules: _,
                pole_position: Some((pole_pos, weak_idx)),
            }) = world
                .get_entities_colliding_with(pos, (1, 1), data_store)
                .into_iter()
                .next()
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
                        ty: _,
                        pos: _,
                        modules: _,
                        info:
                            AssemblerInfo::Powered {
                                id,
                                pole_position: _,
                                weak_index: _,
                            },
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

#[profiling::function]
fn newly_working_assembler<ItemIdxType: IdxTrait, RecipeIdxType: IdxTrait>(
    pos: Position,
    data_store: &DataStore<ItemIdxType, RecipeIdxType>,
) -> CascadingUpdate<ItemIdxType, RecipeIdxType> {
    CascadingUpdate {
        update: Box::new(move |world, sim_state, updates, data_store| {
            let Some(Entity::Assembler {
                ty,
                pos,
                modules: _,
                info:
                    AssemblerInfo::Powered {
                        id,
                        pole_position: _,
                        weak_index: _,
                    },
            }) = world
                .get_entities_colliding_with(pos, (1, 1), data_store)
                .into_iter()
                .next()
            else {
                warn!("Assembler missing in new assembler cascade");
                return;
            };

            let pos = *pos;
            let size = data_store.assembler_info[usize::from(*ty)].size;
            let id = *id;

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

#[profiling::function]
fn removal_of_possible_inserter_connection<ItemIdxType: IdxTrait, RecipeIdxType: IdxTrait>(
    pos: Position,
    size: (u16, u16),
    data_store: &DataStore<ItemIdxType, RecipeIdxType>,
) -> CascadingUpdate<ItemIdxType, RecipeIdxType> {
    CascadingUpdate {
        update: Box::new(move |world, sim_state, updates, data_store| {
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
                                                .remove_ins(*item, movetime, *inserter);

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

impl<ItemIdxType: IdxTrait, RecipeIdxType: IdxTrait> World<ItemIdxType, RecipeIdxType> {
    #[must_use]
    pub fn new() -> Self {
        let mut grid = SparseGrid::new();
        #[cfg(debug_assertions)]
        const WORLDSIZE_CHUNKS: i32 = 200;
        #[cfg(not(debug_assertions))]
        const WORLDSIZE_CHUNKS: i32 = 4000;

        let noise = Simplex::new(1);

        for x in 50..WORLDSIZE_CHUNKS {
            for y in 50..WORLDSIZE_CHUNKS {
                grid.insert(
                    x,
                    y,
                    Chunk {
                        floor_tiles: None,
                        chunk_tile_to_entity_into: None,
                        entities: vec![],
                    },
                );
            }
        }

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

            map_updates: None,
        }
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

    pub fn get_chunks(
        &self,
    ) -> impl IntoIterator<Item = &Chunk<ItemIdxType, RecipeIdxType>, IntoIter: Send> {
        self.chunks.occupied_entries().map(|(a, b)| b)
    }

    pub fn get_chunk(&self, x: i32, y: i32) -> Option<&Chunk<ItemIdxType, RecipeIdxType>> {
        self.chunks.get(x, y)
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

        self.mutate_entities_colliding_with(pos, (1, 1), data_store, |e| {
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
                } => {
                    for module_slot in modules.iter_mut() {
                        *module_slot = cheated_modules;
                    }

                    let assembler_size = data_store.assembler_info[usize::from(*ty)].size;

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
                        modules,
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
            ControlFlow::Break(())
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
        if !self.can_fit(entity.get_pos(), entity.get_size(data_store), data_store) {
            return Err(());
        }

        let pos = entity.get_pos();
        let size = entity.get_size(data_store);

        let chunk_pos = self.get_chunk_pos_for_tile(pos);

        if self.get_chunk_for_tile_mut(pos).is_none() {
            todo!();
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
            for x_offs in 0..entity.get_size(data_store).0 {
                for y_offs in 0..entity.get_size(data_store).1 {
                    let e_pos = entity.get_pos();

                    map_updates.push(Position {
                        x: e_pos.x + i32::from(x_offs),
                        y: e_pos.y + i32::from(y_offs),
                    });
                }
            }
        }

        let chunk = self
            .get_chunk_for_tile_mut(pos)
            .expect("Chunk outside the world!");

        let map = if let Some(map) = &mut chunk.chunk_tile_to_entity_into {
            map
        } else {
            chunk.chunk_tile_to_entity_into = Some(Box::new(
                [[u8::MAX; CHUNK_SIZE as usize]; CHUNK_SIZE as usize],
            ));
            chunk.chunk_tile_to_entity_into.as_mut().unwrap()
        };

        let e_pos: Position = entity.get_pos();
        let e_size = entity.get_size(data_store);

        chunk.entities.push(entity);

        let index = u8::try_from(chunk.entities.len() - 1).expect("Into a chunk of size 16 x 16 we can fit at most 256 entitites. This assumes all entities are at least 1x1");

        for x_offs in 0..e_size.0 {
            for y_offs in 0..e_size.1 {
                let x_in_chunk = usize::try_from((e_pos.x).rem_euclid(CHUNK_SIZE as i32)).unwrap();
                let x = min(x_in_chunk + usize::from(x_offs), CHUNK_SIZE as usize - 1);
                let y_in_chunk = usize::try_from((e_pos.y).rem_euclid(CHUNK_SIZE as i32)).unwrap();
                let y = min(y_in_chunk + usize::from(y_offs), CHUNK_SIZE as usize - 1);

                map[x][y] = index;
            }
        }

        #[cfg(debug_assertions)]
        for x_offs in 0..e_size.0 {
            for y_offs in 0..e_size.1 {
                assert!(self
                    .get_entities_colliding_with(
                        Position {
                            x: e_pos.x + i32::from(x_offs),
                            y: e_pos.y + i32::from(y_offs)
                        },
                        (1, 1),
                        data_store
                    )
                    .into_iter()
                    .next()
                    .is_some());
                assert!(
                    self.get_entities_colliding_with(
                        Position {
                            x: e_pos.x + i32::from(x_offs),
                            y: e_pos.y + i32::from(y_offs)
                        },
                        (1, 1),
                        data_store
                    )
                    .into_iter()
                    .next()
                    .unwrap()
                    .get_pos()
                        == e_pos
                );
            }
        }

        while let Some(update) = cascading_updates.pop() {
            (update.update)(self, sim_state, &mut cascading_updates, data_store);
        }

        Ok(())
    }

    pub fn try_instantiate_inserter(
        &mut self,
        simulation_state: &mut SimulationState<ItemIdxType, RecipeIdxType>,
        pos: Position,
        data_store: &DataStore<ItemIdxType, RecipeIdxType>,
    ) -> Result<Vec<Position>, InstantiateInserterError> {
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
        }) = self
            .get_entities_colliding_with(pos, (1, 1), data_store)
            .into_iter()
            .next()
        else {
            return Err(InstantiateInserterError::NotUnattachedInserter);
        };

        let movetime = user_movetime.map(|v| v.into()).unwrap_or(*type_movetime);

        let start_conn: Option<InserterConnectionPossibility<ItemIdxType, RecipeIdxType>> = self
            .get_entities_colliding_with(*start_pos, (1, 1), data_store)
            .into_iter()
            .next()
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
                    modules: _
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
                Entity::Splitter { pos, id, .. } => {
                    let side = if *pos == *end_pos {
                        SplitterSide::Left
                    } else {
                        SplitterSide::Right
                    };

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
            .get_entities_colliding_with(*end_pos, (1, 1), data_store)
            .into_iter()
            .next()
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
                    modules: _
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
                Entity::Splitter { pos, id, .. } => {
                    let side = if *pos == *end_pos {
                        SplitterSide::Left
                    } else {
                        SplitterSide::Right
                    };

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
            return Err(InstantiateInserterError::ItemConflict);
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
                    return Err(InstantiateInserterError::ItemConflict);
                }
            },
            None => {
                // The user/game has not specified a filter, try and infer it

                // TODO: Figure out what is most intuitive here, for now just use the only possible item otherwise error
                match possible_items {
                    PossibleItem::All => return Err(InstantiateInserterError::PleaseSpecifyFilter),
                    PossibleItem::List(items) => match items.len().cmp(&1) {
                        std::cmp::Ordering::Less => {
                            return Err(InstantiateInserterError::ItemConflict)
                        },
                        std::cmp::Ordering::Equal => items[0],
                        std::cmp::Ordering::Greater => {
                            return Err(InstantiateInserterError::PleaseSpecifyFilter)
                        },
                    },
                    PossibleItem::None => unreachable!(),
                }
            },
        };

        let mut instantiated = vec![];

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
                        self.mutate_entities_colliding_with(end_pos, (1, 1), data_store, |e| {
                            match e {
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
                                    instantiated.push(*chest_pos);
                                    storage = Some(Storage::Static {
                                        index,
                                        static_id: StaticID::Chest as u16,
                                    })
                                },
                                _ => unreachable!(),
                            }
                            ControlFlow::Break(())
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
                        self.mutate_entities_colliding_with(start_pos, (1, 1), data_store, |e| {
                            match e {
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
                                    instantiated.push(*chest_pos);
                                    storage = Some(Storage::Static {
                                        index,
                                        static_id: StaticID::Chest as u16,
                                    })
                                },
                                _ => unreachable!(),
                            }
                            ControlFlow::Break(())
                        });
                        storage.unwrap()
                    },
                };

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
                        self.mutate_entities_colliding_with(start_pos, (1, 1), data_store, |e| {
                            match e {
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
                                    instantiated.push(*chest_pos);
                                    storage = Some(Storage::Static {
                                        index,
                                        static_id: StaticID::Chest as u16,
                                    });
                                },
                                _ => unreachable!(),
                            }
                            ControlFlow::Break(())
                        });
                        storage.unwrap()
                    },
                };

                let dest_storage_untranslated = match dest_storage_untranslated {
                    Static::Done(storage) => storage,
                    Static::ToInstantiate => {
                        let mut storage = None;
                        self.mutate_entities_colliding_with(end_pos, (1, 1), data_store, |e| {
                            match e {
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
                                    instantiated.push(*chest_pos);
                                    storage = Some(Storage::Static {
                                        index,
                                        static_id: StaticID::Chest as u16,
                                    });
                                },
                                _ => unreachable!(),
                            }
                            ControlFlow::Break(())
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
                    movetime,
                    start_storage,
                    dest_storage,
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
        old_id: BeltTileId<ItemIdxType>,
        new_id: BeltTileId<ItemIdxType>,
        belt_pos_earliest: u16,
    ) {
        let old_chunks = self.belt_lookup.belt_id_to_chunks.remove(&old_id);

        for chunk_pos in old_chunks.iter().flatten() {
            let chunk = self
                .chunks
                .get_mut(chunk_pos.0, chunk_pos.1)
                .expect("Ungenerated chunk in belt map!");

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
                                }
                            },
                            AttachedInserter::BeltBelt { item, inserter } => todo!(),
                            AttachedInserter::StorageStorage { .. } => {},
                        },
                    },
                }
            }
        }

        self.belt_lookup
            .belt_id_to_chunks
            .entry(new_id)
            .or_default()
            .extend(old_chunks.into_iter().flatten());
    }

    pub fn update_belt_id(
        &mut self,
        old_id: BeltTileId<ItemIdxType>,
        new_id: BeltTileId<ItemIdxType>,
    ) {
        // Do it for ALL belt_pos
        self.update_belt_id_after(old_id, new_id, 0);
    }

    pub fn modify_belt_pos(&mut self, id_to_change: BeltTileId<ItemIdxType>, offs: i16) {
        let chunks = self.belt_lookup.belt_id_to_chunks.get(&id_to_change);

        for chunk_pos in chunks.into_iter().flatten() {
            let chunk = self
                .chunks
                .get_mut(chunk_pos.0, chunk_pos.1)
                .expect("Ungenerated chunk in belt map!");

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
                            *belt_pos = belt_pos
                                .checked_add_signed(offs)
                                .expect("belt_pos wrapped!");
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
                                    *belt_pos = belt_pos
                                        .checked_add_signed(offs)
                                        .expect("belt_pos wrapped!");
                                }
                            },
                            AttachedInserter::BeltBelt { item, inserter } => todo!(),
                            AttachedInserter::StorageStorage { .. } => {},
                        },
                    },
                }
            }
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

    pub fn get_entity_color(
        &self,
        pos: Position,
        data_store: &DataStore<ItemIdxType, RecipeIdxType>,
    ) -> Color32 {
        let chunk = self.get_chunk_for_tile(pos);

        chunk
            .map(|chunk| {
                let index = if let Some(map) = &chunk.chunk_tile_to_entity_into {
                    map[usize::try_from(pos.x.rem_euclid(CHUNK_SIZE as i32)).unwrap()]
                        [usize::try_from(pos.y.rem_euclid(CHUNK_SIZE as i32)).unwrap()]
                } else {
                    u8::MAX
                };

                if (index as usize) < chunk.entities.len() {
                    chunk.entities[index as usize].get_map_color(data_store)
                } else {
                    // This could be part of an entity in a different chunk
                    let our_chunk_x = pos.x / i32::from(CHUNK_SIZE);
                    let our_chunk_y = pos.y / i32::from(CHUNK_SIZE);

                    let chunk_range_x = ((pos.x - i32::from(data_store.max_entity_size.0))
                        / i32::from(CHUNK_SIZE))
                        ..=our_chunk_x;

                    for chunk_x in chunk_range_x {
                        let chunk_range_y = ((pos.y - i32::from(data_store.max_entity_size.1))
                            / i32::from(CHUNK_SIZE))
                            ..=our_chunk_y;
                        for chunk_y in chunk_range_y {
                            if chunk_x == our_chunk_x && chunk_y == our_chunk_y {
                                continue;
                            }

                            if let Some(colliding) = self
                                .get_chunk(chunk_x, chunk_y)
                                .iter()
                                .flat_map(|chunk| chunk.entities.iter())
                                .find(|e| {
                                    let e_pos = e.get_pos();
                                    let e_size = e.get_size(data_store);

                                    pos.contained_in(e_pos, (e_size.0.into(), e_size.1.into()))
                                })
                            {
                                return colliding.get_map_color(data_store);
                            }
                        }
                    }

                    if let Some(ore) = self.get_original_ore_at_pos(pos) {
                        if ore.1 > 0 {
                            // TODO ORE COLOR
                            Color32::LIGHT_BLUE
                        } else {
                            // TODO: Get floor color
                            Color32::BLUE
                        }
                    } else {
                        // TODO: Get floor color
                        Color32::BLUE
                    }
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
            .filter_map(|(chunk_x, chunk_y)| {
                self.chunks
                    .get(chunk_x, chunk_y)
                    .map(|c| (chunk_x, chunk_y, c))
            })
            .map(move |(chunk_x, chunk_y, chunk)| {
                if size == (1, 1)
                    && pos.x >= chunk_x * i32::from(CHUNK_SIZE)
                    && pos.x < (chunk_x + 1) * i32::from(CHUNK_SIZE)
                    && pos.y >= chunk_y * i32::from(CHUNK_SIZE)
                    && pos.y < (chunk_y + 1) * i32::from(CHUNK_SIZE)
                {
                    let index = if let Some(map) = &chunk.chunk_tile_to_entity_into {
                        map[usize::try_from(pos.x.rem_euclid(i32::from(CHUNK_SIZE))).unwrap()]
                            [usize::try_from(pos.y.rem_euclid(i32::from(CHUNK_SIZE))).unwrap()]
                    } else {
                        u8::MAX
                    };

                    if index as usize >= chunk.entities.len() {
                        debug_assert!(chunk.entities.iter().all(|e| {
                            let e_pos = e.get_pos();
                            let e_size = e.get_size(data_store);

                            !pos.overlap(size, e_pos, (e_size.0.into(), e_size.1.into()))
                        }));
                        [].iter()
                    } else {
                        assert!(chunk.entities[(index as usize)..(index as usize + 1)].len() == 1);
                        {
                            let e_pos = chunk.entities[index as usize].get_pos();
                            let e_size = chunk.entities[index as usize].get_size(data_store);

                            assert!(pos.contained_in(e_pos, e_size))
                        }
                        chunk.entities[(index as usize)..(index as usize + 1)].iter()
                    }
                } else {
                    chunk.entities.iter()
                }
            })
            .flatten()
            .filter(move |e| {
                let e_pos = e.get_pos();
                let e_size = e.get_size(data_store);

                pos.overlap(size, e_pos, (e_size.0.into(), e_size.1.into()))
            })
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
                    let e_size = e.get_size(data_store);

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
        let entity = self
            .get_entities_colliding_with(pos, (1, 1), data_store)
            .into_iter()
            .next();

        let mut cascading_updates = vec![];

        if let Some(entity) = entity {
            let e_pos = entity.get_pos();
            let e_size = entity.get_size(data_store);
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
                            self.mutate_entities_colliding_with(
                                index_update.position,
                                (1, 1),
                                data_store,
                                |e| {
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
                                    ControlFlow::Break(())
                                },
                            );

                            let assembler_size: (u16, u16) =
                                data_store.assembler_info[usize::from(ty)].size;

                            let inserter_search_area = (
                                Position {
                                    x: index_update.position.x - i32::from(max_inserter_range),
                                    y: index_update.position.y - i32::from(max_inserter_range),
                                },
                                (
                                    2 * max_inserter_range as u16 + assembler_size.0,
                                    2 * max_inserter_range as u16 + assembler_size.1,
                                ),
                            );

                            let new_storages: Vec<_> = match index_update.new_pg_entity {
                                PowerGridEntity::Assembler { ty, recipe, index } => data_store
                                    .recipe_to_items[&recipe]
                                    .iter()
                                    .map(|(_dir, item)| {
                                        (
                                            item,
                                            Storage::Assembler {
                                                grid: index_update.new_grid,
                                                recipe_idx_with_this_item: data_store
                                                    .recipe_to_translated_index[&(recipe, *item)],
                                                index,
                                            },
                                        )
                                    })
                                    .collect(),
                                PowerGridEntity::Lab { index, ty } => todo!(),
                                PowerGridEntity::LazyPowerProducer { item, index } => {
                                    todo!("Expand Storage type")
                                },
                                PowerGridEntity::SolarPanel { .. } => {
                                    vec![]
                                },
                                PowerGridEntity::Accumulator { .. } => {
                                    vec![]
                                },
                                PowerGridEntity::Beacon { .. } => {
                                    vec![]
                                },
                            };

                            assert!(new_storages
                                .iter()
                                .map(|(item, _storage)| item)
                                .all_unique());

                            self.mutate_entities_colliding_with(
                                inserter_search_area.0,
                                inserter_search_area.1,
                                data_store,
                                |e| {
                                    match e {
                                        Entity::Inserter {
                                            pos,
                                            direction,
                                            info: InserterInfo::NotAttached { .. },
                                            ..
                                        } => {
                                            // Nothing to do
                                        },
                                        Entity::Inserter {
                                            pos,
                                            direction,
                                            info,
                                            ..
                                        } => {
                                            let (start_pos, end_pos) =
                                                calculate_inserter_positions(*pos, *direction);

                                            if start_pos
                                                .contained_in(index_update.position, assembler_size)
                                                || end_pos.contained_in(
                                                    index_update.position,
                                                    assembler_size,
                                                )
                                            {
                                                // This Inserter is connected to the entity we are removing!
                                                match info {
                                                    InserterInfo::NotAttached {
                                                        start_pos,
                                                        end_pos,
                                                    } => {
                                                        unreachable!()
                                                    },
                                                    InserterInfo::Attached {
                                                        info: attached_inserter,
                                                        ..
                                                    } => {
                                                        match attached_inserter {
                                                            AttachedInserter::BeltStorage {
                                                                id,
                                                                belt_pos,
                                                            } => sim_state
                                                                .factory
                                                                .belts
                                                                .remove_inserter(*id, *belt_pos),
                                                            AttachedInserter::BeltBelt {
                                                                item,
                                                                inserter,
                                                            } => todo!(),
                                                            AttachedInserter::StorageStorage {
                                                                ..
                                                            } => {
                                                                todo!()
                                                            },
                                                        }
                                                        todo!();
                                                    },
                                                }
                                            }
                                        },

                                        _ => {},
                                    }
                                    ControlFlow::Continue(())
                                },
                            );
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

                        self.mutate_entities_colliding_with(
                            unconnected_position,
                            (1, 1),
                            data_store,
                            |e| {
                                match e {
                                    Entity::Assembler {
                                        ty,
                                        pos,
                                        info,
                                        modules,
                                    } => match info {
                                        AssemblerInfo::UnpoweredNoRecipe => unreachable!(),
                                        AssemblerInfo::Unpowered(recipe) => unreachable!(),
                                        AssemblerInfo::PoweredNoRecipe(position) => unreachable!(),
                                        AssemblerInfo::Powered {
                                            id,
                                            pole_position,
                                            weak_index,
                                        } => {
                                            assert!(sim_state
                                                .factory
                                                .power_grids
                                                .pole_pos_to_grid_id
                                                .get(pole_position)
                                                .is_none());

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
                                                        &*modules,
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
                                ControlFlow::Continue(())
                            },
                        );
                    }
                },

                Entity::Belt {
                    pos,
                    direction,
                    ty,
                    id,
                    belt_pos,
                } => todo!(),
                Entity::Underground {
                    pos,
                    underground_dir,
                    direction,
                    ty,
                    id,
                    belt_pos,
                } => todo!(),
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
                            user_movetime.map(|v| v.into()).unwrap_or(*type_movetime),
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
                            *v = u8::MAX;
                        },
                        std::cmp::Ordering::Greater => {
                            // Vec::remove will move us one step to the left
                            *v -= 1;
                        },
                    }
                }
            }

            // Actually remove the entity
            chunk.entities.remove(old_idx as usize);
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
        // TODO: Make sure all chunks are generated
        self.get_entities_colliding_with(pos, (size.0 as u16, size.1 as u16), data_store)
            .into_iter()
            .next()
            .is_none()
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

    // TODO: Does this need to return something
    pub fn update_pole_power(
        &mut self,
        pole_position: Position,
        grid: PowerGridIdentifier,
        data_store: &DataStore<ItemIdxType, RecipeIdxType>,
    ) {
        let Entity::PowerPole {
            ty,
            pos: pole_position,
            ..
        } = self
            .get_entities_colliding_with(pole_position, (1, 1), data_store)
            .into_iter()
            .next()
            .unwrap()
        else {
            unreachable!()
        };
    }

    fn get_power_pole_range(
        &self,
        pole_pos: Position,
        data_store: &DataStore<ItemIdxType, RecipeIdxType>,
    ) -> (u8, (u16, u16)) {
        let Some(Entity::PowerPole { ty, .. }) = self
            .get_entities_colliding_with(pole_pos, (1, 1), data_store)
            .into_iter()
            .next()
        else {
            unreachable!()
        };

        let range = data_store.power_pole_data[usize::from(*ty)].connection_range;
        let size = data_store.power_pole_data[usize::from(*ty)].size;

        (range, size)
    }
}

impl<ItemIdxType: IdxTrait, RecipeIdxType: IdxTrait> Default for World<ItemIdxType, RecipeIdxType> {
    fn default() -> Self {
        Self::new()
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
            let e_size = e.get_size(data_store);

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
            let e_size = e.get_size(data_store);

            pos.contained_in(e_pos, (e_size.0.into(), e_size.1.into()))
        })
    }

    #[must_use]
    fn can_fit(
        &self,
        pos: Position,
        size: (u8, u8),
        data_store: &DataStore<ItemIdxType, RecipeIdxType>,
    ) -> bool {
        self.entities
            .iter()
            .all(|e: &Entity<ItemIdxType, RecipeIdxType>| {
                let e_pos = e.get_pos();
                let e_size = e.get_size(data_store);

                (pos.x + i32::from(size.0)) <= e_pos.x
                    || (pos.y + i32::from(size.1)) <= e_pos.y
                    || (pos.x) >= (e_pos.x + i32::from(e_size.0))
                    || (pos.y) >= (e_pos.y + i32::from(e_size.1))
            })
    }

    #[must_use]
    pub fn get_entities(&self) -> impl IntoIterator<Item = &Entity<ItemIdxType, RecipeIdxType>> {
        &self.entities
    }
}

#[derive(Debug, Clone, Copy, serde::Deserialize, serde::Serialize, PartialEq)]
pub enum AssemblerInfo<RecipeIdxType: WeakIdxTrait> {
    UnpoweredNoRecipe,
    Unpowered(Recipe<RecipeIdxType>),
    PoweredNoRecipe(Position),
    Powered {
        id: AssemblerID<RecipeIdxType>,
        pole_position: Position,
        weak_index: WeakIndex,
    },
}

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

#[derive(Debug, Clone, Copy, PartialEq, Eq, serde::Deserialize, serde::Serialize)]
pub enum UndergroundDir {
    Entrance,
    Exit,
}

#[derive(Debug, Clone, serde::Deserialize, serde::Serialize, PartialEq)]
struct PipeConnection {
    pipe_pos: Position,
    connection_weak_index: WeakIndex,
}

#[derive(Debug, Clone, serde::Deserialize, serde::Serialize, PartialEq)]
pub enum Entity<ItemIdxType: WeakIdxTrait, RecipeIdxType: WeakIdxTrait> {
    Assembler {
        ty: u8,
        pos: Position,
        /// List of all the module slots of this assembler
        modules: Box<[Option<usize>]>,
        info: AssemblerInfo<RecipeIdxType>,
        // fluid_connections: Vec<PipeConnection>,
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
        type_movetime: u16,

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
        /// List of all the module slots of this assembler
        modules: Box<[Option<usize>]>,
        pole_position: Option<(Position, WeakIndex, u32)>,
    },
    Beacon {
        ty: u8,
        pos: Position,
        /// List of all the module slots of this beacon
        modules: Box<[Option<usize>]>,
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
    // },
}

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

    pub fn get_size(&self, data_store: &DataStore<ItemIdxType, RecipeIdxType>) -> (u16, u16) {
        // FIXME: Use data_store for everything
        match self {
            Self::Assembler { ty, .. } => data_store.assembler_info[usize::from(*ty)].size,
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
}

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

#[derive(
    Debug, Clone, Copy, serde::Serialize, serde::Deserialize, PartialEq, Eq, Enum, EnumIter,
)]
pub enum Dir {
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

#[derive(
    Debug, Clone, Copy, serde::Deserialize, serde::Serialize, PartialEq, Eq, PartialOrd, Ord, Hash,
)]
pub struct AssemblerID<RecipeIdxType: WeakIdxTrait> {
    pub recipe: Recipe<RecipeIdxType>,
    pub grid: PowerGridIdentifier,
    pub assembler_index: u32,
}
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
        blueprint::{random_entity_to_place, random_position, Blueprint},
        frontend::{
            action::{place_entity::PlaceEntityInfo, ActionType},
            world::Position,
        },
        rendering::app_state::GameState,
        replays::Replay,
        DATA_STORE,
    };

    proptest! {

        #[test]
        fn test_get_entity(position in random_position(), ent in random_entity_to_place(&DATA_STORE)) {
            let mut state = GameState::new(&DATA_STORE);

            let mut rep = Replay::new(&state, None, &*DATA_STORE);

            rep.append_actions([ActionType::PlaceEntity(PlaceEntityInfo { entities: crate::frontend::action::place_entity::EntityPlaceOptions::Single(ent) })]);

            let bp = Blueprint::from_replay(&rep);

            bp.apply(position, &mut state, &DATA_STORE);

            let mut e_pos = None;
            let mut e_size = None;
            state.world.get_entities_colliding_with(position, (100, 100), &DATA_STORE).into_iter().for_each(|v| {
                e_pos = Some(v.get_pos());
                e_size = Some(v.get_size(&DATA_STORE));
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
