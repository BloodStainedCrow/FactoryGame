use std::collections::HashMap;

use itertools::assert_equal;
use log::warn;

use crate::{
    DataStore, GameState, Position, WeakIdxTrait,
    app_state::{AuxillaryData, Factory, SimulationState, StorageStorageInserterStore},
    belt::{BeltStore, belt::BeltLenType, smart::SmartBelt},
    blueprint::ReusableBlueprint,
    chest::{FullChestStore, MultiChestStore},
    frontend::{
        action::{
            ActionType,
            place_entity::{EntityPlaceOptions, PlaceEntityInfo},
        },
        world::tile::{
            AssemblerInfo, Dir, Entity, ModuleTy, PlaceEntityType, UndergroundDir, World,
        },
    },
    get_size::Mutex,
    inserter::Storage,
    item::{IdxTrait, Indexable, Item, Recipe},
    join_many::join,
    liquid::{
        FluidConnectionDir, FluidSystemStore,
        connection_logic::can_fluid_tanks_connect_to_single_connection,
    },
    network_graph::WeakIndex,
    power::{PowerGridStorage, power_grid::BeaconAffectedEntity},
};

#[derive(Debug, Clone, serde::Serialize, serde::Deserialize)]
pub enum BeltKind<ItemIdxType: WeakIdxTrait> {
    Smart(Item<ItemIdxType>),
    Sushi,
    Empty,
}

// The belt may NOT sideload
#[derive(Debug, Clone, serde::Serialize, serde::Deserialize)]
struct Belt<ItemIdxType: WeakIdxTrait> {
    ty: u8,
    length: BeltLenType,
    kind: BeltKind<ItemIdxType>,
    entities: Vec<BeltEntity>,
}

#[derive(Debug, Clone, Copy, serde::Serialize, serde::Deserialize)]
struct BeltEntity {
    kind: BeltEntityKind,
    pos: Position,
    dir: Dir,
    belt_pos: BeltLenType,
}

#[derive(Debug, Clone, Copy, serde::Serialize, serde::Deserialize)]
enum BeltEntityKind {
    Belt,
    Underground(UndergroundDir),
}

#[derive(Debug, Clone, serde::Serialize, serde::Deserialize)]
pub struct BoundingBox {
    pub top_left: Position,
    pub bottom_right: Position,
}

#[derive(Debug, Clone, Copy, serde::Serialize, serde::Deserialize)]
struct TrustedPowerPolePlacement {
    ty: u8,
    pos: Position,
    rotation: Dir,
    grid: usize,
}

#[derive(Debug, Clone, serde::Serialize, serde::Deserialize)]
struct TrustedPowerPoleStageInfo {
    num_grids: usize,
    poles: Vec<TrustedPowerPolePlacement>,
}

#[derive(Debug, Clone, Copy, serde::Serialize, serde::Deserialize)]
struct TrustedAssemblerPlacement<RecipeIdxType: WeakIdxTrait> {
    ty: u8,
    pos: Position,
    rotation: Dir,
    recipe: Option<(Recipe<RecipeIdxType>, usize)>,
    pole_pos: Option<Position>,
    modules: usize,
}

#[derive(Debug, Clone, Copy, serde::Serialize, serde::Deserialize)]
struct TrustedLabPlacement {
    ty: u8,
    pos: Position,
    rotation: Dir,
    grid: Option<(Position, usize)>,
    modules: usize,
}

#[derive(Debug, Clone, Copy, serde::Serialize, serde::Deserialize)]
struct TrustedBeaconPlacement {
    ty: u8,
    pos: Position,
    rotation: Dir,
    pole_pos: Option<(Position, usize)>,
    modules: usize,
}

#[derive(Debug, Clone, Copy, serde::Serialize, serde::Deserialize)]
struct TrustedChestPlacement<ItemIdxType: WeakIdxTrait> {
    ty: u8,
    pos: Position,
    rotation: Dir,
    item: Option<Item<ItemIdxType>>,
    slot_limit: Option<usize>,
}

#[derive(Debug, Clone, serde::Serialize, serde::Deserialize)]
struct PipeSystem<ItemIdxType: WeakIdxTrait> {
    fluid: Option<Item<ItemIdxType>>,
    size: u64,
    tanks: Vec<TrustedFluidTankPlacement>,
}

#[derive(Debug, Clone, serde::Serialize, serde::Deserialize)]
struct TrustedPipeStageInfo<ItemIdxType: WeakIdxTrait> {
    fluid_networks: Vec<PipeSystem<ItemIdxType>>,
}

#[derive(Debug, Clone, serde::Serialize, serde::Deserialize)]
struct TrustedFluidTankPlacement {
    ty: u8,
    pos: Position,
    rotation: Dir,
    connection_offsets: Vec<[i16; 2]>,
    direct_connections: Vec<(FluidConnectionDir, Position)>,
}

#[derive(Debug, Clone, serde::Serialize, serde::Deserialize)]
pub struct ParGenerateInfo<ItemIdxType: WeakIdxTrait, RecipeIdxType: WeakIdxTrait> {
    module_combinations: Vec<Box<[Option<ModuleTy>]>>,
    power_pole_actions: TrustedPowerPoleStageInfo,
    beacon_actions: Vec<TrustedBeaconPlacement>,
    assembler_actions: Vec<TrustedAssemblerPlacement<RecipeIdxType>>,
    lab_actions: Vec<TrustedLabPlacement>,
    chest_actions: Vec<TrustedChestPlacement<ItemIdxType>>,

    pipe_actions: TrustedPipeStageInfo<ItemIdxType>,
    inserter_actions: Vec<ActionType<ItemIdxType, RecipeIdxType>>,

    belt_actions: Vec<Belt<ItemIdxType>>,

    rest_actions: Vec<ActionType<ItemIdxType, RecipeIdxType>>,
}

impl<ItemIdxType: IdxTrait, RecipeIdxType: IdxTrait> ParGenerateInfo<ItemIdxType, RecipeIdxType> {
    pub fn from_gamestate(
        world: &World<ItemIdxType, RecipeIdxType>,
        sim_state: &SimulationState<ItemIdxType, RecipeIdxType>,
        aux_data: &AuxillaryData,
        data_store: &DataStore<ItemIdxType, RecipeIdxType>,
    ) -> Self {
        let module_combinations = world
            .module_slot_dedup_table
            .iter()
            .map(|v| v.0.slice.into())
            .collect();

        let num_grids = sim_state
            .factory
            .power_grids
            .power_grids
            .iter()
            .filter(|pg| !pg.is_placeholder)
            .count();

        let grid_id_to_idx: Vec<usize> = sim_state
            .factory
            .power_grids
            .power_grids
            .iter()
            .enumerate()
            .map(|(i, _pg)| {
                sim_state.factory.power_grids.power_grids[0..i]
                    .iter()
                    .filter(
                        |pg: &&crate::power::power_grid::PowerGrid<ItemIdxType, RecipeIdxType>| {
                            !pg.is_placeholder
                        },
                    )
                    .count()
            })
            .collect();

        let pole_pos_map = |pole_pos: &Position| {
            grid_id_to_idx[sim_state.factory.power_grids.pole_pos_to_grid_id[pole_pos] as usize]
        };

        let poles: Vec<TrustedPowerPolePlacement> = world
            .get_chunks()
            .flat_map(|chunk| chunk.get_entities())
            .filter_map(|e| match e {
                &Entity::PowerPole { ty, pos } => Some(TrustedPowerPolePlacement {
                    ty,
                    pos,
                    // TODO:
                    rotation: Dir::North,
                    grid: pole_pos_map(&pos),
                }),

                _ => None,
            })
            .collect();

        let beacon_actions = world
            .get_chunks()
            .flat_map(|chunk| chunk.get_entities())
            .filter_map(|e| match e {
                &Entity::Beacon {
                    ty,
                    pos,
                    modules,
                    pole_position,
                } => Some(TrustedBeaconPlacement {
                    ty,
                    pos,
                    // TODO:
                    rotation: Dir::North,
                    pole_pos: pole_position
                        .map(|(pole_pos, _)| (pole_pos, pole_pos_map(&pole_pos))),
                    modules: modules as usize,
                }),

                _ => None,
            })
            .collect();

        let assembler_actions = world
            .get_chunks()
            .flat_map(|chunk| chunk.get_entities())
            .filter_map(|e| match e {
                &Entity::Assembler {
                    ty,
                    pos,
                    modules,
                    info,
                    rotation,
                } => Some(TrustedAssemblerPlacement {
                    ty,
                    pos,
                    rotation,
                    recipe: match info {
                        AssemblerInfo::UnpoweredNoRecipe => None,
                        AssemblerInfo::Unpowered(recipe) => None,
                        AssemblerInfo::PoweredNoRecipe(_) => None,
                        AssemblerInfo::Powered {
                            id, pole_position, ..
                        } => Some((id.recipe, pole_pos_map(&pole_position))),
                    },
                    pole_pos: match info {
                        AssemblerInfo::UnpoweredNoRecipe => None,
                        AssemblerInfo::Unpowered(_) => None,
                        AssemblerInfo::PoweredNoRecipe(position) => Some(position),
                        AssemblerInfo::Powered { pole_position, .. } => Some(pole_position),
                    },
                    modules: modules as usize,
                }),

                _ => None,
            })
            .collect();

        let lab_actions = world
            .get_chunks()
            .flat_map(|chunk| chunk.get_entities())
            .filter_map(|e| match e {
                &Entity::Lab {
                    pos,
                    ty,
                    modules,
                    pole_position,
                } => Some(TrustedLabPlacement {
                    ty,
                    pos,
                    // TODO:
                    rotation: Dir::North,
                    grid: pole_position.map(|(pole_pos, _, _)| (pole_pos, pole_pos_map(&pole_pos))),
                    modules: modules as usize,
                }),

                _ => None,
            })
            .collect();

        let chest_actions = world
            .get_chunks()
            .flat_map(|chunk| chunk.get_entities())
            .filter_map(|e| match e {
                &Entity::Chest {
                    ty,
                    pos,
                    item,
                    slot_limit,
                } => Some(TrustedChestPlacement {
                    ty,
                    pos,
                    // TODO:
                    rotation: Dir::North,
                    item: item.map(|(item, _)| item),
                    slot_limit: Some(slot_limit.into()),
                }),

                _ => None,
            })
            .collect();

        let mut pipe_networks: Vec<PipeSystem<ItemIdxType>> = vec![];
        let mut pipe_id_to_idx: HashMap<crate::liquid::FluidSystemId<ItemIdxType>, usize> =
            HashMap::default();

        for (pos, dir, ty, id) in world
            .get_chunks()
            .flat_map(|chunk| chunk.get_entities())
            .filter_map(|e| match e {
                &Entity::FluidTank { ty, pos, rotation } => {
                    let id = sim_state.factory.fluid_store.fluid_box_pos_to_network_id[&pos];
                    Some((pos, rotation, ty, id))
                },

                _ => None,
            })
        {
            let num_direct_connections = sim_state
                .factory
                .fluid_store
                .get_fluid_network(id)
                .graph
                .weak_components_of_node(pos)
                .count();

            let direct_connections = if num_direct_connections == 0 {
                vec![]
            } else {
                let tank_size = data_store.fluid_tank_infos[ty as usize].size;

                let search_range = data_store.fluid_tank_infos[ty as usize]
                    .fluid_connections
                    .iter()
                    .map(|v| match v.kind {
                        crate::data::PipeConnectionType::Direct => 1,
                        crate::data::PipeConnectionType::Underground { max_distance, .. } => {
                            u16::from(max_distance)
                        },
                    })
                    .max()
                    .unwrap();

                world
                    .get_entities_colliding_with(
                        Position {
                            x: pos.x - i32::from(search_range),
                            y: pos.y - i32::from(search_range),
                        },
                        tank_size.map(|v| v + 2 * search_range).into(),
                        data_store,
                    )
                    .into_iter()
                    .flat_map(|e| match e {
                        Entity::Assembler {
                            ty,
                            pos,
                            rotation,
                            info:
                                AssemblerInfo::Powered {
                                    id: assembler_id, ..
                                },
                            ..
                        } => {
                            match data_store.recipe_to_items[assembler_id.recipe.into_usize()]
                                .iter()
                                .find(|(dir, item)| *item == id.fluid.unwrap())
                            {
                                Some((dir, _)) => Some(
                                    data_store.assembler_info[*ty as usize]
                                        .fluid_connections
                                        .iter()
                                        .map(move |conn| {
                                            (
                                                *pos,
                                                *rotation,
                                                data_store.assembler_info[*ty as usize]
                                                    .size(*rotation),
                                                conn,
                                                match dir {
                                                    crate::data::ItemRecipeDir::Ing => {
                                                        FluidConnectionDir::Output
                                                    },
                                                    crate::data::ItemRecipeDir::Out => {
                                                        FluidConnectionDir::Input
                                                    },
                                                },
                                            )
                                        }),
                                ),
                                None => None,
                            }
                        },

                        _ => None,
                    })
                    .flatten()
                    .flat_map(
                        |(e_pos, e_dir, e_size, (fluid_conn, allowed_dir), fluid_flow_dir)| {
                            let v = can_fluid_tanks_connect_to_single_connection(
                                pos,
                                ty,
                                dir,
                                e_pos,
                                *fluid_conn,
                                e_dir,
                                e_size.into(),
                                data_store,
                            );

                            match v {
                                Some((conn_pos, conn_dir)) => Some((fluid_flow_dir, conn_pos)),
                                None => None,
                            }
                        },
                    )
                    .take(num_direct_connections)
                    .collect()
            };

            match pipe_id_to_idx.entry(id) {
                std::collections::hash_map::Entry::Occupied(occupied_entry) => {
                    let idx: usize = *occupied_entry.get();

                    pipe_networks[idx].tanks.push(TrustedFluidTankPlacement {
                        ty,
                        pos,
                        rotation: dir,
                        connection_offsets: sim_state
                            .factory
                            .fluid_store
                            .get_fluid_network(id)
                            .graph
                            .neighbors(pos)
                            .map(|neighbor_pos| {
                                [
                                    (neighbor_pos.x - pos.x).try_into().unwrap(),
                                    (neighbor_pos.y - pos.y).try_into().unwrap(),
                                ]
                            })
                            .collect(),
                        direct_connections,
                    });
                },
                std::collections::hash_map::Entry::Vacant(vacant_entry) => {
                    pipe_networks.push(PipeSystem {
                        fluid: id.fluid,
                        size: sim_state
                            .factory
                            .fluid_store
                            .get_fluid_network(id)
                            .storage_capacity as u64,
                        tanks: vec![TrustedFluidTankPlacement {
                            ty,
                            pos,
                            rotation: dir,
                            connection_offsets: sim_state
                                .factory
                                .fluid_store
                                .get_fluid_network(id)
                                .graph
                                .neighbors(pos)
                                .map(|neighbor_pos| {
                                    [
                                        (neighbor_pos.x - pos.x).try_into().unwrap(),
                                        (neighbor_pos.y - pos.y).try_into().unwrap(),
                                    ]
                                })
                                .collect(),
                            direct_connections,
                        }],
                    });

                    vacant_entry.insert(pipe_networks.len() - 1);
                },
            }
        }

        for v in pipe_networks.iter_mut() {
            v.tanks.sort_by_key(|tank| tank.pos);
            for tank in &mut v.tanks {
                // Only keep connections which will already be placed.
                // This also ensures there are no double connections
                tank.connection_offsets.retain(|[x_offs, y_offs]| {
                    let conn_pos = Position {
                        x: tank.pos.x + i32::from(*x_offs),
                        y: tank.pos.y + i32::from(*y_offs),
                    };

                    conn_pos < tank.pos
                });
            }
        }

        // let pipe_actions = world
        //     .get_chunks()
        //     .flat_map(|chunk| chunk.get_entities())
        //     .filter_map(|e| match e {
        //         &Entity::FluidTank { ty, pos, rotation } => {
        //             Some(ActionType::PlaceEntity(PlaceEntityInfo {
        //                 entities: EntityPlaceOptions::Single(PlaceEntityType::FluidTank {
        //                     ty,
        //                     pos,
        //                     rotation,
        //                 }),
        //                 force: false,
        //             }))
        //         },

        //         _ => None,
        //     })
        //     .collect();

        let inserter_actions = world
            .get_chunks()
            .flat_map(|chunk| chunk.get_entities())
            .filter_map(|e| match e {
                &Entity::Inserter {
                    ty,
                    user_movetime,
                    pos,
                    direction,
                    filter,
                    info,
                } => Some(ActionType::PlaceEntity(PlaceEntityInfo {
                    entities: EntityPlaceOptions::Single(PlaceEntityType::Inserter {
                        ty,
                        pos,
                        dir: direction,
                        filter,
                        user_movetime,
                    }),
                    force: false,
                })),

                _ => None,
            })
            .collect();

        let mut belts: Vec<Belt<ItemIdxType>> = vec![];
        let mut belt_id_to_idx: HashMap<crate::belt::BeltTileId<ItemIdxType>, usize> =
            HashMap::default();

        for (pos, dir, ty, id, belt_pos, kind) in world
            .get_chunks()
            .flat_map(|chunk| chunk.get_entities())
            .filter_map(|e| match e {
                &Entity::Belt {
                    pos,
                    direction,
                    ty,
                    id,
                    belt_pos,
                } => Some((pos, direction, ty, id, belt_pos, BeltEntityKind::Belt)),
                &Entity::Underground {
                    pos,
                    direction,
                    ty,
                    id,
                    belt_pos,
                    underground_dir,
                } => Some((
                    pos,
                    direction,
                    ty,
                    id,
                    belt_pos,
                    BeltEntityKind::Underground(underground_dir),
                )),

                _ => None,
            })
        {
            match belt_id_to_idx.entry(id) {
                std::collections::hash_map::Entry::Occupied(occupied_entry) => {
                    let idx: usize = *occupied_entry.get();
                    belts[idx].entities.push(BeltEntity {
                        kind,
                        pos,
                        dir,
                        belt_pos,
                    });
                },
                std::collections::hash_map::Entry::Vacant(vacant_entry) => {
                    let length = sim_state.factory.belts.get_len(id);
                    let belt_kind = sim_state.factory.belts.get_current_kind(id);
                    belts.push(Belt {
                        ty,
                        length,
                        kind: belt_kind,
                        entities: vec![BeltEntity {
                            kind,
                            pos,
                            dir,
                            belt_pos,
                        }],
                    });

                    vacant_entry.insert(belts.len() - 1);
                },
            }
        }

        let rest_actions = world
            .get_chunks()
            .flat_map(|chunk| chunk.get_entities())
            .filter_map(|e| match e {
                &Entity::SolarPanel {
                    pos,
                    ty,
                    pole_position,
                } => Some(ActionType::PlaceEntity(PlaceEntityInfo {
                    entities: EntityPlaceOptions::Single(PlaceEntityType::SolarPanel { pos, ty }),
                    force: false,
                })),
                &Entity::Splitter { pos, direction, id } => {
                    Some(ActionType::PlaceEntity(PlaceEntityInfo {
                        entities: EntityPlaceOptions::Single(PlaceEntityType::Splitter {
                            pos,
                            direction,
                            // FIXME:
                            ty: 0,
                            // FIXME:
                            in_mode: None,
                            // FIXME:
                            out_mode: None,
                        }),
                        force: false,
                    }))
                },
                _ => None,
            })
            .collect();

        Self {
            module_combinations,
            power_pole_actions: TrustedPowerPoleStageInfo { num_grids, poles },
            beacon_actions,
            assembler_actions,
            lab_actions,
            chest_actions,
            pipe_actions: TrustedPipeStageInfo {
                fluid_networks: pipe_networks,
            },
            inserter_actions,
            belt_actions: belts,
            rest_actions,
        }
    }
}

/// Its not very parallel for now, but it does use the fact that we know the generation order to skip a lot of searches
pub fn par_generate<ItemIdxType: IdxTrait, RecipeIdxType: IdxTrait>(
    world_size: BoundingBox,
    generation_info: ParGenerateInfo<ItemIdxType, RecipeIdxType>,
    positions: Vec<Position>,
    data_store: &DataStore<ItemIdxType, RecipeIdxType>,
) -> GameState<ItemIdxType, RecipeIdxType> {
    let _timer = Timer::new("par_generate");
    warn!("par_generate");
    let mut world = World::new_with_area(world_size.top_left, world_size.bottom_right);

    world.set_module_combinations_trusted(generation_info.module_combinations.clone());

    let num_grids = generation_info.power_pole_actions.num_grids;

    let mut grid_store = power_pole_stage(
        generation_info.power_pole_actions,
        &mut world,
        positions.iter().copied(),
        data_store,
    );

    let (assembler_entities, (belt_store, belt_entities)) = join!(
        || assembler_stage(
            &generation_info.module_combinations,
            &mut grid_store,
            generation_info.assembler_actions,
            num_grids,
            positions.iter().copied(),
            data_store,
        ),
        || belt_stage(
            generation_info.belt_actions,
            positions.iter().copied(),
            data_store,
        )
    );

    for ent in assembler_entities {
        world.add_entity_trusted(ent, data_store);
    }

    lab_stage(
        &mut world,
        &generation_info.module_combinations,
        &mut grid_store,
        generation_info.lab_actions,
        num_grids,
        positions.iter().copied(),
        data_store,
    );

    beacon_stage(
        &mut world,
        &mut grid_store,
        generation_info.beacon_actions,
        num_grids,
        positions.iter().copied(),
        data_store,
    );

    let mut chest_store = chest_stage(
        &mut world,
        generation_info.chest_actions,
        positions.iter().copied(),
        data_store,
    );

    let mut storage_storage_store = StorageStorageInserterStore::new(data_store);
    let fluid_store = pipe_stage(
        &mut world,
        &mut chest_store,
        &mut storage_storage_store,
        generation_info.pipe_actions.fluid_networks,
        positions.iter().copied(),
        data_store,
    );

    let mut sim_state = SimulationState {
        factory: Factory {
            power_grids: grid_store,
            belts: belt_store,
            chests: chest_store,
            fluid_store,
            storage_storage_inserters: storage_storage_store,
            ..Factory::new(data_store)
        },
        ..SimulationState::new(data_store)
    };

    {
        let _timer = Timer::new("belt_placement_stage");
        warn!("belt_placement_stage");
        for ent in belt_entities {
            world.add_belt_entity_trusted(ent, data_store);
        }
    }

    // splitter_stage();

    {
        let _timer = Timer::new("misc_stage");
        warn!("misc_stage");
        for base_pos in positions.iter().copied() {
            GameState::apply_actions(
                &mut sim_state,
                &mut world,
                generation_info
                    .rest_actions
                    .iter()
                    .map(|action| ReusableBlueprint::set_base_pos(action, base_pos)),
                data_store,
            );
        }
    }

    inserter_stage(
        &mut world,
        &mut sim_state,
        generation_info.inserter_actions,
        positions.iter().copied(),
        data_store,
    );

    GameState {
        world: Mutex::new(world),
        simulation_state: Mutex::new(sim_state),
        aux_data: Mutex::new(AuxillaryData::new(data_store)),
    }
}

// TODO: Do I want to swap beacon stage and assembler stage?
/// belt_stage ------------------------------------------------ inserter_stage
/// power_pole_stage -- assembler_stage -- lab_stage -- beacon_stage ../
/// chest_stage ....................................................../
///

fn power_pole_stage<ItemIdxType: IdxTrait, RecipeIdxType: IdxTrait>(
    pole_pole_stage_info: TrustedPowerPoleStageInfo,
    world: &mut World<ItemIdxType, RecipeIdxType>,

    base_positions: impl IntoIterator<Item = Position>,

    data_store: &DataStore<ItemIdxType, RecipeIdxType>,
) -> PowerGridStorage<ItemIdxType, RecipeIdxType> {
    let _timer = Timer::new("power_pole_stage");
    warn!("power_pole_stage");
    let TrustedPowerPoleStageInfo { num_grids, poles } = pole_pole_stage_info;

    let mut store = PowerGridStorage::new();
    for (i, base_pos) in base_positions.into_iter().enumerate() {
        let power_grids = store.trusted_create_power_grids(num_grids, data_store);
        assert_equal(
            power_grids.map(|v| v as usize),
            (i * num_grids)..((i + 1) * num_grids),
        );

        for action in poles.iter().copied() {
            let TrustedPowerPolePlacement {
                ty,
                pos,
                rotation,
                grid,
            } = action;

            let pos = Position {
                x: base_pos.x + pos.x,
                y: base_pos.y + pos.y,
            };
            debug_assert!((grid as usize) < num_grids);
            let grid = (i * num_grids) + grid;

            let size = data_store.power_pole_data[ty as usize].size;

            // FIXME: Rotation

            let conn_range = data_store.power_pole_data[ty as usize].connection_range;

            store.add_pole_trusted(
                pos,
                grid.try_into().unwrap(),
                world
                    .get_power_poles_which_could_connect_to_pole_at(
                        pos, size, conn_range, data_store,
                    )
                    .into_iter()
                    .map(|e| e.get_pos()),
            );

            world.add_entity_trusted(Entity::PowerPole { ty, pos }, data_store);
        }
    }
    store
}

fn belt_stage<ItemIdxType: IdxTrait, RecipeIdxType: IdxTrait>(
    belt_actions: Vec<Belt<ItemIdxType>>,
    base_positions: impl IntoIterator<Item = Position>,
    data_store: &DataStore<ItemIdxType, RecipeIdxType>,
) -> (
    BeltStore<ItemIdxType>,
    Vec<Entity<ItemIdxType, RecipeIdxType>>,
) {
    let _timer = Timer::new("belt_stage");
    warn!("belt_stage");
    let mut store = BeltStore::new(data_store);

    let mut ret = vec![];

    for base_pos in base_positions {
        for action in belt_actions.iter() {
            let Belt {
                ty,
                length,
                kind,
                entities,
            } = action;

            let id = match kind {
                BeltKind::Smart(item) => store.add_belt(SmartBelt::new(*ty, *length, *item)),
                BeltKind::Sushi => todo!("Make it possible to add sushi belts"),
                BeltKind::Empty => store.add_empty_belt(*ty, *length),
            };

            for &BeltEntity {
                kind,
                pos,
                dir,
                belt_pos,
            } in entities.iter()
            {
                let ent = match kind {
                    BeltEntityKind::Belt => Entity::Belt {
                        pos: Position {
                            x: base_pos.x + pos.x,
                            y: base_pos.y + pos.y,
                        },
                        direction: dir,
                        ty: *ty,
                        id,
                        belt_pos,
                    },
                    BeltEntityKind::Underground(underground_dir) => Entity::Underground {
                        pos: Position {
                            x: base_pos.x + pos.x,
                            y: base_pos.y + pos.y,
                        },
                        underground_dir,
                        direction: dir,
                        ty: *ty,
                        id,
                        belt_pos,
                    },
                };

                ret.push(ent);
            }
        }
    }

    (store, ret)
}

fn assembler_stage<ItemIdxType: IdxTrait, RecipeIdxType: IdxTrait>(
    dedup_table: &[Box<[Option<ModuleTy>]>],
    grid_store: &mut PowerGridStorage<ItemIdxType, RecipeIdxType>,
    assembler_actions: Vec<TrustedAssemblerPlacement<RecipeIdxType>>,
    num_grids: usize,
    base_positions: impl IntoIterator<Item = Position>,
    data_store: &DataStore<ItemIdxType, RecipeIdxType>,
) -> Vec<Entity<ItemIdxType, RecipeIdxType>> {
    let _timer = Timer::new("assembler_stage");
    warn!("assembler_stage");
    let mut ret = vec![];

    for (i, base_pos) in base_positions.into_iter().enumerate() {
        for action in assembler_actions.iter().copied() {
            let TrustedAssemblerPlacement {
                ty,
                pos,
                rotation,
                recipe,
                pole_pos,
                modules,
            } = action;

            let pole_pos = pole_pos.map(|pole_pos| Position {
                x: base_pos.x + pole_pos.x,
                y: base_pos.y + pole_pos.y,
            });

            let pos = Position {
                x: base_pos.x + pos.x,
                y: base_pos.y + pos.y,
            };

            let info = if let Some((recipe, power_grid_id)) = recipe {
                debug_assert!((power_grid_id as usize) < num_grids);
                let power_grid_id = (i * num_grids) + power_grid_id;
                let pole_pos = pole_pos.unwrap();

                let (weak_index, index) = grid_store.power_grids[power_grid_id].add_assembler(
                    ty,
                    power_grid_id.try_into().unwrap(),
                    recipe,
                    &dedup_table[modules],
                    pole_pos,
                    pos,
                    data_store,
                );

                // grid_store.add_beacon_affected_entity(
                //     todo!("Easy"),
                //     todo!("This is the hard one"),
                //     todo!("Easy as well"),
                //     data_store,
                // );

                Some((pole_pos, weak_index, index))
            } else {
                None
            };

            let ent = Entity::Assembler {
                ty,
                pos,
                modules: modules as u32,
                info: match info {
                    Some((pole_pos, id, weak_idx)) => {
                        crate::frontend::world::tile::AssemblerInfo::Powered {
                            id,
                            pole_position: pole_pos,
                            weak_index: weak_idx,
                        }
                    },
                    None => match pole_pos {
                        Some(pole) => {
                            crate::frontend::world::tile::AssemblerInfo::PoweredNoRecipe(pole)
                        },
                        None => {
                            assert!(recipe.is_none());
                            crate::frontend::world::tile::AssemblerInfo::UnpoweredNoRecipe
                        },
                    },
                },
                rotation,
            };

            ret.push(ent);
        }
    }

    ret
}

fn lab_stage<ItemIdxType: IdxTrait, RecipeIdxType: IdxTrait>(
    world: &mut World<ItemIdxType, RecipeIdxType>,
    dedup_table: &[Box<[Option<ModuleTy>]>],
    grid_store: &mut PowerGridStorage<ItemIdxType, RecipeIdxType>,
    lab_actions: Vec<TrustedLabPlacement>,
    num_grids: usize,
    base_positions: impl IntoIterator<Item = Position>,
    data_store: &DataStore<ItemIdxType, RecipeIdxType>,
) {
    let _timer = Timer::new("lab_stage");
    warn!("lab_stage");
    for (i, base_pos) in base_positions.into_iter().enumerate() {
        for action in lab_actions.iter().copied() {
            let TrustedLabPlacement {
                ty,
                pos,
                rotation,
                grid,
                modules,
            } = action;

            let grid = grid.map(|(pole_pos, grid)| {
                (
                    Position {
                        x: base_pos.x + pole_pos.x,
                        y: base_pos.y + pole_pos.y,
                    },
                    grid + (i * num_grids),
                )
            });

            let pos = Position {
                x: base_pos.x + pos.x,
                y: base_pos.y + pos.y,
            };

            let pole_pos = if let Some((pole_pos, grid_id)) = grid {
                let (weak_index, index) = grid_store.power_grids[grid_id as usize].add_lab(
                    pos,
                    ty,
                    &dedup_table[modules],
                    pole_pos,
                    data_store,
                );

                Some((pole_pos, weak_index, index))
            } else {
                None
            };

            let ent = Entity::Lab {
                pos,
                ty,
                modules: modules as u32,
                pole_position: pole_pos,
            };

            world.add_entity_trusted(ent, data_store);
        }
    }
}

fn beacon_stage<ItemIdxType: IdxTrait, RecipeIdxType: IdxTrait>(
    world: &mut World<ItemIdxType, RecipeIdxType>,
    grid_store: &mut PowerGridStorage<ItemIdxType, RecipeIdxType>,
    beacon_actions: Vec<TrustedBeaconPlacement>,
    num_grids: usize,
    base_positions: impl IntoIterator<Item = Position>,
    data_store: &DataStore<ItemIdxType, RecipeIdxType>,
) {
    let _timer = Timer::new("beacon_stage");
    warn!("beacon_stage");
    for (i, base_pos) in base_positions.into_iter().enumerate() {
        for action in beacon_actions.iter().copied() {
            let TrustedBeaconPlacement {
                ty,
                pos,
                rotation,
                pole_pos,
                modules,
            } = action;

            let pole_pos = pole_pos.map(|(pole_pos, grid)| {
                (
                    Position {
                        x: base_pos.x + pole_pos.x,
                        y: base_pos.y + pole_pos.y,
                    },
                    grid + (i * num_grids),
                )
            });

            let pos = Position {
                x: base_pos.x + pos.x,
                y: base_pos.y + pos.y,
            };

            let size = data_store.beacon_info[ty as usize].size;

            let pole_pos = if let Some((pole_pos, grid)) = pole_pos {
                let affected_entities: Vec<BeaconAffectedEntity<RecipeIdxType>> = {
                    let range = data_store.beacon_info[usize::from(ty)].effect_range;

                    let beacon_search_size = range;
                    let beacon_effect_top_left = Position {
                        x: pos.x - (range.0 as i32 - size.0 as i32) / 2,
                        y: pos.y - (range.1 as i32 - size.1 as i32) / 2,
                    };

                    world
                        .get_entities_colliding_with(
                            beacon_effect_top_left,
                            beacon_search_size,
                            data_store,
                        )
                        .into_iter()
                        .filter_map(|e| match e {
                            Entity::Assembler {
                                info: AssemblerInfo::Powered { id, .. },
                                ..
                            } => Some(BeaconAffectedEntity::Assembler { id: *id }),
                            Entity::Assembler { .. } => None,
                            Entity::PowerPole { .. } => None,
                            Entity::Lab {
                                pole_position: Some((_pole_pos, _weak_idx, index)),
                                ..
                            } => Some(BeaconAffectedEntity::Lab {
                                grid: grid.try_into().unwrap(),
                                index: *index,
                            }),
                            Entity::Lab {
                                pole_position: None,
                                ..
                            } => None,
                            Entity::Beacon { .. } => None,

                            _ => unreachable!(),
                        })
                        .collect()
                };

                let weak_index = grid_store.add_beacon_to_grid(
                    ty,
                    pos,
                    pole_pos,
                    grid as u16,
                    &world.module_slot_dedup_table[modules],
                    affected_entities,
                    data_store,
                );

                Some((pole_pos, weak_index))
            } else {
                None
            };

            let ent = Entity::Beacon {
                pos,
                ty,
                modules: modules as u32,
                pole_position: pole_pos,
            };

            world.add_entity_trusted(ent, data_store);
        }
    }
}

fn inserter_stage<ItemIdxType: IdxTrait, RecipeIdxType: IdxTrait>(
    world: &mut World<ItemIdxType, RecipeIdxType>,
    sim_state: &mut SimulationState<ItemIdxType, RecipeIdxType>,
    mut inserter_actions: Vec<ActionType<ItemIdxType, RecipeIdxType>>,
    base_positions: impl IntoIterator<Item = Position> + Clone,
    data_store: &DataStore<ItemIdxType, RecipeIdxType>,
) {
    let _timer = Timer::new("inserter_stage");
    warn!("inserter_stage");
    inserter_actions.sort_by_key(|a| a.get_pos());
    for action in inserter_actions {
        for base_pos in base_positions.clone() {
            match action {
                ActionType::PlaceEntity(PlaceEntityInfo {
                    entities: EntityPlaceOptions::Single(ent),
                    force: _,
                }) => match ent {
                    crate::frontend::world::tile::PlaceEntityType::Inserter {
                        ty,
                        pos,
                        dir,
                        filter,

                        user_movetime,
                    } => world
                        .add_entity(
                            Entity::Inserter {
                                ty,
                                user_movetime,
                                pos: Position {
                                    x: base_pos.x + pos.x,
                                    y: base_pos.y + pos.y,
                                },
                                direction: dir,
                                filter,
                                info: crate::frontend::world::tile::InserterInfo::NotAttached {},
                            },
                            sim_state,
                            data_store,
                        )
                        .unwrap(),

                    _ => unreachable!(),
                },

                _ => unreachable!(),
            }
        }
    }
}

fn chest_stage<ItemIdxType: IdxTrait, RecipeIdxType: IdxTrait>(
    world: &mut World<ItemIdxType, RecipeIdxType>,
    chest_actions: Vec<TrustedChestPlacement<ItemIdxType>>,
    base_positions: impl IntoIterator<Item = Position>,
    data_store: &DataStore<ItemIdxType, RecipeIdxType>,
) -> FullChestStore<ItemIdxType> {
    let _timer = Timer::new("chest_stage");
    warn!("chest_stage");
    let mut store = FullChestStore {
        stores: (0..data_store.item_display_names.len())
            .map(|id| Item {
                id: id.try_into().unwrap(),
            })
            .map(|item| MultiChestStore::new(item))
            .collect(),
    };

    for base_pos in base_positions {
        for TrustedChestPlacement {
            ty,
            pos,
            rotation,
            item,
            slot_limit,
        } in chest_actions.iter().copied()
        {
            let pos = Position {
                x: base_pos.x + pos.x,
                y: base_pos.y + pos.y,
            };

            let item = if let Some(item) = item {
                let res = store.stores[item.into_usize()].add_chest(
                    ty,
                    slot_limit
                        .map(|v| v as u8)
                        .unwrap_or(data_store.chest_num_slots[usize::from(ty)]),
                    data_store,
                );

                Some((item, res))
            } else {
                None
            };

            let ent = Entity::Chest {
                ty,
                pos,
                item,
                slot_limit: slot_limit
                    .map(|v| v as u8)
                    .unwrap_or(data_store.chest_num_slots[usize::from(ty)]),
            };

            world.add_entity_trusted(ent, data_store);
        }
    }

    store
}

fn pipe_stage<ItemIdxType: IdxTrait, RecipeIdxType: IdxTrait>(
    world: &mut World<ItemIdxType, RecipeIdxType>,
    chest_store: &mut FullChestStore<ItemIdxType>,
    storage_storage_store: &mut StorageStorageInserterStore,
    pipe_actions: Vec<PipeSystem<ItemIdxType>>,
    base_positions: impl IntoIterator<Item = Position>,
    data_store: &DataStore<ItemIdxType, RecipeIdxType>,
) -> FluidSystemStore<ItemIdxType> {
    let _timer = Timer::new("pipe_stage");
    warn!("pipe_stage");
    let mut store: FluidSystemStore<ItemIdxType> = FluidSystemStore::new(data_store);

    for base_pos in base_positions {
        for PipeSystem { fluid, size, tanks } in pipe_actions.iter() {
            let id = store.trusted_add_fluid_network_without_fluid_box(*fluid, chest_store);

            for TrustedFluidTankPlacement {
                ty,
                pos,
                rotation,
                connection_offsets,
                direct_connections,
            } in tanks
            {
                let pos = Position {
                    x: pos.x + base_pos.x,
                    y: pos.y + base_pos.y,
                };

                store.trusted_add_fluid_box(
                    id,
                    pos,
                    data_store.fluid_tank_infos[*ty as usize].capacity,
                    connection_offsets
                        .into_iter()
                        .map(|[x_offs, y_offs]| Position {
                            x: pos.x + i32::from(*x_offs),
                            y: pos.y + i32::from(*y_offs),
                        }),
                    direct_connections.into_iter().map(|(dir, pos)| {
                        let pos = Position {
                            x: pos.x + base_pos.x,
                            y: pos.y + base_pos.y,
                        };

                        (
                            *dir,
                            fluid.unwrap(),
                            match world.get_entity_at(pos, data_store).unwrap() {
                                Entity::Assembler {
                                    info: AssemblerInfo::Powered { id, .. },
                                    ..
                                } => Storage::Assembler {
                                    grid: id.grid,
                                    index: id.assembler_index,
                                    recipe_idx_with_this_item: data_store
                                        .recipe_to_translated_index[&(id.recipe, fluid.unwrap())],
                                },
                                _ => unreachable!(),
                            },
                            pos,
                        )
                    }),
                    chest_store,
                    storage_storage_store,
                    data_store,
                );

                let ent = Entity::FluidTank {
                    ty: *ty,
                    pos,
                    rotation: *rotation,
                };

                world.add_entity_trusted(ent, data_store);

                assert_eq!(store.fluid_box_pos_to_network_id.get(&pos), Some(&id));
            }

            assert_eq!(
                u64::from(store.get_fluid_network(id).storage_capacity),
                *size
            );
        }
    }

    store
}

// fn pipe_stage<ItemIdxType: IdxTrait, RecipeIdxType: IdxTrait>(
//     world: &mut World<ItemIdxType, RecipeIdxType>,
//     sim_state: &mut SimulationState<ItemIdxType, RecipeIdxType>,
//     mut pipe_actions: Vec<ActionType<ItemIdxType, RecipeIdxType>>,
//     base_positions: impl IntoIterator<Item = Position>,
//     data_store: &DataStore<ItemIdxType, RecipeIdxType>,
// ) {
//     let _timer = Timer::new("pipe_stage");
//     warn!("pipe_stage");
//     pipe_actions.sort_by_key(|a| a.get_pos());
//     for base_pos in base_positions {
//         for mut action in pipe_actions.iter().cloned() {
//             let ActionType::PlaceEntity(PlaceEntityInfo {
//                 entities: EntityPlaceOptions::Single(PlaceEntityType::FluidTank { pos, .. }),
//                 force: _,
//             }) = &mut action
//             else {
//                 unreachable!()
//             };

//             *pos = Position {
//                 x: base_pos.x + pos.x,
//                 y: base_pos.y + pos.y,
//             };

//             GameState::apply_actions(sim_state, world, std::iter::once(action), data_store);
//         }
//     }
// }

struct Timer {
    start: std::time::Instant,
    text: &'static str,
}

impl Timer {
    fn new(text: &'static str) -> Self {
        Self {
            start: std::time::Instant::now(),
            text,
        }
    }
}

impl Drop for Timer {
    fn drop(&mut self) {
        let dur = self.start.elapsed();
        warn!("[{}]: {:?}", self.text, dur);
    }
}
