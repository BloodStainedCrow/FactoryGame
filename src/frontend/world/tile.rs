use std::{
    collections::{BTreeMap, BTreeSet, HashMap},
    marker::PhantomData,
    ops::{Add, ControlFlow},
};

use enum_map::{Enum, EnumMap};
use strum::EnumIter;

use itertools::Itertools;

use crate::{
    belt::{splitter::SplitterDistributionMode, BeltTileId},
    data::DataStore,
    frontend::world,
    inserter::Storage,
    item::{usize_from, IdxTrait, Item, Recipe, WeakIdxTrait},
    power::power_grid::{PowerGridEntity, PowerGridIdentifier},
    rendering::app_state::{calculate_inserter_positions, SimulationState},
    TICKS_PER_SECOND_LOGIC,
};

use super::{sparse_grid::SparseGrid, Position};

pub const BELT_LEN_PER_TILE: u16 = 4;

pub const CHUNK_SIZE: usize = 16;
pub const CHUNK_SIZE_FLOAT: f32 = 16.0;

#[derive(Debug, Clone, Copy, serde::Serialize, serde::Deserialize)]
pub enum FloorTile {
    Empty,
    Concrete,
    Water,
}

#[derive(Debug, Clone, serde::Deserialize, serde::Serialize)]
pub struct Chunk<ItemIdxType: WeakIdxTrait, RecipeIdxType: WeakIdxTrait> {
    pub floor_tiles: [[FloorTile; CHUNK_SIZE]; CHUNK_SIZE],
    entities: Vec<Entity<ItemIdxType, RecipeIdxType>>,
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
            movement_speed: 1.0 / (TICKS_PER_SECOND_LOGIC as f32),
            inventory: Default::default(),
        }
    }
}

#[derive(Debug, Clone, serde::Deserialize, serde::Serialize)]
pub struct World<ItemIdxType: WeakIdxTrait, RecipeIdxType: WeakIdxTrait> {
    // TODO: I don´t think I want FP
    pub players: Vec<PlayerInfo>,
    chunks: SparseGrid<Chunk<ItemIdxType, RecipeIdxType>>,

    belt_lookup: BeltIdLookup<ItemIdxType>,
    belt_recieving_input_directions: HashMap<Position, EnumMap<Dir, bool>>,
    power_grid_lookup: PowerGridConnectedDevicesLookup,
}

#[derive(Debug, Clone, serde::Deserialize, serde::Serialize)]
struct PowerGridConnectedDevicesLookup {
    grid_to_chunks: BTreeMap<PowerGridIdentifier, BTreeSet<(usize, usize)>>,
}

#[derive(Debug, Clone, serde::Deserialize, serde::Serialize)]
struct BeltIdLookup<ItemIdxType: WeakIdxTrait> {
    belt_id_to_chunks: BTreeMap<BeltTileId<ItemIdxType>, BTreeSet<(usize, usize)>>,
}

#[derive(Debug, Clone, Copy)]
pub struct ChunkMissingError;

#[derive(Debug, Clone, Copy)]
enum AddEntityError {
    ChunkMissingError(ChunkMissingError),
}

impl<ItemIdxType: IdxTrait, RecipeIdxType: IdxTrait> World<ItemIdxType, RecipeIdxType> {
    #[must_use]
    pub fn new() -> Self {
        let mut grid = SparseGrid::new(1_000_000, 1_000_000);

        grid.insert(
            100,
            100,
            Chunk {
                floor_tiles: [[FloorTile::Empty; CHUNK_SIZE]; CHUNK_SIZE],
                entities: vec![],
            },
        );
        grid.insert(
            99,
            100,
            Chunk {
                floor_tiles: [[FloorTile::Empty; CHUNK_SIZE]; CHUNK_SIZE],
                entities: vec![],
            },
        );
        grid.insert(
            100,
            99,
            Chunk {
                floor_tiles: [[FloorTile::Empty; CHUNK_SIZE]; CHUNK_SIZE],
                entities: vec![],
            },
        );
        grid.insert(
            101,
            100,
            Chunk {
                floor_tiles: [[FloorTile::Empty; CHUNK_SIZE]; CHUNK_SIZE],
                entities: vec![],
            },
        );
        grid.insert(
            100,
            101,
            Chunk {
                floor_tiles: [[FloorTile::Empty; CHUNK_SIZE]; CHUNK_SIZE],
                entities: vec![],
            },
        );

        Self {
            chunks: grid,
            players: vec![PlayerInfo::default(), PlayerInfo::default()],

            belt_lookup: BeltIdLookup {
                belt_id_to_chunks: BTreeMap::new(),
            },
            belt_recieving_input_directions: HashMap::new(),
            power_grid_lookup: PowerGridConnectedDevicesLookup {
                grid_to_chunks: BTreeMap::new(),
            },
        }
    }

    pub fn get_belt_possible_inputs(&mut self, pos: Position) -> &EnumMap<Dir, bool> {
        self.belt_recieving_input_directions.entry(pos).or_default()
    }

    pub fn get_chunks(&self) -> impl IntoIterator<Item = &Chunk<ItemIdxType, RecipeIdxType>> {
        self.chunks.occupied_entries().map(|(a, b)| b)
    }

    pub fn get_chunk(&self, x: usize, y: usize) -> Option<&Chunk<ItemIdxType, RecipeIdxType>> {
        self.chunks.get(x, y)
    }

    pub fn set_floor_tile(&mut self, pos: Position, floor_tile: FloorTile) -> Result<(), ()> {
        if let Some(chunk) = self.get_chunk_for_tile_mut(pos) {
            chunk.floor_tiles[pos.x % 16][pos.y % 16] = floor_tile;
            Ok(())
        } else {
            Err(())
        }
    }

    pub fn add_entity(
        &mut self,
        entity: Entity<ItemIdxType, RecipeIdxType>,
        sim_state: &SimulationState<ItemIdxType, RecipeIdxType>,
        data_store: &DataStore<ItemIdxType, RecipeIdxType>,
    ) -> Result<(), ()> {
        if !self.can_fit(entity.get_pos(), entity.get_size(data_store), data_store) {
            return Err(());
        }

        let pos = entity.get_pos();

        let chunk_pos = self.get_chunk_pos_for_tile(pos);

        if self.get_chunk_for_tile_mut(pos).is_none() {
            return Err(());
        }

        match entity {
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
                } => {
                    let lookup_grid =
                        sim_state.factory.power_grids.pole_pos_to_grid_id[&pole_position];
                    assert_eq!(grid, lookup_grid);
                    self.power_grid_lookup
                        .grid_to_chunks
                        .entry(grid)
                        .or_default()
                        .insert(chunk_pos);
                },
            },
            Entity::PowerPole { pos, .. } => {
                let grid = sim_state.factory.power_grids.pole_pos_to_grid_id[&pos];
                self.power_grid_lookup
                    .grid_to_chunks
                    .entry(grid)
                    .or_default()
                    .insert(chunk_pos);
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
            },
            Entity::Splitter {
                pos,
                direction,
                id: splitter_id,
            } => {
                let ids = sim_state
                    .factory
                    .splitters
                    .get_splitter_belt_ids(item, splitter_id)
                    .into_iter()
                    .flatten();

                for id in ids {
                    let chunk_pos_right = self.get_chunk_pos_for_tile(pos + direction.turn_right());

                    self.belt_lookup
                        .belt_id_to_chunks
                        .entry(id)
                        .or_default()
                        .extend([chunk_pos, chunk_pos_right]);

                    self.belt_recieving_input_directions
                        .entry(pos + direction)
                        .or_default()[direction.reverse()] = true;
                    self.belt_recieving_input_directions
                        .entry(pos + direction.turn_right() + direction)
                        .or_default()[direction.reverse()] = true;
                }
            },
            Entity::Inserter { info, .. } => match info {
                InserterInfo::NotAttached { .. } => {},
                InserterInfo::Attached(attached_inserter) => match attached_inserter {
                    AttachedInserter::BeltStorage { id, .. } => {
                        self.belt_lookup
                            .belt_id_to_chunks
                            .entry(id)
                            .or_default()
                            .insert(chunk_pos);
                    },
                    AttachedInserter::BeltBelt { item, inserter } => {
                        todo!("We need to store the position in the belt_id_lookup")
                    },
                    AttachedInserter::StorageStorage(_) => todo!(),
                },
            },
            Entity::Chest {
                ty,
                pos,
                item,
                index,
            } => {},
            Entity::Roboport {
                ty,
                pos,
                power_grid,
                network,
                id,
            } => {},
        }

        let chunk = self
            .get_chunk_for_tile_mut(pos)
            .expect("Chunk outside the world!");

        chunk.entities.push(entity);

        Ok(())
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
                    Entity::Assembler { info, .. } => match info {
                        AssemblerInfo::UnpoweredNoRecipe | AssemblerInfo::Unpowered(_) => {},
                        AssemblerInfo::PoweredNoRecipe(pole_position) => {},
                        AssemblerInfo::Powered {
                            id:
                                AssemblerID {
                                    grid: grid_in_id, ..
                                },
                            pole_position,
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
                        InserterInfo::Attached(attached_inserter) => match attached_inserter {
                            AttachedInserter::BeltStorage { id, belt_pos } => {
                                // TODO
                            },
                            AttachedInserter::BeltBelt { item, inserter } => {
                                // TODO
                            },
                            AttachedInserter::StorageStorage(_) => todo!(),
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
                    | Entity::Chest { .. } => {},
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
                    Entity::Assembler { .. } => {},
                    Entity::PowerPole { .. } => {},
                    Entity::Chest { .. } => {},
                    Entity::Roboport { .. } => {},
                    Entity::Belt { id, belt_pos, .. }
                    | Entity::Underground { id, belt_pos, .. } => {
                        if *id == old_id && belt_pos_earliest <= *belt_pos {
                            *id = new_id;
                        }
                    },
                    Entity::Splitter { pos, direction, .. } => todo!(),
                    Entity::Inserter { info, .. } => match info {
                        InserterInfo::NotAttached { .. } => {},
                        InserterInfo::Attached(attached_inserter) => match attached_inserter {
                            AttachedInserter::BeltStorage { id, belt_pos } => {
                                if *id == old_id && belt_pos_earliest <= *belt_pos {
                                    *id = new_id;
                                }
                            },
                            AttachedInserter::BeltBelt { item, inserter } => todo!(),
                            AttachedInserter::StorageStorage(_) => {},
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
                    Entity::Assembler { .. } => {},
                    Entity::PowerPole { .. } => {},
                    Entity::Chest { .. } => {},
                    Entity::Roboport { .. } => {},
                    Entity::Belt { id, belt_pos, .. }
                    | Entity::Underground { id, belt_pos, .. } => {
                        if *id == id_to_change {
                            *belt_pos = belt_pos
                                .checked_add_signed(offs)
                                .expect("belt_pos wrapped!");
                        }
                    },
                    Entity::Splitter { pos, direction, .. } => todo!(),
                    Entity::Inserter { info, .. } => match info {
                        InserterInfo::NotAttached { .. } => {},
                        InserterInfo::Attached(attached_inserter) => match attached_inserter {
                            AttachedInserter::BeltStorage { id, belt_pos, .. } => {
                                if *id == id_to_change {
                                    *belt_pos = belt_pos
                                        .checked_add_signed(offs)
                                        .expect("belt_pos wrapped!");
                                }
                            },
                            AttachedInserter::BeltBelt { item, inserter } => todo!(),
                            AttachedInserter::StorageStorage(_) => {},
                        },
                    },
                }
            }
        }
    }

    #[must_use]
    pub fn get_chunk_for_tile(&self, pos: Position) -> Option<&Chunk<ItemIdxType, RecipeIdxType>> {
        self.chunks.get(pos.x / CHUNK_SIZE, pos.y / CHUNK_SIZE)
    }

    pub fn get_chunk_for_tile_mut(
        &mut self,
        pos: Position,
    ) -> Option<&mut Chunk<ItemIdxType, RecipeIdxType>> {
        self.chunks.get_mut(pos.x / CHUNK_SIZE, pos.y / CHUNK_SIZE)
    }

    fn get_chunk_pos_for_tile(&self, pos: Position) -> (usize, usize) {
        (pos.x / CHUNK_SIZE, pos.y / CHUNK_SIZE)
    }

    pub fn get_chunk_mut(
        &mut self,
        chunk: (usize, usize),
    ) -> Option<&mut Chunk<ItemIdxType, RecipeIdxType>> {
        self.chunks.get_mut(chunk.0, chunk.1)
    }

    pub fn is_powered_by(
        &self,
        sim_state: &SimulationState<ItemIdxType, RecipeIdxType>,
        entity_pos: Position,
        entity_size: (u8, u8),
        data_store: &DataStore<ItemIdxType, RecipeIdxType>,
    ) -> Option<Position> {
        self.get_entities_colliding_with(
            Position {
                x: entity_pos.x - usize::from(data_store.max_power_search_range),
                y: entity_pos.y - usize::from(data_store.max_power_search_range),
            },
            (
                2 * data_store.max_power_search_range + entity_size.0,
                2 * data_store.max_power_search_range + entity_size.1,
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

                let power_range = data_store.power_pole_data[usize::from(*ty)].power_range;
                let size: (u8, u8) = data_store.power_pole_data[usize::from(*ty)].size;
                if entity_pos.contained_in_sized(
                    entity_size,
                    Position {
                        x: pos.x - usize::from(power_range),
                        y: pos.y - usize::from(power_range),
                    },
                    (2 * power_range + size.0, 2 * power_range + size.1),
                ) {
                    Some(*pos)
                } else {
                    None
                }
            },
            _ => None,
        })
    }

    pub fn get_entities_colliding_with<'a, 'b>(
        &'a self,
        pos: Position,
        size: (u8, u8),
        data_store: &'b DataStore<ItemIdxType, RecipeIdxType>,
    ) -> impl IntoIterator<Item = &'a Entity<ItemIdxType, RecipeIdxType>, IntoIter: Clone>
           + use<'a, 'b, ItemIdxType, RecipeIdxType> {
        let bb_top_left = (pos.x, pos.y);

        let bb_bottom_right = (pos.x + usize::from(size.0), pos.y + usize::from(size.1));

        let chunk_range_x = (bb_top_left.0 / CHUNK_SIZE)..=(bb_bottom_right.0 / CHUNK_SIZE);
        let chunk_range_y = (bb_top_left.1 / CHUNK_SIZE)..=(bb_bottom_right.1 / CHUNK_SIZE);

        debug_assert!(chunk_range_x.clone().count() >= 1);
        debug_assert!(chunk_range_y.clone().count() >= 1);

        chunk_range_x
            .cartesian_product(chunk_range_y)
            .filter_map(|(chunk_x, chunk_y)| self.chunks.get(chunk_x, chunk_y))
            .map(|chunk| chunk.entities.iter())
            .flatten()
            .filter(move |e| {
                let e_pos = e.get_pos();
                let e_size = e.get_size(data_store);

                pos.contained_in_sized(size, e_pos, e_size)
            })
    }

    pub fn mutate_entities_colliding_with<'a, 'b>(
        &'a mut self,
        pos: Position,
        size: (u8, u8),
        data_store: &'b DataStore<ItemIdxType, RecipeIdxType>,
        mut f: impl FnMut(&mut Entity<ItemIdxType, RecipeIdxType>) -> ControlFlow<(), ()>,
    ) {
        let bb_top_left = (pos.x, pos.y);

        let bb_bottom_right = (pos.x + usize::from(size.0), pos.y + usize::from(size.1));

        let chunk_range_x = (bb_top_left.0 / CHUNK_SIZE)..=(bb_bottom_right.0 / CHUNK_SIZE);
        let chunk_range_y = (bb_top_left.1 / CHUNK_SIZE)..=(bb_bottom_right.1 / CHUNK_SIZE);

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

                    if (pos.x + usize::from(size.0)) <= e_pos.x
                        || (pos.y + usize::from(size.1)) <= e_pos.y
                        || (pos.x) >= (e_pos.x + usize::from(e_size.0))
                        || (pos.y) >= (e_pos.y + usize::from(e_size.1))
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

        if let Some(entity) = entity {
            let e_pos = entity.get_pos();
            let e_size = entity.get_size(data_store);
            let max_inserter_range = data_store.max_inserter_search_range;

            let inserter_search_area = (
                Position {
                    x: pos.x - max_inserter_range as usize,
                    y: pos.y - max_inserter_range as usize,
                },
                (
                    2 * max_inserter_range + e_size.0,
                    2 * max_inserter_range + e_size.1,
                ),
            );

            match entity {
                Entity::Assembler { pos, info } => match info {
                    AssemblerInfo::UnpoweredNoRecipe
                    | AssemblerInfo::Unpowered(_)
                    | AssemblerInfo::PoweredNoRecipe(_) => {
                        // Nothing to do besides removing the entity
                    },
                    AssemblerInfo::Powered {
                        id: assembler_id,
                        pole_position,
                    } => {
                        // TODO:
                        let assembler_removal_info = sim_state.factory.power_grids.power_grids
                            [assembler_id.grid as usize]
                            .as_mut()
                            .unwrap()
                            .remove_assembler(*assembler_id, data_store);
                    },
                },
                Entity::PowerPole {
                    ty,
                    pos,
                    connected_power_poles,
                } => {
                    let old_id = sim_state.factory.power_grids.pole_pos_to_grid_id[pos];

                    let (
                        _poles_which_changed,
                        machines_which_changed,
                        no_longer_connected_entity_positions,
                    ) = sim_state.factory.power_grids.remove_pole(*pos, data_store);

                    for index_update in machines_which_changed {
                        self.mutate_entities_colliding_with(
                            index_update.position,
                            (1, 1),
                            data_store,
                            |e| {
                                match (e, index_update.new_storage) {
                                    (
                                        Entity::Assembler {
                                            pos,
                                            info: AssemblerInfo::Powered { id, pole_position },
                                        },
                                        crate::power::power_grid::PowerGridEntity::Assembler {
                                            recipe,
                                            index,
                                        },
                                    ) => {
                                        assert_eq!(id.recipe, recipe);

                                        assert_eq!(id.grid, old_id);

                                        id.assembler_index = index;
                                        id.grid = index_update.new_grid;
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

                        // FIXME: HARDCODED!
                        let assembler_size = (3, 3);

                        let inserter_search_area = (
                            Position {
                                x: index_update.position.x - max_inserter_range as usize,
                                y: index_update.position.y - max_inserter_range as usize,
                            },
                            (
                                2 * max_inserter_range + assembler_size.0,
                                2 * max_inserter_range + assembler_size.1,
                            ),
                        );

                        let new_storages: Vec<_> = match index_update.new_storage {
                            PowerGridEntity::Assembler { recipe, index } => data_store
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
                            PowerGridEntity::Lab { index } => todo!(),
                            PowerGridEntity::LazyPowerProducer { item, index } => {
                                todo!("Expand Storage type")
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
                                    } => {
                                        // Nothing to do
                                    },
                                    Entity::Inserter {
                                        pos,
                                        direction,
                                        info,
                                    } => {
                                        let (start_pos, end_pos) =
                                            calculate_inserter_positions(*pos, *direction);

                                        if start_pos
                                            .contained_in(index_update.position, assembler_size)
                                            || end_pos
                                                .contained_in(index_update.position, assembler_size)
                                        {
                                            // This Inserter is connected to the entity we are removing!
                                            match info {
                                                InserterInfo::NotAttached {
                                                    start_pos,
                                                    end_pos,
                                                } => {
                                                    unreachable!()
                                                },
                                                InserterInfo::Attached(attached_inserter) => {
                                                    match attached_inserter {
                                                        AttachedInserter::BeltStorage {
                                                            id,
                                                            belt_pos,
                                                        } => match id {
                                                            BeltTileId::EmptyBeltId(_) => todo!(),
                                                            BeltTileId::BeltId(belt_id) => {
                                                                sim_state
                                                                    .factory
                                                                    .belts
                                                                    .get_belt_mut(*belt_id)
                                                                    .set_inserter_storage_id(
                                                                        *belt_pos,
                                                                        new_storages
                                                                            .iter()
                                                                            .copied()
                                                                            .find(|(item, _)| {
                                                                                **item
                                                                                    == belt_id.item
                                                                            })
                                                                            .unwrap()
                                                                            .1,
                                                                    );
                                                            },
                                                        },
                                                        AttachedInserter::BeltBelt {
                                                            item,
                                                            inserter,
                                                        } => {
                                                            todo!("Change BeltBelt Inserter Index")
                                                        },
                                                        AttachedInserter::StorageStorage(_) => {
                                                            todo!()
                                                        },
                                                    }
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
                                    Entity::Assembler { pos, info } => match info {
                                        AssemblerInfo::UnpoweredNoRecipe => unreachable!(),
                                        AssemblerInfo::Unpowered(recipe) => unreachable!(),
                                        AssemblerInfo::PoweredNoRecipe(position) => unreachable!(),
                                        AssemblerInfo::Powered { id, pole_position } => {
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

                                                // FIXME: Assembler type
                                                let new_id =
                                                    sim_state.factory.power_grids.power_grids
                                                        [usize::from(grid_id)]
                                                    .as_mut()
                                                    .unwrap()
                                                    .add_assembler(
                                                        0,
                                                        grid_id,
                                                        id.recipe,
                                                        new_pole_pos,
                                                        *pos,
                                                        data_store,
                                                    );

                                                *id = new_id;
                                            } else {
                                                // FIXME: This will delete items!
                                                *info = AssemblerInfo::Unpowered(id.recipe)
                                            }
                                        },
                                    },
                                    Entity::PowerPole { .. } => unreachable!(),

                                    _ => unreachable!(),
                                }
                                ControlFlow::Continue(())
                            },
                        );
                    }
                },

                Entity::Belt {
                    pos,
                    direction,
                    id,
                    belt_pos,
                } => todo!(),
                Entity::Underground {
                    pos,
                    underground_dir,
                    direction,
                    id,
                    belt_pos,
                } => todo!(),
                Entity::Splitter {
                    pos,
                    direction,
                    item,
                    id,
                } => todo!(),

                Entity::Chest {
                    ty,
                    pos,
                    item,
                    index,
                } => {
                    if let Some(item) = item {
                        let chest_removal_info = sim_state.factory.chests.stores
                            [usize_from(item.id)]
                        .remove_chest(*index, data_store);
                    } else {
                        todo!("What does not having an item mean?")
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
                } => {},
                Entity::Inserter {
                    pos,
                    direction,
                    info: InserterInfo::Attached(attached),
                } => match attached {
                    AttachedInserter::BeltStorage { id, belt_pos } => {
                        todo!("Remove BeltStorageInserter")
                    },
                    AttachedInserter::BeltBelt { item, inserter } => {
                        todo!("Remove BeltBelt Inserter")
                    },
                    AttachedInserter::StorageStorage(_) => todo!(),
                },
            }

            // Unattach inserters from self
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
                        } => {
                            // Nothing to do
                        },
                        Entity::Inserter {
                            pos,
                            direction,
                            info,
                        } => {
                            let (start_pos, end_pos) =
                                calculate_inserter_positions(*pos, *direction);

                            if start_pos.contained_in(e_pos, e_size)
                                || end_pos.contained_in(e_pos, e_size)
                            {
                                // This Inserter is connected to the entity we are removing!
                                match info {
                                    InserterInfo::NotAttached { start_pos, end_pos } => {
                                        unreachable!()
                                    },
                                    InserterInfo::Attached(attached_inserter) => {
                                        match attached_inserter {
                                            AttachedInserter::BeltStorage { id, belt_pos } => {
                                                todo!("Remove BeltStorageInserter")
                                            },
                                            AttachedInserter::BeltBelt { item, inserter } => {
                                                todo!("Remove BeltBelt Inserter")
                                            },
                                            AttachedInserter::StorageStorage(_) => todo!(),
                                        }
                                    },
                                }
                            }
                        },

                        _ => {},
                    }
                    ControlFlow::Continue(())
                },
            );
        } else {
            // Nothing to do
        }

        // TODO: Actually remove the entity!
        self.get_chunk_for_tile_mut(pos)
            .unwrap()
            .entities
            .retain(|e| !pos.contained_in(e.get_pos(), e.get_size(data_store)));
    }

    #[must_use]
    pub fn can_fit(
        &self,
        pos: Position,
        size: (u8, u8),
        data_store: &DataStore<ItemIdxType, RecipeIdxType>,
    ) -> bool {
        self.get_entities_colliding_with(pos, size, data_store)
            .into_iter()
            .next()
            .inspect(|e| {
                dbg!(e);
            })
            .is_none()
    }

    pub fn get_power_poles_which_could_connect_to_pole_at<'a, 'b>(
        &'a self,
        pole_pos: Position,
        pole_size: (u8, u8),
        connection_range: u8,
        data_store: &'b DataStore<ItemIdxType, RecipeIdxType>,
    ) -> impl IntoIterator<Item = &'a Entity<ItemIdxType, RecipeIdxType>, IntoIter: Clone>
           + Clone
           + use<'a, 'b, ItemIdxType, RecipeIdxType> {
        self.get_entities_colliding_with(
            Position {
                x: pole_pos.x - usize::from(connection_range),
                y: pole_pos.y - usize::from(connection_range),
            },
            (
                2 * connection_range + pole_size.0,
                2 * connection_range + pole_size.1,
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
    ) -> (u8, (u8, u8)) {
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

            pos.contained_in(e_pos, e_size)
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

            pos.contained_in(e_pos, e_size)
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

                (pos.x + usize::from(size.0)) <= e_pos.x
                    || (pos.y + usize::from(size.1)) <= e_pos.y
                    || (pos.x) >= (e_pos.x + usize::from(e_size.0))
                    || (pos.y) >= (e_pos.y + usize::from(e_size.1))
            })
    }

    // pub fn add_entity(&mut self, entity: Entity<ItemIdxType, RecipeIdxType>) {
    //     if let Entity::Belt {
    //         pos,
    //         direction,
    //         id,
    //         belt_pos: _belt_pos,
    //     } = entity
    //     {
    //         let v = self.belt_exit_chunks.entry(id).or_default();

    //         let my_chunk = (pos.x / CHUNK_SIZE, pos.y / CHUNK_SIZE);

    //         if (pos.x % CHUNK_SIZE).abs_diff(
    //             (pos.x.wrapping_add_signed(direction.into_offset().0.into())) % CHUNK_SIZE,
    //         ) > 1
    //         {
    //             v.insert((
    //                 my_chunk
    //                     .0
    //                     .wrapping_add_signed(direction.into_offset().0.into()),
    //                 my_chunk.1,
    //             ));
    //         }
    //         if (pos.y % CHUNK_SIZE).abs_diff(
    //             (pos.y.wrapping_add_signed(direction.into_offset().1.into())) % CHUNK_SIZE,
    //         ) > 1
    //         {
    //             v.insert((
    //                 my_chunk.0,
    //                 my_chunk
    //                     .1
    //                     .wrapping_add_signed(direction.into_offset().1.into()),
    //             ));
    //         }
    //         if (pos.x % CHUNK_SIZE).abs_diff(
    //             (pos.x
    //                 .wrapping_add_signed(direction.reverse().into_offset().0.into()))
    //                 % CHUNK_SIZE,
    //         ) > 1
    //         {
    //             v.insert((
    //                 my_chunk
    //                     .0
    //                     .wrapping_add_signed(direction.reverse().into_offset().0.into()),
    //                 my_chunk.1,
    //             ));
    //         }
    //         if (pos.y % CHUNK_SIZE).abs_diff(
    //             (pos.y
    //                 .wrapping_add_signed(direction.reverse().into_offset().1.into()))
    //                 % CHUNK_SIZE,
    //         ) > 1
    //         {
    //             v.insert((
    //                 my_chunk.0,
    //                 my_chunk
    //                     .1
    //                     .wrapping_add_signed(direction.reverse().into_offset().1.into()),
    //             ));
    //         }
    //     }

    //     self.entities.push(entity);
    // }

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
    },
}

#[derive(Debug, Clone, Copy, serde::Deserialize, serde::Serialize, PartialEq)]
pub enum InserterInfo<ItemIdxType: WeakIdxTrait> {
    NotAttached {
        start_pos: Position,
        end_pos: Position,
    },
    Attached(AttachedInserter<ItemIdxType>),
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
    StorageStorage(()),
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, serde::Deserialize, serde::Serialize)]
pub enum UndergroundDir {
    Entrance,
    Exit,
}

#[derive(Debug, Clone, serde::Deserialize, serde::Serialize, PartialEq)]
pub enum Entity<ItemIdxType: WeakIdxTrait, RecipeIdxType: WeakIdxTrait> {
    Assembler {
        pos: Position,
        info: AssemblerInfo<RecipeIdxType>,
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
        id: BeltTileId<ItemIdxType>,
        belt_pos: u16,
    },
    Underground {
        pos: Position,
        underground_dir: UndergroundDir,
        direction: Dir,
        id: BeltTileId<ItemIdxType>,
        belt_pos: u16,
    },
    Splitter {
        pos: Position,
        direction: Dir,
        id: usize,
    },
    Inserter {
        pos: Position,
        direction: Dir,

        info: InserterInfo<ItemIdxType>,
    },
    Chest {
        // This means at most 256 different types of Chest can exist, should be fine :)
        ty: u8,
        pos: Position,
        item: Option<Item<ItemIdxType>>,
        index: usize,
    },
    Roboport {
        // This means at most 256 different types of Roboports can exist, should be fine
        ty: u8,
        pos: Position,
        power_grid: Option<PowerGridIdentifier>,
        network: u16,
        id: u32,
    },
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
        }
    }

    pub fn get_size(&self, data_store: &DataStore<ItemIdxType, RecipeIdxType>) -> (u8, u8) {
        match self {
            Self::Assembler { .. } => (3, 3),
            Self::PowerPole { ty, .. } => (1, 1),
            Self::Belt { .. } => (1, 1),
            Self::Inserter { .. } => (1, 1),
            Self::Underground { .. } => (1, 1),
            Self::Splitter { direction, .. } => match direction {
                Dir::North => (2, 1),
                Dir::East => (1, 2),
                Dir::South => (2, 1),
                Dir::West => (1, 2),
            },
            Self::Chest { ty, .. } => data_store.chest_tile_sizes[*ty as usize],
            Self::Roboport { ty, .. } => (4, 4),
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
    Assembler(Position),
    Inserter {
        pos: Position,
        dir: Dir,
        /// The Item the inserter will move, must fit both the in and output side
        filter: Item<ItemIdxType>,
    },
    Belt {
        pos: Position,
        direction: Dir,
    },
    PowerPole {
        pos: Position,
        ty: u8,
    },
    Splitter {
        pos: Position,
        direction: Dir,
        in_mode: Option<SplitterDistributionMode>,
        out_mode: Option<SplitterDistributionMode>,
    },
    Chest {
        pos: Position,
    },
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
    pub assembler_index: u16,
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
            x: self.x.checked_add_signed(offs.0.into()).unwrap(),
            y: self.y.checked_add_signed(offs.1.into()).unwrap(),
        }
    }
}
