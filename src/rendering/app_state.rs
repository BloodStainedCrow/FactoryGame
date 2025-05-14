use std::{borrow::Borrow, fs::File, marker::PhantomData, ops::ControlFlow};

use crate::{
    belt::{
        belt::Belt, splitter::Splitter, BeltBeltInserterAdditionInfo, BeltBeltInserterInfo,
        BeltStore, BeltTileId, MultiBeltStore,
    },
    blueprint::Blueprint,
    chest::{FullChestStore, MultiChestStore},
    data::{DataStore, ItemRecipeDir},
    frontend::{
        action::{
            belt_placement::{handle_belt_placement, handle_splitter_placement},
            set_recipe::SetRecipeInfo,
            ActionType,
        },
        world::{
            self,
            tile::{
                AssemblerID, AssemblerInfo, AttachedInserter, BeltId, Dir, Entity, InserterInfo,
                World,
            },
            Position,
        },
    },
    inserter::{
        belt_belt_inserter::BeltBeltInserter, storage_storage_inserter::StorageStorageInserter,
        StaticID, Storage, MOVETIME,
    },
    item::{usize_from, IdxTrait, Item, Recipe, WeakIdxTrait},
    power::{power_grid::PowerGridIdentifier, PowerGridStorage, Watt},
    research::{ResearchProgress, TechState, Technology},
    statistics::{production::ProductionInfo, recipe::RecipeTickInfo, GenStatistics},
    storage_list::{
        full_to_by_item, grid_size, num_recipes, sizes, storages_by_item, FullStorages,
        SingleItemStorages,
    },
};
use itertools::Itertools;
use log::{error, info, trace, warn};
use rayon::iter::{IndexedParallelIterator, IntoParallelRefMutIterator, ParallelIterator};

use crate::frontend::action::place_tile::PositionInfo;

#[derive(Debug, Clone, serde::Deserialize, serde::Serialize)]
pub struct GameState<ItemIdxType: WeakIdxTrait, RecipeIdxType: WeakIdxTrait> {
    pub current_tick: u64,

    pub world: World<ItemIdxType, RecipeIdxType>,
    pub simulation_state: SimulationState<ItemIdxType, RecipeIdxType>,

    pub statistics: GenStatistics,
}

impl<ItemIdxType: IdxTrait, RecipeIdxType: IdxTrait> GameState<ItemIdxType, RecipeIdxType> {
    pub fn new(data_store: &DataStore<ItemIdxType, RecipeIdxType>) -> Self {
        Self {
            current_tick: 0,
            world: World::new(),
            simulation_state: SimulationState::new(data_store),
            statistics: GenStatistics::new(data_store),
        }
    }

    pub fn new_with_production(data_store: &DataStore<ItemIdxType, RecipeIdxType>) -> Self {
        let mut ret = Self {
            current_tick: 0,
            world: World::new(),
            simulation_state: SimulationState::new(data_store),
            statistics: GenStatistics::new(data_store),
        };

        let file = File::open("test_blueprints/red_sci.bp").unwrap();
        let bp: Blueprint<ItemIdxType, RecipeIdxType> = ron::de::from_reader(file).unwrap();

        for y_pos in (1590..30000).step_by(7) {
            for x_pos in (1590..3000).step_by(20) {
                if rand::random::<u16>() < u16::MAX / 10 {
                    ret.update(data_store);
                }

                bp.apply(Position { x: x_pos, y: y_pos }, &mut ret, data_store);
            }
        }

        ret
    }

    pub fn new_with_bp(data_store: &DataStore<ItemIdxType, RecipeIdxType>, bp_path: &str) -> Self {
        let mut ret = Self {
            current_tick: 0,
            world: World::new(),
            simulation_state: SimulationState::new(data_store),
            statistics: GenStatistics::new(data_store),
        };

        let file = File::open(bp_path).unwrap();
        let bp: Blueprint<ItemIdxType, RecipeIdxType> = ron::de::from_reader(file).unwrap();

        bp.apply(Position { x: 1590, y: 1590 }, &mut ret, data_store);

        ret
    }
}

#[derive(Debug, Clone, serde::Deserialize, serde::Serialize)]
pub struct SimulationState<ItemIdxType: WeakIdxTrait, RecipeIdxType: WeakIdxTrait> {
    tech_state: TechState,
    pub factory: Factory<ItemIdxType, RecipeIdxType>,
    // TODO:
}

impl<ItemIdxType: IdxTrait, RecipeIdxType: IdxTrait> SimulationState<ItemIdxType, RecipeIdxType> {
    pub fn new(data_store: &DataStore<ItemIdxType, RecipeIdxType>) -> Self {
        Self {
            tech_state: TechState {
                current_technology: Some(Technology { id: 0 }),
            },
            factory: Factory::new(data_store),
        }
    }
}

#[derive(Debug, Clone, serde::Deserialize, serde::Serialize)]
pub struct Factory<ItemIdxType: WeakIdxTrait, RecipeIdxType: WeakIdxTrait> {
    pub power_grids: PowerGridStorage<ItemIdxType, RecipeIdxType>,
    pub belts: BeltStore<ItemIdxType, RecipeIdxType>,
    pub storage_storage_inserters: StorageStorageInserterStore<RecipeIdxType>,
    pub chests: FullChestStore<ItemIdxType>,
}

#[derive(Debug, Clone, serde::Deserialize, serde::Serialize)]
pub struct StorageStorageInserterStore<RecipeIdxType: WeakIdxTrait> {
    inserters: Box<[Vec<StorageStorageInserter<RecipeIdxType>>]>,
    holes: Box<[Vec<usize>]>,
}

impl<RecipeIdxType: IdxTrait> StorageStorageInserterStore<RecipeIdxType> {
    fn new<ItemIdxType: IdxTrait>(data_store: &DataStore<ItemIdxType, RecipeIdxType>) -> Self {
        Self {
            inserters: vec![vec![]; data_store.item_names.len()].into_boxed_slice(),
            holes: vec![vec![]; data_store.item_names.len()].into_boxed_slice(),
        }
    }

    fn update<'a, 'b, ItemIdxType: IdxTrait>(
        &mut self,
        full_storages: impl IndexedParallelIterator<Item = SingleItemStorages<'a, 'b>>,
        num_grids_total: usize,
        data_store: &DataStore<ItemIdxType, RecipeIdxType>,
    ) where
        'b: 'a,
    {
        self.inserters
            .par_iter_mut()
            .zip(self.holes.par_iter_mut())
            .zip(full_storages)
            .enumerate()
            .for_each(|(item_id, ((ins, holes), storages))| {
                let item = Item {
                    id: item_id.try_into().unwrap(),
                };

                let grid_size = grid_size(item, data_store);
                let num_recipes = num_recipes(item, data_store);

                ins.iter_mut()
                    .enumerate()
                    // FIXME: This is awful!
                    // Ideally we could replace inserter holes with placeholder that do not do anything, but I don't quite know how those would work.
                    .filter_map(|(i, v)| (!holes.contains(&i)).then_some(v))
                    .for_each(|ins| {
                        ins.update(storages, MOVETIME, num_grids_total, num_recipes, grid_size)
                    });
            });
    }

    pub fn add_ins<ItemIdxType: IdxTrait>(
        &mut self,
        item: Item<ItemIdxType>,
        start: Storage<RecipeIdxType>,
        dest: Storage<RecipeIdxType>,
    ) -> usize {
        let idx = if let Some(hole_idx) = self.holes[usize_from(item.id)].pop() {
            self.inserters[usize_from(item.id)][hole_idx] =
                StorageStorageInserter::new(start, dest);

            hole_idx
        } else {
            self.inserters[usize_from(item.id)].push(StorageStorageInserter::new(start, dest));

            self.inserters[usize_from(item.id)].len() - 1
        };

        idx
    }

    pub fn remove_ins<ItemIdxType: IdxTrait>(&mut self, item: Item<ItemIdxType>, index: usize) {
        self.holes[usize_from(item.id)].push(index);
    }
}

#[derive(Debug, Clone, serde::Deserialize, serde::Serialize)]
pub struct BeltBeltInserterStore<ItemIdxType: WeakIdxTrait> {
    // TODO: Holes
    pub inserters: Box<[Vec<(BeltBeltInserter, BeltBeltInserterInfo<ItemIdxType>)>]>,
}

impl<ItemIdxType: IdxTrait, RecipeIdxType: IdxTrait> Factory<ItemIdxType, RecipeIdxType> {
    fn new(data_store: &DataStore<ItemIdxType, RecipeIdxType>) -> Self {
        Self {
            power_grids: PowerGridStorage::new(),
            belts: BeltStore::new(data_store),
            storage_storage_inserters: StorageStorageInserterStore::new(data_store),
            chests: FullChestStore {
                stores: (0..data_store.item_names.len())
                    .map(|id| Item {
                        id: id.try_into().unwrap(),
                    })
                    .map(|item| MultiChestStore::new(item, data_store))
                    .collect(),
            },
        }
    }

    fn belt_update<'a>(&mut self, data_store: &DataStore<ItemIdxType, RecipeIdxType>) {
        let num_grids_total = self.power_grids.power_grids.len();
        let mut all_storages =
            storages_by_item(&mut self.power_grids, &mut self.chests, data_store);
        let sizes: Vec<_> = sizes(data_store, num_grids_total).into_iter().collect();
        // dbg!(&all_storages);
        assert_eq!(sizes.len(), data_store.item_names.len());
        let storages_by_item = full_to_by_item(&mut all_storages, &sizes);

        let mut storages_by_item: Box<[_]> = storages_by_item.into_iter().collect();

        self.storage_storage_inserters.update(
            storages_by_item.par_iter_mut().map(|v| &mut **v),
            num_grids_total,
            data_store,
        );

        self.belts.update(
            num_grids_total,
            storages_by_item.par_iter_mut().map(|v| &mut **v),
            data_store,
        );
    }
}

pub enum AppState {
    Ingame,
    Loading,
}

#[derive(Debug, Clone, Copy)]
enum InserterUpdateInfo {
    NewAssembler { pos: Position, size: (u8, u8) },
    NewBelt { pos: Position },
}

#[derive(Debug)]
enum InstantiateInserterError {
    NotUnattachedInserter,
    SourceMissing,
    DestMissing,
    PleaseSpecifyFilter,
    ItemConflict,
}

impl<ItemIdxType: IdxTrait, RecipeIdxType: IdxTrait> GameState<ItemIdxType, RecipeIdxType> {
    #[allow(clippy::too_many_lines)]
    pub fn apply_actions(
        &mut self,
        actions: impl IntoIterator<Item = impl Borrow<ActionType<ItemIdxType, RecipeIdxType>>>,
        data_store: &DataStore<ItemIdxType, RecipeIdxType>,
    ) {
        // let num_assemblers = self
        //     .world
        //     .get_chunks()
        //     .into_iter()
        //     .map(|c| {
        //         c.get_entities()
        //             .into_iter()
        //             .filter(|e| matches!(e, Entity::Assembler { .. }))
        //     })
        //     .count();
        // panic!("{}", num_assemblers);

        for action in actions {
            // FIXME: I just clone for now
            match action.borrow().clone() {
                ActionType::PlaceFloorTile(place_floor_tile_by_hand_info) => {
                    let num_items_needed = match place_floor_tile_by_hand_info.ghost_info.position {
                        PositionInfo::Rect { pos, width, height } => width * height,
                        PositionInfo::Single { pos } => 1,
                        PositionInfo::List { ref positions } => positions.len(),
                    };

                    // TODO: Check player inventory for enough resources

                    match place_floor_tile_by_hand_info.ghost_info.position {
                        PositionInfo::Rect { pos, width, height } => {
                            for x in pos.x..(pos.x + width) {
                                for y in pos.y..(pos.y + height) {
                                    self.world.set_floor_tile(
                                        Position { x, y },
                                        place_floor_tile_by_hand_info.ghost_info.tile,
                                    );
                                }
                            }
                        },
                        PositionInfo::Single { pos } => {
                            self.world
                                .set_floor_tile(pos, place_floor_tile_by_hand_info.ghost_info.tile);
                        },
                        PositionInfo::List { positions } => {
                            for pos in positions {
                                self.world.set_floor_tile(
                                    pos,
                                    place_floor_tile_by_hand_info.ghost_info.tile,
                                );
                            }
                        },
                    }
                },
                ActionType::PlaceEntity(place_entity_info) => match place_entity_info.entities {
                    crate::frontend::action::place_entity::EntityPlaceOptions::Single(
                        place_entity_type,
                    ) => match place_entity_type {
                        crate::frontend::world::tile::PlaceEntityType::Assembler { pos, ty } => {
                            info!("Trying to place assembler at {pos:?}");
                            // TODO: get size dynamically
                            if !self.world.can_fit(pos, (3, 3), data_store) {
                                warn!("Tried to place assembler where it does not fit");
                                continue;
                            }

                            let powered_by = self.world.is_powered_by(
                                &self.simulation_state,
                                pos,
                                (3, 3),
                                data_store,
                            );

                            let modules = vec![
                                None;
                                data_store.assembler_info[usize::from(ty)]
                                    .num_module_slots
                                    .into()
                            ]
                            .into_boxed_slice();

                            if let Some(pole_position) = powered_by {
                                self.world.add_entity(
                                    crate::frontend::world::tile::Entity::Assembler {
                                        ty,
                                        pos,
                                        info: AssemblerInfo::PoweredNoRecipe(pole_position),
                                        modules,
                                    },
                                    &self.simulation_state,
                                    data_store,
                                );
                            } else {
                                self.world.add_entity(
                                    crate::frontend::world::tile::Entity::Assembler {
                                        ty,
                                        pos,
                                        info: AssemblerInfo::UnpoweredNoRecipe,
                                        modules,
                                    },
                                    &self.simulation_state,
                                    data_store,
                                );
                            }
                        },
                        crate::frontend::world::tile::PlaceEntityType::Inserter {
                            pos,
                            dir,
                            filter,
                        } => {
                            let ret = self.try_adding_inserter(pos, dir, filter, data_store);
                            trace!("{:?}", ret);
                        },
                        crate::frontend::world::tile::PlaceEntityType::Belt { pos, direction } => {
                            if !self.world.can_fit(pos, (1, 1), data_store) {
                                warn!("Tried to place belt where it does not fit");
                                continue;
                            }

                            handle_belt_placement(self, pos, direction, data_store);

                            self.update_inserters(InserterUpdateInfo::NewBelt { pos }, data_store);
                        },
                        crate::frontend::world::tile::PlaceEntityType::PowerPole {
                            pos: pole_pos,
                            ty,
                        } => {
                            // Check if the powerpole fits
                            if !self.world.can_fit(
                                pole_pos,
                                data_store.power_pole_data[usize::from(ty)].size,
                                data_store,
                            ) {
                                warn!("Tried to place power pole where it does not fit");
                                continue;
                            }

                            // Check which poles are in range to connect to
                            let connection_candidates: Vec<_> = self
                                .world
                                .get_power_poles_which_could_connect_to_pole_at(
                                    pole_pos,
                                    data_store.power_pole_data[usize::from(ty)].size,
                                    data_store.power_pole_data[usize::from(ty)].connection_range,
                                    data_store,
                                )
                                .into_iter()
                                .map(|e| e.get_pos())
                                .collect();

                            if let Some((pole_updates, storage_updates)) =
                                self.simulation_state.factory.power_grids.add_pole(
                                    pole_pos,
                                    connection_candidates.iter().copied(),
                                    data_store,
                                )
                            {
                                // Handle Entities that are now part of another power_grid
                                for pole_position in pole_updates {
                                    let grid = self
                                        .simulation_state
                                        .factory
                                        .power_grids
                                        .pole_pos_to_grid_id[&pole_position];

                                    assert!(
                                        !self.simulation_state.factory.power_grids.power_grids
                                            [grid as usize]
                                            .is_placeholder
                                    );

                                    self.world
                                        .update_pole_power(pole_position, grid, data_store);
                                }

                                // Handle storage updates
                                for storage_update in storage_updates {
                                    self.world.mutate_entities_colliding_with(storage_update.position, (1,1), data_store, |e| {
                                                    match (e, storage_update.new_storage) {
                                                        (Entity::Assembler { ty: _, pos: _, info: AssemblerInfo::Powered { id, pole_position: _ }, modules: _ }, crate::power::power_grid::PowerGridEntity::Assembler { recipe, index }) => {
                                                            assert_eq!(id.recipe, recipe);
                                                            id.grid = storage_update.new_grid;
                                                            id.assembler_index = index;
                                                            // FIXME: Store and update the weak_index
                                                        },
                                                        (Entity::Lab { pos: _, ty: _, pole_position: Some((_pole_pos, weak_idx, lab_store_index)) }, crate::power::power_grid::PowerGridEntity::Lab { ty: _, index: new_idx  }) => {
                                                            *lab_store_index = new_idx;
                                                            // The weak index stays the same since it it still connected to the same power pole
                                                        }

                                                        (_, _) => todo!("Handler storage_update {storage_update:?}")
                                                    }
                                                    ControlFlow::Break(())
                                                });
                                }
                            } else {
                                // No updates needed
                            }

                            let grid = self
                                .simulation_state
                                .factory
                                .power_grids
                                .pole_pos_to_grid_id[&pole_pos];

                            // Handle Entities that are newly powered
                            let power_range = data_store.power_pole_data[ty as usize].power_range;
                            self.world.mutate_entities_colliding_with(
                                Position {
                                    x: pole_pos.x - power_range as usize,
                                    y: pole_pos.y - power_range as usize,
                                },
                                ((2 * power_range + 1).into(), (2 * power_range + 1).into()),
                                data_store,
                                |e| {
                                    match e {
                                        Entity::Assembler {
                                            ty,
                                            pos,
                                            info,
                                            modules,
                                        } => match info {
                                            AssemblerInfo::UnpoweredNoRecipe => {
                                                *info = AssemblerInfo::PoweredNoRecipe(pole_pos);
                                            },
                                            AssemblerInfo::Unpowered(recipe) => {
                                                let assembler_id = self
                                                    .simulation_state
                                                    .factory
                                                    .power_grids
                                                    .power_grids
                                                    [grid as usize]
                                                    .add_assembler(
                                                        *ty, grid, *recipe, &modules, pole_pos,
                                                        *pos, data_store,
                                                    );
                                                *info = AssemblerInfo::Powered {
                                                    id: assembler_id,
                                                    pole_position: pole_pos,
                                                };
                                            },
                                            _ => {},
                                        },
                                        Entity::Roboport {
                                            ty,
                                            pos,
                                            power_grid: power_grid @ None,
                                            network,
                                            id,
                                        } => {
                                            *power_grid = Some(grid);
                                            todo!("Add Roboport to power grid")
                                        },
                                        Entity::SolarPanel {
                                            pos,
                                            ty,
                                            pole_position: pole_position @ None,
                                        } => {
                                            let idx = self
                                                .simulation_state
                                                .factory
                                                .power_grids
                                                .power_grids[usize::from(grid)]
                                            .add_solar_panel(*pos, *ty, pole_pos, data_store);

                                            *pole_position = Some((pole_pos, idx));
                                        },
                                        Entity::Lab {
                                            pos,
                                            ty,
                                            pole_position: pole_position @ None,
                                        } => {
                                            let idx = self
                                                .simulation_state
                                                .factory
                                                .power_grids
                                                .power_grids[usize::from(grid)]
                                            .add_lab(*pos, *ty, pole_pos, data_store);

                                            *pole_position = Some((pole_pos, idx.0, idx.1));
                                        },
                                        _ => {},
                                    }
                                    ControlFlow::Continue(())
                                },
                            );

                            // Add the powerpole entity to the correct chunk
                            self.world.add_entity(
                                Entity::PowerPole {
                                    ty,
                                    pos: pole_pos,
                                    connected_power_poles: connection_candidates,
                                },
                                &self.simulation_state,
                                data_store,
                            );
                        },
                        crate::frontend::world::tile::PlaceEntityType::Splitter {
                            pos,
                            direction,
                            in_mode,
                            out_mode,
                        } => {
                            let splitter = handle_splitter_placement(
                                self, pos, direction, in_mode, out_mode, data_store,
                            );
                        },
                        crate::frontend::world::tile::PlaceEntityType::Chest { pos } => {
                            info!("Trying to place chest at {pos:?}");
                            // TODO: get size dynamically
                            if !self.world.can_fit(pos, (1, 1), data_store) {
                                warn!("Tried to place chest where it does not fit");
                                continue;
                            }

                            // FIXME: Chest item hardcoded
                            let item = Item {
                                id: ItemIdxType::from(0),
                            };

                            let index = self.simulation_state.factory.chests.stores
                                [usize_from(item.id)]
                            .add_chest(0, data_store);

                            self.world.add_entity(
                                // FIXME: Chest type hardcoded
                                Entity::Chest {
                                    ty: 0,
                                    pos,
                                    item: Some(item),
                                    index,
                                },
                                &self.simulation_state,
                                data_store,
                            );
                        },
                        crate::frontend::world::tile::PlaceEntityType::SolarPanel { pos, ty } => {
                            info!("Trying to place solar_panel at {pos:?}");
                            // TODO: get size dynamically
                            if !self.world.can_fit(pos, (3, 3), data_store) {
                                warn!("Tried to place solar_panel where it does not fit");
                                continue;
                            }

                            // FIXME: Hardcoded
                            let powered_by = self.world.is_powered_by(
                                &self.simulation_state,
                                pos,
                                (3, 3),
                                data_store,
                            );

                            let powered_by = if let Some(pole_pos) = powered_by {
                                let grid = self
                                    .simulation_state
                                    .factory
                                    .power_grids
                                    .pole_pos_to_grid_id[&pole_pos];

                                let grid =
                                    &mut self.simulation_state.factory.power_grids.power_grids
                                        [usize::from(grid)];

                                let weak_idx = grid.add_solar_panel(pos, ty, pole_pos, data_store);

                                Some((pole_pos, weak_idx))
                            } else {
                                None
                            };

                            self.world.add_entity(
                                Entity::SolarPanel {
                                    pos,
                                    ty,
                                    pole_position: powered_by,
                                },
                                &self.simulation_state,
                                data_store,
                            );
                        },
                        crate::frontend::world::tile::PlaceEntityType::Lab { pos, ty } => {
                            info!("Trying to place lab at {pos:?}");
                            // TODO: get size dynamically
                            if !self.world.can_fit(pos, (3, 3), data_store) {
                                warn!("Tried to place lab where it does not fit");
                                continue;
                            }

                            // FIXME: Hardcoded
                            let powered_by = self.world.is_powered_by(
                                &self.simulation_state,
                                pos,
                                (3, 3),
                                data_store,
                            );

                            let powered_by = if let Some(pole_pos) = powered_by {
                                let grid = self
                                    .simulation_state
                                    .factory
                                    .power_grids
                                    .pole_pos_to_grid_id[&pole_pos];

                                let grid =
                                    &mut self.simulation_state.factory.power_grids.power_grids
                                        [usize::from(grid)];

                                let weak_idx = grid.add_lab(pos, ty, pole_pos, data_store);

                                Some((pole_pos, weak_idx.0, weak_idx.1))
                            } else {
                                None
                            };

                            self.world.add_entity(
                                Entity::Lab {
                                    pos,
                                    ty,
                                    pole_position: powered_by,
                                },
                                &self.simulation_state,
                                data_store,
                            );
                        },
                    },
                    crate::frontend::action::place_entity::EntityPlaceOptions::Multiple(vec) => {
                        todo!()
                    },
                },
                ActionType::Position(id, pos) => {
                    self.world.players[usize::from(id)].visible = true;
                    self.world.players[usize::from(id)].pos = pos;
                },
                ActionType::Ping(Position { x, y }) => {
                    // Do nothing for now
                    info!("Ping at {:?}", (x, y));
                    // TODO:
                },
                ActionType::SetRecipe(SetRecipeInfo { pos, recipe }) => {
                    let mut needs_update = false;
                    let chunk = self.world.get_chunk_for_tile_mut(pos);

                    if let Some(chunk) = chunk {
                        let assembler = chunk.get_entity_at_mut(pos, data_store);

                        if let Some(mut entity) = assembler {
                            match &mut entity {
                                Entity::Assembler {
                                    ty,
                                    pos,
                                    ref mut info,
                                    modules,
                                } => match info {
                                    AssemblerInfo::UnpoweredNoRecipe => {
                                        *info = AssemblerInfo::Unpowered(recipe)
                                    },
                                    AssemblerInfo::Unpowered(_old_recipe) => {
                                        *info = AssemblerInfo::Unpowered(recipe)
                                    },
                                    AssemblerInfo::PoweredNoRecipe(pole_position) => {
                                        let grid_id = self
                                            .simulation_state
                                            .factory
                                            .power_grids
                                            .pole_pos_to_grid_id[&pole_position];

                                        let new_id = Self::add_assembler_to_sim(
                                            &mut self.simulation_state,
                                            *ty,
                                            recipe,
                                            &modules,
                                            grid_id,
                                            *pole_position,
                                            *pos,
                                            data_store,
                                        );

                                        *info = AssemblerInfo::Powered {
                                            id: new_id,
                                            pole_position: *pole_position,
                                        };

                                        needs_update = true;
                                    },
                                    AssemblerInfo::Powered {
                                        id: assembler_id,
                                        pole_position,
                                    } => {
                                        let old_recipe_id = assembler_id.recipe;

                                        if old_recipe_id == recipe {
                                            continue;
                                        }

                                        let old_assembler =
                                            self.simulation_state.factory.power_grids.power_grids
                                                [assembler_id.grid as usize]
                                                .remove_assembler(*assembler_id, data_store);

                                        let new_id = Self::add_assembler_to_sim(
                                            &mut self.simulation_state,
                                            *ty,
                                            recipe,
                                            &modules,
                                            assembler_id.grid,
                                            *pole_position,
                                            *pos,
                                            data_store,
                                        );

                                        *assembler_id = new_id;

                                        needs_update = true;
                                    },
                                },
                                _ => {
                                    warn!("Tried to change assembler recipe on non assembler!");
                                },
                            }
                        } else {
                            warn!("Tried to change assembler recipe where there was no entity!");
                        }
                    } else {
                        warn!("Tried to change assembler recipe outside world!");
                    }

                    if needs_update {
                        // FIXME: Size hardcoded!
                        self.update_inserters(
                            InserterUpdateInfo::NewAssembler { pos, size: (3, 3) },
                            data_store,
                        );
                    }
                },
                ActionType::Remove(pos) => {
                    self.world
                        .remove_entity_at(pos, &mut self.simulation_state, data_store);
                },
                ActionType::AddModules {
                    pos,
                    modules: new_modules,
                } => {
                    self.world
                        .mutate_entities_colliding_with(pos, (1, 1), data_store, |e| {
                            match e {
                                Entity::Assembler { modules, info, .. } => {
                                    let num_free_module_slots =
                                        modules.iter().filter(|slot| slot.is_none()).count();

                                    if new_modules.len() > num_free_module_slots {
                                        // Not enough space in the module slots
                                        warn!("Tried to insert modules into non assembler");
                                    } else {
                                        // We are okay!

                                        modules
                                            .iter_mut()
                                            .filter(|slot| slot.is_none())
                                            .zip(new_modules.iter().copied())
                                            .for_each(|(slot, new_module)| {
                                                assert!(slot.is_none());
                                                *slot = Some(new_module);
                                            });

                                        match info {
                                            AssemblerInfo::UnpoweredNoRecipe
                                            | AssemblerInfo::Unpowered(_)
                                            | AssemblerInfo::PoweredNoRecipe(_) => {},
                                            AssemblerInfo::Powered { id, pole_position } => {
                                                for module in &new_modules {
                                                    self.simulation_state
                                                        .factory
                                                        .power_grids
                                                        .power_grids[usize::from(id.grid)]
                                                    .add_module_to_assembler(
                                                        *id, *module, data_store,
                                                    );
                                                }
                                            },
                                        }
                                    }
                                },
                                _ => {
                                    warn!("Tried to insert modules into non assembler");
                                },
                            }
                            ControlFlow::Break(())
                        });
                },
                ActionType::RemoveModules { pos, indices } => todo!(),
            }
        }
    }

    pub fn update(&mut self, data_store: &DataStore<ItemIdxType, RecipeIdxType>) {
        self.simulation_state.factory.chests.update();

        self.simulation_state.factory.belt_update(data_store);

        // TODO: Do I want this, or just do it in the belt_update
        //self.simulation_state
        //    .factory
        //    .belt_belt_inserters
        //    .update(&mut self.simulation_state.factory.belts, data_store);

        #[cfg(debug_assertions)]
        {
            let num_placeholders = self
                .simulation_state
                .factory
                .power_grids
                .power_grids
                .iter()
                .filter(|grid| grid.is_placeholder)
                .count();
            dbg!(
                num_placeholders,
                self.simulation_state.factory.power_grids.power_grids.len()
            );
        }

        let (tech_progress, recipe_tick_info): (ResearchProgress, RecipeTickInfo) = self
            .simulation_state
            .factory
            .power_grids
            .power_grids
            .par_iter_mut()
            .map(|grid| grid.update(Watt(600000), &self.simulation_state.tech_state, data_store))
            .reduce(
                || (0, RecipeTickInfo::new(data_store)),
                |(acc_progress, infos), (rhs_progress, info)| {
                    (acc_progress + rhs_progress, infos + &info)
                },
            );

        self.simulation_state
            .tech_state
            .apply_progress(tech_progress);

        self.statistics.append_single_set_of_samples((
            ProductionInfo::from_recipe_info(&recipe_tick_info, data_store),
            tech_progress,
        ));

        // if self.statistics.production.num_samples_pushed % 60 == 0 {
        //     File::create("./stats.svg").unwrap().write(self.statistics.get_chart(1, data_store, Some(|_| true)).svg().unwrap().as_bytes()).unwrap();
        // }

        self.current_tick += 1;
    }

    fn update_inserters(
        &mut self,
        info: InserterUpdateInfo,
        data_store: &DataStore<ItemIdxType, RecipeIdxType>,
    ) {
        let inserter_range = data_store.max_inserter_search_range;

        let mut inserter_positions = vec![];

        match info {
            InserterUpdateInfo::NewAssembler {
                pos: assembler_pos,
                size,
            } => {
                self.world.mutate_entities_colliding_with(
                    Position {
                        x: assembler_pos.x - usize::from(inserter_range),
                        y: assembler_pos.y - usize::from(inserter_range),
                    },
                    (
                        (2 * inserter_range + size.0).into(),
                        (2 * inserter_range + size.1).into(),
                    ),
                    data_store,
                    |e| {
                        match e {
                            Entity::Inserter {
                                pos,
                                direction,
                                info,
                            } => match info {
                                InserterInfo::NotAttached { start_pos, end_pos } => {
                                    if start_pos
                                        .contained_in(assembler_pos, (size.0.into(), size.1.into()))
                                        || end_pos.contained_in(
                                            assembler_pos,
                                            (size.0.into(), size.1.into()),
                                        )
                                    {
                                        inserter_positions.push(*pos);
                                    }
                                },
                                InserterInfo::Attached(..) => {},
                            },
                            _ => {},
                        }
                        ControlFlow::Continue(())
                    },
                );
            },
            InserterUpdateInfo::NewBelt { pos: belt_pos } => {
                self.world.mutate_entities_colliding_with(
                    Position {
                        x: belt_pos.x - usize::from(inserter_range),
                        y: belt_pos.y - usize::from(inserter_range),
                    },
                    (
                        (2 * inserter_range + 1).into(),
                        (2 * inserter_range + 1).into(),
                    ),
                    data_store,
                    |e| {
                        match e {
                            Entity::Inserter {
                                pos,
                                direction,
                                info,
                            } => match info {
                                InserterInfo::NotAttached { start_pos, end_pos } => {
                                    if start_pos.contained_in(belt_pos, (1, 1))
                                        || end_pos.contained_in(belt_pos, (1, 1))
                                    {
                                        inserter_positions.push(*pos);
                                    }
                                },
                                InserterInfo::Attached(..) => {},
                            },
                            _ => {},
                        }
                        ControlFlow::Continue(())
                    },
                );
            },
        }

        for pos in inserter_positions {
            self.try_instantiate_inserter(pos, None, data_store);
        }
    }

    fn add_assembler_to_sim(
        sim_state: &mut SimulationState<ItemIdxType, RecipeIdxType>,
        ty: u8,
        recipe: Recipe<RecipeIdxType>,
        modules: &[Option<usize>],
        power_grid_id: PowerGridIdentifier,
        pole_position: Position,
        assembler_position: Position,
        data_store: &DataStore<ItemIdxType, RecipeIdxType>,
    ) -> AssemblerID<RecipeIdxType> {
        sim_state.factory.power_grids.power_grids[power_grid_id as usize].add_assembler(
            ty,
            power_grid_id,
            recipe,
            modules,
            pole_position,
            assembler_position,
            data_store,
        )
    }

    fn try_instantiate_inserter(
        &mut self,
        pos: Position,
        filter: Option<Item<ItemIdxType>>,
        data_store: &DataStore<ItemIdxType, RecipeIdxType>,
    ) -> Result<(), InstantiateInserterError> {
        enum InserterConnection<ItemIdxType: IdxTrait, RecipeIdxType: IdxTrait> {
            Belt(BeltTileId<ItemIdxType>, u16),
            Storage(Storage<RecipeIdxType>),
        }

        let Some(Entity::Inserter {
            pos: _pos,
            direction,
            info: InserterInfo::NotAttached { start_pos, end_pos },
        }) = self
            .world
            .get_entities_colliding_with(pos, (1, 1), data_store)
            .into_iter()
            .next()
        else {
            return Err(InstantiateInserterError::NotUnattachedInserter);
        };

        let start_conn: Option<(
            InserterConnection<ItemIdxType, RecipeIdxType>,
            Option<Vec<Item<ItemIdxType>>>,
        )> = self
            .world
            .get_entities_colliding_with(*start_pos, (1, 1), data_store)
            .into_iter()
            .next()
            .map(|e| match e {
                Entity::Inserter { .. } | Entity::PowerPole { .. }| Entity::SolarPanel { .. }  => None,

                Entity::Roboport { ty, pos, power_grid, network, id } => {
                    // TODO:
                    warn!("It is currently not possible to add or remove bots from roboports using inserters");
                    None
                }

                Entity::Assembler {
                    ty,
                    pos,
                    info: AssemblerInfo::Powered {
                        id,
                        pole_position
                    },
                    modules
                    // FIXME: Translate the recipe_idx to
                } => Some((
                    InserterConnection::Storage(Storage::Assembler {
                        grid: id.grid,
                        recipe_idx_with_this_item: id.recipe.id,
                        index: id.assembler_index,
                    }),
                    Some(
                        data_store.recipe_to_items[&id.recipe]
                            .iter()
                            .filter_map(|(dir, item)| (*dir == ItemRecipeDir::Out).then_some(*item))
                            .collect(),
                    ),
                )),
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
                } => Some((
                    InserterConnection::Belt(BeltTileId::AnyBelt(*id, PhantomData), *belt_pos),
                    match self
                        .simulation_state
                        .factory
                        .belts
                        .get_pure_item(BeltTileId::AnyBelt(*id, PhantomData))
                    {
                        Some(item) => Some(vec![item]),
                        None => None,
                    },
                )),
                Entity::Splitter { pos, direction, id } => todo!("Inserters on splitters"),
                Entity::Chest {
                    ty,
                    pos,
                    item,
                    index,
                } => Some((
                    InserterConnection::Storage(Storage::Static {
                        static_id: StaticID::Chest,
                        index: *index,
                    }),
                    Some(item.into_iter().copied().collect()),
                )),
                Entity::Lab { pos, ty, pole_position } => {
                    // No removing items from Labs!
                    None
                }
            })
            .flatten();

        let Some(start_conn) = start_conn else {
            return Err(InstantiateInserterError::SourceMissing);
        };

        let dest_conn: Option<(
            InserterConnection<ItemIdxType, RecipeIdxType>,
            Option<Vec<Item<ItemIdxType>>>,
        )> = self
            .world
            .get_entities_colliding_with(*end_pos, (1, 1), data_store)
            .into_iter()
            .next()
            .map(|e| match e {
                Entity::Inserter { .. } | Entity::PowerPole { .. }| Entity::SolarPanel { .. }  => None,

                Entity::Roboport { ty, pos, power_grid, network, id } => {
                    // TODO:
                    warn!("It is currently not possible to add or remove bots from roboports using inserters");
                    None
                }

                Entity::Assembler {
                    ty,
                    pos,
                    info: AssemblerInfo::Powered {
                        id,
                        pole_position,
                    },
                    modules
                    // FIXME: Translate the recipe_idx to
                } => Some((
                    InserterConnection::Storage(Storage::Assembler {
                        grid: id.grid,
                        recipe_idx_with_this_item: id.recipe.id,
                        index: id.assembler_index,
                    }),
                    Some(
                        data_store.recipe_to_items[&id.recipe]
                            .iter()
                            .filter_map(|(dir, item)| (*dir == ItemRecipeDir::Ing).then_some(*item))
                            .collect(),
                    ),
                )),
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
                } => Some((
                    InserterConnection::Belt(BeltTileId::AnyBelt(*id, PhantomData), *belt_pos),
                    match self
                        .simulation_state
                        .factory
                        .belts
                        .get_pure_item(BeltTileId::AnyBelt(*id, PhantomData))
                    {
                        Some(item) => Some(vec![item]),
                        None => None,
                    },
                )),
                Entity::Splitter { pos, direction, id } => todo!(),
                Entity::Chest {
                    ty,
                    pos,
                    item,
                    index,
                } => Some((
                    InserterConnection::Storage(Storage::Static {
                        static_id: StaticID::Chest,
                        index: *index,
                    }),
                    Some(item.into_iter().copied().collect()),
                )),
                Entity::Lab { pos, ty, pole_position } => {
                    if let Some((pole_pos, idx, lab_store_index)) = pole_position {
                        Some((InserterConnection::Storage(Storage::Lab { grid: self.simulation_state.factory.power_grids.pole_pos_to_grid_id[pole_pos], index: *lab_store_index }), Some(data_store.science_bottle_items.iter().copied().collect())))
                    } else {
                        None
                    }
                }
            })
            .flatten();

        let Some(dest_conn) = dest_conn else {
            return Err(InstantiateInserterError::DestMissing);
        };

        // For determining the filter we use this plan:
        // If a filter is specified, use that
        // If we can determine a single source item use that,
        // If we can determine a single destination item use that
        // Else make the user do it
        let filter = filter.unwrap_or(
            if let Some(filter) = (match start_conn.1.into_iter().flatten().all_equal_value() {
                Ok(filter) => Some(filter),
                Err(None) => None,
                Err(Some(wrong)) => {
                    return Err(InstantiateInserterError::PleaseSpecifyFilter);
                },
            })
            .or_else(
                || match dest_conn.1.into_iter().flatten().all_equal_value() {
                    Ok(filter) => Some(filter),
                    Err(None) => None,
                    Err(Some(wrong)) => None,
                },
            ) {
                filter
            } else {
                return Err(InstantiateInserterError::PleaseSpecifyFilter);
            },
        );

        let Entity::Inserter { info, .. } = self
            .world
            .get_chunk_for_tile_mut(pos)
            .unwrap()
            .get_entity_at_mut(pos, data_store)
            .unwrap()
        else {
            unreachable!("We already checked it was an unattached inserter before")
        };

        match (start_conn.0, dest_conn.0) {
            (
                InserterConnection::Belt(start_belt_id, start_belt_pos),
                InserterConnection::Belt(dest_belt_id, dest_belt_pos),
            ) => {
                // FIXME: The movetime should be dependent on the inserter type!
                let index = self.simulation_state.factory.belts.add_belt_belt_inserter(
                    (start_belt_id, start_belt_pos),
                    (dest_belt_id, dest_belt_pos),
                    BeltBeltInserterAdditionInfo {
                        cooldown: MOVETIME,
                        filter,
                    },
                );
                *info = InserterInfo::Attached(AttachedInserter::BeltBelt {
                    item: filter,
                    inserter: index,
                })
            },
            (
                InserterConnection::Belt(start_belt_id, start_belt_pos),
                InserterConnection::Storage(dest_storage_untranslated),
            ) => {
                let dest_storage = dest_storage_untranslated.translate(filter, data_store);

                match self
                    .simulation_state
                    .factory
                    .belts
                    .add_belt_storage_inserter(
                        filter,
                        start_belt_id,
                        start_belt_pos - 1,
                        dest_storage,
                    ) {
                    Ok(()) => {},
                    Err(_) => todo!(),
                };

                *info = InserterInfo::Attached(AttachedInserter::BeltStorage {
                    id: start_belt_id,
                    belt_pos: start_belt_pos - 1,
                });
            },
            (
                InserterConnection::Storage(start_storage_untranslated),
                InserterConnection::Belt(dest_belt_id, dest_belt_pos),
            ) => {
                let start_storage = start_storage_untranslated.translate(filter, data_store);

                match self
                    .simulation_state
                    .factory
                    .belts
                    .add_storage_belt_inserter(
                        filter,
                        dest_belt_id,
                        dest_belt_pos - 1,
                        start_storage,
                    ) {
                    Ok(()) => {},
                    Err(_) => todo!(),
                };
                *info = InserterInfo::Attached(AttachedInserter::BeltStorage {
                    id: dest_belt_id,
                    belt_pos: dest_belt_pos - 1,
                });
            },
            (
                InserterConnection::Storage(start_storage_untranslated),
                InserterConnection::Storage(dest_storage_untranslated),
            ) => {
                let start_storage = start_storage_untranslated.translate(filter, data_store);
                let dest_storage = dest_storage_untranslated.translate(filter, data_store);

                let index = self
                    .simulation_state
                    .factory
                    .storage_storage_inserters
                    .add_ins(filter, start_storage, dest_storage);
                *info = InserterInfo::Attached(AttachedInserter::StorageStorage {
                    item: filter,
                    inserter: index,
                });
            },
        }

        Ok(())
    }

    fn try_adding_inserter(
        &mut self,
        pos: Position,
        dir: Dir,
        filter: Option<Item<ItemIdxType>>,
        data_store: &DataStore<ItemIdxType, RecipeIdxType>,
    ) -> Result<(), InstantiateInserterError> {
        match self.add_unattached_inserter(pos, dir, data_store) {
            Ok(_) => {},
            Err(_) => return Err(InstantiateInserterError::NotUnattachedInserter),
        };

        self.try_instantiate_inserter(pos, filter, data_store)
    }

    fn add_unattached_inserter(
        &mut self,
        pos: Position,
        dir: Dir,
        data_store: &DataStore<ItemIdxType, RecipeIdxType>,
    ) -> Result<(), ()> {
        if !self.world.can_fit(pos, (1, 1), data_store) {
            warn!("Tried to place inserter where it does not fit");
            return Err(());
        }

        let (start_pos, end_pos) = calculate_inserter_positions(pos, dir);

        self.world.add_entity(
            Entity::Inserter {
                pos,
                direction: dir,
                info: InserterInfo::NotAttached { start_pos, end_pos },
            },
            &self.simulation_state,
            data_store,
        );

        Ok(())
    }
}

// TODO: Different types of inserters
pub fn calculate_inserter_positions(pos: Position, dir: Dir) -> (Position, Position) {
    let start_pos = Position {
        x: pos
            .x
            .checked_add_signed(dir.reverse().into_offset().0.into())
            .unwrap(),
        y: pos
            .y
            .checked_add_signed(dir.reverse().into_offset().1.into())
            .unwrap(),
    };
    let end_pos = Position {
        x: pos
            .x
            .checked_add_signed(dir.into_offset().0.into())
            .unwrap(),
        y: pos
            .y
            .checked_add_signed(dir.into_offset().1.into())
            .unwrap(),
    };

    (start_pos, end_pos)
}

#[cfg(test)]
mod tests {
    use std::sync::LazyLock;

    use crate::{
        blueprint::{random_blueprint_strategy, random_position, Blueprint},
        data::{get_raw_data_test, DataStore},
        frontend::{
            action::{place_entity::PlaceEntityInfo, ActionType},
            world::Position,
        },
        item::Recipe,
        rendering::app_state::GameState,
        replays::Replay,
        DATA_STORE,
    };
    use proptest::{prelude::ProptestConfig, proptest};
    use test::Bencher;

    proptest! {
        // #![proptest_config(ProptestConfig::with_cases(1_000))]
        #[test]
        fn test_random_blueprint_does_not_crash(base_pos in random_position(), blueprint in random_blueprint_strategy::<u8, u8>(0..1_000, &DATA_STORE)) {

            let mut game_state = GameState::new(&DATA_STORE);

            blueprint.apply(base_pos, &mut game_state, &DATA_STORE);

        }

        #[test]
        fn test_random_blueprint_does_not_crash_after(base_pos in random_position(), blueprint in random_blueprint_strategy::<u8, u8>(0..100, &DATA_STORE), time in 0usize..600) {

            let mut game_state = GameState::new(&DATA_STORE);

            blueprint.apply(base_pos, &mut game_state, &DATA_STORE);

            for _ in 0usize..time {
                game_state.update(&DATA_STORE)
            }
        }
    }

    #[bench]
    fn bench_single_inserter(b: &mut Bencher) {
        let mut game_state = GameState::new(&DATA_STORE);

        let mut rep = Replay::new(game_state.clone(), (*DATA_STORE).clone());

        rep.append_actions(vec![ActionType::PlaceEntity(PlaceEntityInfo {
            entities: crate::frontend::action::place_entity::EntityPlaceOptions::Single(
                crate::frontend::world::tile::PlaceEntityType::PowerPole {
                    pos: Position { x: 0, y: 5 },
                    ty: 0,
                },
            ),
        })]);

        rep.append_actions(vec![ActionType::PlaceEntity(PlaceEntityInfo {
            entities: crate::frontend::action::place_entity::EntityPlaceOptions::Single(
                crate::frontend::world::tile::PlaceEntityType::SolarPanel {
                    pos: Position { x: 0, y: 2 },
                    ty: 0,
                },
            ),
        })]);

        rep.append_actions(vec![ActionType::PlaceEntity(PlaceEntityInfo {
            entities: crate::frontend::action::place_entity::EntityPlaceOptions::Single(
                crate::frontend::world::tile::PlaceEntityType::Assembler {
                    pos: Position { x: 0, y: 6 },
                    ty: 0,
                },
            ),
        })]);

        rep.append_actions(vec![ActionType::SetRecipe(
            crate::frontend::action::set_recipe::SetRecipeInfo {
                pos: Position { x: 0, y: 6 },
                recipe: Recipe { id: 0 },
            },
        )]);

        rep.append_actions(vec![ActionType::PlaceEntity(PlaceEntityInfo {
            entities: crate::frontend::action::place_entity::EntityPlaceOptions::Single(
                crate::frontend::world::tile::PlaceEntityType::Belt {
                    pos: Position { x: 1, y: 4 },
                    direction: crate::frontend::world::tile::Dir::East,
                },
            ),
        })]);

        rep.append_actions(vec![ActionType::PlaceEntity(PlaceEntityInfo {
            entities: crate::frontend::action::place_entity::EntityPlaceOptions::Single(
                crate::frontend::world::tile::PlaceEntityType::Belt {
                    pos: Position { x: 2, y: 4 },
                    direction: crate::frontend::world::tile::Dir::East,
                },
            ),
        })]);

        rep.append_actions(vec![ActionType::PlaceEntity(PlaceEntityInfo {
            entities: crate::frontend::action::place_entity::EntityPlaceOptions::Single(
                crate::frontend::world::tile::PlaceEntityType::Inserter {
                    pos: Position { x: 1, y: 5 },
                    dir: crate::frontend::world::tile::Dir::North,
                    filter: None,
                },
            ),
        })]);

        let blueprint = Blueprint::from_replay(&rep);

        blueprint.apply(Position { x: 1600, y: 1600 }, &mut game_state, &DATA_STORE);

        dbg!(&game_state
            .world
            .get_chunk_for_tile(Position { x: 1600, y: 1600 }));

        dbg!(game_state.current_tick);

        for _ in 0..60 {
            game_state.update(&DATA_STORE);
        }

        b.iter(|| {
            game_state.update(&DATA_STORE);
        });

        dbg!(game_state.current_tick);

        // dbg!(&game_state.simulation_state.factory.belts);

        assert!(
            dbg!(
                game_state
                    .statistics
                    .production
                    .total
                    .unwrap()
                    .items_produced
            )[0] > 0
        );
    }
}
