use std::{
    collections::HashMap,
    iter::Sum,
    mem,
    ops::{Add, Div, Mul, Sub},
    u64,
};

use itertools::Itertools;
use log::{error, warn};
use power_grid::{
    BeaconAffectedEntity, IndexUpdateInfo, MIN_BEACON_POWER_MULT, PowerGrid, PowerGridEntity,
    PowerGridIdentifier,
};

use std::fmt::Display;

use crate::{
    TICKS_PER_SECOND_LOGIC,
    assembler::AssemblerOnclickInfo,
    data::DataStore,
    frontend::world::{
        Position,
        tile::{AssemblerID, MachineID},
    },
    item::{IdxTrait, WeakIdxTrait},
    network_graph::WeakIndex,
    research::{LabTickInfo, ResearchProgress, TechState},
    statistics::recipe::RecipeTickInfo,
};

use rayon::iter::ParallelIterator;
use rayon::iter::{IntoParallelRefMutIterator, ParallelBridge};

pub mod power_grid;

#[derive(
    Debug,
    Clone,
    Copy,
    PartialEq,
    Eq,
    PartialOrd,
    Ord,
    Default,
    serde::Deserialize,
    serde::Serialize,
)]
pub struct Joule(pub u64);

impl Display for Joule {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        if self.0 > 1_000_000_000 {
            write!(f, "{:.1}GJ", self.0 as f64 / 1_000_000_000.0)
        } else if self.0 > 1_000_000 {
            write!(f, "{:.1}MJ", self.0 as f64 / 1_000_000.0)
        } else if self.0 > 1_000 {
            write!(f, "{:.1}KJ", self.0 as f64 / 1_000.0)
        } else {
            write!(f, "{:.1}J", self.0 as f64)
        }
    }
}

impl Sum for Joule {
    fn sum<I: Iterator<Item = Self>>(iter: I) -> Self {
        Self(iter.map(|mj| mj.0).sum())
    }
}

impl Add<Self> for Joule {
    type Output = Self;

    fn add(self, rhs: Self) -> Self::Output {
        Self(self.0 + rhs.0)
    }
}

impl Sub<Self> for Joule {
    type Output = Self;

    fn sub(self, rhs: Self) -> Self::Output {
        Self(self.0 - rhs.0)
    }
}

impl Mul<u64> for Joule {
    type Output = Self;

    fn mul(self, rhs: u64) -> Self::Output {
        Self(self.0 * rhs)
    }
}

impl Div<Self> for Joule {
    type Output = u64;

    fn div(self, rhs: Self) -> Self::Output {
        self.0 / rhs.0
    }
}

impl Div<u64> for Joule {
    type Output = Self;

    fn div(self, rhs: u64) -> Self::Output {
        Self(self.0 / rhs)
    }
}

impl Joule {
    #[must_use]
    pub const fn watt_from_tick(self) -> Watt {
        Watt(self.0 * TICKS_PER_SECOND_LOGIC)
    }
}

#[derive(
    Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, serde::Deserialize, serde::Serialize,
)]
pub struct Watt(pub u64);

impl Add<Self> for Watt {
    type Output = Self;

    fn add(self, rhs: Self) -> Self::Output {
        Self(self.0 + rhs.0)
    }
}

impl Sub<Self> for Watt {
    type Output = Self;

    fn sub(self, rhs: Self) -> Self::Output {
        Self(self.0 - rhs.0)
    }
}

impl Mul<u64> for Watt {
    type Output = Self;

    fn mul(self, rhs: u64) -> Self::Output {
        Self(self.0 * rhs)
    }
}

impl Div<u64> for Watt {
    type Output = Self;

    fn div(self, rhs: u64) -> Self::Output {
        Self(self.0 / rhs)
    }
}

impl Watt {
    #[must_use]
    pub const fn joules_per_tick(self) -> Joule {
        Joule(self.0 / TICKS_PER_SECOND_LOGIC)
    }
}

impl Display for Watt {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        if self.0 > 1_000_000_000 {
            write!(f, "{:.1}GW", self.0 as f64 / 1_000_000_000.0)
        } else if self.0 > 1_000_000 {
            write!(f, "{:.1}MW", self.0 as f64 / 1_000_000.0)
        } else if self.0 > 1_000 {
            write!(f, "{:.1}KW", self.0 as f64 / 1_000.0)
        } else {
            write!(f, "{:.1}W", self.0 as f64)
        }
    }
}

impl Sum<Watt> for Watt {
    fn sum<I: Iterator<Item = Watt>>(iter: I) -> Self {
        iter.reduce(|acc, v| acc + v).unwrap_or(Watt(0))
    }
}

#[derive(Debug, Clone, serde::Deserialize, serde::Serialize)]
pub struct PowerGridStorage<ItemIdxType: WeakIdxTrait, RecipeIdxType: WeakIdxTrait> {
    pub power_grids: Vec<PowerGrid<ItemIdxType, RecipeIdxType>>,
    pub pole_pos_to_grid_id: HashMap<Position, PowerGridIdentifier>,
}

impl<ItemIdxType: IdxTrait, RecipeIdxType: IdxTrait> PowerGridStorage<ItemIdxType, RecipeIdxType> {
    pub fn new() -> Self {
        Self {
            power_grids: vec![],
            pole_pos_to_grid_id: HashMap::default(),
        }
    }

    pub fn get_assembler_info(
        &self,
        assembler_id: AssemblerID<RecipeIdxType>,
        data_store: &DataStore<ItemIdxType, RecipeIdxType>,
    ) -> AssemblerOnclickInfo<ItemIdxType> {
        self.power_grids[Into::<MachineID<RecipeIdxType>>::into(assembler_id).get_grid() as usize]
            .get_assembler_info(assembler_id, data_store)
    }

    fn create_power_grid(
        &mut self,
        first_pole_position: Position,
        data_store: &DataStore<ItemIdxType, RecipeIdxType>,
    ) -> PowerGridIdentifier {
        // TODO: This is O(N). Is that a problem?
        let hole_idx = self.power_grids.iter().position(|grid| grid.is_placeholder);

        let new_grid = PowerGrid::new(data_store, first_pole_position);

        let id = if let Some(hole_idx) = hole_idx {
            self.power_grids[hole_idx] = new_grid;
            hole_idx
                .try_into()
                .expect("If this is not in range, this means we had too many power grids before?")
        } else {
            let len = self.power_grids.len();
            self.power_grids.push(new_grid);
            len.try_into().expect(&format!(
                "Too many power grids, max, {} allowed",
                PowerGridIdentifier::MAX
            ))
        };

        self.pole_pos_to_grid_id.insert(first_pole_position, id);

        id
    }

    fn add_power_grid(
        &mut self,
        power_grid: PowerGrid<ItemIdxType, RecipeIdxType>,
        data_store: &DataStore<ItemIdxType, RecipeIdxType>,
    ) -> PowerGridIdentifier {
        // TODO: This is O(N). Is that a problem?
        let hole_idx = self.power_grids.iter().position(|grid| grid.is_placeholder);

        let poles: Vec<_> = power_grid.grid_graph.keys().into_iter().copied().collect();

        let id = if let Some(hole_idx) = hole_idx {
            self.power_grids[hole_idx] = power_grid;
            hole_idx
                .try_into()
                .expect("If this is not in range, this means we had too many power grids before?")
        } else {
            let len = self.power_grids.len();
            self.power_grids.push(power_grid);
            len.try_into().expect(&format!(
                "Too many power grids, max, {} allowed",
                PowerGridIdentifier::MAX
            ))
        };

        for pole_pos in poles {
            self.pole_pos_to_grid_id.insert(pole_pos, id);
        }

        id
    }

    /// Returns a list of power poles, which have their PowerGridId changed
    #[profiling::function]
    pub fn add_pole<T: IntoIterator<Item = Position>>(
        &mut self,
        pole_position: Position,
        connected_poles: T,
        data_store: &DataStore<ItemIdxType, RecipeIdxType>,
    ) -> Option<(
        impl IntoIterator<Item = Position> + use<ItemIdxType, RecipeIdxType, T>,
        impl IntoIterator<Item = IndexUpdateInfo<ItemIdxType, RecipeIdxType>>
        + use<ItemIdxType, RecipeIdxType, T>,
    )> {
        #[cfg(debug_assertions)]
        {
            let affected_grids_and_potential_match = self
                .power_grids
                .iter()
                .filter(|grid| !grid.is_placeholder)
                .all(|pg| {
                    pg.beacon_affected_entities
                        .keys()
                        .map(|e| e.get_power_grid())
                        .all(|affected_grid| {
                            pg.potential_beacon_affected_powergrids
                                .contains(&affected_grid)
                        })
                });
            assert!(affected_grids_and_potential_match);
        }
        let mut connected_poles: Vec<_> = connected_poles.into_iter().collect();

        let ret = if !connected_poles.is_empty() {
            // Find the largest grid, and choose it as the base
            let grid = connected_poles
                .iter()
                .map(|pos| self.pole_pos_to_grid_id[pos])
                .max_by_key(|grid_id| {
                    self.power_grids[usize::from(*grid_id)]
                        .grid_graph
                        .node_count()
                })
                .unwrap();

            let need_to_merge = !connected_poles
                .iter()
                .map(|pos| self.pole_pos_to_grid_id[pos])
                .all_equal();

            self.pole_pos_to_grid_id.insert(pole_position, grid);

            if need_to_merge {
                let poles_to_update: Vec<_> = {
                    profiling::scope!("Find poles_to_update");
                    connected_poles
                        .iter()
                        .copied()
                        .filter_map(|pos| {
                            (self.pole_pos_to_grid_id[&pos] != grid)
                                .then_some(self.pole_pos_to_grid_id[&pos])
                        })
                        .unique()
                        .flat_map(|merged_grid| {
                            assert_ne!(merged_grid, grid);
                            self.pole_pos_to_grid_id
                                .iter()
                                .filter_map(move |(k, v)| (*v == merged_grid).then_some(*k))
                        })
                        .collect()
                };

                let mut storage_update_vec = vec![];

                let mut ran_once = false;
                for other_grid in connected_poles
                    .iter()
                    .map(|pos| self.pole_pos_to_grid_id[pos])
                    .unique()
                    .collect::<Vec<_>>()
                {
                    ran_once = true;
                    let storage_updates = self
                        .merge_power_grids(
                            grid,
                            other_grid,
                            data_store,
                            pole_position,
                            connected_poles.iter().copied(),
                        )
                        .into_iter()
                        .flatten();

                    storage_update_vec.extend(storage_updates);
                }
                assert!(ran_once);

                #[cfg(debug_assertions)]
                {
                    for key in self.power_grids[grid as usize].grid_graph.keys() {
                        if let Some(index) = connected_poles.iter().position(|v| v == key) {
                            connected_poles.remove(index);
                        }
                    }
                    assert!(
                        connected_poles.is_empty(),
                        "Not all connected_poles ended up in final power grid!"
                    );

                    assert!(
                        storage_update_vec
                            .iter()
                            .map(|update| update.position)
                            .all_unique()
                    );
                }

                Some((poles_to_update, storage_update_vec))
            } else {
                self.power_grids[grid as usize].add_pole(pole_position, connected_poles);
                None
            }
        } else {
            // Create a new grid
            self.create_power_grid(pole_position, data_store);
            None
        };

        #[cfg(debug_assertions)]
        {
            let affected_grids_and_potential_match = self
                .power_grids
                .iter()
                .filter(|grid| !grid.is_placeholder)
                .all(|pg| {
                    pg.beacon_affected_entities
                        .keys()
                        .map(|e| e.get_power_grid())
                        .all(|affected_grid| {
                            pg.potential_beacon_affected_powergrids
                                .contains(&affected_grid)
                        })
                });
            assert!(affected_grids_and_potential_match);
        }

        ret
    }

    /// Returns a list of power poles, which have their PowerGridId changed
    #[profiling::function]
    pub fn remove_pole(
        &mut self,
        pole_position: Position,
        data_store: &DataStore<ItemIdxType, RecipeIdxType>,
    ) -> (
        impl IntoIterator<Item = Position> + use<ItemIdxType, RecipeIdxType>,
        impl IntoIterator<Item = IndexUpdateInfo<ItemIdxType, RecipeIdxType>>
        + use<ItemIdxType, RecipeIdxType>,
        impl IntoIterator<Item = Position> + use<ItemIdxType, RecipeIdxType>,
    ) {
        let old_id = self.pole_pos_to_grid_id.remove(&pole_position).unwrap();

        let (new_grids, delete_network, no_longer_connected_entity_positions, beacon_updates) =
            self.power_grids[old_id as usize].remove_pole(pole_position, data_store);

        {
            profiling::scope!("Apply Beacon updates");
            for beacon_update in beacon_updates {
                match beacon_update.0 {
                    power_grid::BeaconAffectedEntity::Assembler { id } => {
                        let grid = &mut self.power_grids[usize::from(id.grid)];

                        grid.change_assembler_module_modifiers(id, beacon_update.1, data_store);
                    },
                    power_grid::BeaconAffectedEntity::Lab { grid, index } => todo!(),
                }
            }
        }

        if delete_network {
            debug_assert!(new_grids.into_iter().count() == 0);

            self.remove_power_grid(old_id, data_store);

            return (vec![], vec![], no_longer_connected_entity_positions);
        }

        let mut pole_updates = vec![];
        let mut index_updates = vec![];

        for (new_grid, storages_to_move_into_that_grid) in new_grids {
            let new_id = self.add_power_grid(new_grid, data_store);
            for pole in self.power_grids[new_id as usize].grid_graph.keys() {
                *self.pole_pos_to_grid_id.get_mut(&pole).unwrap() = new_id;
                pole_updates.push(*pole);
            }

            index_updates.extend(storages_to_move_into_that_grid.into_iter().map(
                |(position, old_storage, new_storage)| IndexUpdateInfo {
                    position,
                    new_grid: new_id,
                    old_pg_entity: old_storage,
                    new_pg_entity: new_storage,
                },
            ));
        }

        (
            pole_updates,
            index_updates,
            no_longer_connected_entity_positions,
        )
    }

    fn remove_power_grid(
        &mut self,
        id: PowerGridIdentifier,
        data_store: &DataStore<ItemIdxType, RecipeIdxType>,
    ) {
        self.power_grids[usize::from(id)] = PowerGrid::new_placeholder(data_store);

        #[cfg(debug_assertions)]
        {
            assert!(
                self.pole_pos_to_grid_id
                    .iter()
                    .all(|idx| !self.power_grids[usize::from(*idx.1)].is_placeholder)
            )
        }
    }

    #[must_use]
    #[profiling::function]
    fn merge_power_grids(
        &mut self,
        kept_id: PowerGridIdentifier,
        removed_id: PowerGridIdentifier,
        data_store: &DataStore<ItemIdxType, RecipeIdxType>,
        new_pole_pos: Position,
        new_pole_connections: impl IntoIterator<Item = Position>,
    ) -> Option<impl IntoIterator<Item = IndexUpdateInfo<ItemIdxType, RecipeIdxType>>> {
        #[cfg(debug_assertions)]
        {
            let affected_grids_and_potential_match = self
                .power_grids
                .iter()
                .filter(|grid| !grid.is_placeholder)
                .all(|pg| {
                    pg.beacon_affected_entities
                        .keys()
                        .map(|e| e.get_power_grid())
                        .all(|affected_grid| {
                            pg.potential_beacon_affected_powergrids
                                .contains(&affected_grid)
                        })
                });
            assert!(affected_grids_and_potential_match);
        }

        if kept_id == removed_id {
            warn!("Tried to merge a grid with itself!");
            return None;
        }

        #[cfg(debug_assertions)]
        let num_placeholders = self
            .power_grids
            .iter()
            .filter(|grid| grid.is_placeholder)
            .count();

        assert!(!self.power_grids[usize::from(kept_id)].is_placeholder);
        assert!(!self.power_grids[usize::from(removed_id)].is_placeholder);

        let new_power_mult = {
            if self.power_grids[usize::from(kept_id)].last_power_consumption
                >= self.power_grids[usize::from(removed_id)].last_power_consumption
            {
                self.power_grids[usize::from(kept_id)].last_power_mult
            } else {
                self.power_grids[usize::from(removed_id)].last_power_mult
            }
        };

        let beacon_updates = match (
            new_power_mult >= MIN_BEACON_POWER_MULT,
            self.power_grids[usize::from(removed_id)].last_power_mult >= MIN_BEACON_POWER_MULT,
        ) {
            (true, true) => vec![],
            (true, false) => {
                // Enable the beacons
                self.power_grids[usize::from(removed_id)]
                    .beacon_affected_entities
                    .iter()
                    .map(|(k, v)| (*k, (v.0, v.1, 0)))
                    .collect()
            },
            (false, true) => {
                // Disable the beacons
                self.power_grids[usize::from(removed_id)]
                    .beacon_affected_entities
                    .iter()
                    .map(|(k, v)| (*k, (-v.0, -v.1, -0)))
                    .collect()
            },
            (false, false) => vec![],
        };

        {
            profiling::scope!("Apply Beacon Updates");
            for update in beacon_updates {
                match update.0 {
                    power_grid::BeaconAffectedEntity::Assembler { id } => {
                        self.power_grids[usize::from(id.grid)]
                            .change_assembler_module_modifiers(id, update.1, data_store);
                    },
                    power_grid::BeaconAffectedEntity::Lab { grid, index } => {
                        // TODO:
                        error!("Ignoring Beacon affect on lab");
                    },
                }
            }
        }

        let mut placeholder = PowerGrid::new_placeholder(data_store);

        mem::swap(
            &mut placeholder,
            &mut self.power_grids[usize::from(removed_id)],
        );

        let second = placeholder;

        for pole_pos in second.grid_graph.keys() {
            *self.pole_pos_to_grid_id.get_mut(pole_pos).unwrap() = kept_id;
        }

        // let pole_updates: Vec<PowerPoleUpdateInfo> = second
        //     .grid_graph
        //     .poles()
        //     .into_iter()
        //     .map(|pos| PowerPoleUpdateInfo {
        //         position: pos,
        //         new_grid_id: kept_id,
        //     })
        //     .collect();

        let mut placeholder = PowerGrid::new_placeholder(data_store);

        mem::swap(
            &mut placeholder,
            &mut self.power_grids[usize::from(kept_id)],
        );

        let first = placeholder;

        let mut ret = first.join(
            second,
            data_store,
            kept_id,
            removed_id,
            new_pole_pos,
            new_pole_connections,
        );
        let updates: HashMap<_, _> = ret
            .1
            .into_iter()
            .filter_map(|update| {
                let key = match update.old_pg_entity {
                    PowerGridEntity::Assembler {
                        ty: _,
                        recipe,
                        index,
                    } => Some((Some(recipe), index)),
                    PowerGridEntity::Lab { ty: _, index } => Some((None, index)),
                    _ => None,
                }?;
                Some((key, update))
            })
            .collect();

        // FIXME: We need to update all parts of the simulation which hold references across power_grid borders:
        // This includes:
        // - Inserters
        // - Beacons

        mem::swap(&mut ret.0, &mut self.power_grids[usize::from(kept_id)]);

        // Update Beacons when merging
        {
            profiling::scope!("Update Beacons when merging");

            for pg in self.power_grids.iter_mut() {
                if !pg
                    .potential_beacon_affected_powergrids
                    .contains(&removed_id)
                {
                    // This Power Grid did not affect the Power Grid which has been removed, no need to check all of its beacons
                    continue;
                }
                {
                    profiling::scope!("Update grid_graph.weak_components");
                    pg.grid_graph
                        .weak_components_mut()
                        .par_bridge()
                        .for_each(|(_, pg_entity)| {
                            if let PowerGridEntity::Beacon {
                                affected_entities, ..
                            } = pg_entity
                            {
                                for affected_entity in affected_entities {
                                    match affected_entity {
                                        BeaconAffectedEntity::Assembler { id } => {
                                            if id.grid == removed_id {
                                                let old_recipe = id.recipe;
                                                let old_index = id.assembler_index;

                                                let new_id = updates
                                                    .get(&(Some(old_recipe), old_index))
                                                    .expect("Could not find update for assembler");

                                                let PowerGridEntity::Assembler {
                                                    ty: _,
                                                    recipe: _,
                                                    index,
                                                } = new_id.new_pg_entity
                                                else {
                                                    unreachable!();
                                                };

                                                *id = AssemblerID {
                                                    recipe: id.recipe,
                                                    grid: new_id.new_grid,
                                                    assembler_index: index,
                                                };
                                            }
                                        },
                                        BeaconAffectedEntity::Lab { grid, index } => {
                                            if *grid == removed_id {
                                                let old_index = *index;

                                                let new_id = updates
                                                    .get(&(None, old_index))
                                                    .expect("Could not find update for lab");

                                                let PowerGridEntity::Lab {
                                                    ty: _,
                                                    index: new_index,
                                                } = new_id.new_pg_entity
                                                else {
                                                    unreachable!();
                                                };

                                                *grid = new_id.new_grid;
                                                *index = new_index.into();
                                            }
                                        },
                                    }
                                }
                            }
                        });
                }

                pg.potential_beacon_affected_powergrids.remove(&removed_id);

                let to_change: Vec<_> = {
                    profiling::scope!("Extract beacon_affected_entities");
                    pg.beacon_affected_entities
                        .extract_if(|affected, _| match affected {
                            BeaconAffectedEntity::Assembler { id } => id.grid == removed_id,
                            BeaconAffectedEntity::Lab { grid, index: _ } => *grid == removed_id,
                        })
                        .collect()
                };

                if !to_change.is_empty() {
                    pg.potential_beacon_affected_powergrids.insert(kept_id);
                }
                {
                    profiling::scope!("Insert beacon_affected_entities");
                    pg.beacon_affected_entities
                        .extend(to_change.into_iter().map(|(mut k, v)| {
                            match &mut k {
                                BeaconAffectedEntity::Assembler { id } => {
                                    let old_recipe = id.recipe;
                                    let old_index = id.assembler_index;

                                    let new_id = updates
                                        .get(&(Some(old_recipe), old_index))
                                        .expect("Could not find update for assembler");

                                    let PowerGridEntity::Assembler {
                                        ty: _,
                                        recipe: _,
                                        index,
                                    } = new_id.new_pg_entity
                                    else {
                                        unreachable!();
                                    };

                                    *id = AssemblerID {
                                        recipe: id.recipe,
                                        grid: new_id.new_grid,
                                        assembler_index: index,
                                    };
                                },
                                BeaconAffectedEntity::Lab { grid, index } => {
                                    let old_index = *index;

                                    let new_id = updates
                                        .get(&(None, old_index))
                                        .expect("Could not find update for lab");

                                    let PowerGridEntity::Lab {
                                        ty: _,
                                        index: new_index,
                                    } = new_id.new_pg_entity
                                    else {
                                        unreachable!();
                                    };

                                    *grid = new_id.new_grid;
                                    *index = new_index.into();
                                },
                            }

                            (k, v)
                        }));
                }
            }
        }

        #[cfg(debug_assertions)]
        {
            assert!(
                self.pole_pos_to_grid_id
                    .iter()
                    .all(|idx| !self.power_grids[usize::from(*idx.1)].is_placeholder)
            );

            assert_eq!(
                num_placeholders + 1,
                self.power_grids
                    .iter()
                    .filter(|grid| grid.is_placeholder)
                    .count()
            );

            assert!(
                self.power_grids
                    .iter()
                    .filter(|grid| !grid.is_placeholder)
                    .flat_map(|pg| { pg.beacon_affected_entities.keys() })
                    .all(|e| {
                        match e {
                            BeaconAffectedEntity::Assembler { id } => {
                                !self.power_grids[usize::from(id.grid)].is_placeholder
                            },
                            BeaconAffectedEntity::Lab { grid, index: _ } => {
                                !self.power_grids[usize::from(*grid)].is_placeholder
                            },
                        }
                    })
            );

            assert!(
                self.power_grids
                    .iter()
                    .filter(|grid| !grid.is_placeholder)
                    .flat_map(|pg| { pg.grid_graph.weak_components() })
                    .all(|(_pos, e)| {
                        match e {
                            PowerGridEntity::Beacon {
                                affected_entities, ..
                            } => affected_entities.iter().all(|e| match e {
                                BeaconAffectedEntity::Assembler { id } => {
                                    !self.power_grids[usize::from(id.grid)].is_placeholder
                                },
                                BeaconAffectedEntity::Lab { grid, index: _ } => {
                                    !self.power_grids[usize::from(*grid)].is_placeholder
                                },
                            }),
                            // TODO: Other things to check?
                            _ => true,
                        }
                    })
            );

            let affected_grids_and_potential_match = self
                .power_grids
                .iter()
                .filter(|grid| !grid.is_placeholder)
                .all(|pg| {
                    pg.beacon_affected_entities
                        .keys()
                        .map(|e| e.get_power_grid())
                        .all(|affected_grid| {
                            pg.potential_beacon_affected_powergrids
                                .contains(&affected_grid)
                        })
                });
            assert!(affected_grids_and_potential_match);
        }

        Some(updates.into_values())
    }

    #[profiling::function]
    pub fn update(
        &mut self,
        tech_state: &TechState,
        current_time: u32,
        data_store: &DataStore<ItemIdxType, RecipeIdxType>,
    ) -> (ResearchProgress, RecipeTickInfo, Option<LabTickInfo>) {
        {
            profiling::scope!("Trim Power Grids");
            while self
                .power_grids
                .last()
                .map(|grid| grid.is_placeholder)
                .unwrap_or(false)
            {
                let removed_placeholder = self.power_grids.pop();
                assert!(removed_placeholder.unwrap().is_placeholder);
            }
        }

        let (research_progress, production_info, times_labs_used_science, beacon_updates) = self
            .power_grids
            .par_iter_mut()
            .map(|grid| {
                grid.update(
                    &data_store
                        .solar_panel_info
                        .iter()
                        .map(|info| match &info.power_output {
                            crate::data::SolarPanelOutputFunction::Constant(output) => *output,
                            crate::data::SolarPanelOutputFunction::Lookup(output) => {
                                output[usize::try_from(current_time).unwrap()]
                            },
                        })
                        .collect::<Box<[Watt]>>(),
                    tech_state,
                    data_store,
                )
            })
            .reduce(
                || (0, RecipeTickInfo::new(data_store), 0, vec![]),
                |(acc_progress, infos, times_labs_used_science, mut old_updates),
                 (rhs_progress, info, new_times_labs_used_science, new_updates)| {
                    old_updates.extend(new_updates);
                    (
                        acc_progress + rhs_progress,
                        infos + &info,
                        times_labs_used_science + new_times_labs_used_science,
                        old_updates,
                    )
                },
            );

        {
            profiling::scope!("Propagate beacon modifier changes");
            for update in beacon_updates {
                match update.0 {
                    power_grid::BeaconAffectedEntity::Assembler { id } => {
                        self.power_grids[usize::from(id.grid)]
                            .change_assembler_module_modifiers(id, update.1, data_store);
                    },
                    power_grid::BeaconAffectedEntity::Lab { grid, index } => {
                        // TODO:
                        warn!("Currently Beacons do not affect labs!");
                    },
                }
            }
        }

        (
            research_progress,
            production_info,
            tech_state.current_technology.map(|tech| LabTickInfo {
                times_labs_used_science,
                tech,
            }),
        )
    }

    pub fn add_beacon(
        &mut self,
        ty: u8,
        beacon_pos: Position,
        pole_pos: Position,
        modules: Box<[Option<usize>]>,
        affected_entities: impl IntoIterator<Item = BeaconAffectedEntity<RecipeIdxType>>,
        data_store: &DataStore<ItemIdxType, RecipeIdxType>,
    ) -> WeakIndex {
        #[cfg(debug_assertions)]
        {
            let affected_grids_and_potential_match = self
                .power_grids
                .iter()
                .filter(|grid| !grid.is_placeholder)
                .all(|pg| {
                    pg.beacon_affected_entities
                        .keys()
                        .map(|e| e.get_power_grid())
                        .all(|affected_grid| {
                            pg.potential_beacon_affected_powergrids
                                .contains(&affected_grid)
                        })
                });
            assert!(affected_grids_and_potential_match);
        }

        let effect: (i16, i16, i16) = modules
            .iter()
            .flatten()
            .map(|module_ty| {
                (
                    data_store.module_info[*module_ty].speed_mod.into(),
                    data_store.module_info[*module_ty].prod_mod.into(),
                    data_store.module_info[*module_ty].power_mod.into(),
                )
            })
            .reduce(|acc, v| (acc.0 + v.0, acc.1 + v.1, acc.2 + v.2))
            .unwrap_or((0, 0, 0));

        let effect = if self.power_grids[usize::from(self.pole_pos_to_grid_id[&pole_pos])]
            .last_power_mult
            >= MIN_BEACON_POWER_MULT
        {
            // Add the full beacon effect since we are powered
            (
                effect.0 * data_store.beacon_info[usize::from(ty)].effectiveness.0 as i16
                    / data_store.beacon_info[usize::from(ty)].effectiveness.1 as i16,
                effect.1 * data_store.beacon_info[usize::from(ty)].effectiveness.0 as i16
                    / data_store.beacon_info[usize::from(ty)].effectiveness.1 as i16,
                effect.2 * data_store.beacon_info[usize::from(ty)].effectiveness.0 as i16
                    / data_store.beacon_info[usize::from(ty)].effectiveness.1 as i16,
            )
        } else {
            // Not enough power, only add the power_consumption modifier
            (
                0,
                0,
                effect.2 * data_store.beacon_info[usize::from(ty)].effectiveness.0 as i16
                    / data_store.beacon_info[usize::from(ty)].effectiveness.1 as i16,
            )
        };

        let affected_entities: Vec<BeaconAffectedEntity<RecipeIdxType>> =
            affected_entities.into_iter().collect();

        for affected_entity in affected_entities.iter() {
            match affected_entity {
                BeaconAffectedEntity::Assembler { id } => {
                    self.power_grids[usize::from(id.grid)]
                        .change_assembler_module_modifiers(*id, effect, data_store);
                },
                BeaconAffectedEntity::Lab { grid, index } => {
                    self.power_grids[usize::from(*grid)].change_lab_module_modifiers(
                        (*index).try_into().unwrap(),
                        effect,
                        data_store,
                    );
                },
            }
        }
        // I put this here to ensure we do not use effect after this
        let effect: (i16, i16, i16);

        let idx = self.power_grids[usize::from(self.pole_pos_to_grid_id[&pole_pos])].add_beacon(
            ty,
            beacon_pos,
            pole_pos,
            modules,
            affected_entities,
            data_store,
        );

        #[cfg(debug_assertions)]
        {
            let affected_grids_and_potential_match = self
                .power_grids
                .iter()
                .filter(|grid| !grid.is_placeholder)
                .all(|pg| {
                    pg.beacon_affected_entities
                        .keys()
                        .map(|e| e.get_power_grid())
                        .all(|affected_grid| {
                            pg.potential_beacon_affected_powergrids
                                .contains(&affected_grid)
                        })
                });
            assert!(affected_grids_and_potential_match);
        }

        idx
    }

    pub fn remove_beacon(
        &mut self,
        pole_pos: Position,
        idx: WeakIndex,
        data_store: &DataStore<ItemIdxType, RecipeIdxType>,
    ) {
        let beacon_updates = self.power_grids[usize::from(self.pole_pos_to_grid_id[&pole_pos])]
            .remove_beacon(pole_pos, idx, data_store);

        for (effected_entity, effect_change) in beacon_updates {
            match effected_entity {
                BeaconAffectedEntity::Assembler { id } => {
                    self.power_grids[usize::from(id.grid)].change_assembler_module_modifiers(
                        id,
                        effect_change,
                        data_store,
                    );
                },
                BeaconAffectedEntity::Lab { grid, index } => todo!(),
            }
        }
    }

    pub fn add_beacon_affected_entity(
        &mut self,
        beacon_pole_pos: Position,
        beacon_weak_idx: WeakIndex,
        entity: BeaconAffectedEntity<RecipeIdxType>,
        data_store: &DataStore<ItemIdxType, RecipeIdxType>,
    ) {
        self.power_grids[usize::from(self.pole_pos_to_grid_id[&beacon_pole_pos])]
            .potential_beacon_affected_powergrids
            .insert(entity.get_power_grid());
        let (
            _,
            PowerGridEntity::Beacon {
                ty,
                modules,
                affected_entities,
            },
        ) = self.power_grids[usize::from(self.pole_pos_to_grid_id[&beacon_pole_pos])]
            .grid_graph
            .modify_weak_component(beacon_pole_pos, beacon_weak_idx)
        else {
            unreachable!()
        };

        let ty = *ty;

        if affected_entities.contains(&entity) {
            warn!("Tried to insert beacon affected entity {entity:?} again!");
            return;
        }

        affected_entities.push(entity);

        let effect: (i16, i16, i16) = modules
            .iter()
            .flatten()
            .map(|module_ty| {
                (
                    data_store.module_info[*module_ty].speed_mod.into(),
                    data_store.module_info[*module_ty].prod_mod.into(),
                    data_store.module_info[*module_ty].power_mod.into(),
                )
            })
            .reduce(|acc, v| (acc.0 + v.0, acc.1 + v.1, acc.2 + v.2))
            .unwrap_or((0, 0, 0));

        let raw_effect = (
            effect.0 * data_store.beacon_info[usize::from(ty)].effectiveness.0 as i16
                / data_store.beacon_info[usize::from(ty)].effectiveness.1 as i16,
            effect.1 * data_store.beacon_info[usize::from(ty)].effectiveness.0 as i16
                / data_store.beacon_info[usize::from(ty)].effectiveness.1 as i16,
            effect.2 * data_store.beacon_info[usize::from(ty)].effectiveness.0 as i16
                / data_store.beacon_info[usize::from(ty)].effectiveness.1 as i16,
        );

        let effect = if self.power_grids[usize::from(self.pole_pos_to_grid_id[&beacon_pole_pos])]
            .last_power_mult
            >= MIN_BEACON_POWER_MULT
        {
            // Add the full beacon effect since we are powered
            raw_effect
        } else {
            // Not enough power, only add the power_consumption modifier
            (0, 0, raw_effect.2)
        };

        let effect_sum = self.power_grids[usize::from(self.pole_pos_to_grid_id[&beacon_pole_pos])]
            .beacon_affected_entities
            .entry(entity)
            .or_insert((0, 0, 0));

        effect_sum.0 += raw_effect.0;
        effect_sum.1 += raw_effect.1;
        effect_sum.2 += raw_effect.2;

        match entity {
            BeaconAffectedEntity::Assembler { id } => {
                self.power_grids[usize::from(id.grid)]
                    .change_assembler_module_modifiers(id, effect, data_store);
            },
            BeaconAffectedEntity::Lab { grid, index } => {
                self.power_grids[usize::from(grid)].change_lab_module_modifiers(
                    index.try_into().unwrap(),
                    effect,
                    data_store,
                );
            },
        }
    }

    pub fn remove_beacon_affected_entity(&mut self, pole_pos: Position, weak_idx: WeakIndex) {
        todo!()
    }
}
