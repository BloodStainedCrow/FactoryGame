use std::{
    collections::HashMap,
    iter::Sum,
    ops::{Add, Div, Mul, Sub},
};

use itertools::Itertools;
use log::warn;
use power_grid::{IndexUpdateInfo, PowerGrid, PowerGridIdentifier};

use crate::{
    assembler::AssemblerOnclickInfo,
    data::DataStore,
    frontend::world::{
        tile::{AssemblerID, MachineID},
        Position,
    },
    item::{IdxTrait, WeakIdxTrait},
    TICKS_PER_SECOND_LOGIC,
};

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

impl Watt {
    #[must_use]
    pub const fn joules_per_tick(self) -> Joule {
        Joule(self.0 / TICKS_PER_SECOND_LOGIC)
    }
}

#[derive(Debug, Clone, serde::Deserialize, serde::Serialize)]
pub struct PowerGridStorage<ItemIdxType: WeakIdxTrait, RecipeIdxType: WeakIdxTrait> {
    pub power_grids: Vec<Option<PowerGrid<ItemIdxType, RecipeIdxType>>>,
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
            .as_ref()
            .unwrap()
            .get_assembler_info(assembler_id, data_store)
    }

    fn create_power_grid(
        &mut self,
        first_pole_position: Position,
        data_store: &DataStore<ItemIdxType, RecipeIdxType>,
    ) -> PowerGridIdentifier {
        // TODO: This is O(N). Is that a problem?
        let hole_idx = self.power_grids.iter().position(Option::is_none);

        let new_grid = PowerGrid::new(data_store, first_pole_position);

        let id = if let Some(hole_idx) = hole_idx {
            self.power_grids[hole_idx] = Some(new_grid);
            hole_idx
                .try_into()
                .expect("If this is not in range, this means we had too many power grids before?")
        } else {
            let len = self.power_grids.len();
            self.power_grids.push(Some(new_grid));
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
        let hole_idx = self.power_grids.iter().position(Option::is_none);

        let poles: Vec<_> = power_grid.grid_graph.keys().into_iter().copied().collect();

        let id = if let Some(hole_idx) = hole_idx {
            self.power_grids[hole_idx] = Some(power_grid);
            hole_idx
                .try_into()
                .expect("If this is not in range, this means we had too many power grids before?")
        } else {
            let len = self.power_grids.len();
            self.power_grids.push(Some(power_grid));
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
    pub fn add_pole(
        &mut self,
        pole_position: Position,
        connected_poles: impl IntoIterator<Item = Position>,
        data_store: &DataStore<ItemIdxType, RecipeIdxType>,
    ) -> Option<(
        impl IntoIterator<Item = Position>,
        impl IntoIterator<Item = IndexUpdateInfo<ItemIdxType, RecipeIdxType>>,
    )> {
        let connected_poles: Vec<_> = connected_poles.into_iter().collect();

        if let Some(first_connection) = connected_poles.last() {
            let grid = self.pole_pos_to_grid_id[first_connection];

            let need_to_merge = !connected_poles
                .iter()
                .map(|pos| self.pole_pos_to_grid_id[pos])
                .all_equal();

            self.pole_pos_to_grid_id.insert(pole_position, grid);

            if need_to_merge {
                let poles_to_update: Vec<_> = connected_poles
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
                    .collect();

                dbg!(&poles_to_update);

                let mut storage_update_vec = vec![];

                let mut ran_once = false;
                for other_grid in connected_poles
                    .iter()
                    .map(|pos| self.pole_pos_to_grid_id[pos])
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

                Some((poles_to_update, storage_update_vec))
            } else {
                self.power_grids[grid as usize]
                    .as_mut()
                    .unwrap()
                    .add_pole(pole_position, connected_poles);
                None
            }
        } else {
            // Create a new grid
            self.create_power_grid(pole_position, data_store);
            None
        }
    }

    /// Returns a list of power poles, which have their PowerGridId changed
    pub fn remove_pole(
        &mut self,
        pole_position: Position,
        data_store: &DataStore<ItemIdxType, RecipeIdxType>,
    ) -> (
        impl IntoIterator<Item = Position>,
        impl IntoIterator<Item = (PowerGridIdentifier, PowerGridIdentifier)>,
    ) {
        let old_id = self.pole_pos_to_grid_id[&pole_position];

        let (new_grids, delete_network) = self.power_grids[old_id as usize]
            .as_mut()
            .unwrap()
            .remove_pole(pole_position, data_store);

        if delete_network {
            debug_assert!(new_grids.into_iter().count() == 0);

            let grid_id = self.pole_pos_to_grid_id.remove(&pole_position).unwrap();

            self.remove_power_grid(grid_id);

            return (vec![], vec![]);
        }

        let mut pole_updates = vec![];
        let mut index_updates = vec![];
        let mut grid_updates = vec![];

        for (new_grid, poles_in_this_grid, storages_to_move_into_that_grid) in new_grids {
            let new_id = self.add_power_grid(new_grid, data_store);
            for pole in poles_in_this_grid {
                *self.pole_pos_to_grid_id.get_mut(&pole).unwrap() = new_id;
                pole_updates.push(pole);
            }

            index_updates.extend(storages_to_move_into_that_grid.into_iter().map(
                |(item, old_storage, new_storage)| IndexUpdateInfo {
                    old: (item, old_storage.change_grid(old_id)),
                    new: (item, new_storage.change_grid(new_id)),
                },
            ));

            grid_updates.push((old_id, new_id));
        }

        (pole_updates, grid_updates)
    }

    fn remove_power_grid(&mut self, id: PowerGridIdentifier) {
        self.power_grids[usize::from(id)] = None;
    }

    #[must_use]
    fn merge_power_grids(
        &mut self,
        kept_id: PowerGridIdentifier,
        removed_id: PowerGridIdentifier,
        data_store: &DataStore<ItemIdxType, RecipeIdxType>,
        new_pole_pos: Position,
        new_pole_connections: impl IntoIterator<Item = Position>,
    ) -> Option<impl IntoIterator<Item = IndexUpdateInfo<ItemIdxType, RecipeIdxType>>> {
        if kept_id == removed_id {
            warn!("Tried to merge a grid with itself!");
            return None;
        }

        let Some(second) = self.power_grids[usize::from(removed_id)].take() else {
            warn!("Tried to merge grid with a grid that no longer exists");
            return None;
        };

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

        let mut updates = None;

        // TODO: If the power grid joining fails for whatever reason, this will abort the program!!!
        take_mut::take(
            self.power_grids[usize::from(kept_id)].as_mut().unwrap(),
            |first| {
                let ret = first.join(
                    second,
                    data_store,
                    kept_id,
                    removed_id,
                    new_pole_pos,
                    new_pole_connections,
                );
                updates = Some(ret.1);
                ret.0
            },
        );

        updates
    }
}
