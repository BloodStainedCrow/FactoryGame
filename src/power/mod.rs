use std::{
    iter::Sum,
    ops::{Add, Div, Mul, Sub},
};

use log::warn;
use power_grid::{IndexUpdateInfo, PowerGrid, PowerGridIdentifier, PowerPoleUpdateInfo};

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

mod grid_graph;
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
pub struct PowerGridStorage<RecipeIdxType: WeakIdxTrait> {
    pub power_grids: Vec<Option<PowerGrid<RecipeIdxType>>>,
}

impl<RecipeIdxType: IdxTrait> PowerGridStorage<RecipeIdxType> {
    pub fn new() -> Self {
        Self {
            power_grids: vec![],
        }
    }

    pub fn get_assembler_info<ItemIdxType: IdxTrait>(
        &self,
        assembler_id: AssemblerID<RecipeIdxType>,
        data_store: &DataStore<ItemIdxType, RecipeIdxType>,
    ) -> AssemblerOnclickInfo<ItemIdxType> {
        self.power_grids[Into::<MachineID<RecipeIdxType>>::into(assembler_id).get_grid() as usize]
            .as_ref()
            .unwrap()
            .get_assembler_info(assembler_id, data_store)
    }

    pub fn add_power_grid<ItemIdxType: IdxTrait>(
        &mut self,
        data_store: &DataStore<ItemIdxType, RecipeIdxType>,
    ) -> PowerGridIdentifier {
        // TODO: This is O(N). Is that a problem?
        let hole_idx = self.power_grids.iter().position(Option::is_none);

        if let Some(hole_idx) = hole_idx {
            self.power_grids[hole_idx] = Some(PowerGrid::new(data_store));
            hole_idx
                .try_into()
                .expect("If this is not in range, this means we had too many power grids before?")
        } else {
            let len = self.power_grids.len();
            self.power_grids.push(Some(PowerGrid::new(data_store)));
            len.try_into().expect(&format!(
                "Too many power grids, max, {} allowed",
                PowerGridIdentifier::MAX
            ))
        }
    }

    pub fn remove_power_grid(&mut self, id: PowerGridIdentifier) {
        self.power_grids[usize::from(id)] = None;
    }

    #[must_use]
    pub fn merge_power_grids<ItemIdxType: IdxTrait>(
        &mut self,
        kept_id: PowerGridIdentifier,
        removed_id: PowerGridIdentifier,
        data_store: &DataStore<ItemIdxType, RecipeIdxType>,
        new_pole_pos: Position,
        new_pole_connections: impl IntoIterator<Item = Position>,
    ) -> Option<(
        impl IntoIterator<Item = IndexUpdateInfo<ItemIdxType, RecipeIdxType>>,
        impl IntoIterator<Item = PowerPoleUpdateInfo>,
    )> {
        if kept_id == removed_id {
            warn!("Tried to merge a grid with itself!");
            return None;
        }

        let second = self.power_grids[usize::from(removed_id)].take().unwrap();

        let pole_updates: Vec<PowerPoleUpdateInfo> = second
            .grid_graph
            .poles()
            .into_iter()
            .map(|pos| PowerPoleUpdateInfo {
                position: pos,
                new_grid_id: kept_id,
            })
            .collect();

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

        updates.map(|u| (u, pole_updates))
    }
}
