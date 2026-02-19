use crate::inserter::StaticID;
use crate::item::ITEMCOUNTTYPE;
use crate::mining_drill::only_solo_owned::PureDrillStorageOnlySoloOwned;
use crate::power::power_grid::MAX_POWER_MULT;
use crate::storage_list::MaxInsertionLimit;
use crate::{
    data::DataStore,
    frontend::world::Position,
    item::{IdxTrait, Indexable, Item, WeakIdxTrait},
};
#[cfg(feature = "client")]
use egui_show_info_derive::ShowInfo;
#[cfg(feature = "client")]
use get_size2::GetSize;
use itertools::Itertools;
use log::error;
use rayon::iter::IndexedParallelIterator;
use rayon::iter::IntoParallelRefMutIterator;
use rayon::iter::ParallelIterator;
use std::collections::HashMap;

pub mod only_solo_owned;
pub mod with_shared_ore;

#[cfg_attr(feature = "client", derive(ShowInfo), derive(GetSize))]
#[derive(Debug, Clone, Copy, PartialEq, Eq, serde::Serialize, serde::Deserialize)]
struct MiningDrillID {
    index: u32,
}

#[cfg_attr(feature = "client", derive(ShowInfo), derive(GetSize))]
#[derive(Debug, Clone, serde::Serialize, serde::Deserialize)]
pub struct FullOreStore<ItemIdxType: WeakIdxTrait> {
    stores: Box<[SingleOreStore]>,

    pub drills: MiningDrillStore<ItemIdxType>,
}

#[cfg_attr(feature = "client", derive(ShowInfo), derive(GetSize))]
#[derive(Debug, Clone, serde::Serialize, serde::Deserialize)]
pub struct OreLookup<ItemIdxType: WeakIdxTrait> {
    pub ore_lookup: HashMap<Position, (Item<ItemIdxType>, OreLoc), rustc_hash::FxBuildHasher>,
}

impl<ItemIdxType: WeakIdxTrait> Default for OreLookup<ItemIdxType> {
    fn default() -> Self {
        Self {
            ore_lookup: HashMap::default(),
        }
    }
}

impl<ItemIdxType: IdxTrait> OreLookup<ItemIdxType> {
    pub fn add_ore(&mut self, pos: Position, ore: Item<ItemIdxType>, amount: u32) {
        self.ore_lookup
            .insert(pos, (ore, OreLoc::Unowned { amount }));
    }
}

pub struct MiningDrillInfo<ItemIdxType: WeakIdxTrait> {
    pub timer: f32,
    pub prod_timer: f32,
    pub remaining_ore: Vec<(Item<ItemIdxType>, u32)>,
    // TODO: Power, speed, prod stats
}

#[derive(Debug)]
pub enum AddMinerError {
    NoOre,
}

#[cfg_attr(feature = "client", derive(ShowInfo), derive(GetSize))]
#[derive(Debug, Clone, Copy, PartialEq, Eq, serde::Serialize, serde::Deserialize)]
pub struct MiningDrillIdentifier<ItemIdxType: WeakIdxTrait> {
    kind: MiningDrillIdentifierKind<ItemIdxType>,
}

impl<ItemIdxType: IdxTrait> MiningDrillIdentifier<ItemIdxType> {
    pub fn get_static_id(self) -> StaticID {
        match self.kind {
            MiningDrillIdentifierKind::PureOnlySoloOwned { .. } => {
                StaticID::PureSoloOwnedMiningDrill
            },
        }
    }

    pub fn get_index(self) -> u32 {
        match self.kind {
            MiningDrillIdentifierKind::PureOnlySoloOwned { id, .. } => id.index,
        }
    }

    pub fn items(self) -> Vec<Item<ItemIdxType>> {
        match self.kind {
            MiningDrillIdentifierKind::PureOnlySoloOwned { item, .. } => vec![item],
        }
    }
}

#[cfg_attr(feature = "client", derive(ShowInfo), derive(GetSize))]
#[derive(Debug, Clone, Copy, PartialEq, Eq, serde::Serialize, serde::Deserialize)]
enum MiningDrillIdentifierKind<ItemIdxType: WeakIdxTrait> {
    PureOnlySoloOwned {
        item: Item<ItemIdxType>,
        id: MiningDrillID,
    },
}

pub struct MiningDrillUpdate<ItemIdxType: WeakIdxTrait> {
    pub old_id: MiningDrillIdentifier<ItemIdxType>,
    pub new_id: MiningDrillIdentifier<ItemIdxType>,
}

impl<ItemIdxType: IdxTrait> FullOreStore<ItemIdxType> {
    pub fn new<RecipeIdxType: IdxTrait>(
        data_store: &DataStore<ItemIdxType, RecipeIdxType>,
    ) -> Self {
        Self {
            stores: vec![SingleOreStore::new(); data_store.item_names.len()].into_boxed_slice(),

            drills: MiningDrillStore::new(data_store),
        }
    }

    /// `get_ungenerated_ore`: This function allows the user to not add_ore everything, but instead just give a function to calculate it lazily if they want.
    /// If they do not need this they can just give || None
    pub fn get_ore_at_position(
        &self,
        pos: Position,

        ore_lookup: &OreLookup<ItemIdxType>,
        get_ungenerated_ore: impl Fn() -> Option<(Item<ItemIdxType>, u32)>,
    ) -> Option<(Item<ItemIdxType>, u32)> {
        if let Some((item, loc)) = ore_lookup.ore_lookup.get(&pos) {
            match loc {
                OreLoc::Unowned { amount } => Some((*item, *amount)),
                OreLoc::Shared { index } => {
                    let store = &self.stores[item.into_usize()];
                    Some((*item, store.shared_locations[*index as usize]))
                },
                OreLoc::SoloOwned {
                    owner,
                    index_in_owner,
                } => {
                    let ore_amount = self.drills.pure_solo_owned[item.into_usize()]
                        .get_current_tile_values(owner.index)[*index_in_owner as usize];

                    Some((*item, ore_amount))
                },
            }
        } else {
            get_ungenerated_ore()
        }
    }

    pub fn get_available_ore_for_drill(
        &self,
        drill: MiningDrillIdentifier<ItemIdxType>,
    ) -> Vec<(Item<ItemIdxType>, u32)> {
        match drill.kind {
            MiningDrillIdentifierKind::PureOnlySoloOwned { item, id } => {
                let amount = self.drills.pure_solo_owned[item.into_usize()]
                    .get_current_tile_values(id.index)
                    .into_iter()
                    .sum();

                vec![(item, amount)]
            },
        }
    }

    /// Returns the amount of each item mined
    pub fn update<RecipeIdxType: IdxTrait>(
        &mut self,
        mining_productivity_bonus: &[u16],
        data_store: &DataStore<ItemIdxType, RecipeIdxType>,
    ) -> impl Iterator<Item = u32> {
        let items_from_pure_solo: Vec<_> = self
            .drills
            .pure_solo_owned
            .par_iter_mut()
            .zip(mining_productivity_bonus)
            // TODO: Power
            // TODO: Mining Prod
            .map(|(store, mining_productivity_bonus)| {
                profiling::scope!(
                    "Mining Drill Update",
                    format!(
                        "Item: {}",
                        &data_store.item_display_names[store.item.into_usize()]
                    )
                );
                store.update(MAX_POWER_MULT, *mining_productivity_bonus)
            })
            .collect();

        items_from_pure_solo
            .into_iter()
            .map(|(power, num_prod)| num_prod)
    }

    // TODO: This will have to return some updates for other drills
    pub fn add_drill_mining_positions(
        &mut self,

        positions: impl IntoIterator<Item = Position>,

        ore_lookup: &mut OreLookup<ItemIdxType>,

        get_ungenerated_ore: impl Fn(Position) -> Option<(Item<ItemIdxType>, u32)>,
    ) -> Result<
        (
            MiningDrillIdentifier<ItemIdxType>,
            impl IntoIterator<Item = MiningDrillUpdate<ItemIdxType>>,
        ),
        AddMinerError,
    > {
        let mut positions: Vec<_> = positions.into_iter().collect();
        positions.sort();

        let positions_with_ore: Vec<_> = positions
            .iter()
            .filter_map(|&pos| {
                self.get_ore_at_position(pos, ore_lookup, || get_ungenerated_ore(pos))
                    .map(|v| (pos, v))
            })
            .collect();

        let pure_miner_ore = positions_with_ore
            .iter()
            .map(|(_pos, (item, _amount))| item)
            .all_equal_value();

        match pure_miner_ore {
            Ok(pure_ore) => {
                let pure_ore = *pure_ore;
                let only_solo_owner = positions
                    .iter()
                    .filter_map(|pos| ore_lookup.ore_lookup.get(pos))
                    .all(|(_item, ore_loc)| matches!(ore_loc, OreLoc::Unowned { .. }));

                if only_solo_owner {
                    let drill_index = self.drills.pure_solo_owned[pure_ore.into_usize()].add_drill(
                        positions_with_ore
                            .iter()
                            .map(|&(_pos, (_, amount))| amount)
                            .collect(),
                    );

                    for (i, (pos, (_item, _amount))) in positions_with_ore.into_iter().enumerate() {
                        match ore_lookup.ore_lookup.entry(pos) {
                            std::collections::hash_map::Entry::Occupied(mut entry) => {
                                match &mut entry.get_mut().1 {
                                    loc @ OreLoc::Unowned { .. } => {
                                        *loc = OreLoc::SoloOwned {
                                            owner: MiningDrillID { index: drill_index },
                                            index_in_owner: i.try_into().expect(
                                                "Mining Drill owns more than 256 tiles of ore",
                                            ),
                                        }
                                    },
                                    _ => unreachable!(),
                                }
                            },
                            std::collections::hash_map::Entry::Vacant(entry) => {
                                entry.insert((
                                    pure_ore,
                                    OreLoc::SoloOwned {
                                        owner: MiningDrillID { index: drill_index },
                                        index_in_owner: i
                                            .try_into()
                                            .expect("Mining Drill owns more than 256 tiles of ore"),
                                    },
                                ));
                            },
                        }
                    }
                    Ok((
                        MiningDrillIdentifier {
                            kind: MiningDrillIdentifierKind::PureOnlySoloOwned {
                                item: pure_ore,
                                id: MiningDrillID { index: drill_index },
                            },
                        },
                        vec![],
                    ))
                } else {
                    error!("Mining Drills with overlapping mining areas are not supported yet");
                    return Err(AddMinerError::NoOre);
                    // todo!("Mining Drills with overlapping mining areas are not supported yet")
                }
            },

            Err(None) => return Err(AddMinerError::NoOre),
            Err(Some(_)) => {
                error!("Currently mixed mining is not supported yet");
                return Err(AddMinerError::NoOre);
                // todo!("Currently mixed mining is not supported yet")
            },
        }
    }

    pub fn get_info(
        &self,
        drill_id: MiningDrillIdentifier<ItemIdxType>,
    ) -> MiningDrillInfo<ItemIdxType> {
        match drill_id.kind {
            MiningDrillIdentifierKind::PureOnlySoloOwned { item, id } => {
                self.drills.pure_solo_owned[item.into_usize()].get_info(id.index)
            },
        }
    }
}

#[cfg_attr(feature = "client", derive(ShowInfo), derive(GetSize))]
#[derive(Debug, Clone, serde::Serialize, serde::Deserialize)]
struct SingleOreStore {
    shared_locations: Vec<u32>,
    shared_location_holes: Vec<usize>,
}

// TODO: This should prob not be pub
#[cfg_attr(feature = "client", derive(ShowInfo), derive(GetSize))]
#[derive(Debug, Clone, Copy, serde::Serialize, serde::Deserialize)]
pub enum OreLoc {
    Shared {
        index: usize,
    },
    SoloOwned {
        owner: MiningDrillID,
        index_in_owner: u8,
    },
    Unowned {
        amount: u32,
    },
}

#[cfg_attr(feature = "client", derive(ShowInfo), derive(GetSize))]
#[derive(Debug, Clone, serde::Serialize, serde::Deserialize)]
pub struct MiningDrillStore<ItemIdxType: WeakIdxTrait> {
    pure_solo_owned: Box<[PureDrillStorageOnlySoloOwned<ItemIdxType>]>,
}

impl<ItemIdxType: IdxTrait> MiningDrillStore<ItemIdxType> {
    pub fn new<RecipeIdxType: IdxTrait>(
        data_store: &DataStore<ItemIdxType, RecipeIdxType>,
    ) -> Self {
        Self {
            pure_solo_owned: (0..data_store.item_names.len())
                .map(|item_id| {
                    PureDrillStorageOnlySoloOwned::new(
                        Item {
                            id: item_id.try_into().unwrap(),
                        },
                        data_store,
                    )
                })
                .collect(),
        }
    }

    pub fn storages_by_item(
        &mut self,
    ) -> impl Iterator<Item = (MaxInsertionLimit<'_>, &mut [ITEMCOUNTTYPE])> {
        self.pure_solo_owned
            .iter_mut()
            .map(|store| store.get_inventories())
    }
}

impl SingleOreStore {
    pub fn new() -> Self {
        Self {
            shared_locations: vec![],
            shared_location_holes: vec![],
        }
    }
}
