use std::collections::HashMap;

use crate::{
    data::DataStore,
    frontend::world::{Position, tile::World},
    item::{IdxTrait, Item, WeakIdxTrait},
};

use crate::mining_drill::only_solo_owned::PureDrillStorageOnlySoloOwned;

pub mod only_solo_owned;
pub mod with_shared_ore;

#[derive(Debug, Clone, Copy, serde::Serialize, serde::Deserialize)]
struct MiningDrillID {
    index: u32,
}

#[derive(Debug, Clone, serde::Serialize, serde::Deserialize)]
pub struct FullOreStore {
    stores: Box<[SingleOreStore]>,
}

impl FullOreStore {
    pub fn new<ItemIdxType: IdxTrait, RecipeIdxType: IdxTrait>(
        data_store: &DataStore<ItemIdxType, RecipeIdxType>,
    ) -> Self {
        Self {
            stores: vec![SingleOreStore::new(); todo!()].into_boxed_slice(),
        }
    }

    pub fn get_ore_at_position<ItemIdxType: IdxTrait, RecipeIdxType: IdxTrait>(
        &self,
        world: &World<ItemIdxType, RecipeIdxType>,
        mining_drill_stores: &[MiningDrillStore<ItemIdxType>],
        pos: Position,
    ) -> Option<(Item<ItemIdxType>, u32)> {
        let tracked = self
            .stores
            .iter()
            .zip(mining_drill_stores)
            .enumerate()
            .find_map(|(i, (store, mining_drill_store))| {
                store
                    .get_ore_at_position::<ItemIdxType, RecipeIdxType>(pos, mining_drill_store)
                    .map(|count| (todo!("Map ore idx to item") as Item<ItemIdxType>, count))
            });

        if tracked.is_some() {
            debug_assert!(
                self.stores
                    .iter()
                    .zip(mining_drill_stores)
                    .filter(|(store, mining_drill_store)| store
                        .get_ore_at_position::<ItemIdxType, RecipeIdxType>(pos, mining_drill_store)
                        .is_some())
                    .count()
                    == 1
            );
            return tracked;
        }

        let original_ore = world.get_original_ore_at_pos(pos);

        original_ore
    }
}

#[derive(Debug, Clone, serde::Serialize, serde::Deserialize)]
struct SingleOreStore {
    shared_locations: Vec<u32>,
    shared_location_holes: Vec<usize>,

    ore_lookup: HashMap<Position, OreLoc>,
}

#[derive(Debug, Clone, serde::Serialize, serde::Deserialize)]
enum OreLoc {
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

pub struct MiningDrillStore<ItemIdxType: WeakIdxTrait> {
    pure_solo_owned: Box<[PureDrillStorageOnlySoloOwned<ItemIdxType>]>,
}

impl SingleOreStore {
    pub fn new() -> Self {
        Self {
            shared_locations: vec![],
            shared_location_holes: vec![],
            ore_lookup: HashMap::new(),
        }
    }

    pub fn get_ore_at_position<ItemIdxType: IdxTrait, RecipeIdxType: IdxTrait>(
        &self,
        pos: Position,
        mining_drill_store: &MiningDrillStore<ItemIdxType>,
    ) -> Option<u32> {
        let ore = self.ore_lookup.get(&pos)?;

        match ore {
            OreLoc::Shared { index } => {
                debug_assert!(!self.shared_location_holes.contains(index));
                Some(self.shared_locations[*index])
            },
            OreLoc::SoloOwned {
                owner,
                index_in_owner,
            } => {
                todo!()
            },
            OreLoc::Unowned { amount } => Some(*amount),
        }
    }

    pub fn add_drill_mining_positions<ItemIdxType: IdxTrait, RecipeIdxType: IdxTrait>(
        &mut self,
        self_item: Item<ItemIdxType>,

        positions: impl IntoIterator<Item = Position>,
        world: &mut World<ItemIdxType, RecipeIdxType>,
        mining_drill_store: &mut MiningDrillStore<ItemIdxType>,
    ) {
        let mut positions: Vec<_> = positions.into_iter().collect();

        positions.sort();

        let mut solo_owned_amount = Some(0);

        for pos in &positions {
            match self.ore_lookup.entry(*pos) {
                std::collections::hash_map::Entry::Occupied(mut occupied_entry) => {
                    match occupied_entry.get_mut() {
                        OreLoc::Shared { index } => {
                            // Already shared
                            solo_owned_amount = None;
                        },
                        OreLoc::SoloOwned {
                            owner,
                            index_in_owner,
                        } => {
                            todo!("Make ore shared");
                            solo_owned_amount = None;
                        },
                        OreLoc::Unowned { amount } => {
                            if let Some(solo_amount) = &mut solo_owned_amount {
                                *solo_amount += *amount;
                            }
                        },
                    }
                },
                std::collections::hash_map::Entry::Vacant(vacant_entry) => {
                    let original_ore = world.get_original_ore_at_pos(*pos);

                    if let Some((item, amount)) = original_ore {
                        assert_eq!(self_item, item, "Mixed miner in SingleOreStore::add_drill");

                        if let Some(solo_amount) = &mut solo_owned_amount {
                            *solo_amount += amount;
                        }
                    }
                },
            }
        }

        if let Some(solo_owned_amount) = solo_owned_amount {
        } else {
            todo!("Mining drills sharing ore is not supported yet");
        }
    }
}
