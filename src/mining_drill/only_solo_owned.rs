#[cfg(feature = "client")]
use egui_show_info_derive::ShowInfo;
#[cfg(feature = "client")]
use get_size2::GetSize;
use itertools::Itertools;

use crate::WeakIdxTrait;
use crate::data::DataStore;
use crate::item::ITEMCOUNTTYPE;
use crate::item::IdxTrait;
use crate::item::Indexable;
use crate::item::Item;
use crate::power::Joule;

use std::cmp::min;
use std::mem;

use crate::storage_list::ALWAYS_FULL;

#[cfg_attr(feature = "client", derive(ShowInfo), derive(GetSize))]
#[derive(Debug, Clone, serde::Serialize, serde::Deserialize)]
pub(super) struct PureDrillStorageOnlySoloOwned<ItemIdxType: WeakIdxTrait> {
    item: Item<ItemIdxType>,
    holes: Vec<usize>,

    solo_owned_ore: Vec<u32>,
    // Having a full usize for the length here is OVERKILL, but reducing it would require unsafe, so I will leave it for now
    solo_owned_count_original_values: Vec<Vec<u32>>,

    inventory: Vec<ITEMCOUNTTYPE>,
}

impl<ItemIdxType: IdxTrait> PureDrillStorageOnlySoloOwned<ItemIdxType> {
    pub fn new<RecipeIdxType: IdxTrait>(
        item: Item<ItemIdxType>,
        data_store: &DataStore<ItemIdxType, RecipeIdxType>,
    ) -> Self {
        Self {
            item,
            holes: vec![],
            solo_owned_ore: vec![],
            solo_owned_count_original_values: vec![],
            inventory: vec![],
        }
    }

    pub fn add_drill(&mut self, ore_tiles: Vec<u32>) -> u32 {
        if let Some(hole_idx) = self.holes.pop() {
            self.inventory[hole_idx] = 0;
            self.solo_owned_ore[hole_idx] = ore_tiles.iter().sum();
            self.solo_owned_count_original_values[hole_idx] = ore_tiles;
            u32::try_from(hole_idx).unwrap()
        } else {
            self.inventory.push(0);
            self.solo_owned_ore.push(ore_tiles.iter().sum());
            self.solo_owned_count_original_values.push(ore_tiles);
            u32::try_from(self.inventory.len() - 1).unwrap()
        }
    }

    pub fn remove_drill(&mut self, index: u32) -> (ITEMCOUNTTYPE, Vec<u32>) {
        let mut tmp = vec![];

        mem::swap(
            &mut tmp,
            &mut self.solo_owned_count_original_values[index as usize],
        );

        let ret = (self.inventory[index as usize], tmp);

        self.inventory[index as usize] = 0;

        ret
    }

    /// mining_prod in percent
    pub fn update(&mut self, power_mult: u8, mining_prod: u16) -> (Joule, u32) {
        let mut produced = 0u32;
        for (inventory, solo_items) in self
            .inventory
            .iter_mut()
            .zip(self.solo_owned_ore.iter_mut())
        {
            // TODO: Timer and prod and stuff

            // FIXME: Let's just assume we produce an item every tick for now (and 0 productivity)
            let mut amount_to_remove = u8::from(*inventory < u8::MAX);

            // We are mining from a source, which we control alone

            // Make sure we never underflow!
            amount_to_remove = min(
                amount_to_remove as u8,
                min(u8::MAX as u32, *solo_items) as u8,
            );
            *solo_items -= amount_to_remove as u32;

            // TODO: Productivity
            *inventory += amount_to_remove;
            produced += amount_to_remove as u32;
        }

        (Joule(0), produced)
    }

    pub fn get_current_tile_values(&self, index: u32) -> Vec<u32> {
        let mut owned_resource_tile_list: Vec<_> = self.solo_owned_count_original_values
            [index as usize]
            .iter()
            .copied()
            .collect();

        let mut resources_to_remove =
            owned_resource_tile_list.iter().sum::<u32>() - self.solo_owned_ore[index as usize];

        let tile_count = owned_resource_tile_list.len();
        for (i, tile) in owned_resource_tile_list
            .iter_mut()
            .enumerate()
            .sorted_by_key(|v| *v.1)
        {
            let tiles_left = tile_count - i;
            let resources_from_this_tile = min(*tile, resources_to_remove / tiles_left as u32);

            *tile -= resources_from_this_tile;
            resources_to_remove -= resources_from_this_tile;
        }

        assert_eq!(resources_to_remove, 0);

        owned_resource_tile_list
    }

    pub fn get_inventories(&mut self) -> (&[ITEMCOUNTTYPE], &mut [ITEMCOUNTTYPE]) {
        (ALWAYS_FULL, self.inventory.as_mut_slice())
    }
}
