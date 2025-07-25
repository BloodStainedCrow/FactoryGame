use std::{cmp::min, iter, u8};

use crate::{
    data::DataStore,
    item::{ITEMCOUNTTYPE, IdxTrait, Item, WeakIdxTrait},
    power::Joule,
    storage_list::PANIC_ON_INSERT,
};

struct PureDrillConcept<ItemIdxType: WeakIdxTrait> {
    item: Item<ItemIdxType>,
    // TODO: Maybe u8?
    solo_owned_ore_source_count: u16,
    solo_owned_ore: u32,

    inventory: ITEMCOUNTTYPE,

    /// "Chest" indices
    shared_ore_sources: Vec<usize>,
    output_inserter: Option<!>,
}

pub struct PureDrillStorageWithSharedOreTiles<ItemIdxType: WeakIdxTrait> {
    item: Item<ItemIdxType>,
    holes: Vec<usize>,

    solo_owned_ore: Vec<u32>,
    solo_owned_current_threshhold: Vec<u32>,
    // Having a full usize for the length here is OVERKILL, but reducing it would require unsafe, so I will leave it for now
    solo_owned_count_threshholds: Vec<Vec<u32>>,

    inventory: Vec<ITEMCOUNTTYPE>,
    // TODO: This means the maximium allowed mining area for a mining drill entity is 32x32. Enforce that!
    current_source: Vec<u8>,

    shared_sources: Vec<Vec<u32>>,
}

impl<ItemIdxType: IdxTrait> PureDrillStorageWithSharedOreTiles<ItemIdxType> {
    pub fn new<RecipeIdxType: IdxTrait>(
        item: Item<ItemIdxType>,
        data_store: &DataStore<ItemIdxType, RecipeIdxType>,
    ) -> Self {
        Self {
            item,
            holes: vec![],
            solo_owned_ore: vec![],
            solo_owned_current_threshhold: vec![],
            solo_owned_count_threshholds: vec![],
            inventory: vec![],
            current_source: vec![],
            shared_sources: vec![],
        }
    }

    pub fn add_mining_drill(&mut self, info: !) -> usize {
        todo!()
    }

    pub fn make_ore_shared(
        &mut self,
        index: usize,
        old_owned_sum: u32,
        old_owned_by_newly_shared: u32,
        shared_ore_patches: &mut Vec<u32>,
    ) -> usize {
        // let ratio = self.solo_owned_ore[index] as f64 / old_owned_sum as f64;

        // let new_owned_by_shared = (old_owned_by_newly_shared as f64 * ratio) as u32;

        let new_owned_by_shared: u32 = u32::try_from(
            (u64::from(old_owned_by_newly_shared) * u64::from(self.solo_owned_ore[index]))
                / u64::from(old_owned_sum),
        )
        .expect(
            "This should always be less than old_owned_by_newly_shared which fit into u32 before",
        );

        self.solo_owned_ore[index] -= new_owned_by_shared;

        // FIXME: This is broken, since we modify the threshholds
        let steps = iter::once(&0)
            .chain(self.solo_owned_count_threshholds[index].iter())
            .zip(
                self.solo_owned_count_threshholds[index]
                    .iter()
                    .chain(iter::once(&old_owned_sum)),
            )
            .map(|(next, prev)| {
                assert!(prev >= next);
                prev - next
            });

        let steps_per_tile = steps
            .enumerate()
            .map(|(num_tiles_left, step)| {
                assert_eq!(step % (num_tiles_left as u32 + 1), 0);
                step / (num_tiles_left as u32 + 1)
            })
            .collect::<Vec<_>>();

        let tile_sizes = (0..steps_per_tile.len()).map(|len| steps_per_tile[len..].iter().sum());

        let to_remove_idx = tile_sizes
            .clone()
            .position(|v: u32| v == old_owned_by_newly_shared)
            .unwrap();

        let calculated_tiles = tile_sizes
            .enumerate()
            .filter(|(i, _)| *i != to_remove_idx)
            .map(|(_, v)| v)
            .collect::<Vec<_>>();

        let new_threshhold = calculate_threshholds(&calculated_tiles);

        self.solo_owned_count_threshholds[index] = new_threshhold;

        shared_ore_patches.push(new_owned_by_shared);

        let shared_idx = shared_ore_patches.len() - 1;

        self.shared_sources[index].push(u32::try_from(shared_idx).unwrap());

        shared_idx
    }

    pub fn update<RecipeIdxType: IdxTrait>(
        &mut self,
        power_mult: u8,
        shared_ore_patches: &mut [u32],
        switch_sources: bool,
        data_store: &DataStore<ItemIdxType, RecipeIdxType>,
    ) -> Joule {
        if switch_sources {
            for ((current_source, num_owned_sources), num_shared_sources) in self
                .current_source
                .iter_mut()
                .zip(
                    self.solo_owned_count_threshholds
                        .iter()
                        .zip(self.solo_owned_ore.iter())
                        .map(|(threshholds, solo_ore)| {
                            if threshholds.len() > 0 {
                                threshholds.len() + 1
                            } else {
                                if *solo_ore > 0 { 1 } else { 0 }
                            }
                        }),
                )
                .zip(
                    self.shared_sources
                        .iter()
                        .map(|shared_sources| shared_sources.len()),
                )
            {
                debug_assert!(num_owned_sources < (u8::MAX as usize));
                debug_assert!(num_shared_sources < (u8::MAX as usize));

                // FIXME: This breaks if total_sources is 256, which then wraps to 0
                let total_sources = num_owned_sources as u8 + num_shared_sources as u8;

                // FIXME: And causes a crash here
                *current_source = (*current_source).wrapping_add(1) % total_sources;
            }
        }

        for (
            ((((inventory, current_source), shared_source_indices), threshholds), solo_items),
            current_threshhold,
        ) in self
            .inventory
            .iter_mut()
            .zip(self.current_source.iter_mut())
            .zip(self.shared_sources.iter_mut())
            .zip(self.solo_owned_count_threshholds.iter_mut())
            .zip(self.solo_owned_ore.iter_mut())
            .zip(self.solo_owned_current_threshhold.iter_mut())
        {
            // TODO: Timer and prod and stuff

            // FIXME: Let's just assume we produce an item every tick for now (and 0 productivity)
            let mut amount_to_remove = u8::from(*inventory < u8::MAX);

            if *current_source >= (shared_source_indices.len() as u8) {
                // We are mining from a source, which we control alone

                // Make sure we never underflow!
                amount_to_remove = min(
                    amount_to_remove as u8,
                    min(u8::MAX as u32, *solo_items) as u8,
                );
                *solo_items -= amount_to_remove as u32;

                if *solo_items <= *current_threshhold {
                    if threshholds.pop().is_none() {
                        // This drill no longer has any solo items
                        assert_eq!(*solo_items, 0);
                        assert_eq!(*current_threshhold, 0);
                    } else {
                        while *solo_items <= *threshholds.last().unwrap_or(&0) {
                            if threshholds.pop().is_none() {
                                // This drill no longer has any solo items
                                assert_eq!(*solo_items, 0);
                            }
                            todo!("What to do with current_source?");
                        }
                    }

                    *current_threshhold = threshholds.last().copied().unwrap_or(0);

                    todo!("What to do with current_source?");
                }
            } else {
                let shared_source = shared_source_indices[(*current_source) as usize];

                let shared_source_storage = &mut shared_ore_patches[shared_source as usize];

                // Make sure we never underflow!
                amount_to_remove = min(
                    amount_to_remove as u8,
                    min(u8::MAX as u32, *shared_source_storage) as u8,
                );
                *shared_source_storage -= amount_to_remove as u32;

                if *shared_source_storage == 0 {
                    shared_source_indices.remove((*current_source) as usize);

                    todo!("What to do with current_source?");
                }
            }

            // TODO: Productivity
            *inventory += amount_to_remove;
        }

        Joule(0)
    }

    pub fn get_inventories(&mut self) -> (&[ITEMCOUNTTYPE], &mut [ITEMCOUNTTYPE]) {
        (PANIC_ON_INSERT, self.inventory.as_mut_slice())
    }
}

struct MixedDrill {}

fn calculate_threshholds(owned_resource_tile_list: &[u32]) -> Vec<u32> {
    if owned_resource_tile_list.is_empty() {
        return vec![];
    }

    assert!(owned_resource_tile_list.iter().all(|v| *v > 0));

    let mut owned_resource_tile_list: Vec<_> = owned_resource_tile_list.iter().copied().collect();

    let mut ret = vec![];

    loop {
        if ret.len() == owned_resource_tile_list.len() - 1 {
            break;
        }

        let amount_to_remove = owned_resource_tile_list
            .iter()
            .copied()
            .filter(|v| *v > 0)
            .min()
            .unwrap_or(0);

        let num_empty = owned_resource_tile_list.iter().filter(|v| **v == 0).count();

        let sum = owned_resource_tile_list.iter().sum::<u32>();

        for _ in 0..(owned_resource_tile_list
            .iter()
            .filter(|v| **v == amount_to_remove)
            .count())
        {
            let elem = sum - amount_to_remove * (owned_resource_tile_list.len() - num_empty) as u32;
            ret.insert(0, elem);

            if elem == 0 {
                break;
            }
        }

        owned_resource_tile_list
            .iter_mut()
            .for_each(|v| *v = (*v).saturating_sub(amount_to_remove));
    }

    ret
}

#[cfg(test)]
mod test {
    use std::{
        cmp::{max, min},
        collections::HashSet,
        iter,
    };

    use itertools::Itertools;
    use proptest::{
        array::uniform5,
        prelude::{Just, Strategy},
        prop_assert, prop_assert_eq, proptest,
    };

    use crate::mining_drill::with_shared_ore::calculate_threshholds;

    #[test]
    fn test_threshhold_empty() {
        assert!(calculate_threshholds(&mut []).is_empty());
    }

    #[test]
    fn test_threshhold() {
        assert_eq!(
            calculate_threshholds(&mut [50, 200, 500]).as_slice(),
            &[300, 600]
        );
    }

    #[test]
    fn test_threshhold_same_highest() {
        assert_eq!(
            calculate_threshholds(&mut [100, 500, 500]).as_slice(),
            &[0, 800]
        );
    }

    #[test]
    fn test_threshhold_same_middle() {
        assert_eq!(
            calculate_threshholds(&mut [100, 500, 500, 800]).as_slice(),
            &[300, 300, 1500]
        );
    }

    #[test]
    fn test_threshhold_same_smallest() {
        assert_eq!(
            calculate_threshholds(&mut [500, 500, 800]).as_slice(),
            &[300, 300]
        );
    }

    fn old_values_and_mined() -> impl Strategy<Value = ([u32; 5], u32)> {
        uniform5(1u32..1_000).prop_flat_map(|items| {
            let sum = items.iter().sum();

            (Just(items), 0..=sum)
        })
    }

    proptest! {
        #[test]
        fn test_threshhold_one_slot(value in 1u32..1_000) {
            prop_assert!(calculate_threshholds(&mut [value]).is_empty());
        }

        #[test]
        fn test_threshhold_two_slot(value in 1u32..1_000, value2 in 1u32..1_000) {
            prop_assert!(calculate_threshholds(&mut [value, value2])[0] == max(value, value2) - min(value, value2));
        }


        #[test]
        fn test_threshhold_removal_calculation(to_remove in 0usize..5, old_values in uniform5(1u32..1_0)) {
            let after_removal: [u32; 4] = old_values.iter().copied().enumerate().filter(|(i, _)| *i != to_remove).map(|(_, v)| v).collect_array().unwrap();
            let goal_threshholds = calculate_threshholds(&after_removal);

            let removed_value = old_values[to_remove];
            let old_sum: u32 = old_values.iter().sum();
            let starting_threshholds = calculate_threshholds(&old_values);

            let steps = iter::once(&0).chain(starting_threshholds.iter()).zip(starting_threshholds.iter().chain(iter::once(&old_sum))).map(|(next, prev)| {
                assert!(prev >= next);
                prev - next
            }).collect_vec();


            let steps_per_tile = steps.iter().enumerate().map(|(num_tiles_left, step)| {
                assert_eq!(step % (num_tiles_left as u32 + 1), 0);
                step / (num_tiles_left as u32 + 1)
            }).collect_vec();

            let tile_sizes = (0..steps_per_tile.len()).map(|len| {
                steps_per_tile[len..].iter().sum()
            }).collect_vec();

            prop_assert!(tile_sizes.contains(&removed_value), "{:?}", tile_sizes);

            let to_remove_idx = tile_sizes.iter().position(|v| *v == removed_value).unwrap();

            let calculated_tiles = tile_sizes.iter().copied().enumerate().filter(|(i, _)| *i != to_remove_idx).map(|(_, v)| v).collect_vec();

            let hashset: HashSet<_> = calculated_tiles.iter().copied().collect();

            prop_assert_eq!(hashset, HashSet::from_iter(after_removal.iter().copied()));

            let calculated_new_thrsh = calculate_threshholds(&calculated_tiles);

            prop_assert_eq!(calculated_new_thrsh, goal_threshholds);
        }


        #[test]
        fn test_threshhold_removal_calculation_with_mined_items(to_remove in 0usize..5, (old_values, mined_items) in old_values_and_mined()) {
            let after_removal: [u32; 4] = old_values.iter().copied().enumerate().filter(|(i, _)| *i != to_remove).map(|(_, v)| v).collect_array().unwrap();
            let goal_threshholds = calculate_threshholds(&after_removal);

            let removed_value = old_values[to_remove];
            let old_sum: u32 = old_values.iter().sum();
            let mut starting_threshholds = calculate_threshholds(&old_values);

            let solo_items = old_sum - mined_items;

            starting_threshholds.retain(|v| *v < solo_items);


            let steps = iter::once(&0).chain(starting_threshholds.iter()).zip(starting_threshholds.iter().chain(iter::once(&old_sum))).map(|(next, prev)| {
                assert!(prev >= next);
                prev - next
            }).collect_vec();


            let steps_per_tile = steps.iter().enumerate().map(|(num_tiles_left, step)| {
                assert_eq!(step % (num_tiles_left as u32 + 1), 0);
                step / (num_tiles_left as u32 + 1)
            }).collect_vec();

            let tile_sizes = (0..steps_per_tile.len()).map(|len| {
                steps_per_tile[len..].iter().sum()
            }).collect_vec();

            prop_assert!(tile_sizes.contains(&removed_value), "{:?}", tile_sizes);

            let to_remove_idx = tile_sizes.iter().position(|v| *v == removed_value).unwrap();

            let calculated_tiles = tile_sizes.iter().copied().enumerate().filter(|(i, _)| *i != to_remove_idx).map(|(_, v)| v).collect_vec();

            let hashset: HashSet<_> = calculated_tiles.iter().copied().collect();

            prop_assert_eq!(hashset, HashSet::from_iter(after_removal.iter().copied()));

            let calculated_new_thrsh = calculate_threshholds(&calculated_tiles);

            prop_assert_eq!(calculated_new_thrsh, goal_threshholds);
        }
    }
}
