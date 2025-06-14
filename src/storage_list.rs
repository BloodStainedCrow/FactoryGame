use std::u16;

use itertools::Itertools;
use rayon::iter::IndexedParallelIterator;
use strum::IntoEnumIterator;

use crate::{
    assembler::FullAssemblerStore,
    chest::FullChestStore,
    data::{DataStore, ItemRecipeDir},
    inserter::{FakeUnionStorage, StaticID, Storage},
    item::{usize_from, IdxTrait, Item, ITEMCOUNTTYPE},
    lab::MultiLabStore,
    power::{power_grid::PowerGridIdentifier, PowerGridStorage},
    split_arbitrary::split_arbitrary_mut_slice,
};

// FIXME: We just yeet 10MB of RAM into  the wind here :/
pub const ALWAYS_FULL: &'static [ITEMCOUNTTYPE] = &[0; 10_000_000];
pub const PANIC_ON_INSERT: &'static [ITEMCOUNTTYPE] = &[0; 0];
pub const PANIC_ON_INSERT_DATA: &'static [ITEMCOUNTTYPE] = &[0; 0];

type SingleGridStorage<'a, 'b> = (&'a [ITEMCOUNTTYPE], &'b mut [ITEMCOUNTTYPE]);
pub type SingleItemStorages<'a, 'b> = &'a mut [SingleGridStorage<'b, 'b>]; //[SingleGridStorage; NUM_RECIPES * NUM_GRIDS];
pub type FullStorages<'a, 'b> = Box<[SingleGridStorage<'a, 'b>]>; //[SingleGridStorage; NUM_ITEMS * NUM_RECIPES * NUM_GRIDS];

fn num_labs<ItemIdxType: IdxTrait, RecipeIdxType: IdxTrait>(
    item: Item<ItemIdxType>,
    data_store: &DataStore<ItemIdxType, RecipeIdxType>,
) -> usize {
    usize::from(data_store.item_is_science[usize_from(item.id)])
}

pub fn num_recipes<ItemIdxType: IdxTrait, RecipeIdxType: IdxTrait>(
    item: Item<ItemIdxType>,
    data_store: &DataStore<ItemIdxType, RecipeIdxType>,
) -> usize {
    // TODO: This assumes that no recipe will output one of its ingredients.
    // if that were the case, we could not differentiate it!
    let num_recipes = data_store.item_to_recipe_count_where_its_ingredient[usize_from(item.id)]
        .len()
        + data_store.item_to_recipe_where_its_output[usize_from(item.id)].len();
    num_recipes
}

pub fn static_size<ItemIdxType: IdxTrait, RecipeIdxType: IdxTrait>(
    item: Item<ItemIdxType>,
    data_store: &DataStore<ItemIdxType, RecipeIdxType>,
) -> usize {
    let mut size = 0;

    // Chests
    size += 1;

    size
}

pub fn grid_size<ItemIdxType: IdxTrait, RecipeIdxType: IdxTrait>(
    item: Item<ItemIdxType>,
    data_store: &DataStore<ItemIdxType, RecipeIdxType>,
) -> usize {
    let num_recipes = num_recipes(item, data_store);
    let num_labs = num_labs(item, data_store);
    num_recipes + num_labs
}

fn size_of_single_item_slice<ItemIdxType: IdxTrait, RecipeIdxType: IdxTrait>(
    item: Item<ItemIdxType>,
    num_grids_total: usize,
    data_store: &DataStore<ItemIdxType, RecipeIdxType>,
) -> usize {
    let num_different_static_containers = data_store.num_different_static_containers;
    let grid_size = grid_size(item, data_store);
    let static_size = static_size(item, data_store);

    let first_grid_offs = static_size.div_ceil(grid_size);

    (first_grid_offs + num_grids_total) * grid_size
}

pub fn index<'a, 'b, RecipeIdxType: IdxTrait>(
    slice: SingleItemStorages<'a, 'b>,
    storage_id: Storage<RecipeIdxType>,
    num_grids_total: usize,
    num_recipes: usize,
    grid_size: usize,
    static_size: usize,
) -> (&'a ITEMCOUNTTYPE, &'a mut ITEMCOUNTTYPE) {
    let first_grid_offs_in_grids = static_size.div_ceil(grid_size);

    match storage_id {
        Storage::Assembler {
            grid,
            recipe_idx_with_this_item,
            index,
        } => {
            debug_assert!(
                usize_from(recipe_idx_with_this_item) < num_recipes,
                "The recipe stored in an inserter needs to be translated!"
            );
            let outer = &mut slice[(first_grid_offs_in_grids + Into::<usize>::into(grid))
                * grid_size
                + Into::<usize>::into(recipe_idx_with_this_item)];
            (
                &outer.0[usize::try_from(index).unwrap()],
                &mut outer.1[usize::try_from(index).unwrap()],
            )
        },
        Storage::Lab { grid, index } => {
            let outer = &mut slice
                [(first_grid_offs_in_grids + Into::<usize>::into(grid)) * grid_size + num_recipes];
            (
                &outer.0[usize::try_from(index).unwrap()],
                &mut outer.1[usize::try_from(index).unwrap()],
            )
        },
        Storage::Static { static_id, index } => {
            // debug_assert!(usize::from(static_id) < data_store.num_different_static_containers);
            let outer = &mut slice[Into::<usize>::into(static_id)];
            (
                &outer.0[usize::try_from(index).unwrap()],
                &mut outer.1[usize::try_from(index).unwrap()],
            )
        },
    }
}

pub fn index_fake_union<'a, 'b>(
    slice: SingleItemStorages<'a, 'b>,
    storage_id: FakeUnionStorage,
    num_grids_total: usize,
    num_recipes: usize,
    grid_size: usize,
) -> (&'a ITEMCOUNTTYPE, &'a mut ITEMCOUNTTYPE) {
    let (outer, inner) = storage_id.into_inner_and_outer_indices_with_statics_at_zero(grid_size);

    let subslice = &mut slice[outer];
    (&subslice.0[inner], &mut subslice.1[inner])
}

#[profiling::function]
pub fn sizes<'a, ItemIdxType: IdxTrait, RecipeIdxType: IdxTrait>(
    data_store: &'a DataStore<ItemIdxType, RecipeIdxType>,
    num_grids_total: usize,
) -> impl IntoIterator<Item = usize> + use<'a, ItemIdxType, RecipeIdxType> {
    (0..data_store.item_names.len())
        .map(|i| Item {
            id: ItemIdxType::try_from(i).unwrap(),
        })
        .map(move |item| size_of_single_item_slice(item, num_grids_total, data_store))
}

pub fn full_to_by_item<'a, 'b, 'c, 'd>(
    storages: &'d mut FullStorages<'a, 'b>,
    sizes: &'c [usize],
) -> impl IntoIterator<Item = SingleItemStorages<'a, 'b>>
       + use<'a, 'b, 'c, 'd>
       + IndexedParallelIterator<Item = SingleItemStorages<'a, 'b>>
where
    'b: 'a,
    'd: 'a,
    'd: 'b,
{
    split_arbitrary_mut_slice(storages, &sizes)
}

#[profiling::function]
fn get_full_storage_index<ItemIdxType: IdxTrait, RecipeIdxType: IdxTrait>(
    item: Item<ItemIdxType>,
    storage: Storage<RecipeIdxType>,
    num_power_grids: usize,
    data_store: &DataStore<ItemIdxType, RecipeIdxType>,
) -> usize {
    // FIXME: Recalculating this repeatedly is bad
    let item_offs: usize = (0..item.id.into())
        .map(|id| Item {
            id: ItemIdxType::try_from(id).unwrap(),
        })
        .map(|item| size_of_single_item_slice(item, num_power_grids, data_store))
        .sum();

    let num_recipes = num_recipes(item, data_store);
    let num_labs = num_labs(item, data_store);
    let grid_size = grid_size(item, data_store);
    let static_size = static_size(item, data_store);

    let first_grid_offs = static_size.div_ceil(grid_size);

    let ret = match storage {
        Storage::Assembler {
            grid,
            recipe_idx_with_this_item: recipe,
            index: _,
        } => item_offs + (first_grid_offs + usize::from(grid)) * grid_size + usize_from(recipe),
        Storage::Lab { grid, index: _ } => {
            item_offs + (first_grid_offs + usize::from(grid)) * grid_size + num_recipes
        },
        Storage::Static {
            static_id,
            index: _,
        } => item_offs + usize::from(static_id),
    };

    ret
}

#[profiling::function]
pub fn storages_by_item<'a, ItemIdxType: IdxTrait, RecipeIdxType: IdxTrait>(
    grids: &'a mut PowerGridStorage<ItemIdxType, RecipeIdxType>,
    chest_store: &'a mut FullChestStore<ItemIdxType>,
    data_store: &DataStore<ItemIdxType, RecipeIdxType>,
) -> FullStorages<'a, 'a> {
    let num_power_grids = grids.power_grids.len();

    #[cfg(debug_assertions)]
    {
        assert!(
            all_storages(grids, chest_store, data_store)
                .into_iter()
                .map(|v| get_full_storage_index(v.0, v.1, num_power_grids, data_store))
                .all_unique(),
            "{:?}",
            Vec::from_iter(
                all_storages(grids, chest_store, data_store)
                    .into_iter()
                    .map(|v| {
                        let idx = get_full_storage_index(v.0, v.1, num_power_grids, data_store);
                        (v, idx)
                    })
            )
        );
        let max_index = all_storages(grids, chest_store, data_store)
            .into_iter()
            .map(|v| get_full_storage_index(v.0, v.1, num_power_grids, data_store))
            .max();

        match max_index {
            Some(max_index) => {
                assert_eq!(
                    max_index,
                    all_storages(grids, chest_store, data_store)
                        .into_iter()
                        .count()
                        - 1,
                    "{:?}",
                    {
                        let mut storages: Vec<_> = all_storages(grids, chest_store, data_store)
                            .into_iter()
                            .map(|v| {
                                (
                                    get_full_storage_index(v.0, v.1, num_power_grids, data_store),
                                    (v.0, v.1, v.2.len()),
                                )
                            })
                            .collect();
                        storages.sort_by_key(|v| {
                            get_full_storage_index(v.1 .0, v.1 .1, num_power_grids, data_store)
                        });

                        for i in 0..max_index {
                            if !storages.iter().any(|v| v.0 == i) {
                                dbg!(i);
                            }
                        }

                        storages
                    }
                )
            },
            None => {
                assert_eq!(
                    0,
                    all_storages(grids, chest_store, data_store)
                        .into_iter()
                        .count()
                )
            },
        }
    }

    let all_storages = all_storages(grids, chest_store, data_store);

    let all_storages_sorted = {
        profiling::scope!("Sort Storages");
        let mut ret: Vec<_> = all_storages.into_iter().collect();

        ret.sort_by_cached_key(|v| get_full_storage_index(v.0, v.1, num_power_grids, data_store));

        // TODO: Collecting twice here seems wasteful
        ret.into_iter().map(|v| (v.2, v.3)).collect()
    };
    all_storages_sorted
}

#[profiling::function]
fn all_storages<'a, 'b, ItemIdxType: IdxTrait, RecipeIdxType: IdxTrait>(
    grids: &'a mut PowerGridStorage<ItemIdxType, RecipeIdxType>,
    chest_store: &'a mut FullChestStore<ItemIdxType>,
    data_store: &'b DataStore<ItemIdxType, RecipeIdxType>,
) -> impl IntoIterator<
    Item = (
        Item<ItemIdxType>,
        Storage<RecipeIdxType>,
        &'a [ITEMCOUNTTYPE],
        &'a mut [ITEMCOUNTTYPE],
    ),
> + use<'a, 'b, ItemIdxType, RecipeIdxType> {
    let all_storages = grids
        .power_grids
        .iter_mut()
        .enumerate()
        .flat_map(|(grid_id, grid)| {
            let grid_id = grid_id.try_into().unwrap();
            all_assembler_storages(grid_id, &mut grid.stores, data_store)
                .into_iter()
                .chain(all_lab_storages(grid_id, &mut grid.lab_stores, data_store))
        })
        .chain(all_static_storages(chest_store, data_store));
    all_storages
}

#[profiling::function]
fn all_assembler_storages<'a, 'b, ItemIdxType: IdxTrait, RecipeIdxType: IdxTrait>(
    grid: PowerGridIdentifier,
    assembler_store: &'a mut FullAssemblerStore<RecipeIdxType>,
    data_store: &'b DataStore<ItemIdxType, RecipeIdxType>,
) -> impl IntoIterator<
    Item = (
        Item<ItemIdxType>,
        Storage<RecipeIdxType>,
        &'a [ITEMCOUNTTYPE],
        &'a mut [ITEMCOUNTTYPE],
    ),
> + use<'a, 'b, ItemIdxType, RecipeIdxType> {
    let i = assembler_store
        .assemblers_0_1
        .iter_mut()
        .enumerate()
        .map(move |(recipe_id_0_1, multi)| {
            let item = data_store.recipe_to_items[&multi.recipe]
                .iter()
                .filter_map(|(dir, item)| {
                    if *dir == ItemRecipeDir::Out {
                        Some(item)
                    } else {
                        None
                    }
                })
                .nth(0)
                .copied()
                .unwrap();

            (
                item,
                Storage::Assembler {
                    grid,
                    recipe_idx_with_this_item: data_store.recipe_to_translated_index[&(
                        data_store.ing_out_num_to_recipe[&(0, 1)][recipe_id_0_1],
                        item,
                    )],
                    index: 0,
                },
                ALWAYS_FULL,
                multi.get_outputs_mut(0),
            )
        })
        .chain(
            assembler_store
                .assemblers_1_1
                .iter_mut()
                .enumerate()
                .flat_map(move |(recipe_id_1_1, multi)| {
                    let item_in = data_store.recipe_to_items[&multi.recipe]
                        .iter()
                        .filter_map(|(dir, item)| {
                            if *dir == ItemRecipeDir::Ing {
                                Some(item)
                            } else {
                                None
                            }
                        })
                        .nth(0)
                        .copied()
                        .unwrap();

                    let item_out = data_store.recipe_to_items[&multi.recipe]
                        .iter()
                        .filter_map(|(dir, item)| {
                            if *dir == ItemRecipeDir::Out {
                                Some(item)
                            } else {
                                None
                            }
                        })
                        .nth(0)
                        .copied()
                        .unwrap();

                    let (([ings_max_insert], [ings]), [outputs]) = multi.get_all_mut();

                    [
                        (
                            item_in,
                            Storage::Assembler {
                                grid,
                                recipe_idx_with_this_item: data_store.recipe_to_translated_index[&(
                                    data_store.ing_out_num_to_recipe[&(1, 1)][recipe_id_1_1],
                                    item_in,
                                )],
                                index: 0,
                            },
                            ings_max_insert,
                            ings,
                        ),
                        (
                            item_out,
                            Storage::Assembler {
                                grid,
                                recipe_idx_with_this_item: data_store.recipe_to_translated_index[&(
                                    data_store.ing_out_num_to_recipe[&(1, 1)][recipe_id_1_1],
                                    item_out,
                                )],
                                index: 0,
                            },
                            ALWAYS_FULL,
                            outputs,
                        ),
                    ]
                }),
        )
        .chain(
            assembler_store
                .assemblers_2_1
                .iter_mut()
                .enumerate()
                .flat_map(move |(recipe_id_2_1, multi)| {
                    let mut items_in = data_store.recipe_to_items[&multi.recipe].iter().filter_map(
                        |(dir, item)| {
                            if *dir == ItemRecipeDir::Ing {
                                Some(item)
                            } else {
                                None
                            }
                        },
                    );

                    let item_in0 = *items_in.next().unwrap();
                    let item_in1 = *items_in.next().unwrap();

                    let item_out = data_store.recipe_to_items[&multi.recipe]
                        .iter()
                        .filter_map(|(dir, item)| {
                            if *dir == ItemRecipeDir::Out {
                                Some(item)
                            } else {
                                None
                            }
                        })
                        .nth(0)
                        .copied()
                        .unwrap();

                    let (([ings0_max, ings1_max], [ings0, ings1]), [outputs]) = multi.get_all_mut();

                    [
                        (
                            item_in0,
                            Storage::Assembler {
                                grid,
                                recipe_idx_with_this_item: data_store.recipe_to_translated_index[&(
                                    data_store.ing_out_num_to_recipe[&(2, 1)][recipe_id_2_1],
                                    item_in0,
                                )],
                                index: 0,
                            },
                            ings0_max,
                            ings0,
                        ),
                        (
                            item_in1,
                            Storage::Assembler {
                                grid,
                                recipe_idx_with_this_item: data_store.recipe_to_translated_index[&(
                                    data_store.ing_out_num_to_recipe[&(2, 1)][recipe_id_2_1],
                                    item_in1,
                                )],
                                index: 0,
                            },
                            ings1_max,
                            ings1,
                        ),
                        (
                            item_out,
                            Storage::Assembler {
                                grid,
                                recipe_idx_with_this_item: data_store.recipe_to_translated_index[&(
                                    data_store.ing_out_num_to_recipe[&(2, 1)][recipe_id_2_1],
                                    item_out,
                                )],
                                index: 0,
                            },
                            ALWAYS_FULL,
                            outputs,
                        ),
                    ]
                }),
        )
        .chain(
            assembler_store
                .assemblers_3_1
                .iter_mut()
                .enumerate()
                .flat_map(move |(recipe_id_3_1, multi)| {
                    let mut items_in = data_store.recipe_to_items[&multi.recipe].iter().filter_map(
                        |(dir, item)| {
                            if *dir == ItemRecipeDir::Ing {
                                Some(item)
                            } else {
                                None
                            }
                        },
                    );

                    let item_in0 = *items_in.next().unwrap();
                    let item_in1 = *items_in.next().unwrap();
                    let item_in2 = *items_in.next().unwrap();

                    let item_out = data_store.recipe_to_items[&multi.recipe]
                        .iter()
                        .filter_map(|(dir, item)| {
                            if *dir == ItemRecipeDir::Out {
                                Some(item)
                            } else {
                                None
                            }
                        })
                        .nth(0)
                        .copied()
                        .unwrap();

                    let (([ings0_max, ings1_max, ings2_max], [ings0, ings1, ings2]), [outputs]) =
                        multi.get_all_mut();

                    [
                        (
                            item_in0,
                            Storage::Assembler {
                                grid,
                                recipe_idx_with_this_item: data_store.recipe_to_translated_index[&(
                                    data_store.ing_out_num_to_recipe[&(3, 1)][recipe_id_3_1],
                                    item_in0,
                                )],
                                index: 0,
                            },
                            ings0_max,
                            ings0,
                        ),
                        (
                            item_in1,
                            Storage::Assembler {
                                grid,
                                recipe_idx_with_this_item: data_store.recipe_to_translated_index[&(
                                    data_store.ing_out_num_to_recipe[&(3, 1)][recipe_id_3_1],
                                    item_in1,
                                )],
                                index: 0,
                            },
                            ings1_max,
                            ings1,
                        ),
                        (
                            item_in2,
                            Storage::Assembler {
                                grid,
                                recipe_idx_with_this_item: data_store.recipe_to_translated_index[&(
                                    data_store.ing_out_num_to_recipe[&(3, 1)][recipe_id_3_1],
                                    item_in2,
                                )],
                                index: 0,
                            },
                            ings2_max,
                            ings2,
                        ),
                        (
                            item_out,
                            Storage::Assembler {
                                grid,
                                recipe_idx_with_this_item: data_store.recipe_to_translated_index[&(
                                    data_store.ing_out_num_to_recipe[&(3, 1)][recipe_id_3_1],
                                    item_out,
                                )],
                                index: 0,
                            },
                            ALWAYS_FULL,
                            outputs,
                        ),
                    ]
                }),
        )
        .chain(
            assembler_store
                .assemblers_4_1
                .iter_mut()
                .enumerate()
                .flat_map(move |(recipe_id_4_1, multi)| {
                    let mut items_in = data_store.recipe_to_items[&multi.recipe].iter().filter_map(
                        |(dir, item)| {
                            if *dir == ItemRecipeDir::Ing {
                                Some(item)
                            } else {
                                None
                            }
                        },
                    );

                    let item_in0 = *items_in.next().unwrap();
                    let item_in1 = *items_in.next().unwrap();
                    let item_in2 = *items_in.next().unwrap();
                    let item_in3 = *items_in.next().unwrap();

                    let item_out = data_store.recipe_to_items[&multi.recipe]
                        .iter()
                        .filter_map(|(dir, item)| {
                            if *dir == ItemRecipeDir::Out {
                                Some(item)
                            } else {
                                None
                            }
                        })
                        .nth(0)
                        .copied()
                        .unwrap();

                    let (
                        (
                            [ings0_max, ings1_max, ings2_max, ings3_max],
                            [ings0, ings1, ings2, ings3],
                        ),
                        [outputs],
                    ) = multi.get_all_mut();

                    [
                        (
                            item_in0,
                            Storage::Assembler {
                                grid,
                                recipe_idx_with_this_item: data_store.recipe_to_translated_index[&(
                                    data_store.ing_out_num_to_recipe[&(4, 1)][recipe_id_4_1],
                                    item_in0,
                                )],
                                index: 0,
                            },
                            ings0_max,
                            ings0,
                        ),
                        (
                            item_in1,
                            Storage::Assembler {
                                grid,
                                recipe_idx_with_this_item: data_store.recipe_to_translated_index[&(
                                    data_store.ing_out_num_to_recipe[&(4, 1)][recipe_id_4_1],
                                    item_in1,
                                )],
                                index: 0,
                            },
                            ings1_max,
                            ings1,
                        ),
                        (
                            item_in2,
                            Storage::Assembler {
                                grid,
                                recipe_idx_with_this_item: data_store.recipe_to_translated_index[&(
                                    data_store.ing_out_num_to_recipe[&(4, 1)][recipe_id_4_1],
                                    item_in2,
                                )],
                                index: 0,
                            },
                            ings2_max,
                            ings2,
                        ),
                        (
                            item_in3,
                            Storage::Assembler {
                                grid,
                                recipe_idx_with_this_item: data_store.recipe_to_translated_index[&(
                                    data_store.ing_out_num_to_recipe[&(4, 1)][recipe_id_4_1],
                                    item_in3,
                                )],
                                index: 0,
                            },
                            ings3_max,
                            ings3,
                        ),
                        (
                            item_out,
                            Storage::Assembler {
                                grid,
                                recipe_idx_with_this_item: data_store.recipe_to_translated_index[&(
                                    data_store.ing_out_num_to_recipe[&(4, 1)][recipe_id_4_1],
                                    item_out,
                                )],
                                index: 0,
                            },
                            ALWAYS_FULL,
                            outputs,
                        ),
                    ]
                }),
        );
    i
}

#[profiling::function]
fn all_lab_storages<'a, 'b, ItemIdxType: IdxTrait, RecipeIdxType: IdxTrait>(
    grid: PowerGridIdentifier,
    lab_store: &'a mut MultiLabStore,
    data_store: &'b DataStore<ItemIdxType, RecipeIdxType>,
) -> impl IntoIterator<
    Item = (
        Item<ItemIdxType>,
        Storage<RecipeIdxType>,
        &'a [ITEMCOUNTTYPE],
        &'a mut [ITEMCOUNTTYPE],
    ),
> + use<'a, 'b, ItemIdxType, RecipeIdxType> {
    lab_store
        .sciences
        .iter_mut()
        .zip(lab_store.max_insert.iter())
        .zip(data_store.science_bottle_items.iter().copied())
        .map(move |((science, max_insert), item)| {
            (
                item,
                Storage::Lab { grid, index: 0 },
                max_insert.as_slice(),
                science.as_mut_slice(),
            )
        })
}

#[profiling::function]
fn all_lazy_power_machine_storages() {}

#[profiling::function]
fn all_static_storages<'a, 'b, ItemIdxType: IdxTrait, RecipeIdxType: IdxTrait>(
    chest_store: &'a mut FullChestStore<ItemIdxType>,
    data_store: &'b DataStore<ItemIdxType, RecipeIdxType>,
) -> impl IntoIterator<
    Item = (
        Item<ItemIdxType>,
        Storage<RecipeIdxType>,
        &'a [ITEMCOUNTTYPE],
        &'a mut [ITEMCOUNTTYPE],
    ),
> + use<'a, 'b, ItemIdxType, RecipeIdxType> {
    (0..data_store.item_names.len())
        .zip(chest_store.stores.iter_mut())
        .flat_map(|(id, chest)| {
            let item = Item {
                id: id.try_into().unwrap(),
            };

            let grid_size = grid_size(item, data_store);
            let static_size = static_size(item, data_store);

            let first_grid_offs_in_grids = static_size.div_ceil(grid_size);

            let first_grid_offs = grid_size * first_grid_offs_in_grids;

            assert!(first_grid_offs >= static_size);
            assert!(first_grid_offs % grid_size == 0);

            let (max_insert, data) = chest.storage_list_slices();

            std::iter::repeat(item)
                .zip(
                    std::iter::once((
                        Storage::Static {
                            static_id: crate::inserter::StaticID::Chest as u16,
                            index: 0,
                        },
                        max_insert,
                        data,
                    ))
                    .chain(
                        std::iter::repeat_with(|| (PANIC_ON_INSERT, [].as_mut_slice()))
                            .zip(StaticID::iter().count()..)
                            .map(|((max, data), static_id)| {
                                (
                                    Storage::Static {
                                        static_id: static_id.try_into().unwrap(),
                                        index: 0,
                                    },
                                    max,
                                    data,
                                )
                            }),
                    )
                    .take(first_grid_offs),
                )
                .map(|(item, (a, b, c))| (item, a, b, c))
        })
}

#[profiling::function]
fn all_chest_storages<'a, ItemIdxType: IdxTrait, RecipeIdxType: IdxTrait>(
    chest_store: &'a mut FullChestStore<ItemIdxType>,
) -> impl IntoIterator<
    Item = (
        Item<ItemIdxType>,
        Storage<RecipeIdxType>,
        &'a [ITEMCOUNTTYPE],
        &'a mut [ITEMCOUNTTYPE],
    ),
> + use<'a, ItemIdxType, RecipeIdxType> {
    chest_store
        .stores
        .iter_mut()
        .enumerate()
        .map(|(id, multi)| {
            let item = Item {
                id: id.try_into().unwrap(),
            };

            let (max_insert, data) = multi.storage_list_slices();

            (
                item,
                Storage::Static {
                    static_id: crate::inserter::StaticID::Chest as u16,
                    index: 0,
                },
                max_insert,
                data,
            )
        })
}

fn bot_network_storages<ItemIdxType: IdxTrait>(
) -> impl IntoIterator<Item = (Item<ItemIdxType>, &'static mut [ITEMCOUNTTYPE])> {
    vec![todo!()]
}

fn all_train_station_storages() {}
