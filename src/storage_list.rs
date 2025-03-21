use itertools::Itertools;
use rayon::iter::{IndexedParallelIterator, ParallelBridge};

use crate::{
    assembler::FullAssemblerStore,
    chest::FullChestStore,
    data::{DataStore, ItemRecipeDir},
    inserter::Storage,
    item::{usize_from, IdxTrait, Item, Recipe, ITEMCOUNTTYPE},
    lab::MultiLabStore,
    power::{power_grid::PowerGridIdentifier, PowerGridStorage},
    split_arbitrary::{split_arbitrary_mut, split_arbitrary_mut_slice},
};

const NUM_ITEMS: usize = 0;
static NUM_GRIDS: usize = 0;
const NUM_RECIPES: usize = 0;
static NUM_MACHINES_OF_RECIPE: usize = 0;
static NUMBER_OF_CHESTS: usize = 0;
static NUMBER_OF_BOT_NETWORKS: usize = 0;

type ItemSlot = ITEMCOUNTTYPE;
type SingleGridStorage<'a> = &'a mut [ITEMCOUNTTYPE];
pub type SingleItemStorages<'a, 'b> = &'a mut [SingleGridStorage<'b>]; //[SingleGridStorage; NUM_RECIPES * NUM_GRIDS];
type FullStorages<'a> = Box<[SingleGridStorage<'a>]>; //[SingleGridStorage; NUM_ITEMS * NUM_RECIPES * NUM_GRIDS];

type ChestStorages<'a> = &'a mut [ITEMCOUNTTYPE; NUMBER_OF_CHESTS];
/// Provider, Requester, Storage
const NUMBER_OF_DIFFERENT_CHEST_TYPES: usize = 3;
type SingleBotNetworkChestStorage<'a> =
    [&'a mut [ITEMCOUNTTYPE; NUMBER_OF_CHESTS]; NUMBER_OF_DIFFERENT_CHEST_TYPES];
type BotNetworkStorages<'a> = [SingleBotNetworkChestStorage<'a>; NUMBER_OF_BOT_NETWORKS];

// Ideally we could have Box<[SingleItem; NUM_ITEMS]>
//          SingleItem = Box<[&mut [ITEMCOUNTTYPE]]>

fn num_labs<ItemIdxType: IdxTrait, RecipeIdxType: IdxTrait>(
    item: Item<ItemIdxType>,
    data_store: &DataStore<ItemIdxType, RecipeIdxType>,
) -> usize {
    data_store.lab_infos.len() * usize::from(data_store.item_is_science[usize_from(item.id)])
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
    num_grids_total * grid_size + num_different_static_containers
}

pub fn index<'a, 'b, RecipeIdxType: IdxTrait>(
    slice: SingleItemStorages<'a, 'b>,
    storage_id: Storage<RecipeIdxType>,
    num_grids_total: usize,
    num_recipes: usize,
    grid_size: usize,
) -> &'a mut ITEMCOUNTTYPE {
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
            &mut slice[Into::<usize>::into(grid) * grid_size
                + Into::<usize>::into(recipe_idx_with_this_item)][Into::<usize>::into(index)]
        },
        Storage::Lab { grid, index } => {
            &mut slice[Into::<usize>::into(grid) * grid_size + num_recipes]
                [Into::<usize>::into(index)]
        },
        Storage::Static { static_id, index } => {
            // debug_assert!(usize::from(static_id) < data_store.num_different_static_containers);
            &mut slice[num_grids_total * grid_size + Into::<usize>::into(static_id as u8)]
                [Into::<usize>::into(index)]
        },
    }
}

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

pub fn full_to_by_item<'a, 'b>(
    storages: &'a mut FullStorages<'a>,
    sizes: &'b [usize],
) -> impl IntoIterator<Item = SingleItemStorages<'a, 'a>>
       + use<'a, 'b>
       + IndexedParallelIterator<Item = SingleItemStorages<'a, 'a>> {
    split_arbitrary_mut_slice(storages, &sizes)
}

fn get_full_storage_index<ItemIdxType: IdxTrait, RecipeIdxType: IdxTrait>(
    item: Item<ItemIdxType>,
    storage: Storage<RecipeIdxType>,
    num_grids_total: usize,
    data_store: &DataStore<ItemIdxType, RecipeIdxType>,
) -> usize {
    // FIXME: Recalculating this repeatedly is bad
    let item_offs: usize = (0..item.id.into())
        .map(|id| Item {
            id: ItemIdxType::try_from(id).unwrap(),
        })
        .map(|item| size_of_single_item_slice(item, num_grids_total, data_store))
        .sum();

    let num_recipes = num_recipes(item, data_store);
    let num_labs = num_labs(item, data_store);
    let grid_size = grid_size(item, data_store);

    match storage {
        Storage::Assembler {
            grid,
            recipe_idx_with_this_item: recipe,
            index,
        } => item_offs + usize::from(grid) * grid_size + usize_from(recipe),
        Storage::Lab { grid, index } => item_offs + usize::from(grid) * grid_size + num_recipes,
        Storage::Static { static_id, index } => {
            item_offs + num_grids_total * grid_size + usize::from(static_id as u8)
        },
    }
}

pub fn storages_by_item<'a, ItemIdxType: IdxTrait, RecipeIdxType: IdxTrait>(
    grids: &'a mut PowerGridStorage<RecipeIdxType>,
    chest_store: &'a mut FullChestStore<ItemIdxType>,
    data_store: &DataStore<ItemIdxType, RecipeIdxType>,
) -> FullStorages<'a> {
    let num_grids_total = grids.power_grids.iter().flatten().count();

    #[cfg(debug_assertions)]
    {
        all_storages(grids, chest_store, data_store)
            .into_iter()
            .map(|v| get_full_storage_index(v.0, v.1, num_grids_total, data_store))
            .all_unique();
        let max_index = all_storages(grids, chest_store, data_store)
            .into_iter()
            .map(|v| get_full_storage_index(v.0, v.1, num_grids_total, data_store))
            .max();

        match max_index {
            Some(max_index) => {
                assert_eq!(
                    max_index,
                    all_storages(grids, chest_store, data_store)
                        .into_iter()
                        .count()
                        - 1
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

    let all_storages_sorted = all_storages
        .into_iter()
        .sorted_unstable_by_key(|v| get_full_storage_index(v.0, v.1, num_grids_total, data_store))
        .map(|v| v.2)
        .collect();
    all_storages_sorted
}

fn all_storages<'a, 'b, ItemIdxType: IdxTrait, RecipeIdxType: IdxTrait>(
    grids: &'a mut PowerGridStorage<RecipeIdxType>,
    chest_store: &'a mut FullChestStore<ItemIdxType>,
    data_store: &'b DataStore<ItemIdxType, RecipeIdxType>,
) -> impl IntoIterator<
    Item = (
        Item<ItemIdxType>,
        Storage<RecipeIdxType>,
        &'a mut [ITEMCOUNTTYPE],
    ),
> + use<'a, 'b, ItemIdxType, RecipeIdxType> {
    let all_storages = grids
        .power_grids
        .iter_mut()
        .enumerate()
        .filter_map(|(i, o)| o.as_mut().map(|grid| (i, grid)))
        .flat_map(|(grid_id, grid)| {
            let grid_id = grid_id.try_into().unwrap();
            all_assembler_storages(grid_id, &mut grid.stores, data_store)
                .into_iter()
                .chain(all_lab_storages(grid_id, &mut grid.lab_stores, data_store))
        })
        .chain(all_chest_storages(chest_store));
    all_storages
}
fn all_assembler_storages<'a, 'b, ItemIdxType: IdxTrait, RecipeIdxType: IdxTrait>(
    grid: PowerGridIdentifier,
    assembler_store: &'a mut FullAssemblerStore<RecipeIdxType>,
    data_store: &'b DataStore<ItemIdxType, RecipeIdxType>,
) -> impl IntoIterator<
    Item = (
        Item<ItemIdxType>,
        Storage<RecipeIdxType>,
        &'a mut [ITEMCOUNTTYPE],
    ),
> + use<'a, 'b, ItemIdxType, RecipeIdxType> {
    let i =
        assembler_store
            .assemblers_0_1
            .iter_mut()
            .enumerate()
            .map(move |(recipe_id_0_1, multi)| {
                (
                    data_store.recipe_to_items[&multi.recipe]
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
                        .unwrap(),
                    Storage::Assembler {
                        grid,
                        recipe_idx_with_this_item: RecipeIdxType::try_from(recipe_id_0_1).unwrap(),
                        index: 0,
                    },
                    multi.get_outputs_mut(0),
                )
            });
    i
}

fn all_lab_storages<'a, 'b, ItemIdxType: IdxTrait, RecipeIdxType: IdxTrait>(
    grid: PowerGridIdentifier,
    lab_store: &'a mut MultiLabStore,
    data_store: &'b DataStore<ItemIdxType, RecipeIdxType>,
) -> impl IntoIterator<
    Item = (
        Item<ItemIdxType>,
        Storage<RecipeIdxType>,
        &'a mut [ITEMCOUNTTYPE],
    ),
> + use<'a, 'b, ItemIdxType, RecipeIdxType> {
    // TODO:
    vec![]
}

fn all_lazy_power_machine_storages() {}

fn all_chest_storages<'a, ItemIdxType: IdxTrait, RecipeIdxType: IdxTrait>(
    chest_store: &'a mut FullChestStore<ItemIdxType>,
) -> impl IntoIterator<
    Item = (
        Item<ItemIdxType>,
        Storage<RecipeIdxType>,
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
            (
                item,
                Storage::Static {
                    static_id: crate::inserter::StaticID::Chest,
                    index: 0,
                },
                multi.inout.as_mut_slice(),
            )
        })
}

fn bot_network_storages<ItemIdxType: IdxTrait>(
) -> impl IntoIterator<Item = (Item<ItemIdxType>, &'static mut [ITEMCOUNTTYPE])> {
    vec![todo!()]
}

fn all_train_station_storages() {}
