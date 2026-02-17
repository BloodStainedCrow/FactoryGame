use egui::{ScrollArea, Window};

use crate::{
    TICKS_PER_SECOND_LOGIC,
    data::{DataStore, ItemRecipeDir},
    item::{IdxTrait, Indexable, Item, Recipe, WeakIdxTrait},
};

#[derive(Debug)]
pub(crate) struct Pedia<ItemIdxType: WeakIdxTrait, RecipeIdxType: WeakIdxTrait> {
    open_page: OpenPage,

    entry: Option<OpenEntry<ItemIdxType, RecipeIdxType>>,
}

#[derive(Debug)]
struct OpenPage {
    catagory: String,
}

#[derive(Debug)]
enum OpenEntry<ItemIdxType: WeakIdxTrait, RecipeIdxType: WeakIdxTrait> {
    Item { item: Item<ItemIdxType> },
    Recipe { recipe: Recipe<RecipeIdxType> },
}

impl<ItemIdxType: IdxTrait, RecipeIdxType: IdxTrait> Pedia<ItemIdxType, RecipeIdxType> {
    pub(crate) fn new(_data_store: &DataStore<ItemIdxType, RecipeIdxType>) -> Self {
        Self {
            open_page: OpenPage {
                catagory: "TODO".to_string(),
            },
            entry: Some(OpenEntry::Item {
                item: Item { id: 10.into() },
            }),
        }
    }

    pub(crate) fn show_window(
        &mut self,
        open: &mut bool,
        ctx: &egui::Context,
        data_store: &DataStore<ItemIdxType, RecipeIdxType>,
    ) {
        Window::new("Datapedia").open(open).show(ctx, |ui| {
            ui.columns_const(|[_grid, entry_ui]| {
                // Grid

                // Entry
                match &mut self.entry {
                    Some(entry) => Self::show_entry(entry, entry_ui, data_store),
                    None => {},
                }
            });
        });
    }

    fn show_entry(
        entry: &mut OpenEntry<ItemIdxType, RecipeIdxType>,
        ui: &mut egui::Ui,
        data_store: &DataStore<ItemIdxType, RecipeIdxType>,
    ) {
        ScrollArea::vertical().show(ui, |ui| {
            match entry {
                OpenEntry::Item { item: item_item } => {
                    let item = item_item.into_usize();
                    ui.heading(&data_store.item_display_names[item]);
                    if !data_store.item_is_fluid[item] {
                        ui.label(&format!(
                            "Stack Size: {}",
                            data_store.item_stack_sizes[item]
                        ));
                    }
                    // TODO: How do I want to find the main recipe?
                    let main_recipe = data_store
                        .recipe_names
                        .iter()
                        .position(|recipe_name| **recipe_name == *data_store.item_names[item]);

                    if let Some(main_recipe) = main_recipe {
                        let ingredients = data_store.recipe_to_items_and_amounts[main_recipe]
                            .iter()
                            .filter(|(dir, _item, _count)| *dir == ItemRecipeDir::Ing);
                        let results = data_store.recipe_to_items_and_amounts[main_recipe]
                            .iter()
                            .filter(|(dir, _item, _count)| *dir == ItemRecipeDir::Out);

                        let crafting_time = data_store.recipe_timers[main_recipe] as f32
                            / (TICKS_PER_SECOND_LOGIC as f32);

                        ui.label("Ingredients:");
                        for (_, ing, count) in ingredients {
                            if ui
                                .label(&format!(
                                    "{count}x: {}",
                                    data_store.item_display_names[ing.into_usize()]
                                ))
                                .clicked()
                            {
                                *item_item = *ing;
                            }
                        }
                        ui.label(&format!("{:.2}s Crafting time", crafting_time));
                        ui.label("Results:");
                        for (_, result, count) in results {
                            if ui
                                .label(&format!(
                                    "{count}x: {}",
                                    data_store.item_display_names[result.into_usize()]
                                ))
                                .clicked()
                            {
                                *item_item = *result;
                            }
                        }

                        let made_in = &data_store.recipe_allowed_assembling_machines[main_recipe];

                        ui.label("Made In:");
                        for machine in made_in {
                            let name = &data_store.assembler_info[*machine as usize].display_name;

                            if ui.label(name).clicked() {
                                todo!()
                            }
                        }

                        ui.separator();
                    }

                    let mut alternative_recipes = data_store
                        .recipe_names
                        .iter()
                        .enumerate()
                        .filter(|(_id, recipe_name)| ***recipe_name != *data_store.item_names[item])
                        .filter(|(id, _name)| {
                            data_store.recipe_to_items[*id]
                                .contains(&(ItemRecipeDir::Out, *item_item))
                        })
                        .peekable();

                    if alternative_recipes.peek().is_some() {
                        ui.label("Alternative Recipes:");
                        for (recipe_id, name) in alternative_recipes {
                            if ui
                                .label(&data_store.recipe_display_names[recipe_id])
                                .clicked()
                            {
                                if let Some(item) = data_store
                                    .item_names
                                    .iter()
                                    .position(|item_name| item_name == name)
                                {
                                    // Show the item
                                    *item_item = Item {
                                        id: ItemIdxType::try_from(item).unwrap(),
                                    };
                                    return;
                                } else {
                                    *entry = OpenEntry::Recipe {
                                        recipe: Recipe {
                                            id: RecipeIdxType::try_from(recipe_id).unwrap(),
                                        },
                                    };
                                    return;
                                }
                            }
                        }
                    }

                    let mut used_in = data_store
                        .recipe_names
                        .iter()
                        .enumerate()
                        .filter(|(id, _name)| {
                            data_store.recipe_to_items[*id]
                                .contains(&(ItemRecipeDir::Ing, *item_item))
                        })
                        .peekable();

                    if used_in.peek().is_some() {
                        ui.label("Used In:");
                        for (recipe_id, name) in used_in {
                            if ui
                                .label(&data_store.recipe_display_names[recipe_id])
                                .clicked()
                            {
                                if let Some(item) = data_store
                                    .item_names
                                    .iter()
                                    .position(|item_name| item_name == name)
                                {
                                    // Show the item
                                    *item_item = Item {
                                        id: ItemIdxType::try_from(item).unwrap(),
                                    };
                                    return;
                                } else {
                                    *entry = OpenEntry::Recipe {
                                        recipe: Recipe {
                                            id: RecipeIdxType::try_from(recipe_id).unwrap(),
                                        },
                                    };
                                    return;
                                }
                            }
                        }
                    }
                },
                OpenEntry::Recipe { recipe } => todo!(),
            }
        });
    }
}
