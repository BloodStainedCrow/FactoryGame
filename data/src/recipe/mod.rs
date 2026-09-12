use crate::{
    DATA_STORE, api::recipe::RecipeIdentifier, entity::assember::Recipe, item::item_set::ItemSet,
};

#[derive(Debug)]
pub(crate) struct RecipeInfo {
    pub recipe_identifier: RecipeIdentifier,

    pub inputs: ItemSet,
    pub outputs: ItemSet,
}

#[must_use]
pub fn get_items_produced_by_recipe(recipe: Recipe) -> ItemSet {
    DATA_STORE.recipes[recipe.0 as usize].outputs.clone()
}

#[must_use]
pub fn get_items_consumed_by_recipe(recipe: Recipe) -> ItemSet {
    DATA_STORE.recipes[recipe.0 as usize].inputs.clone()
}
