use std::sync::Arc;

use crate::{DATA_STORE, ModIdentifier, entity::assember::Recipe, item::item_set::ItemSet};

#[derive(Debug, Clone, PartialEq, Eq, Hash, serde::Deserialize)]
pub struct RecipeIdentifier(Arc<str>);

#[derive(Debug, Clone, serde::Deserialize)]
struct RecipeName(String);

impl RecipeIdentifier {
    fn new(mod_: &ModIdentifier, name: &RecipeName) -> Self {
        Self(format!("{}::{}", mod_.0, name.0).into())
    }

    // TODO
    #[must_use]
    pub fn new_raw(full_name: String) -> Self {
        Self(full_name.into())
    }
}

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
