use std::{collections::HashMap, sync::Arc};

use crate::{ModIdentifier, api::item::ItemIdentifier, item::ItemCountType};

#[derive(Debug, Clone, PartialEq, Eq, Hash, serde::Deserialize)]
pub struct RecipeIdentifier(Arc<str>);

#[derive(Debug, Clone, serde::Deserialize)]
pub struct RecipeName(pub(crate) String);

impl RecipeIdentifier {
    pub(crate) fn new(mod_: &ModIdentifier, name: &RecipeName) -> Self {
        Self(format!("{}::{}", mod_.0, name.0).into())
    }

    // TODO
    #[must_use]
    pub fn new_raw(full_name: String) -> Self {
        Self(full_name.into())
    }
}

#[derive(Debug, Clone, serde::Deserialize)]
pub struct RecipeInfo {
    pub name: RecipeName,

    pub ingredients: HashMap<ItemIdentifier, ItemCountType>,
    pub results: HashMap<ItemIdentifier, ItemCountType>,
}
