use std::collections::BTreeMap;

use crate::api::{entity::EntityInfo, recipe::RecipeIdentifier};

#[derive(Debug, serde::Deserialize)]
pub struct AssemblerInfo {
    pub entity_info: EntityInfo,

    // TODO: This will be based on floortile
    pub default_recipe: Option<BTreeMap<(), RecipeIdentifier>>,
}
