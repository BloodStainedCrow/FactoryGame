use crate::{
    frontend::world::Position,
    item::{IdxTrait, Recipe},
};

#[derive(Debug, serde::Serialize, serde::Deserialize)]
pub struct SetRecipeInfo<RecipeIdxType: IdxTrait> {
    pub pos: Position,
    pub recipe: Recipe<RecipeIdxType>,
}
