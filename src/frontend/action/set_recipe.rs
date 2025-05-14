use crate::{
    frontend::world::Position,
    item::{Recipe, WeakIdxTrait},
};

#[derive(Debug, Clone, serde::Serialize, serde::Deserialize)]
pub struct SetRecipeInfo<RecipeIdxType: WeakIdxTrait> {
    pub pos: Position,
    pub recipe: Recipe<RecipeIdxType>,
}
