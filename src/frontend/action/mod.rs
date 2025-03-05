use place_entity::PlaceEntityInfo;
use place_tile::PlaceFloorTileByHandInfo;
use set_recipe::SetRecipeInfo;

use crate::item::IdxTrait;

use super::world::tile::World;

pub mod action_state_machine;
pub mod belt_placement;
pub mod place_entity;
pub mod place_tile;
pub mod set_recipe;

// TODO: Do I want actions to also encode the previous state in some capacity to make discarding nonsensical actions easier with network delays?
#[derive(Debug, Clone, serde::Serialize, serde::Deserialize)]
pub enum ActionType<ItemIdxType: IdxTrait, RecipeIdxType: IdxTrait> {
    PlaceFloorTile(PlaceFloorTileByHandInfo),
    PlaceEntity(PlaceEntityInfo<ItemIdxType>),

    SetRecipe(SetRecipeInfo<RecipeIdxType>),

    Moving((i8, i8)),

    Ping((u64, u64)),
}

// This is not meant as an anti-cheat or anything, just a sanity check to prevent accidentally breaking something because of network lag!
#[must_use]
pub fn still_valid<ItemIdxType: IdxTrait, RecipeIdxType: IdxTrait>(
    world: &World<ItemIdxType, RecipeIdxType>,
    action: &ActionType<ItemIdxType, RecipeIdxType>,
) -> bool {
    match action {
        ActionType::PlaceFloorTile(info) => todo!(),
        ActionType::PlaceEntity(info) => todo!(),
        ActionType::Moving(_) => todo!(),
        ActionType::SetRecipe(_) => todo!(),
        ActionType::Ping(_) => true,
    }
}
