use place_entity::PlaceEntityInfo;
use place_tile::PlaceFloorTileByHandInfo;
use set_recipe::SetRecipeInfo;

use crate::item::WeakIdxTrait;

use super::world::Position;

pub mod action_state_machine;
pub mod belt_placement;
pub mod place_entity;
pub mod place_tile;
pub mod set_recipe;

pub type PLAYERID = u16;

// TODO: Do I want actions to also encode the previous state in some capacity to make discarding nonsensical actions easier with network delays?
#[derive(Debug, Clone, serde::Serialize, serde::Deserialize)]
pub enum ActionType<ItemIdxType: WeakIdxTrait, RecipeIdxType: WeakIdxTrait> {
    PlaceFloorTile(PlaceFloorTileByHandInfo),
    PlaceEntity(PlaceEntityInfo<ItemIdxType>),

    SetRecipe(SetRecipeInfo<RecipeIdxType>),

    Position(PLAYERID, (f32, f32)),

    AddModules { pos: Position, modules: Vec<usize> },
    RemoveModules { pos: Position, indices: Vec<usize> },

    Remove(Position),

    Ping(Position),
}
