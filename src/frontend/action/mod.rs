use place_entity::PlaceEntityInfo;
use place_tile::PlaceFloorTileByHandInfo;

use super::world::tile::World;

pub mod action_state_machine;
pub mod place_entity;
pub mod place_tile;

// TODO: Do I want actions to also encode the previous state in some capacity to make discarding nonsensical actions easier with network delays?
#[derive(Debug, serde::Serialize, serde::Deserialize)]
pub enum ActionType {
    PlaceFloorTile(PlaceFloorTileByHandInfo),
    PlaceEntity(PlaceEntityInfo),

    Moving((i8, i8)),

    Ping((u64, u64)),
}

// This is not meant as an anti-cheat or anything, just a sanity check to prevent accidentally breaking something because of network lag!
#[must_use]
pub fn still_valid(world: &World, action: &ActionType) -> bool {
    match action {
        ActionType::PlaceFloorTile(info) => todo!(),
        ActionType::PlaceEntity(info) => todo!(),
        ActionType::Moving(_) => todo!(),
        ActionType::Ping(_) => true,
    }
}
