use crate::frontend::world::{tile::FloorTile, Position};

#[derive(Debug, Clone, serde::Serialize, serde::Deserialize)]
pub struct PlaceFloorTileByHandInfo {
    pub ghost_info: PlaceFloorTileGhostInfo,
    // TODO:
    pub player: (),
}

#[derive(Debug, Clone, serde::Serialize, serde::Deserialize)]
pub struct PlaceFloorTileGhostInfo {
    pub tile: FloorTile,
    pub position: PositionInfo,
}

#[derive(Debug, Clone, serde::Serialize, serde::Deserialize)]
pub enum PositionInfo {
    Rect {
        pos: Position,
        width: u32,
        height: u32,
    },
    Single {
        pos: Position,
    },
    List {
        positions: Vec<Position>,
    },
}
