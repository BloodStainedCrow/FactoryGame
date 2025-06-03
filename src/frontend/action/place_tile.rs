use crate::{
    frontend::world::{
        tile::{FloorTile, World},
        Position,
    },
    item::IdxTrait,
};

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

// TODO: Do not use usize for anything that might go to another machine, where it could be different size!
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

impl PlaceFloorTileByHandInfo {
    pub(super) fn still_valid<ItemIdxType: IdxTrait, RecipeIdxType: IdxTrait>(
        &self,
        world: &World<ItemIdxType, RecipeIdxType>,
    ) -> bool {
        if !self.ghost_info.still_valid(world) {
            return false;
        }

        // Check the inventory requirementsa when placing from hand
        match self.ghost_info.tile {
            FloorTile::Empty => true,
            FloorTile::Concrete => {
                todo!()
            },
            FloorTile::Water => true,
        }
    }
}

impl PlaceFloorTileGhostInfo {
    fn still_valid<ItemIdxType: IdxTrait, RecipeIdxType: IdxTrait>(
        &self,
        world: &World<ItemIdxType, RecipeIdxType>,
    ) -> bool {
        match self.tile {
            FloorTile::Empty => {
                unreachable!("Invalid action, trying to place \"Empty Tile\": {:?}", self)
            },
            FloorTile::Concrete => {
                todo!()
            },
            FloorTile::Water => {
                unreachable!("Invalid action, trying to place \"Water\": {:?}", self)
            },
        }
    }
}
