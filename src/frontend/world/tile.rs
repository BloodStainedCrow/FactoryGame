use crate::{item::Item, power::PowerGridIdentifier};

use super::{sparse_grid::SparseGrid, Position};

pub const BELT_LEN_PER_TILE: u16 = 8;

#[derive(Debug, Clone, Copy, serde::Serialize, serde::Deserialize)]
pub enum FloorTile {
    Empty,
    Concrete,
    Water,
}

#[derive(Debug, serde::Deserialize, serde::Serialize)]
pub struct Chunk {
    pub floor_tiles: [[FloorTile; 16]; 16],
    pub entities: Vec<Entity>,
}

#[derive(Debug, serde::Deserialize, serde::Serialize)]
pub struct BeltInfo {
    pub id: BeltId,
    // prev_chunk: Option<Dir>,
    // next_chunk: Option<Dir>,
}

#[derive(Debug, serde::Deserialize, serde::Serialize)]
pub struct World {
    // TODO: I don´t think I want FP
    pub player_pos: (f32, f32),
    pub player_move: (f32, f32),
    pub chunks: SparseGrid<Chunk>,
}

impl World {
    #[must_use]
    pub fn new() -> Self {
        let mut grid = SparseGrid::new(1_000_000, 1_000_000);

        grid.insert(
            100,
            100,
            Chunk {
                floor_tiles: [[FloorTile::Empty; 16]; 16],
                entities: vec![],
            },
        );

        Self {
            chunks: grid,
            player_pos: (1600.0, 1600.0),
            player_move: (0.0, 0.0),
        }
    }

    pub fn get_chunk_for_tile(&self, pos: Position) -> Option<&Chunk> {
        self.chunks.get(pos.x / 16, pos.y / 16)
    }

    pub fn get_chunk_for_tile_mut(&mut self, pos: Position) -> Option<&mut Chunk> {
        self.chunks.get_mut(pos.x / 16, pos.y / 16)
    }
}

impl Default for World {
    fn default() -> Self {
        Self::new()
    }
}

impl Chunk {
    // TODO: Add bounding box checks for this search
    pub fn get_entity_at(&self, pos: Position) -> Option<&Entity> {
        let pos = Position {
            x: pos.x % 16,
            y: pos.y % 16,
        };
        self.entities.iter().find(|e| match e {
            Entity::Assembler { pos: e_pos, .. } => pos == *e_pos,
            Entity::PowerPole { pos: e_pos, .. } => pos == *e_pos,
            Entity::Belt { pos: e_pos, .. } => pos == *e_pos,
        })
    }

    // TODO: Add bounding box checks for this search
    pub fn get_entity_at_mut(&mut self, pos: Position) -> Option<&mut Entity> {
        let pos = Position {
            x: pos.x % 16,
            y: pos.y % 16,
        };
        self.entities.iter_mut().find(|e| match e {
            Entity::Assembler { pos: e_pos, .. } => pos == *e_pos,
            Entity::PowerPole { pos: e_pos, .. } => pos == *e_pos,
            Entity::Belt { pos: e_pos, .. } => pos == *e_pos,
        })
    }

    pub fn increase_all_belt_pos(&mut self, belt_id: BeltId, amount: usize) {
        for entity in &mut self.entities {
            if let Entity::Belt { belt_pos, id, .. } = entity {
                if *id == belt_id {
                    *belt_pos += amount;
                }
            }
        }
    }

    pub fn change_belt_id(&mut self, old_belt_id: BeltId, new_belt_id: BeltId) {
        for entity in &mut self.entities {
            if let Entity::Belt { id, .. } = entity {
                if *id == old_belt_id {
                    *id = new_belt_id;
                }
            }
        }
    }
}

#[derive(Debug, Clone, Copy, serde::Deserialize, serde::Serialize)]
pub enum Entity {
    Assembler {
        pos: Position,
        id: AssemblerID,
    },
    PowerPole {
        pos: Position,
        id: PowerGridIdentifier,
    },
    Belt {
        pos: Position,
        direction: Dir,
        // TODO:
        id: BeltId,
        belt_pos: usize,
    },
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, serde::Deserialize, serde::Serialize)]
pub struct BeltId {
    pub item: u16,
    pub index: usize,
}

#[derive(Debug, Clone, Copy, serde::Serialize, serde::Deserialize)]
pub enum PlaceEntityType {
    Assembler(Position),
    Inserter {
        /// Encodes the position of the belt/machine the inserter will take from
        start_pos: Position,
        /// Encodes the position of the belt/machine the inserter will put into
        end_pos: Position,
        /// The Item the inserter will move, must fit both the in and output side
        filter: Item,
    },
    Belt {
        pos: Position,
        direction: Dir,
    },
}

#[derive(Debug, Clone, Copy, serde::Serialize, serde::Deserialize, PartialEq, Eq)]
pub enum Dir {
    North,
    East,
    South,
    West,
}

impl Dir {
    #[must_use]
    pub const fn reverse(self) -> Self {
        match self {
            Self::North => Self::South,
            Self::East => Self::West,
            Self::South => Self::North,
            Self::West => Self::East,
        }
    }

    #[must_use]
    pub const fn into_offset(self) -> (i8, i8) {
        match self {
            Self::North => (0, -1),
            Self::East => (1, 0),
            Self::South => (0, 1),
            Self::West => (-1, 0),
        }
    }

    #[must_use]
    pub fn turn_right(self) -> Self {
        match self {
            Dir::North => Self::East,
            Dir::East => Self::South,
            Dir::South => Self::West,
            Dir::West => Self::North,
        }
    }
}

#[derive(Debug, Clone, Copy, serde::Deserialize, serde::Serialize)]
pub struct AssemblerID {
    pub recipe: u8,
    pub grid: PowerGridIdentifier,
    pub assembler_index: u16,
}
