use std::collections::{HashMap, HashSet};

use enum_map::Enum;
use log::warn;

use crate::{
    item::{IdxTrait, Item, Recipe},
    power::PowerGridIdentifier,
};

use super::{sparse_grid::SparseGrid, Position};

pub const BELT_LEN_PER_TILE: u16 = 4;

#[derive(Debug, Clone, Copy, serde::Serialize, serde::Deserialize)]
pub enum FloorTile {
    Empty,
    Concrete,
    Water,
}

#[derive(Debug, serde::Deserialize, serde::Serialize)]
pub struct Chunk<ItemIdxType: IdxTrait, RecipeIdxType: IdxTrait> {
    pub floor_tiles: [[FloorTile; 16]; 16],
    entities: Vec<Entity<ItemIdxType, RecipeIdxType>>,

    belt_exit_chunks: HashMap<BeltTileId<ItemIdxType>, HashSet<(usize, usize)>>,
}

#[derive(Debug, serde::Deserialize, serde::Serialize)]
pub struct BeltInfo<ItemIdxType: IdxTrait> {
    pub id: BeltId<ItemIdxType>,
    // prev_chunk: Option<Dir>,
    // next_chunk: Option<Dir>,
}

#[derive(Debug, serde::Deserialize, serde::Serialize)]
pub struct World<ItemIdxType: IdxTrait, RecipeIdxType: IdxTrait> {
    // TODO: I don´t think I want FP
    pub player_pos: (f32, f32),
    pub player_move: (f32, f32),
    pub chunks: SparseGrid<Chunk<ItemIdxType, RecipeIdxType>>,
}

impl<ItemIdxType: IdxTrait, RecipeIdxType: IdxTrait> World<ItemIdxType, RecipeIdxType> {
    #[must_use]
    pub fn new() -> Self {
        let mut grid = SparseGrid::new(1_000_000, 1_000_000);

        grid.insert(
            100,
            100,
            Chunk {
                floor_tiles: [[FloorTile::Empty; 16]; 16],
                entities: vec![],
                belt_exit_chunks: HashMap::new(),
            },
        );
        grid.insert(
            99,
            100,
            Chunk {
                floor_tiles: [[FloorTile::Empty; 16]; 16],
                entities: vec![],
                belt_exit_chunks: HashMap::new(),
            },
        );
        grid.insert(
            100,
            99,
            Chunk {
                floor_tiles: [[FloorTile::Empty; 16]; 16],
                entities: vec![],
                belt_exit_chunks: HashMap::new(),
            },
        );
        grid.insert(
            101,
            100,
            Chunk {
                floor_tiles: [[FloorTile::Empty; 16]; 16],
                entities: vec![],
                belt_exit_chunks: HashMap::new(),
            },
        );
        grid.insert(
            100,
            101,
            Chunk {
                floor_tiles: [[FloorTile::Empty; 16]; 16],
                entities: vec![],
                belt_exit_chunks: HashMap::new(),
            },
        );

        Self {
            chunks: grid,
            player_pos: (1600.0, 1600.0),
            player_move: (0.0, 0.0),
        }
    }

    #[must_use]
    pub fn get_chunk_for_tile(&self, pos: Position) -> Option<&Chunk<ItemIdxType, RecipeIdxType>> {
        self.chunks.get(pos.x / 16, pos.y / 16)
    }

    pub fn get_chunk_for_tile_mut(
        &mut self,
        pos: Position,
    ) -> Option<&mut Chunk<ItemIdxType, RecipeIdxType>> {
        self.chunks.get_mut(pos.x / 16, pos.y / 16)
    }

    pub fn get_chunk_mut(
        &mut self,
        chunk: (usize, usize),
    ) -> Option<&mut Chunk<ItemIdxType, RecipeIdxType>> {
        self.chunks.get_mut(chunk.0, chunk.1)
    }

    #[must_use]
    pub fn can_fit(&self, pos: Position, size: (u8, u8)) -> bool {
        // Check all corners
        let Some(top_left) = self.get_chunk_for_tile(pos) else {
            warn!("Bounds check with nonexisting chunk");
            return false;
        };

        let Some(top_right) = self.get_chunk_for_tile(Position {
            x: pos.x,
            y: pos.y + usize::from(size.1),
        }) else {
            warn!("Bounds check with nonexisting chunk");
            return false;
        };

        let Some(bottom_left) = self.get_chunk_for_tile(Position {
            x: pos.x + usize::from(size.0),
            y: pos.y,
        }) else {
            warn!("Bounds check with nonexisting chunk");
            return false;
        };

        let Some(bottom_right) = self.get_chunk_for_tile(Position {
            x: pos.x + usize::from(size.0),
            y: pos.y + usize::from(size.1),
        }) else {
            warn!("Bounds check with nonexisting chunk");
            return false;
        };

        top_left.can_fit(pos, size)
            && top_right.can_fit(pos, size)
            && bottom_left.can_fit(pos, size)
            && bottom_right.can_fit(pos, size)
    }
}

impl<ItemIdxType: IdxTrait, RecipeIdxType: IdxTrait> Default for World<ItemIdxType, RecipeIdxType> {
    fn default() -> Self {
        Self::new()
    }
}

impl<ItemIdxType: IdxTrait, RecipeIdxType: IdxTrait> Chunk<ItemIdxType, RecipeIdxType> {
    #[must_use]
    pub fn get_entity_at(&self, pos: Position) -> Option<&Entity<ItemIdxType, RecipeIdxType>> {
        self.entities.iter().find(|e| {
            let e_pos = e.get_pos();
            let e_size = e.get_size();

            pos.x >= e_pos.x
                && pos.y >= e_pos.y
                && pos.x < e_pos.x + usize::from(e_size.0)
                && pos.y < e_pos.y + usize::from(e_size.1)
        })
    }

    pub fn get_entity_at_mut(
        &mut self,
        pos: Position,
    ) -> Option<&mut Entity<ItemIdxType, RecipeIdxType>> {
        self.entities.iter_mut().find(|e| {
            let e_pos = e.get_pos();
            let e_size = e.get_size();

            pos.x >= e_pos.x
                && pos.y >= e_pos.y
                && pos.x < e_pos.x + usize::from(e_size.0)
                && pos.y < e_pos.y + usize::from(e_size.1)
        })
    }

    pub fn increase_all_belt_pos(
        &mut self,
        belt_id: BeltTileId<ItemIdxType>,
        amount: u16,
    ) -> Option<impl IntoIterator<Item = &(usize, usize)>> {
        for entity in &mut self.entities {
            if let Entity::Belt { belt_pos, id, .. } = entity {
                if *id == belt_id {
                    *belt_pos += amount;
                }
            }
        }

        self.belt_exit_chunks.get(&belt_id)
    }

    pub fn change_belt_id(
        &mut self,
        old_belt_id: BeltTileId<ItemIdxType>,
        new_belt_id: BeltTileId<ItemIdxType>,
    ) -> Option<impl IntoIterator<Item = &(usize, usize)>> {
        for entity in &mut self.entities {
            if let Entity::Belt { id, .. } = entity {
                if *id == old_belt_id {
                    *id = new_belt_id;
                }
            }
        }

        let v = self
            .belt_exit_chunks
            .remove(&old_belt_id)
            .expect("Old belt not in map!");

        assert!(self.belt_exit_chunks.insert(new_belt_id, v).is_none());

        self.belt_exit_chunks.get(&new_belt_id)
    }

    #[must_use]
    fn can_fit(&self, pos: Position, size: (u8, u8)) -> bool {
        self.entities.iter().all(|e| {
            let e_pos = *e.get_pos();
            let e_size = e.get_size();

            (pos.x + usize::from(size.0)) <= e_pos.x
                || (pos.y + usize::from(size.1)) <= e_pos.y
                || (pos.x) >= (e_pos.x + usize::from(e_size.0))
                || (pos.y) >= (e_pos.y + usize::from(e_size.1))
        })
    }

    pub fn add_entity(&mut self, entity: Entity<ItemIdxType, RecipeIdxType>) {
        if let Entity::Belt {
            pos,
            direction,
            id,
            belt_pos: _belt_pos,
        } = entity
        {
            let v = self.belt_exit_chunks.entry(id).or_default();

            let my_chunk = (pos.x / 16, pos.y / 16);

            if (pos.x % 16)
                .abs_diff((pos.x.wrapping_add_signed(direction.into_offset().0.into())) % 16)
                > 1
            {
                v.insert((
                    my_chunk
                        .0
                        .wrapping_add_signed(direction.into_offset().0.into()),
                    my_chunk.1,
                ));
            }
            if (pos.y % 16)
                .abs_diff((pos.y.wrapping_add_signed(direction.into_offset().1.into())) % 16)
                > 1
            {
                v.insert((
                    my_chunk.0,
                    my_chunk
                        .1
                        .wrapping_add_signed(direction.into_offset().1.into()),
                ));
            }
            if (pos.x % 16).abs_diff(
                (pos.x
                    .wrapping_add_signed(direction.reverse().into_offset().0.into()))
                    % 16,
            ) > 1
            {
                v.insert((
                    my_chunk
                        .0
                        .wrapping_add_signed(direction.reverse().into_offset().0.into()),
                    my_chunk.1,
                ));
            }
            if (pos.y % 16).abs_diff(
                (pos.y
                    .wrapping_add_signed(direction.reverse().into_offset().1.into()))
                    % 16,
            ) > 1
            {
                v.insert((
                    my_chunk.0,
                    my_chunk
                        .1
                        .wrapping_add_signed(direction.reverse().into_offset().1.into()),
                ));
            }
        }

        self.entities.push(entity);
    }

    #[must_use]
    pub fn get_entities(&self) -> impl IntoIterator<Item = &Entity<ItemIdxType, RecipeIdxType>> {
        &self.entities
    }
}

#[derive(Debug, Clone, Copy, serde::Deserialize, serde::Serialize)]
pub enum Entity<ItemIdxType: IdxTrait, RecipeIdxType: IdxTrait> {
    AssemblerWithoutRecipe {
        pos: Position,
    },
    Assembler {
        pos: Position,
        id: AssemblerID<RecipeIdxType>,
    },
    PowerPole {
        pos: Position,
        id: PowerGridIdentifier,
    },
    Belt {
        pos: Position,
        direction: Dir,
        // TODO:
        id: BeltTileId<ItemIdxType>,
        belt_pos: u16,
    },
    Inserter {
        pos: Position,
        direction: Dir,

        id: BeltTileId<ItemIdxType>,
        belt_pos: u16,
    },
    UnconnectedInserter {
        pos: Position,
        direction: Dir,
    },
}

impl<ItemIdxType: IdxTrait, RecipeIdxType: IdxTrait> Entity<ItemIdxType, RecipeIdxType> {
    pub const fn get_pos(&self) -> &Position {
        match self {
            Self::AssemblerWithoutRecipe { pos } => pos,
            Self::Assembler { pos, .. } => pos,
            Self::PowerPole { pos, .. } => pos,
            Self::Belt { pos, .. } => pos,
            Self::Inserter { pos, .. } => pos,
            Self::UnconnectedInserter { pos, .. } => pos,
        }
    }

    pub const fn get_size(&self) -> (u8, u8) {
        match self {
            Self::AssemblerWithoutRecipe { pos } => (3, 3),
            Self::Assembler { pos, id } => (3, 3),
            Self::PowerPole { pos, id } => (1, 1),
            Self::Belt {
                pos,
                direction,
                id,
                belt_pos,
            } => (1, 1),
            Self::Inserter {
                pos,
                direction,
                id,
                belt_pos,
            } => (1, 1),
            Self::UnconnectedInserter { pos, direction } => (1, 1),
        }
    }
}

const TEST: usize = const { size_of::<Entity<u8, u8>>() };

#[derive(Debug, Clone, Copy, serde::Deserialize, serde::Serialize, PartialEq, Eq, Hash)]
pub enum BeltTileId<ItemIdxType: IdxTrait> {
    EmptyBeltId(usize),
    BeltId(BeltId<ItemIdxType>),
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, serde::Deserialize, serde::Serialize)]
pub struct BeltId<ItemIdxType: IdxTrait> {
    pub item: Item<ItemIdxType>,
    pub index: usize,
}

#[derive(Debug, Clone, Copy, serde::Serialize, serde::Deserialize)]
pub enum PlaceEntityType<ItemIdxType: IdxTrait> {
    Assembler(Position),
    Inserter {
        pos: Position,
        dir: Dir,
        /// The Item the inserter will move, must fit both the in and output side
        filter: Item<ItemIdxType>,
    },
    Belt {
        pos: Position,
        direction: Dir,
    },
}

#[derive(Debug, Clone, Copy, serde::Serialize, serde::Deserialize, PartialEq, Eq, Enum)]
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
    pub const fn turn_right(self) -> Self {
        match self {
            Self::North => Self::East,
            Self::East => Self::South,
            Self::South => Self::West,
            Self::West => Self::North,
        }
    }
}

#[derive(Debug, Clone, Copy, serde::Deserialize, serde::Serialize)]
pub struct AssemblerID<RecipeIdxType: IdxTrait> {
    pub recipe: Recipe<RecipeIdxType>,
    pub grid: PowerGridIdentifier,
    pub assembler_index: u16,
}
