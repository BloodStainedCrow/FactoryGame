use bitvec::{BitArr, bitarr};
use data::{
    entity::{GlobalTy, extent},
    get_kind,
    spacial::{BoundingBox, Extent, Flipped, Position, Rotation},
};
use middle_indices::{
    AssemblerMiddleID, BeltMiddleID, InserterMiddleID, PipeMiddleID, PowerPoleMiddleID,
};

use crate::entity::{EntityDescriptor, EntityDescriptorKind};

pub const CHUNK_SIZE: u8 = 16;

#[derive(Debug, Clone)]
pub struct Chunk {
    // NOTE(BSC): This has a maximumum length of CHUNK_SIZE * CHUNK_SIZE, which could be used to reduce the memory footprint
    entities: Vec<StoredEntity>,
    // TODO: Floor tiles

    // Solar Panel/Accumulator Optimization
    single_kind_optimization: Option<Box<SameEntityOptimization>>,
}

#[derive(Debug, Clone)]
struct SameEntityOptimization {
    ty: GlobalTy,
    rotation: RotationWithFlipped,
    spots: BitArr!(for usize::from(CHUNK_SIZE) * usize::from(CHUNK_SIZE), in usize),
}

impl Chunk {
    pub const fn empty() -> Self {
        Self {
            entities: vec![],
            single_kind_optimization: None,
        }
    }

    pub fn occupied_bounding_boxes(&self, base_pos: Position) -> impl Iterator<Item = BoundingBox> {
        self.single_kind_optimization
            .as_ref()
            .into_iter()
            .flat_map(move |same| {
                same.spots.iter_ones().map(move |index| {
                    let pos_in_chunk = PosInChunk(index.try_into().expect(
                        "The array is CHUNK_SIZE * CHUNK_SIZE long so this wil always fit",
                    ));

                    let (rotation, flipped) = same.rotation.into();

                    BoundingBox::new(
                        pos_in_chunk.into_real(base_pos),
                        extent(same.ty, rotation, flipped),
                    )
                })
            })
            .chain(self.entities.iter().map(move |entity| {
                BoundingBox::new(entity.pos.into_real(base_pos), entity.get_extent())
            }))
    }

    pub fn add_entity(&mut self, base_pos: Position, entity: EntityDescriptor) {
        let EntityDescriptor {
            position,
            rotation,
            flipped,
            ty,
            kind,
        } = entity;

        let pos =
            PosInChunk::try_from_real(base_pos, position).expect("Entity base pos not in chunk");

        let rotation = (rotation, flipped).into();

        match kind {
            EntityDescriptorKind::Assembler { id } => {
                self.entities.push(StoredEntity {
                    pos,
                    rotation,
                    ty,
                    index: id.0,
                });
            },
            EntityDescriptorKind::Inserter { id } => {
                self.entities.push(StoredEntity {
                    pos,
                    rotation,
                    ty,
                    index: id.0,
                });
            },

            EntityDescriptorKind::Belt { id } => {
                self.entities.push(StoredEntity {
                    pos,
                    rotation,
                    ty,
                    index: id.0,
                });
            },
            EntityDescriptorKind::Pipe { id } => {
                self.entities.push(StoredEntity {
                    pos,
                    rotation,
                    ty,
                    index: id.0,
                });
            },
            EntityDescriptorKind::PowerPole { id } => {
                self.entities.push(StoredEntity {
                    pos,
                    rotation,
                    ty,
                    index: id.0,
                });
            },
            EntityDescriptorKind::SolarPanel {} => {
                if let Some(single) = &mut self.single_kind_optimization {
                    // TODO: Check info
                    if single.rotation == rotation && single.ty == ty {
                        single.spots.set(usize::from(pos.0), true);
                    } else {
                        todo!("Undo SameEntityOptimization")
                    }
                } else {
                    let mut arr = bitarr!(0; usize::from(CHUNK_SIZE) * usize::from(CHUNK_SIZE));

                    arr.set(usize::from(pos.0), true);

                    self.single_kind_optimization = Some(Box::new(SameEntityOptimization {
                        ty,
                        rotation,
                        spots: arr,
                    }));
                }
            },
        }
    }

    /// `position` must be the base pos (i.e. top left corner) of the entity to be removed
    fn remove_entity(
        &mut self,
        base_pos: Position,
        position: Position,
    ) -> Option<EntityDescriptor> {
        let pos_in_chunk =
            PosInChunk::try_from_real(base_pos, position).expect("Position not in chunk");

        if let Some(single) = &mut self.single_kind_optimization {
            single.spots.set(usize::from(pos_in_chunk.0), false);
            let (rotation, flipped) = single.rotation.into();

            // TODO: Assert that the EntityDescriptorKind is correct
            return Some(EntityDescriptor {
                position: pos_in_chunk.into_real(base_pos),
                rotation,
                flipped,
                ty: single.ty,
                kind: EntityDescriptorKind::SolarPanel {},
            });
        }

        self.entities
            .extract_if(.., |e| e.pos == pos_in_chunk)
            .next()
            .map(|e| e.get_descriptor(base_pos))
    }

    /// The order is not specified!
    pub fn get_entities(&self, base_pos: Position) -> impl Iterator<Item = EntityDescriptor> {
        let iter = self
            .single_kind_optimization
            .as_ref()
            .into_iter()
            .flat_map(move |same| {
                same.spots.iter_ones().map(move |index| {
                    let pos_in_chunk = PosInChunk(index.try_into().expect(
                        "The array is CHUNK_SIZE * CHUNK_SIZE long so this will always fit",
                    ));

                    let (rotation, flipped) = same.rotation.into();

                    // TODO: Assert that the EntityDescriptorKind is correct

                    EntityDescriptor {
                        position: pos_in_chunk.into_real(base_pos),
                        rotation,
                        flipped,
                        ty: same.ty,
                        kind: EntityDescriptorKind::SolarPanel {},
                    }
                })
            })
            .chain(
                self.entities
                    .iter()
                    .copied()
                    .map(move |e| e.get_descriptor(base_pos)),
            );

        #[cfg(debug_assertions)]
        {
            use rand::seq::SliceRandom;
            // NOTE(BSC): The order stuff gets returned here should be entirely irrelevant and not be depended on.
            // One way to ensure this is to use shuffle to make this unpredictable which should lead to desyncs if anything depends on it.
            let mut v: Vec<_> = iter.collect();
            v.shuffle(&mut rand::rng());

            v.into_iter()
        }

        #[cfg(not(debug_assertions))]
        {
            iter
        }
    }

    pub fn get_power_pole_entities(
        &self,
        base_pos: Position,
    ) -> impl Iterator<Item = EntityDescriptor> {
        self.entities
            .iter()
            .filter(|e| matches!(get_kind(e.ty), data::EntityPrototypeKind::PowerPole))
            .copied()
            .map(move |e| e.get_descriptor(base_pos))
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
struct PosInChunk(u8);

impl PosInChunk {
    const fn into_real(self, base_pos: Position) -> Position {
        let x_offs = self.0 / CHUNK_SIZE;
        let y_offs = self.0 % CHUNK_SIZE;

        Position {
            x: base_pos.x + i32::from(x_offs),
            y: base_pos.y + i32::from(y_offs),
        }
    }

    const fn try_from_real(base_pos: Position, goal_pos: Position) -> Result<Self, ()> {
        let x_offs = goal_pos.x - base_pos.x;
        let y_offs = goal_pos.y - base_pos.y;

        let (x_offs, y_offs): (u8, u8) = match (x_offs.try_into(), y_offs.try_into()) {
            (Ok(x), Ok(y)) => {
                if x < CHUNK_SIZE && y < CHUNK_SIZE {
                    (x, y)
                } else {
                    return Err(());
                }
            },
            _ => return Err(()),
        };

        Ok(Self(x_offs * CHUNK_SIZE + y_offs))
    }
}

#[derive(Debug, Clone, Copy)]
struct StoredEntity {
    pos: PosInChunk,
    rotation: RotationWithFlipped,
    ty: GlobalTy,
    index: u32,
}

impl StoredEntity {
    fn get_extent(self) -> Extent {
        let (rotation, flipped) = self.rotation.into();
        extent(self.ty, rotation, flipped)
    }

    fn get_descriptor(self, base_pos: Position) -> EntityDescriptor {
        let (rotation, flipped) = self.rotation.into();

        let kind = match get_kind(self.ty) {
            data::EntityPrototypeKind::Assembler => EntityDescriptorKind::Assembler {
                id: AssemblerMiddleID(self.index),
            },
            data::EntityPrototypeKind::Inserter => EntityDescriptorKind::Inserter {
                id: InserterMiddleID(self.index),
            },
            data::EntityPrototypeKind::Belt => EntityDescriptorKind::Belt {
                id: BeltMiddleID(self.index),
            },
            data::EntityPrototypeKind::Pipe => EntityDescriptorKind::Pipe {
                id: PipeMiddleID(self.index),
            },
            data::EntityPrototypeKind::UndergroundBelt => todo!(),
            data::EntityPrototypeKind::Splitter => todo!(),
            data::EntityPrototypeKind::Chest => todo!(),
            data::EntityPrototypeKind::PowerPole => EntityDescriptorKind::PowerPole {
                id: PowerPoleMiddleID(self.index),
            },
            data::EntityPrototypeKind::SolarPanel => EntityDescriptorKind::SolarPanel {},
            data::EntityPrototypeKind::Accumulator => todo!(),
            data::EntityPrototypeKind::Beacon => todo!(),
        };

        EntityDescriptor {
            position: self.pos.into_real(base_pos),
            rotation,
            flipped,
            ty: self.ty,
            kind,
        }
    }
}

impl From<(Rotation, Flipped)> for RotationWithFlipped {
    #[expect(clippy::too_many_lines)]
    fn from(value: (Rotation, Flipped)) -> Self {
        match value {
            (
                Rotation::North,
                Flipped {
                    horizontally: false,
                    vertically: false,
                },
            ) => Self::North,
            (
                Rotation::East,
                Flipped {
                    horizontally: false,
                    vertically: false,
                },
            ) => Self::East,
            (
                Rotation::South,
                Flipped {
                    horizontally: false,
                    vertically: false,
                },
            ) => Self::South,
            (
                Rotation::West,
                Flipped {
                    horizontally: false,
                    vertically: false,
                },
            ) => Self::West,

            (
                Rotation::North,
                Flipped {
                    horizontally: true,
                    vertically: false,
                },
            ) => Self::HNorth,
            (
                Rotation::East,
                Flipped {
                    horizontally: true,
                    vertically: false,
                },
            ) => Self::HEast,
            (
                Rotation::South,
                Flipped {
                    horizontally: true,
                    vertically: false,
                },
            ) => Self::HSouth,
            (
                Rotation::West,
                Flipped {
                    horizontally: true,
                    vertically: false,
                },
            ) => Self::HWest,

            (
                Rotation::North,
                Flipped {
                    horizontally: false,
                    vertically: true,
                },
            ) => Self::VNorth,
            (
                Rotation::East,
                Flipped {
                    horizontally: false,
                    vertically: true,
                },
            ) => Self::VEast,
            (
                Rotation::South,
                Flipped {
                    horizontally: false,
                    vertically: true,
                },
            ) => Self::VSouth,
            (
                Rotation::West,
                Flipped {
                    horizontally: false,
                    vertically: true,
                },
            ) => Self::VWest,

            (
                Rotation::North,
                Flipped {
                    horizontally: true,
                    vertically: true,
                },
            ) => Self::HVNorth,
            (
                Rotation::East,
                Flipped {
                    horizontally: true,
                    vertically: true,
                },
            ) => Self::HVEast,
            (
                Rotation::South,
                Flipped {
                    horizontally: true,
                    vertically: true,
                },
            ) => Self::HVSouth,
            (
                Rotation::West,
                Flipped {
                    horizontally: true,
                    vertically: true,
                },
            ) => Self::HVWest,
        }
    }
}

impl From<RotationWithFlipped> for (Rotation, Flipped) {
    fn from(value: RotationWithFlipped) -> Self {
        match value {
            RotationWithFlipped::North => (Rotation::North, Flipped::unflipped()),
            RotationWithFlipped::East => (Rotation::East, Flipped::unflipped()),
            RotationWithFlipped::South => (Rotation::South, Flipped::unflipped()),
            RotationWithFlipped::West => (Rotation::West, Flipped::unflipped()),
            RotationWithFlipped::HVNorth => (Rotation::North, Flipped::both()),
            RotationWithFlipped::HVEast => (Rotation::East, Flipped::both()),
            RotationWithFlipped::HVSouth => (Rotation::South, Flipped::both()),
            RotationWithFlipped::HVWest => (Rotation::West, Flipped::both()),
            RotationWithFlipped::HNorth => (Rotation::North, Flipped::horizontal()),
            RotationWithFlipped::HEast => (Rotation::East, Flipped::horizontal()),
            RotationWithFlipped::HSouth => (Rotation::South, Flipped::horizontal()),
            RotationWithFlipped::HWest => (Rotation::West, Flipped::horizontal()),
            RotationWithFlipped::VNorth => (Rotation::North, Flipped::vertical()),
            RotationWithFlipped::VEast => (Rotation::East, Flipped::vertical()),
            RotationWithFlipped::VSouth => (Rotation::South, Flipped::vertical()),
            RotationWithFlipped::VWest => (Rotation::West, Flipped::vertical()),
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum RotationWithFlipped {
    HVNorth,
    HVEast,
    HVSouth,
    HVWest,
    VNorth,
    VEast,
    VSouth,
    VWest,
    HNorth,
    HEast,
    HSouth,
    HWest,
    North,
    East,
    South,
    West,
}

#[cfg(test)]
mod test {
    use super::*;

    use data::spacial::strategies::{random_flipping, random_rotation};
    use proptest::{prop_assert_eq, proptest};

    proptest! {
        #[test]
        fn rotion_flipped_bitmash_from_into(rotation in random_rotation(), flipped in random_flipping()) {
            let combined: RotationWithFlipped = (rotation, flipped).into();

            let (post_rot, post_flip) = combined.into();

            prop_assert_eq!(post_rot, rotation);
            prop_assert_eq!(post_flip, flipped);
        }

        #[test]
        fn adding_entity(x in 0..CHUNK_SIZE, y in 0..CHUNK_SIZE, base_x in -1_000_000..1_000_000, base_y in -1_000_000..1_000_000, rotation in random_rotation(), flipped in random_flipping()) {
            let mut chunk = Chunk::empty();

            let entity = EntityDescriptor {
                position: Position {
                    x: base_x + i32::from(x),
                    y: base_y + i32::from(y),
                },
                rotation,
                flipped,
                ty: 0.into(),
                kind: EntityDescriptorKind::Assembler {
                    id: AssemblerMiddleID(100),
                },
            };

            chunk.add_entity(
                Position {
                    x: base_x,
                    y: base_y,
                },
                entity,
            );
        }

        #[test]
        fn adding_and_removing_entity(x in 0..CHUNK_SIZE, y in 0..CHUNK_SIZE, base_x in -1_000_000..1_000_000, base_y in -1_000_000..1_000_000, rotation in random_rotation(), flipped in random_flipping()) {
            let mut chunk = Chunk::empty();

            let entity_pos = Position {
                x: base_x + i32::from(x),
                y: base_y + i32::from(y),
            };

            let entity = EntityDescriptor {
                position: entity_pos,
                rotation,
                flipped,
                ty: 0.into(),
                kind: EntityDescriptorKind::PowerPole {
                    id: PowerPoleMiddleID(100),
                },
            };

            chunk.add_entity(
                Position {
                    x: base_x,
                    y: base_y,
                },
                entity,
            );

            let removed = chunk.remove_entity(
                Position {
                    x: base_x,
                    y: base_y,
                }, entity_pos);

            prop_assert_eq!(removed, Some(entity));
        }
    }
}
