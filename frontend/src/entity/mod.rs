use std::iter;

use data::{
    entity::{GlobalTy, bounding_box},
    spacial::{BoundingBox, Flipped, Position, Rotation},
};
use middle::lists::{AssemblerIndex, BeltIndex, InserterIndex, PipeIndex, PowerPoleIndex};

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub(crate) struct EntityDescriptor {
    pub position: Position,
    pub rotation: Rotation,
    pub flipped: Flipped,
    pub ty: GlobalTy,

    pub kind: EntityDescriptorKind,
}

impl EntityDescriptor {
    fn get_pipe_connections(&self) -> impl Iterator<Item = !> {
        match self.kind {
            EntityDescriptorKind::Assembler { .. } => todo!(),
            EntityDescriptorKind::Pipe { .. } => todo!(),
            _ => iter::empty(),
        }
    }

    pub fn overlaps(&self, other: BoundingBox) -> bool {
        bounding_box(self.ty, self.position, self.rotation, self.flipped).overlaps(other)
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub(crate) enum EntityDescriptorKind {
    Assembler { id: AssemblerIndex },
    Inserter { id: InserterIndex },
    Belt { id: BeltIndex },
    Pipe { id: PipeIndex },
    PowerPole { id: PowerPoleIndex },
    SolarPanel {},
    // ...
}
