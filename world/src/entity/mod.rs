use std::iter;

use data::{
    entity::{GlobalTy, bounding_box},
    spacial::{BoundingBox, Flipped, Position, Rotation},
};
use middle_indices::{
    AssemblerMiddleID, BeltMiddleID, ChestMiddleID, InserterMiddleID, PipeMiddleID,
    PowerPoleMiddleID,
};

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

    pub fn can_be_powered_by_a_pole(&self) -> bool {
        // TODO: Some kinds might not want to be powered
        match self.kind {
            EntityDescriptorKind::Assembler { .. } => true,
            EntityDescriptorKind::Pipe { .. } => false,
            EntityDescriptorKind::Inserter { .. } => true,
            EntityDescriptorKind::Belt { .. } => false,
            EntityDescriptorKind::PowerPole { .. } => false,
            EntityDescriptorKind::SolarPanel { .. } => true,
            EntityDescriptorKind::Chest { .. } => false,
        }
    }

    pub fn overlaps(&self, other: BoundingBox) -> bool {
        bounding_box(self.ty, self.position, self.rotation, self.flipped).overlaps(other)
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub(crate) enum EntityDescriptorKind {
    Assembler { id: AssemblerMiddleID },
    Inserter { id: InserterMiddleID },
    Belt { id: BeltMiddleID },
    Pipe { id: PipeMiddleID },
    PowerPole { id: PowerPoleMiddleID },
    Chest { id: ChestMiddleID },
    SolarPanel {},
    // ...
}
