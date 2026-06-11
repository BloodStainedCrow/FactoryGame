use std::iter;

use data::{
    entity::GlobalTy,
    spacial::{Flipped, Position, Rotation},
};
use middle::lists::{AssemblerIndex, BeltIndex, InserterIndex, PipeIndex};

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
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub(crate) enum EntityDescriptorKind {
    Assembler { id: AssemblerIndex },
    Inserter { id: InserterIndex },
    Belt { id: BeltIndex },
    Pipe { id: PipeIndex },
    SolarPanel {},
    // ...
}
