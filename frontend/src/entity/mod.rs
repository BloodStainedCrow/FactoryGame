use data::spacial::{Flipped, Position, Rotation};
use middle::lists::AssemblerIndex;

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub(crate) struct EntityDescriptor {
    pub position: Position,
    pub rotation: Rotation,
    pub flipped: Flipped,
    pub ty: u16,

    pub kind: EntityDescriptorKind,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub(crate) enum EntityDescriptorKind {
    Assembler { id: AssemblerIndex },
    SolarPanel {},
    // ...
}
