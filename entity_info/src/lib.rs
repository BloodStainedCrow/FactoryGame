use std::iter;

use data::{
    entity::{
        GlobalTy, assember::AssemblerTy, belt::BeltTy, bounding_box, chest::ChestTy,
        inserter::InserterTy, power_pole::PowerPoleTy,
    },
    spacial::{BoundingBox, Flipped, Position, Rotation},
};
use middle_indices::{
    AssemblerMiddleID, BeltTileMiddleID, ChestMiddleID, InserterMiddleID, PipeMiddleID,
    PowerPoleMiddleID,
};

#[derive(Debug)]
pub struct EntityInfo {
    pub position: Position,
    pub rotation: Rotation,
    pub flipped: Flipped,

    pub kind: EntityInfoKind,
}

impl EntityInfo {
    #[must_use]
    pub fn global_ty(&self) -> GlobalTy {
        match self.kind {
            EntityInfoKind::Assembler { ty, .. } => ty.into(),
            EntityInfoKind::PowerPole { ty, .. } => ty.into(),
            EntityInfoKind::Chest { ty, .. } => ty.into(),
            EntityInfoKind::Inserter { ty, .. } => ty.into(),
            EntityInfoKind::Belt { ty, .. } => ty.into(),
        }
    }

    // TODO: This is redundant
    #[must_use]
    pub const fn can_be_powered_by_a_pole(&self) -> bool {
        match self.kind {
            EntityInfoKind::PowerPole { .. }
            | EntityInfoKind::Chest { .. }
            | EntityInfoKind::Belt { .. } => false,
            EntityInfoKind::Assembler { .. } | EntityInfoKind::Inserter { .. } => true,
        }
    }
}

#[derive(Debug)]
pub enum EntityInfoKind {
    Assembler {
        ty: AssemblerTy,
        middle_id: AssemblerMiddleID,
        // ...
    },
    PowerPole {
        ty: PowerPoleTy,
        connected_pole_positions: Vec<Position>,
        middle_id: PowerPoleMiddleID,
        // ...
    },
    Chest {
        ty: ChestTy,
        middle_id: ChestMiddleID,
    },
    Inserter {
        ty: InserterTy,
        middle_id: InserterMiddleID,
    },
    Belt {
        ty: BeltTy,
        middle_id: BeltTileMiddleID,
    },
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct EntityDescriptor {
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

    #[must_use]
    pub const fn can_be_powered_by_a_pole(&self) -> bool {
        // TODO: Some kinds might not want to be powered
        match self.kind {
            EntityDescriptorKind::Pipe { .. }
            | EntityDescriptorKind::Belt { .. }
            | EntityDescriptorKind::PowerPole { .. }
            | EntityDescriptorKind::Chest { .. } => false,
            EntityDescriptorKind::Assembler { .. }
            | EntityDescriptorKind::Inserter { .. }
            | EntityDescriptorKind::SolarPanel { .. } => true,
        }
    }

    #[must_use]
    pub fn bounding_box(&self) -> BoundingBox {
        bounding_box(self.ty, self.position, self.rotation, self.flipped)
    }

    #[must_use]
    pub fn overlaps(&self, other: BoundingBox) -> bool {
        bounding_box(self.ty, self.position, self.rotation, self.flipped).overlaps(other)
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum EntityDescriptorKind {
    Assembler { id: AssemblerMiddleID },
    Inserter { id: InserterMiddleID },
    Belt { id: BeltTileMiddleID },
    Pipe { id: PipeMiddleID },
    PowerPole { id: PowerPoleMiddleID },
    Chest { id: ChestMiddleID },
    SolarPanel {},
    // ...
}
