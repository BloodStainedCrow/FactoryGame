use data::{
    entity::{GlobalTy, assember::AssemblerTy, power_pole::PowerPoleTy},
    spacial::{Flipped, Position, Rotation},
};
use middle_indices::{AssemblerMiddleID, PowerPoleMiddleID};

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
        }
    }

    #[must_use]
    pub fn can_be_powered_by_a_pole(&self) -> bool {
        match self.kind {
            EntityInfoKind::Assembler { ty, .. } => true,
            EntityInfoKind::PowerPole { .. } => false,
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
}
