use data::{
    entity::{
        assember::{AssemblerTy, Recipe},
        beacon::BeaconTy,
        belt::BeltTy,
        chest::ChestTy,
        inserter::InserterTy,
        power_pole::PowerPoleTy,
    },
    spacial::{Flipped, NonSnappingPosition, Position, Rotation},
};

use crate::SurfaceId;

type TechnologyID = ();

#[derive(Debug, Clone, PartialEq, Eq, serde::Serialize, serde::Deserialize)]
pub(crate) enum ActionKind {
    PlaceBuilding {
        ghost: bool,
        force: ForceKind,
        building_info: BuildingInfo,
    },
    RemoveBuilding {
        surface_id: SurfaceId,
        position: Position,
    },
    SetPlayerPos {
        /// A unique identifier for each player
        player_id: u64,

        surface_id: SurfaceId,
        position: NonSnappingPosition,
    },

    AddResearchToQueue {
        tech: TechnologyID,
    },
    RemoveResearchFromQueue {
        tech: TechnologyID,
    },
}

#[derive(Debug, Clone, PartialEq, Eq, serde::Serialize, serde::Deserialize)]
pub(crate) struct BuildingInfo {
    pub(crate) position: Position,
    pub(crate) rotation: Rotation,
    pub(crate) flipped: Flipped,

    pub(crate) kind: BuildingKind,
}

#[derive(Debug, Clone, PartialEq, Eq, serde::Serialize, serde::Deserialize)]
pub(crate) enum BuildingKind {
    Assembler {
        ty: AssemblerTy,
        recipe: Option<Recipe>,
        modules: Vec<()>,
    },
    PowerPole {
        ty: PowerPoleTy,
    },
    Chest {
        ty: ChestTy,
    },
    Inserter {
        ty: InserterTy,
    },
    Belt {
        ty: BeltTy,
    },
    UndergroundBelt {
        ty: BeltTy,
    },
    Beacon {
        ty: BeaconTy,
    },
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, serde::Serialize, serde::Deserialize)]
pub(crate) enum ForceKind {
    None,
    Force,
    SuperForce,
}
