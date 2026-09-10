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
use thiserror::Error;
use world::surface::PlaceEntityError;

use crate::{GameState, SurfaceId};

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
    /// The top_left corner of the building
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

#[derive(Error, Debug)]
pub(crate) enum ApplyActionError {
    #[error("Could not place entity")]
    PlaceEntity(#[from] PlaceEntityError),
}

impl GameState {
    pub(crate) fn apply_action(&mut self, action: &ActionKind) -> Result<(), ApplyActionError> {
        match action {
            ActionKind::PlaceBuilding {
                ghost,
                force,
                building_info,
            } => {
                // FIXME: Support for multiple surfaces
                let surface = &mut self.surfaces[0];

                let &BuildingInfo {
                    position,
                    rotation,
                    flipped,
                    ref kind,
                } = building_info;

                let top_left = position;

                match kind {
                    &BuildingKind::Assembler {
                        ty,
                        recipe,
                        ref modules,
                    } => surface.add_assembler(ty, top_left, rotation, flipped)?,
                    &BuildingKind::PowerPole { ty } => {
                        surface.add_power_pole(ty, top_left, rotation, flipped)?
                    },
                    &BuildingKind::Chest { ty } => {
                        surface.add_chest(ty, top_left, rotation, flipped)?
                    },
                    &BuildingKind::Inserter { ty } => {
                        surface.add_inserter(ty, top_left, rotation, flipped)?
                    },
                    &BuildingKind::Belt { ty } => {
                        // TODO:
                    },
                    &BuildingKind::UndergroundBelt { ty } => {
                        // TODO:
                    },
                    &BuildingKind::Beacon { ty } => {
                        // TODO:
                    },
                }
            },
            ActionKind::RemoveBuilding {
                surface_id,
                position,
            } => todo!(),
            ActionKind::SetPlayerPos {
                player_id,
                surface_id,
                position,
            } => todo!(),
            ActionKind::AddResearchToQueue { tech } => todo!(),
            ActionKind::RemoveResearchFromQueue { tech } => todo!(),
        }

        Ok(())
    }
}
