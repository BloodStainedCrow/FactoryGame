use backend::Backend;
use data::{
    entity::{
        GlobalTy, allows_flipping, allows_rotation,
        assember::{AssemblerTy, default_recipe},
        bounding_box,
        power_pole::{PowerPoleTy, power_pole_supply_area, power_pole_wire_connection_area},
    },
    spacial::{BoundingBox, Flipped, Position, Rotation},
};
use entity_info::{EntityInfo, EntityInfoKind};
use itertools::Itertools;
use middle::{
    Middle, UNATTACHED_POWER_GRID_ID,
    assembler::AssemblerAdditionInfo,
    power_pole::{AUTOMATIC_POLE_CONNECTION_LIMIT, PowerPoleAdditionInfo},
};
use smallvec::SmallVec;
use thiserror::Error;

use crate::{
    entity::{EntityDescriptor, EntityDescriptorKind},
    surface::world::{CanFitError, SurfaceWorld},
};

mod belt_logic;
mod pipe_logic;
mod power_pole_logic;
mod world;

// TODO: This should probably not live in the frontend IMO
#[derive(Debug, Clone)]
pub struct Surface {
    world: SurfaceWorld,
    middle: Middle,
    backend: Backend,
}

#[derive(Debug, Error)]
pub enum PlaceEntityError {
    #[error("Entity may not be rotated")]
    RotationForbidden,
    #[error("Entity may not be flipped")]
    FlippingForbidden,
    #[error("Entity does not fit")]
    CanFit(CanFitError),
    #[error("Entity may on be placed on")]
    FloorRule(!),
    #[error("Cannot mix fluids")]
    PipeFluidMixing(!),
}

pub struct SurfaceCreationOptions {
    pub generated_area: BoundingBox,
}

impl Surface {
    #[must_use]
    pub fn new(options: &SurfaceCreationOptions) -> Self {
        let mut backend = Backend::new();

        Self {
            world: SurfaceWorld::new_with_empty_area(options.generated_area),
            middle: Middle::new(&mut backend),
            backend,
        }
    }

    pub fn get_entity_states_in_area(&self, area: BoundingBox) -> impl Iterator<Item = EntityInfo> {
        self.world
            .get_entities_in_area(area)
            .map(|desc| EntityInfo {
                position: desc.position,
                rotation: desc.rotation,
                flipped: desc.flipped,
                kind: match desc.kind {
                    EntityDescriptorKind::Assembler { id } => EntityInfoKind::Assembler {
                        ty: desc.ty.try_into().expect("Assembler with non AssemblerTy"),
                        middle_id: id,
                    },
                    EntityDescriptorKind::Inserter { id } => todo!(),
                    EntityDescriptorKind::Belt { id } => todo!(),
                    EntityDescriptorKind::Pipe { id } => todo!(),
                    EntityDescriptorKind::PowerPole { id } => EntityInfoKind::PowerPole {
                        ty: desc.ty.try_into().expect("PowerPole with non PowerPoleTy"),
                        connected_pole_positions: self
                            .middle
                            .get_pole_connected_positions(id)
                            .collect(),
                        middle_id: id,
                    },
                    EntityDescriptorKind::SolarPanel {} => todo!(),
                },
            })
    }

    fn follows_rules(
        &self,
        ty: GlobalTy,
        top_left: Position,
        rotation: Rotation,
        flipped: Flipped,
    ) -> Result<BoundingBox, PlaceEntityError> {
        if rotation != Rotation::North && !allows_rotation(ty) {
            return Err(PlaceEntityError::RotationForbidden);
        }

        if flipped != Flipped::unflipped() && !allows_flipping(ty) {
            return Err(PlaceEntityError::FlippingForbidden);
        }

        let bounding_box = bounding_box(ty, top_left, rotation, flipped);

        if let Err(err) = self.world.can_fit(bounding_box) {
            // Cannot fit
            return Err(PlaceEntityError::CanFit(err));
        }

        // let placement_legal: bool =
        //     placement_allowed(ty, todo!("Get the floor from the world"));

        // if !placement_legal {
        //     return Err(PlaceEntityError::FloorRule(todo!()));
        // }

        Ok(bounding_box)
    }

    /// # Errors
    /// If placing this entity is not legal
    pub fn add_assembler(
        &mut self,
        ty: AssemblerTy,
        top_left: Position,
        rotation: Rotation,
        flipped: Flipped,
    ) -> Result<(), PlaceEntityError> {
        log::trace!("Add assembler with ty {ty:?} at {top_left:?}");
        let _bounding_box = self.follows_rules(ty.into(), top_left, rotation, flipped)?;

        let default_recipe = default_recipe(ty);

        // let connected_pipes: Vec<(!, !)> = todo!("Get pipe connections");

        // // Ensure there are no illegal pipe connections
        // for (assembler_conn, pipe_network) in &connected_pipes {
        //     if assembler_conn != pipe_network {
        //         return Err(PlaceEntityError::PipeFluidMixing(todo!()));
        //     }
        // }

        // Placement is allowed. Do the placing

        // let power_grid = (todo!("Find grid") as Option<_>).unwrap_or(0);
        // let connected_inserters: Vec<!> = todo!();

        let middle_assembler_id = self.middle.add_assembler(
            &AssemblerAdditionInfo {
                recipe: default_recipe,
                power_grid: UNATTACHED_POWER_GRID_ID,
            },
            &mut self.backend,
        );

        self.world.add_entity(EntityDescriptor {
            position: top_left,
            rotation,
            flipped,
            ty: ty.into(),
            kind: EntityDescriptorKind::Assembler {
                id: middle_assembler_id,
            },
        });

        Ok(())
    }

    /// # Errors
    /// If placing this entity is not legal
    pub fn add_power_pole(
        &mut self,
        ty: PowerPoleTy,
        top_left: Position,
        rotation: Rotation,
        flipped: Flipped,
    ) -> Result<(), PlaceEntityError> {
        log::trace!("Add power_pole with ty {ty:?} at {top_left:?}");
        let bounding_box = self.follows_rules(ty.into(), top_left, rotation, flipped)?;

        let mut connected_poles: SmallVec<_> = SmallVec::default();
        for conn in self
            .world
            .get_power_poles_overlapping(power_pole_wire_connection_area(
                ty, top_left, rotation, flipped,
            ))
            .filter(|other| {
                let other_connection_area = power_pole_wire_connection_area(
                    other
                        .ty
                        .try_into()
                        .expect("get_power_poles_in_area returned non PowerPole"),
                    other.position,
                    other.rotation,
                    other.flipped,
                );

                other_connection_area.overlaps(bounding_box)
            })
            // FIXME: Manhatten is prob wrong, and using top_left is for sure wrong
            .sorted_by_key(|e| top_left.manhattan_distance(e.position))
            .map(|e| {
                let EntityDescriptorKind::PowerPole { id } = e.kind else {
                    unreachable!()
                };
                id
            })
        {
            if connected_poles.len() > AUTOMATIC_POLE_CONNECTION_LIMIT {
                break;
            }

            if connected_poles
                .iter()
                .all(|already| !self.middle.are_poles_connected([*already, conn]))
            {
                // This will not form a triangle
                connected_poles.push(conn);
            }
        }

        let connected_entities: Vec<_> = self
            .get_powered_entites_for_pole(power_pole_supply_area(ty, top_left, rotation, flipped))
            .collect();

        let index = self.middle.add_power_pole(
            PowerPoleAdditionInfo {
                position: top_left,
                connections: connected_poles,
                connected_entities,
            },
            &mut self.backend,
        );

        self.world.add_entity(EntityDescriptor {
            position: top_left,
            rotation,
            flipped,
            ty: ty.into(),
            kind: EntityDescriptorKind::PowerPole { id: index },
        });

        Ok(())
    }
}
