use data::{
    entity::{
        assember::{AssemblerTy, default_recipe},
        bounding_box,
    },
    spacial::{Flipped, Position, Rotation},
};
use middle::{Middle, assember::AssemblerAdditionInfo};

use crate::{
    entity::{EntityDescriptor, EntityDescriptorKind},
    surface::world::{CanFitError, SurfaceWorld},
};

mod belt_logic;
mod pipe_logic;
mod world;

// TODO: This should probably not live in the frontend IMO
struct Surface {
    world: SurfaceWorld,
    middle: Middle,
    backend: !,
}

enum PlaceEntityError {
    CanFit(CanFitError),
    FloorRule(!),
    PipeFluidMixing(!),
}

impl Surface {
    fn add_assembler(
        &mut self,
        ty: AssemblerTy,
        top_left: Position,
        rotation: Rotation,
        flipped: Flipped,
    ) -> Result<(), PlaceEntityError> {
        let bounding_box = bounding_box(ty.into(), top_left, rotation, flipped);

        if let Err(err) = self.world.can_fit(bounding_box) {
            // Cannot fit
            return Err(PlaceEntityError::CanFit(err));
        }

        // let placement_legal: bool =
        //     placement_allowed(ty.into(), todo!("Get the floor from the world"));

        // if !placement_legal {
        //     return Err(PlaceEntityError::FloorRule(todo!()));
        // }

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
}
