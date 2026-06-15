use data::{
    entity::{assember::AssemblerTy, bounding_box, placement_allowed},
    spacial::{Flipped, Position, Rotation},
};

use crate::surface::world::{CanFitError, SurfaceWorld};

mod belt_logic;
mod pipe_logic;
mod world;

// TODO: This should probably not live in the frontend IMO
struct Surface {
    world: SurfaceWorld,
    middle: !,
    backend: !,
}

enum PlaceEntityError {
    CanFit(CanFitError),
    FloorRule(!),
    PipeFluidMixing(!),
}

impl Surface {
    #[expect(unreachable_code)]
    #[expect(clippy::diverging_sub_expression)]
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

        let placement_legal: bool =
            placement_allowed(ty.into(), todo!("Get the floor from the world"));

        if !placement_legal {
            return Err(PlaceEntityError::FloorRule(todo!()));
        }

        let default_recipe: ! = todo!("Get default recipe from entity ty");

        let connected_pipes: Vec<(!, !)> = todo!("Get pipe connections");

        // Ensure there are no illegal pipe connections
        for (assembler_conn, pipe_network) in &connected_pipes {
            if assembler_conn != pipe_network {
                return Err(todo!());
            }
        }

        // Placement is allowed. Do the placing

        let power_grid = (todo!("Find grid") as Option<_>).unwrap_or(0);
        let connected_inserters: Vec<!> = todo!();

        let middle_inserter_id = todo!("Add Assembler to middle");

        todo!("Add assembler entity to world");

        Ok(())
    }
}
