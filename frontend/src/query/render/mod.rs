use data::{
    entity::extent,
    spacial::{BoundingBox, Extent, Position},
};

use crate::{GameState, SurfaceId};

#[derive(Debug)]
pub struct EntityRenderInfo {
    pub sprite: (),
    pub size: Extent,
    pub position: Position,
}

pub trait WorldRenderQueryEngine {
    fn get_entity_render_infos_for_area(
        &self,
        surface: SurfaceId,
        area: BoundingBox,
    ) -> impl Iterator<Item = EntityRenderInfo>;
}

impl WorldRenderQueryEngine for GameState {
    fn get_entity_render_infos_for_area(
        &self,
        surface: SurfaceId,
        area: BoundingBox,
    ) -> impl Iterator<Item = EntityRenderInfo> {
        let surface = &self.surfaces[surface.0 as usize];

        surface.get_entity_states_in_area(area).map(|info| {
            // TODO: Rendering
            EntityRenderInfo {
                sprite: (),
                size: extent(info.global_ty(), info.rotation, info.flipped),
                position: info.position,
            }
        })
    }
}
