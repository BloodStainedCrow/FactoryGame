use data::spacial::{BoundingBox, Extent, Position};

pub struct EntityRenderInfo {
    pub sprite: !,
    pub size: Extent,
    pub position: Position,
}

pub trait WorldRenderQueryEngine {
    fn get_entity_render_infos_for_area(
        &self,
        area: BoundingBox,
    ) -> impl Iterator<Item = EntityRenderInfo>;
}
