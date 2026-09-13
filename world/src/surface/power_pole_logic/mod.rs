use data::spacial::BoundingBox;
use entity_info::EntityDescriptor;

use crate::surface::world::SurfaceWorld;

impl SurfaceWorld {
    pub(super) fn get_powered_entites_for_pole(
        &self,
        pole_area: BoundingBox,
    ) -> impl Iterator<Item = EntityDescriptor> {
        self.get_entities_in_area(pole_area)
            .filter(|e| e.can_be_powered_by_a_pole())
    }
}
