use data::spacial::BoundingBox;
use entity_info::EntityDescriptor;
use middle_indices::PowerPoleMiddleID;

use crate::surface::world::SurfaceWorld;

// NOTE: These two functions must always match.
// FIXME(BSC): Write proptests asserting that!
impl SurfaceWorld {
    pub(super) fn get_powered_entites_for_pole(
        &self,
        pole_area: BoundingBox,
    ) -> impl Iterator<Item = EntityDescriptor> {
        self.get_entities_in_area(pole_area)
            .filter(entity_info::EntityDescriptor::can_be_powered_by_a_pole)
    }

    pub(super) fn get_pole_for_entity_bounding_box(
        &self,
        entity_bb: BoundingBox,
    ) -> Option<PowerPoleMiddleID> {
        // FIXME:
        None
    }
}
