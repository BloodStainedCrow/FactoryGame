use data::{
    entity::power_pole::{power_pole_search_range, power_pole_supply_area},
    spacial::BoundingBox,
};
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
        self.get_entities_in_area(entity_bb.extend_evenly(power_pole_search_range()))
            .filter_map(|e| match e.kind {
                entity_info::EntityDescriptorKind::PowerPole { id } => {
                    if power_pole_supply_area(
                        e.ty.try_into().expect("Illegal PowerPoleTy"),
                        e.position,
                        e.rotation,
                        e.flipped,
                    )
                    .overlaps(entity_bb)
                    {
                        Some(id)
                    } else {
                        None
                    }
                },

                _ => None,
            })
            .next()
    }
}
