use data::spacial::BoundingBox;
use entity_info::EntityInfo;

use crate::surface::Surface;

impl Surface {
    pub(super) fn get_powered_entites_for_pole(
        &self,
        pole_area: BoundingBox,
    ) -> impl Iterator<Item = EntityInfo> {
        self.get_entity_states_in_area(pole_area)
            .filter(|e| e.can_be_powered_by_a_pole())
    }
}
