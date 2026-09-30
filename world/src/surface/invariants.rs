use entity_info::EntityDescriptor;
use middle::power_pole::GetPoleConn;

use crate::surface::Surface;

impl Surface {
    pub(crate) fn check_invariants(&self) {
        if !cfg!(debug_assertions) {
            return;
        }

        // Check pole connections
        for (e, pole) in self
            .world
            .get_all_entities()
            .filter(EntityDescriptor::can_be_powered_by_a_pole)
            .map(|e| {
                (
                    e,
                    self.world
                        .get_pole_for_entity_bounding_box(e.bounding_box()),
                )
            })
        {
            let thing = e.get_pole_connection().unwrap();
            if let Some(pole) = pole {
                self.middle.assert_pole_contains_thing(pole, thing);
                self.middle.assert_pole_and_thing_agree_on_grid(pole, thing);
            } else {
                self.middle.assert_thing_unconnected(thing);
            }
        }
    }
}
