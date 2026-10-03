use std::cmp::Ordering;

use data::{
    entity::power_pole::{power_pole_search_range, power_pole_supply_area},
    spacial::{BoundingBox, Position},
};
use entity_info::{EntityDescriptor, EntityDescriptorKind};
use middle_indices::PowerPoleMiddleID;

use crate::surface::world::SurfaceWorld;

fn power_pole_strength(_entity_pos: Position, a: Position, b: Position) -> Ordering {
    // TODO: Maybe make this into the closest

    // The smallest position (i.e. top left most wins)
    a.cmp(&b).reverse()
}

// NOTE: These two functions must always match.
// FIXME(BSC): Write proptests asserting that!
impl SurfaceWorld {
    pub(super) fn get_powered_entites_for_pole(
        &self,
        pole_pos: Position,
        pole_area: BoundingBox,
    ) -> impl Iterator<Item = EntityDescriptor> {
        self.get_entities_in_area(pole_area)
            .filter(entity_info::EntityDescriptor::can_be_powered_by_a_pole)
            .filter(
                move |e| match self.get_pole_entity_for_entity_bounding_box(e.bounding_box()) {
                    Some(prev_pole) => {
                        power_pole_strength(e.position, pole_pos, prev_pole.position).is_ge()
                    },
                    None => true,
                },
            )
    }

    fn get_pole_entity_for_entity_bounding_box(
        &self,
        entity_bb: BoundingBox,
    ) -> Option<EntityDescriptor> {
        self.get_entities_in_area(entity_bb.extend_evenly(power_pole_search_range()))
            .filter_map(|e| match e.kind {
                entity_info::EntityDescriptorKind::PowerPole { .. } => {
                    assert_ne!(e.bounding_box(), entity_bb);
                    if power_pole_supply_area(
                        e.ty.try_into().expect("Illegal PowerPoleTy"),
                        e.position,
                        e.rotation,
                        e.flipped,
                    )
                    .overlaps(entity_bb)
                    {
                        Some(e)
                    } else {
                        None
                    }
                },

                _ => None,
            })
            // TODO(BSC): I think this will be expensive :(
            .max_by(|a, b| power_pole_strength(entity_bb.top_left(), a.position, b.position))
    }

    pub(super) fn get_pole_for_entity_bounding_box(
        &self,
        entity_bb: BoundingBox,
    ) -> Option<PowerPoleMiddleID> {
        self.get_pole_entity_for_entity_bounding_box(entity_bb)
            .map(|e| {
                let EntityDescriptorKind::PowerPole { id } = e.kind else {
                    unreachable!()
                };
                id
            })
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use data::entity::{GlobalTy, inserter::InserterTy, power_pole::PowerPoleTy};
    use data::{
        EntityIdentifier,
        spacial::{Extent, Flipped, Rotation},
    };
    use entity_info::{EntityDescriptor, EntityDescriptorKind};
    use middle_indices::{InserterMiddleID, PowerPoleMiddleID};
    use proptest::prelude::*;

    fn surface_world() -> SurfaceWorld {
        SurfaceWorld::new_with_empty_area(BoundingBox::new(
            Position { x: -100, y: -100 },
            Extent {
                width: 400,
                height: 400,
            },
        ))
    }

    fn small_pole_ty() -> PowerPoleTy {
        PowerPoleTy::try_from(
            GlobalTy::try_from(EntityIdentifier::new_raw(
                "factory_game::small_power_pole".to_string(),
            ))
            .expect("small_power_pole must exist in the data store"),
        )
        .expect("small_power_pole must be a power pole")
    }

    fn inserter_ty() -> InserterTy {
        InserterTy::try_from(
            GlobalTy::try_from(EntityIdentifier::new_raw(
                "factory_game::bulk_inserter".to_string(),
            ))
            .expect("bulk_inserter must exist in the data store"),
        )
        .expect("bulk_inserter must be an inserter")
    }

    fn place_pole(world: &mut SurfaceWorld, pos: Position) {
        world.add_entity(EntityDescriptor {
            position: pos,
            rotation: Rotation::North,
            flipped: Flipped::unflipped(),
            ty: small_pole_ty().into(),
            kind: EntityDescriptorKind::PowerPole {
                id: PowerPoleMiddleID(u32::MAX),
            },
        });
    }

    fn place_inserter(world: &mut SurfaceWorld, pos: Position) {
        world.add_entity(EntityDescriptor {
            position: pos,
            rotation: Rotation::North,
            flipped: Flipped::unflipped(),
            ty: inserter_ty().into(),
            kind: EntityDescriptorKind::Inserter {
                id: InserterMiddleID(u32::MAX),
            },
        });
    }

    proptest! {
        // `get_pole_entity_for_entity_bounding_box` must return the strongest
        // covering pole (per `power_pole_strength`), and
        // `get_powered_entites_for_pole` must yield the entity for exactly
        // that pole.
        #[test]
        fn powering_uses_the_strongest_covering_pole(
            pole_positions in prop::collection::vec((0i32..30, 0i32..30), 1..6),
            entity_pos in (0i32..30, 0i32..30),
        ) {
            let mut world = surface_world();
            let ty = small_pole_ty();

            let mut placed: Vec<Position> = Vec::new();
            for (x, y) in pole_positions {
                let pos = Position { x, y };
                if placed.contains(&pos) {
                    continue;
                }
                place_pole(&mut world, pos);
                placed.push(pos);
            }

            let entity_position = Position {
                x: entity_pos.0,
                y: entity_pos.1,
            };
            let entity_bb = BoundingBox::new(entity_position, Extent::single_tile());
            place_inserter(&mut world, entity_position);

            // Ground truth: every pole whose supply area overlaps the entity,
            // of which the strongest must win.
            let covering: Vec<Position> = placed
                .iter()
                .copied()
                .filter(|&pole_pos| {
                    power_pole_supply_area(ty, pole_pos, Rotation::North, Flipped::unflipped())
                        .overlaps(entity_bb)
                })
                .collect();

            let discovered =
                world.get_pole_entity_for_entity_bounding_box(entity_bb);

            if covering.is_empty() {
                prop_assert!(discovered.is_none());
            } else {
                // Ground truth owner: the strongest covering pole, defined
                // identically to the discovery implementation.
                let owner = covering
                    .iter()
                    .copied()
                    .max_by(|a, b| power_pole_strength(entity_position, *a, *b))
                    .expect("Non empty");

                prop_assert_eq!(discovered.as_ref().map(|d| d.position), Some(owner));

                // The claiming rule of `get_powered_entites_for_pole` must agree:
                // the entity is yielded for exactly the owning pole.
                for &pole_pos in &covering {
                    let powered = world
                        .get_powered_entites_for_pole(
                            pole_pos,
                            power_pole_supply_area(ty, pole_pos, Rotation::North, Flipped::unflipped()),
                        )
                    .any(|e| e.position == entity_position);

                    prop_assert_eq!(powered, pole_pos == owner);
                }
            }
        }
    }
}
