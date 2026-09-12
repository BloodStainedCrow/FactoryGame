use crate::{
    DATA_STORE, EntityPrototypeKind,
    entity::{GlobalTy, extent},
    spacial::{Extent, Flipped, Offset, Position, Rotation},
};

#[derive(Debug, Clone, Copy, PartialEq, Eq, serde::Serialize, serde::Deserialize)]
pub struct InserterTy(u16);

impl From<InserterTy> for GlobalTy {
    fn from(value: InserterTy) -> Self {
        Self(
            EntityPrototypeKind::Inserter
                .global_index_for_kind_index(value.0 as usize)
                .expect("Illegal InserterTy")
                .try_into()
                .expect("More than u16::MAX entities"),
        )
    }
}

impl TryFrom<GlobalTy> for InserterTy {
    type Error = ();

    fn try_from(value: GlobalTy) -> Result<Self, Self::Error> {
        EntityPrototypeKind::Inserter
            .kind_index_from_global_index(value.0 as usize)
            .map(|idx| Self(idx.try_into().expect("More than u16::MAX entities")))
            .ok_or(())
    }
}

#[must_use]
pub fn get_input_position(
    ty: InserterTy,
    top_left: Position,
    rotation: Rotation,
    flipped: Flipped,
) -> Position {
    let input_offset = DATA_STORE.inserters[ty.0 as usize].source_offset;
    top_left
        + get_point_rotated_from_top_left(
            extent(ty.into(), rotation, flipped),
            input_offset,
            rotation,
            flipped,
        )
}

// TODO: Which part of the tile
#[must_use]
pub fn get_output_position(
    ty: InserterTy,
    top_left: Position,
    rotation: Rotation,
    flipped: Flipped,
) -> Position {
    let output_offset = DATA_STORE.inserters[ty.0 as usize].dest_offset;
    top_left
        + get_point_rotated_from_top_left(
            extent(ty.into(), rotation, flipped),
            output_offset,
            rotation,
            flipped,
        )
}

#[must_use]
const fn get_original_base_pos(
    entity_extent: Extent,
    rotation: Rotation,
    flipped: Flipped,
) -> Offset {
    let base_extent = entity_extent.rotate(rotation);

    // Flipping mirrors the entity within its base footprint, which moves the
    // base top-left tile to the opposite corner.
    let flipped_base_top_left = Offset {
        x_offs: if flipped.horizontally {
            base_extent.width.cast_signed() - 1
        } else {
            0
        },
        y_offs: if flipped.vertically {
            base_extent.height.cast_signed() - 1
        } else {
            0
        },
    };

    // Then the rotation maps that base-frame point into the placed footprint.
    let (width, height) = (
        entity_extent.width.cast_signed(),
        entity_extent.height.cast_signed(),
    );
    match rotation {
        Rotation::North => flipped_base_top_left,
        Rotation::East => Offset {
            x_offs: width - 1 - flipped_base_top_left.y_offs,
            y_offs: flipped_base_top_left.x_offs,
        },
        Rotation::South => Offset {
            x_offs: width - 1 - flipped_base_top_left.x_offs,
            y_offs: height - 1 - flipped_base_top_left.y_offs,
        },
        Rotation::West => Offset {
            x_offs: flipped_base_top_left.y_offs,
            y_offs: height - 1 - flipped_base_top_left.x_offs,
        },
    }
}

#[must_use]
const fn get_point_rotated_from_top_left(
    entity_extent: Extent,
    offset: Offset,
    rotation: Rotation,
    flipped: Flipped,
) -> Offset {
    // Reflect the offset about the base top-left tile; the footprint shift
    // this misses is already carried by `get_original_base_pos`.
    let mirrored = Offset {
        x_offs: if flipped.horizontally {
            -offset.x_offs
        } else {
            offset.x_offs
        },
        y_offs: if flipped.vertically {
            -offset.y_offs
        } else {
            offset.y_offs
        },
    };

    let anchor = get_original_base_pos(entity_extent, rotation, flipped);
    let rotated_mirrored = mirrored.rotate(rotation);

    Offset {
        x_offs: anchor.x_offs + rotated_mirrored.x_offs,
        y_offs: anchor.y_offs + rotated_mirrored.y_offs,
    }
}

#[cfg(test)]
mod tests {
    use super::{
        InserterTy, get_input_position, get_output_position, get_point_rotated_from_top_left,
    };
    use crate::spacial::{Flipped, Offset, Position, Rotation, strategies};
    use proptest::{prelude::*, prop_assert};

    const BULK_INSERTER: InserterTy = InserterTy(0);

    proptest! {
        #[test]
        fn north_unflipped_is_identity(
            extent in strategies::random_extent(),
            offset in strategies::random_offset(),
        ) {
            prop_assert_eq!(
                get_point_rotated_from_top_left(extent, offset, Rotation::North, Flipped::unflipped()),
                offset,
            );
        }

        #[test]
        fn footprint_points_stay_within_the_placed_footprint(
            (rotation, extent, x, y) in strategies::random_rotation().prop_flat_map(|rotation| {
                strategies::random_extent().prop_flat_map(move |extent| {
                    // The offset is in the entity's base frame, so it must be
                    // generated within the unrotated footprint.
                    let base_extent = extent.rotate(rotation);

                    (
                        Just(rotation),
                        Just(extent),
                        0..base_extent.width,
                        0..base_extent.height,
                    )
                })
            }),
            flipped in strategies::random_flipping(),
        ) {
            let offset = Offset {
                x_offs: x.cast_signed(),
                y_offs: y.cast_signed(),
            };

            let result = get_point_rotated_from_top_left(extent, offset, rotation, flipped);

            prop_assert!(result.x_offs >= 0 && (result.x_offs.cast_unsigned()) < extent.width);
            prop_assert!(result.y_offs >= 0 && (result.y_offs.cast_unsigned()) < extent.height);
        }
    }

    #[test]
    fn bulk_inserter_input_and_output_positions() {
        let top_left = Position { x: 5, y: 5 };

        for (rotation, input, output) in [
            (
                Rotation::North,
                Position { x: 5, y: 6 },
                Position { x: 5, y: 4 },
            ),
            (
                Rotation::East,
                Position { x: 4, y: 5 },
                Position { x: 6, y: 5 },
            ),
            (
                Rotation::South,
                Position { x: 5, y: 4 },
                Position { x: 5, y: 6 },
            ),
            (
                Rotation::West,
                Position { x: 6, y: 5 },
                Position { x: 4, y: 5 },
            ),
        ] {
            assert_eq!(
                get_input_position(BULK_INSERTER, top_left, rotation, Flipped::unflipped()),
                input,
            );
            assert_eq!(
                get_output_position(BULK_INSERTER, top_left, rotation, Flipped::unflipped()),
                output,
            );
        }
    }

    #[test]
    fn vertical_flip_swaps_input_and_output_sides() {
        let top_left = Position { x: 5, y: 5 };
        let flipped = Flipped::vertical();

        assert_eq!(
            get_input_position(BULK_INSERTER, top_left, Rotation::North, flipped),
            Position { x: 5, y: 4 },
        );
        assert_eq!(
            get_output_position(BULK_INSERTER, top_left, Rotation::North, flipped),
            Position { x: 5, y: 6 },
        );
    }
}
