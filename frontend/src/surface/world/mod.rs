use std::ops::RangeInclusive;

use data::{
    max_entity_size,
    spacial::{BoundingBox, Position},
};
use itertools::Itertools;

use crate::{
    chunk::{CHUNK_SIZE, Chunk},
    sparse_grid::{SparseGrid, dynamic::DynamicGrid},
};

type ChunkStore = DynamicGrid<i32, Chunk>;

struct SurfaceConfiguration {
    surface_generation: (),
    has_global_power: bool,
}

pub struct SurfaceWorld {
    chunks: ChunkStore,
}

impl SurfaceWorld {
    pub fn new_with_empty_area(area: BoundingBox) -> Self {
        let x_range: RangeInclusive<i32> = todo!();
        let y_range: RangeInclusive<i32> = todo!();

        Self {
            chunks: DynamicGrid::new_with_filled_grid(
                [*x_range.start(), *y_range.start()],
                [*x_range.end(), *y_range.end()],
                |_| Chunk::empty(),
            ),
        }
    }

    pub fn can_fit(&self, goal_bounding_box: BoundingBox) -> bool {
        self.get_chunks_that_could_contain_entities_colliding_with(
            goal_bounding_box.extend_evenly(max_entity_size()),
        )
        .flat_map(|(chunk, base_pos)| chunk.occupied_bounding_boxes(base_pos))
        .all(|entity_bounding_box| !entity_bounding_box.overlaps(goal_bounding_box))
    }

    fn get_chunks_that_could_contain_entities_colliding_with(
        &self,
        bounding_box: BoundingBox,
    ) -> impl Iterator<Item = (&Chunk, Position)> {
        let x_range: RangeInclusive<i32> = todo!();
        let y_range: RangeInclusive<i32> = todo!();

        // TODO: Ensure the access order is aligned with the storage order for bounding_box_grid
        x_range.cartesian_product(y_range).filter_map(|(x, y)| {
            // Ungenerated chunks are just filtered out here
            self.chunks.get(x, y).map(|chunk| {
                (
                    chunk,
                    Position {
                        x: x * i32::from(CHUNK_SIZE),
                        y: y * i32::from(CHUNK_SIZE),
                    },
                )
            })
        })
    }
}

#[cfg(test)]
mod test {
    use data::spacial::{Extent, strategies::random_bounding_box};
    use proptest::{prop_assert, proptest};

    use super::*;

    proptest! {
        #[test]
        fn create_world(area in random_bounding_box()) {
            let _world = SurfaceWorld::new_with_empty_area(area);
        }

        #[test]
        fn can_fit(area in random_bounding_box(), goal in random_bounding_box()) {
            let world = SurfaceWorld::new_with_empty_area(area);

            let _res = world.can_fit(goal);
        }

        #[test]
        fn can_fit_outside_generated(goal in random_bounding_box()) {
            let world = SurfaceWorld::new_with_empty_area(BoundingBox::new(Position { x: 0, y: 0 }, Extent { width: 0, height: 0 }));

            let can_fit = world.can_fit(goal);

            prop_assert!(!can_fit);
        }

        #[test]
        fn can_fit_inside_generated(goal in random_bounding_box()) {
            let world = SurfaceWorld::new_with_empty_area(BoundingBox::new(Position { x: -20_000, y: -20_000 }, Extent { width: 40_000, height: 40_000 }));

            let can_fit = world.can_fit(goal);

            prop_assert!(can_fit);
        }
    }
}
