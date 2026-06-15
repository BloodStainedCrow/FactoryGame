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

const fn get_chunk_pos_for_tile(pos: Position) -> [i32; 2] {
    [
        pos.x.div_floor(i32::from(CHUNK_SIZE)),
        pos.y.div_floor(i32::from(CHUNK_SIZE)),
    ]
}

impl SurfaceWorld {
    pub fn new_with_empty_area(area: BoundingBox) -> Self {
        // TODO: This should prob be factored out
        let top_left = get_chunk_pos_for_tile(area.top_left());
        let bottom_right = get_chunk_pos_for_tile(area.bottom_right());

        let x_range: RangeInclusive<i32> = top_left[0]..=bottom_right[0];
        let y_range: RangeInclusive<i32> = top_left[1]..=bottom_right[1];

        Self {
            chunks: DynamicGrid::new_with_filled_grid(
                [*x_range.start(), *y_range.start()],
                [*x_range.end(), *y_range.end()],
                |_| Chunk::empty(),
            ),
        }
    }

    pub fn can_fit(&self, goal_bounding_box: BoundingBox) -> bool {
        if !self.all_chunks_generated(goal_bounding_box) {
            return false;
        }

        self.get_chunks_that_could_contain_entities_colliding_with(
            goal_bounding_box.extend_evenly(max_entity_size()),
        )
        .flat_map(|(chunk, base_pos)| chunk.occupied_bounding_boxes(base_pos))
        .all(|entity_bounding_box| !entity_bounding_box.overlaps(goal_bounding_box))
    }

    fn all_chunks_generated(&self, goal_bounding_box: BoundingBox) -> bool {
        // TODO: This should prob be factored out
        let top_left = get_chunk_pos_for_tile(goal_bounding_box.top_left());
        let bottom_right = get_chunk_pos_for_tile(goal_bounding_box.bottom_right());

        let x_range: RangeInclusive<i32> = top_left[0]..=bottom_right[0];
        let y_range: RangeInclusive<i32> = top_left[1]..=bottom_right[1];

        let Some([generated_horizontal_range, generated_vertical_range]) = self.chunks.get_extent()
        else {
            return false;
        };

        if x_range.start() < generated_horizontal_range.start()
            || x_range.end() > generated_horizontal_range.end()
            || y_range.start() < generated_vertical_range.start()
            || y_range.end() > generated_vertical_range.end()
        {
            return false;
        }

        // Check for non-rectangle generated chunks. Here I think we will just need to iterate
        for (x, y) in x_range.cartesian_product(y_range) {
            if self.chunks.get(x, y).is_none() {
                return false;
            }
        }

        true
    }

    fn get_chunks_that_could_contain_entities_colliding_with(
        &self,
        bounding_box: BoundingBox,
    ) -> impl Iterator<Item = (&Chunk, Position)> {
        // TODO: This should prob be factored out
        let top_left = get_chunk_pos_for_tile(bounding_box.top_left());
        let bottom_right = get_chunk_pos_for_tile(bounding_box.bottom_right());

        let x_range: RangeInclusive<i32> = top_left[0]..=bottom_right[0];
        let y_range: RangeInclusive<i32> = top_left[0]..=bottom_right[1];

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

struct CollisionWithUngeneratedChunks;

#[cfg(test)]
mod test {
    use data::spacial::{Extent, strategies::random_bounding_box};
    use proptest::{prop_assert, proptest};

    use super::*;

    #[test]
    fn create_world_with_empty_area() {
        let _world = SurfaceWorld::new_with_empty_area(BoundingBox::new(
            Position { x: 0, y: 0 },
            Extent {
                width: 0,
                height: 0,
            },
        ));
    }

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
            let world = SurfaceWorld::new_with_empty_area(BoundingBox::new(Position { x: -100_000, y: -100_000 }, Extent { width: 0, height: 0 }));

            let can_fit = world.can_fit(goal);

            prop_assert!(!can_fit);
        }

        #[test]
        fn can_fit_inside_generated(goal in random_bounding_box()) {
            let world = SurfaceWorld::new_with_empty_area(BoundingBox::new(Position { x: -2_000, y: -2_000 }, Extent { width: 4_000, height: 4_000 }));

            let can_fit = world.can_fit(goal);

            prop_assert!(can_fit);
        }
    }
}
