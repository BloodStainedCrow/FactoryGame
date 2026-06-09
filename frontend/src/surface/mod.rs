use data::{
    max_entity_size,
    spacial::{BoundingBox, Position},
};

use crate::chunk::Chunk;

// TODO: this needs to be a copy of the old chunk store
struct ChunkStore;

struct SurfaceConfiguration {
    surface_generation: (),
    has_global_power: bool,
}

struct Surface {
    chunks: ChunkStore,
}

impl Surface {
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
        vec![todo!()].into_iter()
    }
}
