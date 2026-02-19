use crate::{frontend::world::Position, item::IdxTrait};

impl<ItemIdxType: IdxTrait, RecipeIdxType: IdxTrait> super::Test<ItemIdxType, RecipeIdxType> {
    pub fn assert_tile_empty(&self, pos: (i32, i32)) {
        assert_eq!(
            self.game_state
                .world
                .lock()
                .get_entity_at(Position { x: pos.0, y: pos.1 }, &self.data_store),
            None,
            "{pos:?} was not empty"
        )
    }

    pub fn assert_tile_occupied(&self, pos: (i32, i32)) {
        assert!(
            matches!(
                self.game_state
                    .world
                    .lock()
                    .get_entity_at(Position { x: pos.0, y: pos.1 }, &self.data_store),
                Some(_),
            ),
            "{pos:?} was not occupied"
        )
    }
}
