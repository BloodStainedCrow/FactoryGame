use std::iter;

use itertools::Itertools;

use crate::{
    DATA_STORE,
    app_state::GameState,
    data::DataStore,
    frontend::{
        action::{
            ActionType,
            action_state_machine::{
                ActionStateMachine, ActionStateMachineState, HeldObject, WIDTH_PER_LEVEL,
            },
        },
        input,
        world::tile::Dir,
    },
    item::IdxTrait,
    replays::GenerationInformation,
};

mod assert;

pub struct Test<ItemIdxType: IdxTrait = u8, RecipeIdxType: IdxTrait = u8> {
    data_store: DataStore<ItemIdxType, RecipeIdxType>,
    game_state: GameState<ItemIdxType, RecipeIdxType>,
    state_machine: ActionStateMachine<ItemIdxType, RecipeIdxType>,

    action_queue: Vec<ActionType<ItemIdxType, RecipeIdxType>>,
}

impl Default for Test {
    fn default() -> Self {
        let data_store = DATA_STORE.clone();
        let game_state = GameState::new(
            "TEST_GAMESTATE".to_string(),
            GenerationInformation::default(),
            &data_store,
        );
        let state_machine = ActionStateMachine::new_from_gamestate(
            0,
            &*game_state.world.lock(),
            &*game_state.simulation_state.lock(),
            &data_store,
        );
        Self {
            data_store,
            game_state,
            state_machine,

            action_queue: vec![],
        }
    }
}

impl<ItemIdxType: IdxTrait, RecipeIdxType: IdxTrait> Test<ItemIdxType, RecipeIdxType> {
    pub fn clear_hand(&mut self) {
        assert!(
            matches!(
                self.state_machine.state,
                ActionStateMachineState::Holding(_)
            ),
            "Expected to be holding something when calling clear_hand: {:?}",
            self.state_machine.state
        );

        self.state_machine.state = ActionStateMachineState::Idle;
    }

    // FIXME: This should not exist and is a compat hazard
    pub fn hold_bad(&mut self, held: HeldObject<ItemIdxType>) {
        self.state_machine.state = ActionStateMachineState::Holding(held);
    }

    pub fn hold(&mut self, item: &str) {
        let item = ident(item);
        let item = self
            .data_store
            .item_names
            .iter()
            .enumerate()
            .filter_map(|(i, item_name)| item_name.contains(&item).then_some(i))
            .exactly_one()
            .expect(&format!("Could not find exclusive match for {}", item));

        todo!("Get Hand from item {item:?}")
    }

    pub fn rotate_holding(&mut self, goal_dir: Dir) {
        assert!(
            matches!(
                self.state_machine.state,
                ActionStateMachineState::Holding(_)
            ),
            "Expected to be holding something when calling rotate_holding: {:?}",
            self.state_machine.state
        );

        match &mut self.state_machine.state {
            ActionStateMachineState::Holding(held_object) => match held_object {
                crate::frontend::action::action_state_machine::HeldObject::Tile(_) => {
                    unreachable!("Tiles cannot be rotated")
                },
                crate::frontend::action::action_state_machine::HeldObject::Entity(
                    place_entity_type,
                ) => match place_entity_type {
                    crate::frontend::world::tile::PlaceEntityType::Assembler {
                        rotation, ..
                    } => *rotation = goal_dir,
                    crate::frontend::world::tile::PlaceEntityType::Inserter { dir, .. } => {
                        *dir = goal_dir
                    },
                    crate::frontend::world::tile::PlaceEntityType::Belt { direction, .. } => {
                        *direction = goal_dir
                    },
                    crate::frontend::world::tile::PlaceEntityType::Underground {
                        direction,
                        ..
                    } => *direction = goal_dir,
                    crate::frontend::world::tile::PlaceEntityType::PowerPole { .. } => todo!(),
                    crate::frontend::world::tile::PlaceEntityType::Splitter {
                        direction, ..
                    } => *direction = goal_dir,
                    crate::frontend::world::tile::PlaceEntityType::Chest { .. } => todo!(),
                    crate::frontend::world::tile::PlaceEntityType::SolarPanel { .. } => todo!(),
                    crate::frontend::world::tile::PlaceEntityType::Accumulator { .. } => todo!(),
                    crate::frontend::world::tile::PlaceEntityType::Lab { .. } => todo!(),
                    crate::frontend::world::tile::PlaceEntityType::Beacon { .. } => todo!(),
                    crate::frontend::world::tile::PlaceEntityType::FluidTank {
                        rotation, ..
                    } => *rotation = goal_dir,
                    crate::frontend::world::tile::PlaceEntityType::MiningDrill {
                        rotation, ..
                    } => *rotation = goal_dir,
                },
                crate::frontend::action::action_state_machine::HeldObject::OrePlacement {
                    ..
                } => unreachable!("Ore cannot be rotated"),
                crate::frontend::action::action_state_machine::HeldObject::Blueprint(_) => {
                    todo!("Blueprints cannot be rotated")
                },
            },

            _ => unreachable!(),
        }
    }

    pub fn place(&mut self, pos: (i32, i32)) {
        self.click(pos);
        self.tick();
    }

    pub fn click(&mut self, pos: (i32, i32)) {
        self.left_mouse_up();
        self.mouse_to(pos);
        self.left_mouse_down();
        self.left_mouse_up();
    }

    fn left_mouse_down(&mut self) {
        self.handle_input(input::Input::LeftClickPressed {
            shift: false,
            ctrl: false,
        });
    }

    fn left_mouse_up(&mut self) {
        self.handle_input(input::Input::LeftClickReleased);
    }

    pub fn mouse_to(&mut self, pos: (i32, i32)) {
        let screen_pos = tile_to_screen(
            self.state_machine.zoom_level,
            self.state_machine.local_player_pos,
            pos,
        )
        .expect("TODO: Position is offscreen");

        self.handle_input(input::Input::MouseMove(screen_pos.0, screen_pos.1));
    }

    fn right_click(&mut self, pos: (i32, i32)) {
        self.mouse_to(pos);
        self.right_mouse_down();
        self.right_mouse_up();
    }

    pub fn right_mouse_down(&mut self) {
        self.handle_input(input::Input::RightClickPressed { shift: false });
    }

    fn right_mouse_up(&mut self) {
        self.handle_input(input::Input::RightClickReleased);
    }

    fn press(&mut self, key: input::Key) {
        self.handle_input(input::Input::KeyPress(key));
    }
    fn release(&mut self, key: input::Key) {
        self.handle_input(input::Input::KeyRelease(key));
    }

    fn type_key(&mut self, key: input::Key) {
        self.press(key);
        self.release(key);
    }

    fn handle_input(&mut self, input: input::Input) {
        let actions = self
            .state_machine
            .handle_inputs(
                iter::once(input),
                &*self.game_state.world.lock(),
                &*self.game_state.simulation_state.lock(),
                &self.data_store,
            )
            .collect::<Vec<_>>();

        self.action_queue.extend(actions);
    }

    pub fn tick(&mut self) {
        self.tick_many(1);
    }

    pub fn tick_many(&mut self, count: usize) {
        for _ in 0..count {
            self.action_queue.extend(
                self.state_machine
                    .once_per_update_actions(&*self.game_state.world.lock(), &self.data_store),
            );

            GameState::apply_actions(
                &mut *self.game_state.simulation_state.lock(),
                &mut *self.game_state.world.lock(),
                self.action_queue.drain(..),
                &self.data_store,
            );

            GameState::update(
                &mut *self.game_state.simulation_state.lock(),
                &mut *self.game_state.aux_data.lock(),
                &self.data_store,
            );
        }
    }
}

impl<ItemIdxType: IdxTrait, RecipeIdxType: IdxTrait> Drop for Test<ItemIdxType, RecipeIdxType> {
    fn drop(&mut self) {
        self.tick_many(20);
    }
}

fn tile_to_screen(zoom_level: f32, camera_pos: (f32, f32), pos: (i32, i32)) -> Option<(f32, f32)> {
    let middle = (pos.0 as f32 + 0.5, pos.1 as f32 + 0.5);

    let camera_space = (middle.0 - camera_pos.0, middle.1 - camera_pos.1);

    let mouse_pos = (
        camera_space.0 / 1.5f32.powf(zoom_level) / WIDTH_PER_LEVEL as f32,
        camera_space.1 / 1.5f32.powf(zoom_level) / WIDTH_PER_LEVEL as f32,
    );

    Some(mouse_pos)
}

fn ident(s: &str) -> String {
    use convert_case::Casing;
    s.to_case(convert_case::Case::Snake)
}

#[cfg(test)]
mod test {
    use super::*;
    use crate::{
        blueprint::test::random_position, frontend::action::action_state_machine::WIDTH_PER_LEVEL,
    };
    use proptest::*;

    pub fn player_mouse_to_tile(
        zoom_level: f32,
        camera_pos: (f32, f32),
        mouse_pos: (f32, f32),
    ) -> (i32, i32) {
        let mouse_pos = (
            ((mouse_pos.0) * (WIDTH_PER_LEVEL as f32))
                .mul_add(1.5f32.powf(zoom_level), camera_pos.0),
            ((mouse_pos.1) * (WIDTH_PER_LEVEL as f32))
                .mul_add(1.5f32.powf(zoom_level), camera_pos.1),
        );

        (mouse_pos.0.floor() as i32, mouse_pos.1.floor() as i32)
    }

    proptest! {

        #[test]
        fn mouse_transform_identity(pos in random_position(), zoom_level in 0.0f32..10.0, camera_pos in (-100.0f32..=100.0, -100.0f32..=100.0)) {
            let mouse_pos = tile_to_screen(zoom_level, camera_pos, (pos.x, pos.y));

            prop_assume!(mouse_pos.is_some());
            let mouse_pos = mouse_pos.expect("Assumed");

            let pos_res = player_mouse_to_tile(zoom_level, camera_pos, mouse_pos);

            prop_assert_eq!((pos.x, pos.y), pos_res);
        }

    }
}
