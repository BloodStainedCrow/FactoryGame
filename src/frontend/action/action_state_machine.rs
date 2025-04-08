use std::{collections::HashSet, marker::PhantomData, sync::mpsc::Receiver};

use log::warn;
use winit::keyboard::KeyCode;

use crate::{
    data::DataStore,
    frontend::{
        action::{
            place_entity::{EntityPlaceOptions, PlaceEntityInfo},
            place_tile::{PlaceFloorTileByHandInfo, PlaceFloorTileGhostInfo},
            set_recipe::SetRecipeInfo,
        },
        input::Input,
        world::{
            tile::{Dir, Entity, FloorTile, PlaceEntityType, World},
            Position,
        },
    },
    item::{IdxTrait, Item, Recipe, WeakIdxTrait},
};

use super::{place_tile::PositionInfo, ActionType, PLAYERID};

const MIN_ZOOM_WIDTH: f32 = 1.0;
pub const WIDTH_PER_LEVEL: usize = 16;

pub struct ActionStateMachine<ItemIdxType: WeakIdxTrait, RecipeIdxType: WeakIdxTrait> {
    pub local_player_pos: (f32, f32),
    pub my_player_id: PLAYERID,

    current_mouse_pos: (f32, f32),
    current_held_keys: HashSet<winit::keyboard::KeyCode>,
    pub state: ActionStateMachineState<ItemIdxType>,

    pub zoom_level: f32,

    recipe: PhantomData<RecipeIdxType>,
}

pub enum ActionStateMachineState<ItemIdxType: WeakIdxTrait> {
    Idle,
    Holding(HeldObject<ItemIdxType>),
    Viewing(Position),
}

pub enum HeldObject<ItemIdxType: WeakIdxTrait> {
    Tile(FloorTile),
    // TODO: PlaceEntityType is not quite right for this case
    Entity(PlaceEntityType<ItemIdxType>),
}

impl<ItemIdxType: IdxTrait, RecipeIdxType: IdxTrait>
    ActionStateMachine<ItemIdxType, RecipeIdxType>
{
    #[must_use]
    pub fn new(my_player_id: PLAYERID, local_player_pos: (f32, f32)) -> Self {
        Self {
            my_player_id,
            local_player_pos,
            current_mouse_pos: (0.0, 0.0),
            current_held_keys: HashSet::new(),
            state: ActionStateMachineState::Idle,

            zoom_level: 1.0,

            recipe: PhantomData,
        }
    }

    #[allow(clippy::too_many_lines)]
    pub fn handle_inputs<'a, 'b, 'c, 'd>(
        &'a mut self,
        input: &'b Receiver<Input>,
        world: &'c World<ItemIdxType, RecipeIdxType>,
        data_store: &'d DataStore<ItemIdxType, RecipeIdxType>,
    ) -> impl IntoIterator<Item = ActionType<ItemIdxType, RecipeIdxType>>
           + use<'a, 'b, 'c, 'd, ItemIdxType, RecipeIdxType> {
        input.try_iter().map(|input| {

            let actions = match input {
                Input::LeftClickPressed => {
                    match &self.state {
                        ActionStateMachineState::Idle => {
                            // TODO: Check if we are hovering over something that can be opened
                            let pos = Self::player_mouse_to_tile(
                                self.zoom_level,
                                self.local_player_pos,
                                self.current_mouse_pos,
                            );

                            let chunk = world.get_chunk_for_tile(pos);

                            if let Some(chunk) = chunk {
                                dbg!(pos);
                                if chunk.get_entity_at(pos, data_store).is_some() {
                                    self.state = ActionStateMachineState::Viewing(pos);
                                }
                            }

                            vec![]
                        },
                        ActionStateMachineState::Holding(held_object) => {
                            // TODO: Check if what we are trying to place would collide

                            match held_object {
                                HeldObject::Tile(floor_tile) => {
                                    vec![ActionType::PlaceFloorTile(PlaceFloorTileByHandInfo {
                                        ghost_info: PlaceFloorTileGhostInfo {
                                            tile: *floor_tile,
                                            position: PositionInfo::Single {
                                                pos: Self::player_mouse_to_tile(
                                                    self.zoom_level,
                                                    self.local_player_pos,
                                                    self.current_mouse_pos,
                                                ),
                                            },
                                        },
                                        player: (),
                                    })]
                                },
                                HeldObject::Entity(place_entity_type) => {
                                    vec![ActionType::PlaceEntity(PlaceEntityInfo {
                                        entities: EntityPlaceOptions::Single(*place_entity_type),
                                    })]
                                },
                            }
                        },
                        ActionStateMachineState::Viewing(Position { x, y }) => {
                            let pos = Self::player_mouse_to_tile(
                                self.zoom_level,
                                self.local_player_pos,
                                self.current_mouse_pos,
                            );

                            let chunk = world.get_chunk_for_tile(pos);

                            if let Some(chunk) = chunk {
                                if chunk.get_entity_at(pos, data_store).is_some() {
                                    self.state = ActionStateMachineState::Viewing(pos);
                                }
                            }

                            vec![]
                        },
                    }
                },
                Input::RightClickPressed => match &self.state {
                    ActionStateMachineState::Idle => vec![],
                    ActionStateMachineState::Holding(_held_object) => {
                        self.state = ActionStateMachineState::Idle;
                        vec![]
                    },
                    ActionStateMachineState::Viewing(_) => {
                        self.state = ActionStateMachineState::Idle;
                        vec![]
                    },
                },
                Input::MouseMove(x, y) => {
                    self.current_mouse_pos = (x, y);

                    match &mut self.state {
                        ActionStateMachineState::Idle | ActionStateMachineState::Viewing(_) => {},
                        ActionStateMachineState::Holding(held_object) => match held_object {
                            HeldObject::Tile(floor_tile) => {},
                            HeldObject::Entity(place_entity_type) => match place_entity_type {
                                PlaceEntityType::Assembler(position) => {
                                    *position = Self::player_mouse_to_tile(
                                        self.zoom_level,
                                        self.local_player_pos,
                                        self.current_mouse_pos,
                                    );
                                },
                                PlaceEntityType::Inserter { filter, pos, dir } => {
                                    *pos = Self::player_mouse_to_tile(
                                        self.zoom_level,
                                        self.local_player_pos,
                                        self.current_mouse_pos,
                                    );
                                },
                                PlaceEntityType::Belt { pos, direction } => {
                                    *pos = Self::player_mouse_to_tile(
                                        self.zoom_level,
                                        self.local_player_pos,
                                        self.current_mouse_pos,
                                    );
                                },
                                PlaceEntityType::PowerPole { pos, ty } => {
                                    *pos = Self::player_mouse_to_tile(
                                        self.zoom_level,
                                        self.local_player_pos,
                                        self.current_mouse_pos,
                                    );
                                },
                                PlaceEntityType::Splitter { pos, direction, in_mode, out_mode } => {
                                    *pos = Self::player_mouse_to_tile(
                                        self.zoom_level,
                                        self.local_player_pos,
                                        self.current_mouse_pos,
                                    );
                                }
                                PlaceEntityType::Chest { pos } => {*pos = Self::player_mouse_to_tile(
                                    self.zoom_level,
                                    self.local_player_pos,
                                    self.current_mouse_pos,
                                );},
                            },
                        },
                    }

                    vec![]
                },
                Input::KeyPress(code) => {
                    if self.current_held_keys.insert(code) {
                        self.handle_start_pressing_key(code, world, data_store)
                            .into_iter()
                            .collect()
                    } else {
                        // This is probably a KeyRepeat from the keyboard while holding a key
                        // warn!("Recieved Key press event for key {:?}, while already pressing the key! Did we miss events?", code);
                        vec![]
                    }
                },
                Input::KeyRelease(code) => {
                    if self.current_held_keys.remove(&code) {
                        self.handle_stop_pressing_key(code).into_iter().collect()
                    } else {
                        warn!("Recieved Key release event for key {:?}, while NOT pressing the key! Did we miss events?", code);
                        vec![]
                    }
                },

                Input::MouseScoll(delta) => {
                    match delta {
                        winit::event::MouseScrollDelta::LineDelta(_, y) => {
                            self.zoom_level -= y / 10.0;
                        },
                        winit::event::MouseScrollDelta::PixelDelta(physical_position) => {
                            self.zoom_level -= physical_position.y as f32 / 10.0;
                        },
                    }
                    if self.zoom_level < MIN_ZOOM_WIDTH {
                        self.zoom_level = MIN_ZOOM_WIDTH;
                    }
                    vec![]
                },

                Input::UnknownInput(..) => {
                    vec![]
                },

                i @ (Input::LeftClickReleased | Input::RightClickReleased) => {
                    dbg!(i);
                    vec![]
                },
            };

            actions
        }).flatten()
    }

    fn handle_start_pressing_key(
        &mut self,
        key: winit::keyboard::KeyCode,
        world: &World<ItemIdxType, RecipeIdxType>,
        data_store: &DataStore<ItemIdxType, RecipeIdxType>,
    ) -> impl IntoIterator<Item = ActionType<ItemIdxType, RecipeIdxType>> {
        match (&self.state, key) {
            (
                ActionStateMachineState::Idle | ActionStateMachineState::Holding(_),
                KeyCode::Digit1,
            ) => {
                self.state =
                    ActionStateMachineState::Holding(HeldObject::Tile(FloorTile::Concrete));
                vec![]
            },
            (
                ActionStateMachineState::Idle | ActionStateMachineState::Holding(_),
                KeyCode::Digit2,
            ) => {
                self.state = ActionStateMachineState::Holding(HeldObject::Entity(
                    PlaceEntityType::Assembler(Self::player_mouse_to_tile(
                        self.zoom_level,
                        self.local_player_pos,
                        self.current_mouse_pos,
                    )),
                ));
                vec![]
            },
            (
                ActionStateMachineState::Idle | ActionStateMachineState::Holding(_),
                KeyCode::Digit3,
            ) => {
                self.state =
                    ActionStateMachineState::Holding(HeldObject::Entity(PlaceEntityType::Belt {
                        pos: Self::player_mouse_to_tile(
                            self.zoom_level,
                            self.local_player_pos,
                            self.current_mouse_pos,
                        ),
                        direction: Dir::North,
                    }));
                vec![]
            },
            (
                ActionStateMachineState::Holding(HeldObject::Entity(PlaceEntityType::Belt {
                    pos,
                    direction,
                })),
                KeyCode::KeyR,
            ) => {
                self.state =
                    ActionStateMachineState::Holding(HeldObject::Entity(PlaceEntityType::Belt {
                        pos: *pos,
                        direction: direction.turn_right(),
                    }));
                vec![]
            },
            (
                ActionStateMachineState::Holding(HeldObject::Entity(PlaceEntityType::Splitter {
                    pos,
                    direction,
                    in_mode,
                    out_mode,
                })),
                KeyCode::KeyR,
            ) => {
                self.state = ActionStateMachineState::Holding(HeldObject::Entity(
                    PlaceEntityType::Splitter {
                        pos: *pos,
                        direction: direction.turn_right(),
                        in_mode: *in_mode,
                        out_mode: *out_mode,
                    },
                ));
                vec![]
            },
            (
                ActionStateMachineState::Idle | ActionStateMachineState::Holding(_),
                KeyCode::Digit4,
            ) => {
                self.state = ActionStateMachineState::Holding(HeldObject::Entity(
                    PlaceEntityType::Inserter {
                        pos: Self::player_mouse_to_tile(
                            self.zoom_level,
                            self.local_player_pos,
                            self.current_mouse_pos,
                        ),
                        dir: Dir::North,
                        // TODO:
                        filter: Item { id: 0.into() },
                    },
                ));
                vec![]
            },
            (
                ActionStateMachineState::Idle | ActionStateMachineState::Holding(_),
                KeyCode::Digit5,
            ) => {
                self.state = ActionStateMachineState::Holding(HeldObject::Entity(
                    PlaceEntityType::PowerPole {
                        pos: Self::player_mouse_to_tile(
                            self.zoom_level,
                            self.local_player_pos,
                            self.current_mouse_pos,
                        ),
                        ty: 0,
                    },
                ));
                vec![]
            },
            (
                ActionStateMachineState::Idle | ActionStateMachineState::Holding(_),
                KeyCode::Digit6,
            ) => {
                self.state = ActionStateMachineState::Holding(HeldObject::Entity(
                    PlaceEntityType::Splitter {
                        pos: Self::player_mouse_to_tile(
                            self.zoom_level,
                            self.local_player_pos,
                            self.current_mouse_pos,
                        ),
                        direction: Dir::North,
                        in_mode: None,
                        out_mode: None,
                    },
                ));
                vec![]
            },
            (
                ActionStateMachineState::Idle | ActionStateMachineState::Holding(_),
                KeyCode::Digit7,
            ) => {
                self.state =
                    ActionStateMachineState::Holding(HeldObject::Entity(PlaceEntityType::Chest {
                        pos: Self::player_mouse_to_tile(
                            self.zoom_level,
                            self.local_player_pos,
                            self.current_mouse_pos,
                        ),
                    }));
                vec![]
            },
            (
                ActionStateMachineState::Holding(HeldObject::Entity(PlaceEntityType::Inserter {
                    pos,
                    dir,
                    filter,
                })),
                KeyCode::KeyR,
            ) => {
                self.state = ActionStateMachineState::Holding(HeldObject::Entity(
                    PlaceEntityType::Inserter {
                        pos: *pos,
                        dir: dir.turn_right(),
                        filter: *filter,
                    },
                ));
                vec![]
            },

            (ActionStateMachineState::Viewing(pos), KeyCode::Digit1) => {
                let chunk = world
                    .get_chunk_for_tile(*pos)
                    .expect("Viewing position out of bounds");
                if matches!(
                    chunk.get_entity_at(*pos, data_store),
                    Some(Entity::Assembler { .. })
                ) {
                    vec![ActionType::SetRecipe(SetRecipeInfo {
                        pos: *pos,
                        recipe: Recipe { id: 0.into() },
                    })]
                } else {
                    vec![]
                }
            },

            (_, _) => vec![],
        }
    }

    fn handle_stop_pressing_key(
        &mut self,
        key: winit::keyboard::KeyCode,
    ) -> impl IntoIterator<Item = ActionType<ItemIdxType, RecipeIdxType>> {
        match (&self.state, key) {
            (_, _) => vec![],
        }
    }

    fn player_mouse_to_tile(
        zoom_level: f32,
        player_pos: (f32, f32),
        mouse_pos: (f32, f32),
    ) -> Position {
        let mouse_pos = (
            ((mouse_pos.0 - 0.5) * (WIDTH_PER_LEVEL as f32)).mul_add(zoom_level, player_pos.0),
            ((mouse_pos.1 - 0.5) * (WIDTH_PER_LEVEL as f32)).mul_add(zoom_level, player_pos.1),
        );

        Position {
            x: mouse_pos.0 as usize,
            y: mouse_pos.1 as usize,
        }
    }

    #[must_use]
    pub fn once_per_update_actions(
        &mut self,
        world: &World<ItemIdxType, RecipeIdxType>,
    ) -> impl IntoIterator<Item = ActionType<ItemIdxType, RecipeIdxType>> {
        let mut actions = Vec::new();

        let mut move_dir = (0, 0);

        if self.current_held_keys.contains(&KeyCode::KeyW) {
            move_dir.1 -= 1;
        }
        if self.current_held_keys.contains(&KeyCode::KeyA) {
            move_dir.0 -= 1;
        }
        if self.current_held_keys.contains(&KeyCode::KeyS) {
            move_dir.1 += 1;
        }
        if self.current_held_keys.contains(&KeyCode::KeyD) {
            move_dir.0 += 1;
        }

        self.local_player_pos.0 +=
            move_dir.0 as f32 * world.players[usize::from(self.my_player_id)].movement_speed;
        self.local_player_pos.1 +=
            move_dir.1 as f32 * world.players[usize::from(self.my_player_id)].movement_speed;

        // TODO: Maybe only send this event if it changed
        actions.push(ActionType::Position(
            self.my_player_id,
            self.local_player_pos,
        ));

        actions
    }
}
