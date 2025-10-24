use std::{
    cmp::{max, min},
    collections::{HashMap, HashSet},
    marker::PhantomData,
    num::NonZero,
};

use egui_graphs::{DefaultEdgeShape, DefaultNodeShape, Graph};
use itertools::Itertools;
use log::{error, warn};
use petgraph::Directed;

use crate::{
    app_state::SimulationState,
    belt::splitter::SplitterDistributionMode,
    blueprint::Blueprint,
    data::{self, DataStore},
    frontend::{
        action::{
            place_entity::{EntityPlaceOptions, PlaceEntityInfo},
            place_tile::{PlaceFloorTileByHandInfo, PlaceFloorTileGhostInfo},
            set_recipe::SetRecipeInfo,
        },
        input::{Input, Key},
        world::{
            Position,
            tile::{AssemblerInfo, Dir, Entity, FloorTile, PlaceEntityType, UndergroundDir, World},
        },
    },
    get_size::RamUsage,
    item::{ITEMCOUNTTYPE, IdxTrait, Item, Recipe, WeakIdxTrait},
    rendering::render_world::SWITCH_TO_MAPVIEW_ZOOM_LEVEL,
};

use super::{ActionType, PLAYERID, place_tile::PositionInfo};

const MAP_VIEW_PAN_SPEED: f32 = 0.05;

const MIN_ZOOM_WIDTH: f32 = 1.0;
pub const WIDTH_PER_LEVEL: usize = 16;

#[derive(Debug)]
pub struct ActionStateMachine<ItemIdxType: WeakIdxTrait, RecipeIdxType: WeakIdxTrait> {
    pub local_player_pos: (f32, f32),
    pub my_player_id: PLAYERID,

    pub statistics_panel_open: bool,
    pub technology_panel_open: bool,
    pub tech_tree_render: Option<
        Graph<
            data::Technology<RecipeIdxType>,
            (),
            Directed,
            u16,
            DefaultNodeShape,
            DefaultEdgeShape,
        >,
    >,
    pub statistics_panel: StatisticsPanel,
    pub production_filters: Vec<bool>,
    pub consumption_filters: Vec<bool>,

    pub current_mouse_pos: (f32, f32),
    current_held_keys: HashSet<Key>,
    pub state: ActionStateMachineState<ItemIdxType>,

    pub escape_menu_open: bool,

    pub debug_view_options: DebugViewOptions,

    pub zoom_level: f32,
    pub map_view_info: Option<(f32, f32)>,

    copy_info: Option<CopyInfo<ItemIdxType, RecipeIdxType>>,

    pub show_graph_dot_output: bool,

    recipe: PhantomData<RecipeIdxType>,

    // #[serde(skip)]
    pub get_size_cache: std::collections::HashMap<String, RamUsage>,

    pub mouse_wheel_sensitivity: f32,

    pub current_fork_save_in_progress: Option<ForkSaveInfo>,
}

#[derive(Debug)]
pub struct ForkSaveInfo {
    pub recv: interprocess::unnamed_pipe::Recver,
    pub current_state: u8,
}

#[derive(Debug)]
pub struct DebugViewOptions {
    pub highlight_sushi_belts: bool,
    pub sushi_belt_len_threshhold: u32,

    pub sushi_finder_view_lock: Option<usize>,
}

#[derive(Debug)]
pub enum CopyInfo<ItemIdxType: WeakIdxTrait, RecipeIdxType: WeakIdxTrait> {
    Recipe {
        recipe: Recipe<RecipeIdxType>,
    },
    ChestLimit {
        num_slots: u8,
    },
    SplitterSetting {
        distribution_mode: SplitterDistributionMode,
    },
    InserterSettings {
        user_movetime: Option<NonZero<u16>>,
        max_stack_size: Option<ITEMCOUNTTYPE>,
        filter: Option<Item<ItemIdxType>>,
    },
}

#[derive(Debug, PartialEq)]
pub enum StatisticsPanel {
    Items(usize),
    Fluids(usize),
}

impl Default for StatisticsPanel {
    fn default() -> Self {
        Self::Items(2)
    }
}

#[derive(Debug)]
pub enum ActionStateMachineState<ItemIdxType: WeakIdxTrait> {
    Idle,
    Deconstructing(Position, u32),
    Holding(HeldObject<ItemIdxType>),
    Viewing(Position),

    CtrlCPressed,
    CopyDragInProgress { start_pos: Position },

    DelPressed,
    DeleteDragInProgress { start_pos: Position },
}

#[derive(Debug)]
pub enum HeldObject<ItemIdxType: WeakIdxTrait> {
    Tile(FloorTile),
    // TODO: PlaceEntityType is not quite right for this case
    Entity(PlaceEntityType<ItemIdxType>),

    Blueprint(Blueprint),
}

impl<ItemIdxType: IdxTrait, RecipeIdxType: IdxTrait>
    ActionStateMachine<ItemIdxType, RecipeIdxType>
{
    #[must_use]
    pub fn new(
        my_player_id: PLAYERID,
        local_player_pos: (f32, f32),
        data_store: &DataStore<ItemIdxType, RecipeIdxType>,
    ) -> Self {
        Self {
            my_player_id,
            local_player_pos,

            tech_tree_render: None,

            statistics_panel_open: false,
            technology_panel_open: true,
            statistics_panel: StatisticsPanel::default(),
            production_filters: vec![true; data_store.item_display_names.len()],
            consumption_filters: vec![true; data_store.item_display_names.len()],

            current_mouse_pos: (0.0, 0.0),
            current_held_keys: HashSet::new(),
            state: ActionStateMachineState::Idle,

            zoom_level: 1.0,
            map_view_info: None,

            escape_menu_open: false,
            debug_view_options: DebugViewOptions {
                highlight_sushi_belts: false,
                sushi_belt_len_threshhold: 1,

                sushi_finder_view_lock: None,
            },

            copy_info: None,

            show_graph_dot_output: false,

            recipe: PhantomData,

            get_size_cache: HashMap::new(),

            mouse_wheel_sensitivity: 1.0,

            current_fork_save_in_progress: None,
        }
    }

    #[allow(clippy::too_many_lines)]
    pub fn handle_inputs<'a, 'b, 'c, 'd, 'e, I: IntoIterator<Item = Input> + 'b>(
        &'a mut self,
        input: I,
        world: &'c World<ItemIdxType, RecipeIdxType>,
        sim_state: &'e SimulationState<ItemIdxType, RecipeIdxType>,
        data_store: &'d DataStore<ItemIdxType, RecipeIdxType>,
    ) -> impl Iterator<Item = ActionType<ItemIdxType, RecipeIdxType>>
    + use<'a, 'b, 'c, 'd, 'e, ItemIdxType, RecipeIdxType, I> {
        input.into_iter().map(|input| {
            if self.escape_menu_open && input != Input::KeyPress(Key::Esc) {
                match input {
                    Input::KeyPress(key) => {
                        self.current_held_keys.insert(key);
                    },
                    Input::KeyRelease(key) => {
                        self.current_held_keys.remove(&key);
                    },
                    _ => {}
                }

                return vec![];
            }

            let actions = match input {
                Input::Copy => {
                    self.state = ActionStateMachineState::CtrlCPressed;
                    vec![]
                },
                Input::LeftClickPressed { shift, ctrl } => {
                    if shift {
                        if let Some(copy_info) = &self.copy_info {
                            let pos = Self::player_mouse_to_tile(
                                self.zoom_level,
                                self.map_view_info.unwrap_or(self.local_player_pos),
                                self.current_mouse_pos,
                            );
                            match world.get_entity_at(pos, data_store) { Some(e) => {
                                match (e, copy_info) {
                                    (Entity::Assembler { .. }, CopyInfo::Recipe { recipe }) => {
                                        vec![ActionType::SetRecipe(SetRecipeInfo { pos, recipe: *recipe })]
                                    },
                                    (Entity::Chest { .. }, CopyInfo::ChestLimit { num_slots }) => {
                                        vec![ActionType::SetChestSlotLimit { pos, num_slots: *num_slots }]
                                    }
                                    (Entity::Inserter { .. }, CopyInfo::InserterSettings { user_movetime, max_stack_size, filter }) => {
                                        // TODO: Add the rest
                                        vec![ActionType::OverrideInserterMovetime { pos, new_movetime: *user_movetime }]
                                    }
                                    (Entity::Splitter { .. }, CopyInfo::SplitterSetting { distribution_mode }) => todo!(),
                                    (_, _) => {
                                        vec![]
                                    }
                                }
                            } _ => {
                                vec![]
                            }}
                        } else {
                            vec![]
                        }
                    } else {
                        match &mut self.state {
                            ActionStateMachineState::Idle => {
                                // TODO: Check if we are hovering over something that can be opened
                                let pos = Self::player_mouse_to_tile(
                                    self.zoom_level,
                                    self.map_view_info.unwrap_or(self.local_player_pos),
                                    self.current_mouse_pos,
                                );

                                if let Some(e) = world.get_entity_at(pos, data_store) {
                                    self.state = ActionStateMachineState::Viewing(e.get_pos());
                                }

                                vec![]
                            },

                            ActionStateMachineState::CtrlCPressed => {
                                self.state = ActionStateMachineState::CopyDragInProgress {
                                    start_pos: Self::player_mouse_to_tile(
                                        self.zoom_level,
                                        self.map_view_info.unwrap_or(self.local_player_pos),
                                        self.current_mouse_pos,
                                    )
                                };

                                vec![]
                            },

                            ActionStateMachineState::CopyDragInProgress { start_pos: _ } => {
                                warn!("Tried starting CopyDraw again!");
                                vec![]
                            },
                            ActionStateMachineState::DelPressed => {
                                self.state = ActionStateMachineState::DeleteDragInProgress {
                                    start_pos: Self::player_mouse_to_tile(
                                        self.zoom_level,
                                        self.map_view_info.unwrap_or(self.local_player_pos),
                                        self.current_mouse_pos,
                                    )
                                };

                                vec![]
                            },

                            ActionStateMachineState::DeleteDragInProgress { start_pos: _ } => {
                                warn!("Tried starting DeleteDraw again!");
                                vec![]
                            },

                            ActionStateMachineState::Holding(held_object) => {
                                // TODO: Check if what we are trying to place would collide

                                let force = ctrl;

                                match held_object {
                                    HeldObject::Blueprint(bp) => {
                                        bp.get_reusable(force, data_store).actions_with_base_pos(Self::player_mouse_to_tile(
                                            self.zoom_level,
                                            self.map_view_info.unwrap_or(self.local_player_pos),
                                            self.current_mouse_pos,
                                        )).collect()
                                    },

                                    HeldObject::Tile(floor_tile) => {
                                        vec![ActionType::PlaceFloorTile(PlaceFloorTileByHandInfo {
                                            ghost_info: PlaceFloorTileGhostInfo {
                                                tile: *floor_tile,
                                                position: PositionInfo::Single {
                                                    pos: Self::player_mouse_to_tile(
                                                        self.zoom_level,
                                                        self.map_view_info.unwrap_or(self.local_player_pos),
                                                        self.current_mouse_pos,
                                                    ),
                                                },
                                            },
                                            player: (),
                                        })]
                                    },
                                    HeldObject::Entity(place_entity_type) => {
                                        let ret = vec![ActionType::PlaceEntity(PlaceEntityInfo {
                                            force,
                                            entities: EntityPlaceOptions::Single(place_entity_type.clone()),
                                        })];

                                        if let PlaceEntityType::Underground { underground_dir, .. } = place_entity_type {
                                            match *underground_dir {
                                                UndergroundDir::Entrance => *underground_dir = UndergroundDir::Exit,
                                                UndergroundDir::Exit => *underground_dir = UndergroundDir::Entrance,
                                            }

                                        }

                                        ret
                                    },
                                }
                            },
                            ActionStateMachineState::Viewing(Position { .. }) => {
                                let pos = Self::player_mouse_to_tile(
                                    self.zoom_level,
                                    self.map_view_info.unwrap_or(self.local_player_pos),
                                    self.current_mouse_pos,
                                );

                                if let Some(e) = world.get_entity_at(pos, data_store) {
                                    self.state = ActionStateMachineState::Viewing(e.get_pos());
                                }

                                vec![]
                            },
                            ActionStateMachineState::Deconstructing(_, _) => {
                                self.state = ActionStateMachineState::Idle;
                                vec![]
                            }
                        }
                    }
                },
                Input::RightClickPressed { shift} => {


                    if shift {
                        let pos = Self::player_mouse_to_tile(
                            self.zoom_level,
                            self.map_view_info.unwrap_or(self.local_player_pos),
                            self.current_mouse_pos,
                        );
                        if let Some(e) = world.get_entity_at(pos, data_store) {
                            match e {
                                Entity::Assembler { info, .. } => match info {
                                    AssemblerInfo::UnpoweredNoRecipe => {},
                                    AssemblerInfo::Unpowered(recipe) => self.copy_info = Some(CopyInfo::Recipe { recipe: *recipe }),
                                    AssemblerInfo::PoweredNoRecipe(_) => {},
                                    AssemblerInfo::Powered { id, .. } => self.copy_info = Some(CopyInfo::Recipe { recipe: id.recipe }),
                                },
                                Entity::PowerPole { .. } => {},
                                Entity::Belt { .. } => {},
                                Entity::Underground { .. } => {},
                                Entity::Splitter { .. } => todo!(),
                                Entity::Inserter { user_movetime, filter, .. } => {
                                    self.copy_info = Some(CopyInfo::InserterSettings { max_stack_size: None, filter: *filter, user_movetime: *user_movetime });
                                },
                                Entity::Chest { slot_limit, .. } => {
                                    self.copy_info = Some(CopyInfo::ChestLimit { num_slots: *slot_limit });
                                },
                                Entity::Roboport { .. } => todo!(),
                                Entity::SolarPanel { .. } => {},
                                Entity::Lab { .. } => {},
                                Entity::Beacon { .. } => {},
                                Entity::FluidTank { .. } => {},
                                Entity::MiningDrill { .. } => {},
                            }
                        }
                        vec![]
                    } else {
                        match &self.state {
                            ActionStateMachineState::Idle => {
                                let pos = Self::player_mouse_to_tile(
                                    self.zoom_level,
                                    self.map_view_info.unwrap_or(self.local_player_pos),
                                    self.current_mouse_pos,
                                );
                                if world.get_entity_at(pos, data_store).is_some() {
                                    self.state = ActionStateMachineState::Deconstructing(pos, 30);
                                }
                                vec![]
                            },
                            ActionStateMachineState::Holding(_held_object) => {
                                self.state = ActionStateMachineState::Idle;
                                vec![]
                            },
                            ActionStateMachineState::Viewing(_) => {
                                self.state = ActionStateMachineState::Idle;
                                vec![]
                            },
                            ActionStateMachineState::Deconstructing(_, _) => vec![],
                            ActionStateMachineState::CtrlCPressed | ActionStateMachineState::CopyDragInProgress { start_pos: _ } => {
                                self.state = ActionStateMachineState::Idle;
                                vec![]
                            },
                            ActionStateMachineState::DelPressed | ActionStateMachineState::DeleteDragInProgress { start_pos: _ } => {
                                self.state = ActionStateMachineState::Idle;
                                vec![]
                            },
                        }
                    }

                }


                ,
                Input::LeftClickReleased => {
                    match self.state {
                        ActionStateMachineState::Idle => {vec![]},
                        ActionStateMachineState::Deconstructing(_, _) => {vec![]},
                        ActionStateMachineState::Holding(_) => {vec![]},
                        ActionStateMachineState::Viewing(_) => {vec![]},
                        ActionStateMachineState::CtrlCPressed => {vec![]},
                        ActionStateMachineState::DelPressed => {vec![]},
                        ActionStateMachineState::CopyDragInProgress { start_pos } => {
                            let end_pos = Self::player_mouse_to_tile(
                                self.zoom_level,
                                self.map_view_info.unwrap_or(self.local_player_pos),
                                self.current_mouse_pos,
                            );

                            let x_range = min(start_pos.x, end_pos.x)..(max(start_pos.x, end_pos.x) + 1);
                            let y_range = min(start_pos.y, end_pos.y)..(max(start_pos.y, end_pos.y) + 1);

                            self.state = ActionStateMachineState::Holding(HeldObject::Blueprint(Blueprint::from_area(world, sim_state, [x_range, y_range], data_store)));
                    vec![]
                },
                        ActionStateMachineState::DeleteDragInProgress { start_pos } => {
                            let end_pos = Self::player_mouse_to_tile(
                                self.zoom_level,
                                self.map_view_info.unwrap_or(self.local_player_pos),
                                self.current_mouse_pos,
                            );

                            let x_range = min(start_pos.x, end_pos.x)..(max(start_pos.x, end_pos.x) + 1);
                            let y_range = min(start_pos.y, end_pos.y)..(max(start_pos.y, end_pos.y) + 1);

                            self.state = ActionStateMachineState::Idle;

                            x_range.cartesian_product(y_range).map(|(x, y)| Position {x, y}).map(|pos| ActionType::Remove(pos)).collect()
                        },
                    }
                },
                Input::RightClickReleased => match &self.state {
                    ActionStateMachineState::Deconstructing(_, _) => {
                        self.state = ActionStateMachineState::Idle;
                        vec![]
                    },
                    _ => vec![]
                },
                Input::MouseMove(x, y) => {
                    self.current_mouse_pos = (x, y);

                    match &mut self.state {
                        ActionStateMachineState::CtrlCPressed | ActionStateMachineState::CopyDragInProgress { start_pos:_ } => {},
                        ActionStateMachineState::DelPressed | ActionStateMachineState::DeleteDragInProgress { start_pos:_ } => {},

                        ActionStateMachineState::Idle | ActionStateMachineState::Viewing(_) => {},
                        ActionStateMachineState::Holding(held_object) => match held_object {
                            HeldObject::Blueprint(_) => {},

                            HeldObject::Tile(_floor_tile) => {},
                            HeldObject::Entity(place_entity_type) => match place_entity_type {
                                PlaceEntityType::Assembler {
                                                                pos: position,
                                                                ty: _,
                                                                rotation: _, ..
                                                            } => {
                                                                *position = Self::player_mouse_to_tile(
                                                                    self.zoom_level,
                                                                    self.map_view_info.unwrap_or(self.local_player_pos),
                                                                    self.current_mouse_pos,
                                                                );
                                                            },
                                PlaceEntityType::Inserter { pos, dir: _, filter: _, ty: _, .. } => {
                                                                *pos = Self::player_mouse_to_tile(
                                                                    self.zoom_level,
                                                                    self.map_view_info.unwrap_or(self.local_player_pos),
                                                                    self.current_mouse_pos,
                                                                );
                                                            },
                                PlaceEntityType::Belt { pos, ty: _, direction: _ } => {
                                                                *pos = Self::player_mouse_to_tile(
                                                                    self.zoom_level,
                                                                    self.map_view_info.unwrap_or(self.local_player_pos),
                                                                    self.current_mouse_pos,
                                                                );
                                                            },
                                                    PlaceEntityType::Underground { pos, ty: _, direction: _, underground_dir: _ } => {
                                                        *pos = Self::player_mouse_to_tile(
                                                            self.zoom_level,
                                                            self.map_view_info.unwrap_or(self.local_player_pos),
                                                            self.current_mouse_pos,
                                                        );
                                                    },
                                PlaceEntityType::PowerPole { pos, ty: _ } => {
                                                                *pos = Self::player_mouse_to_tile(
                                                                    self.zoom_level,
                                                                    self.map_view_info.unwrap_or(self.local_player_pos),
                                                                    self.current_mouse_pos,
                                                                );
                                                            },
                                PlaceEntityType::Splitter { pos, direction: _, ty: _, in_mode: _, out_mode: _ } => {
                                                                *pos = Self::player_mouse_to_tile(
                                                                    self.zoom_level,
                                                                    self.map_view_info.unwrap_or(self.local_player_pos),
                                                                    self.current_mouse_pos,
                                                                );
                                                            },
                                PlaceEntityType::Chest { pos, .. } => {*pos = Self::player_mouse_to_tile(
                                                                self.zoom_level,
                                                                self.map_view_info.unwrap_or(self.local_player_pos),
                                                                self.current_mouse_pos,
                                                            );},
                                PlaceEntityType::SolarPanel { pos, ty: _ } => {*pos = Self::player_mouse_to_tile(
                                                                self.zoom_level,
                                                                self.map_view_info.unwrap_or(self.local_player_pos),
                                                                self.current_mouse_pos,
                                                            );},
                                PlaceEntityType::Lab { pos, ty: _, .. } => {*pos = Self::player_mouse_to_tile(
                                                                self.zoom_level,
                                                                self.map_view_info.unwrap_or(self.local_player_pos),
                                                                self.current_mouse_pos,
                                                            );},
                                PlaceEntityType::Beacon { ty: _, pos, .. } => {*pos = Self::player_mouse_to_tile(
                                    self.zoom_level,
                                    self.map_view_info.unwrap_or(self.local_player_pos),
                                    self.current_mouse_pos,
                                );},
                                PlaceEntityType::FluidTank { ty: _, pos, rotation: _  } => {*pos = Self::player_mouse_to_tile(
                                    self.zoom_level,
                                    self.map_view_info.unwrap_or(self.local_player_pos),
                                    self.current_mouse_pos,
                                );},
                                PlaceEntityType::MiningDrill { ty: _, pos, rotation: _  } => {*pos = Self::player_mouse_to_tile(
                                    self.zoom_level,
                                    self.map_view_info.unwrap_or(self.local_player_pos),
                                    self.current_mouse_pos,
                                );},
                            },
                        },
                        ActionStateMachineState::Deconstructing(position, timer) =>{
                            //todo!("Check if we are still over the same thing")
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
                    let old_width = WIDTH_PER_LEVEL as f32 * 1.5f32.powf(self.zoom_level);
                    let Position {x: mouse_x, y: mouse_y} = Self::player_mouse_to_tile(self.zoom_level, self.map_view_info.unwrap_or(self.local_player_pos), self.current_mouse_pos);
                    match delta {
                        (_, y) => {
                            self.zoom_level -=  y as f32 / 10.0 * self.mouse_wheel_sensitivity;
                        },
                    }
                    if let Some(view_center) = &mut self.map_view_info {
                        let new_width = WIDTH_PER_LEVEL as f32 * 1.5f32.powf(self.zoom_level);

                        let new_center_x = ((old_width - new_width) / old_width) * (mouse_x as f32 - view_center.0) + view_center.0;
                        let new_center_y = ((old_width - new_width) / old_width) * (mouse_y as f32 - view_center.1) + view_center.1;

                        *view_center = (new_center_x, new_center_y);
                    } else {
                        self.zoom_level = if self.zoom_level < *SWITCH_TO_MAPVIEW_ZOOM_LEVEL {
                            self.zoom_level
                        } else {
                            *SWITCH_TO_MAPVIEW_ZOOM_LEVEL
                        };
                    }
                    if self.zoom_level < MIN_ZOOM_WIDTH {
                        self.zoom_level = MIN_ZOOM_WIDTH;
                    }
                    vec![]
                },
                Input::UnknownInput => {
                    error!("Encountered unknown input");
                    vec![]
                }
            };

            actions
        }).flatten()
    }

    fn handle_start_pressing_key(
        &mut self,
        key: Key,
        world: &World<ItemIdxType, RecipeIdxType>,
        data_store: &DataStore<ItemIdxType, RecipeIdxType>,
    ) -> impl Iterator<Item = ActionType<ItemIdxType, RecipeIdxType>> + use<ItemIdxType, RecipeIdxType>
    {
        let ret = match (&mut self.state, key) {
            (ActionStateMachineState::Idle | ActionStateMachineState::Holding(_), Key::Q) => {
                match world.get_entity_at(
                    Self::player_mouse_to_tile(
                        self.zoom_level,
                        self.map_view_info.unwrap_or(self.local_player_pos),
                        self.current_mouse_pos,
                    ),
                    data_store,
                ) {
                    Some(Entity::MiningDrill { ty, rotation, .. }) => {
                        self.state = ActionStateMachineState::Holding(HeldObject::Entity(
                            PlaceEntityType::MiningDrill {
                                pos: Self::player_mouse_to_tile(
                                    self.zoom_level,
                                    self.map_view_info.unwrap_or(self.local_player_pos),
                                    self.current_mouse_pos,
                                ),
                                ty: *ty,
                                rotation: *rotation,
                            },
                        ));
                    },
                    Some(Entity::Assembler { ty, rotation, .. }) => {
                        self.state = ActionStateMachineState::Holding(HeldObject::Entity(
                            PlaceEntityType::Assembler {
                                pos: Self::player_mouse_to_tile(
                                    self.zoom_level,
                                    self.map_view_info.unwrap_or(self.local_player_pos),
                                    self.current_mouse_pos,
                                ),
                                ty: *ty,
                                rotation: *rotation,
                            },
                        ));
                    },
                    Some(Entity::Beacon { ty, .. }) => {
                        self.state = ActionStateMachineState::Holding(HeldObject::Entity(
                            PlaceEntityType::Beacon {
                                pos: Self::player_mouse_to_tile(
                                    self.zoom_level,
                                    self.map_view_info.unwrap_or(self.local_player_pos),
                                    self.current_mouse_pos,
                                ),
                                ty: *ty,
                            },
                        ));
                    },
                    Some(Entity::Belt { direction, ty, .. }) => {
                        self.state = ActionStateMachineState::Holding(HeldObject::Entity(
                            PlaceEntityType::Belt {
                                pos: Self::player_mouse_to_tile(
                                    self.zoom_level,
                                    self.map_view_info.unwrap_or(self.local_player_pos),
                                    self.current_mouse_pos,
                                ),
                                ty: *ty,
                                direction: *direction,
                            },
                        ));
                    },
                    Some(Entity::Chest { ty, .. }) => {
                        self.state = ActionStateMachineState::Holding(HeldObject::Entity(
                            PlaceEntityType::Chest {
                                pos: Self::player_mouse_to_tile(
                                    self.zoom_level,
                                    self.map_view_info.unwrap_or(self.local_player_pos),
                                    self.current_mouse_pos,
                                ),
                                ty: *ty,
                            },
                        ));
                    },
                    Some(Entity::FluidTank { ty, rotation, .. }) => {
                        self.state = ActionStateMachineState::Holding(HeldObject::Entity(
                            PlaceEntityType::FluidTank {
                                pos: Self::player_mouse_to_tile(
                                    self.zoom_level,
                                    self.map_view_info.unwrap_or(self.local_player_pos),
                                    self.current_mouse_pos,
                                ),
                                ty: *ty,
                                rotation: *rotation,
                            },
                        ));
                    },
                    Some(Entity::Inserter {
                        direction,
                        filter,
                        ty,
                        ..
                    }) => {
                        self.state = ActionStateMachineState::Holding(HeldObject::Entity(
                            PlaceEntityType::Inserter {
                                pos: Self::player_mouse_to_tile(
                                    self.zoom_level,
                                    self.map_view_info.unwrap_or(self.local_player_pos),
                                    self.current_mouse_pos,
                                ),
                                dir: *direction,
                                filter: *filter,
                                ty: *ty,
                                user_movetime: None,
                            },
                        ));
                    },
                    Some(Entity::Lab { ty, .. }) => {
                        self.state = ActionStateMachineState::Holding(HeldObject::Entity(
                            PlaceEntityType::Lab {
                                pos: Self::player_mouse_to_tile(
                                    self.zoom_level,
                                    self.map_view_info.unwrap_or(self.local_player_pos),
                                    self.current_mouse_pos,
                                ),
                                ty: *ty,
                            },
                        ));
                    },
                    Some(Entity::PowerPole { ty, .. }) => {
                        self.state = ActionStateMachineState::Holding(HeldObject::Entity(
                            PlaceEntityType::PowerPole {
                                pos: Self::player_mouse_to_tile(
                                    self.zoom_level,
                                    self.map_view_info.unwrap_or(self.local_player_pos),
                                    self.current_mouse_pos,
                                ),
                                ty: *ty,
                            },
                        ));
                    },
                    Some(Entity::Roboport { ty, .. }) => todo!(),
                    Some(Entity::SolarPanel {
                        pos: _,
                        ty,
                        pole_position: _,
                    }) => {
                        self.state = ActionStateMachineState::Holding(HeldObject::Entity(
                            PlaceEntityType::SolarPanel {
                                pos: Self::player_mouse_to_tile(
                                    self.zoom_level,
                                    self.map_view_info.unwrap_or(self.local_player_pos),
                                    self.current_mouse_pos,
                                ),
                                ty: *ty,
                            },
                        ));
                    },
                    Some(Entity::Splitter { direction, .. }) => {
                        self.state = ActionStateMachineState::Holding(HeldObject::Entity(
                            PlaceEntityType::Splitter {
                                pos: Self::player_mouse_to_tile(
                                    self.zoom_level,
                                    self.map_view_info.unwrap_or(self.local_player_pos),
                                    self.current_mouse_pos,
                                ),
                                // FIXME:
                                ty: 0,
                                direction: *direction,
                                // TODO:
                                in_mode: None,
                                out_mode: None,
                            },
                        ));
                    },
                    Some(Entity::Underground {
                        underground_dir,
                        direction,
                        ty,
                        ..
                    }) => {
                        self.state = ActionStateMachineState::Holding(HeldObject::Entity(
                            PlaceEntityType::Underground {
                                pos: Self::player_mouse_to_tile(
                                    self.zoom_level,
                                    self.map_view_info.unwrap_or(self.local_player_pos),
                                    self.current_mouse_pos,
                                ),
                                ty: *ty,
                                direction: *direction,
                                underground_dir: *underground_dir,
                            },
                        ));
                    },
                    None => {},
                }

                vec![]
            },

            (ActionStateMachineState::Idle | ActionStateMachineState::Holding(_), Key::Key1) => {
                self.state = ActionStateMachineState::Holding(HeldObject::Entity(
                    PlaceEntityType::Splitter {
                        ty: 0,
                        pos: Self::player_mouse_to_tile(
                            self.zoom_level,
                            self.map_view_info.unwrap_or(self.local_player_pos),
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
                ActionStateMachineState::Holding(HeldObject::Entity(PlaceEntityType::Assembler {
                    pos,
                    ty,
                    rotation,
                })),
                Key::R,
            ) => {
                self.state = ActionStateMachineState::Holding(HeldObject::Entity(
                    PlaceEntityType::Assembler {
                        pos: *pos,
                        ty: *ty,
                        rotation: rotation.turn_right(),
                    },
                ));
                vec![]
            },
            (
                ActionStateMachineState::Holding(HeldObject::Entity(PlaceEntityType::Assembler {
                    pos,
                    ty,
                    rotation,
                })),
                Key::Key2,
            ) => {
                self.state = ActionStateMachineState::Holding(HeldObject::Entity(
                    PlaceEntityType::Assembler {
                        pos: *pos,
                        ty: (*ty + 1) % data_store.assembler_info.len() as u8,
                        rotation: *rotation,
                    },
                ));
                vec![]
            },
            (ActionStateMachineState::Idle | ActionStateMachineState::Holding(_), Key::Key2) => {
                self.state = ActionStateMachineState::Holding(HeldObject::Entity(
                    PlaceEntityType::Assembler {
                        pos: Self::player_mouse_to_tile(
                            self.zoom_level,
                            self.map_view_info.unwrap_or(self.local_player_pos),
                            self.current_mouse_pos,
                        ),
                        // TODO:
                        ty: 0,
                        rotation: Dir::North,
                    },
                ));
                vec![]
            },
            (ActionStateMachineState::Idle | ActionStateMachineState::Holding(_), Key::Key3) => {
                self.state =
                    ActionStateMachineState::Holding(HeldObject::Entity(PlaceEntityType::Belt {
                        pos: Self::player_mouse_to_tile(
                            self.zoom_level,
                            self.map_view_info.unwrap_or(self.local_player_pos),
                            self.current_mouse_pos,
                        ),
                        direction: Dir::North,
                        ty: 0,
                    }));
                vec![]
            },
            (
                ActionStateMachineState::Holding(HeldObject::Entity(PlaceEntityType::Belt {
                    pos,
                    direction,
                    ty,
                })),
                Key::R,
            ) => {
                self.state =
                    ActionStateMachineState::Holding(HeldObject::Entity(PlaceEntityType::Belt {
                        pos: *pos,
                        direction: direction.turn_right(),
                        ty: *ty,
                    }));
                vec![]
            },
            (
                ActionStateMachineState::Holding(HeldObject::Entity(PlaceEntityType::Splitter {
                    pos,
                    direction,
                    ty,

                    in_mode,
                    out_mode,
                })),
                Key::R,
            ) => {
                self.state = ActionStateMachineState::Holding(HeldObject::Entity(
                    PlaceEntityType::Splitter {
                        pos: *pos,
                        direction: direction.turn_right(),
                        ty: *ty,
                        in_mode: *in_mode,
                        out_mode: *out_mode,
                    },
                ));
                vec![]
            },
            (
                ActionStateMachineState::Holding(HeldObject::Entity(
                    PlaceEntityType::Underground {
                        pos,
                        direction,
                        ty,
                        underground_dir,
                    },
                )),
                Key::R,
            ) => {
                self.state = ActionStateMachineState::Holding(HeldObject::Entity(
                    PlaceEntityType::Underground {
                        pos: *pos,
                        direction: direction.turn_right(),
                        ty: *ty,
                        underground_dir: *underground_dir,
                    },
                ));
                vec![]
            },
            (
                ActionStateMachineState::Holding(HeldObject::Entity(PlaceEntityType::Inserter {
                    ty,
                    pos,
                    dir,
                    filter,
                    user_movetime,
                })),
                Key::Key4,
            ) => {
                self.state = ActionStateMachineState::Holding(HeldObject::Entity(
                    PlaceEntityType::Inserter {
                        ty: (*ty + 1) % (data_store.inserter_infos.len() as u8),
                        pos: *pos,
                        dir: *dir,
                        filter: *filter,
                        user_movetime: *user_movetime,
                    },
                ));
                vec![]
            },
            (ActionStateMachineState::Idle | ActionStateMachineState::Holding(_), Key::Key4) => {
                self.state = ActionStateMachineState::Holding(HeldObject::Entity(
                    PlaceEntityType::Inserter {
                        pos: Self::player_mouse_to_tile(
                            self.zoom_level,
                            self.map_view_info.unwrap_or(self.local_player_pos),
                            self.current_mouse_pos,
                        ),
                        dir: Dir::North,
                        // TODO:
                        filter: None,

                        ty: 0,
                        user_movetime: None,
                    },
                ));
                vec![]
            },
            (
                ActionStateMachineState::Holding(HeldObject::Entity(PlaceEntityType::PowerPole {
                    pos,
                    ty,
                })),
                Key::Key5,
            ) => {
                self.state = ActionStateMachineState::Holding(HeldObject::Entity(
                    PlaceEntityType::PowerPole {
                        pos: *pos,
                        ty: (*ty + 1) % data_store.power_pole_data.len() as u8,
                    },
                ));
                vec![]
            },
            (ActionStateMachineState::Idle | ActionStateMachineState::Holding(_), Key::Key5) => {
                self.state = ActionStateMachineState::Holding(HeldObject::Entity(
                    PlaceEntityType::PowerPole {
                        pos: Self::player_mouse_to_tile(
                            self.zoom_level,
                            self.map_view_info.unwrap_or(self.local_player_pos),
                            self.current_mouse_pos,
                        ),
                        ty: 0,
                    },
                ));
                vec![]
            },
            // (ActionStateMachineState::Idle | ActionStateMachineState::Holding(_), Key::Key6) => {
            //     self.state = ActionStateMachineState::Holding(HeldObject::Entity(
            //         PlaceEntityType::FluidTank {
            //             ty: 0,
            //             pos: Self::player_mouse_to_tile(
            //                 self.zoom_level,
            //                 self.map_view_info.unwrap_or(self.local_player_pos),
            //                 self.current_mouse_pos,
            //             ),
            //             rotation: Dir::North,
            //         },
            //     ));
            //     vec![]
            // },
            (ActionStateMachineState::Idle | ActionStateMachineState::Holding(_), Key::Key6) => {
                self.state =
                    ActionStateMachineState::Holding(HeldObject::Entity(PlaceEntityType::Chest {
                        ty: 0,
                        pos: Self::player_mouse_to_tile(
                            self.zoom_level,
                            self.map_view_info.unwrap_or(self.local_player_pos),
                            self.current_mouse_pos,
                        ),
                    }));
                vec![]
            },
            (
                ActionStateMachineState::Holding(HeldObject::Entity(
                    PlaceEntityType::Underground {
                        pos,
                        direction,
                        ty,
                        underground_dir,
                    },
                )),
                Key::Key7,
            ) => {
                self.state = ActionStateMachineState::Holding(HeldObject::Entity(
                    PlaceEntityType::Underground {
                        pos: *pos,
                        direction: *direction,
                        ty: dbg!((*ty + 1) % u8::try_from(data_store.belt_infos.len()).unwrap()),
                        underground_dir: *underground_dir,
                    },
                ));
                vec![]
            },
            (ActionStateMachineState::Idle | ActionStateMachineState::Holding(_), Key::Key7) => {
                self.state = ActionStateMachineState::Holding(HeldObject::Entity(
                    PlaceEntityType::Underground {
                        pos: Self::player_mouse_to_tile(
                            self.zoom_level,
                            self.map_view_info.unwrap_or(self.local_player_pos),
                            self.current_mouse_pos,
                        ),
                        ty: 0,
                        direction: Dir::North,
                        underground_dir: UndergroundDir::Entrance,
                    },
                ));
                vec![]
            },
            // (ActionStateMachineState::Idle | ActionStateMachineState::Holding(_), Key::Key7) => {
            //     self.state =
            //         ActionStateMachineState::Holding(HeldObject::Entity(PlaceEntityType::Chest {
            //             pos: Self::player_mouse_to_tile(
            //                 self.zoom_level,
            //                 self.map_view_info.unwrap_or(self.local_player_pos),
            //                 self.current_mouse_pos,
            //             ),
            //             ty: 0,
            //         }));
            //     vec![]
            // },
            (
                ActionStateMachineState::Holding(HeldObject::Entity(PlaceEntityType::SolarPanel {
                    pos,
                    ty,
                })),
                Key::Key8,
            ) => {
                self.state = ActionStateMachineState::Holding(HeldObject::Entity(
                    PlaceEntityType::SolarPanel {
                        pos: *pos,
                        ty: (*ty + 1) % data_store.solar_panel_info.len() as u8,
                    },
                ));
                vec![]
            },
            (ActionStateMachineState::Idle | ActionStateMachineState::Holding(_), Key::Key8) => {
                self.state = ActionStateMachineState::Holding(HeldObject::Entity(
                    PlaceEntityType::SolarPanel {
                        pos: Self::player_mouse_to_tile(
                            self.zoom_level,
                            self.map_view_info.unwrap_or(self.local_player_pos),
                            self.current_mouse_pos,
                        ),
                        ty: 0,
                    },
                ));
                vec![]
            },
            // (
            //     ActionStateMachineState::Holding(HeldObject::Entity(PlaceEntityType::FluidTank {
            //         pos,
            //         ty,
            //         rotation,
            //     })),
            //     Key::Key9,
            // ) => {
            //     self.state = ActionStateMachineState::Holding(HeldObject::Entity(
            //         PlaceEntityType::FluidTank {
            //             pos: *pos,
            //             ty: (*ty + 1) % data_store.fluid_tank_infos.len() as u8,
            //             rotation: *rotation,
            //         },
            //     ));
            //     vec![]
            // },
            (ActionStateMachineState::Idle | ActionStateMachineState::Holding(_), Key::Key9) => {
                self.state =
                    ActionStateMachineState::Holding(HeldObject::Entity(PlaceEntityType::Lab {
                        ty: 0,
                        pos: Self::player_mouse_to_tile(
                            self.zoom_level,
                            self.map_view_info.unwrap_or(self.local_player_pos),
                            self.current_mouse_pos,
                        ),
                    }));
                vec![]
            },
            // (ActionStateMachineState::Idle | ActionStateMachineState::Holding(_), Key::Key0) => {
            //     self.state =
            //         ActionStateMachineState::Holding(HeldObject::Entity(PlaceEntityType::Beacon {
            //             pos: Self::player_mouse_to_tile(
            //                 self.zoom_level,
            //                 self.map_view_info.unwrap_or(self.local_player_pos),
            //                 self.current_mouse_pos,
            //             ),
            //             ty: 0,
            //         }));
            //     vec![]
            // },
            (ActionStateMachineState::Idle | ActionStateMachineState::Holding(_), Key::Key0) => {
                self.state = ActionStateMachineState::Holding(HeldObject::Entity(
                    PlaceEntityType::MiningDrill {
                        pos: Self::player_mouse_to_tile(
                            self.zoom_level,
                            self.map_view_info.unwrap_or(self.local_player_pos),
                            self.current_mouse_pos,
                        ),
                        ty: 0,
                        rotation: Dir::North,
                    },
                ));
                vec![]
            },
            (
                ActionStateMachineState::Holding(HeldObject::Entity(
                    PlaceEntityType::MiningDrill { pos, ty, rotation },
                )),
                Key::Key0,
            ) => {
                self.state = ActionStateMachineState::Holding(HeldObject::Entity(
                    PlaceEntityType::MiningDrill {
                        pos: *pos,
                        // FIXME: Increase the ty
                        ty: 0,
                        rotation: *rotation,
                    },
                ));
                vec![]
            },
            (
                ActionStateMachineState::Holding(HeldObject::Entity(PlaceEntityType::Inserter {
                    pos,
                    dir,
                    filter,
                    ty,
                    user_movetime,
                })),
                Key::R,
            ) => {
                self.state = ActionStateMachineState::Holding(HeldObject::Entity(
                    PlaceEntityType::Inserter {
                        pos: *pos,
                        dir: dir.turn_right(),
                        filter: *filter,
                        ty: *ty,
                        user_movetime: *user_movetime,
                    },
                ));
                vec![]
            },
            (
                ActionStateMachineState::Holding(HeldObject::Entity(PlaceEntityType::FluidTank {
                    pos,
                    ty,
                    rotation,
                })),
                Key::R,
            ) => {
                self.state = ActionStateMachineState::Holding(HeldObject::Entity(
                    PlaceEntityType::FluidTank {
                        ty: *ty,
                        pos: *pos,
                        rotation: rotation.turn_right(),
                    },
                ));
                vec![]
            },

            (_, Key::Del) => {
                self.state = ActionStateMachineState::DelPressed;

                vec![]
            },

            (ActionStateMachineState::Viewing(pos), Key::Key1) => {
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

            (_, Key::P) => {
                self.statistics_panel_open = !self.statistics_panel_open;
                vec![]
            },

            (_, Key::T) => {
                self.technology_panel_open = !self.technology_panel_open;
                vec![]
            },

            (_, Key::M) => {
                if self.map_view_info.is_some() {
                    self.map_view_info = None;
                } else {
                    self.map_view_info = Some(self.local_player_pos);
                }
                vec![]
            },

            (ActionStateMachineState::Holding(HeldObject::Blueprint(bp)), Key::V) => {
                bp.flip_vertical(data_store);
                vec![]
            },
            (ActionStateMachineState::Holding(HeldObject::Blueprint(bp)), Key::H) => {
                bp.flip_horizontal(data_store);
                vec![]
            },
            (ActionStateMachineState::Holding(HeldObject::Blueprint(bp)), Key::R) => {
                bp.turn_right(data_store);
                vec![]
            },

            (_, Key::Esc) => {
                self.escape_menu_open = !self.escape_menu_open;
                vec![]
            },

            (_, _) => vec![],
        };

        // Do not send any actions if we are in the escape menu
        if self.escape_menu_open {
            vec![].into_iter()
        } else {
            ret.into_iter()
        }
    }

    fn handle_stop_pressing_key(
        &mut self,
        key: Key,
    ) -> impl Iterator<Item = ActionType<ItemIdxType, RecipeIdxType>> + use<ItemIdxType, RecipeIdxType>
    {
        match (&self.state, key) {
            (_, _) => vec![].into_iter(),
        }
    }

    pub fn player_mouse_to_tile(
        zoom_level: f32,
        camera_pos: (f32, f32),
        mouse_pos: (f32, f32),
    ) -> Position {
        let mouse_pos = (
            ((mouse_pos.0) * (WIDTH_PER_LEVEL as f32))
                .mul_add(1.5f32.powf(zoom_level), camera_pos.0),
            ((mouse_pos.1) * (WIDTH_PER_LEVEL as f32))
                .mul_add(1.5f32.powf(zoom_level), camera_pos.1),
        );

        Position {
            x: mouse_pos.0 as i32,
            y: mouse_pos.1 as i32,
        }
    }

    #[must_use]
    pub fn once_per_update_actions(
        &mut self,
        world: &World<ItemIdxType, RecipeIdxType>,
        data_store: &DataStore<ItemIdxType, RecipeIdxType>,
    ) -> impl Iterator<Item = ActionType<ItemIdxType, RecipeIdxType>> + use<ItemIdxType, RecipeIdxType>
    {
        let mut actions = Vec::new();

        if let ActionStateMachineState::Deconstructing(pos, timer) = &mut self.state {
            // Check if we are still over the thing we were deconstructing
            if world.get_entity_at(*pos, data_store)
                == world.get_entity_at(
                    Self::player_mouse_to_tile(
                        self.zoom_level,
                        self.map_view_info.unwrap_or(self.local_player_pos),
                        self.current_mouse_pos,
                    ),
                    data_store,
                )
            {
                *timer -= 1;
                if *timer == 0 {
                    let pos = *pos;
                    self.state = ActionStateMachineState::Idle;
                    actions.push(ActionType::Remove(pos));
                }
            } else {
                self.state = ActionStateMachineState::Idle;
            }
        }

        let mut move_dir = (0, 0);

        if self.current_held_keys.contains(&Key::W) {
            move_dir.1 -= 1;
        }
        if self.current_held_keys.contains(&Key::A) {
            move_dir.0 -= 1;
        }
        if self.current_held_keys.contains(&Key::S) {
            move_dir.1 += 1;
        }
        if self.current_held_keys.contains(&Key::D) {
            move_dir.0 += 1;
        }

        if let Some(map_view_pos) = &mut self.map_view_info {
            map_view_pos.0 += move_dir.0 as f32 * 1.5f32.powf(self.zoom_level) * MAP_VIEW_PAN_SPEED;
            map_view_pos.1 += move_dir.1 as f32 * 1.5f32.powf(self.zoom_level) * MAP_VIEW_PAN_SPEED;
        } else {
            self.local_player_pos.0 +=
                move_dir.0 as f32 * world.players[usize::from(self.my_player_id)].movement_speed;
            self.local_player_pos.1 +=
                move_dir.1 as f32 * world.players[usize::from(self.my_player_id)].movement_speed;

            if move_dir.0 != 0 || move_dir.1 != 0 {
                // Only send this event if it changed
                actions.push(ActionType::Position(
                    self.my_player_id,
                    self.local_player_pos,
                ));
            }
        }

        actions.into_iter()
    }
}
