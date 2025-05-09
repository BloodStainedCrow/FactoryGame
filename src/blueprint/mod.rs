use std::{borrow::Borrow, ops::Range};

use proptest::{
    prelude::{prop, Just, Strategy},
    prop_oneof,
};

use crate::{
    data::DataStore,
    frontend::{
        action::{
            place_entity::{EntityPlaceOptions, PlaceEntityInfo},
            place_tile::{PlaceFloorTileByHandInfo, PlaceFloorTileGhostInfo, PositionInfo},
            set_recipe::SetRecipeInfo,
            ActionType,
        },
        world::{
            tile::{Dir, FloorTile, PlaceEntityType},
            Position,
        },
    },
    item::{IdxTrait, Item, Recipe, WeakIdxTrait},
    rendering::app_state::GameState,
    replays::Replay,
};

// For now blueprint will just be a list of actions
#[derive(Debug, serde::Deserialize, serde::Serialize)]
pub struct Blueprint<ItemIdxType: WeakIdxTrait, RecipeIdxType: WeakIdxTrait> {
    actions: Vec<ActionType<ItemIdxType, RecipeIdxType>>,
}

impl<ItemIdxType: IdxTrait, RecipeIdxType: IdxTrait> Blueprint<ItemIdxType, RecipeIdxType> {
    pub fn apply(
        &self,
        base_pos: Position,
        game_state: &mut GameState<ItemIdxType, RecipeIdxType>,
        data_store: &DataStore<ItemIdxType, RecipeIdxType>,
    ) {
        // FIXME: currently base_pos is unused!
        game_state.apply_actions(
            self.actions.iter().map(|a| match a {
                ActionType::PlaceFloorTile(PlaceFloorTileByHandInfo {
                    ghost_info:
                        PlaceFloorTileGhostInfo {
                            tile,
                            position: PositionInfo::Single { pos },
                        },
                    player,
                }) => ActionType::PlaceFloorTile(PlaceFloorTileByHandInfo {
                    ghost_info: PlaceFloorTileGhostInfo {
                        tile: *tile,
                        position: PositionInfo::Single {
                            pos: Position {
                                x: base_pos.x + pos.x,
                                y: base_pos.y + pos.y,
                            },
                        },
                    },
                    player: *player,
                }),
                ActionType::PlaceEntity(PlaceEntityInfo {
                    entities: EntityPlaceOptions::Single(PlaceEntityType::Assembler(pos)),
                }) => ActionType::PlaceEntity(PlaceEntityInfo {
                    entities: EntityPlaceOptions::Single(PlaceEntityType::Assembler(Position {
                        x: base_pos.x + pos.x,
                        y: base_pos.y + pos.y,
                    })),
                }),
                ActionType::PlaceEntity(PlaceEntityInfo {
                    entities: EntityPlaceOptions::Single(PlaceEntityType::Belt { pos, direction }),
                }) => ActionType::PlaceEntity(PlaceEntityInfo {
                    entities: EntityPlaceOptions::Single(PlaceEntityType::Belt {
                        pos: Position {
                            x: base_pos.x + pos.x,
                            y: base_pos.y + pos.y,
                        },
                        direction: *direction,
                    }),
                }),
                ActionType::PlaceEntity(PlaceEntityInfo {
                    entities: EntityPlaceOptions::Single(PlaceEntityType::Chest { pos }),
                }) => ActionType::PlaceEntity(PlaceEntityInfo {
                    entities: EntityPlaceOptions::Single(PlaceEntityType::Chest {
                        pos: Position {
                            x: base_pos.x + pos.x,
                            y: base_pos.y + pos.y,
                        },
                    }),
                }),
                ActionType::PlaceEntity(PlaceEntityInfo {
                    entities:
                        EntityPlaceOptions::Single(PlaceEntityType::Inserter { pos, dir, filter }),
                }) => ActionType::PlaceEntity(PlaceEntityInfo {
                    entities: EntityPlaceOptions::Single(PlaceEntityType::Inserter {
                        pos: Position {
                            x: base_pos.x + pos.x,
                            y: base_pos.y + pos.y,
                        },
                        dir: *dir,
                        filter: *filter,
                    }),
                }),
                ActionType::PlaceEntity(PlaceEntityInfo {
                    entities: EntityPlaceOptions::Single(PlaceEntityType::PowerPole { pos, ty }),
                }) => ActionType::PlaceEntity(PlaceEntityInfo {
                    entities: EntityPlaceOptions::Single(PlaceEntityType::PowerPole {
                        pos: Position {
                            x: base_pos.x + pos.x,
                            y: base_pos.y + pos.y,
                        },
                        ty: *ty,
                    }),
                }),
                ActionType::PlaceEntity(PlaceEntityInfo {
                    entities:
                        EntityPlaceOptions::Single(PlaceEntityType::Splitter {
                            pos,
                            direction,
                            in_mode,
                            out_mode,
                        }),
                }) => ActionType::PlaceEntity(PlaceEntityInfo {
                    entities: EntityPlaceOptions::Single(PlaceEntityType::Splitter {
                        pos: Position {
                            x: base_pos.x + pos.x,
                            y: base_pos.y + pos.y,
                        },
                        direction: *direction,
                        in_mode: *in_mode,
                        out_mode: *out_mode,
                    }),
                }),
                ActionType::PlaceEntity(PlaceEntityInfo {
                    entities: EntityPlaceOptions::Single(PlaceEntityType::SolarPanel { pos, ty }),
                }) => ActionType::PlaceEntity(PlaceEntityInfo {
                    entities: EntityPlaceOptions::Single(PlaceEntityType::SolarPanel {
                        pos: Position {
                            x: base_pos.x + pos.x,
                            y: base_pos.y + pos.y,
                        },
                        ty: *ty,
                    }),
                }),
                ActionType::PlaceEntity(PlaceEntityInfo {
                    entities: EntityPlaceOptions::Single(PlaceEntityType::Lab { pos, ty }),
                }) => ActionType::PlaceEntity(PlaceEntityInfo {
                    entities: EntityPlaceOptions::Single(PlaceEntityType::Lab {
                        pos: Position {
                            x: base_pos.x + pos.x,
                            y: base_pos.y + pos.y,
                        },
                        ty: *ty,
                    }),
                }),
                ActionType::SetRecipe(SetRecipeInfo { pos, recipe }) => {
                    ActionType::SetRecipe(SetRecipeInfo {
                        pos: Position {
                            x: base_pos.x + pos.x,
                            y: base_pos.y + pos.y,
                        },
                        recipe: *recipe,
                    })
                },
                ActionType::Remove(position) => ActionType::Remove(Position {
                    x: base_pos.x + position.x,
                    y: base_pos.y + position.y,
                }),
                _ => unreachable!(),
            }),
            data_store,
        );
    }

    pub fn from_replay<DS: Borrow<DataStore<ItemIdxType, RecipeIdxType>>>(
        replay: &Replay<ItemIdxType, RecipeIdxType, DS>,
    ) -> Self {
        Self {
            actions: replay.actions.iter().map(|ra| ra.action.clone()).collect(),
        }
    }
}

pub fn random_blueprint_strategy<ItemIdxType: IdxTrait, RecipeIdxType: IdxTrait>(
    len_range: Range<usize>,
    data_store: &DataStore<ItemIdxType, RecipeIdxType>,
) -> impl Strategy<Value = Blueprint<ItemIdxType, RecipeIdxType>> {
    prop::collection::vec(random_blueprint_action(data_store), len_range)
        .prop_map(|actions| Blueprint { actions })
}

pub fn random_action<ItemIdxType: IdxTrait, RecipeIdxType: IdxTrait>(
    data_store: &DataStore<ItemIdxType, RecipeIdxType>,
) -> impl Strategy<Value = ActionType<ItemIdxType, RecipeIdxType>> {
    prop_oneof![
        random_position().prop_map(|pos| ActionType::Ping(pos)),
        random_position().prop_map(|pos| ActionType::Position(0, (pos.x as f32, pos.y as f32))),
        random_position().prop_map(|pos| ActionType::PlaceFloorTile(PlaceFloorTileByHandInfo {
            ghost_info: PlaceFloorTileGhostInfo {
                tile: FloorTile::Concrete,
                position: PositionInfo::Single { pos: pos }
            },
            player: ()
        })),
        (random_position(), random_recipe(data_store))
            .prop_map(|(pos, recipe)| ActionType::SetRecipe(SetRecipeInfo { pos, recipe })),
        random_entity_to_place(data_store).prop_map(|ty| {
            ActionType::PlaceEntity(PlaceEntityInfo {
                entities: EntityPlaceOptions::Single(ty),
            })
        }),
        // random_position().prop_map(|pos| ActionType::Remove(pos)),
    ]
}

pub fn random_blueprint_action<ItemIdxType: IdxTrait, RecipeIdxType: IdxTrait>(
    data_store: &DataStore<ItemIdxType, RecipeIdxType>,
) -> impl Strategy<Value = ActionType<ItemIdxType, RecipeIdxType>> {
    prop_oneof![
        // random_blueprint_offs().prop_map(|pos| ActionType::PlaceFloorTile(
        //     PlaceFloorTileByHandInfo {
        //         ghost_info: PlaceFloorTileGhostInfo {
        //             tile: FloorTile::Concrete,
        //             position: PositionInfo::Single { pos: pos }
        //         },
        //         player: ()
        //     }
        // )),
        (random_blueprint_offs(), random_recipe(data_store))
            .prop_map(|(pos, recipe)| ActionType::SetRecipe(SetRecipeInfo { pos, recipe })),
        random_entity_to_place(data_store).prop_map(|ty| {
            ActionType::PlaceEntity(PlaceEntityInfo {
                entities: EntityPlaceOptions::Single(ty),
            })
        }),
        // random_blueprint_offs().prop_map(|pos| ActionType::Remove(pos)),
    ]
}

pub fn random_entity_to_place<ItemIdxType: IdxTrait, RecipeIdxType: IdxTrait>(
    data_store: &DataStore<ItemIdxType, RecipeIdxType>,
) -> impl Strategy<Value = PlaceEntityType<ItemIdxType>> {
    prop_oneof![
        random_blueprint_offs().prop_map(|pos| PlaceEntityType::Assembler(pos)),
        random_blueprint_offs().prop_map(|pos| PlaceEntityType::Chest { pos }),
        (
            random_blueprint_offs()
            // 0..(data_store.solar_panel_sizes.len().try_into().unwrap())
        )
        .prop_map(|(pos)| PlaceEntityType::SolarPanel { pos, ty: 0 }),
        (random_blueprint_offs(), random_dir()).prop_map(|(pos, dir)| PlaceEntityType::Belt {
            pos,
            direction: dir
        }),
        (random_blueprint_offs(), 0..data_store.power_pole_data.len()).prop_map(|(pos, ty)| {
            PlaceEntityType::PowerPole {
                pos,
                ty: ty.try_into().unwrap(),
            }
        }),
        (
            random_blueprint_offs(),
            random_dir(),
            random_item(data_store)
        )
            .prop_map(|(pos, dir, filter)| {
                PlaceEntityType::Inserter {
                    pos,
                    dir,
                    filter: Some(filter),
                }
            }),
        (random_blueprint_offs(), (0u8..1))
            .prop_map(|(pos, ty)| { PlaceEntityType::Lab { pos, ty } }),
        // (random_blueprint_offs(), random_dir()).prop_map(|(pos, dir)| {
        //     PlaceEntityType::Splitter {
        //         pos,
        //         direction: dir,

        //         // TODO: Test inout modes
        //         in_mode: None,
        //         out_mode: None,
        //     }
        // })
    ]
}

pub fn random_dir() -> impl Strategy<Value = Dir> {
    prop_oneof![
        Just(Dir::North),
        Just(Dir::South),
        Just(Dir::East),
        Just(Dir::West)
    ]
}

pub fn random_recipe<ItemIdxType: IdxTrait, RecipeIdxType: IdxTrait>(
    data_store: &DataStore<ItemIdxType, RecipeIdxType>,
) -> impl Strategy<Value = Recipe<RecipeIdxType>> {
    (0..data_store.recipe_timers.len())
        .prop_map(|recipe_idx| RecipeIdxType::try_from(recipe_idx).unwrap())
        .prop_map(|id| Recipe { id })
}

pub fn random_item<ItemIdxType: IdxTrait, RecipeIdxType: IdxTrait>(
    data_store: &DataStore<ItemIdxType, RecipeIdxType>,
) -> impl Strategy<Value = Item<ItemIdxType>> {
    (0..data_store.item_names.len())
        .prop_map(|item_idx| ItemIdxType::try_from(item_idx).unwrap())
        .prop_map(|id| Item { id })
}

pub fn random_blueprint_offs() -> impl Strategy<Value = Position> {
    ((0u32..16), (0u32..16)).prop_map(|(x, y)| Position {
        x: x as usize,
        y: y as usize,
    })
}

pub fn random_position() -> impl Strategy<Value = Position> {
    ((1600u32..=1602u32), (1600u32..=1602u32)).prop_map(|(x, y)| Position {
        x: x as usize,
        y: y as usize,
    })
}
