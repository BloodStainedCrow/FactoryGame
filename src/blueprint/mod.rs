use std::{borrow::Borrow, ops::Range};

use proptest::{
    prelude::{Just, Strategy, prop},
    prop_oneof,
};
use tilelib::types::{DrawInstance, Layer};

use crate::{
    data::DataStore,
    frontend::{
        action::{
            ActionType,
            place_entity::{EntityPlaceOptions, PlaceEntityInfo},
            place_tile::{PlaceFloorTileByHandInfo, PlaceFloorTileGhostInfo, PositionInfo},
            set_recipe::SetRecipeInfo,
        },
        world::{
            Position,
            tile::{AssemblerID, AssemblerInfo, Dir, FloorTile, PlaceEntityType, World},
        },
    },
    item::{IdxTrait, Item, Recipe, WeakIdxTrait},
    rendering::{TextureAtlas, app_state::GameState},
    replays::Replay,
};

// For now blueprint will just be a list of actions
#[derive(Debug, Clone, serde::Deserialize, serde::Serialize)]
pub struct Blueprint<ItemIdxType: WeakIdxTrait, RecipeIdxType: WeakIdxTrait> {
    pub actions: Vec<ActionType<ItemIdxType, RecipeIdxType>>,
}

impl<ItemIdxType: IdxTrait, RecipeIdxType: IdxTrait> Blueprint<ItemIdxType, RecipeIdxType> {
    pub fn actions_with_base_pos(
        &self,
        base_pos: Position,
    ) -> impl Iterator<Item = ActionType<ItemIdxType, RecipeIdxType>> {
        self.actions.iter().map(move |a| match a {
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
                entities: EntityPlaceOptions::Single(PlaceEntityType::Assembler { pos, ty }),
            }) => ActionType::PlaceEntity(PlaceEntityInfo {
                entities: EntityPlaceOptions::Single(PlaceEntityType::Assembler {
                    pos: Position {
                        x: base_pos.x + pos.x,
                        y: base_pos.y + pos.y,
                    },
                    ty: *ty,
                }),
            }),
            ActionType::PlaceEntity(PlaceEntityInfo {
                entities:
                    EntityPlaceOptions::Single(PlaceEntityType::Belt { pos, direction, ty }),
            }) => ActionType::PlaceEntity(PlaceEntityInfo {
                entities: EntityPlaceOptions::Single(PlaceEntityType::Belt {
                    pos: Position {
                        x: base_pos.x + pos.x,
                        y: base_pos.y + pos.y,
                    },
                    direction: *direction,
                    ty: *ty,
                }),
            }),
            ActionType::PlaceEntity(PlaceEntityInfo {
                entities:
                    EntityPlaceOptions::Single(PlaceEntityType::Underground {
                        pos,
                        direction,
                        ty,
                        underground_dir,
                    }),
            }) => ActionType::PlaceEntity(PlaceEntityInfo {
                entities: EntityPlaceOptions::Single(PlaceEntityType::Underground {
                    pos: Position {
                        x: base_pos.x + pos.x,
                        y: base_pos.y + pos.y,
                    },
                    direction: *direction,
                    ty: *ty,
                    underground_dir: *underground_dir,
                }),
            }),
            ActionType::PlaceEntity(PlaceEntityInfo {
                entities: EntityPlaceOptions::Single(PlaceEntityType::Chest { pos, ty }),
            }) => ActionType::PlaceEntity(PlaceEntityInfo {
                entities: EntityPlaceOptions::Single(PlaceEntityType::Chest {
                    pos: Position {
                        x: base_pos.x + pos.x,
                        y: base_pos.y + pos.y,
                    },
                    ty: *ty,
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
                        ty,
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
                    ty: *ty,
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
            ActionType::PlaceEntity(PlaceEntityInfo {
                entities: EntityPlaceOptions::Single(PlaceEntityType::Beacon { pos, ty }),
            }) => ActionType::PlaceEntity(PlaceEntityInfo {
                entities: EntityPlaceOptions::Single(PlaceEntityType::Beacon {
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
            ActionType::AddModules { pos, modules } => ActionType::AddModules {
                pos: Position {
                    x: base_pos.x + pos.x,
                    y: base_pos.y + pos.y,
                },
                modules: modules.clone(),
            },
            ActionType::SetChestSlotLimit { pos, num_slots } => ActionType::SetChestSlotLimit {
                pos: Position {
                    x: base_pos.x + pos.x,
                    y: base_pos.y + pos.y,
                },
                num_slots: *num_slots,
            },
            ActionType::OverrideInserterMovetime { pos, new_movetime } => {
                ActionType::OverrideInserterMovetime {
                    pos: Position {
                        x: base_pos.x + pos.x,
                        y: base_pos.y + pos.y,
                    },
                    new_movetime: *new_movetime,
                }
            },
            a => unreachable!("{:?}", a),
        })
    }

    pub fn apply(
        &self,
        base_pos: Position,
        game_state: &mut GameState<ItemIdxType, RecipeIdxType>,
        data_store: &DataStore<ItemIdxType, RecipeIdxType>,
    ) {
        game_state.apply_actions(self.actions_with_base_pos(base_pos), data_store);
    }

    pub fn from_replay<DS: Borrow<DataStore<ItemIdxType, RecipeIdxType>>>(
        replay: &Replay<ItemIdxType, RecipeIdxType, DS>,
    ) -> Self {
        Self {
            actions: replay.actions.iter().map(|ra| ra.action.clone()).collect(),
        }
    }

    pub fn from_area(
        world: &World<ItemIdxType, RecipeIdxType>,
        area: [Range<i32>; 2],
        data_store: &DataStore<ItemIdxType, RecipeIdxType>,
    ) -> Self {
        let mut bp = Self { actions: vec![] };

        let entities: Vec<_> = world
            .get_entities_colliding_with(
                Position {
                    x: area[0].start,
                    y: area[1].start,
                },
                (
                    (area[0].end - area[0].start).try_into().unwrap(),
                    (area[1].end - area[1].start).try_into().unwrap(),
                ),
                data_store,
            )
            .into_iter()
            .collect();

        if entities.is_empty() {
            return Self { actions: vec![] };
        }

        let base_pos = Position {
            x: entities.iter().map(|e| e.get_pos().x).min().unwrap(),
            y: entities.iter().map(|e| e.get_pos().y).min().unwrap(),
        };

        // FIXME: This could be unreproducable if the power connection order matters
        // FIXME: This will underflow if a entity extends past the edge of the selected area
        for e in entities {
            let actions = match e {
                crate::frontend::world::tile::Entity::Assembler {
                    ty,
                    pos,
                    info: AssemblerInfo::PoweredNoRecipe(_) | AssemblerInfo::UnpoweredNoRecipe,
                    modules,
                } => vec![
                    ActionType::PlaceEntity(PlaceEntityInfo {
                        entities: EntityPlaceOptions::Single(PlaceEntityType::Assembler {
                            pos: Position {
                                x: pos.x - base_pos.x,
                                y: pos.y - base_pos.y,
                            },
                            ty: *ty,
                        }),
                    }),
                    ActionType::AddModules {
                        pos: Position {
                            x: pos.x - base_pos.x,
                            y: pos.y - base_pos.y,
                        },
                        modules: modules.iter().flatten().copied().collect(),
                    },
                ],
                crate::frontend::world::tile::Entity::Assembler {
                    ty,
                    pos,
                    info:
                        AssemblerInfo::Powered {
                            id: AssemblerID { recipe, .. },
                            ..
                        }
                        | AssemblerInfo::Unpowered(recipe),
                    modules,
                } => vec![
                    ActionType::PlaceEntity(PlaceEntityInfo {
                        entities: EntityPlaceOptions::Single(PlaceEntityType::Assembler {
                            pos: Position {
                                x: pos.x - base_pos.x,
                                y: pos.y - base_pos.y,
                            },
                            ty: *ty,
                        }),
                    }),
                    ActionType::SetRecipe(SetRecipeInfo {
                        pos: Position {
                            x: pos.x - base_pos.x,
                            y: pos.y - base_pos.y,
                        },
                        recipe: *recipe,
                    }),
                    ActionType::AddModules {
                        pos: Position {
                            x: pos.x - base_pos.x,
                            y: pos.y - base_pos.y,
                        },
                        modules: modules.iter().flatten().copied().collect(),
                    },
                ],
                crate::frontend::world::tile::Entity::PowerPole { ty, pos, .. } => {
                    vec![ActionType::PlaceEntity(PlaceEntityInfo {
                        entities: EntityPlaceOptions::Single(PlaceEntityType::PowerPole {
                            pos: Position {
                                x: pos.x - base_pos.x,
                                y: pos.y - base_pos.y,
                            },
                            ty: *ty,
                        }),
                    })]
                },
                crate::frontend::world::tile::Entity::Belt {
                    pos, ty, direction, ..
                } => {
                    vec![ActionType::PlaceEntity(PlaceEntityInfo {
                        entities: EntityPlaceOptions::Single(PlaceEntityType::Belt {
                            pos: Position {
                                x: pos.x - base_pos.x,
                                y: pos.y - base_pos.y,
                            },
                            direction: *direction,
                            ty: *ty,
                        }),
                    })]
                },
                crate::frontend::world::tile::Entity::Underground {
                    pos,
                    underground_dir,
                    direction,
                    ty,
                    ..
                } => {
                    vec![ActionType::PlaceEntity(PlaceEntityInfo {
                        entities: EntityPlaceOptions::Single(PlaceEntityType::Underground {
                            pos: Position {
                                x: pos.x - base_pos.x,
                                y: pos.y - base_pos.y,
                            },
                            direction: *direction,
                            ty: *ty,
                            underground_dir: *underground_dir,
                        }),
                    })]
                },
                crate::frontend::world::tile::Entity::Splitter { pos, direction, id } => todo!(),
                crate::frontend::world::tile::Entity::Inserter {
                    user_movetime,
                    pos,
                    direction,
                    ..
                } => {
                    let mut ret = vec![ActionType::PlaceEntity(PlaceEntityInfo {
                        entities: EntityPlaceOptions::Single(PlaceEntityType::Inserter {
                            pos: Position {
                                x: pos.x - base_pos.x,
                                y: pos.y - base_pos.y,
                            },
                            dir: *direction,
                            filter: None,
                        }),
                    })];

                    if let Some(user_movetime) = *user_movetime {
                        ret.push(ActionType::OverrideInserterMovetime {
                            pos: Position {
                                x: pos.x - base_pos.x,
                                y: pos.y - base_pos.y,
                            },
                            new_movetime: Some(user_movetime),
                        });
                    }

                    ret
                },
                crate::frontend::world::tile::Entity::Chest { pos, ty, .. } => {
                    vec![ActionType::PlaceEntity(PlaceEntityInfo {
                        entities: EntityPlaceOptions::Single(PlaceEntityType::Chest {
                            pos: Position {
                                x: pos.x - base_pos.x,
                                y: pos.y - base_pos.y,
                            },
                            ty: *ty,
                        }),
                    })]
                },
                crate::frontend::world::tile::Entity::Roboport {
                    ty,
                    pos,
                    power_grid,
                    network,
                    id,
                } => todo!(),
                crate::frontend::world::tile::Entity::SolarPanel { pos, ty, .. } => {
                    vec![ActionType::PlaceEntity(PlaceEntityInfo {
                        entities: EntityPlaceOptions::Single(PlaceEntityType::SolarPanel {
                            ty: *ty,
                            pos: Position {
                                x: pos.x - base_pos.x,
                                y: pos.y - base_pos.y,
                            },
                        }),
                    })]
                },
                crate::frontend::world::tile::Entity::Lab { pos, ty, .. } => {
                    vec![ActionType::PlaceEntity(PlaceEntityInfo {
                        entities: EntityPlaceOptions::Single(PlaceEntityType::Lab {
                            ty: *ty,
                            pos: Position {
                                x: pos.x - base_pos.x,
                                y: pos.y - base_pos.y,
                            },
                        }),
                    })]
                },
                crate::frontend::world::tile::Entity::Beacon {
                    ty, pos, modules, ..
                } => {
                    vec![
                        ActionType::PlaceEntity(PlaceEntityInfo {
                            entities: EntityPlaceOptions::Single(PlaceEntityType::Beacon {
                                ty: *ty,
                                pos: Position {
                                    x: pos.x - base_pos.x,
                                    y: pos.y - base_pos.y,
                                },
                            }),
                        }),
                        ActionType::AddModules {
                            pos: Position {
                                x: pos.x - base_pos.x,
                                y: pos.y - base_pos.y,
                            },
                            modules: modules.into_iter().copied().flatten().collect(),
                        },
                    ]
                },
                crate::frontend::world::tile::Entity::FluidTank { ty, pos, rotation } => {
                    vec![ActionType::PlaceEntity(PlaceEntityInfo {
                        entities: EntityPlaceOptions::Single(PlaceEntityType::FluidTank {
                            ty: *ty,
                            pos: *pos,
                            rotation: *rotation,
                        }),
                    })]
                },
            };

            bp.actions.extend(actions);
        }

        bp
    }

    pub fn draw(
        &self,
        base_pos: (f32, f32),
        camera_pos: (f32, f32),
        layer: &mut Layer,
        texture_atlas: &TextureAtlas,
        data_store: &DataStore<ItemIdxType, RecipeIdxType>,
    ) {
        // let bottom_right = Position {
        //     x: self
        //         .actions
        //         .iter()
        //         .flat_map(|e| e.get_pos())
        //         .map(|pos| pos.x)
        //         .max()
        //         .unwrap(),
        //     y: self
        //         .actions
        //         .iter()
        //         .flat_map(|e| e.get_pos())
        //         .map(|pos| pos.y)
        //         .max()
        //         .unwrap(),
        // };

        // let raw_base_pos = Position {
        //     x: self
        //         .actions
        //         .iter()
        //         .flat_map(|e| e.get_pos())
        //         .map(|pos| pos.x)
        //         .min()
        //         .unwrap(),
        //     y: self
        //         .actions
        //         .iter()
        //         .flat_map(|e| e.get_pos())
        //         .map(|pos| pos.y)
        //         .min()
        //         .unwrap(),
        // };

        // layer.draw_sprite(
        //     &texture_atlas.dark_square,
        //     DrawInstance {
        //         position: [
        //             raw_base_pos.x as f32 - camera_pos.0 + base_pos.0,
        //             raw_base_pos.y as f32 - camera_pos.1 + base_pos.1,
        //         ],
        //         size: [
        //             bottom_right.x as f32 - raw_base_pos.x as f32,
        //             bottom_right.y as f32 - raw_base_pos.y as f32,
        //         ],
        //         animation_frame: 0,
        //     },
        // );

        for action in &self.actions {
            let pos = action.get_pos();
            let size = action.get_building_size(data_store);

            let (Some(pos), Some(size)) = (pos, size) else {
                continue;
            };

            layer.draw_sprite(
                &texture_atlas.dark_square,
                DrawInstance {
                    position: [
                        pos.x as f32 - camera_pos.0 + base_pos.0,
                        pos.y as f32 - camera_pos.1 + base_pos.1,
                    ],
                    size: [size[0] as f32, size[1] as f32],
                    animation_frame: 0,
                },
            );
        }
    }
}

pub fn random_blueprint_strategy<ItemIdxType: IdxTrait, RecipeIdxType: IdxTrait>(
    len_range: Range<usize>,
    data_store: &DataStore<ItemIdxType, RecipeIdxType>,
) -> impl Strategy<Value = Blueprint<ItemIdxType, RecipeIdxType>> + use<ItemIdxType, RecipeIdxType>
{
    prop::collection::vec(random_blueprint_action(data_store), len_range)
        .prop_map(|actions| Blueprint { actions })
}

pub fn random_action<ItemIdxType: IdxTrait, RecipeIdxType: IdxTrait>(
    data_store: &DataStore<ItemIdxType, RecipeIdxType>,
) -> impl Strategy<Value = ActionType<ItemIdxType, RecipeIdxType>> + use<ItemIdxType, RecipeIdxType>
{
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
) -> impl Strategy<Value = ActionType<ItemIdxType, RecipeIdxType>> + use<ItemIdxType, RecipeIdxType>
{
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
) -> impl Strategy<Value = PlaceEntityType<ItemIdxType>> + use<ItemIdxType, RecipeIdxType> {
    prop_oneof![
        random_blueprint_offs().prop_map(|pos| PlaceEntityType::Assembler { pos, ty: 0 }),
        (random_blueprint_offs(), 0..data_store.chest_num_slots.len()).prop_map(|(pos, ty)| {
            PlaceEntityType::Chest {
                pos,
                ty: ty.try_into().unwrap(),
            }
        }),
        (
            random_blueprint_offs()
            // 0..(data_store.solar_panel_sizes.len().try_into().unwrap())
        )
        .prop_map(|pos| PlaceEntityType::SolarPanel { pos, ty: 0 }),
        (random_blueprint_offs(), random_dir()).prop_map(|(pos, dir)| PlaceEntityType::Belt {
            pos,
            direction: dir,

            // TODO:
            ty: 0,
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
        (
            random_blueprint_offs(),
            (0u8..(data_store.lab_info.len().try_into().unwrap()))
        )
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
) -> impl Strategy<Value = Recipe<RecipeIdxType>> + use<ItemIdxType, RecipeIdxType> {
    (0..data_store.recipe_timers.len())
        .prop_map(|recipe_idx| RecipeIdxType::try_from(recipe_idx).unwrap())
        .prop_map(|id| Recipe { id })
}

pub fn random_item<ItemIdxType: IdxTrait, RecipeIdxType: IdxTrait>(
    data_store: &DataStore<ItemIdxType, RecipeIdxType>,
) -> impl Strategy<Value = Item<ItemIdxType>> + use<ItemIdxType, RecipeIdxType> {
    (0..data_store.item_names.len())
        .prop_map(|item_idx| ItemIdxType::try_from(item_idx).unwrap())
        .prop_map(|id| Item { id })
}

pub fn random_blueprint_offs() -> impl Strategy<Value = Position> {
    ((0u32..16), (0u32..16)).prop_map(|(x, y)| Position {
        x: x as i32,
        y: y as i32,
    })
}

pub fn random_position() -> impl Strategy<Value = Position> {
    ((1600u32..=1602u32), (1600u32..=1602u32)).prop_map(|(x, y)| Position {
        x: x as i32,
        y: y as i32,
    })
}
