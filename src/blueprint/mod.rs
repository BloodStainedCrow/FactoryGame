use log::error;
use proptest::{
    prelude::{Just, Strategy, prop},
    prop_oneof,
};
use std::num::NonZero;
use std::{borrow::Borrow, ops::Range};
use tilelib::types::{DrawInstance, Layer};

use crate::{belt::splitter::SplitterDistributionMode, item::Indexable};
use crate::{frontend::world::tile::UndergroundDir, item::WeakIdxTrait};

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
    item::{IdxTrait, Item, Recipe},
    rendering::{TextureAtlas, app_state::GameState},
    replays::Replay,
};

// For now blueprint will just be a list of actions
#[derive(Debug, Clone, serde::Serialize, serde::Deserialize)]
pub struct Blueprint {
    actions: Vec<BlueprintAction>,
}

#[derive(Debug, Clone)]
pub struct ReusableBlueprint<ItemIdxType: WeakIdxTrait, RecipeIdxType: WeakIdxTrait> {
    actions: Vec<ActionType<ItemIdxType, RecipeIdxType>>,
}

#[derive(Debug, Clone, serde::Deserialize, serde::Serialize)]
enum BlueprintAction {
    PlaceEntity(BlueprintPlaceEntity),

    SetRecipe {
        pos: Position,
        recipe: String,
    },

    OverrideInserterMovetime {
        pos: Position,
        new_movetime: Option<NonZero<u16>>,
    },

    AddModules {
        pos: Position,
        modules: Vec<String>,
    },

    SetChestSlotLimit {
        pos: Position,
        num_slots: u8,
    },
}

#[derive(Debug, Clone, serde::Deserialize, serde::Serialize)]
enum BlueprintPlaceEntity {
    Assembler {
        pos: Position,
        ty: String,
    },
    Inserter {
        pos: Position,
        dir: Dir,
        /// The Item the inserter will move, must fit both the in and output side
        filter: Option<String>,
    },
    Belt {
        pos: Position,
        direction: Dir,
        ty: String,
    },
    Underground {
        pos: Position,
        direction: Dir,
        ty: String,
        underground_dir: UndergroundDir,
    },
    PowerPole {
        pos: Position,
        ty: String,
    },
    Splitter {
        pos: Position,
        direction: Dir,
        ty: String,

        in_mode: Option<SplitterDistributionMode>,
        out_mode: Option<SplitterDistributionMode>,
    },
    Chest {
        pos: Position,
        ty: String,
    },
    SolarPanel {
        pos: Position,
        ty: String,
    },
    Lab {
        pos: Position,
        ty: String,
    },
    Beacon {
        ty: String,
        pos: Position,
    },
    FluidTank {
        ty: String,
        pos: Position,
        rotation: Dir,
    },
    MiningDrill {
        ty: String,
        pos: Position,
        rotation: Dir,
    },
}

impl BlueprintAction {
    fn from<ItemIdxType: IdxTrait, RecipeIdxType: IdxTrait>(
        action: &ActionType<ItemIdxType, RecipeIdxType>,
        data_store: &DataStore<ItemIdxType, RecipeIdxType>,
    ) -> Self {
        match action {
            ActionType::PlaceFloorTile(_) => unimplemented!(),
            ActionType::PlaceEntity(place_entity_info) => {
                match place_entity_info.entities.clone() {
                    EntityPlaceOptions::Single(place_entity_type) => {
                        let ty = match place_entity_type {
                            PlaceEntityType::Assembler { pos, ty } => {
                                BlueprintPlaceEntity::Assembler {
                                    pos,
                                    ty: data_store.assembler_info[ty as usize].name.clone(),
                                }
                            },
                            PlaceEntityType::Inserter { pos, dir, filter } => {
                                BlueprintPlaceEntity::Inserter {
                                    pos,
                                    dir,
                                    filter: filter.map(|item| {
                                        data_store.item_names[item.into_usize()].clone()
                                    }),
                                }
                            },
                            PlaceEntityType::Belt { pos, direction, ty } => {
                                BlueprintPlaceEntity::Belt {
                                    pos,
                                    direction,
                                    ty: data_store.belt_infos[ty as usize].name.clone(),
                                }
                            },
                            PlaceEntityType::Underground {
                                pos,
                                direction,
                                ty,
                                underground_dir,
                            } => BlueprintPlaceEntity::Underground {
                                pos,
                                direction,
                                ty: data_store.belt_infos[ty as usize].name.clone(),
                                underground_dir,
                            },
                            PlaceEntityType::PowerPole { pos, ty } => {
                                BlueprintPlaceEntity::PowerPole {
                                    pos,
                                    ty: data_store.power_pole_data[ty as usize].name.clone(),
                                }
                            },
                            PlaceEntityType::Splitter {
                                pos,
                                direction,
                                ty,
                                in_mode,
                                out_mode,
                            } => BlueprintPlaceEntity::Splitter {
                                pos,
                                direction,
                                ty: todo!(),
                                in_mode,
                                out_mode,
                            },
                            PlaceEntityType::Chest { pos, ty } => BlueprintPlaceEntity::Chest {
                                pos,
                                ty: data_store.chest_names[ty as usize].clone(),
                            },
                            PlaceEntityType::SolarPanel { pos, ty } => {
                                BlueprintPlaceEntity::SolarPanel {
                                    pos,
                                    ty: data_store.solar_panel_info[ty as usize].name.clone(),
                                }
                            },
                            PlaceEntityType::Lab { pos, ty } => BlueprintPlaceEntity::Lab {
                                pos,
                                ty: data_store.lab_info[ty as usize].name.clone(),
                            },
                            PlaceEntityType::Beacon { ty, pos } => BlueprintPlaceEntity::Beacon {
                                pos,
                                ty: data_store.beacon_info[ty as usize].name.clone(),
                            },
                            PlaceEntityType::FluidTank { ty, pos, rotation } => {
                                BlueprintPlaceEntity::FluidTank {
                                    pos,
                                    ty: data_store.fluid_tank_infos[ty as usize].name.clone(),
                                    rotation,
                                }
                            },
                            PlaceEntityType::MiningDrill { ty, pos, rotation } => {
                                BlueprintPlaceEntity::MiningDrill {
                                    pos,
                                    ty: data_store.mining_drill_info[ty as usize].name.clone(),
                                    rotation,
                                }
                            },
                        };
                        Self::PlaceEntity(ty)
                    },
                    EntityPlaceOptions::Multiple(_) => unimplemented!(),
                }
            },
            ActionType::SetRecipe(set_recipe_info) => Self::SetRecipe {
                pos: set_recipe_info.pos,
                recipe: data_store.recipe_names[set_recipe_info.recipe.into_usize()].clone(),
            },
            ActionType::OverrideInserterMovetime { pos, new_movetime } => {
                Self::OverrideInserterMovetime {
                    pos: *pos,
                    new_movetime: *new_movetime,
                }
            },
            ActionType::Position(_, _) => unimplemented!(),
            ActionType::AddModules { pos, modules } => Self::AddModules {
                pos: *pos,
                modules: modules
                    .iter()
                    .map(|module| data_store.module_info[*module].name.clone())
                    .collect(),
            },
            ActionType::RemoveModules { .. } => unimplemented!(),
            ActionType::SetChestSlotLimit { pos, num_slots } => Self::SetChestSlotLimit {
                pos: *pos,
                num_slots: *num_slots,
            },
            ActionType::Remove(_) => unimplemented!(),
            ActionType::SetActiveResearch { .. } => unimplemented!(),
            ActionType::CheatUnlockTechnology { .. } => unimplemented!(),
            ActionType::Ping(_) => unimplemented!(),
        }
    }

    fn try_into<ItemIdxType: IdxTrait, RecipeIdxType: IdxTrait>(
        &self,
        data_store: &DataStore<ItemIdxType, RecipeIdxType>,
    ) -> Result<ActionType<ItemIdxType, RecipeIdxType>, ()> {
        let action = match self {
            BlueprintAction::PlaceEntity(blueprint_place_entity) => {
                let entity = match blueprint_place_entity {
                    BlueprintPlaceEntity::Assembler { pos, ty } => PlaceEntityType::Assembler {
                        pos: *pos,
                        ty: data_store
                            .assembler_info
                            .iter()
                            .position(|info| info.name == *ty)
                            .map(|v| v.try_into().unwrap())
                            .ok_or(())?,
                    },
                    BlueprintPlaceEntity::Inserter { pos, dir, filter } => {
                        PlaceEntityType::Inserter {
                            pos: *pos,
                            dir: *dir,
                            filter: filter
                                .as_ref()
                                .map(|item| {
                                    data_store
                                        .item_names
                                        .iter()
                                        .position(|ds_item| ds_item == item)
                                        .map(|index| Item {
                                            id: index.try_into().unwrap(),
                                        })
                                        .ok_or(())
                                })
                                .transpose()?,
                        }
                    },
                    BlueprintPlaceEntity::Belt { pos, direction, ty } => PlaceEntityType::Belt {
                        pos: *pos,
                        direction: *direction,
                        ty: data_store
                            .belt_infos
                            .iter()
                            .position(|info| info.name == *ty)
                            .map(|v| v.try_into().unwrap())
                            .ok_or(())?,
                    },
                    BlueprintPlaceEntity::Underground {
                        pos,
                        direction,
                        ty,
                        underground_dir,
                    } => PlaceEntityType::Underground {
                        pos: *pos,
                        direction: *direction,
                        ty: data_store
                            .belt_infos
                            .iter()
                            .position(|info| info.name == *ty)
                            .map(|v| v.try_into().unwrap())
                            .ok_or(())?,
                        underground_dir: *underground_dir,
                    },
                    BlueprintPlaceEntity::PowerPole { pos, ty } => PlaceEntityType::PowerPole {
                        pos: *pos,
                        ty: data_store
                            .power_pole_data
                            .iter()
                            .position(|info| info.name == *ty)
                            .map(|v| v.try_into().unwrap())
                            .ok_or(())?,
                    },
                    BlueprintPlaceEntity::Splitter {
                        pos,
                        direction,
                        ty,
                        in_mode,
                        out_mode,
                    } => todo!(),
                    BlueprintPlaceEntity::Chest { pos, ty } => PlaceEntityType::Chest {
                        pos: *pos,
                        ty: data_store
                            .chest_names
                            .iter()
                            .position(|name| name == ty)
                            .map(|v| v.try_into().unwrap())
                            .ok_or(())?,
                    },
                    BlueprintPlaceEntity::SolarPanel { pos, ty } => PlaceEntityType::SolarPanel {
                        pos: *pos,
                        ty: data_store
                            .solar_panel_info
                            .iter()
                            .position(|info| info.name == *ty)
                            .map(|v| v.try_into().unwrap())
                            .ok_or(())?,
                    },
                    BlueprintPlaceEntity::Lab { pos, ty } => PlaceEntityType::Lab {
                        pos: *pos,
                        ty: data_store
                            .lab_info
                            .iter()
                            .position(|info| info.name == *ty)
                            .map(|v| v.try_into().unwrap())
                            .ok_or(())?,
                    },
                    BlueprintPlaceEntity::Beacon { ty, pos } => PlaceEntityType::Beacon {
                        pos: *pos,
                        ty: data_store
                            .beacon_info
                            .iter()
                            .position(|info| info.name == *ty)
                            .map(|v| v.try_into().unwrap())
                            .ok_or(())?,
                    },
                    BlueprintPlaceEntity::FluidTank { ty, pos, rotation } => {
                        PlaceEntityType::FluidTank {
                            pos: *pos,
                            ty: data_store
                                .fluid_tank_infos
                                .iter()
                                .position(|info| info.name == *ty)
                                .map(|v| v.try_into().unwrap())
                                .ok_or(())?,
                            rotation: *rotation,
                        }
                    },
                    BlueprintPlaceEntity::MiningDrill { ty, pos, rotation } => {
                        PlaceEntityType::MiningDrill {
                            pos: *pos,
                            ty: data_store
                                .mining_drill_info
                                .iter()
                                .position(|info| info.name == *ty)
                                .map(|v| v.try_into().unwrap())
                                .ok_or(())?,
                            rotation: *rotation,
                        }
                    },
                };

                ActionType::PlaceEntity(PlaceEntityInfo {
                    entities: EntityPlaceOptions::Single(entity),
                })
            },
            BlueprintAction::SetRecipe { pos, recipe } => ActionType::SetRecipe(SetRecipeInfo {
                pos: *pos,
                recipe: data_store
                    .recipe_names
                    .iter()
                    .position(|ds_recipe| ds_recipe == recipe)
                    .map(|index| Recipe {
                        id: index.try_into().unwrap(),
                    })
                    .ok_or(())?,
            }),
            BlueprintAction::OverrideInserterMovetime { pos, new_movetime } => {
                ActionType::OverrideInserterMovetime {
                    pos: *pos,
                    new_movetime: *new_movetime,
                }
            },
            BlueprintAction::AddModules { pos, modules } => ActionType::AddModules {
                pos: *pos,
                modules: modules
                    .iter()
                    .map(|action_module| {
                        data_store
                            .module_info
                            .iter()
                            .position(|module| module.name == *action_module)
                    })
                    .collect::<Option<_>>()
                    .ok_or(())?,
            },
            BlueprintAction::SetChestSlotLimit { pos, num_slots } => {
                ActionType::SetChestSlotLimit {
                    pos: *pos,
                    num_slots: *num_slots,
                }
            },
        };

        Ok(action)
    }
}

impl<ItemIdxType: IdxTrait, RecipeIdxType: IdxTrait> ReusableBlueprint<ItemIdxType, RecipeIdxType> {
    pub fn apply(
        &self,
        base_pos: Position,
        game_state: &mut GameState<ItemIdxType, RecipeIdxType>,
        data_store: &DataStore<ItemIdxType, RecipeIdxType>,
    ) {
        game_state.apply_actions(self.actions_with_base_pos(base_pos), data_store);
    }

    pub fn actions_with_base_pos(
        &self,
        base_pos: Position,
    ) -> impl Iterator<Item = ActionType<ItemIdxType, RecipeIdxType>> {
        self.actions
            .iter()
            .cloned()
            .map(move |a| match a {
                ActionType::PlaceFloorTile(PlaceFloorTileByHandInfo {
                    ghost_info:
                        PlaceFloorTileGhostInfo {
                            tile,
                            position: PositionInfo::Single { pos },
                        },
                    player,
                }) => ActionType::PlaceFloorTile(PlaceFloorTileByHandInfo {
                    ghost_info: PlaceFloorTileGhostInfo {
                        tile,
                        position: PositionInfo::Single {
                            pos: Position {
                                x: base_pos.x + pos.x,
                                y: base_pos.y + pos.y,
                            },
                        },
                    },
                    player,
                }),
                ActionType::PlaceEntity(PlaceEntityInfo {
                    entities: EntityPlaceOptions::Single(PlaceEntityType::Assembler { pos, ty }),
                }) => ActionType::PlaceEntity(PlaceEntityInfo {
                    entities: EntityPlaceOptions::Single(PlaceEntityType::Assembler {
                        pos: Position {
                            x: base_pos.x + pos.x,
                            y: base_pos.y + pos.y,
                        },
                        ty,
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
                        direction,
                        ty,
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
                        direction,
                        ty,
                        underground_dir,
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
                        ty,
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
                        dir,
                        filter,
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
                        ty,
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
                        direction,
                        in_mode,
                        out_mode,
                        ty,
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
                        ty,
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
                        ty,
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
                        ty,
                    }),
                }),
                ActionType::PlaceEntity(PlaceEntityInfo {
                    entities: EntityPlaceOptions::Single(PlaceEntityType::FluidTank { pos, ty, rotation }),
                }) => ActionType::PlaceEntity(PlaceEntityInfo {
                    entities: EntityPlaceOptions::Single(PlaceEntityType::FluidTank {
                        pos: Position {
                            x: base_pos.x + pos.x,
                            y: base_pos.y + pos.y,
                        },
                        ty,
                        rotation,
                    }),
                }),
                ActionType::SetRecipe(SetRecipeInfo { pos, recipe }) => {
                    ActionType::SetRecipe(SetRecipeInfo {
                        pos: Position {
                            x: base_pos.x + pos.x,
                            y: base_pos.y + pos.y,
                        },
                        recipe,
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
                    num_slots,
                },
                ActionType::OverrideInserterMovetime { pos, new_movetime } => {
                    ActionType::OverrideInserterMovetime {
                        pos: Position {
                            x: base_pos.x + pos.x,
                            y: base_pos.y + pos.y,
                        },
                        new_movetime,
                    }
                },
                a => unreachable!("{:?}", a),
            })
    }
}

impl Blueprint {
    pub fn get_reusable<ItemIdxType: IdxTrait, RecipeIdxType: IdxTrait>(
        &self,
        data_store: &DataStore<ItemIdxType, RecipeIdxType>,
    ) -> ReusableBlueprint<ItemIdxType, RecipeIdxType> {
        ReusableBlueprint {
            actions: self
                .actions
                .iter()
                .map(|bp_action| {
                    bp_action.try_into(data_store).expect(
                        format!("Action not possible with current mod set: {:?}", bp_action)
                            .as_str(),
                    )
                })
                .collect(),
        }
    }

    pub fn apply<ItemIdxType: IdxTrait, RecipeIdxType: IdxTrait>(
        &self,
        base_pos: Position,
        game_state: &mut GameState<ItemIdxType, RecipeIdxType>,
        data_store: &DataStore<ItemIdxType, RecipeIdxType>,
    ) {
        let reusable = self.get_reusable(data_store);
        game_state.apply_actions(reusable.actions_with_base_pos(base_pos), data_store);
    }

    pub fn from_replay<
        ItemIdxType: IdxTrait,
        RecipeIdxType: IdxTrait,
        DS: Borrow<DataStore<ItemIdxType, RecipeIdxType>>,
    >(
        replay: &Replay<ItemIdxType, RecipeIdxType, DS>,
    ) -> Self {
        Self {
            actions: replay
                .actions
                .iter()
                .map(|ra| BlueprintAction::from(&ra.action, replay.data_store.borrow()))
                .collect(),
        }
    }

    pub fn from_area<ItemIdxType: IdxTrait, RecipeIdxType: IdxTrait>(
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
                            pos: Position {
                                x: pos.x - base_pos.x,
                                y: pos.y - base_pos.y,
                            },
                            rotation: *rotation,
                        }),
                    })]
                },
            };

            bp.actions.extend(
                actions
                    .into_iter()
                    .map(|action| BlueprintAction::from(&action, data_store)),
            );
        }

        bp
    }

    pub fn draw<ItemIdxType: IdxTrait, RecipeIdxType: IdxTrait>(
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
            let Ok(action) = action.try_into(data_store) else {
                error!("Could not draw blueprint!");
                return;
            };
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

pub fn random_blueprint_strategy<'a, ItemIdxType: IdxTrait, RecipeIdxType: IdxTrait>(
    len_range: Range<usize>,
    data_store: &'a DataStore<ItemIdxType, RecipeIdxType>,
) -> impl Strategy<Value = Blueprint> + use<'a, ItemIdxType, RecipeIdxType> {
    prop::collection::vec(random_blueprint_action(data_store), len_range).prop_map(|actions| {
        Blueprint {
            actions: actions
                .into_iter()
                .map(|v| BlueprintAction::from(&v, data_store))
                .collect(),
        }
    })
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
    (0..data_store.item_display_names.len())
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
