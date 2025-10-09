use std::num::NonZero;

use place_entity::PlaceEntityInfo;
use place_tile::PlaceFloorTileByHandInfo;
use set_recipe::SetRecipeInfo;

use super::world::{Position, tile::PlaceEntityType};
use crate::frontend::world::tile::ModuleTy;
use crate::{
    data::DataStore,
    item::{IdxTrait, WeakIdxTrait},
    research::Technology,
};

#[cfg(feature = "client")]
pub mod action_state_machine;
pub mod belt_placement;
pub mod place_entity;
pub mod place_tile;
pub mod set_recipe;

pub type PLAYERID = u16;

// TODO: Do I want actions to also encode the previous state in some capacity to make discarding nonsensical actions easier with network delays?
#[derive(Debug, Clone, serde::Serialize, serde::Deserialize)]
pub enum ActionType<ItemIdxType: WeakIdxTrait, RecipeIdxType: WeakIdxTrait> {
    PlaceFloorTile(PlaceFloorTileByHandInfo),
    PlaceEntity(PlaceEntityInfo<ItemIdxType>),

    SetRecipe(SetRecipeInfo<RecipeIdxType>),

    OverrideInserterMovetime {
        pos: Position,
        new_movetime: Option<NonZero<u16>>,
    },

    Position(PLAYERID, (f32, f32)),

    AddModules {
        pos: Position,
        modules: Vec<ModuleTy>,
    },
    RemoveModules {
        pos: Position,
        indices: Vec<usize>,
    },

    SetChestSlotLimit {
        pos: Position,
        num_slots: u8,
    },

    Remove(Position),

    SetActiveResearch {
        tech: Option<Technology>,
    },

    CheatUnlockTechnology {
        tech: Technology,
    },

    CheatRelockTechnology {
        tech: Technology,
    },

    Ping(Position),
}

impl<ItemIdxType: IdxTrait, RecipeIdxType: IdxTrait> ActionType<ItemIdxType, RecipeIdxType> {
    pub fn get_pos(&self) -> Option<Position> {
        match self {
            ActionType::PlaceFloorTile(place_floor_tile_by_hand_info) => todo!(),
            ActionType::PlaceEntity(place_entity_info) => match &place_entity_info.entities {
                place_entity::EntityPlaceOptions::Single(place_entity_type) => {
                    match place_entity_type {
                        super::world::tile::PlaceEntityType::Assembler { pos, .. } => Some(*pos),
                        super::world::tile::PlaceEntityType::Inserter { pos, .. } => Some(*pos),
                        super::world::tile::PlaceEntityType::Belt { pos, .. } => Some(*pos),
                        super::world::tile::PlaceEntityType::Underground { pos, .. } => Some(*pos),
                        super::world::tile::PlaceEntityType::PowerPole { pos, .. } => Some(*pos),
                        super::world::tile::PlaceEntityType::Splitter { pos, .. } => Some(*pos),
                        super::world::tile::PlaceEntityType::Chest { pos, .. } => Some(*pos),
                        super::world::tile::PlaceEntityType::SolarPanel { pos, .. } => Some(*pos),
                        super::world::tile::PlaceEntityType::Lab { pos, .. } => Some(*pos),
                        super::world::tile::PlaceEntityType::Beacon { pos, .. } => Some(*pos),
                        super::world::tile::PlaceEntityType::FluidTank { pos, .. } => Some(*pos),
                        super::world::tile::PlaceEntityType::MiningDrill { pos, .. } => Some(*pos),
                    }
                },
                place_entity::EntityPlaceOptions::Multiple(place_entity_types) => todo!(),
            },
            ActionType::SetRecipe(set_recipe_info) => Some(set_recipe_info.pos),
            ActionType::OverrideInserterMovetime { pos, .. } => Some(*pos),
            ActionType::Position(_, _) => todo!(),
            ActionType::AddModules { pos, .. } => Some(*pos),
            ActionType::RemoveModules { pos, .. } => Some(*pos),
            ActionType::SetChestSlotLimit { pos, .. } => Some(*pos),
            ActionType::Remove(position) => Some(*position),
            ActionType::SetActiveResearch { .. } => None,
            ActionType::CheatUnlockTechnology { .. } => None,
            ActionType::CheatRelockTechnology { .. } => None,
            ActionType::Ping(position) => Some(*position),
        }
    }

    pub fn get_building_size(
        &self,
        data_store: &DataStore<ItemIdxType, RecipeIdxType>,
    ) -> Option<[u16; 2]> {
        match self {
            ActionType::PlaceFloorTile(place_floor_tile_by_hand_info) => todo!(),
            ActionType::PlaceEntity(place_entity_info) => match &place_entity_info.entities {
                place_entity::EntityPlaceOptions::Single(place_entity_type) => {
                    match place_entity_type {
                        PlaceEntityType::Assembler { ty, rotation, .. } => Some(
                            data_store.assembler_info[usize::from(*ty)]
                                .size(*rotation)
                                .into(),
                        ),
                        // FIXME: ty
                        PlaceEntityType::Inserter {
                            pos: _,
                            dir: _,
                            filter: _,
                            ty: _,
                        } => Some([1, 1]),
                        PlaceEntityType::Belt { .. } => Some([1, 1]),
                        PlaceEntityType::Underground { .. } => Some([1, 1]),
                        PlaceEntityType::PowerPole { ty, .. } => {
                            Some(data_store.power_pole_data[*ty as usize].size.into())
                        },
                        PlaceEntityType::Splitter { direction, .. } => {
                            Some(match direction.compare(super::world::tile::Dir::North) {
                                super::world::tile::DirRelative::SameDir
                                | super::world::tile::DirRelative::Opposite => [2, 1],
                                super::world::tile::DirRelative::Turned => [1, 2],
                            })
                        },
                        PlaceEntityType::Chest { ty, .. } => {
                            Some(data_store.chest_tile_sizes[*ty as usize].into())
                        },
                        PlaceEntityType::SolarPanel { ty, .. } => {
                            Some(data_store.solar_panel_info[*ty as usize].size)
                        },
                        PlaceEntityType::Lab { ty, .. } => {
                            Some(data_store.lab_info[*ty as usize].size.into())
                        },
                        PlaceEntityType::Beacon { ty, .. } => {
                            Some(data_store.beacon_info[*ty as usize].size.into())
                        },
                        PlaceEntityType::FluidTank { ty, .. } => {
                            Some(data_store.fluid_tank_infos[*ty as usize].size.into())
                        },
                        PlaceEntityType::MiningDrill { ty, .. } => {
                            Some(data_store.mining_drill_info[*ty as usize].size)
                        },
                    }
                },
                place_entity::EntityPlaceOptions::Multiple(place_entity_types) => todo!(),
            },
            ActionType::SetRecipe(_) => None,
            ActionType::OverrideInserterMovetime { .. } => None,
            ActionType::Position(_, _) => None,
            ActionType::AddModules { .. } => None,
            ActionType::RemoveModules { .. } => None,
            ActionType::SetChestSlotLimit { .. } => None,
            ActionType::Remove(_) => None,
            ActionType::SetActiveResearch { .. } => None,
            ActionType::CheatUnlockTechnology { .. } => None,
            ActionType::CheatRelockTechnology { .. } => None,
            ActionType::Ping(_) => None,
        }
    }

    pub fn get_effect_size(
        &self,
        data_store: &DataStore<ItemIdxType, RecipeIdxType>,
    ) -> Option<[u16; 2]> {
        self.get_building_size(data_store).or(match self {
            ActionType::PlaceFloorTile(place_floor_tile_by_hand_info) => todo!(),
            ActionType::PlaceEntity(place_entity_info) => match &place_entity_info.entities {
                place_entity::EntityPlaceOptions::Single(place_entity_type) => None,
                place_entity::EntityPlaceOptions::Multiple(place_entity_types) => None,
            },
            ActionType::SetRecipe(set_recipe_info) => Some([1, 1]),
            ActionType::OverrideInserterMovetime { .. } => Some([1, 1]),
            ActionType::Position(_, _) => None,
            ActionType::AddModules { .. } => Some([1, 1]),
            ActionType::RemoveModules { .. } => Some([1, 1]),
            ActionType::SetChestSlotLimit { .. } => Some([1, 1]),
            ActionType::Remove(position) => Some([1, 1]),
            ActionType::SetActiveResearch { .. } => None,
            ActionType::CheatUnlockTechnology { .. } => None,
            ActionType::CheatRelockTechnology { .. } => None,
            ActionType::Ping(position) => None,
        })
    }
}
