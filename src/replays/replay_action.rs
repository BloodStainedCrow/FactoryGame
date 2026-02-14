use std::num::NonZero;

use petgraph::graph::NodeIndex;

use crate::Position;
use crate::belt::splitter::SplitterDistributionMode;
use crate::data::DataStore;
use crate::frontend::action::place_entity::{EntityPlaceOptions, PlaceEntityInfo};
use crate::frontend::action::set_recipe::SetRecipeInfo;
use crate::frontend::world::tile::Dir;
use crate::frontend::world::tile::PlaceEntityType;
use crate::frontend::world::tile::UndergroundDir;
use crate::item::{Indexable, Item, Recipe};
use crate::{frontend::action::ActionType, item::IdxTrait};

#[derive(Debug, Clone, serde::Serialize, serde::Deserialize)]
struct ReplayPlaceEntity {
    pos: Position,
    ty: String,
    rotation: Dir,

    kind: ReplayPlaceEntityKind,
}

#[derive(Debug, Clone, serde::Serialize, serde::Deserialize)]
enum ReplayPlaceEntityKind {
    Assembler {},
    Inserter {
        /// The Item the inserter will move, must fit both the in and output side
        filter: Option<String>,

        user_movetime: Option<NonZero<u16>>,
    },
    Belt {},
    Underground {
        underground_dir: UndergroundDir,
    },
    PowerPole {},
    Splitter {
        in_mode: Option<SplitterDistributionMode>,
        out_mode: Option<SplitterDistributionMode>,
    },
    Chest {},
    SolarPanel {},
    Accumulator {},
    Lab {},
    Beacon {},
    FluidTank {},
    MiningDrill {},
}
impl ReplayPlaceEntity {
    fn from_entity_place<A: IdxTrait, B: IdxTrait>(
        ty: &PlaceEntityType<A>,
        data_store: &DataStore<A, B>,
    ) -> Self {
        use ReplayPlaceEntityKind::*;
        match ty.clone() {
            PlaceEntityType::Assembler { pos, ty, rotation } => Self {
                pos,
                ty: data_store.assembler_info[ty as usize].name.to_string(),
                rotation,
                kind: Assembler {},
            },
            PlaceEntityType::Inserter {
                ty,
                pos,
                dir,
                filter,
                user_movetime,
            } => Self {
                pos,
                ty: data_store.inserter_infos[ty as usize].name.to_string(),
                rotation: dir,
                kind: Inserter {
                    filter: filter.map(|item| data_store.item_names[item.into_usize()].to_string()),
                    user_movetime,
                },
            },
            PlaceEntityType::Belt { pos, direction, ty } => Self {
                pos,
                ty: data_store.belt_infos[ty as usize].name.to_string(),
                rotation: direction,
                kind: Belt {},
            },
            PlaceEntityType::Underground {
                pos,
                direction,
                ty,
                underground_dir,
            } => Self {
                pos,
                ty: data_store.belt_infos[ty as usize].name.to_string(),
                rotation: direction,
                kind: Underground { underground_dir },
            },
            PlaceEntityType::PowerPole { pos, ty } => Self {
                pos,
                ty: data_store.power_pole_data[ty as usize].name.to_string(),
                rotation: Dir::default(),
                kind: PowerPole {},
            },
            PlaceEntityType::Splitter {
                pos,
                direction,
                ty,
                in_mode,
                out_mode,
            } => Self {
                pos,
                ty: data_store.belt_infos[ty as usize].name.to_string(),
                rotation: direction,
                kind: Splitter { in_mode, out_mode },
            },
            PlaceEntityType::Chest { pos, ty } => Self {
                pos,
                ty: data_store.chest_names[ty as usize].to_string(),
                rotation: Dir::default(),
                kind: Chest {},
            },
            PlaceEntityType::SolarPanel { pos, ty } => Self {
                pos,
                ty: data_store.solar_panel_info[ty as usize].name.to_string(),
                rotation: Dir::default(),
                kind: SolarPanel {},
            },
            PlaceEntityType::Accumulator { pos, ty } => Self {
                pos,
                ty: data_store.accumulator_info[ty as usize].name.to_string(),
                rotation: Dir::default(),
                kind: Accumulator {},
            },
            PlaceEntityType::Lab { pos, ty } => Self {
                pos,
                ty: data_store.lab_info[ty as usize].name.to_string(),
                rotation: Dir::default(),
                kind: Lab {},
            },
            PlaceEntityType::Beacon { ty, pos } => Self {
                pos,
                ty: data_store.beacon_info[ty as usize].name.to_string(),
                rotation: Dir::default(),
                kind: Beacon {},
            },
            PlaceEntityType::FluidTank { ty, pos, rotation } => Self {
                pos,
                ty: data_store.fluid_tank_infos[ty as usize].name.to_string(),
                rotation,
                kind: FluidTank {},
            },
            PlaceEntityType::MiningDrill { ty, pos, rotation } => Self {
                pos,
                ty: data_store.mining_drill_info[ty as usize].name.to_string(),
                rotation,
                kind: MiningDrill {},
            },
        }
    }

    fn to_entity_place<A: IdxTrait, B: IdxTrait>(
        self,
        data_store: &DataStore<A, B>,
    ) -> Result<PlaceEntityType<A>, ReplayActionError> {
        use PlaceEntityType::*;
        let Self {
            pos,
            ty,
            rotation,
            kind,
        } = self;
        let ty = match kind {
            ReplayPlaceEntityKind::Assembler {} => Assembler {
                pos,
                ty: data_store
                    .assembler_info
                    .iter()
                    .position(|info| *info.name == *ty)
                    .map(|v| v.try_into().unwrap())
                    .ok_or(ReplayActionError::MissingDatastoreEntry(format!(
                        "Missing Assembler {}",
                        ty
                    )))?,
                rotation,
            },
            ReplayPlaceEntityKind::Inserter {
                filter,
                user_movetime,
            } => Inserter {
                ty: data_store
                    .inserter_infos
                    .iter()
                    .position(|info| *info.name == *ty)
                    .map(|v| v.try_into().unwrap())
                    .ok_or(ReplayActionError::MissingDatastoreEntry(format!(
                        "Missing Inserter {}",
                        ty
                    )))?,
                pos,
                dir: rotation,
                filter: filter
                    .map(|filter| {
                        data_store
                            .item_names
                            .iter()
                            .position(|info| **info == *ty)
                            .map(|v| Item::from(A::try_from(v).unwrap()))
                            .ok_or(ReplayActionError::MissingDatastoreEntry(format!(
                                "Missing Item {}",
                                filter
                            )))
                    })
                    .transpose()?,
                user_movetime,
            },
            ReplayPlaceEntityKind::Belt {} => Belt {
                pos,
                direction: rotation,
                ty: data_store
                    .belt_infos
                    .iter()
                    .position(|info| *info.name == *ty)
                    .map(|v| v.try_into().unwrap())
                    .ok_or(ReplayActionError::MissingDatastoreEntry(format!(
                        "Missing Belt {}",
                        ty
                    )))?,
            },
            ReplayPlaceEntityKind::Underground { underground_dir } => Underground {
                pos,
                direction: rotation,
                ty: data_store
                    .belt_infos
                    .iter()
                    .position(|info| *info.name == *ty)
                    .map(|v| v.try_into().unwrap())
                    .ok_or(ReplayActionError::MissingDatastoreEntry(format!(
                        "Missing Belt {}",
                        ty
                    )))?,
                underground_dir,
            },
            ReplayPlaceEntityKind::PowerPole {} => PowerPole {
                pos,
                ty: data_store
                    .power_pole_data
                    .iter()
                    .position(|info| *info.name == *ty)
                    .map(|v| v.try_into().unwrap())
                    .ok_or(ReplayActionError::MissingDatastoreEntry(format!(
                        "Missing Power Pole {}",
                        ty
                    )))?,
            },
            ReplayPlaceEntityKind::Splitter { in_mode, out_mode } => Splitter {
                pos,
                direction: rotation,
                ty: data_store
                    .belt_infos
                    .iter()
                    .position(|info| *info.name == *ty)
                    .map(|v| v.try_into().unwrap())
                    .ok_or(ReplayActionError::MissingDatastoreEntry(format!(
                        "Missing Belt {}",
                        ty
                    )))?,
                in_mode,
                out_mode,
            },
            ReplayPlaceEntityKind::Chest {} => Chest {
                pos,
                ty: data_store
                    .chest_names
                    .iter()
                    .position(|info| **info == *ty)
                    .map(|v| v.try_into().unwrap())
                    .ok_or(ReplayActionError::MissingDatastoreEntry(format!(
                        "Missing Chest {}",
                        ty
                    )))?,
            },
            ReplayPlaceEntityKind::SolarPanel {} => SolarPanel {
                pos,
                ty: data_store
                    .solar_panel_info
                    .iter()
                    .position(|info| *info.name == *ty)
                    .map(|v| v.try_into().unwrap())
                    .ok_or(ReplayActionError::MissingDatastoreEntry(format!(
                        "Missing Solar Panel {}",
                        ty
                    )))?,
            },
            ReplayPlaceEntityKind::Accumulator {} => Accumulator {
                pos,
                ty: data_store
                    .accumulator_info
                    .iter()
                    .position(|info| *info.name == *ty)
                    .map(|v| v.try_into().unwrap())
                    .ok_or(ReplayActionError::MissingDatastoreEntry(format!(
                        "Missing Accumulator {}",
                        ty
                    )))?,
            },
            ReplayPlaceEntityKind::Lab {} => Lab {
                pos,
                ty: data_store
                    .lab_info
                    .iter()
                    .position(|info| *info.name == *ty)
                    .map(|v| v.try_into().unwrap())
                    .ok_or(ReplayActionError::MissingDatastoreEntry(format!(
                        "Missing Lab {}",
                        ty
                    )))?,
            },
            ReplayPlaceEntityKind::Beacon {} => Beacon {
                ty: data_store
                    .beacon_info
                    .iter()
                    .position(|info| *info.name == *ty)
                    .map(|v| v.try_into().unwrap())
                    .ok_or(ReplayActionError::MissingDatastoreEntry(format!(
                        "Missing Beacon {}",
                        ty
                    )))?,
                pos,
            },
            ReplayPlaceEntityKind::FluidTank {} => FluidTank {
                ty: data_store
                    .fluid_tank_infos
                    .iter()
                    .position(|info| *info.name == *ty)
                    .map(|v| v.try_into().unwrap())
                    .ok_or(ReplayActionError::MissingDatastoreEntry(format!(
                        "Missing FluidTank {}",
                        ty
                    )))?,
                pos,
                rotation,
            },
            ReplayPlaceEntityKind::MiningDrill {} => MiningDrill {
                ty: data_store
                    .mining_drill_info
                    .iter()
                    .position(|info| *info.name == *ty)
                    .map(|v| v.try_into().unwrap())
                    .ok_or(ReplayActionError::MissingDatastoreEntry(format!(
                        "Missing Mining Drill {}",
                        ty
                    )))?,
                pos,
                rotation,
            },
        };

        Ok(ty)
    }
}

#[derive(Debug, Clone, serde::Serialize, serde::Deserialize)]
pub(super) enum ReplayAction {
    PlaceFloorTile {
        pos: Position,
        ty: String,
    },
    PlaceEntity {
        force: bool,
        info: ReplayPlaceEntity,
    },

    SetRecipe {
        pos: Position,
        recipe: String,
    },

    OverrideInserterMovetime {
        pos: Position,
        new_movetime: Option<NonZero<u16>>,
    },

    Position {
        player: String,
        pos: (f32, f32),
    },

    AddModules {
        pos: Position,
        modules: Vec<String>,
    },
    RemoveModules {
        pos: Position,
        indices: Vec<usize>,
    },

    SetChestSlotLimit {
        pos: Position,
        num_slots: u8,
    },

    Remove {
        pos: Position,
    },

    AddResearchToQueue {
        tech: String,
    },

    RemoveResearchFromQueue {
        tech: String,
    },

    CheatUnlockTechnology {
        tech: String,
    },

    CheatRelockTechnology {
        tech: String,
    },

    PlaceOre {
        pos: Position,
        ore: String,
        amount: u32,
    },

    SpawnPlayer {},
}

impl ReplayAction {
    pub(super) fn from_action<A: IdxTrait, B: IdxTrait>(
        action: ActionType<A, B>,
        data_store: &DataStore<A, B>,
    ) -> Self {
        match action {
            ActionType::PlaceFloorTile(_) => todo!(),
            ActionType::PlaceEntity(PlaceEntityInfo {
                entities: EntityPlaceOptions::Single(ty),
                force,
            }) => Self::PlaceEntity {
                force,
                info: ReplayPlaceEntity::from_entity_place(&ty, data_store),
            },
            ActionType::SetRecipe(set_recipe_info) => Self::SetRecipe {
                pos: set_recipe_info.pos,
                recipe: data_store.recipe_names[set_recipe_info.recipe.into_usize()].to_string(),
            },
            ActionType::OverrideInserterMovetime { pos, new_movetime } => {
                Self::OverrideInserterMovetime { pos, new_movetime }
            },
            ActionType::Position(id, pos) => Self::Position {
                player: format!("{id}"),
                pos,
            },
            ActionType::AddModules { pos, modules } => Self::AddModules {
                pos,
                modules: modules
                    .into_iter()
                    .map(|module| data_store.module_info[module as usize].name.to_string())
                    .collect(),
            },
            ActionType::RemoveModules { pos, indices } => Self::RemoveModules { pos, indices },
            ActionType::SetChestSlotLimit { pos, num_slots } => {
                Self::SetChestSlotLimit { pos, num_slots }
            },
            ActionType::Remove(position) => Self::Remove { pos: position },
            ActionType::AddResearchToQueue { tech } => Self::AddResearchToQueue {
                tech: data_store
                    .technology_tree
                    .node_weight(NodeIndex::new(tech.id.into()))
                    .unwrap()
                    .name
                    .clone(),
            },
            ActionType::RemoveResearchFromQueue { tech } => Self::RemoveResearchFromQueue {
                tech: data_store
                    .technology_tree
                    .node_weight(NodeIndex::new(tech.id.into()))
                    .unwrap()
                    .name
                    .clone(),
            },
            ActionType::CheatUnlockTechnology { tech } => Self::CheatUnlockTechnology {
                tech: data_store
                    .technology_tree
                    .node_weight(NodeIndex::new(tech.id.into()))
                    .unwrap()
                    .name
                    .clone(),
            },
            ActionType::CheatRelockTechnology { tech } => Self::CheatRelockTechnology {
                tech: data_store
                    .technology_tree
                    .node_weight(NodeIndex::new(tech.id.into()))
                    .unwrap()
                    .name
                    .clone(),
            },
            ActionType::PlaceOre { pos, ore, amount } => Self::PlaceOre {
                pos,
                ore: data_store.item_names[ore.into_usize()].to_string(),
                amount,
            },
            ActionType::Ping(position) => todo!(),
            ActionType::SpawnPlayer {} => Self::SpawnPlayer {},
        }
    }

    pub(super) fn to_action<A: IdxTrait, B: IdxTrait>(
        self,
        data_store: &DataStore<A, B>,
    ) -> Result<ActionType<A, B>, ReplayActionError> {
        use ActionType::*;
        let action = match self {
            ReplayAction::PlaceFloorTile { pos, ty } => todo!(),
            ReplayAction::PlaceEntity { force, info } => PlaceEntity(PlaceEntityInfo {
                entities: EntityPlaceOptions::Single(info.to_entity_place(data_store)?),
                force,
            }),
            ReplayAction::SetRecipe { pos, recipe } => SetRecipe(SetRecipeInfo {
                pos,
                recipe: Recipe::from(
                    B::try_from(
                        data_store
                            .recipe_names
                            .iter()
                            .position(|name| **name == *recipe)
                            .ok_or(ReplayActionError::MissingDatastoreEntry(format!(
                                "Missing recipe {}",
                                recipe
                            )))?,
                    )
                    .unwrap(),
                ),
            }),
            ReplayAction::OverrideInserterMovetime { pos, new_movetime } => {
                OverrideInserterMovetime { pos, new_movetime }
            },
            ReplayAction::Position { player, pos } => Position(player.parse().unwrap(), pos),
            ReplayAction::AddModules { pos, modules } => AddModules {
                pos,
                modules: modules
                    .into_iter()
                    .map(|mod_name| {
                        data_store
                            .module_info
                            .iter()
                            .position(|modinfo| *modinfo.name == *mod_name)
                            .map(|v| v.try_into().unwrap())
                            .ok_or(ReplayActionError::MissingDatastoreEntry(format!(
                                "Missing module {}",
                                mod_name
                            )))
                    })
                    .try_collect()?,
            },
            ReplayAction::RemoveModules { pos, indices } => RemoveModules { pos, indices },
            ReplayAction::SetChestSlotLimit { pos, num_slots } => {
                SetChestSlotLimit { pos, num_slots }
            },
            ReplayAction::Remove { pos } => Remove(pos),
            ReplayAction::AddResearchToQueue { tech } => AddResearchToQueue {
                tech: crate::research::Technology {
                    id: data_store
                        .technology_tree
                        .node_indices()
                        .find(|index| {
                            data_store.technology_tree.node_weight(*index).unwrap().name == tech
                        })
                        .ok_or(ReplayActionError::MissingDatastoreEntry(format!(
                            "Missing technology {}",
                            tech
                        )))?
                        .index()
                        .try_into()
                        .unwrap(),
                },
            },
            ReplayAction::RemoveResearchFromQueue { tech } => RemoveResearchFromQueue {
                tech: crate::research::Technology {
                    id: data_store
                        .technology_tree
                        .node_indices()
                        .find(|index| {
                            data_store.technology_tree.node_weight(*index).unwrap().name == tech
                        })
                        .ok_or(ReplayActionError::MissingDatastoreEntry(format!(
                            "Missing technology {}",
                            tech
                        )))?
                        .index()
                        .try_into()
                        .unwrap(),
                },
            },
            ReplayAction::CheatUnlockTechnology { tech } => CheatUnlockTechnology {
                tech: crate::research::Technology {
                    id: data_store
                        .technology_tree
                        .node_indices()
                        .find(|index| {
                            data_store.technology_tree.node_weight(*index).unwrap().name == tech
                        })
                        .ok_or(ReplayActionError::MissingDatastoreEntry(format!(
                            "Missing technology {}",
                            tech
                        )))?
                        .index()
                        .try_into()
                        .unwrap(),
                },
            },
            ReplayAction::CheatRelockTechnology { tech } => CheatRelockTechnology {
                tech: crate::research::Technology {
                    id: data_store
                        .technology_tree
                        .node_indices()
                        .find(|index| {
                            data_store.technology_tree.node_weight(*index).unwrap().name == tech
                        })
                        .ok_or(ReplayActionError::MissingDatastoreEntry(format!(
                            "Missing technology {}",
                            tech
                        )))?
                        .index()
                        .try_into()
                        .unwrap(),
                },
            },
            ReplayAction::PlaceOre { pos, ore, amount } => PlaceOre {
                pos,
                ore: Item::from(
                    A::try_from(
                        data_store
                            .item_names
                            .iter()
                            .position(|name| **name == *ore)
                            .ok_or(ReplayActionError::MissingDatastoreEntry(format!(
                                "Missing item {}",
                                ore
                            )))?,
                    )
                    .unwrap(),
                ),
                amount,
            },
            ReplayAction::SpawnPlayer {} => SpawnPlayer {},
        };

        Ok(action)
    }
}

#[derive(Debug)]
pub enum ReplayActionError {
    MissingDatastoreEntry(String),
}
