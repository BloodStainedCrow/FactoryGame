use std::{
    collections::{BTreeMap, HashMap},
    iter,
};

use crate::{
    DataStore, EntityIdentifier, EntityPrototypeKind, ModIdentifier,
    api::{
        entity::{
            assembler::AssemblerInfo, belt::BeltInfo, chest::ChestInfo, inserter::InserterInfo,
            power_pole::PowerPoleInfo,
        },
        item::ItemInfo,
        recipe::RecipeInfo,
    },
    entity::{GlobalTy, assember::Recipe, power_pole::PowerPoleData},
    item::{Item, item_set::ItemSet},
    spacial::{Extent, Offset},
};

pub(crate) mod entity;
pub(crate) mod item;
pub(crate) mod recipe;

pub use item::ItemIdentifier;
pub use recipe::RecipeIdentifier;

pub(crate) struct ModData {
    pub mod_name: ModIdentifier,

    pub inserters: Vec<InserterInfo>,
    pub assemblers: Vec<AssemblerInfo>,
    pub power_poles: Vec<PowerPoleInfo>,
    pub chests: Vec<ChestInfo>,
    pub belts: Vec<BeltInfo>,

    pub items: Vec<ItemInfo>,
    pub recipes: Vec<RecipeInfo>,
}

impl DataStore {
    #[expect(clippy::too_many_lines)]
    pub fn from_mods(mods: &[ModData]) -> Self {
        let entities: Vec<_> =
            mods.iter()
                .flat_map(|mod_| {
                    let ModData {
                        mod_name,
                        inserters,
                        assemblers,
                        power_poles,
                        chests,
                        belts,

                        items: _,
                        recipes: _,
                    } = mod_;

                    power_poles
                        .iter()
                        .map(|pole| (&pole.entity_info, EntityPrototypeKind::PowerPole))
                        .chain(
                            inserters.iter().map(|inserter| {
                                (&inserter.entity_info, EntityPrototypeKind::Inserter)
                            }),
                        )
                        .chain(assemblers.iter().map(|assembler_info| {
                            (&assembler_info.entity_info, EntityPrototypeKind::Assembler)
                        }))
                        .chain(chests.iter().map(|chest_info| {
                            (&chest_info.entity_info, EntityPrototypeKind::Chest)
                        }))
                        .chain(belts.iter().map(|belt_info| {
                            assert_eq!(
                                belt_info.entity_info.size,
                                Extent {
                                    width: 1,
                                    height: 1
                                }
                            );

                            (&belt_info.entity_info, EntityPrototypeKind::Belt)
                        }))
                        .map(move |(entity_info, kind)| crate::EntityInfo {
                            size: entity_info.size,
                            can_be_rotated: entity_info.can_be_rotated,
                            can_be_flipped: entity_info.can_be_flipped,
                            kind,
                            name: EntityIdentifier::new(mod_name, &entity_info.name),
                            display_name: entity_info.display_name.clone(),
                            placement_rules: entity_info.placement_rules.clone(),
                        })
                })
                .collect();

        let items: Vec<_> = mods
            .iter()
            .flat_map(|mod_| {
                let ModData { items, .. } = mod_;

                items.iter().map(|item| crate::item::ItemInfo {
                    identifier: ItemIdentifier::new(&mod_.mod_name, &item.name),
                    stack_size: item.stack_size,
                })
            })
            .collect();

        let item_to_id: HashMap<ItemIdentifier, Item> = items
            .iter()
            .enumerate()
            .map(|(idx, e)| {
                (
                    e.identifier.clone(),
                    Item(u16::try_from(idx).expect("More than u16::MAX items")),
                )
            })
            .collect();

        let recipes: Vec<_> = iter::once(crate::recipe::RecipeInfo {
            recipe_identifier: RecipeIdentifier::new_raw("no_recipe".to_string()),
            inputs: ItemSet::empty(),
            outputs: ItemSet::empty(),
        })
        .chain(mods.iter().flat_map(|mod_| {
            let ModData { recipes, .. } = mod_;

            recipes.iter().map(|recipe| crate::recipe::RecipeInfo {
                recipe_identifier: RecipeIdentifier::new(&mod_.mod_name, &recipe.name),
                inputs: recipe
                    .ingredients
                    .keys()
                    .map(|item| item_to_id[item])
                    .collect(),
                outputs: recipe.results.keys().map(|item| item_to_id[item]).collect(),
            })
        }))
        .collect();

        assert!(u16::try_from(entities.len()).is_ok());

        let full_name_to_global_id: HashMap<EntityIdentifier, GlobalTy> = entities
            .iter()
            .enumerate()
            .map(|(idx, e)| {
                (
                    e.name.clone(),
                    GlobalTy::from(u16::try_from(idx).expect("More than u16::MAX entities")),
                )
            })
            .collect();

        let recipe_to_id: HashMap<RecipeIdentifier, Recipe> = recipes
            .iter()
            .enumerate()
            .map(|(idx, e)| {
                (
                    e.recipe_identifier.clone(),
                    Recipe(u16::try_from(idx).expect("More than u16::MAX recipes")),
                )
            })
            .collect();

        let power_poles = mods
            .iter()
            .flat_map(|mod_| {
                mod_.power_poles.iter().map(|pole| PowerPoleData {
                    wire_connection_offset: Offset {
                        x_offs: -i32::from(pole.wire_reach),
                        y_offs: -i32::from(pole.wire_reach),
                    },
                    wire_connection_area: Extent {
                        width: u32::from(pole.wire_reach * 2),
                        height: u32::from(pole.wire_reach * 2),
                    },

                    supply_range: pole.range.into(),
                })
            })
            .collect();

        let inserters: Vec<_> = mods
            .iter()
            .flat_map(|mod_| mod_.inserters.iter())
            .cloned()
            .collect();

        let assemblers: Vec<_> = mods
            .iter()
            .flat_map(|mod_| mod_.assemblers.iter())
            .map(|data| crate::AssemblerInfo {
                default_recipe: data.default_recipe.clone().map_or_else(
                    || BTreeMap::from_iter([(None, Recipe(0))]),
                    |mapping| {
                        BTreeMap::from_iter(
                            mapping
                                .into_iter()
                                .map(|(k, v)| (Some(k), recipe_to_id[&v]))
                                .chain([(None, Recipe(0))]),
                        )
                    },
                ),
            })
            .collect();

        Self {
            entities,
            power_poles,
            inserters,
            assemblers,

            items,
            recipes,
        }
    }
}
