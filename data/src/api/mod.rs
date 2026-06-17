use std::collections::HashMap;

use crate::{
    DataStore, EntityIdentifier, EntityPrototypeKind, ModIdentifier,
    api::entity::{inserter::InserterInfo, power_pole::PowerPoleInfo},
    entity::{GlobalTy, power_pole::PowerPoleData},
    spacial::{Extent, Offset},
};

pub(crate) mod entity;

pub(crate) struct ModData {
    pub mod_name: ModIdentifier,

    pub inserters: Vec<InserterInfo>,
    pub power_poles: Vec<PowerPoleInfo>,
}

impl DataStore {
    pub fn from_mods(mods: &[ModData]) -> Self {
        let entities: Vec<_> = mods
            .iter()
            .flat_map(|mod_| {
                mod_.power_poles
                    .iter()
                    .map(|pole| (&pole.entity_info, EntityPrototypeKind::PowerPole))
                    .chain(
                        mod_.inserters
                            .iter()
                            .map(|inserter| (&inserter.entity_info, EntityPrototypeKind::Inserter)),
                    )
                    .map(|(entity_info, kind)| crate::EntityInfo {
                        size: entity_info.size,
                        can_be_rotated: entity_info.can_be_rotated,
                        can_be_flipped: entity_info.can_be_flipped,
                        kind,
                        name: EntityIdentifier::new(&mod_.mod_name, &entity_info.name),
                        display_name: entity_info.display_name.clone(),
                        placement_rules: entity_info.placement_rules.clone(),
                    })
            })
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

        let power_poles = mods
            .iter()
            .flat_map(|mod_| {
                mod_.power_poles.iter().map(|pole| PowerPoleData {
                    wire_connection_offset: Offset {
                        x_offs: -i32::from(pole.wire_reach),
                        y_offs: -i32::from(pole.wire_reach),
                    },
                    wire_connection_area: Extent {
                        width: u32::from(pole.wire_reach),
                        height: u32::from(pole.wire_reach),
                    },
                })
            })
            .collect();

        Self {
            entities,
            power_poles,
        }
    }
}
