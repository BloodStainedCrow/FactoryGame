use std::collections::btree_map::Entry;

use data::spacial::Position;
use middle::{chest::ChestAdditionInfo, inserter::conn::Conn};

use crate::{
    entity::{EntityDescriptor, EntityDescriptorKind},
    surface::Surface,
};

impl Surface {
    pub fn get_source_conns_or_add_floor_conn(&mut self, position: Position) -> Vec<Conn> {
        match self.get_entity_at(position) {
            Some(entity) => entity.get_source_conn(),
            None => match self.floor_chests.entry(position) {
                Entry::Vacant(vacant_entry) => {
                    let floor_chest_id = self
                        .middle
                        .add_chest(ChestAdditionInfo { num_slots: 1 }, &mut self.backend);

                    vacant_entry.insert(floor_chest_id);

                    vec![Conn::Chest { id: floor_chest_id }]
                },
                Entry::Occupied(occupied_entry) => {
                    vec![Conn::Chest {
                        id: *occupied_entry.get(),
                    }]
                },
            },
        }
    }

    pub fn get_dest_conns_or_add_floor_conn(&mut self, position: Position) -> Option<Conn> {
        match self.get_entity_at(position) {
            Some(entity) => entity.get_dest_conn(),
            None => match self.floor_chests.entry(position) {
                Entry::Vacant(vacant_entry) => {
                    let floor_chest_id = self
                        .middle
                        .add_chest(ChestAdditionInfo { num_slots: 1 }, &mut self.backend);

                    vacant_entry.insert(floor_chest_id);

                    Some(Conn::Chest { id: floor_chest_id })
                },
                Entry::Occupied(occupied_entry) => Some(Conn::Chest {
                    id: *occupied_entry.get(),
                }),
            },
        }
    }
}

impl EntityDescriptor {
    fn get_source_conn(&self) -> Vec<Conn> {
        match self.kind {
            EntityDescriptorKind::Assembler { id } => vec![Conn::Assembler { id }],
            EntityDescriptorKind::Inserter { id } => vec![],
            EntityDescriptorKind::Belt { id } => todo!(),
            EntityDescriptorKind::Pipe { id } => vec![],
            EntityDescriptorKind::PowerPole { id } => vec![],
            EntityDescriptorKind::Chest { id } => vec![Conn::Chest { id }],
            EntityDescriptorKind::SolarPanel {} => vec![],
        }
    }

    fn get_dest_conn(&self) -> Option<Conn> {
        match self.kind {
            EntityDescriptorKind::Assembler { id } => Some(Conn::Assembler { id }),
            EntityDescriptorKind::Inserter { id } => None,
            EntityDescriptorKind::Belt { id } => todo!(),
            EntityDescriptorKind::Pipe { id } => None,
            EntityDescriptorKind::PowerPole { id } => None,
            EntityDescriptorKind::Chest { id } => Some(Conn::Chest { id }),
            EntityDescriptorKind::SolarPanel {} => None,
        }
    }
}
