use std::collections::btree_map::Entry;

use data::{
    entity::inserter::{get_input_position, get_output_position, inserter_search_range},
    spacial::{BoundingBox, Position},
};
use entity_info::{EntityDescriptor, EntityDescriptorKind};
use middle::{chest::ChestAdditionInfo, inserter::conn::Conn};
use middle_indices::InserterMiddleID;

use crate::surface::{Surface, world::SurfaceWorld};

impl Surface {
    pub fn get_source_conns_or_add_floor_conn(&mut self, position: Position) -> Vec<Conn> {
        match self.get_entity_at(position) {
            Some(entity) => entity.get_source_conn(),
            None => match self.floor_chests.entry(position) {
                Entry::Vacant(vacant_entry) => {
                    let floor_chest_id = self
                        .middle
                        .add_chest(&ChestAdditionInfo { num_slots: 1 }, &mut self.backend);

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
                        .add_chest(&ChestAdditionInfo { num_slots: 1 }, &mut self.backend);

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

impl SurfaceWorld {
    pub fn get_inserters_connected_to(
        &self,
        bb: BoundingBox,
    ) -> impl Iterator<Item = (InserterMiddleID, bool, bool)> {
        self.get_entities_in_area(bb.extend_evenly(inserter_search_range()))
            .filter_map(move |entity| match entity.kind {
                EntityDescriptorKind::Inserter { id } => {
                    let source = get_input_position(
                        entity.ty.try_into().expect("Inserter with non InserterTy"),
                        entity.position,
                        entity.rotation,
                        entity.flipped,
                    );
                    let dest = get_output_position(
                        entity.ty.try_into().expect("Inserter with non InserterTy"),
                        entity.position,
                        entity.rotation,
                        entity.flipped,
                    );

                    Some((id, bb.contains(source), bb.contains(dest)))
                },

                _ => None,
            })
    }
}

trait ConnTrait {
    fn get_source_conn(&self) -> Vec<Conn>;
    fn get_dest_conn(&self) -> Option<Conn>;
}

impl ConnTrait for EntityDescriptor {
    fn get_source_conn(&self) -> Vec<Conn> {
        match self.kind {
            EntityDescriptorKind::Assembler { id } => vec![Conn::Assembler { id }],
            EntityDescriptorKind::Inserter { .. } => vec![],
            EntityDescriptorKind::Belt { id } => vec![Conn::BeltTile { id }],
            EntityDescriptorKind::Pipe { .. } => vec![],
            EntityDescriptorKind::PowerPole { .. } => vec![],
            EntityDescriptorKind::Chest { id } => vec![Conn::Chest { id }],
            EntityDescriptorKind::SolarPanel {} => vec![],
        }
    }

    fn get_dest_conn(&self) -> Option<Conn> {
        match self.kind {
            EntityDescriptorKind::Assembler { id } => Some(Conn::Assembler { id }),
            EntityDescriptorKind::Inserter { .. } => None,
            EntityDescriptorKind::Belt { id } => Some(Conn::BeltTile { id }),
            EntityDescriptorKind::Pipe { .. } => None,
            EntityDescriptorKind::PowerPole { .. } => None,
            EntityDescriptorKind::Chest { id } => Some(Conn::Chest { id }),
            EntityDescriptorKind::SolarPanel {} => None,
        }
    }
}
