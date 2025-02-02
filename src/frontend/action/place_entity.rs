use crate::frontend::world::tile::{PlaceEntityType, World};

#[derive(Debug, serde::Serialize, serde::Deserialize)]
pub struct PlaceEntityInfo {
    pub entities: EntityPlaceOptions,
}

#[derive(Debug, serde::Serialize, serde::Deserialize)]
pub enum EntityPlaceOptions {
    Single(PlaceEntityType),
    Multiple(Vec<PlaceEntityType>),
}

impl PlaceEntityInfo {
    pub(super) fn still_valid(&self, world: &World) -> bool {
        // let check_single = |ty: &EntityType| {
        //     match ty {
        //     EntityType::Assembler(position) => todo!("Ensure the assembler fits there!"),
        //     EntityType::Inserter {
        //         start_pos,
        //         end_pos,
        //         filter,
        //     } => todo!("Ensure that the inserter fits, the input and outputs are valid and that the filter is sensical!"),
        // }
        // };

        // match &self.entities {
        //     EntityPlaceOptions::Single(entity_type) => check_single(entity_type),
        //     EntityPlaceOptions::Multiple(vec) => {
        //         for ent in vec {
        //             if !check_single(ent) {
        //                 // TODO: Do I want to discard the whole action or just the parts that still work?
        //                 return false;
        //             }
        //         }

        //         true
        //     },
        // }

        todo!()
    }
}
