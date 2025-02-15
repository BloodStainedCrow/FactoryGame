use crate::{
    frontend::world::tile::{PlaceEntityType, World},
    item::IdxTrait,
};

#[derive(Debug, serde::Serialize, serde::Deserialize)]
pub struct PlaceEntityInfo<ItemIdxType: IdxTrait> {
    pub entities: EntityPlaceOptions<ItemIdxType>,
}

#[derive(Debug, serde::Serialize, serde::Deserialize)]
pub enum EntityPlaceOptions<ItemIdxType: IdxTrait> {
    Single(PlaceEntityType<ItemIdxType>),
    Multiple(Vec<PlaceEntityType<ItemIdxType>>),
}

// impl<ItemIdxType: IdxTrait> PlaceEntityInfo<ItemIdxType> {
//     pub(super) fn still_valid(&self, world: &World) -> bool {
//         // let check_single = |ty: &EntityType| {
//         //     match ty {
//         //     EntityType::Assembler(position) => todo!("Ensure the assembler fits there!"),
//         //     EntityType::Inserter {
//         //         start_pos,
//         //         end_pos,
//         //         filter,
//         //     } => todo!("Ensure that the inserter fits, the input and outputs are valid and that the filter is sensical!"),
//         // }
//         // };

//         // match &self.entities {
//         //     EntityPlaceOptions::Single(entity_type) => check_single(entity_type),
//         //     EntityPlaceOptions::Multiple(vec) => {
//         //         for ent in vec {
//         //             if !check_single(ent) {
//         //                 // TODO: Do I want to discard the whole action or just the parts that still work?
//         //                 return false;
//         //             }
//         //         }

//         //         true
//         //     },
//         // }

//         todo!()
//     }
// }
