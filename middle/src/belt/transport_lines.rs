use backend::{
    Backend,
    transport_lines::{BeltLenType, TransportLineBackendID},
};
use data::item::item_set::ItemSet;
use middle_indices::{BeltTileMiddleID, TransportLineMiddleID};

use crate::Middle;

#[derive(Debug, Clone)]
pub(crate) struct TransportLineInfo {
    pub length: BeltLenType,
    pub backend_id: TransportLineBackendID,
    pub inferred_items: ItemSet,

    pub connected_tiles: Vec<BeltTileMiddleID>,
}

#[derive(Debug)]
pub(super) struct TransportLineAdditionInfo {
    pub tiles: Vec<BeltTileMiddleID>,
    pub length: u32,
}

#[derive(Debug)]
pub(super) enum TransportLineEnd {
    Front,
    Back,
}

impl Middle {
    pub(super) fn add_transport_line(
        &mut self,
        info: TransportLineAdditionInfo,
        backend: &mut Backend,
    ) -> TransportLineMiddleID {
        let TransportLineAdditionInfo { tiles, length } = info;

        let next_index = self.belt_list.next_push_index();

        let result =
            backend.add_transport_line(backend::transport_lines::TransportLineAdditionInfo {
                length,
                items: ItemSet::empty(),
            });

        let backend_id = match result {
            backend::AdditionResult::Added {
                new_id,
                relocations,
            } => {
                if !relocations.is_empty() {
                    todo!("Handle relocations");
                }
                new_id
            },
            backend::AdditionResult::Failed { info } => todo!(),
        };

        let index = self.belt_list.push(TransportLineInfo {
            length,
            connected_tiles: tiles,
            backend_id,
            inferred_items: ItemSet::empty(),
        });

        assert_eq!(next_index, index);

        TransportLineMiddleID(index.try_into().expect("More than u32::MAX belts"))
    }

    pub(super) fn get_transport_line_length(&self, id: TransportLineMiddleID) -> BeltLenType {
        self.belt_list[id.0 as usize].length
    }

    pub(super) fn extent_transport_line(
        &mut self,
        belt: TransportLineMiddleID,
        end: TransportLineEnd,
        amount: u32,
        backend: &mut Backend,
    ) {
        let belt = &mut self.belt_list[belt.0 as usize];

        todo!()
    }

    pub(super) fn merge_transport_lines(
        &mut self,
        front: TransportLineMiddleID,
        back: TransportLineMiddleID,
        backend: &mut Backend,
    ) -> TransportLineMiddleID {
        todo!()
    }

    pub(super) fn remove_transport_line(
        &mut self,
        id: TransportLineMiddleID,
        backend: &mut Backend,
    ) -> TransportLineMiddleID {
        todo!()
    }
}
