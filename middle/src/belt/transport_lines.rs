use backend::Backend;
use data::item::item_set::ItemSet;
use middle_indices::TransportLineMiddleID;

use crate::Middle;

#[derive(Debug, Clone)]
pub struct TransportLineInfo {
    backend_id: !,
    inferred_items: ItemSet,
}

#[derive(Debug)]
pub(super) struct TransportLineAdditionInfo {
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
        let TransportLineAdditionInfo { length } = info;

        let next_index = self.belt_list.next_push_index();

        let index = self.belt_list.push(TransportLineInfo {
            backend_id: todo!(),
            inferred_items: ItemSet::empty(),
        });

        assert_eq!(next_index, index);

        TransportLineMiddleID(index.try_into().expect("More than u32::MAX belts"))
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
