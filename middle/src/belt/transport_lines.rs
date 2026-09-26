use backend::{
    Backend,
    transport_lines::{BeltLenType, TransportLineBackendID},
};
use data::item::item_set::ItemSet;
use middle_indices::{BeltTileMiddleID, InserterMiddleID, TransportLineMiddleID};

use crate::Middle;

#[derive(Debug, Clone)]
pub struct TransportLineInfo {
    pub length: BeltLenType,
    pub backend_id: TransportLineBackendID,
    pub inferred_items: ItemSet,

    pub connected_tiles: Vec<BeltTileMiddleID>,
    pub connected_inserters: Vec<(BeltTileMiddleID, InserterMiddleID)>,
}

#[derive(Debug)]
pub(super) struct TransportLineAdditionInfo {
    pub tiles: Vec<BeltTileMiddleID>,
    pub length: u32,
}

#[derive(Debug, Clone, Copy)]
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

        let next_index = self.transport_line_list.next_push_index();

        let result =
            backend.add_transport_line(&backend::transport_lines::TransportLineAdditionInfo {
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
            backend::AdditionResult::Failed { info: _ } => todo!(),
        };

        let index = self.transport_line_list.push(TransportLineInfo {
            length,
            connected_tiles: tiles,
            backend_id,
            inferred_items: ItemSet::empty(),
            // TODO
            connected_inserters: vec![],
        });

        // dbg!(index);
        // let count = dbg!(self.transport_line_list.iter().count());
        // dbg!(
        //     self.transport_line_list
        //         .values()
        //         .map(|v| v.length)
        //         .sum::<u32>()
        // );
        // dbg!(
        //     self.transport_line_list
        //         .values()
        //         .map(|v| v.length as f64)
        //         .sum::<f64>()
        //         / count as f64
        // );

        assert_eq!(next_index, index);

        TransportLineMiddleID(index.try_into().expect("More than u32::MAX belts"))
    }

    pub(super) fn get_transport_line_length(&self, id: TransportLineMiddleID) -> BeltLenType {
        self.transport_line_list[id.0 as usize].length
    }

    #[expect(clippy::needless_pass_by_ref_mut)]
    pub(super) fn extend_transport_line(
        &mut self,
        id: TransportLineMiddleID,
        end: TransportLineEnd,
        amount: BeltLenType,
        new_tiles: impl IntoIterator<Item = BeltTileMiddleID>,
        _backend: &mut Backend,
    ) {
        let line = &mut self.transport_line_list[id.0 as usize];

        line.length += amount;

        match end {
            TransportLineEnd::Front => {
                for tile in &line.connected_tiles {
                    self.belt_tile_list[tile.0 as usize].belt_pos += amount;
                }

                for inserter in &line.connected_inserters {
                    todo!("Move inserter position")
                }
            },
            TransportLineEnd::Back => {
                // No need to update belt pos of anything
            },
        }

        line.connected_tiles.extend(new_tiles);

        // FIXME: Apply backend changes
    }

    #[expect(clippy::needless_pass_by_ref_mut)]
    pub(super) fn merge_transport_lines(
        &mut self,
        front: TransportLineMiddleID,
        back: TransportLineMiddleID,
        new_tiles: impl IntoIterator<Item = BeltTileMiddleID>,
        _backend: &mut Backend,
    ) -> TransportLineMiddleID {
        if front == back {
            todo!("Make circular")
        }

        let front_len = self.get_transport_line_length(front);

        let TransportLineInfo {
            length: _,
            backend_id,
            inferred_items,
            connected_tiles,
            connected_inserters,
        } = self
            .transport_line_list
            .remove(back.0 as usize)
            .expect("Tried to merge non existant transport line");

        for tile in &connected_tiles {
            self.belt_tile_list[tile.0 as usize].transport_line = front;
            self.belt_tile_list[tile.0 as usize].belt_pos += front_len;
        }

        for inserter in &connected_inserters {
            todo!("Move inserter position and change id")
        }

        self.transport_line_list[front.0 as usize]
            .connected_tiles
            .extend(new_tiles);

        self.transport_line_list[front.0 as usize]
            .connected_tiles
            .extend(connected_tiles);

        self.transport_line_list[front.0 as usize]
            .connected_inserters
            .extend(connected_inserters);

        if self.transport_line_list[front.0 as usize].inferred_items != inferred_items {
            todo!("Graph updates")
        }

        // FIXME: Backend merge

        front
    }

    #[expect(clippy::needless_pass_by_ref_mut)]
    pub(super) fn remove_transport_line(
        &mut self,
        id: TransportLineMiddleID,
        backend: &mut Backend,
    ) -> TransportLineMiddleID {
        let middle = self
            .transport_line_list
            .remove(id.0 as usize)
            .expect("Tried to remove non existant transport line");

        todo!()
    }
}
