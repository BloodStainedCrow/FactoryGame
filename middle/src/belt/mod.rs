use backend::Backend;
use itertools::Either;
use middle_indices::{BeltTileMiddleID, SplitterMiddleID, TransportLineMiddleID};

use crate::{
    Middle,
    belt::{
        splitter::{SplitterEnd, SplitterSide},
        transport_lines::{TransportLineAdditionInfo, TransportLineEnd},
    },
};

mod splitter;
mod transport_lines;

pub(crate) use splitter::SplitterInfo;
pub(crate) use transport_lines::TransportLineInfo;

pub type BeltConnection = Either<BeltTileMiddleID, (SplitterMiddleID, SplitterSide)>;

pub trait GetID: Copy {
    fn get_id(self, middle: &Middle, end: SplitterEnd) -> BeltTileMiddleID;
}

impl GetID for BeltConnection {
    fn get_id(self, middle: &Middle, end: SplitterEnd) -> BeltTileMiddleID {
        match self {
            Self::Left(belt) => belt,
            Self::Right((splitter, side)) => {
                middle.splitter_list[splitter.0 as usize].belts[end][side]
            },
        }
    }
}

#[derive(Debug)]
pub struct BeltTileAdditionInfo {
    pub length: u32,

    pub front_merge: Option<BeltConnection>,
    pub back_merge: Option<BeltConnection>,

    pub left_sideload_source: Option<BeltConnection>,
    pub right_sideload_source: Option<BeltConnection>,
}

#[derive(Debug, Clone)]
pub(crate) struct BeltTileInfo {
    transport_line: TransportLineMiddleID,
}

impl Middle {
    pub fn add_belt_tile(
        &mut self,
        info: BeltTileAdditionInfo,
        backend: &mut Backend,
    ) -> BeltTileMiddleID {
        let BeltTileAdditionInfo {
            length,

            front_merge,
            back_merge,
            left_sideload_source,
            right_sideload_source,
        } = info;

        let front_merge = front_merge.map(|v| v.get_id(self, SplitterEnd::Back));
        let back_merge = back_merge.map(|v| v.get_id(self, SplitterEnd::Front));
        let left_sideload_source = left_sideload_source.map(|v| v.get_id(self, SplitterEnd::Front));
        let right_sideload_source =
            right_sideload_source.map(|v| v.get_id(self, SplitterEnd::Front));

        let next_index = self.belt_tile_list.next_push_index();

        let transport_line = match front_merge {
            Some(front_belt) => {
                let transport_line = self.belt_tile_list[front_belt.0 as usize].transport_line;

                self.extent_transport_line(transport_line, TransportLineEnd::Back, length, backend);

                Some(transport_line)
            },
            None => None,
        };

        let transport_line = match (transport_line, back_merge) {
            (None, None) => None,
            (None, Some(back_belt)) => {
                let transport_line = self.belt_tile_list[back_belt.0 as usize].transport_line;

                self.extent_transport_line(
                    transport_line,
                    TransportLineEnd::Front,
                    length,
                    backend,
                );

                Some(transport_line)
            },
            (Some(transport_line), None) => Some(transport_line),
            (Some(front), Some(back_belt)) => {
                let back = self.belt_tile_list[back_belt.0 as usize].transport_line;

                let merged = self.merge_transport_lines(front, back, backend);

                Some(merged)
            },
        };

        let final_transport_line = match transport_line {
            Some(transport_line) => {
                // We already added us to something
                transport_line
            },
            None => self.add_transport_line(TransportLineAdditionInfo { length }, backend),
        };

        let index = self.belt_tile_list.push(BeltTileInfo {
            transport_line: final_transport_line,
        });

        assert_eq!(next_index, index);

        BeltTileMiddleID(index.try_into().expect("More than u32::MAX belt tiles"))
    }

    pub fn remove_belt_tile(&mut self, id: BeltTileMiddleID, backend: &mut Backend) {
        todo!()
    }
}
