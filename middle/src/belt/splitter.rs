use backend::Backend;
use enum_map::{Enum, EnumMap};
use middle_indices::{BeltTileMiddleID, SplitterMiddleID};

use crate::Middle;

#[derive(Debug, Clone, Copy, Enum)]
pub enum SplitterSide {
    Left,
    Right,
}

#[derive(Debug, Clone, Copy, Enum)]
pub(super) enum SplitterEnd {
    Front,
    Back,
}

#[derive(Debug, Clone)]
pub struct SplitterInfo {
    pub(super) belts: EnumMap<SplitterEnd, EnumMap<SplitterSide, BeltTileMiddleID>>,
}

pub struct SplitterAdditionInfo {
    merges: EnumMap<SplitterEnd, EnumMap<SplitterSide, Option<BeltTileMiddleID>>>,
}

impl Middle {
    pub fn add_splitter(&mut self, _info: !, _backend: &mut Backend) -> SplitterMiddleID {
        todo!()
    }
}
