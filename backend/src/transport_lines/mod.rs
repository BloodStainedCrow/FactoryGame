use data::item::item_set::ItemSet;
use middle_indices::TransportLineMiddleID;
use stable_vec::StableVec;

use crate::{AdditionResult, Backend};

mod pure;
mod sushi;

pub type BeltLenType = u32;

pub(crate) use sushi::SushiTransportLine;

#[derive(Debug, Clone)]
pub(super) struct TransportLineStore {
    sushi: StableVec<SushiTransportLine>,
}

pub struct TransportLineAdditionInfo {
    pub length: BeltLenType,
    pub items: ItemSet,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
pub struct FullTransportLineIdentifier<'a> {
    pub id: TransportLineBackendID,
    pub items: &'a ItemSet,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
pub struct TransportLineBackendID(u32);

impl TransportLineStore {
    pub fn new() -> Self {
        Self {
            sushi: vec![].into(),
        }
    }

    fn add_transport_line(&mut self, info: &TransportLineAdditionInfo) -> TransportLineBackendID {
        self.add_transport_line_internal(SushiTransportLine::new(info.length))
    }

    fn add_transport_line_internal(&mut self, sushi: SushiTransportLine) -> TransportLineBackendID {
        let index = self.sushi.push(sushi);

        TransportLineBackendID(
            index
                .try_into()
                .expect("More than u32::MAX transport lines"),
        )
    }

    fn remove_transport_line(&mut self, ident: FullTransportLineIdentifier) -> SushiTransportLine {
        self.sushi
            .remove(ident.id.0 as usize)
            .expect("Tried to remove non-existant transport line")
    }
}

impl Backend {
    pub fn add_transport_line(
        &mut self,
        info: &TransportLineAdditionInfo,
    ) -> AdditionResult<TransportLineMiddleID, TransportLineBackendID> {
        let id = self.transport_lines.add_transport_line(info);

        AdditionResult::Added {
            new_id: id,
            relocations: vec![],
        }
    }

    pub(crate) fn add_transport_line_internal(
        &mut self,
        state: SushiTransportLine,
    ) -> AdditionResult<TransportLineMiddleID, TransportLineBackendID> {
        let id = self.transport_lines.add_transport_line_internal(state);

        AdditionResult::Added {
            new_id: id,
            relocations: vec![],
        }
    }

    pub(crate) fn remove_transport_line_internal(
        &mut self,
        ident: FullTransportLineIdentifier,
    ) -> SushiTransportLine {
        self.transport_lines.remove_transport_line(ident)
    }
}
