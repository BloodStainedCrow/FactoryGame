use data::item::item_set::ItemSet;
use stable_vec::StableVec;

use crate::{Backend, transport_lines::sushi::SushiTransportLine};

mod pure;
mod sushi;

pub type BeltLenType = u32;

#[derive(Debug, Clone)]
pub(super) struct TransportLineStore {
    sushi: StableVec<SushiTransportLine>,
}

pub struct TransportLineAdditionInfo {
    pub length: BeltLenType,
    pub items: ItemSet,
}

#[derive(Debug, Clone, Copy)]
pub struct FullTransportLineIdentifier<'a> {
    pub id: TransportLineBackendID,
    pub items: &'a ItemSet,
}

#[derive(Debug, Clone, Copy)]
pub struct TransportLineBackendID(u32);

impl TransportLineStore {
    pub fn new() -> Self {
        Self {
            sushi: vec![].into(),
        }
    }

    pub fn add_transport_line(
        &mut self,
        info: TransportLineAdditionInfo,
    ) -> TransportLineBackendID {
        let TransportLineAdditionInfo { length, items } = info;

        let index = self.sushi.push(SushiTransportLine::new(length));

        TransportLineBackendID(
            index
                .try_into()
                .expect("More than u32::MAX transport lines"),
        )
    }
}

impl Backend {
    pub fn add_transport_line(
        &mut self,
        info: TransportLineAdditionInfo,
    ) -> TransportLineBackendID {
        self.transport_lines.add_transport_line(info)
    }
}
