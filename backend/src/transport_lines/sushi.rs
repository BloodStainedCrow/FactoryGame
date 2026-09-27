use std::collections::VecDeque;

use data::item::Item;

use crate::transport_lines::BeltLenType;

#[derive(Debug, Clone, serde::Deserialize, serde::Serialize)]
pub struct SushiTransportLine {
    is_circular: bool,
    locs: VecDeque<Option<Item>>,
}

impl SushiTransportLine {
    pub(super) fn new(length: BeltLenType) -> Self {
        Self {
            is_circular: false,
            locs: vec![None; length as usize].into(),
        }
    }

    pub(super) fn make_circular(&mut self) {
        self.is_circular = true;
    }
}
