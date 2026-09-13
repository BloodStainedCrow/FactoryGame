use data::item::{Item, item_set::ItemSet};

use crate::{
    chests::FullChestIdentifier,
    power_grid::assembler::FullAssemblerIdentifier,
    transport_lines::{BeltLenType, FullTransportLineIdentifier, TransportLineBackendID},
};

#[derive(Debug)]
pub(super) enum InserterConnection {
    PureChest {
        item: Item,
        index: u32,
    },
    SushiChest {
        index: u32,
    },
    SushiBelt {
        id: TransportLineBackendID,
        pos: BeltLenType,
    },
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
pub enum BackendInserterConnection<'a> {
    Assembler {
        ident: FullAssemblerIdentifier,
    },
    Chest {
        ident: FullChestIdentifier<'a>,
    },
    TransportLine {
        ident: FullTransportLineIdentifier<'a>,
        belt_pos: BeltLenType,
    },
}

impl BackendInserterConnection<'_> {
    pub fn get_list_entries(
        conns: &[Self],
        inserter_items: &ItemSet,
    ) -> impl Iterator<Item = InserterConnection> {
        conns.iter().map(|conn| conn.get_list_entry(inserter_items))
    }

    pub fn get_list_entry(&self, inserter_items: &ItemSet) -> InserterConnection {
        match self {
            Self::Assembler {
                ident:
                    FullAssemblerIdentifier {
                        recipe,
                        grid,
                        assembler_id,
                    },
            } => match inserter_items.is_pure() {
                // TODO: Get the assembler slot index
                Ok(pure_item) => InserterConnection::PureChest {
                    item: pure_item,
                    index: u32::MAX,
                },
                Err(None) => unreachable!(),
                Err(Some(_)) => todo!("Multiple items"),
            },
            Self::Chest {
                ident: FullChestIdentifier { items, id },
            } => {
                if let Ok(item) = items.is_pure() {
                    InserterConnection::PureChest { item, index: id.0 }
                } else {
                    InserterConnection::SushiChest { index: id.0 }
                }
            },
            Self::TransportLine {
                ident: FullTransportLineIdentifier { id, items },
                belt_pos,
            } => InserterConnection::SushiBelt {
                id: *id,
                pos: *belt_pos,
            },
        }
    }
}
