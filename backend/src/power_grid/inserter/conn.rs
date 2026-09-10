use data::item::{Item, item_set::ItemSet};

use crate::{chests::FullChestIdentifier, power_grid::assembler::FullAssemblerIdentifier};

#[derive(Debug)]
pub(super) enum InserterConnection {
    PureChest { item: Item, index: u32 },
    SushiChest { index: u32 },
}

#[derive(Debug, Clone, Copy)]
pub enum BackendInserterConnection<'a> {
    Assembler { ident: FullAssemblerIdentifier },
    Chest { ident: FullChestIdentifier<'a> },
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
            } => todo!(),
            Self::Chest {
                ident: FullChestIdentifier { items, id },
            } => {
                if let Ok(item) = items.is_pure() {
                    InserterConnection::PureChest {
                        item,
                        index: todo!(),
                    }
                } else {
                    InserterConnection::SushiChest { index: id.0 }
                }
            },
        }
    }
}
