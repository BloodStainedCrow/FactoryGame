use data::item::{Item, item_set::ItemSet};
use itertools::Itertools;
use middle_indices::InserterMiddleID;

use crate::{
    AdditionResult, Backend,
    inserter::pure::InserterRenderState,
    power_grid::{
        PowerGridBackendID,
        inserter::conn::{BackendInserterConnection, InserterConnection},
    },
};

pub mod conn;
pub(crate) mod store;

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct InserterBackendID(pub(crate) u32);

#[derive(Debug)]
pub struct InserterAdditionInfo<'a> {
    pub power_grid: PowerGridBackendID,
    pub middle_id: InserterMiddleID,

    pub source: Vec<BackendInserterConnection<'a>>,
    pub dest: BackendInserterConnection<'a>,
    pub items: ItemSet,
    // TODO: Stats
    pub movetime: u16,
}

#[derive(Debug, Clone, Copy)]
pub struct FullInserterIdentifier<'a> {
    pub grid: PowerGridBackendID,
    pub inserter_id: InserterBackendID,
    pub inferred_items: &'a ItemSet,
    pub source: &'a [BackendInserterConnection<'a>],
    pub dest: BackendInserterConnection<'a>,
    pub movetime: u16,
}

#[derive(Debug, Clone)]
pub(crate) struct SingleInserterInfo {
    pub(crate) middle_id: InserterMiddleID,
    pub(crate) movetime: u16,
}

impl Backend {
    pub fn add_inserter(
        &mut self,
        info: InserterAdditionInfo,
    ) -> AdditionResult<InserterMiddleID, InserterBackendID> {
        self.add_inserter_internal(
            info.power_grid,
            InserterKind::from_addition(&info),
            SingleInserterInfo {
                middle_id: info.middle_id,
                movetime: info.movetime,
            },
        )
    }

    fn add_inserter_internal(
        &mut self,
        grid: PowerGridBackendID,
        kind: InserterKind,
        data: SingleInserterInfo,
    ) -> AdditionResult<InserterMiddleID, InserterBackendID> {
        let power_grid = &mut self.power_grids[grid.0 as usize];

        let new_id = power_grid.inserters.add_inserter(kind, data);

        AdditionResult::Added {
            new_id,
            relocations: vec![],
        }
    }

    /// NOTE: This inserter needs to already not have any inserter connections
    pub fn remove_inserter(&mut self, inserter: FullInserterIdentifier) {
        todo!()
    }

    fn remove_inserter_internal(
        &mut self,
        inserter: FullInserterIdentifier,
    ) -> (SingleInserterInfo, InserterKind) {
        let grid = &mut self.power_grids[inserter.grid.0 as usize];

        let kind = InserterKind::from_ident(inserter);
        let state = grid.inserters.remove_inserter(inserter.inserter_id, &kind);

        (state, kind)
    }

    pub fn move_inserter(
        &mut self,
        inserter: FullInserterIdentifier,
        new_grid: PowerGridBackendID,
    ) -> AdditionResult<InserterMiddleID, InserterBackendID> {
        let (state, kind) = self.remove_inserter_internal(inserter);

        self.add_inserter_internal(new_grid, kind, state)
    }

    pub fn get_inserter_state(&self, inserter: FullInserterIdentifier) -> InserterRenderState {
        let grid = &self.power_grids[inserter.grid.0 as usize];

        grid.inserters
            .get_inserter_state(inserter.inserter_id, InserterKind::from_ident(inserter))
    }
}

// TODO: Move this
pub enum InserterKind {
    EmptyInserter {},

    OneToOneSingleItem {
        item: Item,
        source: u32,
        dest: u32,
        movetime: u16,
    },
}

impl InserterKind {
    pub fn from_ident(ident: FullInserterIdentifier) -> Self {
        Self::from_data((
            ident.movetime,
            &ident.inferred_items,
            ident.source,
            ident.dest,
        ))
    }

    fn from_addition(info: &InserterAdditionInfo) -> Self {
        Self::from_data((info.movetime, &info.items, &info.source, info.dest))
    }

    fn from_data(
        (movetime, inferred_items, source, dest): (
            u16,
            &ItemSet,
            &[BackendInserterConnection],
            BackendInserterConnection,
        ),
    ) -> Self {
        match inferred_items.is_pure() {
            Ok(item) => match (
                BackendInserterConnection::get_list_entries(source, inferred_items)
                    .collect_vec()
                    .as_slice(),
                dest.get_list_entry(inferred_items),
            ) {
                ([], _) => unreachable!(),
                ([source_conn], dest_conn) => match (source_conn, dest_conn) {
                    (
                        InserterConnection::PureChest {
                            item: source_item,
                            index: source_index,
                        },
                        InserterConnection::PureChest {
                            item: dest_item,
                            index: dest_index,
                        },
                    ) => {
                        assert_eq!(*source_item, item);
                        assert_eq!(dest_item, item);

                        Self::OneToOneSingleItem {
                            item,
                            source: *source_index,
                            dest: dest_index,
                            movetime,
                        }
                    },
                    (
                        InserterConnection::PureChest { .. },
                        InserterConnection::SushiChest { .. },
                    ) => todo!(),
                    (
                        InserterConnection::SushiChest { .. },
                        InserterConnection::PureChest { .. },
                    ) => todo!(),
                    (
                        InserterConnection::SushiChest { .. },
                        InserterConnection::SushiChest { .. },
                    ) => todo!(),
                },

                _ => todo!(),
            },
            Err(None) => InserterKind::EmptyInserter {},
            Err(_) => todo!("Sushi"),
        }
    }
}
