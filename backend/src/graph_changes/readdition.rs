use std::collections::BTreeMap;

use middle_indices::{ChestMiddleID, TransportLineMiddleID};

use crate::{
    AdditionResult, Backend,
    chests::{ChestBackendID, FullChestIdentifier, FullChestState},
    graph_changes::{NewChestState, NewInserterState, NewTransportLineState},
    power_grid::inserter::{FullInserterIdentifier, InserterKind, SingleInserterInfo},
    transport_lines::{FullTransportLineIdentifier, SushiTransportLine, TransportLineBackendID},
};

impl Backend {
    pub(super) fn add_all_inserters<'a>(
        &mut self,
        inserters: impl IntoIterator<
            Item = (
                FullInserterIdentifier<'a>,
                (SingleInserterInfo, InserterKind),
                NewInserterState,
            ),
        >,
    ) -> BTreeMap<FullInserterIdentifier<'a>, (SingleInserterInfo, InserterKind)> {
        // inserters
        //     .into_iter()
        //     .map(|(ident, state, changes)| {
        //         (
        //             ident,
        //             self.add_inserter_internal(ident.grid, todo!(), state),
        //         )
        //     })
        //     .collect()

        // FIXME:
        BTreeMap::new()
    }

    pub(super) fn add_all_chests<'a>(
        &mut self,
        chests: impl IntoIterator<Item = (FullChestIdentifier<'a>, FullChestState, NewChestState)>,
    ) -> BTreeMap<
        FullChestIdentifier<'a>,
        (AdditionResult<ChestMiddleID, ChestBackendID>, NewChestState),
    > {
        chests
            .into_iter()
            .map(|(ident, state, changes)| {
                (
                    ident,
                    (
                        self.add_chest_internal(&changes.inferred_items, state),
                        changes,
                    ),
                )
            })
            .collect()
    }

    pub(super) fn add_all_transport_lines<'a>(
        &mut self,
        transport_lines: impl IntoIterator<
            Item = (
                FullTransportLineIdentifier<'a>,
                SushiTransportLine,
                NewTransportLineState,
            ),
        >,
    ) -> BTreeMap<
        FullTransportLineIdentifier<'a>,
        (
            AdditionResult<TransportLineMiddleID, TransportLineBackendID>,
            NewTransportLineState,
        ),
    > {
        transport_lines
            .into_iter()
            .map(|(ident, state, changes)| {
                (ident, (self.add_transport_line_internal(state), changes))
            })
            .collect()
    }
}
