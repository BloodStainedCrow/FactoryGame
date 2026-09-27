use std::collections::BTreeMap;

use middle_indices::{ChestMiddleID, InserterMiddleID, TransportLineMiddleID};

use crate::{
    AdditionResult, Backend,
    chests::{ChestBackendID, FullChestIdentifier, FullChestState},
    graph_changes::{NewChestState, NewInserterState, NewTransportLineState},
    power_grid::inserter::{
        FullInserterIdentifier, InserterBackendID, InserterKind, SingleInserterInfo,
    },
    transport_lines::{FullTransportLineIdentifier, SushiTransportLine, TransportLineBackendID},
};

impl Backend {
    #[expect(clippy::unused_self, clippy::needless_pass_by_ref_mut)]
    pub(super) fn add_all_inserters<'a>(
        &mut self,
        inserters: impl IntoIterator<
            Item = (
                FullInserterIdentifier<'a>,
                (SingleInserterInfo, InserterKind),
                NewInserterState,
            ),
        >,
    ) -> BTreeMap<
        FullInserterIdentifier<'a>,
        (
            AdditionResult<InserterMiddleID, InserterBackendID>,
            NewInserterState,
        ),
    > {
        inserters
            .into_iter()
            .map(|(ident, (state, _old_kind), changes)| {
                (
                    ident,
                    (
                        self.add_inserter_internal(
                            ident.grid,
                            &InserterKind::from_ident(FullInserterIdentifier {
                                inferred_items: &changes.inferred_items,

                                ..ident
                            }),
                            state,
                        ),
                        changes,
                    ),
                )
            })
            .collect()
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
