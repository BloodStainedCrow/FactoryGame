use std::collections::BTreeMap;

use crate::{
    Backend,
    chests::{FullChestIdentifier, FullChestState},
    graph_changes::{NewChestState, NewInserterState, NewTransportLineState},
    power_grid::inserter::{FullInserterIdentifier, InserterKind, SingleInserterInfo},
    transport_lines::{FullTransportLineIdentifier, SushiTransportLine},
};

impl Backend {
    pub(super) fn remove_all_inserters<'a>(
        &mut self,
        inserters: impl IntoIterator<Item = (FullInserterIdentifier<'a>, NewInserterState)>,
    ) -> BTreeMap<FullInserterIdentifier<'a>, ((SingleInserterInfo, InserterKind), NewInserterState)>
    {
        inserters
            .into_iter()
            .map(|(ident, state)| (ident, (self.remove_inserter_internal(ident), state)))
            .collect()
    }

    pub(super) fn remove_all_chests<'a>(
        &mut self,
        chests: impl IntoIterator<Item = (FullChestIdentifier<'a>, NewChestState)>,
    ) -> BTreeMap<FullChestIdentifier<'a>, (FullChestState, NewChestState)> {
        chests
            .into_iter()
            .map(|(ident, state)| (ident, (self.remove_chest_internal(ident), state)))
            .collect()
    }

    pub(super) fn remove_all_transport_lines<'a>(
        &mut self,
        transport_lines: impl IntoIterator<
            Item = (FullTransportLineIdentifier<'a>, NewTransportLineState),
        >,
    ) -> BTreeMap<FullTransportLineIdentifier<'a>, (SushiTransportLine, NewTransportLineState)>
    {
        transport_lines
            .into_iter()
            .map(|(ident, state)| (ident, (self.remove_transport_line_internal(ident), state)))
            .collect()
    }
}
