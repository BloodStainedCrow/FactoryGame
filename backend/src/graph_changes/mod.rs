use std::collections::BTreeMap;

use data::item::item_set::ItemSet;
use middle_indices::{ChestMiddleID, InserterMiddleID, TransportLineMiddleID};

use crate::{
    AdditionResult, Backend,
    chests::{ChestBackendID, FullChestIdentifier},
    power_grid::inserter::{FullInserterIdentifier, InserterBackendID},
    transport_lines::{FullTransportLineIdentifier, TransportLineBackendID},
};

mod readdition;
mod removal;

// TODO: This will need to include everything that has references to the changed stuff too.
// I.e. Inserters that do not change but need to have the references to the stuff in here updated
#[derive(Debug)]
pub struct GraphChanges<'a> {
    pub inserter_changes: BTreeMap<FullInserterIdentifier<'a>, NewInserterState>,
    pub chest_changes: BTreeMap<FullChestIdentifier<'a>, NewChestState>,
    pub transport_line_changes: BTreeMap<FullTransportLineIdentifier<'a>, NewTransportLineState>,
}

#[derive(Debug, Clone)]
pub struct NewInserterState {
    pub middle_id: InserterMiddleID,
    pub inferred_items: ItemSet,
}

#[derive(Debug, Clone)]
pub struct NewChestState {
    pub middle_id: ChestMiddleID,
    pub inferred_items: ItemSet,
}

#[derive(Debug, Clone)]
pub struct NewTransportLineState {
    pub middle_id: TransportLineMiddleID,
    pub inferred_items: ItemSet,
}

pub struct GraphChangesResult {
    pub inserter_updates: Vec<(
        AdditionResult<InserterMiddleID, InserterBackendID>,
        NewInserterState,
    )>,
    pub chest_updates: Vec<(AdditionResult<ChestMiddleID, ChestBackendID>, NewChestState)>,
    pub transport_lines_updates: Vec<(
        AdditionResult<TransportLineMiddleID, TransportLineBackendID>,
        NewTransportLineState,
    )>,
}

impl Backend {
    #[must_use]
    pub fn apply_graph_changes(&mut self, changes: GraphChanges<'_>) -> GraphChangesResult {
        let _inserters = self.remove_all_inserters(changes.inserter_changes);
        let chests = self.remove_all_chests(changes.chest_changes);
        let transport_lines = self.remove_all_transport_lines(changes.transport_line_changes);

        let chest_updates = self.add_all_chests(
            chests
                .into_iter()
                .map(|(ident, (state, changes))| (ident, state, changes)),
        );
        let transport_lines_updates = self.add_all_transport_lines(
            transport_lines
                .into_iter()
                .map(|(ident, (state, changes))| (ident, state, changes)),
        );

        // TODO: Inserter readdition. That required a ton of logic compared to the rest

        GraphChangesResult {
            inserter_updates: Vec::new(),
            chest_updates: chest_updates.into_values().collect(),
            transport_lines_updates: transport_lines_updates.into_values().collect(),
        }
    }
}
