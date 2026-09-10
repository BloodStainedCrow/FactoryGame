use std::collections::BTreeMap;

use data::item::item_set::ItemSet;

use crate::Backend;

impl Backend {
    pub fn apply_graph_changes(
        &mut self,
        container_changes: BTreeMap<!, ItemSet>,
        edge_changes: BTreeMap<!, ItemSet>,
    ) -> ! {
        todo!()
    }
}
