use std::iter;

use backend::{
    Backend,
    power_grid::inserter::{FullInserterIdentifier, InserterBackendID},
};
use data::item::item_set::{ItemSet, LimitedItemSet};
use itertools::Itertools;
use middle_indices::{InserterMiddleID, PowerGridMiddleID};

pub mod conn;

use crate::{Middle, assembler::InserterTransfer, inserter::conn::Conn};

pub const MAX_CONN_COUNT: usize = 2;
static_assertions::const_assert!(std::mem::size_of::<Option<Conn>>() <= 8);

#[derive(Debug, Clone)]
pub(crate) struct InserterInfo {
    pub(crate) backend_id: InserterBackendID,

    // NOTE: Each inserter belonges to a power grid. There will be a special power grid that holds all the actually unconnected entities
    pub(crate) power_grid_id: PowerGridMiddleID,

    pub(crate) sources: [Option<Conn>; MAX_CONN_COUNT],
    pub(crate) dest: Option<Conn>,
    pub(crate) inferred_items: ItemSet,
    pub(crate) movetime: u16,

    pub(crate) user_filter: LimitedItemSet,
}

#[derive(Debug)]
pub struct InserterAdditionInfo {
    pub power_grid_id: PowerGridMiddleID,

    pub sources: Vec<Conn>,
    pub dest: Option<Conn>,
    pub item_filter: LimitedItemSet,
    pub movetime: u16,
}

impl Middle {
    pub fn add_inserter(
        &mut self,
        info: InserterAdditionInfo,
        backend: &mut Backend,
    ) -> InserterMiddleID {
        let next_index = self
            .inserter_list
            .next_push_index()
            .try_into()
            .expect("More than u32::MAX inserters");

        assert!(
            info.sources.len() <= 1,
            "Multi source inserter not supported yet"
        );

        let source_items = info
            .sources
            .iter()
            .map(|source| self.get_items_takeable_from(*source))
            .reduce(|mut a, b| {
                a.union(&b);
                a
            })
            .unwrap_or(ItemSet::empty());

        let dest_items = info.dest.map(|dest| self.get_items_placeable_into(dest));

        let mut items = source_items;
        items.intersection(&info.item_filter);
        if let Some(dest_items) = &dest_items {
            items.intersection(dest_items);
        }

        if let Some(dest) = &info.dest {
            for &source in &info.sources {
                self.apply_effect_of_new_edge(source, *dest, &items, backend);
            }
        }

        let backend_id =
            match backend.add_inserter(&backend::power_grid::inserter::InserterAdditionInfo {
                power_grid: self.power_grid_list[info.power_grid_id.0 as usize].backend_id,
                middle_id: InserterMiddleID(next_index),
                source: info
                    .sources
                    .iter()
                    .map(|conn| self.get_backend_conn(*conn))
                    .collect(),
                dest: info.dest.map(|dest| self.get_backend_conn(dest)),
                movetime: info.movetime,
                items: items.clone(),
            }) {
                backend::AdditionResult::Added {
                    new_id,
                    relocations,
                } => {
                    if !relocations.is_empty() {
                        todo!("Handle relocations");
                    }
                    new_id
                },
                backend::AdditionResult::Failed { info: _ } => todo!(),
            };

        let index = self.inserter_list.push(InserterInfo {
            power_grid_id: info.power_grid_id,
            backend_id,

            sources: info
                .sources
                .iter()
                .copied()
                .map(Option::Some)
                .chain(iter::repeat(None))
                .take(MAX_CONN_COUNT)
                .collect_array()
                .expect("Take ensures len"),
            dest: info.dest,
            inferred_items: items,

            movetime: info.movetime,

            user_filter: info.item_filter,
        });

        assert_eq!(next_index, index as u32);

        InserterMiddleID(next_index)
    }

    pub fn remove_inserter(&mut self, id: InserterMiddleID, backend: &mut Backend) {
        let info = self
            .inserter_list
            .remove(id.0 as usize)
            .expect("Tried to remove non-existant inserter");

        let grid = self.power_grid_list[info.power_grid_id.0 as usize].backend_id;

        let sources = info
            .sources
            .map(|slot| slot.map(|conn| self.get_backend_conn(conn)));

        backend.remove_inserter(FullInserterIdentifier {
            grid,
            inserter_id: info.backend_id,
            inferred_items: &info.inferred_items,
            source: sources,
            dest: info.dest.map(|dest| self.get_backend_conn(dest)),
            movetime: info.movetime,
        });

        // TODO: We currently do not apply the effect of this removed edge. This will overestimate the amount of items everywhere which is safe
        //       Just bad for performance
    }

    pub(crate) fn handle_inserter_transfer(
        &mut self,
        transfers: impl IntoIterator<Item = InserterTransfer>,
        backend: &mut Backend,
    ) {
        // TODO: Support changes without disrupting the backend as much (i.e. keep swing)
        for transfer in transfers {
            assert!(transfer.sources.is_some() || transfer.dest.is_some());
            let info = self.inserter_list.remove(transfer.id.0 as usize).unwrap();

            let new_source = transfer
                .sources
                .unwrap_or_else(|| info.sources.into_iter().flatten().collect());
            let new_dest = transfer.dest.unwrap_or(info.dest);

            let grid = self.power_grid_list[info.power_grid_id.0 as usize].backend_id;

            let source_items = new_source
                .iter()
                .map(|source| self.get_items_takeable_from(*source))
                .reduce(|mut a, b| {
                    a.union(&b);
                    a
                })
                .unwrap_or(ItemSet::empty());

            let dest_items = new_dest.map(|dest| self.get_items_placeable_into(dest));

            let mut items = source_items;
            items.intersection(&info.user_filter);
            if let Some(dest_items) = &dest_items {
                items.intersection(dest_items);
            }

            if let Some(dest) = &new_dest {
                for &source in &new_source {
                    self.apply_effect_of_new_edge(source, *dest, &items, backend);
                }
            }

            let new_sources = new_source
                .iter()
                .copied()
                .map(Option::Some)
                .chain(iter::repeat(None))
                .take(MAX_CONN_COUNT)
                .collect_array()
                .expect("Take ensures len");

            let new_backend_sources = new_source
                .into_iter()
                .map(|conn| self.get_backend_conn(conn))
                .collect();

            let sources = info
                .sources
                .map(|slot| slot.map(|conn| self.get_backend_conn(conn)));

            let res = backend.change_inserter_conn(
                FullInserterIdentifier {
                    grid,
                    inserter_id: info.backend_id,
                    inferred_items: &info.inferred_items,
                    source: sources,
                    dest: info.dest.map(|dest| self.get_backend_conn(dest)),
                    movetime: info.movetime,
                },
                &backend::power_grid::inserter::InserterAdditionInfo {
                    power_grid: grid,
                    middle_id: transfer.id,
                    source: new_backend_sources,
                    dest: new_dest.map(|dest| self.get_backend_conn(dest)),
                    items: items.clone(),
                    movetime: info.movetime,
                },
            );

            let new_id = match res {
                backend::AdditionResult::Added {
                    new_id,
                    relocations,
                } => {
                    if !relocations.is_empty() {
                        todo!("Handle relocations");
                    }
                    new_id
                },
                backend::AdditionResult::Failed { info: _ } => todo!(),
            };

            self.inserter_list.insert(
                transfer.id.0 as usize,
                InserterInfo {
                    backend_id: new_id,
                    power_grid_id: info.power_grid_id,
                    sources: new_sources,
                    dest: new_dest,
                    inferred_items: items,
                    movetime: info.movetime,
                    user_filter: info.user_filter,
                },
            );
        }
    }
}
