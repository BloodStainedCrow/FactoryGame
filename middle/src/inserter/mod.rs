use std::iter;

use backend::{Backend, power_grid::inserter::InserterBackendID};
use data::item::item_set::ItemSet;
use itertools::Itertools;
use middle_indices::{InserterMiddleID, PowerGridMiddleID};

pub mod conn;

use crate::{Middle, inserter::conn::Conn};

pub const MAX_CONN_COUNT: usize = 2;
static_assertions::const_assert!(std::mem::size_of::<Option<Conn>>() <= 8);

#[derive(Debug, Clone)]
pub(crate) struct InserterInfo {
    pub(crate) backend_id: InserterBackendID,

    // NOTE: Each inserter belonges to a power grid. There will be a special power grid that holds all the actually unconnected entities
    pub(crate) power_grid_id: PowerGridMiddleID,

    sources: [Option<Conn>; MAX_CONN_COUNT],
    dest: Conn,
    inferred_items: ItemSet,

    user_filter: (),
}

#[derive(Debug)]
pub struct InserterAdditionInfo {
    pub power_grid_id: PowerGridMiddleID,

    pub sources: Vec<Conn>,
    pub dest: Conn,
    pub item_filter: ItemSet,
}

impl Middle {
    pub fn add_inserter(
        &mut self,
        info: &InserterAdditionInfo,
        backend: &mut Backend,
    ) -> InserterMiddleID {
        let next_index = self
            .inserter_list
            .next_push_index()
            .try_into()
            .expect("More than u32::MAX inserters");

        assert_eq!(
            info.sources.len(),
            1,
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
            .expect("Need at least one source");

        let dest_items = self.get_items_placeable_into(info.dest);

        let mut items = source_items;
        items.intersection(&info.item_filter);
        items.intersection(&dest_items);

        for &source in &info.sources {
            self.apply_effect_of_new_edge(source, info.dest, &items, backend);
        }

        let backend_id =
            match backend.add_inserter(backend::power_grid::inserter::InserterAdditionInfo {
                power_grid: self.power_grid_list[info.power_grid_id.0 as usize].backend_id,
                middle_id: InserterMiddleID(next_index),
                source: todo!(),
                dest: todo!(),
                items,
            }) {
                backend::AdditionResult::Added {
                    new_id,
                    relocations,
                } => {
                    todo!("Handle relocations");
                    new_id
                },
                backend::AdditionResult::Failed { info } => todo!(),
            };

        let index = self.inserter_list.push(InserterInfo {
            power_grid_id: info.power_grid_id,
            backend_id,

            sources: info
                .sources
                .into_iter()
                .map(Option::Some)
                .chain(iter::repeat(None))
                .take(MAX_CONN_COUNT)
                .collect_array()
                .unwrap(),
            dest: info.dest,
            inferred_items: items,

            user_filter: (),
        });

        assert_eq!(next_index, index as u32);

        InserterMiddleID(next_index)
    }
}
