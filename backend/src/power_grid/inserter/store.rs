use std::collections::BTreeMap;

use data::item::Item;
use itertools::Either;
use middle_indices::InserterMiddleID;
use stable_vec::StableVec;

use crate::{
    RelocationInfo,
    inserter::pure::{InserterRenderState, PureOneToOneInserterStore},
    power_grid::inserter::{InserterBackendID, InserterKind, SingleInserterInfo},
};

#[derive(Debug, Clone, Default)]
pub(crate) struct InserterStore {
    empty_inserters: StableVec<SingleInserterInfo>,

    pure_to_pure: BTreeMap<Item, BTreeMap<u16, PureOneToOneInserterStore>>,
}

impl InserterStore {
    pub fn add_inserter(
        &mut self,
        kind: InserterKind,
        info: SingleInserterInfo,
    ) -> InserterBackendID {
        match kind {
            InserterKind::EmptyInserter {} => {
                let index = self.empty_inserters.push(info);
                InserterBackendID(index.try_into().expect("More than u32::MAX inserters"))
            },
            InserterKind::OneToOneSingleItem {
                item,
                source,
                dest,
                movetime,
            } => {
                let item_list = self.pure_to_pure.entry(item).or_default();
                let movetime_list = item_list
                    .entry(info.movetime)
                    .or_insert_with(|| PureOneToOneInserterStore::new(item, info.movetime));

                let id = movetime_list.add_inserter();

                // FIXME: Add this into the waitlist

                id
            },
        }
    }

    pub(crate) fn get_inserter_state(
        &self,
        id: InserterBackendID,
        kind: super::InserterKind,
    ) -> InserterRenderState {
        match kind {
            InserterKind::OneToOneSingleItem {
                item,
                source,
                dest,
                movetime,
            } => {
                // FIXME: Check source waitlist
                // FIXME: Check dest waitlist

                // CORRECTNESS: We checked waitlist before
                self.pure_to_pure[&item][&movetime].get_state_after_checking_waitlist(id)
            },

            InserterKind::EmptyInserter {} => InserterRenderState::WaitingForItems(0),
        }
    }

    pub(crate) fn remove_inserter(
        &mut self,
        id: InserterBackendID,
        inserter: &InserterKind,
    ) -> SingleInserterInfo {
        match inserter {
            InserterKind::OneToOneSingleItem {
                item,
                source,
                dest,
                movetime,
            } => {
                // FIXME: Check source waitlist
                // FIXME: Check dest waitlist

                // CORRECTNESS: We checked waitlist before
                let state = self
                    .pure_to_pure
                    .get_mut(&item)
                    .unwrap()
                    .get_mut(&movetime)
                    .unwrap()
                    .remove_inserter(id, false);

                match state {
                    Either::Left(moving) => SingleInserterInfo {
                        middle_id: todo!(),
                        movetime: *movetime,
                    },
                    Either::Right(waiting) => SingleInserterInfo {
                        middle_id: todo!(),
                        movetime: *movetime,
                    },
                }
            },

            InserterKind::EmptyInserter {} => self
                .empty_inserters
                .remove(id.0 as usize)
                .expect("Tried to remove missing inserter"),
        }
    }

    pub(crate) fn merge(
        &mut self,
        removed: Self,
    ) -> Vec<RelocationInfo<InserterMiddleID, InserterBackendID>> {
        let Self {
            empty_inserters,
            pure_to_pure,
        } = removed;

        let mut relocation = vec![];

        for (old_index, value) in empty_inserters {
            let middle_id = value.middle_id;
            let new_index = self.empty_inserters.push(value);

            relocation.push(RelocationInfo {
                middle: middle_id,
                new_backend: InserterBackendID(
                    new_index.try_into().expect("More than u32::MAX inserters"),
                ),
            });
        }

        for (item, value) in pure_to_pure {
            todo!()
        }

        relocation
    }
}
