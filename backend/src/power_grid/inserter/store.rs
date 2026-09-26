use std::collections::{BTreeMap, HashMap};

use data::item::Item;
use itertools::Either;
use middle_indices::InserterMiddleID;
use stable_vec::StableVec;

use crate::{
    RelocationInfo,
    inserter::pure::{InserterRenderState, PureOneToOneInserterStore},
    power_grid::{
        PowerGridBackendID,
        inserter::{FullInserterIdentifier, InserterBackendID, InserterKind, SingleInserterInfo},
    },
};

#[derive(Debug, Clone, Default)]
pub struct InserterStore {
    empty_inserters: StableVec<SingleInserterInfo>,

    pure_to_pure: BTreeMap<Item, BTreeMap<u16, PureOneToOneInserterStore>>,
}

impl InserterStore {
    pub fn add_inserter(
        &mut self,
        kind: &InserterKind,
        info: SingleInserterInfo,
    ) -> InserterBackendID {
        match kind {
            InserterKind::EmptyInserter {} => {
                let index = self.empty_inserters.push(info);
                InserterBackendID(index.try_into().expect("More than u32::MAX inserters"))
            },
            InserterKind::OneToOneSingleItem {
                item,
                source: _,
                dest: _,
                movetime: _,
            } => {
                let item_list = self.pure_to_pure.entry(*item).or_default();
                let movetime_list = item_list
                    .entry(info.movetime)
                    .or_insert_with(|| PureOneToOneInserterStore::new(*item, info.movetime));

                let id = movetime_list.add_inserter();

                // FIXME: Add this into the waitlist

                id
            },
            InserterKind::SushiToSushiSingleItem { .. } => todo!(),
            InserterKind::PureBeltToOneSingleItem { .. } => todo!(),
            InserterKind::OneToPureBeltSingleItem { .. } => todo!(),
        }
    }

    pub(crate) fn get_inserter_state(
        &self,
        id: InserterBackendID,
        kind: &InserterKind,
    ) -> InserterRenderState {
        match kind {
            InserterKind::OneToOneSingleItem {
                item,
                source: _,
                dest: _,
                movetime,
            } => {
                // FIXME: Check source waitlist
                // FIXME: Check dest waitlist

                // CORRECTNESS: We checked waitlist before
                self.pure_to_pure[&item][&movetime].get_state_after_checking_waitlist(id)
            },
            InserterKind::SushiToSushiSingleItem { .. } => todo!(),
            InserterKind::PureBeltToOneSingleItem { .. } => todo!(),
            InserterKind::OneToPureBeltSingleItem { .. } => todo!(),

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
                source: _,
                dest: _,
                movetime,
            } => {
                // FIXME: Check source waitlist
                // FIXME: Check dest waitlist

                // CORRECTNESS: We checked waitlist before
                let state = self
                    .pure_to_pure
                    .get_mut(item)
                    .expect("Tried to remove inserter with item that did not exist")
                    .get_mut(movetime)
                    .expect("Tried to remove inserter with movetime that did not exist")
                    .remove_inserter(id, false);

                match state {
                    Either::Left(_moving) => SingleInserterInfo {
                        middle_id: todo!(),
                        movetime: *movetime,
                    },
                    Either::Right(_waiting) => SingleInserterInfo {
                        middle_id: todo!(),
                        movetime: *movetime,
                    },
                }
            },
            InserterKind::SushiToSushiSingleItem { .. } => todo!(),
            InserterKind::PureBeltToOneSingleItem { .. } => todo!(),
            InserterKind::OneToPureBeltSingleItem { .. } => todo!(),

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

        for (_old_index, value) in empty_inserters {
            let middle_id = value.middle_id;
            let new_index = self.empty_inserters.push(value);

            relocation.push(RelocationInfo {
                middle: middle_id,
                new_backend: InserterBackendID(
                    new_index.try_into().expect("More than u32::MAX inserters"),
                ),
            });
        }

        for (_item, _value) in pure_to_pure {
            todo!()
        }

        relocation
    }

    #[expect(clippy::needless_pass_by_ref_mut)]
    pub(crate) fn split(
        &mut self,
        old_grid_id: PowerGridBackendID,
        new_count: usize,
        inserter_map: &HashMap<FullInserterIdentifier<'_>, u8>,
        grid_id_map: &[PowerGridBackendID],
    ) -> ! {
        todo!()
    }
}
