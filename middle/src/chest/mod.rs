use backend::{
    AdditionResult, Backend,
    chests::{ChestBackendID, FullChestIdentifier},
};
use data::item::item_set::ItemSet;
use middle_indices::{ChestMiddleID, InserterMiddleID};
use smallvec::SmallVec;

use crate::{Middle, assembler::InserterTransfer};

#[derive(Debug, Clone)]
pub(crate) struct ChestInfo {
    pub(crate) backend_id: ChestBackendID,
    pub(crate) inferred_items: ItemSet,

    pub(crate) connected_inserters: SmallVec<[InserterMiddleID; 4]>,
}

#[derive(Debug)]
pub struct ChestAdditionInfo {
    pub num_slots: u16,
    // TODO: Slot count override
}

pub struct ChestRemovalInfo<I: IntoIterator<Item = InserterTransfer>> {
    pub id: ChestMiddleID,
    pub inserter_changes: I,
}

impl Middle {
    pub fn add_chest(&mut self, info: &ChestAdditionInfo, backend: &mut Backend) -> ChestMiddleID {
        let ChestAdditionInfo { num_slots } = info;

        let index = self.chest_list.next_push_index();

        let backend_id = backend.add_chest(backend::chests::ChestAdditionInfo {
            items: &ItemSet::empty(),
            num_slots: *num_slots,
        });

        match backend_id {
            AdditionResult::Added {
                new_id,
                relocations,
            } => {
                if !relocations.is_empty() {
                    todo!("Handle relocations")
                }

                let real_index = self.chest_list.push(ChestInfo {
                    backend_id: new_id,
                    inferred_items: ItemSet::empty(),
                    connected_inserters: vec![].into(),
                });

                assert_eq!(index, real_index);

                ChestMiddleID(index.try_into().expect("More than u32::MAX chests"))
            },
            AdditionResult::Failed { info: _ } => todo!(),
        }
    }

    /// The chest must not be connected to any inserters
    pub fn remove_chest(
        &mut self,
        info: ChestRemovalInfo<impl IntoIterator<Item = InserterTransfer>>,
        backend: &mut Backend,
    ) {
        let chest = self
            .chest_list
            .remove(info.id.0 as usize)
            .expect("Tried to remove chest that did not exist");

        backend.remove_chest(FullChestIdentifier {
            items: &chest.inferred_items,
            id: chest.backend_id,
        });

        for transfer in info.inserter_changes {
            todo!("Move inserter")
        }
    }
}
