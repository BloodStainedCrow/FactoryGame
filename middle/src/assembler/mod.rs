use backend::{Backend, RelocationInfo, power_grid::assembler::AssemblerBackendID};
use data::{entity::assember::Recipe, item::ItemStack};
use middle_indices::{AssemblerMiddleID, PowerGridMiddleID};

use crate::Middle;

#[derive(Debug, Clone)]
pub(crate) struct MiddleAssemblerInfo {
    // TODO: Do I want to internally have each assembler have a recipe? Even ones that do not actually have one, by just giving them a 0 -> 0 recipe? Probably. This makes stuff homogeneous
    pub(crate) current_recipe: Recipe,
    pub(crate) backend_id: AssemblerBackendID,

    // NOTE: Each assembler belonges to a power grid. There will be a special power grid that holds all the actually unconnected entities
    // TODO: I might want to make this into an enum, that encodes, that some assemblers might be shared by multiple power grids
    // Likely this would mean either Solo(ID) or Shared (i.e. Option)
    pub(crate) power_grid_id: PowerGridMiddleID,

    connected_inserters: Vec<!>,
    // modules: !,
}

pub struct AssemblerAdditionInfo {
    pub recipe: Recipe,
    pub power_grid: PowerGridMiddleID,
}

pub struct AssemblerRemovalInfo {
    pub returned_items: Vec<ItemStack>,
}

impl Middle {
    #[must_use]
    pub fn get_assembler_recipe(&self, id: AssemblerMiddleID) -> Recipe {
        self.assembler_list[id.0 as usize].current_recipe
    }

    #[must_use]
    pub fn get_assembler_power_grid(&self, id: AssemblerMiddleID) -> PowerGridMiddleID {
        self.assembler_list[id.0 as usize].power_grid_id
    }

    pub(crate) fn handle_assembler_relocations(
        &mut self,
        relocations: Vec<RelocationInfo<AssemblerMiddleID, AssemblerBackendID>>,
    ) {
        for relocation in relocations {
            self.assembler_list[relocation.middle.0 as usize].backend_id = relocation.new_backend;
        }
    }

    // TODO: All additional info
    #[must_use]
    pub fn add_assembler(
        &mut self,
        info: &AssemblerAdditionInfo,
        backend: &mut Backend,
    ) -> AssemblerMiddleID {
        let next_index = self.assembler_list.next_push_index();

        let backend_id =
            match backend.add_assembler(backend::power_grid::assembler::AssemblerAdditionInfo {
                power_grid: self.power_grid_list[info.power_grid.0 as usize].backend_id,
                recipe: info.recipe,
                middle_id: AssemblerMiddleID(next_index.try_into().unwrap()),
            }) {
                backend::AdditionResult::Added {
                    new_id,
                    relocations,
                } => {
                    self.handle_assembler_relocations(relocations);
                    new_id
                },
                backend::AdditionResult::Failed { info } => todo!(),
            };

        let index = self.assembler_list.push(MiddleAssemblerInfo {
            current_recipe: info.recipe,
            power_grid_id: info.power_grid,
            backend_id,
            connected_inserters: Default::default(),
        });

        assert_eq!(next_index, index);

        AssemblerMiddleID(index.try_into().expect("More than u32::MAX assemblers"))
    }

    pub fn change_assembler_recipe(
        &mut self,
        id: AssemblerMiddleID,
        new_recipe: Recipe,
        backend: &mut Backend,
    ) {
        // TODO: This will impact the graph
        self.assembler_list[id.0 as usize].current_recipe = new_recipe;
    }

    #[must_use]
    pub fn remove_assembler(
        &mut self,
        id: AssemblerMiddleID,
        backend: &mut Backend,
    ) -> AssemblerRemovalInfo {
        todo!()
    }
}
