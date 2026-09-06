use data::entity::assember::Recipe;
use middle_indices::AssemblerMiddleID;

use crate::{
    AdditionResult, Backend,
    power_grid::{PowerGridBackendID, SingleRecipeAssemblerInfo},
};

#[derive(Debug, Clone, Copy)]
pub struct AssemblerBackendID(pub(super) usize);

#[derive(Debug)]
pub struct AssemblerAdditionInfo {
    pub power_grid: PowerGridBackendID,
    pub recipe: Recipe,
    pub middle_id: AssemblerMiddleID,
    // TODO: Stats
}

#[derive(Debug, Clone, Copy)]
pub struct FullAssemblerIdentifier {
    pub recipe: Recipe,
    pub grid: PowerGridBackendID,
    pub assembler_id: AssemblerBackendID,
}

impl Backend {
    pub fn add_assembler(
        &mut self,
        info: AssemblerAdditionInfo,
    ) -> AdditionResult<AssemblerMiddleID, AssemblerBackendID> {
        self.add_assembler_internal(
            info.recipe,
            info.power_grid,
            SingleRecipeAssemblerInfo {
                middle: info.middle_id,
            },
        )
    }

    fn add_assembler_internal(
        &mut self,
        recipe: Recipe,
        grid: PowerGridBackendID,
        data: SingleRecipeAssemblerInfo,
    ) -> AdditionResult<AssemblerMiddleID, AssemblerBackendID> {
        let assembler_list = &mut self.power_grids[grid.0]
            .assemblers
            .entry(recipe)
            .or_default();

        let index = assembler_list.push(data);

        AdditionResult::Added {
            new_id: AssemblerBackendID(index),
            relocations: vec![],
        }
    }

    /// NOTE: This assembler needs to already not have any inserter connections
    pub fn remove_assembler(&mut self, assembler: FullAssemblerIdentifier) {
        todo!()
    }

    fn remove_assembler_internal(
        &mut self,
        assembler: FullAssemblerIdentifier,
    ) -> SingleRecipeAssemblerInfo {
        let FullAssemblerIdentifier {
            recipe,
            grid,
            assembler_id,
        } = assembler;

        let assembler_list = &mut self.power_grids[grid.0]
            .assemblers
            .get_mut(&recipe)
            .expect("Tried to move assembler which did not exist");

        let assembler = assembler_list
            .remove(assembler_id.0)
            .expect("Tried to move assembler which did not exist");

        assembler
    }

    pub fn move_assembler(
        &mut self,
        assembler: FullAssemblerIdentifier,
        new_grid: PowerGridBackendID,
    ) -> AdditionResult<AssemblerMiddleID, AssemblerBackendID> {
        let data = self.remove_assembler_internal(assembler);

        self.add_assembler_internal(assembler.recipe, new_grid, data)
    }

    pub fn get_assembler_state(&self) {}
}
