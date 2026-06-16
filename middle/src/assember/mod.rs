use data::{entity::assember::Recipe, item::ItemStack};

use crate::{Middle, lists::AssemblerIndex};

pub(crate) struct MiddleAssemblerInfo {
    // TODO: Do I want to internally have each assembler have a recipe?
    current_recipe: Recipe,
    // backend_id: !,
    // modules: !,
}

pub struct AssemblerAdditionInfo {
    pub recipe: Recipe,
}

pub struct AssemblerRemovalInfo {
    pub returned_items: Vec<ItemStack>,
}

impl Middle {
    #[must_use]
    pub fn get_assembler_recipe(&self, id: AssemblerIndex) -> Recipe {
        self.assembler_list[id.0 as usize].current_recipe
    }

    // TODO: All additional info
    pub fn add_assembler(
        &mut self,
        info: &AssemblerAdditionInfo,
        backend: &mut !,
    ) -> AssemblerIndex {
        let index = self.assembler_list.push(MiddleAssemblerInfo {
            current_recipe: info.recipe,
        });

        AssemblerIndex(index.try_into().expect("More than u32::MAX assemblers"))
    }

    pub fn change_assembler_recipe(
        &mut self,
        id: AssemblerIndex,
        new_recipe: Recipe,
        backend: &mut !,
    ) {
        // TODO: This will impact the graph
        self.assembler_list[id.0 as usize].current_recipe = new_recipe;
    }

    pub fn remove_assembler(
        &mut self,
        id: AssemblerIndex,
        backend: &mut !,
    ) -> AssemblerRemovalInfo {
        todo!()
    }
}
