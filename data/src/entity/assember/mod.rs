use std::collections::BTreeMap;

use crate::{DATA_STORE, EntityPrototypeKind, api::recipe::RecipeIdentifier, entity::GlobalTy};

#[derive(Debug, Clone, Copy, PartialEq, Eq, serde::Serialize, serde::Deserialize)]
pub struct AssemblerTy(u16);

impl From<AssemblerTy> for GlobalTy {
    fn from(value: AssemblerTy) -> Self {
        Self(
            EntityPrototypeKind::Assembler
                .global_index_for_kind_index(value.0 as usize)
                .expect("Illegal AssemblerTy")
                .try_into()
                .expect("More than u16::MAX entities"),
        )
    }
}

impl TryFrom<GlobalTy> for AssemblerTy {
    type Error = ();

    fn try_from(value: GlobalTy) -> Result<Self, Self::Error> {
        EntityPrototypeKind::Assembler
            .kind_index_from_global_index(value.0 as usize)
            .map(|idx| Self(idx.try_into().expect("More than u16::MAX entities")))
            .ok_or(())
    }
}

#[derive(Debug)]
pub struct AssemblerInfo {
    pub(crate) default_recipe: BTreeMap<Option<()>, Recipe>,
}

#[derive(
    Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash, serde::Serialize, serde::Deserialize,
)]
pub struct Recipe(pub(crate) u16);

impl<'a> TryFrom<&'a RecipeIdentifier> for Recipe {
    type Error = ();

    fn try_from(value: &'a RecipeIdentifier) -> Result<Self, Self::Error> {
        Ok(DATA_STORE
            .recipes
            .iter()
            .position(|recipe_info| &recipe_info.recipe_identifier == value)
            .map_or(Self(0), |index| {
                Self(index.try_into().expect("More than u32::MAX recipies"))
            }))
    }
}

// This might need more info like the tiles its placed on
#[must_use]
pub fn default_recipe(ty: AssemblerTy, floor: Option<()>) -> Recipe {
    DATA_STORE.assemblers[ty.0 as usize]
        .default_recipe
        .get(&floor)
        .copied()
        .unwrap_or_else(|| DATA_STORE.assemblers[ty.0 as usize].default_recipe[&None])
}
