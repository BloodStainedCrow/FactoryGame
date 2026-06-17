use crate::{EntityPrototypeKind, entity::GlobalTy};

#[derive(Debug, Clone, Copy)]
pub struct AssemblerTy(u16);

impl From<AssemblerTy> for GlobalTy {
    fn from(value: AssemblerTy) -> Self {
        Self(
            EntityPrototypeKind::Assembler
                .global_index_for_kind_index(value.0 as usize)
                .expect("Illegal PowerPoleTy")
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

#[derive(Debug, Clone, Copy)]
pub struct Recipe(u16);

// This might need more info like the tiles its placed on
#[must_use]
pub fn default_recipe(ty: AssemblerTy) -> Recipe {
    todo!()
}
