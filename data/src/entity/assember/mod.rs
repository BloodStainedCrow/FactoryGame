use crate::entity::GlobalTy;

#[derive(Debug, Clone, Copy)]
pub struct AssemblerTy(u16);

impl From<AssemblerTy> for GlobalTy {
    fn from(value: AssemblerTy) -> Self {
        todo!()
    }
}

impl TryFrom<GlobalTy> for AssemblerTy {
    type Error = ();

    fn try_from(value: GlobalTy) -> Result<Self, Self::Error> {
        todo!()
    }
}

#[derive(Debug, Clone, Copy)]
pub struct Recipe(u16);

// This might need more info like the tiles its placed on
#[must_use]
pub fn default_recipe(ty: AssemblerTy) -> Recipe {
    todo!()
}
