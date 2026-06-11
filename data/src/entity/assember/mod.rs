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

pub struct Recipe(u16);
