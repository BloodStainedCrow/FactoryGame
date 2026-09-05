use crate::{EntityPrototypeKind, entity::GlobalTy};

#[derive(Debug, Clone, Copy, PartialEq, Eq, serde::Serialize, serde::Deserialize)]
pub struct InserterTy(u16);

impl From<InserterTy> for GlobalTy {
    fn from(value: InserterTy) -> Self {
        Self(
            EntityPrototypeKind::Inserter
                .global_index_for_kind_index(value.0 as usize)
                .expect("Illegal InserterTy")
                .try_into()
                .expect("More than u16::MAX entities"),
        )
    }
}

impl TryFrom<GlobalTy> for InserterTy {
    type Error = ();

    fn try_from(value: GlobalTy) -> Result<Self, Self::Error> {
        EntityPrototypeKind::Inserter
            .kind_index_from_global_index(value.0 as usize)
            .map(|idx| Self(idx.try_into().expect("More than u16::MAX entities")))
            .ok_or(())
    }
}
