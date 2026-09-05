use crate::{EntityPrototypeKind, entity::GlobalTy};

#[derive(Debug, Clone, Copy, PartialEq, Eq, serde::Serialize, serde::Deserialize)]
pub struct ChestTy(u16);

impl From<ChestTy> for GlobalTy {
    fn from(value: ChestTy) -> Self {
        Self(
            EntityPrototypeKind::Chest
                .global_index_for_kind_index(value.0 as usize)
                .expect("Illegal ChestTy")
                .try_into()
                .expect("More than u16::MAX entities"),
        )
    }
}

impl TryFrom<GlobalTy> for ChestTy {
    type Error = ();

    fn try_from(value: GlobalTy) -> Result<Self, Self::Error> {
        EntityPrototypeKind::Chest
            .kind_index_from_global_index(value.0 as usize)
            .map(|idx| Self(idx.try_into().expect("More than u16::MAX entities")))
            .ok_or(())
    }
}
