use crate::{EntityPrototypeKind, entity::GlobalTy};

mod above_ground;
mod underground;

#[derive(Debug, Clone, Copy, PartialEq, Eq, serde::Serialize, serde::Deserialize)]
pub struct BeltTy(u16);

impl From<BeltTy> for GlobalTy {
    fn from(value: BeltTy) -> Self {
        Self(
            EntityPrototypeKind::Belt
                .global_index_for_kind_index(value.0 as usize)
                .expect("Illegal BeltTy")
                .try_into()
                .expect("More than u16::MAX entities"),
        )
    }
}

impl TryFrom<GlobalTy> for BeltTy {
    type Error = ();

    fn try_from(value: GlobalTy) -> Result<Self, Self::Error> {
        EntityPrototypeKind::Belt
            .kind_index_from_global_index(value.0 as usize)
            .map(|idx| Self(idx.try_into().expect("More than u16::MAX entities")))
            .ok_or(())
    }
}

/// The speed of the belt
#[must_use]
pub fn belt_speed(ty: BeltTy) -> ! {
    todo!()
}
