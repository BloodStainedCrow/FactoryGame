use crate::{EntityPrototypeKind, entity::GlobalTy};

#[derive(Debug, Clone, Copy, PartialEq, Eq, serde::Serialize, serde::Deserialize)]
pub struct BeaconTy(u16);

impl From<BeaconTy> for GlobalTy {
    fn from(value: BeaconTy) -> Self {
        Self(
            EntityPrototypeKind::Beacon
                .global_index_for_kind_index(value.0 as usize)
                .expect("Illegal BeaconTy")
                .try_into()
                .expect("More than u16::MAX entities"),
        )
    }
}

impl TryFrom<GlobalTy> for BeaconTy {
    type Error = ();

    fn try_from(value: GlobalTy) -> Result<Self, Self::Error> {
        EntityPrototypeKind::Beacon
            .kind_index_from_global_index(value.0 as usize)
            .map(|idx| Self(idx.try_into().expect("More than u16::MAX entities")))
            .ok_or(())
    }
}
