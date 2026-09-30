use crate::{DATA_STORE, EntityPrototypeKind, entity::GlobalTy};

#[derive(
    Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, serde::Serialize, serde::Deserialize,
)]
pub struct SolarPanelTy(u16);

impl From<SolarPanelTy> for usize {
    fn from(value: SolarPanelTy) -> Self {
        value.0.into()
    }
}

impl From<SolarPanelTy> for GlobalTy {
    fn from(value: SolarPanelTy) -> Self {
        Self(
            EntityPrototypeKind::SolarPanel
                .global_index_for_kind_index(value.0 as usize)
                .expect("Illegal SolarPanelTy")
                .try_into()
                .expect("More than u16::MAX entities"),
        )
    }
}

impl TryFrom<GlobalTy> for SolarPanelTy {
    type Error = ();

    fn try_from(value: GlobalTy) -> Result<Self, Self::Error> {
        EntityPrototypeKind::SolarPanel
            .kind_index_from_global_index(value.0 as usize)
            .map(|idx| Self(idx.try_into().expect("More than u16::MAX entities")))
            .ok_or(())
    }
}

pub fn num_solar_panel_tys() -> usize {
    DATA_STORE
        .entities
        .iter()
        .filter(|e| e.kind == EntityPrototypeKind::SolarPanel)
        .count()
}
