use crate::api::entity::EntityInfo;

#[derive(Debug, serde::Deserialize)]
pub struct SolarPanelInfo {
    pub entity_info: EntityInfo,
    // TODO: Power gen
}
