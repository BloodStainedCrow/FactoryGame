use crate::api::entity::EntityInfo;

#[derive(Debug, serde::Deserialize)]
pub struct PowerPoleInfo {
    pub entity_info: EntityInfo,

    pub range: u8,
    pub wire_reach: u8,
}
