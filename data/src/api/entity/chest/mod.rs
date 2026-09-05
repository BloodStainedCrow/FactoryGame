use crate::api::entity::EntityInfo;

#[derive(Debug, serde::Deserialize)]
pub struct ChestInfo {
    pub entity_info: EntityInfo,
}
