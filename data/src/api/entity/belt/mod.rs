use crate::api::entity::EntityInfo;

#[derive(Debug, serde::Deserialize)]
pub struct BeltInfo {
    pub entity_info: EntityInfo,
}
