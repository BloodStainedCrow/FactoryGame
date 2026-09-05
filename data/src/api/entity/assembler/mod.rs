use crate::api::entity::EntityInfo;

#[derive(Debug, serde::Deserialize)]
pub struct AssemblerInfo {
    pub entity_info: EntityInfo,
}
