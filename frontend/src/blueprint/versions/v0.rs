use crate::blueprint::versions::VersionedBlueprint;

#[derive(Debug, serde::Serialize, serde::Deserialize)]
pub struct Blueprint {}

impl VersionedBlueprint for Blueprint {
    fn get_version() -> u32 {
        0
    }
}
