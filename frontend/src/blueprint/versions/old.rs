/// This is the blueprint format of the pre-refactor codebase. This is here only so I can reuse (and reexport) my test blueprints.
use std::u32;

use crate::blueprint::versions::VersionedBlueprint;

#[derive(Debug, serde::Deserialize)]
pub struct Blueprint {}

impl VersionedBlueprint for Blueprint {
    fn get_version() -> u32 {
        u32::MAX
    }
}

impl Into<super::Blueprint> for Blueprint {
    fn into(self) -> super::Blueprint {
        super::Blueprint {}
    }
}
