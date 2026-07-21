use crate::blueprint::versions::VersionedBlueprint;

#[derive(Debug, serde::Serialize, serde::Deserialize)]
pub struct Blueprint {}

impl VersionedBlueprint for Blueprint {
    fn get_version() -> u32 {
        0
    }
}

impl<'a> TryFrom<&'a [u8]> for Blueprint {
    type Error = postcard::Error;

    fn try_from(data: &'a [u8]) -> Result<Self, Self::Error> {
        postcard::from_bytes(data)
    }
}
