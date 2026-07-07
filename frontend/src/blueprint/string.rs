use base64::{Engine, prelude::BASE64_URL_SAFE};

use crate::blueprint::versions::VersionedBlueprint;

#[derive(Debug)]
pub struct BlueprintString(String);

impl From<super::Blueprint> for BlueprintString {
    fn from(value: super::Blueprint) -> Self {
        let raw = RawBlueprintStringData::from(VersionedBlueprintStringData::from(value));

        Self(BASE64_URL_SAFE.encode(raw.0))
    }
}

pub(super) struct RawBlueprintStringData(Vec<u8>);

impl From<VersionedBlueprintStringData> for RawBlueprintStringData {
    fn from(mut value: VersionedBlueprintStringData) -> Self {
        value.data.splice(..0, value.version.to_le_bytes());

        Self(value.data)
    }
}

#[derive(Debug)]
pub enum BlueprintStringCorrupt {
    NotBase64,
    MissingVersion,
    UnknownVersion(u32),
    CannotDeserialize,
    // TODO: Naming
    MissingThing(String),
}

impl TryFrom<BlueprintString> for RawBlueprintStringData {
    type Error = BlueprintStringCorrupt;

    fn try_from(value: BlueprintString) -> Result<Self, Self::Error> {
        BASE64_URL_SAFE
            .decode(value.0)
            .map(|v| Self(v))
            .map_err(|_e| BlueprintStringCorrupt::NotBase64)
    }
}

impl RawBlueprintStringData {
    pub(super) fn get_versioned(
        &self,
    ) -> Result<VersionedBlueprintStringDataBorrowed, BlueprintStringCorrupt> {
        let (version_slice, data_slice) = self.0.split_at(4);

        let Ok(version_arr) = version_slice.try_into() else {
            return Err(BlueprintStringCorrupt::MissingVersion);
        };

        let version = u32::from_le_bytes(version_arr);
        Ok(VersionedBlueprintStringDataBorrowed {
            version,
            data: data_slice,
        })
    }
}

struct VersionedBlueprintStringData {
    version: u32,
    data: Vec<u8>,
}

impl From<super::Blueprint> for VersionedBlueprintStringData {
    fn from(value: super::Blueprint) -> Self {
        let version = super::Blueprint::get_version();

        let data: Vec<u8> = todo!();

        Self { version, data }
    }
}

pub(super) struct VersionedBlueprintStringDataBorrowed<'a> {
    pub(super) version: u32,
    pub(super) data: &'a [u8],
}
