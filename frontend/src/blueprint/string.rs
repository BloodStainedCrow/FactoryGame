use base64::{
    Engine,
    prelude::{BASE64_STANDARD, BASE64_URL_SAFE},
};

use crate::blueprint::versions::VersionedBlueprint;

#[derive(Debug, Clone)]
pub struct BlueprintString(pub String);

impl From<super::Blueprint> for BlueprintString {
    fn from(value: super::Blueprint) -> Self {
        let raw = RawBlueprintStringData::from(VersionedBlueprintStringData::from(value));

        Self(BASE64_URL_SAFE.encode(raw.0))
    }
}

pub(super) struct RawBlueprintStringData(pub(super) Vec<u8>);

impl From<VersionedBlueprintStringData> for RawBlueprintStringData {
    fn from(mut value: VersionedBlueprintStringData) -> Self {
        value.data.splice(..0, value.version.to_le_bytes());

        Self(value.data)
    }
}

#[derive(Debug)]
pub enum BlueprintStringCorrupt {
    NotBase64(base64::DecodeError),
    MissingVersion,
    UnknownVersion(u32),
    CannotDeserialize,
    // TODO: Naming
    MissingThing(String),
}

impl From<!> for BlueprintStringCorrupt {
    fn from(value: !) -> Self {
        value
    }
}

impl TryFrom<BlueprintString> for RawBlueprintStringData {
    type Error = BlueprintStringCorrupt;

    fn try_from(value: BlueprintString) -> Result<Self, Self::Error> {
        BASE64_STANDARD
            .decode(value.0)
            .map(|v| Self(v))
            .map_err(|e| BlueprintStringCorrupt::NotBase64(e))
    }
}

impl RawBlueprintStringData {
    pub(super) fn get_versioned(
        &self,
    ) -> Result<VersionedBlueprintStringDataBorrowed<'_>, BlueprintStringCorrupt> {
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

        let data: Vec<u8> = bincode::serde::encode_to_vec(&value, bincode::config::standard())
            .expect("This should not fail");

        Self { version, data }
    }
}

pub(super) struct VersionedBlueprintStringDataBorrowed<'a> {
    pub(super) version: u32,
    pub(super) data: &'a [u8],
}
