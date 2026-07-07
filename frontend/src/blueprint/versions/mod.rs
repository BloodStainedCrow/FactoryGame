mod old;
mod v0;

use std::u32;

pub use v0::Blueprint;

use crate::blueprint::string::{BlueprintString, BlueprintStringCorrupt, RawBlueprintStringData};

type CurrentBlueprint = Blueprint;

pub trait VersionedBlueprint: Into<CurrentBlueprint> {
    fn get_version() -> u32;
}

impl TryFrom<BlueprintString> for CurrentBlueprint {
    type Error = BlueprintStringCorrupt;

    fn try_from(value: BlueprintString) -> Result<Self, Self::Error> {
        let raw: RawBlueprintStringData = value.try_into()?;

        let versioned = raw.get_versioned()?;

        let current = match versioned.version {
            0 => {
                let v0: v0::Blueprint = postcard::from_bytes(versioned.data)
                    .map_err(|_e| BlueprintStringCorrupt::CannotDeserialize)?;

                v0.into()
            },

            u32::MAX => {
                let old: old::Blueprint = todo!();

                old.into()
            },

            _ => return Err(BlueprintStringCorrupt::UnknownVersion(versioned.version)),
        };

        Ok(current)
    }
}

impl CurrentBlueprint {
    pub fn get_actions(&self) -> impl Iterator<Item = !> {
        vec![todo!()].into_iter()
    }
}
