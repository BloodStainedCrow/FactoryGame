mod old;
mod v0;

use std::u32;

use log::error;
pub use v0::Blueprint;

use crate::{
    action::ActionKind,
    blueprint::string::{BlueprintString, BlueprintStringCorrupt, RawBlueprintStringData},
};

type CurrentBlueprint = Blueprint;

pub trait VersionedBlueprint: Into<CurrentBlueprint> + for<'a> TryFrom<&'a [u8]> {
    fn get_version() -> u32;
}

impl TryFrom<&BlueprintString> for CurrentBlueprint {
    type Error = BlueprintStringCorrupt;

    fn try_from(value: &BlueprintString) -> Result<Self, Self::Error> {
        let raw: RawBlueprintStringData = value.clone().try_into()?;

        let versioned = raw.get_versioned()?;

        let current = match versioned.version {
            0 => {
                let v0: v0::Blueprint = v0::Blueprint::try_from(versioned.data)
                    .map_err(|_e| BlueprintStringCorrupt::CannotDeserialize)?;

                v0.into()
            },

            u32::MAX => {
                let old: old::Blueprint = old::Blueprint::try_from(versioned.data)?;

                old.into()
            },

            x => {
                error!("FIXME: Unknown blueprint version {x}. Using OLD as a stopgap!");

                let old: old::Blueprint = old::Blueprint::try_from(value.0.as_bytes())?;

                old.into()
            },

            _ => return Err(BlueprintStringCorrupt::UnknownVersion(versioned.version)),
        };

        Ok(current)
    }
}

impl CurrentBlueprint {
    pub fn get_actions(&self) -> impl Iterator<Item = &ActionKind> {
        self.actions.iter()
    }
}
