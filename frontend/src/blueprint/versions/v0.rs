use crate::{action::ActionKind, blueprint::versions::VersionedBlueprint};

#[derive(Debug, Clone, PartialEq, serde::Serialize, serde::Deserialize)]
pub struct Blueprint {
    pub(super) actions: Vec<ActionKind>,
}

impl VersionedBlueprint for Blueprint {
    fn get_version() -> u32 {
        0
    }
}

// TODO: Remove at some point and replace with reordering get_actions
impl From<Vec<ActionKind>> for Blueprint {
    fn from(actions: Vec<ActionKind>) -> Self {
        Self { actions }
    }
}

impl<'a> TryFrom<&'a [u8]> for Blueprint {
    type Error = bincode::error::DecodeError;

    fn try_from(data: &'a [u8]) -> Result<Self, Self::Error> {
        let (slf, len) = bincode::serde::decode_from_slice(data, bincode::config::standard())?;

        Ok(slf)
    }
}
