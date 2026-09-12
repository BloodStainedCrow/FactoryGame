use std::sync::Arc;

use crate::{ModIdentifier, item::ItemCountType};

#[derive(Debug, Clone, PartialEq, Eq, Hash, serde::Deserialize)]
pub struct ItemIdentifier(Arc<str>);

#[derive(Debug, Clone, serde::Deserialize)]
pub struct ItemName(pub(crate) String);

impl ItemIdentifier {
    pub(crate) fn new(mod_: &ModIdentifier, name: &ItemName) -> Self {
        Self(format!("{}::{}", mod_.0, name.0).into())
    }

    // TODO
    #[must_use]
    pub fn new_raw(full_name: String) -> Self {
        Self(full_name.into())
    }
}

#[derive(Debug, Clone, serde::Deserialize)]
pub struct ItemInfo {
    pub name: ItemName,

    pub stack_size: ItemCountType,
}
