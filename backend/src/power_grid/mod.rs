use crate::Backend;

pub mod addition;
mod merge;
mod update;

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct PowerGridBackendID(usize);

#[derive(Debug, Clone)]
pub(super) struct PowerGrid {}

#[derive(Debug, PartialEq, Eq, PartialOrd, Ord)]
pub struct PowerGridSize(usize);

impl Backend {
    pub fn get_power_grid_size(&self, id: PowerGridBackendID) -> PowerGridSize {
        // TODO:
        PowerGridSize(0)
    }
}
