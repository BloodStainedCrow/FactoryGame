mod sparse_grid;
pub mod tile;

// TODO: Do not use usize for anything that might go to another machine, where it could be different size!
#[derive(Debug, Clone, Copy, serde::Serialize, serde::Deserialize, PartialEq, Eq)]
pub struct Position {
    pub x: usize,
    pub y: usize,
}
