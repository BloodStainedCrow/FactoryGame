mod sparse_grid;
pub mod tile;

// TODO: Do not use usize for anything that might go to another machine, where it could be different size!
#[derive(
    Debug, Clone, Copy, serde::Serialize, serde::Deserialize, PartialEq, Eq, Hash, PartialOrd, Ord,
)]
pub struct Position {
    pub x: usize,
    pub y: usize,
}

impl Position {
    pub fn contained_in(self, other: Position, size: (u8, u8)) -> bool {
        self.x >= other.x
            && self.y >= other.y
            && self.x < other.x + usize::from(size.0)
            && self.y < other.y + usize::from(size.1)
    }

    pub fn contained_in_sized(self, self_size: (u8, u8), other: Position, size: (u8, u8)) -> bool {
        !((self.x + usize::from(self_size.0)) <= other.x
            || (self.y + usize::from(self_size.1)) <= other.y
            || (self.x) >= (other.x + usize::from(size.0))
            || (self.y) >= (other.y + usize::from(size.1)))
    }
}
