use std::{
    cmp::{max, min},
    ops::Add,
};

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct Position {
    pub x: i32,
    pub y: i32,
}

// TODO(BSC): Do I want to be able to support zero sized bounding boxes?
#[derive(Debug, Clone, Copy)]
pub struct BoundingBox {
    // Inclusive
    top_left: Position,
    // Inclusive
    bottom_right: Position,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct Flipped {
    pub horizontally: bool,
    pub vertically: bool,
}

impl Flipped {
    #[must_use]
    pub const fn unflipped() -> Self {
        Self {
            horizontally: false,
            vertically: false,
        }
    }

    #[must_use]
    pub const fn horizontal() -> Self {
        Self {
            horizontally: true,
            vertically: false,
        }
    }

    #[must_use]
    pub const fn vertical() -> Self {
        Self {
            horizontally: false,
            vertically: true,
        }
    }

    #[must_use]
    pub const fn both() -> Self {
        Self {
            horizontally: true,
            vertically: true,
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Rotation {
    North,
    East,
    South,
    West,
}

#[derive(Debug, Clone, Copy, serde::Deserialize)]
pub struct Extent {
    pub width: i32,
    pub height: i32,
}

impl Extent {
    #[must_use]
    pub const fn rotate(self, rotation: Rotation) -> Self {
        match rotation {
            Rotation::North | Rotation::South => self,
            Rotation::East | Rotation::West => Self {
                width: self.height,
                height: self.width,
            },
        }
    }
}

impl BoundingBox {
    #[must_use]
    pub const fn new(top_left: Position, extent: Extent) -> Self {
        let bottom_right = Position {
            x: top_left.x + extent.width,
            y: top_left.y + extent.height,
        };

        Self {
            top_left,
            bottom_right,
        }
    }

    /// # Errors
    /// If `top_left` is not top left of `bottom_right`
    pub const fn try_new(top_left: Position, bottom_right: Position) -> Result<Self, ()> {
        if top_left.x > bottom_right.x || top_left.y > bottom_right.y {
            return Err(());
        }

        Ok(Self {
            top_left,
            bottom_right,
        })
    }

    #[must_use]
    pub fn new_unordered(corners: [Position; 2]) -> Self {
        let [a, b] = corners;

        let top_left = Position {
            x: min(a.x, b.x),
            y: min(a.y, b.y),
        };

        let bottom_right = Position {
            x: max(a.x, b.x),
            y: max(a.y, b.y),
        };

        Self {
            top_left,
            bottom_right,
        }
    }

    #[must_use]
    pub const fn top_left(self) -> Position {
        self.top_left
    }

    #[must_use]
    pub const fn bottom_right(self) -> Position {
        self.bottom_right
    }

    #[must_use]
    pub fn overlaps(self, other: Self) -> bool {
        todo!()
    }

    #[must_use]
    pub const fn contains(self, position: Position) -> bool {
        position.x >= self.top_left.x
            && position.x <= self.bottom_right.x
            && position.y >= self.top_left.y
            && position.y <= self.bottom_right.y
    }

    #[must_use]
    pub const fn move_to(self, new_top_left: Position) -> Self {
        Self {
            top_left: new_top_left,
            bottom_right: Position {
                x: self.bottom_right.x - self.top_left.x + new_top_left.x,
                y: self.bottom_right.y - self.top_left.y + new_top_left.y,
            },
        }
    }

    #[must_use]
    pub const fn extend_evenly(self, amount: u32) -> Self {
        Self {
            top_left: Position {
                x: self.top_left.x.strict_sub_unsigned(amount),
                y: self.top_left.y.strict_sub_unsigned(amount),
            },
            bottom_right: Position {
                x: self.top_left.x.strict_add_unsigned(amount),
                y: self.top_left.y.strict_add_unsigned(amount),
            },
        }
    }
}

#[derive(Debug, Clone, Copy)]
pub struct Offset {
    x_offs: i32,
    y_offs: i32,
}

impl Add<Offset> for Position {
    type Output = Self;

    fn add(self, rhs: Offset) -> Self::Output {
        Self {
            x: self.x + rhs.x_offs,
            y: self.y + rhs.y_offs,
        }
    }
}

#[cfg(feature = "test")]
pub mod strategies {
    use proptest::{
        prelude::{Just, Strategy},
        prop_oneof,
    };

    use crate::spacial::{Flipped, Rotation};

    pub fn random_rotation() -> impl Strategy<Value = Rotation> {
        prop_oneof![
            Just(Rotation::North),
            Just(Rotation::East),
            Just(Rotation::South),
            Just(Rotation::West),
        ]
    }

    pub fn random_flipping() -> impl Strategy<Value = Flipped> {
        prop_oneof![
            Just(Flipped::unflipped()),
            Just(Flipped::horizontal()),
            Just(Flipped::vertical()),
            Just(Flipped::both()),
        ]
    }
}

#[cfg(test)]
pub mod test {}
