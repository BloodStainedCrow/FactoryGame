use std::{
    cmp::{max, min},
    ops::{Add, Sub},
};

use enum_map::Enum;

#[derive(Debug, Clone, Copy, PartialEq, Eq, serde::Deserialize, serde::Serialize)]
pub struct Position {
    pub x: i32,
    pub y: i32,
}

impl Sub for Position {
    type Output = Offset;

    fn sub(self, rhs: Self) -> Self::Output {
        Offset {
            x_offs: self.x - rhs.x,
            y_offs: self.y - rhs.y,
        }
    }
}

impl Position {
    #[must_use]
    pub const fn axis_aligned_distance_signed(self, other: Self) -> Option<i32> {
        let x_offs = self.x - other.x;
        let y_offs = self.y - other.y;

        if x_offs != 0 && y_offs != 0 {
            None
        } else if x_offs != 0 {
            Some(x_offs)
        } else if y_offs != 0 {
            Some(y_offs)
        } else {
            Some(0)
        }
    }

    #[must_use]
    pub const fn manhattan_distance(self, other: Self) -> u32 {
        self.x.abs_diff(other.x) + self.y.abs_diff(other.y)
    }

    #[must_use]
    pub fn pos_in_bounding_box_f64(self, bounding_box: BoundingBox) -> [f64; 2] {
        assert!(bounding_box.contains(self));

        [
            (f64::from(self.x) - f64::from(bounding_box.top_left().x))
                / f64::from(bounding_box.width()),
            (f64::from(self.y) - f64::from(bounding_box.top_left().y))
                / f64::from(bounding_box.height()),
        ]
    }
}

// TODO(BSC): Do I want to be able to support zero sized bounding boxes?
#[derive(Debug, Clone, Copy, serde::Deserialize, serde::Serialize)]
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

#[derive(Debug, Clone, Copy, PartialEq, Eq, Enum)]
pub enum Direction {
    North,
    East,
    South,
    West,
}

impl Direction {
    #[must_use]
    pub const fn rotate_right(self) -> Self {
        match self {
            Self::North => Self::East,
            Self::East => Self::South,
            Self::South => Self::West,
            Self::West => Self::North,
        }
    }

    #[must_use]
    pub const fn reverse(self) -> Self {
        match self {
            Self::North => Self::South,
            Self::East => Self::West,
            Self::South => Self::North,
            Self::West => Self::East,
        }
    }
}

impl From<Direction> for Offset {
    fn from(value: Direction) -> Self {
        match value {
            Direction::North => Self {
                x_offs: 0,
                y_offs: -1,
            },
            Direction::East => Self {
                x_offs: -1,
                y_offs: 0,
            },
            Direction::South => Self {
                x_offs: 0,
                y_offs: 1,
            },
            Direction::West => Self {
                x_offs: 1,
                y_offs: 0,
            },
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Enum)]
pub enum Rotation {
    North,
    East,
    South,
    West,
}

impl Rotation {
    #[must_use]
    pub const fn rotate_right(self) -> Self {
        match self {
            Self::North => Self::East,
            Self::East => Self::South,
            Self::South => Self::West,
            Self::West => Self::North,
        }
    }
}

#[derive(Debug, Clone, Copy, serde::Deserialize)]
pub struct Extent {
    pub width: u32,
    pub height: u32,
}

impl Extent {
    #[must_use]
    pub const fn single_tile() -> Self {
        Self {
            width: 1,
            height: 1,
        }
    }

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
            x: top_left.x.strict_add_unsigned(extent.width),
            y: top_left.y.strict_add_unsigned(extent.height),
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
    pub fn width(self) -> u32 {
        (self.bottom_right().x - self.top_left().x)
            .try_into()
            .expect("Bounding box borked")
    }

    #[must_use]
    pub fn height(self) -> u32 {
        (self.bottom_right().y - self.top_left().y)
            .try_into()
            .expect("Bounding box borked")
    }

    #[must_use]
    pub const fn overlaps(self, other: Self) -> bool {
        let self_right_of_other = self.top_left.x > other.bottom_right.x;
        let self_left_of_other = self.bottom_right.x < other.top_left.x;
        let self_above_other = self.bottom_right.y < other.top_left.y;
        let self_below_other = self.top_left.y > other.bottom_right.y;
        !(self_right_of_other || self_left_of_other || self_above_other || self_below_other)
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
    pub fn move_by(self, offset: Offset) -> Self {
        Self {
            top_left: self.top_left + offset,
            bottom_right: self.bottom_right + offset,
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
                x: self.bottom_right.x.strict_add_unsigned(amount),
                y: self.bottom_right.y.strict_add_unsigned(amount),
            },
        }
    }
}

#[derive(Debug, Clone, Copy, serde::Serialize, serde::Deserialize)]
pub struct Offset {
    pub x_offs: i32,
    pub y_offs: i32,
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
        prop_compose, prop_oneof,
    };

    use crate::spacial::{BoundingBox, Flipped, Position, Rotation};

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

    prop_compose! {
        pub fn random_position()(x in -1_000_000..1_000_000, y in -1_000_000..1_000_000) -> Position {
            Position { x, y }
        }
    }

    pub fn random_position_in(bounding_box: BoundingBox) -> impl Strategy<Value = Position> {
        (bounding_box.top_left().x..=bounding_box.bottom_right().x).prop_flat_map(move |x| {
            (bounding_box.top_left().y..=bounding_box.bottom_right().y)
                .prop_map(move |y| Position { x, y })
        })
    }

    prop_compose! {
        pub fn random_bounding_box()(corners in [random_position(), random_position()]) -> BoundingBox {
            BoundingBox::new_unordered(corners)
        }
    }

    prop_compose! {
        pub fn random_bounding_box_contained_in(outer: BoundingBox)(corners in [random_position_in(outer), random_position_in(outer)]) -> BoundingBox {
            BoundingBox::new_unordered(corners)
        }
    }
}

#[cfg(test)]
pub mod test {}
