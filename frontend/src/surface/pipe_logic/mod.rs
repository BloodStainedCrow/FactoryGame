use std::cmp::min;

use data::spacial::{Direction, Position};

#[derive(Debug, Clone, Copy)]
struct PipeConnectionInfo {
    position: Position,
    underground_info: Option<UndergroundInfo>,
    direction: Direction,
}

#[derive(Debug, Clone, Copy)]
struct UndergroundInfo {
    range: u32,
}

impl PipeConnectionInfo {
    fn can_connect(self, other: Self) -> bool {
        assert!(self.position != other.position);

        if other.direction.reverse() != self.direction {
            return false;
        }

        if self.underground_info.is_some() != other.underground_info.is_some() {
            return false;
        }

        // For the connection to be aligned at least one of the axis must be equal
        if self.position.x != other.position.x && self.position.y != other.position.y {
            return false;
        }

        if let Some(self_underground) = &self.underground_info
            && let Some(other_underground) = &other.underground_info
        {
            self.position
                .axis_aligned_distance_signed(other.position)
                .is_some_and(|distance| {
                    // TODO: Check for off-by-one
                    distance.unsigned_abs() < min(self_underground.range, other_underground.range)
                })
        } else {
            // Direct connection needed. The directions match
            (self.position + self.direction.into()) == other.position
        }
    }
}
