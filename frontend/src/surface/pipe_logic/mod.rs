use std::{cmp::min, iter};

use data::spacial::{Direction, Position};
use middle::lists::PipeIndex;

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct PipeConnectionInfo {
    position: Position,
    underground_info: Option<UndergroundConnectionInfo>,
    direction: Direction,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
struct UndergroundConnectionInfo {
    range: u32,
}

// TODO: Tests
impl PipeConnectionInfo {
    pub fn can_connect(self, other: Self) -> bool {
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
                    distance.unsigned_abs()
                        <= min(self_underground.range, other_underground.range) + 1
                })
        } else {
            // Direct connection needed. The directions match
            (self.position + self.direction.into()) == other.position
        }
    }

    pub fn will_connect<I: Iterator<Item = Self>>(
        self,
        get_positions: impl Fn(Position) -> I,
        other: Self,
    ) -> bool {
        if !self.can_connect(other) {
            return false;
        }

        let other_conns_on_the_way = std::iter::successors(Some(self.position), |pos| {
            Some(*pos + self.direction.into())
        })
        .take_while(|pos| *pos != other.position)
        .flat_map(get_positions);

        for conn in other_conns_on_the_way {
            // The conns in question will only connect IF no other valid connections are in the way
            if self.can_connect(conn) || other.can_connect(conn) {
                return false;
            }
        }

        true
    }
}

pub struct NewPipeChange {
    new_connection: Option<[(PipeConnectionInfo, PipeIndex); 2]>,
    break_connection: Option<[(PipeConnectionInfo, PipeIndex); 2]>,
}

// TODO: Lots of tests
pub fn get_changes_after_adding<I: Iterator<Item = PipeConnectionInfo> + Clone>(
    info: PipeConnectionInfo,
    new_index: PipeIndex,
    get_positions: impl Fn(Position) -> (PipeIndex, I),
) -> NewPipeChange {
    let mut ret = NewPipeChange {
        new_connection: None,
        break_connection: None,
    };

    let other_conns_on_the_way = std::iter::successors(Some(info.position), |pos| {
        Some(*pos + info.direction.into())
    })
    .skip(1)
    .take(
        (info.underground_info.map_or(0, |info| info.range) + 1)
            .try_into()
            .expect("32 bit minimum"),
    )
    .flat_map(|pos| {
        let (index, v) = (get_positions)(pos);
        v.zip(iter::repeat(index))
    });

    for (conn, index) in other_conns_on_the_way.clone() {
        assert!(conn != info);
        if conn.can_connect(info) {
            ret.new_connection = Some([(info, new_index), (conn, index)]);
        }
    }

    if let Some([_, (other_conn, other_index)]) = ret.new_connection {
        // Check for breakage of the new connection

        let other_conns_on_the_way = std::iter::successors(Some(other_conn.position), |pos| {
            Some(*pos + other_conn.direction.into())
        })
        .skip(1)
        .take_while(|pos| *pos != info.position)
        .flat_map(|pos| {
            let (index, v) = (get_positions)(pos);
            v.zip(iter::repeat(index))
        });

        for (conn, index) in other_conns_on_the_way.clone() {
            assert!(conn != other_conn);
            if conn.can_connect(other_conn) {
                ret.break_connection = Some([(conn, index), (other_conn, other_index)]);
            }
        }
    }

    if info.underground_info.is_none() {
        assert!(
            ret.break_connection.is_none(),
            "Non-Underground cannot break connections"
        );
    }

    ret
}
