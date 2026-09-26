use data::{
    entity::belt::BeltTy,
    spacial::{Flipped, Offset, Position, Rotation},
};
use entity_info::EntityDescriptorKind;
use enum_map::EnumMap;
use itertools::Either;
use middle::belt::SplitterSide;
use middle_indices::{BeltTileMiddleID, SplitterMiddleID};

use crate::surface::Surface;

struct BeltStateInfo {
    slf: BeltInfo,
    neighbors: EnumMap<Rotation, Option<BeltInfo>>,
}

#[derive(Debug, Clone, Copy)]
struct BeltInfo {
    rotation: Rotation,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum BeltResult {
    SideloadToSelf,
    ConnectToFront,
}

impl Surface {
    pub(crate) fn get_front_merge_belt(
        &self,
        _ty: BeltTy,
        position: Position,
        rotation: Rotation,
        _flipped: Flipped,
    ) -> Option<Either<BeltTileMiddleID, (SplitterMiddleID, SplitterSide)>> {
        let front_offset = Offset::north().rotate(rotation);

        let front_tile = position + front_offset;

        let entity = self.get_entity_at(front_tile)?;

        match entity.kind {
            EntityDescriptorKind::Belt { id } => {
                // FIXME: Make sure the front belt tile is rotated correctly and that sideloading rules are correct
                Some(Either::Left(id))
            },

            _ => None,
        }
    }

    pub(crate) fn get_back_belt_merge(
        &self,
        _ty: BeltTy,
        position: Position,
        rotation: Rotation,
        _flipped: Flipped,
    ) -> Option<Either<BeltTileMiddleID, (SplitterMiddleID, SplitterSide)>> {
        let back_offset = Offset::north().rotate(Rotation::South).rotate(rotation);

        let back_tile = position + back_offset;

        let entity = self.get_entity_at(back_tile)?;

        match entity.kind {
            EntityDescriptorKind::Belt { id } => {
                // FIXME: Make sure the back belt tile is rotated correctly
                Some(Either::Left(id))
            },

            _ => {
                // Look to the sides
                let left_offset = Offset::north().rotate(Rotation::East).rotate(rotation);
                let right_offset = Offset::north().rotate(Rotation::West).rotate(rotation);

                let left_entity = self
                    .get_entity_at(position + left_offset)
                    .map(|e| match e.kind {
                        EntityDescriptorKind::Belt { id } => {
                            // FIXME: Make sure the back belt tile is rotated correctly
                            Some(Either::Left(id))
                        },

                        _ => None,
                    })
                    .flatten();
                let right_entity = self
                    .get_entity_at(position + right_offset)
                    .map(|e| match e.kind {
                        EntityDescriptorKind::Belt { id } => {
                            // FIXME: Make sure the back belt tile is rotated correctly
                            Some(Either::Left(id))
                        },

                        _ => None,
                    })
                    .flatten();

                match (left_entity, right_entity) {
                    (None, None) => None,
                    (None, Some(v)) => Some(v),
                    (Some(v), None) => Some(v),
                    (Some(_), Some(_)) => {
                        todo!("Sideload")
                    },
                }
            },
        }
    }
}

// TODO: This needs some tests
fn get_state(mut info: BeltStateInfo) -> EnumMap<Rotation, Option<BeltResult>> {
    let mut rotations = 0;
    while info.slf.rotation != Rotation::North {
        info.slf.rotation = info.slf.rotation.rotate_right();
        info.neighbors.as_mut_array().rotate_right(1);
        rotations += 1;
    }
    let mut res = get_state_canonical(info.neighbors);
    res.as_mut_array().rotate_left(rotations);
    res
}

fn get_state_canonical(
    neighbors: EnumMap<Rotation, Option<BeltInfo>>,
) -> EnumMap<Rotation, Option<BeltResult>> {
    if let Some(bottom) = neighbors[Rotation::South]
        && bottom.rotation == Rotation::North
    {
        // We have a bottom facing into us. This means we are straight
        if let Some(left) = neighbors[Rotation::West]
            && left.rotation == Rotation::East
            && let Some(left) = neighbors[Rotation::East]
            && left.rotation == Rotation::West
        {
            // We are being double sideloaded
            EnumMap::from_array([
                None,
                Some(BeltResult::SideloadToSelf),
                Some(BeltResult::ConnectToFront),
                Some(BeltResult::SideloadToSelf),
            ])
        } else if let Some(left) = neighbors[Rotation::West]
            && left.rotation == Rotation::East
        {
            // We being sideloaded onto
            EnumMap::from_array([
                None,
                None,
                Some(BeltResult::ConnectToFront),
                Some(BeltResult::SideloadToSelf),
            ])
        } else if let Some(left) = neighbors[Rotation::East]
            && left.rotation == Rotation::West
        {
            // We being sideloaded onto
            EnumMap::from_array([
                None,
                Some(BeltResult::SideloadToSelf),
                Some(BeltResult::ConnectToFront),
                None,
            ])
        } else {
            EnumMap::from_array([None, None, Some(BeltResult::ConnectToFront), None])
        }
    } else {
        if let Some(left) = neighbors[Rotation::West]
            && left.rotation == Rotation::East
            && let Some(left) = neighbors[Rotation::East]
            && left.rotation == Rotation::West
        {
            // We are being double sideloaded
            EnumMap::from_array([
                None,
                Some(BeltResult::SideloadToSelf),
                None,
                Some(BeltResult::SideloadToSelf),
            ])
        } else if let Some(left) = neighbors[Rotation::West]
            && left.rotation == Rotation::East
        {
            // We are curved
            EnumMap::from_array([None, None, None, Some(BeltResult::ConnectToFront)])
        } else if let Some(left) = neighbors[Rotation::East]
            && left.rotation == Rotation::West
        {
            // We are curved
            EnumMap::from_array([None, Some(BeltResult::ConnectToFront), None, None])
        } else {
            // We are straight
            EnumMap::from_array([const { None }; 4])
        }
    }
}

#[cfg(test)]
mod test {
    use super::*;

    #[test]
    fn no_neighbors() {
        let res = get_state_canonical(EnumMap::from_array([const { None }; 4]));

        assert!(res.values().all(Option::is_none));
    }

    #[test]
    fn bottom_input_facing_away() {
        let res = get_state_canonical(EnumMap::from_array([
            None,
            None,
            Some(BeltInfo {
                rotation: Rotation::East,
            }),
            None,
        ]));

        assert!(res.values().all(Option::is_none));
    }

    #[test]
    fn bottom_input_facing_into_us() {
        let res = get_state_canonical(EnumMap::from_array([
            None,
            None,
            Some(BeltInfo {
                rotation: Rotation::North,
            }),
            None,
        ]));

        assert_eq!(
            res,
            EnumMap::from_array([None, None, Some(BeltResult::ConnectToFront), None])
        );
    }

    #[test]
    fn side_input_facing_away() {
        let res = get_state_canonical(EnumMap::from_array([
            None,
            Some(BeltInfo {
                rotation: Rotation::North,
            }),
            None,
            None,
        ]));

        assert!(res.values().all(Option::is_none));
    }

    #[test]
    fn side_input_facing_into_us() {
        let res = get_state_canonical(EnumMap::from_array([
            None,
            Some(BeltInfo {
                rotation: Rotation::West,
            }),
            None,
            None,
        ]));

        assert_eq!(
            res,
            EnumMap::from_array([None, Some(BeltResult::ConnectToFront), None, None])
        );
    }

    #[test]
    fn side_input_facing_away_with_bottom() {
        let res = get_state_canonical(EnumMap::from_array([
            None,
            Some(BeltInfo {
                rotation: Rotation::North,
            }),
            Some(BeltInfo {
                rotation: Rotation::North,
            }),
            None,
        ]));

        assert_eq!(
            res,
            EnumMap::from_array([None, None, Some(BeltResult::ConnectToFront), None])
        );
    }

    #[test]
    fn side_input_facing_into_us_with_bottom() {
        let res = get_state_canonical(EnumMap::from_array([
            None,
            Some(BeltInfo {
                rotation: Rotation::West,
            }),
            Some(BeltInfo {
                rotation: Rotation::North,
            }),
            None,
        ]));

        assert_eq!(
            res,
            EnumMap::from_array([
                None,
                Some(BeltResult::SideloadToSelf),
                Some(BeltResult::ConnectToFront),
                None
            ])
        );
    }
}
