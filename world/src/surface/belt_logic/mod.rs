use data::spacial::Rotation;
use enum_map::EnumMap;

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
