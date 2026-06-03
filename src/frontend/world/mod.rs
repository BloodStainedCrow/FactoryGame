mod sparse_grid;
pub mod tile;

// TODO: Do not use usize for anything that might go to another machine, where it could be different size!
#[cfg_attr(
    feature = "show-info",
    derive(egui_show_info_derive::ShowInfo),
    derive(get_size2::GetSize)
)]
#[derive(
    Debug,
    Clone,
    Copy,
    Default,
    serde::Serialize,
    serde::Deserialize,
    PartialEq,
    Eq,
    Hash,
    PartialOrd,
    Ord,
)]
pub struct Position {
    pub x: i32,
    pub y: i32,
}

impl Position {
    pub fn contained_in(self, other: Position, size: (u16, u16)) -> bool {
        self.x >= other.x
            && self.y >= other.y
            && self.x < other.x + i32::from(size.0)
            && self.y < other.y + i32::from(size.1)
    }

    pub fn overlap(self, self_size: (u16, u16), other: Position, size: (u16, u16)) -> bool {
        !((self.x + i32::from(self_size.0)) <= other.x
            || (self.y + i32::from(self_size.1)) <= other.y
            || (self.x) >= (other.x + i32::from(size.0))
            || (self.y) >= (other.y + i32::from(size.1)))
    }

    pub fn self_minus_other(
        self,
        self_size: (u16, u16),
        other: Position,
        size: (u16, u16),
    ) -> [(Self, (u16, u16)); 4] {
        let sx1 = self.x;
        let sy1 = self.y;
        let sx2 = sx1 + self_size.0 as i32;
        let sy2 = sy1 + self_size.1 as i32;

        let ox1 = other.x;
        let oy1 = other.y;
        let ox2 = ox1 + size.0 as i32;
        let oy2 = oy1 + size.1 as i32;

        // Intersection
        let ix1 = sx1.max(ox1);
        let iy1 = sy1.max(oy1);
        let ix2 = sx2.min(ox2);
        let iy2 = sy2.min(oy2);

        // No overlap
        if ix1 >= ix2 || iy1 >= iy2 {
            assert!(!self.overlap(self_size, other, size));

            return [
                (self, self_size),
                (self, (0, 0)),
                (self, (0, 0)),
                (self, (0, 0)),
            ];
        }

        assert!(self.overlap(self_size, other, size));

        [
            // Top strip
            (
                Position { x: sx1, y: sy1 },
                (self_size.0, (iy1.strict_sub(sy1)) as u16),
            ),
            // Bottom strip
            (
                Position { x: sx1, y: iy2 },
                (self_size.0, (sy2.strict_sub(iy2)) as u16),
            ),
            // Left strip
            (
                Position { x: sx1, y: iy1 },
                ((ix1.strict_sub(sx1)) as u16, (iy2.strict_sub(iy1)) as u16),
            ),
            // Right strip
            (
                Position { x: ix2, y: iy1 },
                ((sx2.strict_sub(ix2)) as u16, (iy2.strict_sub(iy1)) as u16),
            ),
        ]
    }
}

#[cfg(test)]
mod test {
    use std::cmp::max;

    use crate::blueprint::test::random_position;
    use itertools::Itertools;
    use proptest::{prop_assert, prop_assert_eq, prop_assume, proptest};

    proptest! {
        // #![proptest_config(ProptestConfig{ cases: 1_000, max_global_rejects: 100_000, ..Default::default()})]
        #[test]
        fn position_contained_in_itself(position in random_position(), size in (1u16..100, 1u16..100)) {
            prop_assert!(position.contained_in(position, size));
        }

        #[test]
        fn position_contained_in(position in random_position(), test_position in random_position(), size in (1u16..10, 1u16..10)) {
            prop_assume!(test_position.x >= position.x);
            prop_assume!(test_position.y >= position.y);
            prop_assume!(test_position.x < position.x + size.0 as i32);
            prop_assume!(test_position.y < position.y + size.1 as i32);

            prop_assert!(test_position.contained_in(position, size));
        }

        #[test]
        fn position_not_contained_in(position in random_position(), test_position in random_position(), size in (1u16..10, 1u16..10)) {
            prop_assume!(test_position.x < position.x || test_position.y < position.y || test_position.x >= position.x + size.0 as i32 || test_position.y >= position.y + size.1 as i32);

            prop_assert!(!test_position.contained_in(position, size));
        }

        #[test]
        fn position_contained_in_itself_sized(position in random_position(), self_size in (1u16..100, 1u16..100), other_size in  (1u16..100, 1u16..100)) {
            prop_assert!(position.overlap(self_size, position, other_size));
        }

        #[test]
        fn position_contained_in_sized(position in random_position(), test_position in random_position(), self_size in  (1u16..100, 1u16..100), other_size in  (1u16..100, 1u16..100)) {
            prop_assume!(test_position.x + self_size.0 as i32 >= position.x);
            prop_assume!(test_position.y + self_size.1 as i32 >= position.y);
            prop_assume!(test_position.x < position.x + other_size.0 as i32);
            prop_assume!(test_position.y < position.y + other_size.1 as i32);

            prop_assert!(position.overlap(self_size, position, other_size));
        }


        #[test]
        fn super_stupid_test_unsized(position in random_position(), test_position in random_position(), size in (1u16..10, 1u16..10)) {
            prop_assume!(position.x >= 0 && position.y >= 0);

            let grid_size = max(position.x + size.0 as i32, max(test_position.x, max(position.y + size.1 as i32, test_position.y))) + 1;

            let mut grid = vec![vec![false; grid_size.try_into().unwrap()]; grid_size.try_into().unwrap()];

            for x_offs in 0..size.0 {
                for y_offs in 0..size.1 {
                    grid[(position.x + x_offs as i32) as usize][(position.y + y_offs as i32) as usize] = true;
                }
            }

            prop_assert_eq!(grid[test_position.x as usize][test_position.y as usize], test_position.contained_in(position, size))
        }

        #[test]
        fn super_stupid_test_unsized_sized(position in random_position(), test_position in random_position(), size in (1u16..3, 1u16..3), test_size in (1u16..3, 1u16..3)) {
            prop_assume!(position.x >= 0 && position.y >= 0);

            let grid_size = max(position.x as usize + size.0 as usize, max(test_position.x as usize + test_size.0 as usize, max(position.y as usize + size.1 as usize, test_position.y as usize + test_size.1 as usize))) + 1;

            let mut grid = vec![vec![false; grid_size]; grid_size];

            for x_offs in 0..size.0 {
                for y_offs in 0..size.1 {
                    grid[position.x as usize + x_offs as usize][position.y as usize + y_offs as usize] = true;
                }
            }

            let mut hit = false;
            for x_offs in 0..test_size.0 {
                for y_offs in 0..test_size.1 {
                    hit |= grid[test_position.x as usize + x_offs as usize][test_position.y as usize + y_offs as usize];
                }
            }
            prop_assert_eq!(hit, test_position.overlap(test_size, position, size), "grid_size: {:?}, grid: {:?}", grid_size, grid[1600..].iter().map(|v| &v[1600..]).collect_vec());
        }

    }
}
