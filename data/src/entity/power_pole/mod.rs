use crate::{
    entity::GlobalTy,
    spacial::{BoundingBox, Flipped, Position, Rotation},
};

#[derive(Debug, Clone, Copy)]
pub struct PowerPoleTy(u16);

impl From<PowerPoleTy> for GlobalTy {
    fn from(value: PowerPoleTy) -> Self {
        todo!()
    }
}

impl TryFrom<GlobalTy> for PowerPoleTy {
    type Error = ();

    fn try_from(value: GlobalTy) -> Result<Self, Self::Error> {
        todo!()
    }
}

#[must_use]
pub fn power_pole_connection_area(
    ty: PowerPoleTy,
    top_left: Position,
    rotation: Rotation,
    flipped: Flipped,
) -> BoundingBox {
    todo!()
}
