use crate::{
    DATA_STORE, EntityPrototypeKind,
    entity::GlobalTy,
    spacial::{BoundingBox, Extent, Flipped, Offset, Position, Rotation},
};

#[derive(Debug)]
pub struct PowerPoleData {
    pub wire_connection_offset: Offset,
    pub wire_connection_area: Extent,
}

#[derive(Debug, Clone, Copy)]
pub struct PowerPoleTy(u16);

impl From<PowerPoleTy> for GlobalTy {
    fn from(value: PowerPoleTy) -> Self {
        Self(
            EntityPrototypeKind::PowerPole
                .global_index_for_kind_index(value.0 as usize)
                .expect("Illegal PowerPoleTy")
                .try_into()
                .expect("More than u16::MAX entities"),
        )
    }
}

impl TryFrom<GlobalTy> for PowerPoleTy {
    type Error = ();

    fn try_from(value: GlobalTy) -> Result<Self, Self::Error> {
        EntityPrototypeKind::PowerPole
            .kind_index_from_global_index(value.0 as usize)
            .map(|idx| Self(idx.try_into().expect("More than u16::MAX entities")))
            .ok_or(())
    }
}

/// The are in which other poles can connect via copper wire
#[must_use]
pub fn power_pole_wire_connection_area(
    ty: PowerPoleTy,
    top_left: Position,
    rotation: Rotation,
    _flipped: Flipped,
) -> BoundingBox {
    let data = &DATA_STORE.power_poles[usize::from(ty.0)];
    BoundingBox::new(
        top_left + data.wire_connection_offset,
        data.wire_connection_area.rotate(rotation),
    )
}
