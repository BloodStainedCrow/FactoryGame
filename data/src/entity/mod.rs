use crate::{
    DATA_STORE, EntityIdentifier,
    spacial::{BoundingBox, Extent, Flipped, Position, Rotation},
};

pub mod assember;
pub mod beacon;
pub mod belt;
pub mod chest;
pub mod inserter;
pub mod power_pole;

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct GlobalTy(u16);

impl From<GlobalTy> for usize {
    fn from(value: GlobalTy) -> Self {
        Self::from(value.0)
    }
}

// FIXME: I do not like this existing much
impl From<u16> for GlobalTy {
    fn from(value: u16) -> Self {
        Self(value)
    }
}

#[derive(Debug, Clone, serde::Deserialize)]
pub struct PlacementRules {
    cnf: Vec<Vec<PlacementRule>>,
}

impl PlacementRules {
    #[must_use]
    pub const fn no_restriction() -> Self {
        Self { cnf: vec![] }
    }

    fn eval(&self, floor: !) -> bool {
        // This is the cnf eval
        self.cnf
            .iter()
            .all(|inner| inner.iter().any(|rule| rule.eval(floor)))
    }
}

#[derive(Debug, Clone, serde::Deserialize)]
enum PlacementRule {
    FloorTileRequired { area: BoundingBox, tile: () },
    Not(Box<Self>),
}

impl PlacementRule {
    fn eval(&self, floor: !) -> bool {
        match self {
            Self::FloorTileRequired { area, tile } => {
                // TODO:
                true
            },
            Self::Not(placement_rule) => !placement_rule.eval(floor),
        }
    }
}

#[must_use]
pub fn allows_rotation(entity_id: GlobalTy) -> bool {
    DATA_STORE.entities[usize::from(entity_id)].can_be_rotated
}

#[must_use]
pub fn allows_flipping(entity_id: GlobalTy) -> bool {
    DATA_STORE.entities[usize::from(entity_id)].can_be_flipped
}

#[must_use]
pub fn placement_allowed(entity_id: GlobalTy, floor: !) -> bool {
    DATA_STORE.entities[usize::from(entity_id)]
        .placement_rules
        .eval(floor)
}

#[must_use]
pub fn bounding_box(
    entity_id: GlobalTy,
    top_left: Position,
    rotation: Rotation,
    flipped: Flipped,
) -> BoundingBox {
    BoundingBox::new(top_left, extent(entity_id, rotation, flipped))
}

#[must_use]
pub fn extent(entity_id: GlobalTy, rotation: Rotation, flipped: Flipped) -> Extent {
    // TODO: Is flipped being unused correct?
    DATA_STORE.entities[usize::from(entity_id)]
        .size
        .rotate(rotation)
}

impl From<GlobalTy> for EntityIdentifier {
    fn from(value: GlobalTy) -> Self {
        DATA_STORE.entities[usize::from(value)].name.clone()
    }
}

impl TryFrom<EntityIdentifier> for GlobalTy {
    type Error = String;

    fn try_from(value: EntityIdentifier) -> Result<Self, Self::Error> {
        DATA_STORE
            .entities
            .iter()
            .enumerate()
            .find_map(|(idx, e)| {
                (e.name == value).then_some(
                    u16::try_from(idx)
                        .expect("More that u16::MAX entities")
                        .into(),
                )
            })
            .ok_or_else(|| {
                format!(
                    "Cannot find entity: {value:?} in {:?}",
                    &DATA_STORE.entities
                )
            })
    }
}
