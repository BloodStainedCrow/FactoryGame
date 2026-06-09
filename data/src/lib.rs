use crate::spacial::{Extent, Rotation};

pub const TICKS_PER_SECOND_LOGIC: usize = 60;
#[expect(clippy::cast_precision_loss)]
pub const TICKS_PER_SECOND_LOGIC_F32: f32 = TICKS_PER_SECOND_LOGIC as f32;

pub mod assember;
pub mod energy;
pub mod inserter;
pub mod spacial;

#[derive(Debug, serde::Deserialize)]
struct EntityInfo {
    // TODO: Add bounding box types
    size: Extent,

    #[serde(default)]
    can_be_rotated: bool,
    #[serde(default)]
    can_be_flipped: bool,

    name: String,
    // FIXME(BSC): localisation support!
    display_name: String,
    // TODO: Icon, Collision, BuildRules, Sound, Placement (i.e. which item places it), mapcolor
}

pub struct DataStore {}

/// The parsed data of the currently loaded mod set
static mut DATA_STORE: DataStore = DataStore {};

/// # Safety
/// The caller is responsible that no reads are currently happening
/// and that no references to the `DATA_STORE` are currently live.
/// This is easiest to ensure by stopping any active update loops (by stopping simulations or the running game)
pub unsafe fn set_data(data_store: DataStore) {
    unsafe {
        DATA_STORE = data_store;
    }
}

#[must_use]
pub fn get_entity_extent(entity_ty: u16, entity_rotation: Rotation) -> Extent {
    Extent {
        width: todo!(),
        height: todo!(),
    }
    .rotate(entity_rotation)
}

#[expect(clippy::missing_const_for_fn)]
#[must_use]
pub fn max_entity_size() -> u32 {
    10
}

#[cfg(test)]
mod tests {}
