use crate::{
    TICKS_PER_SECOND_LOGIC_F32, api::entity::EntityInfo, energy::EnergySource, spacial::Offset,
};

pub type InserterMovetime = u16;

#[derive(Debug, serde::Deserialize)]
pub struct InserterInfo {
    pub entity_info: EntityInfo,

    pub source_offset: Offset,
    pub dest_offset: Offset,
    pub movetime: InserterMovementTime,

    // TODO: near far things
    pub energy_source: EnergySource,

    pub filter_count: u8,

    // Inherent hand size bonus
    pub hand_size_bonus: u8,
}

#[derive(Debug, Clone, Copy, serde::Serialize, serde::Deserialize)]
enum InserterMovementTime {
    TicksPerTrip(InserterMovetime),
    RotationPerSecond { degrees: f32 },
}

impl InserterMovementTime {
    fn into_trip_time(self) -> InserterMovetime {
        match self {
            Self::TicksPerTrip(v) => v,

            #[expect(clippy::cast_possible_truncation)]
            #[expect(clippy::cast_sign_loss)]
            Self::RotationPerSecond { degrees } => {
                assert!(degrees >= 0.0);

                let val = 360.0 / degrees * TICKS_PER_SECOND_LOGIC_F32;

                assert!(val <= f32::from(InserterMovetime::MAX));

                val as InserterMovetime
            },
        }
    }
}
