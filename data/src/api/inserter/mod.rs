use crate::{EntityInfo, TICKS_PER_SECOND_LOGIC_F32, energy::EnergySource, spacial::Offset};

pub type InserterMovetime = u16;

struct InserterInfo {
    entity_info: EntityInfo,

    source_offset: Offset,
    dest_offset: Offset,
    movetime: InserterMovementTime,

    // TODO: near far things
    energy_source: EnergySource,

    filter_count: u8,

    // Inherent hand size bonus
    hand_size_bonus: u8,
}

#[derive(Debug, Clone, Copy)]
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
