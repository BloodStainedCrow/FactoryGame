use smallvec::SmallVec;

use crate::{Middle, lists::PowerPoleIndex};

pub const AUTOMATIC_POLE_CONNECTION_LIMIT: usize = 4;

pub(crate) struct MiddlePowerPoleInfo {
    connections: SmallVec<[PowerPoleIndex; AUTOMATIC_POLE_CONNECTION_LIMIT]>,
    // grid_id: !,

    // TODO: Do I want this in here?
    // connected_entities: Vec<!>
}

pub struct PowerPoleAdditionInfo {
    pub connections: SmallVec<[PowerPoleIndex; AUTOMATIC_POLE_CONNECTION_LIMIT]>,
    // connected_entities: Vec<!>
}

impl Middle {
    // TODO: All additional info
    pub fn add_power_pole(
        &mut self,
        info: PowerPoleAdditionInfo,
        backend: &mut !,
    ) -> PowerPoleIndex {
        let PowerPoleAdditionInfo { connections } = info;

        let index = self
            .power_pole_list
            .push(MiddlePowerPoleInfo { connections });

        PowerPoleIndex(index.try_into().expect("More than u32::MAX assemblers"))
    }

    pub fn remove_power_pole(&mut self, id: PowerPoleIndex, backend: &mut !) {
        todo!()
    }
}
