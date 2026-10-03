use backend::{
    Backend, power_grid::solar_panel::SolarPanelAdditionInfo as BackendSolarPanelAdditionInfo,
};
use data::entity::solar_panel::SolarPanelTy;
use middle_indices::PowerPoleMiddleID;

use crate::{Middle, UNATTACHED_POWER_GRID_ID};

pub struct SolarPanelAdditionInfo {
    pub ty: SolarPanelTy,
    pub pole: Option<PowerPoleMiddleID>,
}

impl Middle {
    pub fn add_solar_panel(&mut self, info: SolarPanelAdditionInfo, backend: &mut Backend) {
        let grid = match info.pole {
            Some(pole) => self.add_solar_panel_to_pole(pole, info.ty),
            None => UNATTACHED_POWER_GRID_ID,
        };

        let grid = self.power_grid_list[grid.0 as usize].backend_id;

        backend.add_solar_panel(BackendSolarPanelAdditionInfo { ty: info.ty, grid });
    }

    pub fn remove_solar_panel(&mut self, info: SolarPanelAdditionInfo, backend: &mut Backend) {
        let grid = match info.pole {
            Some(pole) => self.remove_solar_panel_from_pole(pole, info.ty),
            None => UNATTACHED_POWER_GRID_ID,
        };

        let grid = self.power_grid_list[grid.0 as usize].backend_id;

        backend.remove_solar_panel(BackendSolarPanelAdditionInfo { ty: info.ty, grid });
    }
}
