use data::entity::solar_panel::SolarPanelTy;

use crate::{Backend, power_grid::PowerGridBackendID};

pub struct SolarPanelAdditionInfo {
    pub ty: SolarPanelTy,
    pub grid: PowerGridBackendID,
}

impl Backend {
    pub fn add_solar_panel(&mut self, info: SolarPanelAdditionInfo) {
        self.power_grids[info.grid.0 as usize].solar_panel_counts[usize::from(info.ty)] += 1;
    }

    pub fn remove_solar_panel(&mut self, info: SolarPanelAdditionInfo) {
        self.power_grids[info.grid.0 as usize].solar_panel_counts[usize::from(info.ty)] -= 1;
    }
}
