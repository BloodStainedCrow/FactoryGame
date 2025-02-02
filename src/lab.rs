use bytemuck::TransparentWrapper;

use crate::{
    item::{Iron, ItemStorage},
    power::{Joule, Watt, MAX_POWER_MULT},
    research::Technology,
};

#[derive(Debug, Clone, serde::Deserialize, serde::Serialize)]
pub struct MultiLabStore {
    // red: ItemStorage<RedScience>,
    // green: ItemStorage<GreenScience>,
    // military: ItemStorage<MilitaryScience>,
    // purple: ItemStorage<PurpleScience>,
    // yellow: ItemStorage<YellowScience>,
    // space: ItemStorage<SpaceScience>,
    red: Vec<ItemStorage<Iron>>,
    green: Vec<ItemStorage<Iron>>,
    military: Vec<ItemStorage<Iron>>,
    purple: Vec<ItemStorage<Iron>>,
    yellow: Vec<ItemStorage<Iron>>,
    space: Vec<ItemStorage<Iron>>,
    timer: Vec<u16>,
}

impl MultiLabStore {
    #[must_use]
    const fn new() -> Self {
        Self {
            red: vec![],
            green: vec![],
            military: vec![],
            purple: vec![],
            yellow: vec![],
            space: vec![],
            timer: vec![],
        }
    }

    #[inline(never)]
    // TODO: The generated assembly is REALLY weird. Investigate
    pub fn update(&mut self, power_mult: u8, current_research: &Technology) -> (Joule, u16) {
        const POWER_CONSUMPTION: Watt = Watt(600);
        const TICKS_PER_SCIENCE: u16 = 1800;

        assert_eq!(self.red.len(), self.green.len());
        assert_eq!(self.red.len(), self.military.len());
        assert_eq!(self.red.len(), self.purple.len());
        assert_eq!(self.red.len(), self.yellow.len());
        assert_eq!(self.red.len(), self.space.len());
        assert_eq!(self.red.len(), self.timer.len());

        let mut running = 0;
        let increase =
            (u16::from(power_mult) * (u16::MAX / u16::from(MAX_POWER_MULT))) / TICKS_PER_SCIENCE;

        let r_min = u16::from(current_research.cost[0] > 0);
        let g_min = u16::from(current_research.cost[1] > 0);
        let m_min = u16::from(current_research.cost[2] > 0);
        let p_min = u16::from(current_research.cost[3] > 0);
        let y_min = u16::from(current_research.cost[4] > 0);
        let s_min = u16::from(current_research.cost[5] > 0);

        for ((((((r, g), m), p), y), s), timer) in self
            .red
            .iter_mut()
            .zip(self.green.iter_mut())
            .zip(self.military.iter_mut())
            .zip(self.purple.iter_mut())
            .zip(self.yellow.iter_mut())
            .zip(self.space.iter_mut())
            .zip(self.timer.iter_mut())
        {
            let ing_mul = u16::from(*ItemStorage::<Iron>::peel_mut(r) >= r_min)
                * u16::from(*ItemStorage::<Iron>::peel_mut(g) >= g_min)
                * u16::from(*ItemStorage::<Iron>::peel_mut(m) >= m_min)
                * u16::from(*ItemStorage::<Iron>::peel_mut(p) >= p_min)
                * u16::from(*ItemStorage::<Iron>::peel_mut(y) >= y_min)
                * u16::from(*ItemStorage::<Iron>::peel_mut(s) >= s_min);
            let new_timer = timer.wrapping_add(increase * ing_mul);

            let timer_mul = u16::from(*timer < new_timer);
            let work_done_mul = u16::from(*timer != new_timer);

            // Power calculation
            // We use power if any work was done
            running += work_done_mul;

            *ItemStorage::<Iron>::peel_mut(r) -= r_min * timer_mul;
            *ItemStorage::<Iron>::peel_mut(g) -= g_min * timer_mul;
            *ItemStorage::<Iron>::peel_mut(m) -= m_min * timer_mul;
            *ItemStorage::<Iron>::peel_mut(p) -= p_min * timer_mul;
            *ItemStorage::<Iron>::peel_mut(y) -= y_min * timer_mul;
            *ItemStorage::<Iron>::peel_mut(s) -= s_min * timer_mul;
        }

        (
            POWER_CONSUMPTION.joules_per_tick() * running.into(),
            running,
        )
    }
}

impl Default for MultiLabStore {
    fn default() -> Self {
        Self::new()
    }
}
