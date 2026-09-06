#[derive(Debug, Clone, Copy)]
pub struct PowerMultTimer(pub(crate) u64);
#[derive(Debug, Clone, Copy)]
pub struct PowerMult(u8);

const FULL_POWER: PowerMult = PowerMult(64);

pub enum AdvanceResult {
    Tick,
    NoTick,
}

impl PowerMultTimer {
    pub fn advance(&mut self, mult: PowerMult) -> AdvanceResult {
        let old = self.0;
        let new = old + u64::from(mult.0);

        self.0 = new;

        if new.div_floor(u64::from(FULL_POWER.0)) > old.div_floor(u64::from(FULL_POWER.0)) {
            AdvanceResult::Tick
        } else {
            AdvanceResult::NoTick
        }
    }
}
