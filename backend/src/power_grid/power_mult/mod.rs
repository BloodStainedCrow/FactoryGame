#[derive(Debug, Clone, Copy)]
pub struct PowerMultTimer(u8);
#[derive(Debug, Clone, Copy)]
pub struct PowerMult(u8);

const FULL_POWER: PowerMult = PowerMult(64);

pub enum AdvanceResult {
    Tick,
    NoTick,
}

impl PowerMultTimer {
    pub fn advance(&mut self, mult: PowerMult) -> AdvanceResult {
        let new = self.0 + mult.0;

        self.0 = new % FULL_POWER.0;

        if new >= FULL_POWER.0 {
            AdvanceResult::Tick
        } else {
            AdvanceResult::NoTick
        }
    }
}
