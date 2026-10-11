const MAX_POWER_MULT: PowerMult = PowerMult(64);

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub(super) struct PowerMult(u8);

#[derive(Debug, Clone, Copy, PartialEq, Eq, Default)]
pub(super) struct MonotonicPowerTick(u64);
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub(super) struct RealTick(u16);

pub(super) struct Tick;
pub(super) struct NoTick;

impl MonotonicPowerTick {
    pub fn advance(&mut self, amount: PowerMult) -> Result<Tick, NoTick> {
        let old = self.0 / u64::from(MAX_POWER_MULT.0);
        self.0 += u64::from(amount.0);
        let new = self.0 / u64::from(MAX_POWER_MULT.0);

        assert!(new - old <= 1);

        if new > old { Ok(Tick) } else { Err(NoTick) }
    }

    pub fn get_current_real(&self) -> RealTick {
        RealTick((self.0 / u64::from(MAX_POWER_MULT.0)) as u16)
    }

    pub fn time_since(&self, old: RealTick) -> u16 {
        let current = self.get_current_real();

        match current.0.checked_sub(old.0) {
            Some(diff) => diff,
            None => {
                // We wrapped
                (u32::from(current.0) + (1 << 16) - u32::from(old.0))
                    .try_into()
                    .unwrap()
            },
        }
    }
}

#[cfg(test)]
mod test {
    use std::cmp::min;

    use super::*;
    use proptest::{prop_assert, prop_assert_eq, proptest};

    proptest! {
        #[test]
        fn always_ticks_at_full_power(start_tick in 0..(u32::MAX as u64)) {
            let mut tick = MonotonicPowerTick(start_tick);

            let res = tick.advance(MAX_POWER_MULT);

            prop_assert!(res.is_ok())
        }

        #[test]
        fn never_ticks_at_zero_power(start_tick in 0..(u32::MAX as u64)) {
            let mut tick = MonotonicPowerTick(start_tick);

            let res = tick.advance(PowerMult(0));

            prop_assert!(res.is_err())
        }

        #[test]
        fn real_ticks_slower(start_tick in 0..(u32::MAX as u64)) {
            let tick = MonotonicPowerTick(start_tick);

            let real_tick = tick.get_current_real();

            prop_assert!(real_tick.0 as u64 <= start_tick)
        }

        #[test]
        fn time_since_does_not_crash(start_tick in 0..(u32::MAX as u64), advance in 0..=MAX_POWER_MULT.0) {
            let mut tick = MonotonicPowerTick(start_tick);
            let real_tick = tick.get_current_real();

            let _ = tick.advance(PowerMult(advance));

            tick.time_since(real_tick);
        }

        #[test]
        fn time_since_increases(start_tick in 0..(u32::MAX as u64), mut advance in 0..=((u16::MAX) as u32 * MAX_POWER_MULT.0 as u32)) {
            let mut tick = MonotonicPowerTick(start_tick);
            let real_tick = tick.get_current_real();

            let mut real_ticks = 0;
            while advance > 0 {
                let amount = PowerMult(min(MAX_POWER_MULT.0, advance.try_into().unwrap_or(u8::MAX)));
                if tick.advance(amount).is_ok() {
                    real_ticks += 1;
                }
                advance -= u32::from(amount.0);
            }

            let passed = tick.time_since(real_tick);
            prop_assert_eq!(passed, real_ticks);
        }
    }
}
