#![feature(never_type)]

use stable_vec::StableVec;

use crate::{assember::MiddleAssemblerInfo, power_pole::MiddlePowerPoleInfo};

pub mod assember;
pub mod lists;
pub mod power_pole;

#[derive(Debug, Clone)]
pub struct Middle {
    assembler_list: StableVec<MiddleAssemblerInfo>,
    power_pole_list: StableVec<MiddlePowerPoleInfo>,
}

impl Middle {
    #[must_use]
    pub fn new() -> Self {
        Self {
            assembler_list: vec![].into(),
            power_pole_list: vec![].into(),
        }
    }
}

#[cfg(test)]
mod tests {}
