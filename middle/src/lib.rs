#![feature(never_type)]

use stable_vec::StableVec;

use crate::{assember::MiddleAssemblerInfo, power_pole::MiddlePowerPoleInfo};

pub mod assember;
pub mod lists;
pub mod power_pole;

pub struct Middle {
    assembler_list: StableVec<MiddleAssemblerInfo>,
    power_pole_list: StableVec<MiddlePowerPoleInfo>,
}

#[cfg(test)]
mod tests {}
