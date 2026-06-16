#![feature(never_type)]

use stable_vec::StableVec;

use crate::assember::MiddleAssemblerInfo;

pub mod assember;
pub mod lists;

pub struct Middle {
    assembler_list: StableVec<MiddleAssemblerInfo>,
}

#[cfg(test)]
mod tests {}
