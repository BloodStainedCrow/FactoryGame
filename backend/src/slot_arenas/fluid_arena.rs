use crate::liquid::FluidSystemID;

pub const NO_TOKEN: FluidSystemID = FluidSystemID(u32::MAX);

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct FluidIndex(pub u32);

pub struct SingleFluidSlice<'a> {
    pub current: &'a mut [u8],
    pub max: &'a [u8],
    pub tokens: &'a mut [FluidSystemID],
}
