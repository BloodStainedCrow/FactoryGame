#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct AssemblerMiddleID(pub u32);

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct InserterMiddleID(pub u32);

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct BeltMiddleID(pub u32);

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct ChestMiddleID(pub u32);

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct PipeMiddleID(pub u32);

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct PowerPoleMiddleID(pub u32);

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct PowerGridMiddleID(pub u32);
