#[derive(Debug, Clone, Copy)]
pub struct SingleItemSlotIndex(u32);

pub struct SingleItemSlice<'a> {
    pub current: &'a mut [u8],
    pub max: &'a [u8],
    pub inserter_wait_list: &'a mut [!],
}
