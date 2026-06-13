use bitvec::{order::Lsb0, slice::BitSlice};

struct PureTransportLineSlotArena {
    // This is a single collection of all the BitSlices for all TransportLines
    data: Vec<usize>,
}

struct PureTransportLine {}

fn create_bitslice_from_mut(data: &mut [usize], bit_len: u32, index: u32) -> &mut BitSlice {
    let bit_len = bit_len as usize;
    let index = index as usize;

    let data_len = bit_len.div_ceil(usize::BITS.try_into().expect("Hardcoded"));
    let data_slice = &mut data[index..(index + data_len)];

    let bit_slice = &mut BitSlice::<usize, Lsb0>::from_slice_mut(data_slice)[..bit_len];

    debug_assert_eq!(bit_slice.len(), bit_len);

    bit_slice
}

#[cfg(test)]
mod test {
    use super::*;

    use proptest::proptest;

    proptest! {
        #[test]
        fn create_bitslice(index in 0u32..100, bit_len in 0u32..1_000) {
            let mut data = vec![0; 1_000];
            let _bitslice = create_bitslice_from_mut(&mut data, index, bit_len);
        }
    }
}
