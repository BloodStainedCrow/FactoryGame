// #![feature(generic_const_exprs)]
#![feature(portable_simd)]
#![feature(iter_array_chunks)]

use std::simd::{cmp::SimdPartialEq, Simd};

#[cfg(feature = "dhat-heap")]
#[global_allocator]
static ALLOC: dhat::Alloc = dhat::Alloc;

fn main() {
    #[cfg(feature = "dhat-heap")]
    let _profiler = dhat::Profiler::new_heap();

    factory::main();
}

#[cfg(test)]
mod test {
    use itertools::Itertools;
    use rand::Rng;
    use std::{
        arch::x86_64::{
            __m256i, _mm256_add_epi8, _mm256_blendv_epi8, _mm256_cmpeq_epi8, _mm256_loadu_si256,
            _mm256_movemask_epi8, _mm256_set1_epi8, _mm256_set_epi8, _mm256_setzero_si256,
            _mm256_storeu_si256,
        },
        array, mem, thread,
        time::{Duration, Instant},
    };

    use rayon::iter::{IntoParallelIterator, ParallelIterator};

    use rand::random;

    // #[test]
    fn main_test() {
        const LEN: usize = 1_000;
        const NUM_ITER: u32 = 100;
        const NUM_CORES: usize = 12;

        (0..NUM_CORES).into_par_iter().for_each(|_| {
            let mut belt = Vec::new();
            belt.resize_with(LEN * 64, || rand::thread_rng().gen_bool(0.0001));

            let mut indices = Vec::new();
            indices.resize_with(LEN * 64, || random::<u8>());

            let mut inserter_hands = Vec::new();
            inserter_hands.resize_with(LEN * 64, || random::<u8>() % 8);

            let mut inserter_timers = Vec::new();
            inserter_timers.resize_with(LEN * 64, || random::<u8>() % 8);

            let mut output = Vec::new();
            output.resize(256, 0);

            let sum = Duration::from_secs(0);

            let size = belt.len() * size_of::<bool>()
                + indices.len() * size_of::<u8>()
                + inserter_hands.len() * size_of::<u8>();

            println!("Total size: {}", size);

            let transfers = 2 * belt.len() * size_of::<bool>()
                + 1 * indices.len() * size_of::<u8>()
                + 2 * inserter_hands.len() * size_of::<u8>();

            let start = Instant::now();
            for i in 0..NUM_ITER {
                test(
                    &mut belt,
                    &indices,
                    &mut inserter_hands,
                    &mut inserter_timers,
                    &mut output,
                );
            }
            let time = start.elapsed();

            println!("avg {:?}", time / NUM_ITER);
            println!(
                "gb/s {:?}",
                size as f64 / (time / NUM_ITER).as_secs_f64() / 1_000_000_000.0
            );
            y
        });

        loop {
            thread::sleep(Duration::from_secs(10));
        }
        // let mut belt = Vec::new();
        // belt.resize(LEN * 64, true);

        // let mut indices = Vec::new();
        // indices.resize_with(LEN * 64, || random::<u8>());

        // let mut inserter_hands = Vec::new();
        // inserter_hands.resize_with(LEN * 64, || random::<u8>() % 8);

        // let mut output = Vec::new();
        // output.resize(256, 0);

        // let mut sum = Duration::from_secs(0);

        // let size = belt.len() * size_of::<bool>()
        //     + indices.len() * size_of::<u8>()
        //     + inserter_hands.len() * size_of::<u8>();

        // println!("Total size: {}", size);

        // let start = Instant::now();
        // for i in 0..NUM_ITER {
        //     test(&mut belt, &indices, &mut inserter_hands, &mut output);
        // }
        // let time = start.elapsed();

        // println!("avg {:?}", time / NUM_ITER);
        // println!(
        //     "gb/s {:?}",
        //     size as f64 / (time / NUM_ITER).as_secs_f64() / 1_000_000_000.0
        // );

        // dbg!(inserter_hands);
    }

    fn test(
        belt: &mut [bool],
        indices: &[u8],
        inserter_hands: &mut [u8],
        inserter_timers: &mut [u8],
        output: &mut [u16],
    ) {
        assert!(output.len() >= 256);

        unsafe {
            let zeroes = _mm256_setzero_si256();
            let ones = _mm256_set1_epi8(1); // Create a vector with all 1's
            let eights = _mm256_set_epi8(
                8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8,
                8, 8, 8, 8,
            ); // Create a vector with all 8's

            // Process 32 items at a time using AVX2
            for i in (0..belt.len()).step_by(32) {
                // Prefetch the next indices and output entries
                // Caching is not helpful, since output is small enough to always be in L1
                // for j in 0..32 {
                //     // Prefetch the corresponding output index (machine counter)
                //     let machine_index = indices.as_ptr().add(i + j);
                //     _mm_prefetch::<_MM_HINT_T0>(machine_index as *const i8);
                // }

                // Load the next 32 items from the belt
                let belt_vals =
                    _mm256_loadu_si256(belt.as_ptr().add(i) as *const bool as *const __m256i);

                // Create a mask where each byte is 1 if belt[i] == 1, otherwise 0
                let mask = _mm256_cmpeq_epi8(belt_vals, ones); // Compare to 1

                let current_inserter_timers = _mm256_loadu_si256(inserter_timers.as_ptr().add(i)
                    as *const u8
                    as *const __m256i);

                let current_inserters_standing = _mm256_cmpeq_epi8(current_inserter_timers, zeroes);

                // let current_inserters_

                let current_inserter_hands = _mm256_loadu_si256(inserter_hands.as_ptr().add(i)
                    as *const u8
                    as *const __m256i);

                let inserter_hands_added = _mm256_add_epi8(current_inserter_hands, ones);

                let new_inserter_hands =
                    _mm256_blendv_epi8(current_inserter_hands, inserter_hands_added, mask);

                let inserter_full_mask = _mm256_cmpeq_epi8(new_inserter_hands, eights);

                let final_inserter =
                    _mm256_blendv_epi8(new_inserter_hands, zeroes, inserter_full_mask);

                _mm256_storeu_si256(
                    inserter_hands.as_mut_ptr().add(i) as *mut u8 as *mut __m256i,
                    final_inserter,
                );

                // Get the bitmask (integer) from the mask (this will be a 32-bit value)
                let bitmask = _mm256_movemask_epi8(inserter_full_mask); // Returns a 32-bit integer with each bit corresponding to a slot on the belt

                // This checks helps if most inserters do not insert on a specific frame (which is probably a good idea)
                if bitmask != 0x0 {
                    // Directly update the machine counters using the bitmask
                    for j in 0..32 {
                        // Check if the bit at position j is set in the bitmask
                        let machine_index = *indices.get_unchecked(i + j);
                        // Accumulate the count for the corresponding machine index using the bitmask
                        *output.get_unchecked_mut(usize::from(machine_index)) +=
                            ((bitmask >> j) & 1) as u16;
                    }
                }
            }

            // // Scalar fallback for remaining items if belt_size is not a multiple of 32
            // for (; i < belt_size; i++) {
            //     output[indices[i]] += belt[i];
            // }

            output.iter_mut().for_each(|o| *o *= 8);
        }
    }

    pub fn test2(
        belts: &mut [&mut [bool]],
        hand_content: &mut [u8],
        belt_id: &[u32],
        belt_index: &[u16],
    ) {
        const ARR_LEN: usize = 1;

        assert_eq!(hand_content.len(), belt_id.len());
        assert_eq!(hand_content.len(), belt_index.len());

        let mut iter = hand_content
            .iter_mut()
            .zip(belt_id.iter().zip(belt_index))
            .array_chunks();

        unsafe {
            for (hands, belt_id, belt_index) in iter.map(|arr: [_; ARR_LEN]| split_array(arr)) {
                // FIXME: Hack for type inferance
                let mut hands: [_; ARR_LEN] = hands;
                let belt_id: [_; ARR_LEN] = belt_id;
                let belt_index: [_; ARR_LEN] = belt_index;

                let belt_ptr = belts.as_mut_ptr();
                let belts: [_; ARR_LEN] = belt_id.map(|id| belt_ptr.add(*id as usize));
                let belt_positions: [&mut bool; ARR_LEN] = array::from_fn(|i| {
                    let belt = (*belts[i]).as_mut_ptr();
                    &mut (*belt.add(*belt_index[i] as usize))
                });

                let to_move: [bool; ARR_LEN] =
                    array::from_fn(|i| (*hands[i] > 0) && *belt_positions[i]);

                for ((hand, to_move), belt_position) in
                    hands.iter_mut().zip(to_move).zip(belt_positions)
                {
                    **hand -= u8::from(to_move);
                    *belt_position = to_move;
                }
            }
        }
    }

    type BeltTransmuteType = u32;
    pub fn update_non_overlapping_inserters(
        belts: &mut [&mut [BeltTransmuteType]],
        belt_first_free_sure: &mut [bool],
        belt_first_free_pos: &[u16],
        hand_content: &mut [u8],
        belt_id: &[u32],
        belt_index: &[u16],
    ) {
        assert_eq!(hand_content.len(), belt_id.len());
        assert_eq!(hand_content.len(), belt_index.len());

        unsafe {
            for (hand, (belt_id, belt_index)) in
                hand_content.iter_mut().zip(belt_id.iter().zip(belt_index))
            {
                let belt = belts.get_unchecked(*belt_id as usize);
                let belt_position = {
                    belt.get_unchecked(
                        (*belt_index as usize) / size_of::<BeltTransmuteType>() * size_of::<bool>(),
                    )
                };

                let belt_items = (*belt_position
                    >> 8 * (*belt_index as usize % size_of::<BeltTransmuteType>()))
                    as u8;

                *hand -= u8::from(belt_items);
            }

            for (belt_id, belt_index) in belt_id.iter().zip(belt_index) {
                let belt_ptr = belts.as_mut_ptr();
                let belt = belt_ptr.add(*belt_id as usize);
                let belt_position = {
                    let belt = (*belt).as_mut_ptr();
                    belt.add(
                        (*belt_index as usize) / size_of::<BeltTransmuteType>() * size_of::<bool>(),
                    )
                };
                *belt_position =
                    u32::from(true) << 8 * (*belt_index as usize % size_of::<BeltTransmuteType>());

                *belt_first_free_sure.get_unchecked_mut(*belt_id as usize) =
                    !(*belt_first_free_pos.get_unchecked(*belt_id as usize) == *belt_index)
                        && *belt_first_free_sure.get_unchecked(*belt_id as usize);
            }
        }
    }

    pub fn update_non_overlapping_inserters_pre_indexed(
        belts: &mut [&mut BeltTransmuteType],
        belt_first_free_pos: &[u16],
        hand_content: &mut [u8],
        belt_index: &[u16],
    ) {
        assert_eq!(hand_content.len(), belts.len());
        assert_eq!(hand_content.len(), belt_index.len());

        unsafe {
            for (hand, (belt_position, belt_index)) in hand_content
                .iter_mut()
                .zip(belts.iter_mut().zip(belt_index))
            {
                let belt_items = (**belt_position
                    >> 8 * (*belt_index as usize % size_of::<BeltTransmuteType>()))
                    as u8;

                *hand -= u8::from(belt_items);
            }

            for (belt_position, belt_index) in belts.iter_mut().zip(belt_index) {
                **belt_position =
                    u32::from(true) << 8 * (*belt_index as usize % size_of::<BeltTransmuteType>());
            }
        }
    }

    fn split_array<const N: usize, A, B, C>(arr: [(A, (B, C)); N]) -> ([A; N], [B; N], [C; N]) {
        unsafe {
            let first = array::from_fn(|idx| ((&arr[idx].0) as *const A).read());
            let second = array::from_fn(|idx| ((&arr[idx].1 .0) as *const B).read());
            let third = array::from_fn(|idx| ((&arr[idx].1 .1) as *const C).read());

            mem::forget(arr);
            (first, second, third)
        }
    }
}

pub fn find_first_free_pos(locs: &[u8]) -> usize {
    let (_, locs_simd, _) = locs.as_simd();

    let Some(simd_index) = locs_simd.into_iter().position(|v| any_zero_byte(*v)) else {
        return locs.len();
    };

    for i in (simd_index * 32)..((simd_index + 1) * 32) {
        if locs[i] == 0 {
            return i;
        }
    }

    return locs.len();
}

fn any_zero_byte(v: Simd<u8, 32>) -> bool {
    const ZERO: Simd<u8, 32> = Simd::splat(0);

    let eq = v.simd_eq(ZERO);
    eq.any()
}

// trait SimdElem: Sized {
//     const SIZE: usize = size_of::<Self>();
// }

// const OPTIMAL_SIMD_SIZE_BYTES: usize = 64;
// struct Simd<T: SimdElem, const N: usize = { OPTIMAL_SIMD_SIZE_BYTES / T::SIZE }> {
//     v: [T; N],
// }

// impl SimdElem for u32 {}
// impl SimdElem for u16 {}

// type Test1 = Simd<u32>;
// type Test2 = Simd<u16>;

// const ORIG: Test2 = todo!();
// const TEST: Simd<u16, 100> = ORIG;
