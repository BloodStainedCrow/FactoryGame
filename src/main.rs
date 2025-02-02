use std::{
    arch::x86_64::{
        __m256i, _mm256_add_epi8, _mm256_blendv_epi8, _mm256_cmpeq_epi8, _mm256_loadu_si256,
        _mm256_movemask_epi8, _mm256_set1_epi8, _mm256_set_epi8, _mm256_setzero_si256,
        _mm256_storeu_si256, _mm_prefetch, _MM_HINT_T0,
    },
    thread,
    time::{Duration, Instant},
};

use rand::{random, Rng};
use rayon::iter::{IntoParallelIterator, ParallelIterator};

fn main() {
    factory::main();
}

fn main_test() {
    const LEN: usize = 1_000_000;
    const NUM_ITER: u32 = 100;
    const NUM_CORES: usize = 12;

    (0..NUM_CORES).into_par_iter().for_each(|_| {
        let mut belt = Vec::new();
        belt.resize_with(LEN * 64, || rand::thread_rng().gen_bool(0.0001));

        let mut indices = Vec::new();
        indices.resize_with(LEN * 64, || random::<u8>());

        let mut inserter_hands = Vec::new();
        inserter_hands.resize_with(LEN * 64, || random::<u8>() % 8);

        let mut output = Vec::new();
        output.resize(256, 0);

        let mut sum = Duration::from_secs(0);

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
                &mut Vec::new(),
                &mut output,
            );
        }
        let time = start.elapsed();

        println!("avg {:?}", time / NUM_ITER);
        println!(
            "gb/s {:?}",
            size as f64 / (time / NUM_ITER).as_secs_f64() / 1_000_000_000.0
        );
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

#[inline(never)]
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
            8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8,
            8, 8, 8,
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
            let belt_vals = _mm256_loadu_si256(&belt[i] as *const bool as *const __m256i);

            // Create a mask where each byte is 1 if belt[i] == 1, otherwise 0
            let mask = _mm256_cmpeq_epi8(belt_vals, ones); // Compare to 1

            let current_inserter_timers =
                _mm256_loadu_si256(&inserter_timers[i] as *const u8 as *const __m256i);

            let current_inserters_standing = _mm256_cmpeq_epi8(current_inserter_timers, zeroes);

            // let current_inserters_

            let current_inserter_hands =
                _mm256_loadu_si256(&inserter_hands[i] as *const u8 as *const __m256i);

            let inserter_hands_added = _mm256_add_epi8(current_inserter_hands, ones);

            let new_inserter_hands =
                _mm256_blendv_epi8(current_inserter_hands, inserter_hands_added, mask);

            let inserter_full_mask = _mm256_cmpeq_epi8(new_inserter_hands, eights);

            let final_inserter = _mm256_blendv_epi8(new_inserter_hands, zeroes, inserter_full_mask);

            _mm256_storeu_si256(
                &mut inserter_hands[i] as *mut u8 as *mut __m256i,
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
