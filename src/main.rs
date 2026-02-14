use std::env;

#[cfg(feature = "dhat-heap")]
#[global_allocator]
static ALLOC: dhat::Alloc = dhat::Alloc;

#[cfg(not(feature = "dhat-heap"))]
#[cfg(not(debug_assertions))]
#[cfg(not(target_arch = "wasm32"))]
#[global_allocator]
static GLOBAL: mimalloc::MiMalloc = mimalloc::MiMalloc;

// #[cfg(not(feature = "dhat-heap"))]
// #[global_allocator]
// static GLOBAL: tikv_jemallocator::Jemalloc = tikv_jemallocator::Jemalloc;

fn main() -> Result<(), ()> {
    #[cfg(feature = "dhat-heap")]
    let _profiler = dhat::Profiler::new_heap();

    let args: Vec<String> = env::args().collect();

    match factory::main(&args) {
        Ok(()) => {
            log::info!("Exiting");
            Ok(())
        },
        Err(e) => {
            eprintln!("{e}");
            Err(())
        },
    };

    Ok(())
}
