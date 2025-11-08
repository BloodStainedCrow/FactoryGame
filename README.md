# What is this?
This project is an academic recreation of the factory game [Factorio](https://www.factorio.com/).

I created it as an exercise to see how far I could optimize the basic mechanics and algorithms of the genre in terms of performance.
Another goal that emerged along the way, was learning about the way modern CPUs actually work.

# Why did you start?
I was playing Factorio and started being unable to expand due to performance issues. So in my hubris I declared: "How hard can it be?".

# Current State
Most logic for power grids, belts, splitters, assemblers, labs, inserters, mining drills, solar panels and accumulators is working. This allowed me to recreate a Factorio base, giving me a point for performance comparison.
I was able to run a base comprised of 40 copies of [this](https://factoriobox.1au.us/map/view/2824bc1566bd95b5825baf3bd2eb8fa32de8397526464f5a0327bcb82d64ebf8/#1/nauvis/15/2942/1158/0/447) Factorio Megabase by Smurphy (which Factorio runs at ~40 UPS) at 60 UPS on my machine.

# Running it
It should run on Linux, Windows and MacOS. Assuming you have [rust and cargo](https://rust-lang.org), just `cargo run --release`. On NixOS the included `shell.nix` contains all you need.

# Attributions
All graphics used with the `graphics` feature are from the Factorio Mod [Krastorio 2 Assets](https://codeberg.org/raiguard/Krastorio2Assets).