# What is this?
This project is an academic recreation of the factory game [Factorio](https://www.factorio.com/) taking additional ideas from [Dyson Sphere Program](https://store.steampowered.com/app/1366540/Dyson_Sphere_Program/).

I created it as an exercise to see how far I could optimize the basic concepts and algorithms of the genre in terms of performance, while allowing myself  minor changes to the games' rules.
Another goal that emerged along the way, was learning about the way modern CPUs actually work.

# Roadmap
Currently I adding beacons and thinking about how to efficiently add logistics bots. Then I want to build a comprehensive suit of benchmark test to show if/by how much I was able to improve performance.

# Why did you start?
I was playing the above games and started being unable to expand due to performance issues. So in my hubris I declared: "How hard can it be?".

## TODOS
- ~~Place Power Production~~
- ~~Blueprints so I can actually do perf tests~~
- ~~Permanently running replay system, so I can easily recreate crashes~~
- ~~Test harness for replays, to ensure they do not crash~~
- ~~Automatic insertion limit~~
- ~~Assembler Module Support~~
- ~~World listener support (i.e. update whenever something changes in the world, for power, beacons and inserters)~~
- Lazy Terrain Generation
- ~~Assembler Module Frontend~~
- ~~Assembler Power Consumption Modifier Support~~
- ~~Beacons~~
- ~~FIX Beacon Flicker due to lowering power consumption when beacons are unpowered~~
- ~~Storage Storage Inserters~~
- ~~Science Consumption in Labs~~
- ~~Inserter connections to labs~~
- ~~Debug inserters~~
- ~~Production Graphs~~
- ~~Liquids~~
- ~~Map View~~
- Technology
- Mining Drills
- ~~Underground belts~~
- Fix Underground Pipe connection breaking/overlap
- Place Steam Turbines
- ~~Splitters~~
- Allow Belts of different types to connect to one another
- Decide if I want beacons to match factorio behaviour or keep the hard switch on/off
- ~~Ore Generation~~
- Add tile requirements for buildings/recipes (for offshore pump)
- Bots
- MAYBE: A canonical version of the simulation that can be used for diff testing (and as some weird documentation of the mechanics I suppose)