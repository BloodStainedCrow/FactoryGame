As long as the assembler update is reversible (Which it should trivially be for a simd implementation, and still be with some work for a bucketed implementation),
We can just run an optimistic update with 100% Power Satisfaction, collecting the power requirements on the way, then try to extract this power from the grid, and IF (which should be rare) the power is insufficient, re-calculate the assembler using this new power satisfaction percentage.

This scheme does set a hard requirement on power consumption of any machine not depending on power availabilty (at least not in the same tick) (i.e. beacons may not change power consumption the same tick), since otherwise the new power satisfaction might change the required power leading to an infinite-ish loop of updates

This scheme allows me to only loop over the grid-machines once, in the likely scenario where power satisfaction is at 100%, only needing the two loops (that I think factorio must do), whenever power satisfaction drops