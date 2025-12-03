Ensure the assembler waitlist is only read if it produced anything.

easiest to test by making the "generate" recipes slow and duplicating lots of them and observing the patterns in lag spikes.

Issue:
Checking the waitlist is a very significant part of the runtime of the assembler update.
Figure out a way to improve this.
Maybe a bitvec to store if there are any inserters in the list? This would be set whenever an inserter enters the waitlist (adding an additional cache line read + write), but would allow skipping the waitlist check if said bit is not set.
Depending on the cache hit rate of this bitset, it could be beneficial or not, since checking this bit is a cacheline (theoretically) on its own.
In practice this seems unlikely, since a cacheline fits 64 * 8 = 512 flags.
The higher the crafting speed/(lower the recipe craft time) the more important this optimization becomes, and also the more likely the bitvec is to be already cached, reducing random access costs.

This is of course not beneficial if each assembler always has (at least one) inserter per item in the waitlist whenever it finished a craft.


If I implement this, one thing I should test is, not using a bitvec, but instead just a bool slice, since it should not require reading the memory when setting the flag.

This flag would be (potentially) reset, whenever the assembler crafts and subsequently checks the waitlist. Since we already checked the waitlist, checking if it is empty, should not incur any additional memory bandwidth costs.


Another thing, that could theoretically help is reducing the size of an inserter waitlist to 32 bytes (maybe by reducing the capacity to 2), and evaluating if this is an improvement or not.
My guess is it is unlikely, since the additional spinning inserters will kill all improvements we could get.