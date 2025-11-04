# Idea
Instead of doing the belt swap update (Costing 2 cachelines) for each update, we could store after which point the items should have moved forward one, and just respect that when querying.
This would cost 0 cachelines (besides the cost of the SmartBelt struct), whenever we cannot follow this scheme, since i.e. there is an item in the spot where we said it moved left, we do the actual update in a quick loop, hopefully fully utilizing the cache.
The actual update also has to be done, whenever an inserter tries to insert in a spot, which due to the lazy-offset overlaps the "stuck" part of the belt. This means it benefits from having some unused length at the end of the belt/allocation. It might make sense to increase the allocation length to always accomodate a large lazy-count even if the player does not add any additional length. Since this length is never read unless the lazy-scheme is in effect, it does not cost any resources, except RAM usage (and not a lot, since 4 Bytes per belt would allow lazy-counts up to 32 while increasing Late Game RAM usage by ~ 4 Bytes * 100_000 = 400Kb)
This scheme benefits from long runs of empty spots (like are created by inserters with large hand sizes)

# Result
