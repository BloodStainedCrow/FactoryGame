One quirk I noticed while profiling is, I got no samples for the branch where the inserter was outgoing
and we are at less than old_first_free.


While that might be normal (due to this situation immediately stopping itself from happening next tick),
investigate to make sure.