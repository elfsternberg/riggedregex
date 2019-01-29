# Kleene Regular Expressions with Rigging, in Rust

This is the stock implementation of regular expressions as defined by
Kleene, with a semiring (a "rig"), wrapped around the result values so
that more comprehensive extraction of results can be done. It has
absolutely no optimizations, and is deadly slow. However, because of its
comprehensiveness, it works on just about any data.
