# Rigged Glushkov Regular Expressions in Rust

This code is significantly different from the Haskell version (Haskell
Experiment 07), in that I decided to "go for it" and merge the process
of instantiation and rigging into a single structure. 

Prior to Rust Experiment 06, the Rust experiments had followed the
Haskell versions' pattern of building the regular expression first using
Kleene expressions, and then lifting them on-the-fly into more complex,
"rigged" versions, before processing them with the given Kleene or
Glushkov construction.

Rust famously doesn't have memory management, but instead uses lifetimes
and scopes to place much of what it does safely on the stack.  It takes
some fiddling to make types, lifetimes, and scopes line up, so as far
back as the first Rust experiment I had individual factory functions for
the different Regex sub-types.  These take the place of the simple
`data` types seen in the equivalent Haskell experiments.

With that in mind, there was no reason to have two different data
structures; for this experiment, there is only the one `enum` types and
its sub-types.

In this experiment, as in the Haskell version, I build on their idea of
recording already found "empty" and "final" versions of nodes, so the
data structure is now a record of (empty, final, expression).  The
`emp`, `alt`, `seq`, and `rep` expressions are pretty much as you'd
expect; one thing you won't find in the base code is the implementation
for `sym`.  `sym` must implemented independently for different semiring
implementations.

The `Sym` expression is a *trait* now; it says that users must provide
an implementation with a single method, `is`, that takes a symbol and
returns a semiring.

I've had to abandon the use of `num_traits` and `std::ops`, as
`std::ops::Mul` and `std::ops::Add` don't provide a framework for
passing in references.  I've gone back to my initial instincts and
provided a comprehensive `Semiring` trait which can take references for
the `mul` and `add` operations.  This works very well, as now I can
analyze and operate on immutable `HashSet<String>` collections without
having to clone them to pass them to the cartesian product operation.
That's a massive win in terms of memory and CPU savings.

The construction processing the expression using Gluskov's progressive
algorithm is the same as the unrigged version, only we cache the "empty"
and "final" values when they're found and do not recalculate them.

Down in the tests, you'll find the Boolean version (`Recognizer`) as
well as a string version (`Parser`).  Both versions show how to
implement a semiring for doing data extraction, including how to define
a specific Sym implementation for the `Sym` trait, and include a
function to instantiate that implementation for your use case.

The `Parser` version has a specific moment of complexity that can't be
elided: in the `mul` implementation, the multiplication of two sets is
the cartesian product of those sets: a new set containing all possible
combinations of ordered tuples made up of a member of the first set with
a member of the second set.  For our purposes, this is a still a string,
so our implementation involves building those tuples and then generating
a new string by concatenating the orderded pair.  This takes a bit of
memory thrashing, but much less now that I've solved the `mul(&x, &y)`
issue.

## License

As this is entirely my work, it is copyright (c) 2019, and licensed
under the Mozilla Public License v. 2.0.  See the
[LICENSE.md](../../LICENSE.md) in the root directory.
