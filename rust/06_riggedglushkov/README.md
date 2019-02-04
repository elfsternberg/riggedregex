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

It turns out that the `Mul` and `Add` traits cannot take references.  In
my [first commit of this
experiment](https://github.com/elfsternberg/riggedregex/tree/41c39a1c4a94bd7edd2d20883cc77cd53bf2966e),
I ended up implementing a lot of `.clone()`ing of the semiring
implementation, which is okay for `bool` and `int`, but when I started
doing it with `HashSet<String>` well, that's a lot of memory and cpu to
waste.

I've had to implement my own versions, `RMul` and `RAdd`, and their
follow-on operators.  It feels very cluttered, but it also handles only
one copy of any given set.

The construction processing the expression using Gluskov's progressive
algorithm is the same as the unrigged version, only we cache the "empty"
and "final" values when they're found and do not recalculate them.

Down in the tests, you'll find the Boolean version (`Recognizer`) as
well as a string version (`Parser`).  Both versions show how to
implement a semiring for doing data extraction, including how to define
a specific Sym implementation for the `Sym` trait, and include a
function to instantiate that implementation for your use case.

The `Parser` version has a specific moment of complexity that can't be
elided: in the `Mul` implementation, the multiplication of two sets is
the cartesian product of those sets: a new set containing all possible
combinations of ordered tuples made up of a member of the first set with
a member of the second set.  For our purposes, this is a still a string,
so our implementation involves building those tuples and then generating
a new string by concatenating the orderded pair.  This takes a bit of
memory thrashing.
