# Rigged Glushkov Regular Expressions in Rust

This code is significantly different from the Haskell version (Haskell
Experiment 07), in that I decided to "go for it" and merge the process
of instantiation and rigging into a single structure.  That decision was
predicated on the fact that I already had convenience constructors for
my node types, which meant that I already had all the methods necessary
for users to build regular expressions in the simple, internal
representation.

In the Rust version, we build on the Haskell idea of recording already
found "empty" and "final" versions, so our data structure being passed
around is a record of (empty, final, expression).  The `emp`, `alt`,
`seq`, and `rep` expressions are pretty much as you'd expect; one thing
you won't find in the base code is the implementation for `sym`.  `sym`
must implemented independently for different semiring implementations.

The `Sym` expression is a *trait* now; it says that users must provide
an implementation with a single method, `is`, that takes a symbol and
returns a semiring.

The processing of the expression using Gluskov's progressive algorithm
is the same as the unrigged version.  One thing that greatly frustrated
me in this implementation is the overuse of the `.clone()` method to
provide copies of many items, some of which aren't modified!  I haven't
yet figured out how to change that, although I suspect it involves
modifying `Mul` and `Add` to handle references-to-things as well as the
things themselves.  I seem to recall [Bodil](https://github.com/bodil)
solved a similar problem in her Immutable Structures for Rust library
and had an interesting solution for it.

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
a new string by concatenating the orderded pair.  This takes a lot of
memory thrashing.
