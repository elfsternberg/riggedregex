# Kleene Regular Expressions with Rigging, in Haskell

This program builds on the simple regular expressions in Version 01,
provding a new definition of a regular expression `Regw` that takes two
types, a source type and an output type.  The output type must be a
[*Semiring*](https://en.wikipedia.org/wiki/Semiring).

A semiring is a set R equipped with two binary operations + and ⋅, and
two constants identified as 0 and 1.  By providing a semiring to the
regular expression, we change the return type of the regular expression
to any set that can obey the semiring laws.  There's a surprising amount
of stuff you can do with the semiring laws.

In this example, I've providing a function, `rigged`, that takes a
simple regular expression from Version 01, and wraps or extracts
the contents of that regular expression into the `Regw` datatype.
Instead of the boolean mathematics of Version 01, we use the semiring
symbols `add` and `mul` to represent the sum and product operations on
the return type.  We then define the "symbol accepted" boolean to return
either the `zero` or `one` type of the semiring.

I've provided two semirings: One of (0, 1, +, *, Integers), and one of
(False, True, ||, &&, Booleans).  Both work well.

The `accept expression string` function of the original still works, but
if you say `accept (rigged expression) string :: Int`, Haskell will *go
find* a Semiring that allows this function to work and return the number
of ambiguities encountered during parsing.  If you ask for Bool as a
return type, it will behave as the original.

Sometimes, Haskell is bleeding magical.

