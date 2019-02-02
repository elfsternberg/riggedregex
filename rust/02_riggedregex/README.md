# Kleene Regular Expressions with Rigging, in Rust

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

Rust isn't nearly as magical as Haskell.  (See the Readme in the
equivalent Haskell version for my comments on that.)  On the other hand,
it's not necessary to define a Semiring explicitly; instead, we define a
nominative type, a struct containing our real return type, and then
provide implementations of One, Zero, Mul, and Add for that type.  Here,
my two semirings are name `Recognizer` and `Ambigcounter`, and to make
them work we have to say that our recognizer is a `Regw<Recognizer>`;
Rust won't magically glue everything together the way Haskell will.

Still, this was a straightforward implementation of the rigged regular
expression, and is a good stepping stone for future projects.
