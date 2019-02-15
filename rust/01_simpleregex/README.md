# Kleene Regular Expressions, in Rust.

This is literally the definition of a simple string recognizing regular
expression in Rust.  It consists of the `Reg` datatype encompassing
the five standard operations of regular expressions and an `accept`
function that takes the expression and a string and returns a Boolean
yes/no on recognition or failure. It is a direct implementation of
Kleene's algebra:

    L[[ε]] = {ε}
    L[[a]] = {a}
    L[[r · s]] = {u · v | u ∈ L[[r]] and v ∈ L[[s]]}
    L[[r | s]] = L[[r]] ∪ L[[s]]
    L[[r∗]] = {ε} ∪ L[[r · r*]]
    
Those equations are for: recognizing an empty string, recognizing a
letter, recognizing two expressions in sequence, recognizing two
expression alternatives, and the repetition operation.

Composition is by simple reference-counted pointers to child
expressions.  I've provided convenient constructor functions to make the
creation of new regexes easier.

The `accept` function has two helper functions that split the string,
and all substrings, into all possible substrings such that *every
possible combination* of string and expression are tested, and if the
resulting tree of `and`s (from Sequencing and Repetition) and `or`s
(from Alternation) has at least one complete collection of `True` from
top to bottom then the function returns true.

This generation and comparison of substrings is grossly inefficient; an
string of eight 'a's with `a*` will take 30 seconds on a modern laptop;
increase that to twelve and you'll be waiting about an hour.  The cost
is `2^(n - 1)`, where `n` is the length of the string; this is a
consequence of the sequencing operation.  Sequences aren't just about
letters: they could be about anything, including repetition (which
itself creates new sequences) and other sequences, and the cost of
examining every possible combination of sequencing creates this
exponential cost.

While not as clean (no pun intended) as the Haskell version, especially
in the helper functions, it's still surprisingly easy to read, and the
`accept` function is almost line-for-line as clear as the Haskell
version.  The use of `.any` and `.all` for the `and` and `or` functions
makes a lot of sense here.

## License

As this is entirely my work, it is copyright (c) 2019, and licensed
under the Mozilla Public License v. 2.0.  See the
[LICENSE.md](../../LICENSE.md) in the root directory.
