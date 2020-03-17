# Rigged Generic Brzozowski μ-Regular Expressions

This experiment realizes an important and significant step in the
series of experiments.  In this variant, the `*` *operation* has been
completely removed; the `*` *operator* is now implemented as a recursive
definition:

    // ONE is the identity operator under concatenation
    r* = alt(eps(ONE), seq(r, r*))

This may seem nonsensical to a programmer, but it's actually quite
implementable in a language that allows mutation under limited effects.

This means that I now have working *μ-regular expressions*, regular
expressions that use a fixpoint operator to identify the least common
fixpoint of a recursive regular expression, and one that allows regular
expressions to be encapsulated as variables and composed just as one
would compose ordinary functions.

This effort took a *large* number of evolutions, as I went down various
paths trying to write code faster than I could think or understand.  At
least twice I had to delete the work in progress and back up to an
earlier commit, throwing away hours of work.

But this is *it*, for some definition of "it."  This is what everything
has been working up to.

## Understanding this code

The first thing to appreciate is that `nullable()` is now a
self-terminating recursive implementation.  At its core is the same
nullable() instruction we've been using for a while now, but now when we
determine the nullability of a node in the expression, we cache that
value and we notify all of its parent nodes that they may also be able
to determine nullability.  This is useful for cases such as the Alt(),
which has two children: if one is determined to be nullable, then the
other may be as well, in which case it's now possible to mark (cache)
that the entire expression is always nullable.  And that's useful if the
expression is going to be re-used.

And in this code, expressions are composable, re-usable elements of
code.  They can be re-used.

Also of note: the "mutate" codes are implementations of the short-outs
described in Might's last paper on the topic; take a node, take its
inputs, and determine if one of those inputs is already null or empty;
if that's the case, then a *different* node must be expressed in that
position, one that's simpler and faster.

## License

As this is entirely my work, it is copyright (c) 2019, and licensed
under the Mozilla Public License v. 2.0.  See the
[LICENSE.md](../../LICENSE.md) in the root directory.
