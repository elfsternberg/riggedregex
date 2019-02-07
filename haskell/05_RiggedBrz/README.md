# Rigged Brzozowski Regular Expressions, in Haskell

This is the naive implementation of Brzozowski's Algorithm, but with a
Semiring implementation for gathering complex information from the parse
process.  This implementation is "naive" in that it saves everything,
including the very large number of dead branches that hang off Sequence
processing, and then discards them at the very end of the process.

This implementation finally proves to me something that I've been trying
to express for a while: Might, Adams, et. al.'s implementations of tree
parsing *are* Semiring implementations, they just don't call it that,
but the fundamental underlying operations are the same.

I'm fascinated by the lack of the nullability operator.  Instead, it's
just resolved by Emp being parsed as `zero` and Eps as `one * s` where
`s` is the product of the previous operation, and then the new `Delta`
operator preserves this semantic, using multiplicative annhilation to
discard false parses while also being immune to the `Sequence` semantic
that destroys success parse history.

This can't last.  And Might admits it doesn't last.  Darais's
implementation goes back to having a separate function for nullability
that both preserves the status of known-nullable expressions and handles
recursion.  Darais's version also implements an incredible number of
optimizations to prune, compact, and process the parse tree early,
enabling a number of speedups and caching strategies that get you within
spitting distance of RE2.


