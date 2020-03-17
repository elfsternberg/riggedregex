# Glushkov Regular Expressions, in Rust

This is a Glushkov's construction of regular expressions. The basic idea
is that for every symbol encountered during parsing, a corresponding
symbol in the tree is marked (or, if no symbols are marked, the parse
is a failure).  Composites are followed to their ends for each
character, and if the symbol matches it is "marked".

In this instance, we create a Glushkov regular expression tree, and for
each character it returns a new, complete copy of the tree, only with
the marks "shifted" to where they should be given the character.  In
this way, each iteration of the tree keeps the NFA list of states that
are active; they are the paths that lead to marked symbols.

`ended` here means that no more symbols have to be read to match the
expression.  `empty` here means that the expression matches only the
empty string.  This function was named `final` in the Haskell version,
but the word `ended` is used here because `final` is a reserved word in
Rust.

'ended' is used here to determine if, for the Glushkov expression
passed in, does the expression contain a marked symbol?  This is
used both to determine the end state of the expression, and in
sequences to determine if the rightmost expression must be evaluted,
that is, if we're currently going down a 'marked' path and the left
expression can handle the empty string OR the left expression is
ended.

The accept method is just a fold over the expression.  The initial
value is the shift of the first character, with the assumed mark of
`True` being included because we can always parse infinitely many
empty strings before the sample begins.  The returned value of that
shift is our new regular expression, on which we then progressively
call `shift False accg c`; here False means that we're only going to
shift marks we've already found.

The "trick" to understanding how this works is to consider the string
"abc" for the expression "abc".  The first time through, we start with
True, and what gets marked is the symbol 'a':

`(seq 'a' (seq 'b' 'c')) -> (seq 'a'* (seq 'b' 'c'))`

When we pass the letter 'b', what happens?  Well, the returned
expression will have the 'a' symbol unmarked (it didn't match the
character), but the second part of the shift expression says that the
left expression is ended (it's a symbol and it was marked!), so we call
`shift True (Sym 'b') 'b'`, and the new symbol generated will be marked,
moving the mark to the correct destination.  The same thing happens on
the next iteration.  The *inner seq* will get back that `(sym 'b')` is
marked, so 'c' will match the `(sym 'c')` and shift will be in a `True`
state, so now the expression comes back `(seq 'a' (seq 'b' 'c'*))`.  

When we run out of letters or regex, we can ask, "Is the expression
final?"  Again, the tricky part is inside sequences: we're only final if
the left side is final and the right side can handle an empty string, or
if the right side is final.

Porting this from Haskell was *much* more straightforward than porting 
the straight regex versions, and is slightly more efficient, although 
it still has the "transition the entire parse tree every character" 
problem.  That's to be solved later.

## License

As this is entirely my work, it is copyright (c) 2019, and licensed
under the Mozilla Public License v. 2.0.  See the
[LICENSE.md](../../LICENSE.md) in the root directory.
