# Glushkov Regular Expressions, in Haskell

This is a Glushkov's construction of regular expressions. The basic
idea is that for every symbol encountered during parsing, a
corresponding symbol in the tree is marked (or, if not symbols are
marked, the parse is a failure).  Composites are followed to their
ends for each character, and if the symbol matches it is "marked".

In this instance, are passing a Glushkov regular expression tree,
and for each character it returns a new, complete copy of the tree,
only with the marks "shifted" to where they should be given the
character.  In this way, each iteration of the tree keeps the NFA
list of states that are active; they are the paths that lead to
marked symbols.

'final' here means that no more symbols have to be read to match
the expression.  'empty' here means that the expression matches
only the empty string.

'final' is used here to determine if, for the Glushkov expression
passed in, does the expression contain a marked symbol?  This is
used both to determine the end state of the expression, and in
sequences to determine if the rightmost expression must be evaluted,
that is, if we're currently going down a 'marked' path and the left
expression can handle the empty string OR the left expression is
final.

The accept method is just a fold over the expression.  The initial
value is the shift of the first character, with the assumed mark of
'True' being included because we can always parse infinitely many
empty strings before the sample begins.  The returned value of that
shift is our new regular expression, on which we then progressively
call `shift False accg c`; here False means that we're only going to
shift marks we've already found.

The "trick" to understand this is to consider the string "ab" for
the sequence "ab".  The first time through, we start with True, and
what gets marked is the symbol 'a'.

When we pass the letter 'b', what happens?  Well, the 'a' symbol
will be unmarked (it didn't match the character), but the second
part of the shift expression says that the left expression is final
(it's a symbol and it's marked!), so we call `shift True (Sym 'b')
'b'`, and the mark moves to the correct destination.

It continues to blow my mind that so much of mathematics can be directly
translated into Haskell with no loss of fidelity.
