# Brzozowski Regular Expressions, in Haskell

This is a regex recognizer implementing Brzozowski's Algorithm, in
Haskell.  Brzozowski's Algorithm has been a bit of a fascination for me,
because it made generally much more sense that the traditional
algorithm, especially since the Pumping Lemma is much more intelligible
under Brzozowski than it is with more common forms of automata analysis.

Brzozowski's algorithm basically says that a regular expression is a
function that, given a string and a regular expression, returns three
things: the remainder of the input after the leading character has been
consumed, and a new function that represents the rest of the regular
expression after that leading character has been analyzed, and the
status of the analysis thus far.

Brzozowski called this "the derivative of the regular expression."

The only trick to dealing with Brzozowski's Algorithm is with respect to
nullability: it is important to know if a regular expression _may be
nullable_ (that is, it may accept the empty string).  A separate
function describes the nullability of the different kinds of expressions
in our system.





