# Rigged Glushkov Regular Expressions, in Haskell

This is by far the most successful Haskell experiment yet.  It builds on
Experiment 04, "Glushkov Regular Expressions," and adds the Semiring
implementation.

We use the familiar pattern of building our regular expressions using
the Kleene primitive pattern developed for Experiment 01, then lift the
constructed expression into our Gluskhov representation and run it
through a modified version of the 'shift' function to produce a result.
In this version, as in previous rigged versions, we apply the logic of
regular expressions to our semiring data during parsing.

One thing that was necessary here was that, to support more complex
semirings, those that are not just primitive data with simple zero or
one representations, I needed to provide a constructor to the shift
function that knew how to build new symbol operations.  When you "rig"
the Kleene representation, you must provide a function that takes a char
and returns a symbol operator that includes the semiring.

Rigging examples were *not* included in the paper.  This was the first
experiment where I had to come up with some parts of the solution on my
own, and solving it was a fun problem.  This particular version took
about four hours to puzzle out, but it was worth it.  I'm sure there are
alternatives to my rigging-with-constructor solution, but this works and
I'm not unhappy with it.  It does look a bit cluttered, but that's
actually how it's presented in the paper; my solution actually reduces
some of the clutter.

Otherwise, this version works pretty much the same way you'd expect a
merger of the Kleene Semiring version and the Glushkov boolean version
work.

One thing that came out of the paper was the use of a Haskell
record-type to record whether or not a node had already been analyzed
for its finality and emptiness; this caches those results and "shorts
out" traversing down the tree to rediscover these properties, resulting
in a bit of a speed-up.
