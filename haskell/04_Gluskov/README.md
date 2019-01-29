# Glushkov Regular Expressions, in Haskell

This is a regular expression recognizer written using a dynamic version
of Glushkov's constructions.  Future nodes are built on-the-fly, as they
are in Brzozowski's algorithm.  This implementation is pretty much the
one from the paper mentioned in the root README.  The final analysis is
a bit ugly and I'm not sure why the paper went that way with it.
