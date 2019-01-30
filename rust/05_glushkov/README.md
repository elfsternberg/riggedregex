# Glushkov Regular Expressions, in Rust

This is a regular expression recognizer written using a dynamic version
of Glushkov's constructions.  Future nodes are built on-the-fly, as they
are in Brzozowski's algorithm.  This implementation is pretty much the
one from the paper mentioned in the root README.

Porting this from Haskell was *much* more straightforward than porting 
the straight regex versions, and is slightly more efficient, although 
it still has the "transition the entire parse tree every character" 
problem.  That's to be solved later.
