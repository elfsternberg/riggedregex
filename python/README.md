# Python Experiments!

This directory contains some simple experiments, in Python.  Python is,
frankly, easier to instrument than Haskell, so figuring out the
underlying operation and stepping through it with pdb, can sometimes be
easier to do in Python3

`01_rigged_brzowoski.py`: A naive implementation of Brzozowski's regular
expression library, using the `Delta` operator to distinguish between
nullable and not-nullable branches of the `Sequence` operator.  What's
remarkable about it, if anything, is just *how much* it resembles
Haskell Experiment 05: Rigged Brzozowski Regular Expressions.  Part of
that is using the `namedtuple` as an easy hack for Haskell's data
constructors, and then implementing the `derive()` and `parsenull()`
functions using map functions as a substitute for Haskell's pattern
matching.

This is mostly proof that "One can write Haskell poorly in any
language."
