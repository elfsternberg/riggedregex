This is the classic implementation of Brzozowski's Algorithm with
Weighted Semirings.  There are very few optimizations in this code.  One
aspect that frustrates me is the use of the `Del()` operator; it's a
holdover from a time when I didn't quite understand the interaction
between regular expressions and semirings; its purpose is to preserve
the results of a lazy parsenull() of the sequence.  Later, we replace
that with a smarter algorithm.
