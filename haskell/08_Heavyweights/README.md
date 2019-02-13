# Rigged Glushkov Regular Expressions in Haskell: Compliance Experiments

This implementation doesn't differ much from [Experiment 07: Rigged
Glushkov Regular Expressions in Haskell](../07_Rigged_Glushkov), except
that it adds two new Semiring implementations to the library.

Recall the basics of Semiring theory: There is a zero, a one, an
"addition" operation and a "multiplication" operation.  These two
operators have identities (in numbers, addition has zero, multiplication
has one) and the operators behave similarly (multiplication times zero
always equals zero, or "nothing."), and a data type on which these
operations work.

We've used these principles to do boolean recognition; multiplication is
the boolean `and` operator, used to encode sequences using annihilation:
any sequence that doesn't match is `False`, and `False && x` is always
`False`.  If the entire truth of an expression depends on an annihilated
sequence, then it's not true.

We've used it to count ambiguities, via integers: by using addition *as*
addition, we count the number of different regular languages encoded in
our initial expression that could have produced the string submitted,
thus revealing the number of ambiguities in our expression.  Each `or`
that returns 1 reveals a different path, so an alternate pattern will
return the sum of the paths that pass through it.

And we've even used it to identify the strings that match.  By saying
that our Semiring is a "set of strings", our addition is union (that is,
we keep the set of all paths through the alternate pattern), and
multiplication is the concatenation of the cartesian products of the two
elements of a sequence (so for a basic pattern with no alternatives it's
just a concatenation of the two strings, but for alternatives with
multiples, it's the concatenation of all possible combinations), we've
created a way to extract the exact string(s) that we submitted to the
machine that matched.

In *Heavyweights*, Fischer, Huch and Wilke go further and show how
clever choices among zeros and ones can lead to some rather powerful
outcomes.

The first thing to appreciate is that our symbol operator, `sym`, has
never actually been about symbols.  It's about predicates.  Our base
implementation has been to pass a closured comparison with our desired
symbol, returning zero or one.

For the string implementation, which is *not* covered in the paper and
which I managed to extract, successfully, from Might's work, I passed to
`sym` instead a closured comparison to the desired symbol, and the
return value was either the zero or `singleton [c]`, meaning a set with
a string of one character in it.  (I'm quite proud of that work; it both
affirmed my notion that Might & Adams had a semiring implementation,
they just didn't call it that, and that I was able to merge two
different equational systems, applying some notions of category theory
to do so.)

The definition of `sym` was: `sym :: (Semiring s, Eq c) => c -> Reg c
s`.  I added `syms`: `syms :: Char -> Reg Char (Set String)`.  Now the
three provide `symi`: symi :: Semiringi s => Char -> Reg (Int, Char) s`
This is a semiring that *takes* both an Int and a Char, and their
`accept` method `zip`s the input value with a position value, so that
both are available for processing.  Remember that everything else
depends on the Semiring, and *not* the input type; only `sym` cares.

Now they add an "indexed semiring," and to it provide a version of `sym`
that returns the `index` semiring when true, and zero otherwise.

    class Semiring s => Semiringi s where
        index :: Int -> s

    symi :: Semiringi s => Char -> Glue (Int, Char) s
    symi c = symw weight
        where weight (pos, x) | x == c    = index pos
                              | otherwise = zero

But what *is* the `index` semiring?  Here's where things get
interesting.  Fischer, et. al., want to encode the length of the longest
submatch.  The first thing they do is define submatch as a variant of
accept, with a lead-in that just matches everything.  This is okay, as
this is a Glushkov machine and that just means that the 'arb' NFA will
almost always be active, but it won't be important to us, it's not
working with `symi` values.

    submatch :: Semiring s => Glue (Int, c) s -> [c] -> s
    submatch r s =
        accept (seqw arb (seqw r arb)) (zip [0..] s)
            where arb = repw (symw (\_ -> one))

So... what are the zero and one of a "longest submatch" operation?  The
zero is that no match ever occurred.  The one is that a match is
possible, but hasn't yet occurred.  Any other value is a submatch.  The
final value is the longest interval of the submatch.

Fischer, et al. break up their semiring into two parts:

    data LeftLong = NoLeftLong | LeftLong Range deriving (Show)
    data Range = NoRange | Range Int Int deriving (Show)
    
`NoLeftLong` is zero; it could never happen, there was no match.
`NoRange` is the one, meaning it could still happen, it just hasn't
yet.  And `Range` is a submatch that has been found.

For addition (which symbolizes alternation, recall), adding a failure to
anything is the anything, no `add NoLeftLong x = x`, and that's true the
other way.  Adding a range with an empty range is just the range, and
adding two ranges is to pick the longer of the two.

For multiplication, again, multiplying by failure is just failure.
Multiplying anything with `NoRange` means that the anything is preserved
unchanged, and multiplying two ranges is a new range with the start of
the first range and the end of the latter range. (Recall that for
Semirings, the operations are associative but they are *not*
commutative.  They may *be* commutative for some sets, but it's not a
requirement of semirings and you shouldn't count on commutativity.)

