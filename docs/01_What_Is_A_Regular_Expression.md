# What is a regular expression?

So what *is* a regular expression?  Let's build up from the bottom: we
start with:

Alphabet
: An alphabet is a set of symbols (or we can call them letters)

Word
: A word is a sequence of symbols from an alphabet

Language
: A language is a set of word sequences

If our alphabet is ASCII and words are English, then a very simple
language would be something like 

    Common_Pets: {dog, cat, fish, hamster, parakeet}.

Stephen Cole Kleene proposed a formal definition for "regular languages"
in 1959, and what we have developed since then is a series of
refinements that allow us to parse regular languages in something like
linear time.  Kleene's operations were meant to *generate* languages,
and the research program since that time has been to turn generators
into recognizers.  But let's start with Kleene's generators.

## Regular Languages

There are six basic operators in a regular language, and each of them is
itself a regular language.  The first three are the base languages,
encoding the "zero," "one," and "element" of the regular language, and
the second three are composite languages; they contain other regular
languages (including other composites) to describe a complete
generator.

Given an alphabet, `A`, we can say:

`L[[∅]] = ∅`
: A language that contains nothing is made up of nothing.

`L[[ε]] = {ε}` 
: A language containing only the empty string can only
generate empty strings.

`L[[a]] = {a}`
: A language containing only the letter 'a' can only generate a single
instance of the letter 'a'.  (This is true for all letters in the
alphabet.)

`L[[r · s]] = {u · v | u ∈ L[[r]] and v ∈ L[[s]]}`
: A composite language made up of the *sequence* of two other regular
expressions `r` and `s` can generate any tuple `uv` for every `u`
generated by `r` and every `v` generated by `s`.

`L[[r | s]] = L[[r]] ∪ L[[s]]`
: A composite language made up of the *alternatives* of two other
regular expressions `r` and `s` can generate either the strings of `r`
or the strings of `s`, or both!

`L[[r∗]] = {ε} ∪ L[[r · r*]]`
: A composite language that repeats `r` zero or more times can generate
zero or more instances of the strings generated by `r`.

## Regular Expressions

What we usually think of as "regular expressions" are in fact a small
programming language designed to be parsed and to internally generate a
function that recognizes whether or not a string "belongs to" the sets
of strings described by a Kleene Algebra.  Programatically, a regular
expression is a function that takes a regular language and a string, and
returns back a boolean value indicating whether or not the string
belongs to the set of strings described by the regular language.

Regular expressions take Kleene's algebra and turn it backwards, asking
"Can this given string be generated by an expression in Kleene's
algebra?"  In both the Rust and Haskell branches you'll find the
SimpleRegex implementations, which take this quite literally.  The
Haskell version is the most concise; it literally encodes Kleene's five
generative operations (the language of null doesn't generate anything)
and *all possible combinations of `r` and `s` for any given composite
language* and then tests all those combinations to see if the expression
generated any of them.

This is, of course, inexcusably slow.  For any string of length `n`, the
number of comparisons done, thanks mostly to the Sequence composite, is
2<sup>n-1</sup> operations.  For a string of 8 letters, that's 256
different combinations of strings that have to be matched, and on my
fairly modern laptop that takes a little longer than 20 seconds.
Increase that to 15 letters and you'll be waiting almost an hour.

The entirety of the modern parsing research program has been to make
this faster and easier to use.  There have been many attempts, and this
project isn't meant to break new ground; instead, its goal is to take
promising results from a variety of different academic research projects
and explore whether there's anything new and interesting that we can
exploit in a modern systems language like Rust or C++.

