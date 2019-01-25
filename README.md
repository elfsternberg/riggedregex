![Language: Haskell](https://img.shields.io/badge/language-Haskell-yellowgreen.svg)
![Language: Rust](https://img.shields.io/badge/language-Rust-green.svg)
![Topic: Academic](https://img.shields.io/badge/topic-Academic-red.svg)
![Topic: Regular Expressions](https://img.shields.io/badge/topic-Regular_Expressions-red.svg)
![Status: In Progress](https://img.shields.io/badge/status-In_Progress-yellow.svg)

# Rigged Regular Expressions

The two code branches contain versions of Weighted Regular Expression as
described in the paper *A Play on Regular Expressions: A Functional
Pearl*
([paper](http://sebfisch.github.io/haskell-regexp/regexp-play.pdf),
[slides](http://sebfisch.github.io/haskell-regexp/regexp-talk.pdf)), by
Sebastian Fischer, Frank Huch, and Thomas Wilke.  The Haskell version is
faithful to the paper, and is more or less what's presented in the
paper, entered by hand (as that's pretty much the only way anything gets
into my brain these days).  The Rust version is a port of the Haskell
version, inefficiencies and all, in order to see what it takes to take
academic results and reimplement them in what is presented as a
"systems" language.

The paper uses the term "weighted" to describe the post-processing they
do with an algebraic construct known as a
[*semiring*](https://en.wikipedia.org/wiki/Semiring), which exploits
categorical similarities between the set of booleans, the set of
ordinary numbers, and other data structures to turn analyzing the return
value of a successful recognition into a parseable dataset all its own.
I'm using the word "rig" as it's shorter; a semiring is related to the
algebraic construct called a "ring," but cannot handle **n**egative
elements, and so... a "rig."

I'm hip to the idea of [Fritz Henglein's
dream](http://www.cs.ox.ac.uk/ralf.hinze/WG2.8/27/slides/fritz.pdf) of
extracting all possible parsing information via a catamorphism over the
parse tree, and that's exactly what semiring does.  I'm evaluating the
use of the semiring over [Might's parser combinator
approach](http://matt.might.net/papers/might2011derivatives.pdf) to see
which is easier to implement and provides the most bang for the buck. So
having regular expressions "rigged" just seemed like the right term.

## Status

The paper describes itself as "a play," and is presented in three acts;
the first act has two scenes, the second and third have three scenes.
The end of each scene describes a regular expression engine of some
efficiency and capability.  In total there are eight different regular
expression engines implemented.

Right now, the Haskell version implements the first two.  The Rust
version only implements the first.  That the Rust version works at all
is somewhat amazing.

Both versions are hideously inefficient, in that they do no epsilon
shorting at all and so every possible combination of string and
expression is analyzed.  For a string only eight letters long, the
analysis takes about 30 seconds on my laptop; push that to twelve
letters, and you'll be waiting an hour.

## Lessons learned

A major goal for this exercise is to illustrate the challenges, and
perhaps impossibilities, of porting idiomatic Haskell to idiomatic
Rust.  The regular expressions being built are static and compiled at
compile-time with the rest of the code.

The best thing I've learned is that many Haskell idioms do port over
easily to Rust.  Looking at the Rust version of `accept` and comparing
it to the Haskell above, you can readily see just how straightforward it
translates.

On the other hand, Haskell's ability to define list processing
recursively really shines in the definitions of `split` and `parts`,
which are both three lines long in Haskell, but 21 and 29 lines long
respectively, in Rust.

## LICENSE 

The Rust code is Copyright [Elf M. Sternberg](https://elfsternberg.com) (c) 2019.

The Haskell code as presented so far is straight from the paper and
therefor belongs to Sebastian Fischer, Frank Huch, and Thomas Wilke.  In
the event that I choose an alternative DFA constructor (and I probably
will, because they use [Glushkov's
Construction](https://en.wikipedia.org/wiki/Glushkov%27s_construction_algorithm),
and I'm trying to use [Brzozowski's
Algorithm](https://en.wikipedia.org/wiki/Brzozowski_derivative#Derivative_of_a_regular_expression)),
the files will be annotated to indicate which is which.

