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

Currently written:
- [Kleene Booleann regular expressions in Haskell](./haskell/01_SimpleRegex) (Section I.1)
- [Kleene Boolean regular expressions in Rust](./rust/01_simpleregex) (Section I.1)
- [Rigged Kleene regular expressions in Haskell](./haskell/02_RiggedRegex) (Section I.2)
- [Rigged Kleene regular expressions in Rust](./rust/02_riggedregex) (Section I.2)
- [Glushkov construction of efficient Boolean regular expressions in Haskell](./haskell/04_Gluskov) (Section II.1a)
- [Glushkov construction of efficient Boolean regular expressions in Rust](./rust/05_glushkov), (Section II.1a)
- [Brzozowski's Boolean algorithm for regular expressions in Haskell](./haskell/03_Brzozowski)
- [Brzozowski's Boolean algorithm for regular expressions in Rust](./rust/03_brzozowski_1), with the main
algorithm in a central function.
- [Brzozowski's Boolean algorithm for regular expressions, in Rust](./rust/03_brzozowski_2), with
the main algorithm divvied among the implementation sub-handlers.
- [Rigged Kleene regular expressions in Haskell with Parse Forests](./haskell/06_RiggedRegex)
- [Rigged Glushkov Regular Expressions in Haskell](./haskell/07_Rigged_Glushkov) (Section II.1b)
- [Rigged Glushkov Regular Expressions in Rust](./rust/06_riggedglushkov) (Section II.1b)
- [Rigged Brzozowski's Regular Expressions in Haskell](./haskell/05_RiggedBrz/)
- [Rigged Brzozowski's Regular Expressions in Python](./python/)

## Todo

- Section II.2 of the paper (Substring matching)
- An abstraction of the Brzozowski algorithm, without an 'enum' dispatch type.
- Rigged implementations of the Brzozowski's algorithm, in Rust
- An exploration of whether or not extended regular expressions (regular
expressions supporting the Intersection, Negation, and Interleaf
operations) is theoretically sound.
- An implementation of a Rigged Brzozowski Algorithm with Adams's and
Darais's optimizations.
- An implementation of a Rigged Brzozowski Algorithm with Might's
recursion enabled, using Darais's laziness as its basis.

## Lessons learned

A major goal for this exercise is to illustrate the challenges, and
perhaps impossibilities, of porting idiomatic Haskell to idiomatic
Rust.  The regular expressions being built are static and compiled at
compile-time with the rest of the code.

The best thing I've learned is that many Haskell idioms do port over
easily to Rust.  Looking at the Rust version of `accept` in the first
two examples and comparing them to the Haskell, you can readily see just
how straightforward it translates.

On the other hand, Haskell's ability to define list processing
recursively really shines in the definitions of `split` and `parts`,
which are both three lines long in Haskell, but 21 and 29 lines long
respectively, in Rust.

The other thing is that the "Rigged Kleene regular expressions in
Haskell with Parse Forests" makes me unhappy; I don't have a good
theoretical model for the change I made.  My expectation is that "one"
here isn't just one, but also carries with it knowledge of what that
"one-ness" means; the zero/one relationship sustains as far as the 
poset is concerned, but the one value now carries knowledge the semiring
needs to assemble the resulting parse forest.  This is similar to Matt
Might's "smart epsilons," but I haven't gotten there yet.

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

