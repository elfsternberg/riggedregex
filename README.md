![Language: Haskell](https://img.shields.io/badge/language-Haskell-yellowgreen.svg)
![Language: Rust](https://img.shields.io/badge/language-Rust-green.svg)
![Topic: Academic](https://img.shields.io/badge/topic-Academic-red.svg)
![Topic: Regular Expressions](https://img.shields.io/badge/topic-Regular_Expressions-red.svg)
![Status: In Progress](https://img.shields.io/badge/status-In_Progress-yellow.svg)

# Rigged Regular Expressions

The two main code branches, the `rust` and `haskell` branches, contain
versions of Weighted Regular Expression as described in the paper *A
Play on Regular Expressions: A Functional Pearl*
([paper](http://sebfisch.github.io/haskell-regexp/regexp-play.pdf),
[slides](http://sebfisch.github.io/haskell-regexp/regexp-talk.pdf)), by
Sebastian Fischer, Frank Huch, and Thomas Wilke.  The early Haskell
versions are faithful to the paper, and are more or less what's
presented in the paper, entered by hand (as that's pretty much the only
way anything gets into my brain these days).  The Rust versions are a
ports of the Haskell version, inefficiencies and all, in order to see
what it takes to take academic results and reimplement them in what is
presented as a "systems" language.

The paper uses the term "weighted" to describe the post-processing they
do with an algebraic construct known as a
[*semiring*](https://en.wikipedia.org/wiki/Semiring), which exploits
categorical similarities between the set of booleans, the set of
ordinary numbers, and other data structures to turn analyzing the return
value of a successful recognition into a parseable dataset all its own.
I'm using the word "rig" as it's shorter; a semiring is related to the
algebraic construct called a "ring," but cannot handle **n**egative
elements, and so... a "rig."

I'm interested in the ideas of [Fritz Henglein's
dream](http://www.cs.ox.ac.uk/ralf.hinze/WG2.8/27/slides/fritz.pdf) of
extracting all possible parsing information via a catamorphism over the
parse tree, and that's exactly what semiring does.  I'm evaluating the
use of the semiring over [Might's parser combinator
approach](http://matt.might.net/papers/might2011derivatives.pdf) to see
which is easier to implement and provides the most bang for the buck. So
having regular expressions "rigged" just seemed like the right term.

Later implementations in both branches apply the *Play* strategies to
Brzozowski's "derivatives of regular expressions" algorithm, and
demonstrates that the results hold.  

**This proves what I set out to show**, that [Goodman's 1979 paper on
Semiring Parsing](http://www.aclweb.org/anthology/J99-4004) and
[Brzozowski's Parsing With
Derivatives](http://www.ccs.neu.edu/home/turon/re-deriv.pdf) are
compatible, and that these results are expressible in a strongly typed
systems language like [Rust](https://www.rust-lang.org/).

Primary mission accomplished.

## Secondary mission

While the primary goal is complete, the entirety of the project is not.
There are several things left until I consider this project done.  They
are:

- Implement *recursive* regular expressions
- Apply the [equational optimizations](https://arxiv.org/pdf/1604.04695.pdf) for efficiency.
- Supply an *aggregating semiring* so that multiple and correlative
semantic extractions can happen at the same time.
- Supply Adams's [LoL: Language of
Languages](https://bitbucket.org/ucombinator/derp-3/src/86bca8a720231e010a3ad6aefd1aa1c0f35cbf6b/src/derp.rkt?at=master&fileviewer=file-view-default#derp.rkt-489)
as an interface layer to a richer parsing experience.

These are probably separate projects, as they're specific meta-syntactic
wrappers around the final version:

- Use the semiring implementation to provide a
[PEG](https://arxiv.org/abs/1801.10490) engine
- Use the PEG to provide [Rosie in Rust](https://rosie-lang.org/about/)

And then there's a reality that [Russ
Cox](https://swtch.com/~rsc/regexp/regexp1.html) and [Geoff
Langdale](https://branchfree.org/2019/02/28/paper-hyperscan-a-fast-multi-pattern-regex-matcher-for-modern-cpus/)
are so far ahead of me that it's not even funny.

## Status

The paper describes itself as "a play," and is presented in three acts;
the first act has two scenes, the second and third have three scenes.
The end of each scene describes a regular expression engine of some
efficiency and capability.  In total there are eight different regular
expression engines implemented.

Currently written:
- [Kleene Booleann regular expressions in Haskell](./haskell/01_SimpleRegex) (Section I.1)
- [Kleene Boolean regular expressions in Rust](./rust/01_simpleregex) (Section I.1)
- [Kleene Boolean regular expressions in Typescript](./node/) (Section I.1)
- [Rigged Kleene regular expressions in Haskell](./haskell/02_RiggedRegex) (Section I.2)
- [Rigged Kleene regular expressions in Rust](./rust/02_riggedregex) (Section I.2)
- [Glushkov construction of efficient Boolean regular expressions in Haskell](./haskell/04_Gluskov) (Section II.1a)
- [Glushkov construction of efficient Boolean regular expressions in Rust](./rust/05_glushkov), (Section II.1a)
- [Brzozowski's Boolean algorithm for regular expressions in Haskell](./haskell/03_Brzozowski)
- [Brzozowski's Boolean algorithm for regular expressions in Rust](./rust/03_brzozowski_1), with the main algorithm in a central function.
- [Brzozowski's Boolean algorithm for regular expressions, in Rust](./rust/03_brzozowski_2), with the main algorithm divvied among the implementation sub-handlers.
- [Rigged Kleene regular expressions in Haskell with Parse Forests](./haskell/06_RiggedRegex)
- [Rigged Glushkov Regular Expressions in Haskell](./haskell/07_Rigged_Glushkov) (Section II.1b)
- [Rigged Glushkov Regular Expressions in Rust](./rust/06_riggedglushkov) (Section II.1b)
- [Rigged Brzozowski's Regular Expressions in Haskell](./haskell/05_RiggedBrz/)
- [Rigged Brzozowski's Regular Expressions in Python](./python/)
- [Rigged Brzozowski's Regular Expressions in **Rust**](./rust/08_riggedbrz/)
- [The Heavyweight Experiments: Glushkov Regex in Haskell with Disambiguation](./haskell/08_Heavyweights) (Section II.2)
- [The Heavyweight Experiments: Glushkov Regex in Rust with Disambiguation](./rust/07_heavyweights) (Section II.2)

## Todo

- An exploration of whether or not extended regular expressions (regular
expressions supporting the Intersection, Negation, and Interleaf
operations) is theoretically sound.
- An implementation of a Rigged Brzozowski Algorithm with Adams's and
Darais's optimizations.
- An implementation of a Rigged Brzozowski Algorithm with Might's
recursion enabled, using Adams's laziness as its basis.

## Lessons learned

A major goal for this exercise is to illustrate the challenges, and
perhaps impossibilities, of porting idiomatic Haskell to idiomatic Rust.
The regular expressions being built are static and compiled at
compile-time with the rest of the code.

The best thing I've learned is that many Haskell idioms do port over
easily to Rust.  Looking at the Rust version of `accept` in the first
two examples and comparing them to the Haskell, you can readily see just
how straightforward it translates.

On the other hand, Haskell's ability to define list processing
recursively really shines in the definitions of `split` and `parts`,
which are both three lines long in Haskell, but 21 and 29 lines long
respectively, in Rust.

<s>The other thing is that the "Rigged Kleene regular expressions in
Haskell with Parse Forests" makes me unhappy; I don't have a good
theoretical model for the change I made.</s> It turns out my theoretical
model was just fine; I just didn't know or understand it sufficiently at
the time.  The expectation is that yes, you can return an annihilator,
or you can return a value.  The default value is "one," but the meaning
of "one" can be whatever you want it to mean, so long as the Semiring
laws hold for the definitions you invent.  In short, the value treats
the zero and one as "annihilator" and "identity" operations against your
return value.  When the value is primitive, such as in the Boolean or
Integer case, all you need are the zero and one; when the value is
compositional, you need the `mul` operator to do the dirty work for you.

And then you remove the 'identity' pass because, well, the CPU doesn't
need to waste time on that.

## LICENSE 

The Haskell code in Haskell Experiments 01, 02, and 04 is straight from
the *Play* paper and therefor belongs to Sebastian Fischer, Frank Huch,
and Thomas Wilke.  The code in Haskell Experiments 07 and 08 take the
fragments offered in the same paper and realize them as full-blown
recognizers and parsers.

All other code is Copyright [Elf M. Sternberg](https://elfsternberg.com)
(c) 2019, and licensed with the Mozilla Public License vers. 2.0.  A
copy of the license file is included in the root folder.
