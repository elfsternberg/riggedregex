When [last we left our
hero](https://elfsternberg.com/2019/02/12/a-play-on-regular-expressions-part-3-the-first-haskell-to-rust-port/),
he had successfully implemented [Rigged Regular
Expressions](https://github.com/elfsternberg/riggedregex) in Rust.

There's been a lot of progress since then.

## Matt Might's Parse Result Data Type Is A Semiring!

One of my strongest intuitions when I started reading [*A Play on
Regular
Expressions*](https://www-ps.informatik.uni-kiel.de/~sebf/pub/regexp-play.html)
was that under the covers Matt Might's implementation of Brzozowski's
Regular Expressions used a data structure exactly like those in the
*Play* paper.  [I was
correct](https://github.com/elfsternberg/riggedregex/tree/master/haskell/05_RiggedBrz);
I was able to implement a (non-recursive) Brzozowski engine in Haskell,
and use a Semiring formalism to accurately reproduce Might's outcome and
data structure from [*Parsing With Derivatives: A Functional
Pearl*](http://matt.might.net/papers/might2011derivatives.pdf).

### And Might's parser-combinators are functors between semirings!

To me, this is something of a monumental discovery. Intellectually, I'm
happy to show that the data structures Might is using are semirings and
his parser-combinator approach is actually just
[functors](https://en.wikibooks.org/wiki/Haskell/Category_theory)
between semirings.  Personally, I'm *thrilled* that I was able to do
this in Haskell, writing code that is *not* from either paper to show
that the two have signficant overlap.  Even better, I was able to show
a different category of complexity can be encompassed by the semiring
properties from those in the *Play* paper.

## Semiring-as-a-trait in Rust

The first thing is that, in that last post, I discussed not having to
implement a Semiring trait in Rust, because I could abstract it further
using the `num_trait` crate to define my zero and one, and the
`std::ops` trait to define multiplication and addition.

Unfortunately, that didn't hold.  It worked fine when my values were
primitives such as Boolean or Integer, but when I implemented Matt
Might's sets of strings as a return value, the `std::ops`
implementations were insufficient.  To work as an operation, Rust
*requires* that the data must be bitwise copyable without loss of
fidelity; unfortunately, bitwise copying of sets-of-strings doesn't
work; the underlying implementation has much of its data distributed
across the heap.  So I had to implement the Semiring as a trait to
exploit Rust references and get high-performance operations, and it
[worked just
fine](https://github.com/elfsternberg/riggedregex/tree/master/rust/06_riggedglushkov).

## You can write Haskell (poorly) in any language

At one point developing the Rigged Brzozowski in Haskell implementation,
I was stuck on a hard-to-surface bug.  That whole bit about how "If it
compiles it's probably correct" Haskell superiority nonsense is
nonsense; there are lots of little fiddly bits in this algorithm ("Is
this a sum or product operation?" "Is the initializer one or zero?" "Is
the precedence correct?" "Is this test on the left or right operand?")
that can go wrong.

I [ported the implementation to
Python](https://github.com/elfsternberg/riggedregex/tree/master/python)
in order to litter it with print statements and watch it work.  This was
useful, and helped me track down the bug.  I was able to port the
working instance back to Haskell easily.

One thing to note is that the Python is rather... odd.  There are the
[six regex
operations](https://elfsternberg.com/2018/10/17/thinking-about-refactoring-barre-and-what-to-do-next/)
that can be performed on a string which are both unique and finite.  By
using `namedtuple` I was able to create the same structure as the
Haskell `data` or Rust `enum` operation, and with clever use of Python's
reflection capabilities I was likewise able to make Python do something
like Haskell or Rust's pattern-matching, using dictionaries.  The result
is, to my eye, pleasing, but I've been told it's "not very Pythonic."

Easier to maintain, at any rate.

## Are we done here?

That's... really the twelve thousand dollar question now, isn't it?
I've finished sections one and two; the third section is about adopting
this framework to recursive regular expressions, which I'm already
somewhat proficient in from working with Darais' Racket implementation.
So there are a couple of different ways I could go about this:

### I could just keep plugging away

I could proceed as I have been, implementing:

- Heavyweights using Brzozowski in Haskell
- Heavyweights using Brzozowski in Rust
- Recursive Regular Expressions in Haskell
- Recursive Regular Expressions in Rust
- Recursive Brzozowski Regular Expressions

### I could show it works in C++

I could combine what I did with the Python implementation and my
[limited](https://github.com/elfsternberg/SimpleGarbageCollector) ([very
limited](https://github.com/elfsternberg/beercode20150806)) C++
knowledge and try to port one of the Rigged Glushkov engines to C++.
The state-of-the-art in C++ unicode support looks absolutely terrifying,
though.

### I could add to the feature set: incremental regular expressions

One of the big changes I made in Rust was that, toward the end, I
changed the input value from an `Iteratable` to an `Iterator`, thus
simplifying the API.  I want to do the same thing for the output, that
is, I want the *receiver* to get not just a semiring containing a set,
but to get instead an iterator that produces elements from the semiring
as they're produced, in order.  I want to create an [incremental regular
expression](https://dl.acm.org/citation.cfm?id=357066).

### I could add to the feature set: compile-time regular expressions

In [the paper that started this whole
thing](http://www.ccs.neu.edu/home/turon/re-deriv.pdf), Owens, Reppy &
Turon showed (Section 3.3) that Brzozowski's algorithm can produce
static DFAs, and that high-performance compile-time regular expressions
are possible.  Combined with Rust's [Procedural
Macros](https://doc.rust-lang.org/book/first-edition/procedural-macros.html)
and the iterator ideas above, this could lead to static regular
expressions becoming a first-class data type next to the `container`
library.

### I could add to the feature set: compile-time *bitmapped* regular expressions

Fritz Henglein has a paper in which he discusses [Bit Coded Regular
Expressions](https://pdfs.semanticscholar.org/024b/75a6608d53edc0a72e8b106a4841751a5be6.pdf),
which look fairly adaptable.  BCRE requires that you not have character
classes in your regular expression library (no "\\w+", no "\\{Number}",
and no "." operator!), but in exchange what you get is an *insanely*
fast regular expression matching algorithm that stores and outputs its
results as a bitmap, which happens to make "I found X" vs "I found Y"
incredibly cheap on modern hardware; the engine knows which bitmap
corresponds to which sequence exactly.  This speed is exactly what you
need to analyze the terabytes of data that flow through a modern logging
engine.

### I could add to the feature set: extended regular expressions

There are four other operations that are known to work with regular
expressions: *Intersection* ("this AND that at the same time"),
*Negation* ("NOT that"), *Interleaf* ("This AND that AND that,
independent of order of appearance"), and *Cut* ("The biggest THIS").
The *Cut* operation can be fully simulated using the Semiring
implementation for the "longest submatch" (which can be adapted to
emulate *any* of the *Alternative* behaviors found in the while:
longest, atomic longest, first match, last match).  The *Interleaf*
operator is useful for parsing declarative languages in which elements
appear in any order (for example, HTML attributes), and *Intersection*
plus *Negation* are already commonplace in engines like PCRE.

Unfortunately, these fall outside of the the Semiring implementation.
That doesn't make them *bad*, I just don't have a good intellectual
grasp on how to implement them in a way that has a solid theoretical
foundation.  Even worse, there's some evidence that Intersection plus
Negation together create a parser that has some edge cases with gnarly
performance penalties.

### I could abstract Brzozowki's algorithm into a finite state transducer

More abstractly, Brzozowski's algorithm can actually be abstracted
(eek!) into a bidirectional scanner.  In some ways, a regular expression
is a set-membership function with a functor transforming the membership
determinant into something more useful than `true`-or-`false`.  If I'm
reading the paper [Clowns to the Left of Me, Jokers to the
Right](http://strictlypositive.org/CJ.pdf) correctly (no promises),
Conor McBride shows that you could start *anywhere* in an abstract
syntax tree, and Brzozowski's algorithm (really, any regular expression
algorithm, but Brzozowksi's seems easiest here, actually) could provide
derivatives of the left and right parts of the tree, producing a new
tree transformed according to a regular expression substitution rule on
the old tree.  Do this progressively enough, and you end up with a fully
functional tree transformer, or a very powerful abstraction of a [finite
state transducer](https://en.wikipedia.org/wiki/Finite-state_transducer)
that's fully explicable in Church terms, rather than the Turing terms
used in the Wikipedia article.

### I could finish Barre

Or I could just "go for it," and just start re-writing Barre with the
knowledge I've picked up working on these.  One of the big goals of
Barre is to implement Adams & Darais' "Language of Languages," a
superset of Kleene's base six regular expressions to make it easier to
build primitive parsers, giving you the "strap" part of bootstrapping
this implementation into a full-powered parser generator.

I just haven't decided which to do yet.


