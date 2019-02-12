In the [last
post](https://elfsternberg.com/2019/01/23/a-play-on-regular-expressions-part-2/)
on "[A Play on Regular
Expressions](https://www-ps.informatik.uni-kiel.de/~sebf/pub/regexp-play.html),"
I showed how we go from a boolean regular expression to a "rigged" one;
one that uses an arbitrary data structure to extract data from the
process of recognizing regular expressions.  The data structure must
conform to a set of mathematical laws (the
[semiring](https://en.wikipedia.org/wiki/Semiring) laws), but that
simple requirement led us to some surprisingly robust results.

Now, the question is: Can we port this to Rust?

Easily.

The first thing to do, however, is to *not* implement a Semiring.  A
Semiring is a conceptual item, and in Rust it turns out that you can get
away without defining a Semiring as a trait; instead, it's a collection
of traits derived from the `num_traits` crate: `Zero, zero, One, one`;
the capitalized versions are the traits, and the lower case ones are the
implementations we have to provide.

I won't post the entire code here, but you can check it out in [Rigged
Kleene Regular Expressions in
Rust](https://github.com/elfsternberg/riggedregex/tree/master/rust/02_riggedregex).
Here are a few highlights:

The `accept()` function for the Haskell version looked like this: 

    acceptw :: Semiring s => Regw c s -> [c] -> s
    acceptw Epsw u     = if null u then one else zero
    acceptw (Symw f) u = case u of [c] -> f c;  _ -> zero
    acceptw (Altw p q) u = acceptw p u `add` acceptw q u
    acceptw (Seqw p q) u = sumr [ acceptw p u1 `mul` acceptw q u2 | (u1, u2) <- split u ]
    acceptw (Repw r)   u = sumr [ prodr [ acceptw r ui | ui <- ps ] | ps <- parts u ]

The `accept()` function in Rust looks almost the same:

    pub fn acceptw<S>(r: &Regw<S>, s: &[char]) -> S
        where S: Zero + One
    {
        match r {
            Regw::Eps => if s.is_empty() { one() } else { zero() },
            Regw::Sym(c) => if s.len() == 1 { c(s[0]) } else { zero() },
            Regw::Alt(r1, r2) => S::add(acceptw(&r1, s), acceptw(&r2, s)),
            Regw::Seq(r1, r2) => split(s)
                .into_iter()
                .map(|(u1, u2)| acceptw(r1, &u1) * acceptw(r2, &u2))
                .fold(S::zero(), sumr),
            Regw::Rep(r) => parts(s)
                .into_iter()
                .map(|ps| ps.into_iter().map(|u| acceptw(r, &u)).fold(S::one(), prod))
                .fold(S::zero(), sumr)
        }
    }

There's a bit more machinery here to support the `sum`-over and
`product`-over maps.  There's also the `where S: Zero + One` clause,
which tells us that our Semiring must be something that understands
those two notions and have implementations for them.

To restore our boolean version of our engine, we have to build a nominal
container that supports the various traits of our semiring.  To do that,
we need to implement the methods associated with `Zero`, `One`, `Mul`,
and `Add`, and explain what they mean to the datatype of our semiring.
The actual work is straightforward.

    pub struct Recognizer(bool);

    impl Zero for Recognizer {
        fn zero() -> Recognizer { Recognizer(false) }
        fn is_zero(&self) -> bool { !self.0 }
    }

    impl One for Recognizer {
        fn one() -> Recognizer { Recognizer(true) }
    }

    impl Mul for Recognizer {
        type Output = Recognizer;
        fn mul(self, rhs: Recognizer) -> Recognizer { Recognizer(self.0 && rhs.0) }
    }

    impl Add for Recognizer {
        type Output = Recognizer;
        fn add(self, rhs: Recognizer) -> Recognizer { Recognizer(self.0 || rhs.0) }
    }

Also, unlike Haskell, Rust must be explicitly told what kind of Semiring
will be used before processing, whereas Haskell will see what kind of
Semiring you need to produce the processed result and hook up the
machinery for you, but that's not surprising.  In Rust, you "lift" a
straight expression to a rigged one thusly:

    let rigged: Regw<Recognizer>  = rig(&evencs);

All in all, porting the Haskell to Rust was extremely straightforward.
The code looks remarkably similar, but for one detail.  In the Kleene
version of regular expressions we're emulating as closely as possible
the "all possible permutations of our input string" implicit in the
set-theoretic language of Kleene's 1956 paper.  That slows us down a
lot, but in Haskell the code for doing it was extremely straightforward,
which two simple functions to create all possible permutations for both
the sequence and repetition options:

    split []     = [([], [])]
    split (c:cs) = ([], c : cs) : [(c : s1, s2) | (s1, s2) <- split cs]
    parts []     = [[]]
    parts [c]    = [[[c]]]
    parts (c:cs) = concat [[(c : p) : ps, [c] : p : ps] | p:ps <- parts cs]

In Rust, these two functions were 21 and 29 lines long, respectively.
Rust's demands that you pay attention to memory usage and the rules
about it require that you also be very explicit about when you want it,
so Rust knows exactly when you no longer want it and can release it back
to the allocator.

Rust's syntax and support are amazing, and the way Haskell can be ported
to Rust with little to no loss of fidelity makes me happy to work in
both.
