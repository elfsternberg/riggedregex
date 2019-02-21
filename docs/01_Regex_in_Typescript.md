Denis Kyashif recently wrote a piece called "[Implementing a Regular
Expression
Engine](https://deniskyashif.com/2019/02/17/implementing-a-regular-expression-engine/),"
and showed how to do it in Javascript.  It's a good piece, and I
recommend it.  Kyashif does a very good job of describing regular
expressions from a Turing-theoretic approach using finite automata, but
there's another way to think about regular expressions, and that's from
the Church-theoretic approach.  And since I've implemented
[<s>sixteen</s> *seventeen*
regular expression engines](https://github.com/elfsternberg/riggedregex)
in a variety of languages from a primarily Church-theoretic approach,
the approach originally used by Stephen Kleene in 1956, I'm going to
cover how to think about regular expressions from that direction.

## Alphabets, Languages, Primitives and Composites

You'll notice in the DFA approach that rarely does one ask the question,
"What is a regular expression expressing, and about what?"  The "about
what" is easier to explain: it's called a *regular language* and is a
set of *strings* composed out of a finite alphabet of *symbols*.  The
most common symbols are just character sets, and the most common of
those are the ASCII Set and the Unicode Standard.

The set of strings in a regular language can be finite and concrete.  In
the common DSL known as *regex*, "foo" is a regular language of one
string.  "foo|bar" is a regular language of two strings.  Or they can be
infinite: "(foo|bar)*" is a regular language with an infinite number of
strings that consist of the repetition of the previous example:
"foobarbarbarfoo" and "barfoobarfoofoo" are both in that language.

In programming terms, a regular expression is a function that takes a
string and returns a boolean indicating whether or not that string is a
member of a specific regular language.  That function is composed out of
six other functions, three of which are called the *primitives* and
three of which are *composites* built out of other regular expressions.
All of these functions are themselves regular expressions, and have the
same inputs and outputs.

- `null(s)`: Always returns False
- `empty(s)`: Is the string empty?
- `sym<sub>c</sub>(s)`: Constructed with the symbol `c`, does the string
consist of (and only of) the symbol `c`?
- `alt<sub>r1,r2</sub>(s)`: Constructed out of two other regular
expressions, and true only if `s` matches either.
- `seq<sub>r1,r2</sub>(s)`: Constructed out of two other regular
expressions, and true only if `s` consists of the first expression
immediately followed by the second.
- `rep<sub>r1</sub>(s)`: Constructed out of another regular expression,
true if `s` consists of zero or more repeated instances of that
expression.

Every regular expression is a tree of these sub-expressions, and given a
string, it starts at the top of the tree and works its way down the
chain of functions until it determines the truth proposition "is this
string a member of the language described by this regular expression?"

## Expressing regular expressions Kleene-ly

In Haskell, it's possible to turn Kleene's formula directly into source
code, and from here, it's possible to convert this directly to
Javascript.  This is the Haskell version:

    data Reg = Emp | Sym Char | Alt Reg Reg | Seq Reg Reg | Rep Reg
    
    accept :: Reg -> String -> Bool
    accept Emp u       = null u
    accept (Sym c) u   = u == [c]
    accept (Alt p q) u = accept p u || accept q u
    accept (Seq p q) u = or [accept p u1 && accept q u2 | (u1, u2) <- split u]
    accept (Rep r) u   = or [and [accept r ui | ui <- ps] | ps <- parts u]
    
    split :: [a] -> [([a], [a])]
    split []     = [([], [])]
    split (c:cs) = ([], c : cs) : [(c : s1, s2) | (s1, s2) <- split cs]
    
    parts :: [a] -> [[[a]]]
    parts []     = [[]]
    parts [c]    = [[[c]]]
    parts (c:cs) = concat [[(c : p) : ps, [c] : p : ps] | p:ps <- parts cs]
    
Those `split` and `parts` functions are necessary to express the
sequence (r1 followed by r2) operator from Kleene's original math:

> `L[[r · s]] = {u · v | u ∈ L[[r]] and v ∈ L[[s]]}`

Those membership tests are universal: *every possible combination* of
sequences in the string being tested must be tested.  `split()` takes
every possible substring and decomposes it into a list of all possible
pairs of strings, so that `Seq{}` can be compared against them.
`parts()` goes even further, devolving every possible substring into the
powerset of lists of strings, so that `Rep{}` can be compared to every
possible variation of the ordered input string.  Mathematically, this is
elegant and sensible; computationally, it's inefficient and ridiculous;
a string of `n` letters requires `2<sup>n-1</sup>` tests!

## Doing It In <s>Javascript</s> Typescript

Porting the Kleene version of this to Javascript was difficult only
insofar as Javascript is notorious about copying vs. referencing,
especially when it comes to heavily nested arrays like those used
above.  The ports of `split` and `parts` were also significantly more
complex, although Typescript's type system was of enormous help in
sorting out what was happening each step of the way.

The conversion is straightforward because the Haskell doesn't use any
higher-kinded types: no applicatives, no functions, and certainly no
monads!

The datatype `Reg` in Haskell, along with a couple of convenience
factory functions, becomes:

    interface Regcom { kind: string };
    class Eps implements Regcom { kind: "eps"; };
    class Sym implements Regcom { kind: "sym"; s: string; }
    class Alt implements Regcom { kind: "alt"; l: Regex; r: Regex };
    class Seq implements Regcom { kind: "seq"; l: Regex; r: Regex };
    class Rep implements Regcom { kind: "rep"; r: Regex };
    
    function eps():                   Eps { return { kind: "eps" }; };
    function sym(c: string):          Sym { return { kind: "sym", s: c }; };
    function alt(l: Regex, r: Regex): Alt { return { kind: "alt", l: l, r: r }; };
    function seq(l: Regex, r: Regex): Seq { return { kind: "seq", l: l, r: r }; };
    function rep(r: Regex):           Rep { return { kind: "rep", r: r }; };
    
    type Regex = Eps | Sym | Alt | Seq | Rep;

And the `accept` looks remarkably similar.  The `some()` and `every()`
methods on Arrays were especially useful here, as they implement the
same behavior as `and` and `or` over Haskell lists.

    function accept(r: Regex, s: string): boolean {
        switch(r.kind) {
        case "eps":
            return s.length == 0;
        case "sym":
            return s.length == 1 && r.s == s[0];
        case "alt":
            return accept(r.l, s) || accept(r.r, s);
        case "seq":
            return split(s).some((v: Array<string>) => accept(r.l, v[0]) && accept(r.r, v[1]));
        case "rep":
            return parts(s).some((v: Array<string>) => v.every((u: string) => accept(r.r, u)));
        }
    }
    
`split()` required a significant amount of munging to make sure the
arrays were copied and not just referenced, but looks much like the
Haskell version:

    function split(s: string) {
        if (s.length == 0) {
            return [["", ""]];  
        }
        return [["", s.slice()]].concat(
            split(s.slice(1)).map(
                (v) => [s[0].slice().concat(v[0].slice()), v[1].slice()]));
    }
    
`parts()`, too, learns a lot from the Haskell version:

    function parts(s: string): Array<Array<string>> {
        if (s.length == 0) {
            return [[]];
        }
    
        if (s.length == 1) {
            return [[s]];
        }
    
        let c = s[0];
        let cs = s.slice(1);
        return parts(cs).reduce((acc, pps) => {
            let p: string  = pps[0];
            let ps: Array<string> = pps.slice(1);
            let l:  Array<string> = [c + p].concat(ps);
            let r:  Array<string> = [c].concat(p).concat(ps);
            return acc.concat([l, r]);
        }, [[]]).filter((c) => c.length != 0);
    }

You use this code in a straightforward fashion:

    let nocs = rep(alt(sym("a"), sym("b")));
    let onec = seq(nocs, sym("c"));
    let evencs = seq(rep(seq(onec, onec)), nocs);
    console.log(accept(evencs, "abcc") == true);           // "true"
    console.log(accept(evencs, "abccababbbbcc") == true);  // "true
     
## Wrap up

Most regular expression "under the covers" tutorials come from a
Turing-theoretic approach, describing the finite automata that
transition from state-to-state, ultimately ending up somewhere in a
table with a flag that says "This is an accept state."

I approached this from a Church-theoretic approach.  The [Church-Turing
Thesis](https://en.wikipedia.org/wiki/Church%E2%80%93Turing_thesis) says
that these two approaches are equivalent, but use different notation.
Turing's approach is mechanical and engineering oriented; Church's
approach and notation are mathematical.

Stephen Kleene's original 1956 paper on Regular Expressions was
primarily written from a Church-theoretic approach, and I showed that
this approach can legitimately, if inefficiently, be implemented in an
ordinary programming language like Javascript.  I showed how Kleene's
six basic operations can be composed together to create complete and
effective regular expressions.

The code for this Typescript implementation, the eight other Haskell
variants, the seven other Rust variants, and one Python variant, are all
[available on Github](https://github.com/elfsternberg/riggedregex/).
