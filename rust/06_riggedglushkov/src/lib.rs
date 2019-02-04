//! This crate provides a series of simple functions for building a
//! regular expression, and an `accept` function which takes a
//! completed regular expression and a string and returns a boolean
//! value describing if the expression matched the string (or not).
//!

extern crate num_traits;
use num_traits::{one, zero, One, Zero};
use std::collections::HashSet;

use std::ops::Deref;
use std::ops::{Add, Mul};
use std::rc::Rc;

/// Implementors of this code MUST implement both the `Rmul` and `Mul`
/// versions, and the `Radd` and `Add` versions, of these operations,
/// to support semiring processing correctly.  Rust's ops traits
/// cannot, to the best of my knowledge, handle references, and
/// thrashing a lot of clones of large parse trees about was, shall we
/// say, sub-optimal.

pub trait Rmul<ITY=Self> {
    fn rmul(lhs: &ITY, rhs: &ITY) -> ITY;
}

pub trait Radd<ITY=Self> {
    fn radd(lhs: &ITY, rhs: &ITY) -> ITY;
}

/// The Sym trait represents what to do for a single character.  It has
/// a single method, "is", that returns the semiring.  Implementers of
/// "is" must provide a corresponding construction factory.
pub trait Sym<S> 
where
    S: Zero + One + Clone + Rmul + Radd
{
    fn is(&self, c: char) -> S;
}

pub enum Glui<S>
where
    S: Zero + One + Clone + Rmul + Radd,
{
    Eps,
    Sym(Rc<Sym<S>>),
    Alt(Rc<Glu<S>>, Rc<Glu<S>>),
    Seq(Rc<Glu<S>>, Rc<Glu<S>>),
    Rep(Rc<Glu<S>>),
}

// Empty, Final, Data
pub struct Glu<S: Zero + One + Clone + Rmul + Radd> (S, S, Glui<S>);

/// Recognize only the empty string
pub fn eps<S>() -> Rc<Glu<S>>
where
    S: Zero + One + Clone + Rmul + Radd,
{
    Rc::new(Glu(one(), one(), Glui::Eps))
}

/// Recognize alternatives between two other regexes
pub fn alt<S>(r1: &Rc<Glu<S>>, r2: &Rc<Glu<S>>) -> Rc<Glu<S>>
where
    S: Zero + One + Clone + Rmul + Radd,
{
    Rc::new(Glu(S::radd(&r1.0, &r2.0), S::radd(&r1.1, &r2.1), Glui::Alt(r1.clone(), r2.clone())))
}

/// Recognize a sequence of regexes in order
pub fn seq<S>(r1: &Rc<Glu<S>>, r2: &Rc<Glu<S>>) -> Rc<Glu<S>>
where
    S: Zero + One + Clone + Rmul + Radd,
{
    Rc::new(Glu(S::radd(&r1.0, &r2.0), S::radd(&S::rmul(&r1.1, &r2.0), &r2.1), Glui::Seq(r1.clone(), r2.clone())))
}

/// Recognize a regex repeated zero or more times.
pub fn rep<S>(r1: &Rc<Glu<S>>) -> Rc<Glu<S>>
where
    S: Zero + One + Clone + Rmul + Radd,
{
    Rc::new(Glu(one(), r1.1.clone(), Glui::Rep(r1.clone())))
}

// The main function: repeatedly traverses the tree, modifying as it
// goes, generating a new tree, marking the nodes where the expression
// currently "is," for any given character.  The values of the nodes
// are cached for performance, but this probably isn't a win in Rust
// as Rust won't keep the intermediate functions generated, nor
// provide them ad-hoc to future operations the way Haskell does.
//
fn shift<S>(g: &Rc<Glu<S>>, m: &S, c: char) -> Rc<Glu<S>>
where
    S: One + Zero + Clone + Rmul + Radd
{
    use self::Glui::*;
    match &g.deref().2 {
        Eps         => eps(),
        Sym(f)      => Rc::new(Glu(zero(), S::rmul(m, &f.is(c)), Glui::Sym(f.clone()))),
        Alt(r1, r2) => alt(&shift(&r1, m, c), &shift(&r2, m, c)),
        Seq(r1, r2) => seq(&shift(&r1, m, c), &shift(&r2, &S::radd(&r1.1, &S::rmul(m, &r1.0)), c)),
        Rep(r)      => rep(&shift(&r, &S::radd(m, &r.1), c)),
    }
}
    
pub fn accept<S>(g: &Rc<Glu<S>>, s: &str) -> S
where
    S: One + Zero + Clone + Rmul + Radd
{
    if s.is_empty() {
        return g.0.clone()
    }

    let ashift = |g, c| { shift(&g, &zero(), c) };

    // This is kinda cool. I wonder if I can make the Brz versions look
    // like this.
    let mut seq = s.chars();
    let start = shift(g, &one(), seq.next().unwrap());
    (&seq.fold(start, ashift)).1.clone()
}


#[cfg(test)]
mod tests {

    use super::*;

    macro_rules! set {
        ( $( $x:expr ),* ) => {{
                let mut temp_set = HashSet::new();
                $( temp_set.insert($x); )*
                temp_set //
            }};
    }

    #[derive(Debug, Copy, Clone)]
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

    impl Rmul for Recognizer {
        fn rmul(lhs: &Recognizer, rhs: &Recognizer) -> Recognizer { Recognizer(lhs.0 && rhs.0) }
    }

    impl Radd for Recognizer {
        fn radd(lhs: &Recognizer, rhs: &Recognizer) -> Recognizer { Recognizer(lhs.0 || rhs.0) }
    }

    pub struct SimpleSym
    {
        c: char,
    }
    
    impl Sym<Recognizer> for SimpleSym {
        fn is(&self, c: char) -> Recognizer {
            if c == self.c { one() } else { zero() }
        }
    }
            
    #[test]
    fn basics() {
        
        pub fn sym(sample: char) -> Rc<Glu<Recognizer>>
        {
            Rc::new(Glu(zero(), zero(), Glui::Sym(Rc::new(SimpleSym{ c: sample }))))
        }
        
        let cases = [
            ("empty", eps(), "", true),
            ("char", sym('a'), "a", true),
            ("not char", sym('a'), "b", false),
            ("char vs empty", sym('a'), "", false),
            ("left alt", alt(&sym('a'), &sym('b')), "a", true),
            ("right alt", alt(&sym('a'), &sym('b')), "b", true),
            ("neither alt", alt(&sym('a'), &sym('b')), "c", false),
            ("empty alt", alt(&sym('a'), &sym('b')), "", false),
            ("empty rep", rep(&sym('a')), "", true),
            ("sequence", seq(&sym('a'), &sym('b')), "ab", true),
            ("sequence with empty", seq(&sym('a'), &sym('b')), "", false),
            ("bad long sequence", seq(&sym('a'), &sym('b')), "abc", false),
            ("bad short sequence", seq(&sym('a'), &sym('b')), "a", false),
            ("one rep", rep(&sym('a')), "a", true),
            ("short multiple failed rep", rep(&sym('a')), "ab", false),
            ("multiple rep", rep(&sym('a')), "aaaaaaaaa", true),
            (
                "multiple rep with failure",
                rep(&sym('a')),
                "aaaaaaaaab",
                false,
            ),
        ];

        for (name, case, sample, result) in &cases {
            println!("{:?}", name);
            assert_eq!(accept(case, &sample.to_string()).0, *result);
        }
    }

    #[derive(Debug, Clone)]
    pub struct Parser(HashSet<String>);

    impl Zero for Parser {
        fn zero() -> Parser { Parser(set![]) }
        fn is_zero(&self) -> bool { self.0.len() == 0 }
    }

    impl One for Parser {
        fn one() -> Parser { Parser(set!["".to_string()]) }
    }

    

    // This is the tricky one; the product of two sets of strings is a
    // set containing the concatenation of the tuples of the cartesian
    // products from the two sets.

    impl Rmul for Parser {
        fn rmul(lhs: &Parser, rhs: &Parser) -> Parser {
            let mut temp = set![];
            for i in lhs.0.iter().cloned() {
                for j in &rhs.0 {
                    temp.insert(i.clone() + &j);
                }
            }
            Parser(temp)
        }
    }

    impl Radd for Parser {
        fn radd(lhs: &Parser, rhs: &Parser) -> Parser { Parser(lhs.0.union(&rhs.0).cloned().collect()) }
    }

    impl Mul for Parser {
        type Output = Parser;
        fn mul(self, rhs: Parser) -> Parser {
            Parser::rmul(&self, &rhs)
        }
    }

    impl Add for Parser {
        type Output = Parser;
        fn add(self, rhs: Parser) -> Parser {
            Parser::radd(&self, &rhs)
        }
    }
    
    pub struct ParserSym
    {
        c: char,
    }
    
    impl Sym<Parser> for ParserSym {
        fn is(&self, c: char) -> Parser {
            if c == self.c { Parser(set![c.to_string()]) } else { Parser::zero() }
        }
    }

    #[test]
    fn string_basics() {
        pub fn sym(sample: char) -> Rc<Glu<Parser>>
        {
            Rc::new(Glu(zero(), zero(), Glui::Sym(Rc::new(ParserSym{ c: sample }))))
        }
        
        let cases = [
            ("empty", eps(), "", Some("")),
            ("char", sym('a'), "a", Some("a")),
            ("not char", sym('a'), "b", None),
            ("char vs empty", sym('a'), "", None),
            ("left alt", alt(&sym('a'), &sym('b')), "a", Some("a")),
            ("right alt", alt(&sym('a'), &sym('b')), "b", Some("b")),
            ("neither alt", alt(&sym('a'), &sym('b')), "c", None),
            ("empty alt", alt(&sym('a'), &sym('b')), "", None),
            ("empty rep", rep(&sym('a')), "", Some("")),
            ("sequence", seq(&sym('a'), &sym('b')), "ab", Some("ab")),
            ("sequence with empty", seq(&sym('a'), &sym('b')), "", None),
            ("bad long sequence", seq(&sym('a'), &sym('b')), "abc", None),
            ("bad short sequence", seq(&sym('a'), &sym('b')), "a", None),
            ("one rep", rep(&sym('a')), "a", Some("a")),
            ("short multiple failed rep", rep(&sym('a')), "ab", None),
            ("multiple rep", rep(&sym('a')), "aaaaaaaaa", Some("aaaaaaaaa")),
            ("multiple rep with failure", rep(&sym('a')), "aaaaaaaaab", None),
        ];

        for (name, case, sample, result) in &cases {
            println!("{:?}", name);
            let ret = accept(case, &sample.to_string()).0;
            match result {
                Some(r) => {
                    let v = ret.iter().next();
                    if let Some(s) = v {
                        assert_eq!(s, sample);
                    } else {
                        panic!("Strings did not match: {:?}, {:?}", r, v);
                    }
                    assert_eq!(1, ret.len());
                },
                None    => assert_eq!(0, ret.len())
            }
        }
    }
}
