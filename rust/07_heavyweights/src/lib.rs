//! This crate provides a series of simple functions for building a
//! regular expression, and an `accept` function which takes a
//! completed regular expression and a string and returns a boolean
//! value describing if the expression matched the string (or not).
//!

// Generic type conventions:
//   R: Our Ring Type
//   D: Our inbound Data type
//   I: An Iterator type

#[allow(unused_imports)]
use std::collections::HashSet;
use std::iter::Iterator;
use std::rc::Rc;

#[allow(unused_macros)]
macro_rules! matches {
    ($expression:expr, $($pattern:tt)+) => {
        match $expression {
            $($pattern)+ => true,
            _ => false
        }
    }
}

pub trait Semiring {
    fn zero() -> Self;
    fn one() -> Self;
    fn is_zero(&self) -> bool;
    fn mul(&self, rhs: &Self) -> Self;
    fn add(&self, rhs: &Self) -> Self;
}

/// The Sym trait represents what to do for a single character.  It has
/// a single method, "is", that returns the semiring.  Implementers of
/// "is" must provide a corresponding construction factory.

pub trait Sym<R, D>
where
    R: Semiring,
{
    fn is(&self, c: &D) -> R;
}

pub enum Glui<R, D>
where
    R: Semiring,
{
    Eps,
    Sym(Rc<Sym<R, D>>),
    Alt(Rc<Glu<R, D>>, Rc<Glu<R, D>>),
    Seq(Rc<Glu<R, D>>, Rc<Glu<R, D>>),
    Rep(Rc<Glu<R, D>>),
}

// Empty, Final, Data
pub struct Glu<R, D>(R, R, Glui<R, D>)
where
    R: Semiring;

/// Recognize only the empty string
pub fn eps<R, D>() -> Rc<Glu<R, D>>
where
    R: Semiring,
{
    Rc::new(Glu(R::one(), R::zero(), Glui::Eps))
}

/// Recognize alternatives between two other regexes
pub fn alt<R, D>(l: &Rc<Glu<R, D>>, r: &Rc<Glu<R, D>>) -> Rc<Glu<R, D>>
where
    R: Semiring,
{
    Rc::new(Glu(
        l.0.add(&r.0),
        l.1.add(&r.1),
        Glui::Alt(l.clone(), r.clone()),
    ))
}

/// Recognize a sequence of regexes in order
pub fn seq<R, D>(l: &Rc<Glu<R, D>>, r: &Rc<Glu<R, D>>) -> Rc<Glu<R, D>>
where
    R: Semiring,
{
    Rc::new(Glu(
        l.0.mul(&r.0),
        l.1.mul(&r.0).add(&r.1),
        Glui::Seq(l.clone(), r.clone()),
    ))
}

/// Recognize a regex repeated zero or more times.
pub fn rep<R, D>(c: &Rc<Glu<R, D>>) -> Rc<Glu<R, D>>
where
    R: Semiring + Clone,
{
    Rc::new(Glu(R::one(), c.1.clone(), Glui::Rep(c.clone())))
}

// The main function: repeatedly traverses the tree, modifying as it
// goes, generating a new tree, marking the nodes where the expression
// currently "is," for any given character.  The values of the nodes
// are cached for performance, but this probably isn't a win in Rust
// as Rust won't keep the intermediate functions generated, nor
// provide them ad-hoc to future operations the way Haskell does.
//
fn shift<R, D>(g: &Rc<Glu<R, D>>, m: &R, c: &D) -> Rc<Glu<R, D>>
where
    R: Semiring + Clone,
{
    use self::Glui::*;
    match &g.2 {
        Eps => eps(),
        Sym(f) => Rc::new(Glu(R::zero(), m.mul(&f.is(c)), Glui::Sym(f.clone()))),
        Alt(l, r) => alt(&shift(&l, m, c), &shift(&r, m, c)),
        Seq(l, r) => seq(&shift(&l, m, c), &shift(&r, &(m.mul(&l.0).add(&l.1)), c)),
        Rep(r) => rep(&shift(&r, &(m.add(&r.1)), c)),
    }
}

pub fn accept<R, D, I>(g: &Rc<Glu<R, D>>, source: &mut I) -> R
where
    R: Semiring + Clone + PartialEq,
    I: Iterator<Item = D>,
{
    let ashift = |g, c| shift(&g, &R::zero(), &c);

    let start = if let Some(c) = source.next() {
        shift(g, &R::one(), &c)
    } else {
        return g.0.clone();
    };
    (&source.fold(start, ashift)).1.clone()
}

#[cfg(test)]
mod tests {

    use super::*;

    macro_rules! set {
        ( $( $x:expr ),* ) => {{
                #[allow(unused_mut)]
                let mut temp_set = HashSet::new();
                $( temp_set.insert($x); )*
                temp_set //
            }};
    }

    //  ___                       _
    // | _ \___ __ ___  __ _ _ _ (_)______
    // |   / -_) _/ _ \/ _` | ' \| |_ / -_)
    // |_|_\___\__\___/\__, |_||_|_/__\___|
    //                 |___/

    #[derive(Debug, Copy, Clone, PartialEq)]
    pub struct Recognizer(bool);

    impl Semiring for Recognizer {
        fn one() -> Recognizer {
            Recognizer(true)
        }
        fn zero() -> Recognizer {
            Recognizer(false)
        }
        fn is_zero(&self) -> bool {
            !self.0
        }
        fn mul(&self, rhs: &Recognizer) -> Recognizer {
            Recognizer(self.0 && rhs.0)
        }
        fn add(&self, rhs: &Recognizer) -> Recognizer {
            Recognizer(self.0 || rhs.0)
        }
    }

    pub struct SimpleSym {
        c: char,
    }

    impl Sym<Recognizer, char> for SimpleSym {
        fn is(&self, c: &char) -> Recognizer {
            if *c == self.c {
                Recognizer::one()
            } else {
                Recognizer::zero()
            }
        }
    }

    #[test]
    fn basics() {
        pub fn sym(sample: char) -> Rc<Glu<Recognizer, char>> {
            Rc::new(Glu(
                Recognizer::zero(),
                Recognizer::zero(),
                Glui::Sym(Rc::new(SimpleSym { c: sample })),
            ))
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
            assert_eq!(accept(case, &mut sample.to_string().chars()).0, *result);
        }
    }

    //  _         __ _   _                __   __    _ _    _
    // | |   ___ / _| |_| |___ _ _  __ _  \ \ / /_ _| (_)__| |
    // | |__/ -_)  _|  _| / _ \ ' \/ _` |  \ V / _` | | / _` |
    // |____\___|_|  \__|_\___/_||_\__, |   \_/\__,_|_|_\__,_|
    //                             |___/

    #[test]
    fn assert_leftlong_tests_valid() {
        pub struct AnySym {};
        impl Sym<Recognizer, char> for AnySym {
            fn is(&self, _: &char) -> Recognizer {
                return { Recognizer::one() };
            }
        }

        pub fn sym(sample: char) -> Rc<Glu<Recognizer, char>> {
            Rc::new(Glu(
                Recognizer::zero(),
                Recognizer::zero(),
                Glui::Sym(Rc::new(SimpleSym { c: sample })),
            ))
        }

        pub fn asy() -> Rc<Glu<Recognizer, char>> {
            Rc::new(Glu(
                Recognizer::zero(),
                Recognizer::zero(),
                Glui::Sym(Rc::new(AnySym {})),
            ))
        }

        let any = rep(&asy());
        let a = sym('a');
        let ab = rep(&alt(&a, &sym('b')));
        let aaba = seq(&seq(&a, &ab), &a);

        let cases = [
            ("any", &seq(&any, &sym('c')), "cbcdc", true),
            ("leftlong sample zero", &aaba, "ab", false),
            ("leftlong sample five", &aaba, "bababa", true),
            ("leftlong sample one", &aaba, "aa", true),
        ];

        for (name, case, sample, result) in &cases {
            println!("{:?}", name);
            let arb = seq(&any, &seq(&case, &any));
            assert_eq!(accept(&arb, &mut sample.to_string().chars()).0, *result);
        }
    }

    //  ___
    // | _ \__ _ _ _ ___ ___
    // |  _/ _` | '_(_-</ -_)
    // |_| \__,_|_| /__/\___|
    //

    #[derive(Debug, Clone, PartialEq)]
    pub struct Parser(HashSet<String>);

    impl Semiring for Parser {
        fn one() -> Parser {
            Parser(set!["".to_string()])
        }
        fn zero() -> Parser {
            Parser(set![])
        }
        fn is_zero(&self) -> bool {
            self.0.len() == 0
        }
        fn mul(self: &Parser, rhs: &Parser) -> Parser {
            let mut temp = set![];
            for i in self.0.iter().cloned() {
                for j in &rhs.0 {
                    temp.insert(i.clone() + &j);
                }
            }
            Parser(temp)
        }
        fn add(self: &Parser, rhs: &Parser) -> Parser {
            Parser(self.0.union(&rhs.0).cloned().collect())
        }
    }

    pub struct ParserSym {
        c: char,
    }

    impl Sym<Parser, char> for ParserSym {
        fn is(&self, c: &char) -> Parser {
            if *c == self.c {
                Parser(set![c.to_string()])
            } else {
                Parser::zero()
            }
        }
    }

    #[test]
    fn string_basics() {
        pub fn sym(sample: char) -> Rc<Glu<Parser, char>> {
            Rc::new(Glu(
                Parser::zero(),
                Parser::zero(),
                Glui::Sym(Rc::new(ParserSym { c: sample })),
            ))
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
            (
                "multiple rep",
                rep(&sym('a')),
                "aaaaaaaaa",
                Some("aaaaaaaaa"),
            ),
            (
                "multiple rep with failure",
                rep(&sym('a')),
                "aaaaaaaaab",
                None,
            ),
        ];

        for (name, case, sample, result) in &cases {
            println!("{:?}", name);
            let ret = accept(case, &mut sample.to_string().chars()).0;
            match result {
                Some(r) => {
                    let v = ret.iter().next();
                    if let Some(s) = v {
                        assert_eq!(s, sample);
                    } else {
                        panic!("Strings did not match: {:?}, {:?}", r, v);
                    }
                    assert_eq!(1, ret.len());
                }
                None => assert_eq!(0, ret.len()),
            }
        }
    }

    //  _         __ _   _
    // | |   ___ / _| |_| |___ _ _  __ _
    // | |__/ -_)  _|  _| / _ \ ' \/ _` |
    // |____\___|_|  \__|_\___/_||_\__, |
    //                             |___/

    // The semiring and semiring_i implementations for the LeftLong
    // Range operations.

    pub trait Semiringi<S: Semiring> {
        fn index(i: usize) -> S;
    }

    #[derive(Debug, Clone, Eq, PartialEq)]
    pub enum Leftlong {
        Notfound,            // Zero
        Scanning,            // One
        Found(usize, usize), // The set of useful datapoints
    }

    impl Semiring for Leftlong {
        fn one() -> Leftlong {
            Leftlong::Scanning
        }
        fn zero() -> Leftlong {
            Leftlong::Notfound
        }
        fn is_zero(&self) -> bool {
            matches!(self, Leftlong::Notfound)
        }
        fn add(self: &Leftlong, rhs: &Leftlong) -> Leftlong {
            use self::Leftlong::*;
            match (self, rhs) {
                (Notfound, c) | (c, Notfound) | (Scanning, c) | (c, Scanning) => c.clone(),
                (Found(i, j), Found(k, l)) => {
                    if i < k || i == k && j >= l {
                        self.clone()
                    } else {
                        rhs.clone()
                    }
                }
            }
        }

        fn mul(self: &Leftlong, rhs: &Leftlong) -> Leftlong {
            use self::Leftlong::*;
            match (self, rhs) {
                (Notfound, _) | (_, Notfound) => Leftlong::zero(),
                (Scanning, c) | (c, Scanning) => c.clone(),
                (Found(i, _), Found(_, l)) => Leftlong::Found(*i, *l),
            }
        }
    }

    impl Semiringi<Leftlong> for Leftlong {
        fn index(i: usize) -> Leftlong {
            Leftlong::Found(i, i)
        }
    }

    // The position and character being analyzed.

    pub struct Pc(usize, char);

    #[derive(Clone)]
    pub struct RangeSym(char);

    impl Sym<Leftlong, Pc> for RangeSym {
        fn is(&self, pc: &Pc) -> Leftlong {
            if pc.1 == self.0 {
                Leftlong::index(pc.0)
            } else {
                Leftlong::zero()
            }
        }
    }

    #[derive(Debug, Clone)]
    pub struct AnySym {}
    impl Sym<Leftlong, Pc> for AnySym {
        fn is(&self, _: &Pc) -> Leftlong {
            Leftlong::one()
        }
    }

    pub fn asym() -> Rc<Glu<Leftlong, Pc>> {
        Rc::new(Glu(
            Leftlong::zero(),
            Leftlong::zero(),
            Glui::Sym(Rc::new(AnySym {})),
        ))
    }

    #[test]
    fn leftlong_basics() {
        pub fn symi(sample: char) -> Rc<Glu<Leftlong, Pc>> {
            Rc::new(Glu(
                Leftlong::zero(),
                Leftlong::zero(),
                Glui::Sym(Rc::new(RangeSym(sample))),
            ))
        }

        let a = symi('a');
        let ab = rep(&alt(&a.clone(), &symi('b')));
        let aaba = seq(&a.clone(), &seq(&ab, &a.clone()));

        let cases = [
            ("leftlong zero", &aaba.clone(), "ab", Leftlong::Notfound),
            ("leftlong one", &aaba.clone(), "aa", Leftlong::Found(0, 1)),
            (
                "leftlong five",
                &aaba.clone(),
                "bababa",
                Leftlong::Found(1, 5),
            ),
        ];

        for (name, case, sample, result) in &cases {
            println!("{:?}", name);
            let any = rep(&asym());
            let arb = seq(&any, &seq(&case, &any));
            let ret = accept(
                &arb,
                &mut sample
                    .to_string()
                    .chars()
                    .into_iter()
                    .enumerate()
                    .map(|c| Pc(c.0, c.1)),
            );
            assert_eq!(result, &ret);
        }
    }
}
