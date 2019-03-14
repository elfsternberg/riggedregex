//! This crate provides a series of simple functions for building a
//! regular expression, and an `accept` function which takes a
//! completed regular expression and a string and returns a boolean
//! value describing if the expression matched the string (or not).
//!

#[allow(unused_imports)]
use std::collections::HashSet;
use std::ops::Deref;
use std::rc::Rc;

// Generic type conventions:
//   R: Our Ring Type
//   D: Our inbound Data type
//   I: An Iterator type

pub trait Semiring {
    fn zero() -> Self;
    fn one() -> Self;
    fn is_zero(&self) -> bool;
    fn mul(&self, rhs: &Self) -> Self;
    fn add(&self, rhs: &Self) -> Self;
}

pub trait Sym<R, D>
where
    R: Semiring,
{
    fn is(&self, c: &D) -> R;
}

pub enum Brz<R, D>
where
    R: Semiring,
{
    Emp,
    Eps(R),
    Sym(Rc<Sym<R, D>>),
    Alt(Rc<Brz<R, D>>, Rc<Brz<R, D>>),
    Seq(Rc<Brz<R, D>>, Rc<Brz<R, D>>),
    Rep(Rc<Brz<R, D>>),
}

pub fn emp<R, D>() -> Rc<Brz<R, D>>
where
    R: Semiring,
{
    Rc::new(Brz::Emp)
}

pub fn eps<R, D>(e: R) -> Rc<Brz<R, D>>
where
    R: Semiring,
{
    Rc::new(Brz::Eps(e))
}

pub fn alt<R, D>(r1: &Rc<Brz<R, D>>, r2: &Rc<Brz<R, D>>) -> Rc<Brz<R, D>>
where
    R: Semiring,
{
    match (r1.deref(), r2.deref()) {
        (_, Brz::Emp) => r1.clone(),
        (Brz::Emp, _) => r2.clone(),
        _ => Rc::new(Brz::Alt(r1.clone(), r2.clone())),
    }
}

pub fn seq<R, D>(r1: &Rc<Brz<R, D>>, r2: &Rc<Brz<R, D>>) -> Rc<Brz<R, D>>
where
    R: Semiring,
{
    match (r1.deref(), r2.deref()) {
        (_, Brz::Emp) => emp(),
        (Brz::Emp, _) => emp(),
        _ => Rc::new(Brz::Seq(r1.clone(), r2.clone())),
    }
}

pub fn rep<R, D>(r1: &Rc<Brz<R, D>>) -> Rc<Brz<R, D>>
where
    R: Semiring,
{
    Rc::new(Brz::Rep(r1.clone()))
}

pub fn derive<R, D>(n: &Rc<Brz<R, D>>, c: &D) -> Rc<Brz<R, D>>
where
    R: Semiring,
    R: std::fmt::Debug,
    D: std::fmt::Debug,
{
    use self::Brz::*;

    match n.deref() {
        Emp => emp(),
        Eps(_) => emp(),
        Sym(f) => {
            let r = f.is(c);
            println!("S: {:?}, {:?}", c, r);
            eps(r)
        },
        Seq(l, r) => {
            if nullable(&l) {
                alt(&seq(&derive(l, c), r), &derive(r, c))
            } else {
                seq(&derive(l, c), r)
            }
        }
        Alt(l, r) => alt(&derive(l, c), &derive(r, c)),
        Rep(r) => seq(&derive(r, c), &n.clone()),
    }
}

pub fn nullable<R, D>(n: &Rc<Brz<R, D>>) -> bool
where
    R: Semiring
{
    use self::Brz::*;

    match n.deref() {
        Emp => false,
        Eps(_) => true,
        Sym(_) => false,
        Seq(l, r) => nullable(l) && nullable(r),
        Alt(l, r) => nullable(l) || nullable(r),
        Rep(_) => true,
    }
}

pub fn parsenull<R, D>(r: &Rc<Brz<R, D>>) -> R
where
    R: Semiring + Clone
{
    use self::Brz::*;

    match r.deref() {
        Emp => R::zero(),
        Eps(s) => (*s).clone(),
        Rep(_) => R::one(),
        Sym(_) => R::zero(),
        Seq(l, r) => parsenull(l).mul(&parsenull(r)),
        Alt(l, r) => parsenull(l).add(&parsenull(r)),
    }
}

pub fn parse<R, D, I>(r: &Rc<Brz<R, D>>, source: &mut I) -> R
where
    R: Semiring + Clone,
    R: std::fmt::Debug,
    D: std::fmt::Debug,
    I: Iterator<Item = D>,
{
    let start = if let Some(c) = source.next() {
        derive(r, &c)
    } else {
        return parsenull(r);
    };

    let innerderive = |b, c| derive(&b, &c);
    parsenull(&source.fold(start, innerderive))
}

#[cfg(test)]
mod tests {

    use super::*;

//    //  ___                       _
//    // | _ \___ __ ___  __ _ _ _ (_)______
//    // |   / -_) _/ _ \/ _` | ' \| |_ / -_)
//    // |_|_\___\__\___/\__, |_||_|_/__\___|
//    //                 |___/
//
//    #[derive(Debug, Copy, Clone, PartialEq)]
//    pub struct Recognizer(bool);
//
//    impl Semiring for Recognizer {
//        fn one() -> Recognizer {
//            Recognizer(true)
//        }
//        fn zero() -> Recognizer {
//            Recognizer(false)
//        }
//        fn is_zero(&self) -> bool {
//            !self.0
//        }
//        fn mul(&self, rhs: &Recognizer) -> Recognizer {
//            Recognizer(self.0 && rhs.0)
//        }
//        fn add(&self, rhs: &Recognizer) -> Recognizer {
//            Recognizer(self.0 || rhs.0)
//        }
//    }
//
//    pub struct SimpleSym {
//        c: char,
//    }
//
//    impl Sym<Recognizer, char> for SimpleSym {
//        fn is(&self, c: &char) -> Recognizer {
//            if *c == self.c {
//                Recognizer::one()
//            } else {
//                Recognizer::zero()
//            }
//        }
//    }
//
//    #[test]
//    fn basics() {
//        pub fn sym(sample: char) -> Rc<Brz<Recognizer, char>> {
//            Rc::new(Brz::Sym(Rc::new(SimpleSym { c: sample })))
//        }
//
//        let cases = [
//            ("empty", eps(Recognizer::one()), "", true),
//            ("char", sym('a'), "a", true),
//            ("not char", sym('a'), "b", false),
//            ("char vs empty", sym('a'), "", false),
//            ("left alt", alt(&sym('a'), &sym('b')), "a", true),
//            ("right alt", alt(&sym('a'), &sym('b')), "b", true),
//            ("neither alt", alt(&sym('a'), &sym('b')), "c", false),
//            ("empty alt", alt(&sym('a'), &sym('b')), "", false),
//            ("empty rep", rep(&sym('a')), "", true),
//            ("sequence", seq(&sym('a'), &sym('b')), "ab", true),
//            ("sequence with empty", seq(&sym('a'), &sym('b')), "", false),
//            ("bad long sequence", seq(&sym('a'), &sym('b')), "abc", false),
//            ("bad short sequence", seq(&sym('a'), &sym('b')), "a", false),
//            ("one rep", rep(&sym('a')), "a", true),
//            ("short multiple failed rep", rep(&sym('a')), "ab", false),
//            ("multiple rep", rep(&sym('a')), "aaaaaaaaa", true),
//            (
//                "multiple rep with failure",
//                rep(&sym('a')),
//                "aaaaaaaaab",
//                false,
//            ),
//        ];
//
//        for (name, case, sample, result) in &cases {
//            println!("{:?}", name);
//            assert_eq!(parse(&case, &mut sample.to_string().chars()).0, *result);
//        }
//    }
//
//    //   _         __ _   _                __   __    _ _    _
//    //  | |   ___ / _| |_| |___ _ _  __ _  \ \ / /_ _| (_)__| |
//    //  | |__/ -_)  _|  _| / _ \ ' \/ _` |  \ V / _` | | / _` |
//    //  |____\___|_|  \__|_\___/_||_\__, |   \_/\__,_|_|_\__,_|
//    //                              |___/
//
//    // This is a short set of tests done to see if the 'anychar' and
//    // 'skip until' features worked.
//    
//    #[test]
//    fn assert_leftlong_tests_valid() {
//        pub struct AnySym {};
//
//        impl Sym<Recognizer, char> for AnySym {
//            fn is(&self, _: &char) -> Recognizer {
//                return { Recognizer::one() };
//            }
//        }
//
//        pub fn sym(sample: char) -> Rc<Brz<Recognizer, char>> {
//            Rc::new(Brz::Sym(Rc::new(SimpleSym { c: sample })))
//        }
//
//        pub fn asy() -> Rc<Brz<Recognizer, char>> {
//            Rc::new(Brz::Sym(Rc::new(AnySym {})))
//        }
//
//        let any = rep(&asy());
//        let a = sym('a');
//        let ab = rep(&alt(&a, &sym('b')));
//        let aaba = seq(&seq(&a, &ab), &a);
//
//        let cases = [
//            ("any", &seq(&any, &sym('c')), "cbcdc", true),
//            ("leftlong sample zero", &aaba, "ab", false),
//            ("leftlong sample five", &aaba, "bababa", true),
//            ("leftlong sample one", &aaba, "aa", true),
//        ];
//
//        for (name, case, sample, result) in &cases {
//            println!("{:?}", name);
//            let arb = seq(&any, &seq(&case, &any));
//            assert_eq!(parse(&arb, &mut sample.to_string().chars()).0, *result);
//        }
//    }
//
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
            match self {
                Leftlong::Notfound => true,
                _ => false,
            }
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

    #[derive(Debug)]
    pub struct Pc(usize, char);

    #[derive(Clone, Debug)]
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

    #[test]
    fn leftlong_basics() {
        pub fn asym() -> Rc<Brz<Leftlong, Pc>> {
            Rc::new(Brz::Sym(Rc::new(AnySym {})))
        }

        pub fn symi(sample: char) -> Rc<Brz<Leftlong, Pc>> {
            Rc::new(Brz::Sym(Rc::new(RangeSym(sample))))
        }

        let a = symi('a');
        let ab = rep(&alt(&a.clone(), &symi('b')));
        let aaba = seq(&a.clone(), &seq(&ab, &a.clone()));
        // /a(ab)*a/
        
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
            let ret = parse(
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

//    //   ___
//    //   | _ \__ _ _ _ ___ ___
//    //   |  _/ _` | '_(_-</ -_)
//    //   |_| \__,_|_| /__/\___|
//    //
//    //
//
//    macro_rules! set {
//        ( $( $x:expr ),* ) => {{
//            #[allow(unused_mut)]
//            let mut temp_set = HashSet::new();
//            $( temp_set.insert($x); )*
//                temp_set //
//        }};
//    }
//
//    #[derive(Debug, Clone, PartialEq)]
//    pub struct Parser(HashSet<String>);
//
//    impl Semiring for Parser {
//        fn one() -> Parser {
//            Parser(set!["".to_string()])
//        }
//        fn zero() -> Parser {
//            Parser(set![])
//        }
//        fn is_zero(&self) -> bool {
//            self.0.len() == 0
//        }
//        fn mul(self: &Parser, rhs: &Parser) -> Parser {
//            let mut temp = set![];
//            for i in self.0.iter().cloned() {
//                for j in &rhs.0 {
//                    temp.insert(i.clone() + &j);
//                }
//            }
//            Parser(temp)
//        }
//        fn add(self: &Parser, rhs: &Parser) -> Parser {
//            Parser(self.0.union(&rhs.0).cloned().collect())
//        }
//    }
//
//    pub struct ParserSym {
//        c: char,
//    }
//
//    impl Sym<Parser, char> for ParserSym {
//        fn is(&self, c: &char) -> Parser {
//            if *c == self.c {
//                Parser(set![c.to_string()])
//            } else {
//                Parser::zero()
//            }
//        }
//    }
//
//    #[test]
//    fn string_basics() {
//        pub fn sym(sample: char) -> Rc<Brz<Parser, char>> {
//            Rc::new(Brz::Sym(Rc::new(ParserSym { c: sample })))
//        }
//
//        let cases = [
//            ("char", sym('a'), "a", Some("a")),
//            ("not char", sym('a'), "b", None),
//            ("char vs empty", sym('a'), "", None),
//            ("left alt", alt(&sym('a'), &sym('b')), "a", Some("a")),
//            ("right alt", alt(&sym('a'), &sym('b')), "b", Some("b")),
//            ("neither alt", alt(&sym('a'), &sym('b')), "c", None),
//            ("empty alt", alt(&sym('a'), &sym('b')), "", None),
//            ("empty rep", rep(&sym('a')), "", Some("")),
//            ("sequence", seq(&sym('a'), &sym('b')), "ab", Some("ab")),
//            ("sequence with empty", seq(&sym('a'), &sym('b')), "", None),
//            ("bad long sequence", seq(&sym('a'), &sym('b')), "abc", None),
//            ("bad short sequence", seq(&sym('a'), &sym('b')), "a", None),
//            ("one rep", rep(&sym('a')), "a", Some("a")),
//            ("short multiple failed rep", rep(&sym('a')), "ab", None),
//            (
//                "multiple rep",
//                rep(&sym('a')),
//                "aaaaaaaaa",
//                Some("aaaaaaaaa"),
//            ),
//            (
//                "multiple rep with failure",
//                rep(&sym('a')),
//                "aaaaaaaaab",
//                None,
//            ),
//        ];
//
//        for (name, case, sample, result) in &cases {
//            println!("{:?}", name);
//            let ret = parse(case, &mut sample.to_string().chars()).0;
//            match result {
//                Some(r) => {
//                    let v = ret.iter().next();
//                    if let Some(s) = v {
//                        assert_eq!(s, sample);
//                    } else {
//                        panic!("Strings did not match: {:?}, {:?}", r, v);
//                    }
//                    assert_eq!(1, ret.len());
//                }
//                None => assert_eq!(0, ret.len()),
//            }
//        }
//    }

}
