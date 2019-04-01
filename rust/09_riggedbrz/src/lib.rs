//! This crate provides a series of simple functions for building a
//! regular expression, and an `accept` function which takes a
//! completed regular expression and a string and returns a boolean
//! value describing if the expression matched the string (or not).
//!

#[allow(unused_imports)]
use std::collections::HashSet;
use std::rc::Rc;
use std::cell::RefCell;

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
    D: PartialEq + Copy + Clone
{
    Emp,
    Eps(R),
    Sym(Rc<Sym<R, D>>),
    Del(Rc<RefCell<Expr<R, D>>>),
    Alt(Rc<RefCell<Expr<R, D>>>, Rc<RefCell<Expr<R, D>>>),
    Seq(Rc<RefCell<Expr<R, D>>>, Rc<RefCell<Expr<R, D>>>),
    Rep(Rc<RefCell<Expr<R, D>>>),
}

pub struct DerivativeCache<R, D>
where
    R: Semiring,
    D: PartialEq + Copy + Clone
{
    token: D,
    derivative: Expt<R, D>
}
    
pub enum Nullability {
    Accept,
    Reject,
    InProgress,
    Unvisited
}

type Expt<R, D> = Rc<RefCell<Expr<R, D>>>;

pub struct Expr<R, D>
where
    R: Semiring,
    D: PartialEq + Copy + Clone
{
    expr: Brz<R, D>,
    known_derivative: Option<DerivativeCache<R, D>>,
    nullable: Nullability,
    product: Option<R>,
    listeners: Vec<Expt<R, D>>
}

impl<R, D> Expr<R, D>
where
    R: Semiring,
    D: PartialEq + Copy + Clone
{
    pub fn new(b: Brz<R, D>, n: Nullability) -> Expr<R, D> {
        Expr {
            expr: b,
            known_derivative: None,
            nullable: n,
            product: None,
            listeners: Vec::new(),
        }
    }
}

#[inline]
pub fn expr<R, D>(b: Brz<R, D>, n: Nullability) -> Expt<R, D>
where
    R: Semiring,
    D: PartialEq + Copy + Clone
{
    Rc::new(RefCell::new(Expr::new(b, n)))
}

pub fn emp<R, D>() -> Expt<R, D>
where
    R: Semiring,
    D: PartialEq + Copy + Clone
{
    expr(Brz::Emp, Nullability::Reject)
}

pub fn eps<R, D>(e: R) -> Expt<R, D>
where
    R: Semiring,
    D: PartialEq + Copy + Clone
{
    expr(Brz::Eps(e), Nullability::Accept)
}

pub fn alt<R, D>(r1: &Expt<R, D>, r2: &Expt<R, D>) -> Expt<R, D>
where
    R: Semiring,
    D: PartialEq + Copy + Clone
{
    match (&r1.borrow().expr, &r2.borrow().expr) {
        (_, Brz::Emp) => r1.clone(),
        (Brz::Emp, _) => r2.clone(),
        _ => expr(Brz::Alt(r1.clone(), r2.clone()), Nullability::Unvisited),
    }
}

pub fn seq<R, D>(r1: &Expt<R, D>, r2: &Expt<R, D>) -> Expt<R, D>
where
    R: Semiring,
    D: PartialEq + Copy + Clone
{
    match (&r1.borrow().expr, &r2.borrow().expr) {
        (_, Brz::Emp) => emp(),
        (Brz::Emp, _) => emp(),
        _ => expr(Brz::Seq(r1.clone(), r2.clone()), Nullability::Unvisited),
    }
}

pub fn rep<R, D>(r1: &Expt<R, D>) -> Expt<R, D>
where
    R: Semiring,
    D: PartialEq + Copy + Clone
{
    expr(Brz::Rep(r1.clone()), Nullability::Accept)
}

pub fn del<R, D>(r1: &Expt<R, D>) -> Expt<R, D>
where
    R: Semiring,
    D: PartialEq + Copy + Clone
{
    expr(Brz::Del(r1.clone()), Nullability::Reject)
}

pub fn derive<R, D>(n: &Expt<R, D>, c: &D) -> Expt<R, D>
where
    R: Semiring,
    D: PartialEq + Copy + Clone,
{
    use self::Brz::*;

    {
        if let Some(kd) = &n.borrow().known_derivative {
            if kd.token == *c {
                return kd.derivative.clone()
            }
        }
    }
    
    let res = match &n.borrow().expr {
        Emp => emp(),
        Eps(_) => emp(),
        Del(_) => emp(),
        Sym(f) => eps(f.is(c)),
        Seq(l, r) => {
            let dl = seq(&derive(&l, c), &r);
            let dr = seq(&del(&l), &derive(&r, c));
            alt(&dl, &dr)
        }
        Alt(l, r) => alt(&derive(&l, c), &derive(&r, c)),
        Rep(r) => seq(&derive(&r, c), &n.clone()),
    };

    {
        let mut nbm = n.borrow_mut();
        nbm.known_derivative = Some(DerivativeCache {
            token: *c,
            derivative: res.clone()
        })
    }
    
    res
}

pub fn parsenull<R, D>(r: &Expt<R, D>) -> R
where
    R: Semiring + Clone,
    D: PartialEq + Copy + Clone
{
    use self::Brz::*;

    {
        if let Some(product) = &r.borrow().product {
            return product.clone()
        }
    }

    let product = match &r.borrow().expr {
        Emp => R::zero(),
        Eps(s) => s.clone(),
        Del(s) => parsenull(&s),
        Rep(_) => R::one(),
        Sym(_) => R::zero(),
        Seq(l, r) => parsenull(&l).mul(&parsenull(&r)),
        Alt(l, r) => parsenull(&l).add(&parsenull(&r)),
    };

    {
        let mut nbm = r.borrow_mut();
        nbm.product= Some(product.clone());
    }

    product
}

pub fn parse<R, D, I>(r: &Expt<R, D>, source: &mut I) -> R
where
    R: Semiring + Clone,
    D: PartialEq + Copy + Clone,
    I: Iterator<Item = D>,
{
    let start = if let Some(c) = source.next() {
        derive(&r, &c)
    } else {
        return parsenull(r);
    };

    let innerderive = |b, c| derive(&b, &c);
    parsenull(&source.fold(start, innerderive))
}

#[cfg(test)]
mod tests {

    use super::*;

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
        pub fn sym(sample: char) -> Expt<Recognizer, char> {
            expr(Brz::Sym(Rc::new(SimpleSym { c: sample })), Nullability::Reject)
        }

        let cases = [
            ("empty", eps(Recognizer::one()), "", true),
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
            assert_eq!(parse(&case, &mut sample.to_string().chars()).0, *result);
        }
    }

    //   _         __ _   _                __   __    _ _    _
    //  | |   ___ / _| |_| |___ _ _  __ _  \ \ / /_ _| (_)__| |
    //  | |__/ -_)  _|  _| / _ \ ' \/ _` |  \ V / _` | | / _` |
    //  |____\___|_|  \__|_\___/_||_\__, |   \_/\__,_|_|_\__,_|
    //                              |___/

    #[test]
    fn assert_leftlong_tests_valid() {
        pub struct AnySym {};

        impl Sym<Recognizer, char> for AnySym {
            fn is(&self, _: &char) -> Recognizer {
                return { Recognizer::one() };
            }
        }

        pub fn sym(sample: char) -> Expt<Recognizer, char> {
            expr(Brz::Sym(Rc::new(SimpleSym { c: sample })), Nullability::Reject)
        }

        pub fn asy() -> Expt<Recognizer, char> {
            expr(Brz::Sym(Rc::new(AnySym {})), Nullability::Reject)
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
            assert_eq!(parse(&arb, &mut sample.to_string().chars()).0, *result);
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

    #[derive(Eq, PartialEq, Copy, Clone)]
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

    #[test]
    fn leftlong_basics() {
        pub fn asym() -> Expt<Leftlong, Pc> {
            expr(Brz::Sym(Rc::new(AnySym {})), Nullability::Reject)
        }

        pub fn symi(sample: char) -> Expt<Leftlong, Pc> {
            expr(Brz::Sym(Rc::new(RangeSym(sample))), Nullability::Reject)
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

    //   ___
    //   | _ \__ _ _ _ ___ ___
    //   |  _/ _` | '_(_-</ -_)
    //   |_| \__,_|_| /__/\___|
    //
    //

    macro_rules! set {
        ( $( $x:expr ),* ) => {{
            #[allow(unused_mut)]
            let mut temp_set = HashSet::new();
            $( temp_set.insert($x); )*
                temp_set //
        }};
    }

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
        pub fn sym(sample: char) -> Expt<Parser, char> {
            expr(Brz::Sym(Rc::new(ParserSym { c: sample })), Nullability::Reject)
        }

        let cases = [
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
            let ret = parse(case, &mut sample.to_string().chars()).0;
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

}
