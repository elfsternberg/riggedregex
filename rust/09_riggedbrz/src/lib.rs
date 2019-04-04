//! This crate provides a series of simple functions for building a
//! regular expression, and an `accept` function which takes a
//! completed regular expression and a string and returns a boolean
//! value describing if the expression matched the string (or not).
//!

use std::cell::RefCell;
#[allow(unused_imports)]
use std::collections::HashSet;
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

type Expt<R, D> = Rc<RefCell<Expr<R, D>>>;

pub enum Brz<R, D>
where
    R: Semiring,
    D: PartialEq + Copy,
{
    Emp,
    Ukn,
    Eps(Rc<R>),
    Sym(Rc<Sym<R, D>>),
    Alt(Expt<R, D>, Expt<R, D>),
    Seq(Expt<R, D>, Expt<R, D>),
    Red(Expt<R, D>, Rc<Fn(&Rc<R>) -> Rc<R>>),
}

pub struct DerivativeCache<R, D>
where
    R: Semiring,
    D: PartialEq + Copy,
{
    token: D,
    derivative: Expt<R, D>,
}

pub enum Nullable {
    Accept,
    Reject,
    InProgress,
    Unvisited,
}

pub struct Expr<R, D>
where
    R: Semiring,
    D: PartialEq + Copy,
{
    expr: Brz<R, D>,
    known_derivative: Option<DerivativeCache<R, D>>,
    nullable: Nullable,
    product: Option<Rc<R>>,
    listeners: Vec<Expt<R, D>>,
}

impl<R, D> Expr<R, D>
where
    R: Semiring,
    D: PartialEq + Copy,
{
    pub fn new(b: Brz<R, D>, n: Nullable) -> Expr<R, D> {
        Expr {
            expr: b,
            known_derivative: None,
            nullable: n,
            product: None,
            listeners: Vec::new(),
        }
    }

    pub fn mutate(&mut self, b: Brz<R, D>, n: Nullable) {
        self.expr = b;
        self.nullable = n;
    }
}

#[inline]
pub fn expr<R, D>(b: Brz<R, D>, n: Nullable) -> Expt<R, D>
where
    R: Semiring,
    D: PartialEq + Copy,
{
    Rc::new(RefCell::new(Expr::new(b, n)))
}

pub fn emp<R, D>() -> Expt<R, D>
where
    R: Semiring,
    D: PartialEq + Copy,
{
    expr(Brz::Emp, Nullable::Reject)
}

pub fn emp_mutate<R, D>(target: &mut Expt<R, D>)
where
    R: Semiring,
    D: PartialEq + Copy,
{
    target
        .borrow_mut()
        .mutate(Brz::Emp, Nullable::Reject);
}

pub fn eps<R, D>(e: R) -> Expt<R, D>
where
    R: Semiring,
    D: PartialEq + Copy,
{
    expr(Brz::Eps(Rc::new(e)), Nullable::Accept)
}

pub fn eps_mutate<R, D>(target: &mut Expt<R, D>, rig: Rc<R>)
where
    R: Semiring,
    D: PartialEq + Copy,
{
    target
        .borrow_mut()
        .mutate(Brz::Eps(rig.clone()), Nullable::Accept);
}

pub fn alt<R, D>(r1: &Expt<R, D>, r2: &Expt<R, D>) -> Expt<R, D>
where
    R: Semiring,
    D: PartialEq + Copy,
{
    match (&r1.borrow().expr, &r2.borrow().expr) {
        (_, Brz::Emp) => r1.clone(),
        (Brz::Emp, _) => r2.clone(),
        _ => expr(Brz::Alt(r1.clone(), r2.clone()), Nullable::Unvisited),
    }
}

pub fn alt_mutate<R, D>(target: &mut Expt<R, D>, l: &Expt<R, D>, r: &Expt<R, D>)
where
    R: Semiring,
    D: PartialEq + Copy,
{
    target
        .borrow_mut()
        .mutate(Brz::Alt(l.clone(), r.clone()), Nullable::Unvisited);
}

fn alt_mutate_optimized<R, D>(target: &mut Expt<R, D>, l: &Expt<R, D>, r: &Expt<R, D>) -> bool
where
    R: Semiring,
    D: PartialEq + Copy,
{
    match (&l.borrow().expr, &r.borrow().expr) {
        (Brz::Eps(ref leps), Brz::Eps(ref reps)) => {
            eps_mutate(target, Rc::new((*leps).add(&*reps)));
            true
        }
        
        _ => {
            alt_mutate(target, l, r);
            false
        }
    }
}

pub fn seq<R, D>(r1: &Expt<R, D>, r2: &Expt<R, D>) -> Expt<R, D>
where
    R: Semiring,
    D: PartialEq + Copy,
{
    match (&r1.borrow().expr, &r2.borrow().expr) {
        (_, Brz::Emp) => emp(),
        (Brz::Emp, _) => emp(),
        _ => expr(Brz::Seq(r1.clone(), r2.clone()), Nullable::Unvisited),
    }
}

pub fn seq_mutate<R, D>(target: &mut Expt<R, D>, l: &Expt<R, D>, r: &Expt<R, D>)
where
    R: Semiring,
    D: PartialEq + Copy,
{
    target
        .borrow_mut()
        .mutate(Brz::Seq(l.clone(), r.clone()), Nullable::Unvisited);
}

fn seq_mutate_optimized<R, D>(target: &mut Expt<R, D>, l: &Expt<R, D>, r: &Expt<R, D>) -> bool
where
    R: Semiring + 'static,
    D: PartialEq + Copy,
{
    use self::Brz::*;
    match &l.borrow().expr {
        Emp => {
            emp_mutate(target);
            true
        }

        Eps(ref rig) => {
            let closed_rig = rig.clone();
            red_mutate(target, &r, Rc::new(move |rg| Rc::new(closed_rig.mul(&rg))));
            true
        }

        _ => {
            seq_mutate(target, l, r);
            false
        }
    }
}

pub fn rep<R, D>(r1: &Expt<R, D>) -> Expt<R, D>
where
    R: Semiring,
    D: PartialEq + Copy,
{
    let mut rstar = ukn();
    // r* = ε | r ⊗ r*
    let right = seq(&r1.clone(), &rstar.clone());
    alt_mutate(&mut rstar, &eps(R::one()), &right);
    rstar
}

pub fn red<R, D>(r: &Expt<R, D>, func: Rc<Fn(&Rc<R>) -> Rc<R>>) -> Expt<R, D>
where
    R: Semiring,
    D: PartialEq + Copy,
{
    let closed_fn = func.clone();
    expr(Brz::Red(r.clone(), closed_fn), Nullable::Unvisited)
}

pub fn red_mutate<R, D>(target: &mut Expt<R, D>, l: &Expt<R, D>, func: Rc<Fn(&Rc<R>) -> Rc<R>>)
where
    R: Semiring,
    D: PartialEq + Copy,
{
    target
        .borrow_mut()
        .mutate(Brz::Red(l.clone(), func), Nullable::Unvisited);
}

pub fn red_mutate_optimized<R, D>(target: &mut Expt<R, D>, l: &Expt<R, D>, func: &Rc<Fn(&Rc<R>) -> Rc<R>>) -> bool
where
    R: Semiring + 'static,
    D: PartialEq + Copy,
{
    use self::Brz::*;

    match &l.borrow().expr {
        Emp => {
            emp_mutate(target);
            true
        }

        Eps(ref rig) => {
            let res_rig = func(&*rig);
            eps_mutate(target, res_rig);
            true
        }

        // Given two reductions, create a single reduction that runs
        // both in composition.
        Red(ref child, ref gunc) => {
            let f = func.clone();
            let g = gunc.clone();
            red_mutate(target, child, Rc::new(move |ts| f(&g(ts))));
            true
        }

        _ => {
            red_mutate(target, l, func.clone());
            false
        }
    }
}

pub fn ukn<R, D>() -> Expt<R, D>
where
    R: Semiring,
    D: PartialEq + Copy,
{
    expr(Brz::Ukn, Nullable::Unvisited)
}

pub fn derive<R, D>(n: &Expt<R, D>, c: &D) -> Expt<R, D>
where
    R: Semiring + 'static,
    D: PartialEq + Copy + Clone + 'static,
{
    use self::Brz::*;

    {
        if let Some(kd) = &n.borrow().known_derivative {
            if kd.token == *c {
                return kd.derivative.clone();
            }
        }
    }
    
    let mut res = match &n.borrow().expr {
        Emp => emp(),
        Eps(_) => emp(),
        Sym(f) => eps(f.is(c)),
        Seq(_, _) | Red(_, _) | Alt(_, _) => ukn(),
        Ukn => unreachable!(),
    };

    // This scope is absolutely necessary, as we reborrow this object
    // later in the same function.
    {
        let mut nbm = n.borrow_mut();
        nbm.known_derivative = Some(DerivativeCache {
            token: *c,
            derivative: res.clone(),
        })
    }

    match &n.borrow().expr {
        Emp | Eps(_) | Sym(_) => {}
        Seq(l, r) => {
            if nullable(l) {
                let dl = derive(l, c);
                let dr = derive(r, c);
                let cont_l = l.clone();
                let red = red(
                    &dr,
                    Rc::new(move |ts2| {
                        let ts1 = &parsenull(&cont_l);
                        Rc::new(ts1.mul(ts2))
                    }),
                );
                alt_mutate(&mut res, &red, &seq(&dl, r))
            } else {
                let dl = derive(l, c);
                seq_mutate_optimized(&mut res, &dl, r);
            }
        }
        Alt(l, r) => { alt_mutate_optimized(&mut res, &derive(&l, c), &derive(&r, c)); },
        Red(r, f) => { red_mutate_optimized(&mut res, &derive(&r, c), &f.clone()); },
        Ukn => unreachable!(),
    };

    res
}

pub fn nullable<R, D>(node: &Expt<R, D>) -> bool
where
    R: Semiring,
    D: PartialEq + Copy,
{
    cached_nullable(&node, None, &Nullable::Unvisited)
}

pub fn maybe_add_listener<R, D>(parent: Option<&Expt<R, D>>, node: &Expt<R, D>)
where
    R: Semiring,
    D: PartialEq + Copy,
{
    if let Some(parent) = parent {
        node.borrow_mut().listeners.push(parent.clone());
    }
}

fn cached_nullable<R, D>(node: &Expt<R, D>, parent: Option<&Expt<R, D>>, status: &Nullable) -> bool
where
    R: Semiring,
    D: PartialEq + Copy,
{
    use self::Nullable::*;

    // Sorry about this; this dance is necessary to prevent a
    // borrow/borrow exception.

    let addparent = {
        match &node.borrow().nullable {
            Accept => return true,
            Reject | InProgress => true,
            Unvisited => false,
        }
    };

    if addparent {
        maybe_add_listener(parent, node);
        return false;
    }

    node.borrow_mut().nullable = InProgress;

    if compute_notify_nullable(node, status) {
        true
    } else {
        maybe_add_listener(parent, node);
        false
    }
}

fn compute_notify_nullable<R, D>(node: &Expt<R, D>, status: &Nullable) -> bool
where
    R: Semiring,
    D: PartialEq + Copy,
{
    use self::Nullable::*;
    if !base_nullable(node, status) {
        return false;
    }

    node.borrow_mut().nullable = Accept;
    for childnode in &node.borrow().listeners {
        compute_notify_nullable(childnode, status);
    }
    node.borrow_mut().listeners.clear();
    true
}

fn base_nullable<R, D>(node: &Expt<R, D>, status: &Nullable) -> bool
where
    R: Semiring,
    D: PartialEq + Copy,
{
    use self::Brz::*;
    match &node.borrow().expr {
        Emp => false,
        Eps(_) => true,
        Sym(_) => false,
        Alt(cl, cr) => {
            cached_nullable(&cl, Some(node), status) || cached_nullable(&cr, Some(node), status)
        }
        Seq(cl, cr) => {
            cached_nullable(&cl, Some(node), status) && cached_nullable(&cr, Some(node), status)
        }
        Red(cl, _) => cached_nullable(&cl, Some(node), status),
        Ukn => unreachable!(),
    }
}

pub fn parsenull<R, D>(r: &Expt<R, D>) -> Rc<R>
where
    R: Semiring,
    D: PartialEq + Copy,
{
    use self::Brz::*;

    if let Some(product) = &r.borrow().product {
        return product.clone();
    }

    if ! nullable(&r) {
        return Rc::new(R::zero());
    }
    
    let product = match &r.borrow().expr {
        Emp => Rc::new(R::zero()),
        Eps(s) => s.clone(),
        Red(c, f) => f(&parsenull(c)),
        Sym(_) => Rc::new(R::zero()),
        Seq(l, r) => Rc::new(parsenull(&l).mul(&parsenull(&r))),
        Alt(l, r) => Rc::new(parsenull(&l).add(&parsenull(&r))),
        Ukn => unreachable!(),
    };

    {
        let mut nbm = r.borrow_mut();
        nbm.product = Some(product.clone());
    }

    product
}

pub fn parse<R, D, I>(r: &Expt<R, D>, source: &mut I) -> Rc<R>
where
    R: Semiring + 'static,
    D: PartialEq + Copy + Clone + 'static,
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
            expr(Brz::Sym(Rc::new(SimpleSym { c: sample })), Nullable::Reject)
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
            expr(Brz::Sym(Rc::new(SimpleSym { c: sample })), Nullable::Reject)
        }

        pub fn asy() -> Expt<Recognizer, char> {
            expr(Brz::Sym(Rc::new(AnySym {})), Nullable::Reject)
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
            expr(Brz::Sym(Rc::new(AnySym {})), Nullable::Reject)
        }

        pub fn symi(sample: char) -> Expt<Leftlong, Pc> {
            expr(Brz::Sym(Rc::new(RangeSym(sample))), Nullable::Reject)
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
            assert_eq!(result, &*ret);
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
            expr(Brz::Sym(Rc::new(ParserSym { c: sample })), Nullable::Reject)
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
            let ret = &parse(case, &mut sample.to_string().chars()).0;
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

    #[test]
    fn recursion_loop() {
        pub fn sym(sample: char) -> Expt<Parser, char> {
            expr(Brz::Sym(Rc::new(ParserSym{ c: sample })), Nullable::Reject)
        }

        let ee = seq(&sym('e'), &sym('e'));
        let mut estar = ukn();
        let eep = seq(&ee, &estar);
        let eto = eps(Parser::one());
        alt_mutate(&mut estar, &eto, &eep);
        let beer = seq(&sym('b'), &seq(&estar, &sym('r')));

        let cases = [
            ("br", &beer, "br", Some("br")),
            ("beer", &beer, "beer", Some("beer")),
            ("beeeeeer", &beer, "beeeeeer", Some("beeeeeer")),
            ("bad beeer", &beer, "beeer", None),
            ("bad bear", &beer, "bear", None)
        ];

        for (name, case, sample, result) in &cases {
            println!("{:?}", name);
            let ret = &parse(case, &mut sample.to_string().chars()).0;
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
