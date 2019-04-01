//! This crate provides a series of simple functions for building a
//! regular expression, and an `accept` function which takes a
//! completed regular expression and a string and returns a boolean
//! value describing if the expression matched the string (or not).
//!

use hashbrown::{HashMap, HashSet};
use std::cell::{Ref, RefMut, RefCell};
use std::hash::{Hash, Hasher};
use std::rc::Rc;
use std::ops::Deref;

macro_rules! set {
    ( $( $x:expr ),* ) => {{
        #[allow(unused_mut)]
        let mut temp_set = HashSet::new();
        $( temp_set.insert($x); )*
        temp_set //
    }};
}

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
    Eps(Rc<R>),
    Sym(Rc<Sym<R, D>>),
    Alt(Brzi<R, D>, Brzi<R, D>),
    Seq(Brzi<R, D>, Brzi<R, D>),
    Rep(Brzi<R, D>),
    Red(Brzi<R, D>, Rc<Fn(&Rc<R>) -> Rc<R>>),
    Ukn,
}

#[derive(Copy, Clone, Debug, PartialEq)]
pub enum Nullable {
    Accept,
    Reject,
    InProgress,
    Unvisited,
}

pub struct BMeta<R, D>
where
    R: Semiring
{
    token: Option<D>,
    derivative: Option<Rc<Brzi<R, D>>>,
    listeners: HashSet<Brzi<R, D>>,
    nullable: Nullable,
    value: Option<Rc<R>>
}
 
pub struct Brzi<R, D>(Rc<RefCell<Brz<R, D>>>, *mut Brz<R, D>, Rc<RefCell<BMeta<R, D>>>)
where
    R: Semiring;

impl<R, D> Brzi<R, D>
where
    R: Semiring,
{
    pub fn new(b: Brz<R, D>, n: Nullable) -> Brzi<R, D> {
        let r = Rc::new(RefCell::new(b));
        let p = r.as_ptr();
        Brzi(r, p, Rc::new(RefCell::new(BMeta {
            token: None,
            derivative: None,
            value: None,
            nullable: n,
            listeners: HashSet::new()
        })))
    }

    pub fn borrow(&self) -> Ref<Brz<R, D>> {
        self.0.borrow()
    }

    pub fn replace(&mut self, b: Brz<R, D>, n: Nullable) {
        self.0.replace(b);
        let mut m = self.meta_mut();
        m.nullable = n;
    }

    pub fn meta(&mut self) -> Ref<BMeta<R, D>> {
        self.2.borrow()
    }

    pub fn meta_mut(&mut self) -> RefMut<BMeta<R, D>> {
        self.2.borrow_mut()
    }
    
}

impl<R, D> Clone for Brzi<R, D>
where
    R: Semiring,
{
    fn clone(&self) -> Brzi<R, D> {
        Brzi(self.0.clone(), self.1, self.2.clone())
    }
}

impl<R, D> std::cmp::PartialEq for Brzi<R, D>
where
    R: Semiring,
{
    fn eq(&self, other: &Brzi<R, D>) -> bool {
        Rc::ptr_eq(&self.0, &other.0)
    }
}

impl<'a, R, D> Eq for Brzi<R, D> where R: Semiring {}

impl<R, D> Hash for Brzi<R, D>
where
    R: Semiring,
{
    fn hash<H: Hasher>(&self, state: &mut H) {
        self.1.hash(state);
    }
}

pub fn symproto<R, D, S>(s: S) -> Brzi<R, D>
where
    R: Semiring,
    S: Sym<R, D> + 'static,
{
    Brzi::new(Brz::Sym(Rc::new(s)), Nullable::Reject)
}

pub fn emp<R, D>() -> Brzi<R, D>
where
    R: Semiring,
{
    Brzi::new(Brz::Emp, Nullable::Reject)
}

pub fn set_emp<R, D>(target: &mut Brzi<R, D>)
where
    R: Semiring,
{
    target.replace(Brz::Emp, Nullable::Reject);
}

pub fn eps<R, D>(rig: R) -> Brzi<R, D>
where
    R: Semiring,
{
    Brzi::new(Brz::Eps(Rc::new(rig)), Nullable::Accept)
}

pub fn set_eps<R, D>(target: &mut Brzi<R, D>, rig: Rc<R>)
where
    R: Semiring,
{
    target.replace(Brz::Eps(rig.clone()), Nullable::Accept);
}

pub fn alt<R, D>(l: &Brzi<R, D>, r: &Brzi<R, D>) -> Brzi<R, D>
where
    R: Semiring,
{
    use self::Brz::*;
    match (&*l.borrow(), &*r.borrow()) {
        (_, Emp) => l.clone(),
        (Emp, _) => r.clone(),
        _ => Brzi::new(Brz::Alt(l.clone(), r.clone()), Nullable::Unvisited),
    }
}

pub fn set_alt<R, D>(target: &mut Brzi<R, D>, l: &Brzi<R, D>, r: &Brzi<R, D>)
where
    R: Semiring,
{
    target.replace(Brz::Alt(l.clone(), r.clone()), Nullable::Unvisited);
}

fn set_optimized_alt<R, D>(target: &mut Brzi<R, D>, l: &Brzi<R, D>, r: &Brzi<R, D>) -> bool
where
    R: Semiring + 'static,
{
    match (&*l.borrow(), &*r.borrow()) {
        (Brz::Eps(ref leps), Brz::Eps(ref reps)) => {
            set_eps(target, Rc::new((*leps).add(&*reps)));
            true
        }

        _ => {
            set_alt(target, l, r);
            false
        }
    }
}

pub fn seq<R, D>(l: &Brzi<R, D>, r: &Brzi<R, D>) -> Brzi<R, D>
where
    R: Semiring,
{
    use self::Brz::*;
    match (&*l.borrow(), &*r.borrow()) {
        (_, Emp) => emp(),
        (Emp, _) => emp(),
        _ => Brzi::new(Brz::Seq(l.clone(), r.clone()), Nullable::Unvisited),
    }
}

pub fn set_seq<R, D>(target: &mut Brzi<R, D>, l: &Brzi<R, D>, r: &Brzi<R, D>)
where
    R: Semiring,
{
    target.replace(Brz::Seq(l.clone(), r.clone()), Nullable::Unvisited);
}

pub fn optimized_seq<R, D>(l: &Brzi<R, D>, r: &Brzi<R, D>) -> Brzi<R, D>
where
    R: Semiring + 'static,
{
    use self::Brz::*;
    match &*l.borrow() {
        Emp => emp(),

        Eps(ref rig) => {
            let closed_rig = rig.clone();
            red(&r, Rc::new(move |rg| Rc::new(closed_rig.mul(&rg))))
        }

        _ => seq(l, r),
    }
}

fn set_optimized_seq<R, D>(target: &mut Brzi<R, D>, l: &Brzi<R, D>, r: &Brzi<R, D>) -> bool
where
    R: Semiring + 'static,
{
    use self::Brz::*;
    match &*l.borrow() {
        Emp => {
            set_emp(target);
            true
        }

        Eps(ref rig) => {
            let closed_rig = rig.clone();
            set_red(target, &r, Rc::new(move |rg| Rc::new(closed_rig.mul(&rg))));
            true
        }

        _ => {
            set_seq(target, l, r);
            false
        }
    }
}

pub fn rep<R, D>(l: &Brzi<R, D>) -> Brzi<R, D>
where
    R: Semiring,
{
    Brzi::new(Brz::Rep(l.clone()), Nullable::Accept)
}

pub fn set_rep<R, D>(target: &mut Brzi<R, D>, l: &Brzi<R, D>)
where
    R: Semiring,
{
    target.replace(Brz::Rep(l.clone()), Nullable::Accept);
}

pub fn red<R, D>(l: &Brzi<R, D>, func: Rc<Fn(&Rc<R>) -> Rc<R>>) -> Brzi<R, D>
where
    R: Semiring + 'static,
{
    let closed_fn = func.clone();
    Brzi::new(Brz::Red(l.clone(), Rc::new(move |ts| closed_fn(ts))), Nullable::Unvisited)
}

pub fn set_red<R, D>(target: &mut Brzi<R, D>, l: &Brzi<R, D>, func: Rc<Fn(&Rc<R>) -> Rc<R>>)
where
    R: Semiring + 'static,
{
    target.replace(Brz::Red(l.clone(), func.clone()), Nullable::Unvisited);
}

pub fn optimized_red<R, D>(l: &Brzi<R, D>, func: Rc<Fn(&Rc<R>) -> Rc<R>>) -> Brzi<R, D>
where
    R: Semiring + 'static,
{
    use self::Brz::*;

    match &*l.borrow() {
        Emp => emp(),

        Eps(ref rig) => {
            let res_rig = func(&*rig);
            Brzi::new(Eps(res_rig), Nullable::Accept)
        }

        Red(ref child, ref gunc) => {
            let f = func.clone();
            let g = gunc.clone();
            red(child, Rc::new(move |ts| f(&g(ts))))
        }

        _ => red(l, func),
    }
}

pub fn set_optimized_red<R, D>(
    target: &mut Brzi<R, D>,
    l: &Brzi<R, D>,
    func: &Rc<Fn(&Rc<R>) -> Rc<R>>,
) -> bool
where
    R: Semiring + 'static,
{
    use self::Brz::*;

    match &*l.borrow() {
        Emp => {
            set_emp(target);
            true
        }

        Eps(ref rig) => {
            let res_rig = func(&*rig);
            set_eps(target, res_rig);
            true
        }

        // Given two reductions, create a single reduction that runs
        // both in composition.
        Red(ref child, ref gunc) => {
            let f = func.clone();
            let g = gunc.clone();
            set_red(target, child, Rc::new(move |ts| f(&g(ts))));
            true
        }

        _ => {
            set_red(target, l, func.clone());
            false
        }
    }
}

pub fn ukn<R, D>() -> Brzi<R, D>
where
    R: Semiring,
{
    Brzi::new(Brz::Ukn, Nullable::Unvisited)
}

pub fn parsenull<R, D>(node: &mut Brzi<R, D>) -> Rc<R>
where
    R: Semiring
{
    use self::Brz::*;
    {
        let meta = node.meta();
        if let Some(value) = &meta.value {
            return value.clone();
        }
    }

    if ! nullable(node) {
        return Rc::new(R::zero());
    }
    
    let value = match &*node.borrow() {
        Emp => Rc::new(R::zero()),
        Eps(ref rig) => rig.clone(),
        Rep(_) => Rc::new(R::one()),
        Sym(_) => Rc::new(R::zero()),
        Seq(l, r) => Rc::new((parsenull(&mut l)).mul(&(parsenull(&mut r)))),
        Alt(l, r) => Rc::new((parsenull(&mut l)).add(&(parsenull(&mut r)))),
        Red(c, f) => f(&parsenull(&mut c)),
        Ukn => unreachable!(),
    };
    
    {
        let meta = node.meta_mut();
        meta.value = Some(value);
    }

    value
}


pub fn derive<R, D>(node: &Brzi<R, D>, c: &D) -> Brzi<R, D>
where
    R: Semiring + 'static,
    D: 'static
{
    use self::Brz::*;
    
    {
        let meta = node.meta();
        if let Some(token) = meta.token {
            return meta.derivative.unwrap().deref().clone();
        }
    }
    
    let mut next_derivative = match &*node.borrow() {
        Emp => emp(),
        Eps(_) => emp(),
        Sym(f) => eps(f.is(c)),
        Seq(_, _) | Alt(_, _) | Red(_, _) => ukn(),
        Rep(_) => ukn(),
        Ukn => unreachable!(),
    };
    
    {
        let meta = node.meta_mut();
        meta.token = Some(*c);
        meta.derivative = Some(Rc::new(next_derivative));
    }
    
    match &*node.borrow() {
        Seq(cl, cr) => {
            if nullable(cl) {
                let l = derive(&cl, c);
                let r = derive(&cr, c);
                let sac_l = cl.clone();
                let red = optimized_red(
                    &r,
                    Rc::new(move |ts2| {
                        let ts1 = parsenull(&mut sac_l);
                        Rc::new(ts1.mul(&ts2))
                    }),
                );
                set_alt(&mut next_derivative, &red, &optimized_seq(&l, &cr));
            } else {
                set_optimized_seq(&mut next_derivative, &derive(&cl, c), &cr);
            }
        }
        
        Alt(l, r) => {
            set_optimized_alt(
                &mut next_derivative,
                &derive(&l, c),
                &derive(&r, c),
            );
        }
        
        Rep(r) => {
            set_optimized_seq(&mut next_derivative, &derive(&r, c), &node.clone());
        }
        
        Red(ch, func) => {
            set_optimized_red(&mut next_derivative, &derive(&ch, c), func);
        }
        
        _ => {}
    };
    
    next_derivative
}

pub fn nullable<R, D>(node: &Brzi<R, D>) -> bool
where
    R: Semiring
{
    cached_nullable(&node, None, &Nullable::Unvisited)
}

pub fn maybe_add_listener<R, D>(parent: Option<&Brzi<R, D>>, node: &Brzi<R, D>)
where
    R: Semiring
{
    if let Some(parent) = parent {
        let meta = node.meta_mut();
        meta.listeners.insert(node.clone());
    }
}

fn cached_nullable<R, D>(node: &Brzi<R, D>, parent: Option<&Brzi<R, D>>, status: &Nullable) -> bool
where
    R: Semiring
{
    use self::Nullable::*;

    {
        let meta = node.meta();
        match meta.nullable {
            Accept => { return true },
            Reject | InProgress => {
                maybe_add_listener(parent, node);
                return false
            },
            Unvisited => {},
        }
    }

    {
        let meta = node.meta_mut();
        meta.nullable = InProgress;
    }
    
    if compute_notify_nullable(node, status) {
        true
    } else {
        maybe_add_listener(parent, node);
        false
    }
}

fn compute_notify_nullable<R, D>(node: &Brzi<R, D>, status: &Nullable) -> bool
where
    R: Semiring
{
    use self::Nullable::*;
    if ! base_nullable(node, status) {
        return false;
    }
    
    {
        let meta = node.meta_mut();
        meta.nullable = Accept;
    }

    {
        let meta = node.meta();
        for mut childnode in meta.listeners {
            compute_notify_nullable(&mut childnode, status);
        }
        meta.listeners.clear();
    }
    true
}

fn base_nullable<R, D>(node: &Brzi<R, D>, status: &Nullable) -> bool
where
    R: Semiring
{
    use self::Brz::*;
    match &*node.0.borrow() {
        Emp => false,
        Eps(_) => true,
        Sym(_) => false,
        Rep(_) => true,
        Alt(cl, cr) => {
            cached_nullable(&cl, Some(node), status)
                || cached_nullable(&cr, Some(node), status)
        }
        Seq(cl, cr) => {
            cached_nullable(&cl, Some(node), status)
                && cached_nullable(&cr, Some(node), status)
        }
        Red(cl, _) => cached_nullable(&cl, Some(node), status),
        Ukn => unreachable!(),
    }
}

pub fn parse<R, D, I>(b: &Brzi<R, D>, source: &mut I) -> Rc<R>
where
    R: Semiring + 'static,
    D: 'static,
    I: Iterator<Item = D>,
{
    let start = if let Some(c) = source.next() {
        derive(&b, &c)
    } else {
            return parsenull(&mut b);
    };
    
    let innerderive = |b2, c2| derive(&b2, &c2);
    parsenull(&mut source.fold(start, innerderive))
}


#[cfg(test)]
mod tests {

    use super::*;

    //  ___                       _
    // | _ \___ __ ___  __ _ _ _ (_)______
    // |   / -_) _/ _ \/ _` | ' \| |_ / -_)
    // |_|_\___\__\___/\__, |_||_|_/__\___|
    //                 |___/

    #[derive(Debug, Hash, Copy, Clone, PartialEq)]
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
        pub fn sym(sample: char) -> Brzi<Recognizer, char> {
            symproto(SimpleSym { c: sample })
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

        pub fn sym(sample: char) -> Brzi<Recognizer, char> {
            symproto(SimpleSym { c: sample })
        }

        pub fn asy() -> Brzi<Recognizer, char> {
            symproto(AnySym {})
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

    #[derive(Debug, Hash, Clone, Eq, PartialEq)]
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
    #[derive(Hash, Eq, PartialEq, Clone)]
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
        pub fn asym() -> Brzi<Leftlong, Pc> {
            symproto(AnySym {})
        }

        pub fn symi(sample: char) -> Brzi<Leftlong, Pc> {
            symproto(RangeSym(sample))
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
            assert_eq!(result, ret.deref());
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
        pub fn sym(sample: char) -> Brzi<Parser, char> {
            symproto(ParserSym { c: sample })
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
        pub fn sym(sample: char) -> Brzi<Parser, char> {
            symproto(ParserSym { c: sample })
        }

        let ee = seq(&sym('e'), &sym('e'));
        let mut estar = ukn();
        let eep = seq(&ee, &estar);
        let eto = eps(Parser::one());
        set_alt(&mut estar, &eto, &eep);
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
