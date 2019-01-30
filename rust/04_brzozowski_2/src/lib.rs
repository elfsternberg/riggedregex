use std::ops::Deref;
use std::rc::Rc;

macro_rules! matches {
    ($expression:expr, $($pattern:tt)+) => {
        match $expression {
            $($pattern)+ => true,
            _ => false
        }
    }
}

macro_rules! cond {
    ($($pred:expr => $body:block),+ ,_ => $default:block) => {
        {
            $(if $pred $body else)+
            $default
        }
    }
}

#[derive(Debug)]
pub struct Emp;
#[derive(Debug)]
pub struct Eps;
#[derive(Debug)]
pub struct Sym(char);
#[derive(Debug)]
pub struct Alt(Rc<Brz>, Rc<Brz>);
#[derive(Debug)]
pub struct Seq(Rc<Brz>, Rc<Brz>);
#[derive(Debug)]
pub struct Rep(Rc<Brz>);

#[derive(Debug)]
pub enum Brz {
    Emp(Emp),
    Eps(Eps),
    Sym(Sym),
    Alt(Alt),
    Seq(Seq),
    Rep(Rep)
}

impl Brz {
    fn derive(&self, c: &char) -> Rc<Brz> {
        match self {
            Brz::Emp(emp) => emp.derive(c),
            Brz::Eps(eps) => eps.derive(c),
            Brz::Sym(sym) => sym.derive(c),
            Brz::Alt(alt) => alt.derive(c),
            Brz::Seq(seq) => seq.derive(c),
            Brz::Rep(rep) => rep.derive(c),
        }
    }

    fn nullable(&self) -> bool {
        match self {
            Brz::Emp(emp) => emp.nullable(),
            Brz::Eps(eps) => eps.nullable(),
            Brz::Sym(sym) => sym.nullable(),
            Brz::Alt(alt) => alt.nullable(),
            Brz::Seq(seq) => seq.nullable(),
            Brz::Rep(rep) => rep.nullable(),
        }
    }

}
                
trait Brznode {
    fn derive(&self, c: &char) -> Rc<Brz>;
    fn nullable(&self) -> bool;
}

impl Brznode for Emp {
    fn derive(&self, _: &char) -> Rc<Brz> {
        Rc::new(Brz::Emp(Emp{}))
    }
    fn nullable(&self) -> bool { false }
}

impl Brznode for Eps {
    fn derive(&self, _: &char) -> Rc<Brz> {
        Rc::new(Brz::Emp(Emp{}))
    }
    fn nullable(&self) -> bool { true }
}

impl Brznode for Sym {
    fn derive(&self, c: &char) -> Rc<Brz> {
        Rc::new(if *c == self.0 { Brz::Eps(Eps{}) } else { Brz::Emp(Emp{}) })
    }
    fn nullable(&self) -> bool { false }
}

pub fn alt(r1: &Rc<Brz>, r2: &Rc<Brz>) -> Rc<Brz> {
    cond!(
        matches!(r1.deref(), Brz::Emp(_)) => { r2.clone() },
        matches!(r2.deref(), Brz::Emp(_)) => { r1.clone() },
        _ => { Rc::new(Brz::Alt(Alt(r1.clone(), r2.clone()))) }
    )
}

impl Brznode for Alt {
    fn derive(&self, c: &char) -> Rc<Brz> {
        let l = &self.0.derive(c);
        let r = &self.1.derive(c);
        alt(l, r)
    }
    fn nullable(&self) -> bool { self.0.nullable() || self.1.nullable() }
}    

pub fn seq(r1: &Rc<Brz>, r2: &Rc<Brz>) -> Rc<Brz> {
    cond!(
        matches!(r1.deref(), Brz::Emp(_)) => { emp() },
        matches!(r2.deref(), Brz::Emp(_)) => { emp() },
        _ => { Rc::new(Brz::Seq(Seq(r1.clone(), r2.clone()))) }
    )
}

impl Brznode for Seq {
    fn derive(&self, c: &char) -> Rc<Brz> {
        let s = seq(&self.0.derive(c), &self.1);
        if self.0.nullable() {
            alt(&s, &self.1.derive(c))
        } else {
            s
        }
    }
    fn nullable(&self) -> bool { self.0.nullable() && self.1.nullable() }
}    

impl Brznode for Rep {
    fn derive(&self, c: &char) -> Rc<Brz> {
        seq(&self.0.derive(c), &rep(&self.0))
    }
    fn nullable(&self) -> bool { true }
}    

pub fn emp() -> Rc<Brz> {
    Rc::new(Brz::Emp(Emp))
}

pub fn eps() -> Rc<Brz> {
    Rc::new(Brz::Eps(Eps))
}

pub fn sym(c: char) -> Rc<Brz> {
    Rc::new(Brz::Sym(Sym(c)))
}

pub fn rep(r1: &Rc<Brz>) -> Rc<Brz> {
    Rc::new(Brz::Rep(Rep(r1.clone())))
}

pub fn accept(n: &Rc<Brz>, s: String) -> bool {
    let mut source = s.chars().peekable();
    let mut r = n.clone();
    loop {
        match source.next() {
            None => { break r.nullable() },
            Some(ref c) => {
                let np = r.derive(c);
                match np.deref() {
                    Brz::Emp(_) => return false,
                    Brz::Eps(_) => {
                        break match source.peek() {
                            None => true,
                            Some(_) => false,
                        }
                    }
                    _ => r = np.clone(),
                }
            }
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn basics() {
        let cases = [
            ("empty", eps(), "", true),
            ("null", emp(), "", false),
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
            ("multiple rep with failure", rep(&sym('a')), "aaaaaaaaab", false),
        ];
        
        for (name, case, sample, result) in &cases {
            println!("{:?}", name);
            assert_eq!(accept(case, sample.to_string()), *result);
        }
    }
}
