use std::ops::Deref;
use std::rc::Rc;

#[derive(Debug)]
pub enum Brz {
    Emp,
    Eps,
    Sym(char),
    Alt(Rc<Brz>, Rc<Brz>),
    Seq(Rc<Brz>, Rc<Brz>),
    Rep(Rc<Brz>),
}

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

pub fn emp() -> Rc<Brz> {
    Rc::new(Brz::Emp)
}

pub fn eps() -> Rc<Brz> {
    Rc::new(Brz::Eps)
}

pub fn sym(c: char) -> Rc<Brz> {
    Rc::new(Brz::Sym(c))
}

pub fn alt(r1: &Rc<Brz>, r2: &Rc<Brz>) -> Rc<Brz> {
    cond!(
        matches!(r1.deref(), Brz::Emp) => { r2.clone() },
        matches!(r2.deref(), Brz::Emp) => { r1.clone() },
        _ => { Rc::new(Brz::Alt(r1.clone(), r2.clone())) }
    )
}

pub fn seq(r1: &Rc<Brz>, r2: &Rc<Brz>) -> Rc<Brz> {
    cond!(
        matches!(r1.deref(), Brz::Emp) => { emp() },
        matches!(r2.deref(), Brz::Emp) => { emp() },
        _ => { Rc::new(Brz::Seq(r1.clone(), r2.clone())) }
    )
}

pub fn rep(r1: &Rc<Brz>) -> Rc<Brz> {
    Rc::new(Brz::Rep(r1.clone()))
}

pub fn derive(n: &Rc<Brz>, c: char) -> Rc<Brz> {
    use self::Brz::*;

    match n.deref() {
        Emp => emp(),
        Eps => emp(),
        Sym(u) => {
            if c == *u { eps() } else { emp() }
        }
        Seq(l, r) => {
            let s = seq(&derive(l, c), r);
            if nullable(l) {
                alt(&s, &derive(r, c))
            } else {
                s
            }
        }
        Alt(l, r) => alt(&derive(l, c), &derive(r, c)) ,
        Rep(r)    => seq(&derive(r, c), &n.clone()),
    }
}

pub fn nullable(n: &Rc<Brz>) -> bool {
    use self::Brz::*;

    match n.deref() {
        Emp => false,
        Eps => true,
        Sym(_) => false,
        Seq(l, r) => nullable(l) && nullable(r),
        Alt(l, r) => nullable(l) || nullable(r),
        Rep(_) => true,
    }
}

pub fn accept(n: &Rc<Brz>, s: String) -> bool {
    use self::Brz::*;

    let mut source = s.chars().peekable();
    let mut r = n.clone();
    loop {
        match source.next() {
            None => { break nullable(&r) },
            Some(ref c) => {
                let np = derive(&r, *c);
                println!("{:?}", np);
                match np.deref() {
                    Emp => return false,
                    Eps => {
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
            ("one rep", rep(&sym('a')), "a", true),
            ("short multiple failed rep", rep(&sym('a')), "ab", false),
            ("multiple rep", rep(&sym('a')), "aaaaaaaaa", true),
            ("multiple rep with failure", rep(&sym('a')), "aaaaaaaaab", false),
            ("sequence", seq(&sym('a'), &sym('b')), "ab", true),
            ("sequence with empty", seq(&sym('a'), &sym('b')), "", false),
            ("bad short sequence", seq(&sym('a'), &sym('b')), "a", false),
            ("bad long sequence", seq(&sym('a'), &sym('b')), "abc", false)
        ];
        
        for (name, case, sample, result) in &cases {
            println!("{:?}", name);
            assert_eq!(accept(case, sample.to_string()), *result);
        }
    }
}
