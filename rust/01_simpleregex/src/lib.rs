use std::rc::Rc;

// data Reg = Eps | Sym Char | Alt Reg Reg | Seq Reg Reg | Rep Reg

#[derive(Debug)]
pub enum Reg {
    Eps,
    Sym(char),
    Alt(Rc<Reg>, Rc<Reg>),
    Seq(Rc<Reg>, Rc<Reg>),
    Rep(Rc<Reg>),
}

// Some rust-specific helpers to make constructing regular expressions
// easier.

pub fn eps() -> Rc<Reg> {
    Rc::new(Reg::Eps)
}
pub fn sym(c: char) -> Rc<Reg> {
    Rc::new(Reg::Sym(c))
}
pub fn alt(r1: &Rc<Reg>, r2: &Rc<Reg>) -> Rc<Reg> {
    Rc::new(Reg::Alt(r1.clone(), r2.clone()))
}
pub fn seq(r1: &Rc<Reg>, r2: &Rc<Reg>) -> Rc<Reg> {
    Rc::new(Reg::Seq(r1.clone(), r2.clone()))
}
pub fn rep(r1: &Rc<Reg>) -> Rc<Reg> {
    Rc::new(Reg::Rep(r1.clone()))
}

// split :: [a] -> [([a], [a])]
// split []     = [([], [])]
// split (c:cs) = ([], c : cs) : [(c : s1, s2) | (s1, s2) <- split cs]

pub fn split(s: &[char]) -> Vec<(Vec<char>, Vec<char>)> {
    if s.is_empty() {
        return vec![(vec![], vec![])];
    }

    let mut ret = vec![(vec![], s.to_vec())];
    let c = s[0];

    fn permute(c: char, s1: &mut Vec<char>, s2: &[char]) -> (Vec<char>, Vec<char>) {
        let mut r1 = vec![c];
        r1.append(s1);
        (r1, s2.to_vec())
    }

    ret.append(
        &mut split(&s[1..])
            .iter_mut()
            .map(|(s1, s2)| permute(c, s1, &s2))
            .collect(),
    );
    ret
}

// parts :: [a] -> [[[a]]]
// parts []     = [[]]
// parts [c]    = [[[c]]]
// parts (c:cs) = concat [[(c : p) : ps, [c] : p : ps] | p:ps <- parts cs]

// This was challenging to port to Rust.  Haskell's automatic
// conversion of [Char] to String obscured what was going on under the
// covers.
//
// parts (c:cs) = concat [[(c : p) : ps, [c] : p : ps] | p:ps <- parts cs]
// The two elements are:
//
// - ([c]:[[p]]):[[[ps]]]
// The char 'c' is converted to a string, and that string is consed to
// list 'p', and then list 'p' is consed onto the list 'ps'
//
// - [[c]]:[[p]]:[[[ps]]]
// The char 'c' is made into a string and then wrapped in a list, and
// then [[p]] and [[c]] are both consed onto list 'ps'
//
// It really took writing it all out on paper to understand the order
// operation.

pub fn parts(s: &[char]) -> Vec<Vec<Vec<char>>> {
    if s.is_empty() {
        return vec![vec![]];
    }
    if s.len() == 1 {
        return vec![vec![s.to_vec()]];
    }

    let head = s[0];
    let tail = &s[1..];

    let mut ret = vec![];
    for pps in parts(tail) {
        let phead = &pps[0];
        let ptail = &pps[1..];

        let mut left = vec![head];
        left.append(&mut phead.to_vec());

        let mut left_1 = vec![left];
        left_1.append(&mut ptail.to_vec());
        ret.push(left_1);

        let mut right = vec![vec![head]];
        right.push(phead.to_vec());
        right.append(&mut ptail.to_vec());
        ret.push(right);
    }
    ret
}

// accept :: Reg -> String -> Bool
// accept Eps u       = null u
// accept (Sym c) u   = u == [c]
// accept (Alt p q) u = accept p u || accept q u
// accept (Seq p q) u = or [accept p u1 && accept q u2 | (u1, u2) <- split u]
// accept (Rep r) u   = or [and [accept r ui | ui <- ps] | ps <- parts u]

pub fn accept(r: &Reg, s: &[char]) -> bool {
    match r {
        Reg::Eps => s.is_empty(),
        Reg::Sym(c) => (s.len() == 1 && s[0] == *c),
        Reg::Alt(r1, r2) => accept(&r1, s) || accept(&r2, s),
        Reg::Seq(r1, r2) => split(s)
            .into_iter()
            .any(|(u1, u2)| accept(r1, &u1) && accept(r2, &u2)),
        Reg::Rep(r) => parts(s)
            .into_iter()
            .any(|ps| ps.into_iter().all(|u| accept(r, &u))),
    }
}
    
#[cfg(test)]
mod tests {
    use super::*;

    fn vectostr(r: &(Vec<char>, Vec<char>)) -> (String, String) {
        let (a, b) = r;
        let c: String = a.into_iter().collect();
        let d: String = b.into_iter().collect();
        (c, d)
    }

    #[test]
    fn test_split() {
        let c1: Vec<char> = String::from("").chars().into_iter().collect();
        let s: Vec<(String, String)> = split(&c1).into_iter().map(|r| vectostr(&r)).collect();
        assert_eq!(s, [("".to_string(), "".to_string())]);
    }

    #[test]
    fn test_simple() {
        let c1: Vec<char> = String::from("acc").chars().into_iter().collect();
        assert_eq!(c1, ['a', 'c', 'c']);

        let c2: Vec<char> = String::from("a").chars().into_iter().collect();
        let onea = sym('a');
        assert!(accept(&onea, &c2));

        let c3: Vec<char> = String::from("abab").chars().into_iter().collect();
        let nocs = rep(&alt(&sym('a'), &sym('b')));
        assert!(accept(&nocs, &c3));
    }

    #[test]
    fn test_seq() {
        let c3: Vec<char> = String::from("abc").chars().into_iter().collect();
        let abc = seq(&sym('a'), &seq(&sym('b'), &sym('c')));
        assert!(accept(&abc, &c3));
    }

    #[test]
    fn test_rc() {
        let c3: Vec<char> = String::from("abab").chars().into_iter().collect();
        let ab = seq(&sym('a'), &sym('b'));
        let abab = seq(&ab, &ab);
        assert!(accept(&abab, &c3));
    }

    #[test]
    fn test_empty_rep() {
        let c3: Vec<char> = String::from("").chars().into_iter().collect();
        let a = rep(&sym('a'));
        assert!(accept(&a, &c3));
    }

    #[test]
    fn test_two() {
        let c4: Vec<char> = String::from("abcc").chars().into_iter().collect();
        let nocs = rep(&alt(&sym('a'), &sym('b')));
        let onec = seq(&nocs, &sym('c'));
        let evencs = seq(&rep(&seq(&onec, &onec)), &nocs);
        assert!(accept(&evencs, &c4))
    }
}
