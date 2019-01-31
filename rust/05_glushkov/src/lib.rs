//! This crate provides a series of simple functions for building a
//! regular expression, and an `accept` function which takes a
//! completed regular expression and a string and returns a boolean
//! value describing if the expression matched the string (or not).
//!
//! # Quick Preview
//!
//! ```
//! use glushkov::*;
//! // `(fred|dino)`
//! let expr = alt(&seq(&sym('f'), &seq(&sym('r'), &seq(&sym('e'), &sym('d')))),
//!                &seq(&sym('d'), &seq(&sym('i'), &seq(&sym('n'), &sym('o')))));
//! accept(&expr, "fred") == true;
//! accept(&expr, "dino") == true;
//! accept(&expr, "wilma") == false;
//! ```

use std::ops::Deref;
use std::rc::Rc;

#[derive(Debug)]
pub enum Glu {
    Eps,
    Sym(bool, char),
    Alt(Rc<Glu>, Rc<Glu>),
    Seq(Rc<Glu>, Rc<Glu>),
    Rep(Rc<Glu>),
}

/// Recognize only the empty string
pub fn eps() -> Rc<Glu> {
    Rc::new(Glu::Eps)
}

/// Recognize a single character
pub fn sym(c: char) -> Rc<Glu> {
    Rc::new(Glu::Sym(false, c))
}

/// Recognize alternatives between two other regexes
pub fn alt(r1: &Rc<Glu>, r2: &Rc<Glu>) -> Rc<Glu> {
    Rc::new(Glu::Alt(r1.clone(), r2.clone()))
}

/// Recognize a sequence of regexes in order
pub fn seq(r1: &Rc<Glu>, r2: &Rc<Glu>) -> Rc<Glu> {
    Rc::new(Glu::Seq(r1.clone(), r2.clone()))
}

/// Recognize a regex repeated zero or more times.
pub fn rep(r1: &Rc<Glu>) -> Rc<Glu> {
    Rc::new(Glu::Rep(r1.clone()))
}

// The main function: repeatedly traverses the tree, modifying as it
// goes, generating a new tree, marking the nodes where the expression
// currently "is," for any given character.
//
pub fn shift(g: &Rc<Glu>, m: bool, c: char) -> Rc<Glu> {
    match g.deref() {
        Glu::Eps => eps(),
        Glu::Sym(_, s) => Rc::new(Glu::Sym(m && *s == c, *s)),
        Glu::Alt(r1, r2) => alt(&shift(r1, m, c), &shift(r2, m, c)),
        Glu::Seq(r1, r2) => {
            let l_end = empty(r1);
            let l_fin = ended(r1);
            seq(&shift(r1, m, c), &shift(r2, m && l_end || l_fin, c))
        },
        Glu::Rep(r) => rep(&shift(r, m || ended(r), c)),
    }
}

// Helper function that describes whether or not the expression passed
// in contains the mark; used to determine if, when either the string
// or the expression runs out, if the expression is in an "accept"
// state.
//
pub fn ended(g: &Rc<Glu>) -> bool {
    match g.deref() {
        Glu::Eps => false,
        Glu::Sym(m, _) => *m,
        Glu::Alt(r1, r2) => ended(r1) || ended(r2),
        Glu::Seq(r1, r2) => ended(r1) && empty(r2) || ended(r2),
        Glu::Rep(r) => ended(r),
    }
}

// Helper function that describes whether or not the expression
// supplied handles the empty string.
//
pub fn empty(g: &Rc<Glu>) -> bool {
    match g.deref() {
        Glu::Eps => true,
        Glu::Sym(_, _) => false,
        Glu::Alt(r1, r2) => empty(r1) || empty(r2),
        Glu::Seq(r1, r2) => empty(r1) && empty(r2),
        Glu::Rep(_) => true,
    }
}

/// Takes a regular expression and a string and returns whether or not
/// the expression and the string match (the string belongs to the
/// set of languages recognized by the expression).
pub fn accept(g: &Rc<Glu>, s: &str) -> bool {
    if s.is_empty() {
        return empty(g);
    }

    pub fn ashift(g: Rc<Glu>, c: char) -> Rc<Glu> {
        shift(&g, false, c)
    }

    // This is kinda cool. I wonder if I can make the Brz versions look
    // like this.
    let mut seq = s.chars();
    let start = shift(g, true, seq.next().unwrap());
    ended(&seq.fold(start, ashift))
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn basics() {
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
            assert_eq!(accept(case, &sample.to_string()), *result);
        }
    }
}
