interface Regcom { kind: string };
class Eps implements Regcom { kind: "eps"; };
class Sym implements Regcom { kind: "sym"; s: string; }
class Alt implements Regcom { kind: "alt"; l: Regex; r: Regex };
class Seq implements Regcom { kind: "seq"; l: Regex; r: Regex };
class Rep implements Regcom { kind: "rep"; r: Regex };

function eps():                   Eps { return { kind: "eps" }; };
function sym(c: string):          Sym { return { kind: "sym", s: c }; };
function alt(l: Regex, r: Regex): Alt { return { kind: "alt", l: l, r: r }; };
function seq(l: Regex, r: Regex): Seq { return { kind: "seq", l: l, r: r }; };
function rep(r: Regex):           Rep { return { kind: "rep", r: r }; };

type Regex = Eps | Sym | Alt | Seq | Rep;

// split :: [a] -> [([a], [a])]
// split []     = [([], [])]
// split (c:cs) = ([], c : cs) : [(c : s1, s2) | (s1, s2) <- split cs]

function split(s: string) {
    if (s.length == 0) {
        return [["", ""]];  
    }
    return [["", s.slice()]].concat(split(s.slice(1)).map((v) => [s[0].slice().concat(v[0].slice()), v[1].slice()]));
}

// parts :: [a] -> [[[a]]]
// parts []     = [[]]
// parts [c]    = [[[c]]]
// parts (c:cs) = concat [[(c : p) : ps, [c] : p : ps] | p:ps <- parts cs]

function parts(s: string): Array<Array<string>> {
    if (s.length == 0) {
        return [[]];
    }

    if (s.length == 1) {
        return [[s]];
    }

    let c = s[0];
    let cs = s.slice(1);
    return parts(cs).reduce((acc, pps) => {
        let p: string  = pps[0];
        let ps: Array<string> = pps.slice(1);
        let l:  Array<string> = [c + p].concat(ps);
        let r:  Array<string> = [c].concat(p).concat(ps);
        return acc.concat([l, r]);
    }, [[]]).filter((c) => c.length != 0);
}

function one(a: Array<any>, test: (s: any) => boolean): boolean {
    return a.reduce((acc: boolean, sc: any) => acc || test(sc), false);
}

function all(a: Array<any>, test: (s: any) => boolean): boolean {
    return a.reduce((acc: boolean, sc: any) => acc && test(sc), true);
}


function accept(r: Regex, s: string): boolean {
    switch(r.kind) {
    case "eps":
        return s.length == 0;
    case "sym":
        return s.length == 1 && r.s == s[0];
    case "alt":
        return accept(r.l, s) || accept(r.r, s);
    case "seq":
        return split(s).some((v: Array<string>) => accept(r.l, v[0]) && accept(r.r, v[1]));
    case "rep":
        return parts(s).some((v: Array<string>) => v.every((u: string) => accept(r.r, u)));
    }
}

function run_tests() {

    function assert(l: any) {
        console.log("   ", l);
    }

    let units = {
        test_simple: () => {
            let onea = sym("a");
            assert(accept(onea, "a"));

            let nocs = rep(alt(sym("a"), sym("b")));
            assert(accept(nocs, "abab"));
        },

        test_seq: () => {
            let abc = seq(sym("a"), seq(sym("b"), sym("c")));
            assert(accept(abc, "abc"));
        },

        test_rc: () => {
            let ab = seq(sym("a"), sym("b"));
            let abab = seq(ab, ab);
            assert(accept(abab, "abab"));
        },

        test_fail: () => {
            let ab = seq(sym("a"), sym("b"));
            let abab = seq(ab, ab);
            assert(! accept(abab, "abacb"));
        },

        test_empty_rep: () => {
            let a = rep(sym("a"));
            assert(accept(a, ""));
        },

        test_some_rep: () => {
            let a = rep(sym("a"));
            assert(accept(a, "a"));
        },

        test_many_rep: () => {
            let a = rep(sym("a"));
            assert(accept(a, "aaaaaaa"));
        },

        test_many_rep_dead_l: () => {
            let a = rep(sym("a"));
            assert(! accept(a, "!aaaaaa"));
        },

        test_many_rep_dead_r: () => {
            let a = rep(sym("a"));
            assert(! accept(a, "aaaaaa!"));
        },

        test_many_rep_dead_m: () => {
            let a = rep(sym("a"));
            assert(! accept(a, "aaa!aaa"));
        },

        test_two: () => {
            let nocs = rep(alt(sym("a"), sym("b")));
            let onec = seq(nocs, sym("c"));
            let evencs = seq(rep(seq(onec, onec)), nocs);
            assert(accept(evencs, "abcc"));
            assert(accept(evencs, "abccababbbbcc"));
        }
    }

    console.log("Running tests...");
    for (let k of Object.keys(units)) {
        console.log(k); units[k]();
    }
}

run_tests();
