#!/usr/bin/env python3

from collections import namedtuple
import re

Emp = namedtuple("Emp", [])
Eps = namedtuple("Eps", ["tok"])
Sym = namedtuple("Sym", ["c"])
Alt = namedtuple("Alt", ["l", "r"])
Seq = namedtuple("Seq", ["l", "r"])
Rep = namedtuple("Rep", ["r"])

cname = re.compile(r'^(\w+)\(')


def cn(s):
    """ Find the canonical name of the regex op"""
    return cname.match(s.__doc__).group(1)


def derive(r, c):
    """ Take a regex op and a character, and return the derivative regex op."""
    def sym(r, c):
        if c == r.c:
            return Eps(set([c]))
        return Emp()

    def alt(r, c):
        l1 = derive(r.l, c)
        r1 = derive(r.r, c)
        if cn(l1) == 'Emp':
            return r1
        if cn(r1) == 'Emp':
            return l1
        return Alt(l1, r1)

    def seq(r, c):
        if nullable(r.l):
            return Alt(Seq(derive(r.l, c), r.r), derive(r.r, c))
        return Seq(derive(r.l, c), r.r)

    def rep(r, c):
        return Seq(derive(r.r, c), r)

    def emp(r, c):
        return Emp()

    nextfn = {
        "Emp": emp,
        "Eps": emp,
        "Sym": sym,
        "Alt": alt,
        "Seq": seq,
        "Rep": rep,
    }.get(cn(r))

    return nextfn(r, c)

def nullable(r):
    def zer(r): return False
    def one(r): return True
    def alt(r): return nullable(r.l) or nullable(r.r)
    def seq(r): return nullable(r.l) and nullable(r.r)

    nextfn = {
        "Emp": zer,
        "Sym": zer,
        "Rep": one,
        "Eps": one,
        "Alt": alt,
        "Seq": seq
    }.get(cn(r))

    return nextfn(r)

def parsenull(r):
    """ Extract the generated parse forest from the residual regular expression."""

    def emp(r): return set()

    def eps(r): return r.tok

    def sym(r): return set([""])

    def alt(r): return parsenull(r.l).union(parsenull(r.r))

    def seq(r): return set([i + j
                            for j in parsenull(r.r)
                            for i in parsenull(r.l)])

    nextfn = {
        "Emp": emp,
        "Sym": emp,
        "Rep": sym,
        "Eps": eps,
        "Alt": alt,
        "Seq": seq
    }.get(cn(r))

    return nextfn(r)


def parse(r, s):
    """Iterate through the string, generating a new regular expression for each character, until done."""
    head = r
    for i in s:
        print head, "\n"
        head = derive(head, i)
    print head
    return parsenull(head)


if __name__ == '__main__':
#    nocs = Rep(Alt(Sym('a'), (Sym('b'))))
#    onec = Seq(nocs, Sym('c'))
#    evencs = Seq(Rep(Seq(onec, onec)), nocs)
#
#    aas = Alt(Sym('a'), Rep(Sym('a')))
#    bbs = Alt(Sym('b'), Rep(Sym('b')))
#

    sym = Seq(Sym('a'), Seq(Rep(Sym('b')), Sym('c')))
    parse(sym, "ac")
