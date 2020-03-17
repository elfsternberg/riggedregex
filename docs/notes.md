Owens and Reppy did a much better job than I originally thought. They
use the tilde to mean "is recognized by," as in "r ~ u" means "`r`
*recognizes* the string `u`".

Following on the nullability issue, r ~ ε ⇔ ν(r) = ε, r ~ aw ⇔ δ(a)r ~ w
(`r` recognizes `aw` if the derivative of r with respect to `a`
recognizes only `w`).

r ≡ s (r is equivalent to s) if 𝓛⟦r⟧ = 𝓛⟦s⟧.  Note that this is
"equivalance" under set theory, where given a binary equivalence
operation.  That is, if the elements of some set S have an equivalence
notion, then the set S can be split into *equivalence classes*. 

1. At each step we have a residual regular expression `r` for the
   residual string `s`
   
2. Instead of computing the derivative on the fly, we precompute the
   derivative of `r` for each symbol in our alphabet `Σ`, thereby
   constructing a DFA for the language in `r`.
   
3. Computing equivalence can be expensive

4. It is not practical to iterate over every Unicode codepoint for
   each state.
   
5. A scanner-generator takes a collection of REs, not just one.

Owens & Reppy introduce a notion of *weak equivalence*, which is a set
of rules for harmonizing some regular expression equivalents.  These
look a lot like some of the performance optimizations found in Might &
Adams.

They define a *class*, **S**, where **S** ⊆ Σ.  **S** covers both the
empty set and the single character set, as well as a multi-character
*class*.

They then add equivalence expressions: **R** + **S** ≈ **T** where 
T = R ∪ S. (Note that this works for *recognition*.  But what about more
complex operations?)

We say that a and b are equivalent in r only if δ(a)r ≡ δ(b)r.

r = a + b · a + c

(Do we read this "a OR ba OR c" or "(a or b)(a or c)".  If we read it
the first way, then this makes sense: the equivalence classes for r
produce three possible derivatives: {a, c}, transition to `ε`; {b},
transition to `a`; or Σ\{a,b,c}, which is the alphabet that excludes a,
b, or c, and transitions to `⊘`.)

All right, having gotten that out of the way, we say that i ≅ᵣ j (the
derivative class of r(i) is equivalent to the derivative class of r(j))
if `δᵢr ≡ δⱼr`.

fun goto q (S, (Q, δ)) =
	let c ∈ S
	let q c = ∂ c q
	in
		if ∃q 0 ∈ Q such that q 0 ≈ q c
			then (Q, δ ∪ {(q, S) 7→ q 0 })
			else
				let Q 0 = Q ∪ {q c }
				let δ 0 = δ ∪ {(q, S) 7→ q c }
				in explore (Q 0 , δ 0 , q c )

let explore (Q, δ, q) = fold (goto q) (Q, δ) (C(q))

fun mkDFA r =
	let q 0 = ∂ ε r
	let (Q, δ) = explore ({q 0 }, {}, q 0 )
	let F = {q | q ∈ Q and ν(q) = ε}
	in hQ, q 0 , F, δi





