{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE FlexibleInstances #-}

module RiggedRegex ( accept, acceptw, Reg (..), Regw (..), rigged, riggeds ) where

import Data.Set hiding (split)
    
data Reg = 
    Eps         -- Epsilon
  | Sym Char    -- Character
  | Alt Reg Reg -- Alternation
  | Seq Reg Reg -- Sequence
  | Rep Reg     -- R*

accept :: Reg -> String -> Bool
-- Epsilon
accept Eps u       = Prelude.null u
-- Accept if the character offered matches the character constructed
accept (Sym c) u   = u == [c]
-- Constructed of two other expressions, accept if either one does.
accept (Alt p q) u = accept p u || accept q u
-- Constructed of two other expressions, accept if p accepts some part
-- of u and q accepts the rest, where u is split arbitrarily
accept (Seq p q) u = or [accept p u1 && accept q u2 | (u1, u2) <- split u]
-- For all convolutions of u containing no empty strings,
-- if all the entries of that convolution are accepted,
-- then at least one convolution is acceptable.
accept (Rep r) u   = or [and [accept r ui | ui <- ps] | ps <- parts u]

-- Generate a list of all possible combinations of a prefix and suffix
-- for the string offered.w
split :: [a] -> [([a], [a])]
split []     = [([], [])]
split (c:cs) = ([], c : cs) : [(c : s1, s2) | (s1, s2) <- split cs]

-- Generate lists of lists that contain all possible convolutions of
-- the input string, not including the empty string.
parts :: [a] -> [[[a]]]
parts []     = [[]]
parts [c]    = [[[c]]]
parts (c:cs) = concat [[(c : p) : ps, [c] : p : ps] | p:ps <- parts cs]

-- A semiring is an algebraic structure with a zero, a one, a
-- "multiplication" operation, and an "addition" operation.  Zero is
-- the identity operator for addition, One is the identity operator for
-- multiplication, both composition operators are associative (it does
-- not matter how sequential operations are grouped), and addition is
-- commutative (the order of the operations does not matter).  Also,
-- zero `mul` anything is always zero.
--
-- Which, in regular expressions in general, holds that the null regex
-- is zero, and the empty string regex is one, alternation is addition
-- and ... sequence is multiplication?  Like "sum" and "product" types?
               
-- Symw (c -> s) represents a mapping from a symbol to its given weight.

class Semiring s where
    zero, one :: s
    mul, add  :: s -> s -> s

sym :: Semiring s => Char -> Regw Char s
sym c = Symw (\b -> if b == c then one else zero)      
                
data Regw c s =                
    Epsw                       -- Epsilon
  | Symw (c -> s)              -- Character
  | Altw (Regw c s) (Regw c s) -- Alternation
  | Seqw (Regw c s) (Regw c s) -- Sequence
  | Repw (Regw c s)            -- R*

rigging :: Semiring s => (Char -> Regw Char s) -> Reg -> Regw Char s
rigging s = \case
         Eps       -> Epsw
         (Sym c)   -> s c
         (Alt p q) -> Altw (rigging s p) (rigging s q)
         (Seq p q) -> Seqw (rigging s p) (rigging s q)
         (Rep r)   -> Repw (rigging s r)

rigged :: Semiring s => Reg -> Regw Char s
rigged = rigging sym

acceptw :: Semiring s => Regw c s -> [c] -> s
acceptw Epsw u     = if Prelude.null u then one else zero
acceptw (Symw f) u =
    case u of
      [c] -> f c
      _ -> zero
acceptw (Altw p q) u = acceptw p u `add` acceptw q u
acceptw (Seqw p q) u = sumr [ acceptw p u1 `mul` acceptw q u2 | (u1, u2) <- split u ]
acceptw (Repw r)   u = sumr [ prodr [ acceptw r ui | ui <- ps ] | ps <- parts u ]

-- Something feels hacky about this.  I mean, I know, on the one
-- hand than any epsilon is still "one" as far as the system is
-- concerned; on the other hand, I would much rather have a better
-- theoretical ground for what I just did here...
                       
syms :: Char -> Regw Char (Set String)
syms c = Symw (\b -> if b == c then singleton [c] else zero)
         
riggeds :: Reg -> Regw Char (Set String)
riggeds = rigging syms

sumr, prodr :: Semiring r => [r] -> r
sumr = Prelude.foldr add zero
prodr = Prelude.foldr mul one

instance Semiring Int where
    zero = 0
    one = 1
    add = (Prelude.+)
    mul = (Prelude.*)

instance Semiring Bool where
    zero = False
    one = True
    add = (||)
    mul = (&&)

-- εs = {(ε, s)} Empty Word
-- c =  {(c, c)} Token
-- L1 ◦ L2 = {(uv,(s, t)) | (u, s) ∈ L1 and (v, t) ∈ L2} Concatenation
-- L1 ∪ L2 = {(u, s) | (u, s)} ∈ L1 Alternation

-- Boolean Semiring (TRUE, FALSE,∨,∧, FALSE, TRUE) recognition
-- Inside Semiring (R(1/0), +, ×, 0, 1) string probability
-- Counting Semiring (N(∞/0), +, ×, 0, 1) number of derivations
-- Derivation Forests Semiring (2E,∪, ·, ∅, {<>}) set of derivation

instance Semiring (Set String) where
    zero    = empty 
    one     = singleton ""
    add     = union
    mul a b = Data.Set.map (uncurry (++)) $ cartesianProduct a b
