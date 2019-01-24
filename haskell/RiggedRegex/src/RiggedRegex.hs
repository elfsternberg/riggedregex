module RiggedRegex ( accept, acceptw, Reg (..), Regw (..), rigged ) where

data Reg = 
    Eps         -- Epsilon
  | Sym Char    -- Character
  | Alt Reg Reg -- Alternation
  | Seq Reg Reg -- Sequence
  | Rep Reg     -- R*

accept :: Reg -> String -> Bool
-- Epsilon
accept Eps u       = null u
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
-- for the string offered.
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
               
class Semiring s where
    zero, one :: s
    mul, add  :: s -> s -> s

-- Symw (c -> s) represents a mapping from a symbol to its given weight.

sym :: Semiring s => Char -> Regw Char s
sym c = Symw (\b -> if b == c then one else zero)      
                
data Regw c s =                
    Epsw                       -- Epsilon
  | Symw (c -> s)              -- Character
  | Altw (Regw c s) (Regw c s) -- Alternation
  | Seqw (Regw c s) (Regw c s) -- Sequence
  | Repw (Regw c s)            -- R*

rigged :: Semiring s => Reg -> Regw Char s
rigged Eps = Epsw
rigged (Sym c) = sym c
rigged (Alt p q) = Altw (rigged p) (rigged q)
rigged (Seq p q) = Seqw (rigged p) (rigged q)
rigged (Rep r)   = Repw (rigged r)

acceptw :: Semiring s => Regw c s -> [c] -> s
acceptw Epsw u     = if null u then one else zero
acceptw (Symw f) u =
    case u of
      [c] -> f c
      _ -> zero
acceptw (Altw p q) u = acceptw p u `add` acceptw q u
acceptw (Seqw p q) u = sumr [ acceptw p u1 `mul` acceptw q u2 | (u1, u2) <- split u ]
acceptw (Repw r)   u = sumr [ prodr [ acceptw r ui | ui <- ps ] | ps <- parts u ]

sumr, prodr :: Semiring r => [r] -> r
sumr = foldr add zero
prodr = foldr mul one

instance Semiring Bool where
    zero = False
    one = True
    add = (||)
    mul = (&&)

instance Semiring Int where
    zero = 0
    one = 1
    add = (+)
    mul = (*)
