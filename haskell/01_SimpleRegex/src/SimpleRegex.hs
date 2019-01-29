{-# LANGUAGE LambdaCase #-}

module SimpleRegex ( accept, Reg (..) ) where

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

