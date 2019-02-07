{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE FlexibleInstances #-}

module RiggedBrz ( Brz (..), parse, rigged, riggeds ) where

import Data.Set
    
data Brz = Emp | Eps | Sym Char | Alt Brz Brz | Seq Brz Brz | Rep Brz deriving (Eq)

-- Transform a Brz into a Brzr.  That's all it does.  It's not magical.
              
rigging :: Semiring s => (Char -> Brzr Char s) -> Brz -> Brzr Char s
rigging s = \case
             Emp -> Empr
             Eps -> Epsr one
             (Sym c) -> s c
             (Alt p q) -> Altr (rigging s p) (rigging s q)
             (Seq p q) -> Seqr (rigging s p) (rigging s q)
             (Rep r) -> Repr (rigging s r)

class Semiring s where
  zero, one :: s
  mul, add :: s -> s -> s

data Brzr c s = Empr
              | Epsr s
              | Delr (Brzr c s)
              | Symr (c -> s)
              | Altr (Brzr c s) (Brzr c s)
              | Seqr (Brzr c s) (Brzr c s)
              | Repr (Brzr c s)

deriver :: Semiring s => Brzr c s -> c -> Brzr c s
deriver Empr _        = Empr
deriver (Epsr _) _    = Empr
deriver (Delr _) _    = Empr
deriver (Symr f) u    = Epsr $ (f u)

deriver (Seqr l r) u  =
    Altr dl dr
        where
          dl = Seqr (deriver l u) r
          dr = Seqr (Delr l) (deriver r u)

deriver (Altr l r) u = go (deriver l u) (deriver r u)
    where go Empr r1 = r1
          go r1 Empr = r1
          go l1 r1 = Altr l1 r1
        
deriver (Repr r) u = Seqr (deriver r u) (Repr r)

parsenull :: Semiring s => (Brzr c s) -> s
parsenull Empr = zero
parsenull (Symr _)   = zero
parsenull (Repr _)   = one
parsenull (Epsr s)   = s
parsenull (Delr s)   = parsenull s
parsenull (Altr p q) = parsenull p `add` parsenull q
parsenull (Seqr p q) = parsenull p `mul` parsenull q

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

instance Semiring (Set String) where
    zero    = empty 
    one     = singleton ""
    add     = union
    mul a b = Data.Set.map (uncurry (++)) $ cartesianProduct a b

-- Rigging for boolean and integer values.              

sym :: Semiring s => Char -> Brzr Char s
sym c = Symr (\b -> if b == c then one else zero)      

rigged :: Semiring s => Brz -> Brzr Char s
rigged = rigging sym

-- Rigging for parse forests
         
syms :: Char -> Brzr Char (Set String)
syms c = Symr (\b -> if b == c then singleton [c] else zero)
         
riggeds :: Brz -> Brzr Char (Set String)
riggeds = rigging syms
              
parse :: (Semiring s) => (Brzr Char s) -> String -> s
parse w [] = parsenull w
parse w (c:cs) = parse (deriver w c) cs
       
