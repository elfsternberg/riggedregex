{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase        #-}

module RiggedGlushkov ( Glu(..), accept, acceptg, rigged, riggeds ) where

import           Data.Set hiding (foldl, split)

data Glu
  = Eps
  | Sym Bool Char
  | Alt Glu Glu
  | Seq Glu Glu
  | Rep Glu

shift :: Bool -> Glu -> Char -> Glu
shift _ Eps _       = Eps
shift m (Sym _ x) c = Sym (m && x == c) x
shift m (Alt p q) c = Alt (shift m p c) (shift m q c)
shift m (Seq p q) c = Seq (shift m p c) (shift (m && emptys p || final p) q c)
shift m (Rep r) c   = Rep (shift (m || final r) r c)

emptys :: Glu -> Bool
emptys Eps       = True
emptys (Sym _ _) = False
emptys (Alt p q) = emptys p || emptys q
emptys (Seq p q) = emptys p && emptys q
emptys (Rep _)   = True

final :: Glu -> Bool
final Eps       = False
final (Sym b _) = b
final (Alt p q) = final p || final q
final (Seq p q) = final p && emptys q || final q
final (Rep r)   = final r

accept :: Glu -> String -> Bool
accept r []     = emptys r
accept r (c:cs) = final (foldl (shift False) (shift True r c) cs)

-- Just as with the Kleene versions, we're going to exploit the fact
-- that we have a working version.  For Rust, we're going to do
-- something a little different.  But for now...
--
-- This is interesting.  The paper decides that, to keep the cost of
-- processing down, we're going to cache the results of empty and
-- final.  One of the prices paid, though, is in the complexity of the
-- data type for our expressions, and that complexity is now managed
-- through factories.

class Semiring s where
  zero, one :: s
  mul, add :: s -> s -> s

data Glue c s = Glue
  { emptye :: s
  , finale :: s
  , gluw   :: Gluw c s
  }

data Gluw c s
  = Epsw
  | Symw (c -> s)
  | Altw (Glue c s) (Glue c s)
  | Seqw (Glue c s) (Glue c s)
  | Repw (Glue c s)

epsw :: Semiring s => Glue c s
epsw = Glue {emptye = one, finale = zero, gluw = Epsw}

symw :: Semiring s => (c -> s) -> Glue c s
symw f = Glue {emptye = zero, finale = zero, gluw = Symw f}

altw :: Semiring s => Glue c s -> Glue c s -> Glue c s
altw l r =
  Glue
    { emptye = add (emptye l) (emptye r),
      finale = add (finale l) (finale r),
      gluw = Altw l r
    }

seqw :: Semiring s => Glue c s -> Glue c s -> Glue c s
seqw l r =
  Glue
    { emptye = mul (emptye l) (emptye r),
      finale = add (mul (finale l) (emptye r)) (finale r),
      gluw = Seqw l r
    }

repw :: Semiring s => Glue c s -> Glue c s
repw r = Glue {emptye = one, finale = finale r, gluw = Repw r}

-- for my edification, the syntax under Symw is syntax for "replace
-- this value in the created record."
--     > data Foo = Foo { a :: Int, b :: Int } deriving (Show)
--     > (Foo 1 2) { b = 4 }
--     Foo { a = 1, b = 4 }
-- It doesn't seem to be functional, i.e. Foo 1 2 $ { b = 4 } doesn't work.
shifte :: Semiring s => s -> Gluw c s -> c -> Glue c s
shifte _ Epsw _ = epsw
shifte m (Symw f) c = (symw f) {finale = m `mul` f c}
shifte m (Seqw l r) c =
  seqw
    (shifte m (gluw l) c)
    (shifte (add (m `mul` (emptye l)) (finale l)) (gluw r) c)
shifte m (Altw l r) c = altw (shifte m (gluw l) c) (shifte m (gluw r) c)
shifte m (Repw r) c = repw (shifte (m `add` finale r) (gluw r) c)

sym :: (Semiring s, Eq c) => c -> Glue c s
sym c = symw (\b -> if b == c then one else zero)

rigging :: Semiring s => (Char -> Glue Char s) -> Glu -> Glue Char s
rigging s =
  \case
    Eps -> epsw
    (Sym _ c) -> s c
    (Alt p q) -> altw (rigging s p) (rigging s q)
    (Seq p q) -> seqw (rigging s p) (rigging s q)
    (Rep r) -> repw (rigging s r)

rigged :: Semiring s => Glu -> Glue Char s
rigged = rigging sym

syms :: Char -> Glue Char (Set String)
syms c = symw (\b -> if b == c then singleton [c] else zero)

riggeds :: Glu -> Glue Char (Set String)
riggeds = rigging syms

instance Semiring (Set String) where
  zero = empty
  one = singleton ""
  add = union
  mul a b = Data.Set.map (uncurry (++)) $ cartesianProduct a b

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

acceptg :: Semiring s => Glue c s -> [c] -> s
acceptg r [] = emptye r
acceptg r (c:cs) =
  finale (foldl (shifte zero . gluw) (shifte one (gluw r) c) cs)
