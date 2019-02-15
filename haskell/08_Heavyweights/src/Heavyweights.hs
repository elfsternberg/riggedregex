{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase        #-}

module Heavyweights ( Reg(..), Glue, accept, rigged, riggeds, riggew, submatch, symi, altw, seqw, repw, LeftLong(..) ) where

import           Data.Set hiding (foldl, split)

data Reg
  = Eps
  | Sym Bool Char
  | Alt Reg Reg
  | Seq Reg Reg
  | Rep Reg

-- Just as with the Kleene versions, we're going to exploit the fact
-- that we have a working version.  For Rust, we're going to do
-- something a little different.  But for now...
--
-- This is interesting.  The paper decides that, to keep the cost of
-- processing down, we're going to cache the results of emptyg and
-- final.  One of the prices paid, though, is in the complexity of the
-- data type for our expressions, and that complexity is now managed
-- through factories.

class Semiring s where
  zero, one :: s
  mul, add :: s -> s -> s

data Glue c s = Glue
  { emptyg :: s
  , final :: s
  , glu   :: Glu c s
  }

-- 'Glu' is just the representative of the regex element
-- 'Glue' is the extended representation with cached values
              
data Glu c s
  = Epsw
  | Symw (c -> s)
  | Altw (Glue c s) (Glue c s)
  | Seqw (Glue c s) (Glue c s)
  | Repw (Glue c s)

epsw :: Semiring s => Glue c s
epsw = Glue {emptyg = one, final = zero, glu = Epsw}

symw :: Semiring s => (c -> s) -> Glue c s
symw f = Glue {emptyg = zero, final = zero, glu = Symw f}

altw :: Semiring s => Glue c s -> Glue c s -> Glue c s
altw l r =
  Glue
    { emptyg = add (emptyg l) (emptyg r),
      final = add (final l) (final r),
      glu = Altw l r
    }

seqw :: Semiring s => Glue c s -> Glue c s -> Glue c s
seqw l r =
  Glue
    { emptyg = mul (emptyg l) (emptyg r),
      final = add (mul (final l) (emptyg r)) (final r),
      glu = Seqw l r
    }

repw :: Semiring s => Glue c s -> Glue c s
repw r = Glue {emptyg = one, final = final r, glu = Repw r}

-- for my edification, the syntax under Symw is syntax for "replace
-- this value in the created record."
--     > data Foo = Foo { a :: Int, b :: Int } deriving (Show)
--     > (Foo 1 2) { b = 4 }
--     Foo { a = 1, b = 4 }
-- It doesn't seem to be functional, i.e. Foo 1 2 $ { b = 4 } doesn't work.

shift :: Semiring s => s -> Glu c s -> c -> Glue c s
shift _ Epsw _ = epsw
shift m (Symw f) c = (symw f) {final = m `mul` f c}
shift m (Seqw l r) c =
  seqw
    (shift m (glu l) c)
    (shift (add (m `mul` (emptyg l)) (final l)) (glu r) c)
shift m (Altw l r) c = altw (shift m (glu l) c) (shift m (glu r) c)
shift m (Repw r) c = repw (shift (m `add` final r) (glu r) c)

sym :: (Semiring s, Eq c) => c -> Glue c s
sym c = symw (\b -> if b == c then one else zero)

rigging :: Semiring s => (Char -> Glue t s) -> Reg -> Glue t s
rigging s =
  \case
    Eps -> epsw
    (Sym _ c) -> s c
    (Alt p q) -> altw (rigging s p) (rigging s q)
    (Seq p q) -> seqw (rigging s p) (rigging s q)
    (Rep r) -> repw (rigging s r)

rigged :: Semiring s => Reg -> Glue Char s
rigged = rigging sym

syms :: Char -> Glue Char (Set String)
syms c = symw (\b -> if b == c then singleton [c] else zero)

riggeds :: Reg -> Glue Char (Set String)
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

accept :: Semiring s => Glue c s -> [c] -> s
accept r [] = emptyg r
accept r (c:cs) =
  final (foldl (shift zero . glu) (shift one (glu r) c) cs)

submatch :: Semiring s => Glue (Int, c) s -> [c] -> s
submatch r s =
    accept (seqw arb (seqw r arb)) (zip [0..] s)
        where arb = repw (symw (\_ -> one))
          
class Semiring s => Semiringi s where
    index :: Int -> s

symi :: Semiringi s => Char -> Glue (Int, Char) s
symi c = symw weight
    where weight (pos, x) | x == c    = index pos
                          | otherwise = zero

riggew :: Semiringi s => Reg -> Glue (Int, Char) s
riggew = rigging symi         

data Leftmost = NoLeft | Leftmost Start deriving (Show)
data Start = NoStart | Start Int deriving (Show)

instance Semiring Leftmost where
    zero = NoLeft
    one  = Leftmost NoStart
    add NoLeft x = x
    add x NoLeft = x
    add (Leftmost x) (Leftmost y) = Leftmost (leftmost x y)
        where leftmost NoStart NoStart     = NoStart
              leftmost NoStart (Start i)   = Start i
              leftmost (Start i) NoStart   = Start i
              leftmost (Start i) (Start j) = Start (min i j)
    mul NoLeft _ = NoLeft
    mul _ NoLeft = NoLeft
    mul (Leftmost x) (Leftmost y) = Leftmost (start x y)
        where start NoStart s = s
              start s _       = s

instance Semiringi Leftmost where
    index = Leftmost . Start

-- Leftlong Implementation!
            
data LeftLong = NoLeftLong | NoRange | Range Int Int deriving (Show, Eq)

instance Semiring LeftLong where
    zero = NoLeftLong
    one  = NoRange

-- The addition of two leftlongs is the selection
-- of the longer of the two, provided there are
-- two.

    add NoLeftLong x    = x
    add x NoLeftLong    = x
    add NoRange x       = x
    add x NoRange       = x
    add (Range i j) (Range k l)
        | i < k || i == k && j > l = Range i j
        | otherwise             = Range k l

-- The multiplication of two leftlongs is the the longest possible
-- range among the leftlongs provided; the zero is still annhilation,
-- the one is still identity, and `mul` here is the start of the left
-- component and the end of the right component.

    mul NoLeftLong _ = NoLeftLong
    mul _ NoLeftLong = NoLeftLong
    mul NoRange x    = x
    mul x NoRange    = x
    mul (Range i _) (Range _ l) = Range i l

instance Semiringi LeftLong where
    index i = Range i i                                        
                                              
