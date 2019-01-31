module Glushkov (Glu (..), accept) where

data Glu = Eps
         | Sym Bool Char
         | Alt Glu Glu
         | Seq Glu Glu
         | Rep Glu

shift :: Bool -> Glu -> Char -> Glu
shift _ Eps _       = Eps
shift m (Sym _ x) c = Sym (m && x == c) x
shift m (Alt p q) c = Alt (shift m p c) (shift m q c)
shift m (Seq p q) c = Seq (shift m p c) (shift (m && empty p || final p) q c)
shift m (Rep r)   c = Rep (shift (m || final r) r c)

empty :: Glu -> Bool
empty Eps       = True
empty (Sym _ _) = False
empty (Alt p q) = empty p || empty q
empty (Seq p q) = empty p && empty q
empty (Rep _)   = True                      

final :: Glu -> Bool
final Eps       = False
final (Sym b _) = b
final (Alt p q) = final p || final q
final (Seq p q) = final p && empty q || final q
final (Rep r)   = final r                  

accept :: Glu -> String -> Bool
accept r []      = empty r
accept r (c:cs)  = final (foldl (shift False) (shift True r c) cs)
