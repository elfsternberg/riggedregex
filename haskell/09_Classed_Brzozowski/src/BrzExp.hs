module BrzExp ( accept, nullable, Brz (..) ) where
data Brz = Emp | Eps | Sym (Char -> Bool) | Alt Brz Brz | Seq Brz Brz | Rep Brz

derive :: Brz -> Char -> Brz
derive Emp _       = Emp
derive Eps _       = Emp
derive (Sym c) u   = if (c u) then Eps else Emp
derive (Seq l r) u
    | nullable l = Alt (Seq (derive l u) r) (derive r u)
    | otherwise  = Seq (derive l u) r

derive (Alt Emp r) u = derive r u                    
derive (Alt l Emp) u = derive l u                    
derive (Alt l r) u   = Alt (derive r u) (derive l u)

derive (Rep r) u = Seq (derive r u) (Rep r)

nullable :: Brz -> Bool
nullable Emp       = False
nullable Eps       = True
nullable (Sym _)   = False
nullable (Alt l r) = nullable l || nullable r
nullable (Seq l r) = nullable l && nullable r
nullable (Rep _)   = True                     

accept :: Brz -> String -> Bool
accept r [] = nullable r
accept r (s:ss) = accept (derive r s) ss

       
