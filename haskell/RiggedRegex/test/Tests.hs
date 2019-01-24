import Test.Hspec        (Spec, it, shouldBe)
import Test.Hspec.Runner (configFastFail, defaultConfig, hspecWith)
import RiggedRegex (Reg (..), Regw (..), accept, acceptw, rigged)

main :: IO ()
main = hspecWith defaultConfig {configFastFail = True} specs

specs :: Spec
specs = do

     let nocs = Rep ( Alt ( Sym 'a' ) ( Sym 'b' ) )
     let onec = Seq nocs (Sym 'c')
     let evencs = Seq ( Rep ( Seq onec onec ) ) nocs

     let as = Alt (Sym 'a') (Rep (Sym 'a'))                  
     let bs = Alt (Sym 'b') (Rep (Sym 'b'))

     it "simple expression" $
        accept evencs "acc" `shouldBe` True

     it "lifted expression" $
        (acceptw (rigged evencs) "acc" :: Bool) `shouldBe` True

     it "lifted expression short" $
        (acceptw (rigged evencs) "acc" :: Int) `shouldBe` 1

     it "lifted expression counter two" $
        (acceptw (rigged as) "a" :: Int) `shouldBe` 2

     it "lifted expression counter one" $
        (acceptw (rigged as) "aa" :: Int) `shouldBe` 1

     it "lifted expression dynamic counter four" $
        (acceptw (rigged (Seq as bs)) "ab" :: Int) `shouldBe` 4

  
