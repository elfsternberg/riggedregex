{-# OPTIONS_GHC -fno-warn-type-defaults #-}
{-# LANGUAGE RecordWildCards #-}
import           Data.Foldable     (for_)
import Test.Hspec        (Spec, it, shouldBe)
import Test.Hspec.Runner (configFastFail, defaultConfig, hspecWith)
import RiggedRegex (Reg (..), accept, acceptw, rigged)

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

     for_ cases test
        where
          test Case {..} = it description assertion
              where
                assertion = (acceptw (rigged regex) sample :: Bool) `shouldBe` result

data Case = Case
  { description :: String
  , regex       :: Reg
  , sample      :: String
  , result      :: Bool
  }

cases :: [Case]
cases =
  [ Case {description = "empty", regex = Eps, sample = "", result = True}
  , Case {description = "char", regex = Sym 'a', sample = "a", result = True}
  , Case
      {description = "not char", regex = Sym 'a', sample = "b", result = False}
  , Case
      { description = "char vs empty"
      , regex = Sym 'a'
      , sample = ""
      , result = False
      }
  , Case
      { description = "left alt"
      , regex = Alt (Sym 'a') (Sym 'b')
      , sample = "a"
      , result = True
      }
  , Case
      { description = "right alt"
      , regex = Alt (Sym 'a') (Sym 'b')
      , sample = "b"
      , result = True
      }
  , Case
      { description = "neither alt"
      , regex = Alt (Sym 'a') (Sym 'b')
      , sample = "c"
      , result = False
      }
  , Case
      { description = "empty alt"
      , regex = Alt (Sym 'a') (Sym 'b')
      , sample = ""
      , result = False
      }
  , Case
      { description = "empty rep"
      , regex = Rep (Sym 'a')
      , sample = ""
      , result = True
      }
  , Case
      { description = "one rep"
      , regex = Rep (Sym 'a')
      , sample = "a"
      , result = True
      }
  , Case
      { description = "multiple rep"
      , regex = Rep (Sym 'a')
      , sample = "aaaaaaaaa"
      , result = True
      }
  , Case
      { description = "multiple rep with failure"
      , regex = Rep (Sym 'a')
      , sample = "aaaaaaaaab"
      , result = False
      }
  , Case
      { description = "sequence"
      , regex = Seq (Sym 'a') (Sym 'b')
      , sample = "ab"
      , result = True
      }
  , Case
      { description = "sequence with empty"
      , regex = Seq (Sym 'a') (Sym 'b')
      , sample = ""
      , result = False
      }
  , Case
      { description = "bad short sequence"
      , regex = Seq (Sym 'a') (Sym 'b')
      , sample = "a"
      , result = False
      }
  , Case
      { description = "bad long sequence"
      , regex = Seq (Sym 'a') (Sym 'b')
      , sample = "abc"
      , result = False
      }
  ]
          
  
