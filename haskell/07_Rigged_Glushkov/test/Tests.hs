{-# OPTIONS_GHC -fno-warn-type-defaults #-}
{-# LANGUAGE RecordWildCards #-}

import           Data.Foldable     (for_)
import           Test.Hspec        (Spec, describe, it, shouldBe)
import           Test.Hspec.Runner (configFastFail, defaultConfig, hspecWith)
import           RiggedGlushkov    (Glu (..), acceptg, rigged, riggeds)
import Data.Set
import Data.List (sort)

main :: IO ()
main = hspecWith defaultConfig {configFastFail = True} specs

msym :: Char -> Glu       
msym c = Sym False c       
       
specs :: Spec
specs = do

     let nocs = Rep ( Alt ( msym 'a' ) ( msym 'b' ) )
     let onec = Seq nocs (msym 'c')
     let evencs = Seq ( Rep ( Seq onec onec ) ) nocs

     let as = Alt (msym 'a') (Rep (msym 'a'))                  
     let bs = Alt (msym 'b') (Rep (msym 'b'))

--     it "lifted expression" $
--        (acceptg (rigged evencs) "acc" :: Bool) `shouldBe` True

     it "lifted expression short" $
        (acceptg (rigged evencs) "acc" :: Int) `shouldBe` 1

     it "lifted expression counter two" $
        (acceptg (rigged as) "a" :: Int) `shouldBe` 2

     it "lifted expression counter one" $
        (acceptg (rigged as) "aa" :: Int) `shouldBe` 1

     it "lifted expression dynamic counter four" $
        (acceptg (rigged (Seq as bs)) "ab" :: Int) `shouldBe` 4

     it "parse forests" $
            (sort $ toList $ (acceptg (riggeds (Seq as bs)) "ab" :: Set String)) `shouldBe` ["ab"]
                                                  
     for_ cases test
        where
          test Case {..} = it description assertion
              where
                assertion = (acceptg (rigged regex) sample :: Bool) `shouldBe` result

data Case = Case
  { description :: String
  , regex       :: Glu
  , sample      :: String
  , result      :: Bool
  }

cases :: [Case]
cases =
  [ Case {description = "empty", regex = Eps, sample = "", result = True}
  , Case {description = "char", regex = msym 'a', sample = "a", result = True}
  , Case
      {description = "not char", regex = msym 'a', sample = "b", result = False}
  , Case
      { description = "char vs empty"
      , regex = msym 'a'
      , sample = ""
      , result = False
      }
  , Case
      { description = "left alt"
      , regex = Alt (msym 'a') (msym 'b')
      , sample = "a"
      , result = True
      }
  , Case
      { description = "right alt"
      , regex = Alt (msym 'a') (msym 'b')
      , sample = "b"
      , result = True
      }
  , Case
      { description = "neither alt"
      , regex = Alt (msym 'a') (msym 'b')
      , sample = "c"
      , result = False
      }
  , Case
      { description = "empty alt"
      , regex = Alt (msym 'a') (msym 'b')
      , sample = ""
      , result = False
      }
  , Case
      { description = "empty rep"
      , regex = Rep (msym 'a')
      , sample = ""
      , result = True
      }
  , Case
      { description = "one rep"
      , regex = Rep (msym 'a')
      , sample = "a"
      , result = True
      }
  , Case
      { description = "multiple rep"
      , regex = Rep (msym 'a')
      , sample = "aaaaaaaaa"
      , result = True
      }
  , Case
      { description = "multiple rep with failure"
      , regex = Rep (msym 'a')
      , sample = "aaaaaaaaab"
      , result = False
      }
  , Case
      { description = "sequence"
      , regex = Seq (msym 'a') (msym 'b')
      , sample = "ab"
      , result = True
      }
  , Case
      { description = "sequence with empty"
      , regex = Seq (msym 'a') (msym 'b')
      , sample = ""
      , result = False
      }
  , Case
      { description = "bad short sequence"
      , regex = Seq (msym 'a') (msym 'b')
      , sample = "a"
      , result = False
      }
  , Case
      { description = "bad long sequence"
      , regex = Seq (msym 'a') (msym 'b')
      , sample = "abc"
      , result = False
      }
  ]
          
  
