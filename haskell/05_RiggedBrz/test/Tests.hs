{-# OPTIONS_GHC -fno-warn-type-defaults #-}
{-# LANGUAGE RecordWildCards #-}

import           Data.Foldable     (for_)
import           Test.Hspec        (Spec, describe, it, shouldBe)
import           Test.Hspec.Runner (configFastFail, defaultConfig, hspecWith)

import           RiggedBrz         (Brz (..), accept)

main :: IO ()
main = hspecWith defaultConfig {configFastFail = True} specs

specs :: Spec
specs = describe "accept" $ for_ cases test
  where
    test Case {..} = it description assertion
      where
        assertion = accept regex sample `shouldBe` result

data Case = Case
  { description :: String
  , regex       :: Brz
  , sample      :: String
  , result      :: Bool
  }

-- let nocs = Rep ( Alt ( Sym 'a' ) ( Sym 'b' ) )
--     onec = Seq nocs (Sym 'c')
--     evencs = Seq ( Rep ( Seq onec onec ) ) nocs
--     as = Alt (Sym 'a') (Rep (Sym 'a'))
--     bs = Alt (Sym 'b') (Rep (Sym 'b'))
cases :: [Case]
cases =
  [ Case {description = "empty", regex = Eps, sample = "", result = True}
  , Case {description = "null", regex = Emp, sample = "", result = False}
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
