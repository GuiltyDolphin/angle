module Test.Angle.Exec.Operations
    ( tests
    ) where

import Test.QuickCheck

import Angle.Exec.Operations
import Angle.Types.Lang
import TestHelper


testAndLitBool :: [Bool] -> Property
testAndLitBool xs = monadicIO $ do
                      res <- run $ runExec $ andLit (map LitBool xs)
                      assertEqual (LitBool $ and xs) res


tests :: [TestTree]
tests = [ testGroup "add"
          [
          ]
        , testGroup "and"
          [ testProperty "andLit (with bools)" testAndLitBool
          ]
        ]



