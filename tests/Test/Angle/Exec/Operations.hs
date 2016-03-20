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

testPassAddAsFunction :: Int -> Int -> Property
testPassAddAsFunction x y
  = monadicIO $ do
    res <- runExBuiltin $ concat
      ["defun foo(f) f(", show x, ", ", show y,  "); foo(+);"]
    assertEqual (LitInt (x + y)) res


tests :: [TestTree]
tests = [ testGroup "add"
          [ testProperty "passed to a higher-order function" $ once testPassAddAsFunction
          ]
        , testGroup "and"
          [ testProperty "andLit (with bools)" testAndLitBool
          ]
        ]



