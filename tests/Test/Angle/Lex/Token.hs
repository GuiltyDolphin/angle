module Test.Angle.Lex.Token
    ( tests
    ) where

import Control.Monad (liftM)
import Numeric (showFFloat)

import Angle.Lex.Token
import TestHelper


testTokInt :: Int -> Bool
testTokInt x = evalScan (show x) tokInt == Right x


testTokFloat :: Double -> Bool
testTokFloat x = evalScan (showFFloat Nothing x "") tokFloat == Right x


tests :: [TestTree]
tests = [ testGroup "numbers"
          [ testProperty "integer" testTokInt
          , testProperty "float" testTokFloat
          ]
        ]










