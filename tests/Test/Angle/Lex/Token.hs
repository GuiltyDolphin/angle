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


testTokChar :: Char -> Bool
testTokChar c = evalScan (show c) tokChar == Right c


testTokCharAscii :: AsciiChar -> Bool
testTokCharAscii (AsciiChar c) = evalScan (show c) tokChar == Right c




newtype AsciiChar = AsciiChar {getAsciiChar :: Char}
  deriving (Show, Eq, Ord)


instance Arbitrary AsciiChar where
  arbitrary = liftM AsciiChar (elements [toEnum 0..toEnum 128])


tests :: [TestTree]
tests = [ testGroup "numbers"
          [ testProperty "integer" testTokInt
          , testProperty "float" testTokFloat
          ]
        , testGroup "strings and characters"
          [ testProperty "tokChar" testTokChar
          , testProperty "tokCharAscii" testTokCharAscii
          ]
        ]










