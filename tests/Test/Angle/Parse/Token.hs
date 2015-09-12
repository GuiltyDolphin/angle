module Test.Angle.Parse.Token
    ( tests
    ) where

import Control.Monad (liftM)
import Numeric (showFFloat)

import Angle.Parse.Token
import TestHelper


testTokInt :: Int -> Bool
testTokInt x = evalParse (show x) tokInt == Right x


testTokFloat :: Double -> Bool
testTokFloat x = evalParse (showFFloat Nothing x "") tokFloat == Right x


testTokChar :: Char -> Bool
testTokChar c = evalParse (show c) tokChar == Right c


testTokCharAscii :: AsciiChar -> Bool
testTokCharAscii (AsciiChar c) = evalParse (show c) tokChar == Right c




newtype AsciiChar = AsciiChar Char
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










