{-# LANGUAGE FlexibleInstances #-}
module Test.Angle.Lex.Helpers
    ( tests
    ) where

import Test.QuickCheck.Function

import Angle.Lex.Helpers
import TestHelper


newtype CharScan = CharScan Char


instance Arbitrary CharScan where
    arbitrary = arbitrary >>= return . CharScan


instance Show CharScan where
    show (CharScan c) = "charScan " ++ show c


testChar :: Char -> Bool
testChar x = evalScan [x] (char x) == Right x


testString :: String -> Bool
testString s = evalScan s (string s) == Right s


instance Arbitrary (Scanner Char) where
    arbitrary = do
      c <- arbitrary
      oneof [return $ char c, return $ notChar c]


instance Arbitrary (Scanner String) where
    arbitrary = do
      s <- arbitrary
      oneof [return $ string s]


testAnyCharEmpty :: Bool
testAnyCharEmpty = case evalScan "" anyChar of
                     Right _ -> False
                     Left _ -> True


testAnyChar :: Char -> Bool
testAnyChar c = evalScan [c] anyChar == Right c


testCond :: Fun Char Bool -> Char -> Property
testCond f c = apply f c ==> evalScan [c] (cond (apply f)) == Right c


tests :: [TestTree]
tests = [ testGroup "basics"
          [ testProperty "cond" testCond
          ]
        , testGroup "characters and strings"
          [ testProperty "char" testChar
          , testProperty "string" testString
          , testProperty "anyChar" testAnyChar
          , testProperty "anyCharEmpty" testAnyCharEmpty
          ]
        ]

