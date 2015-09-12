{-# LANGUAGE FlexibleInstances #-}
module Test.Angle.Parse.Helpers
    ( tests
    ) where

import Test.QuickCheck.Function

import Angle.Parse.Helpers
import TestHelper


newtype CharScan = CharScan Char


instance Arbitrary CharScan where
    arbitrary = arbitrary >>= return . CharScan


instance Show CharScan where
    show (CharScan c) = "charScan " ++ show c


testChar :: Char -> Bool
testChar x = evalParse [x] (char x) == Right x


testString :: String -> Bool
testString s = evalParse s (string s) == Right s


instance Arbitrary (Parser Char) where
    arbitrary = do
      c <- arbitrary
      oneof [return $ char c, return $ notChar c]


instance Arbitrary (Parser String) where
    arbitrary = do
      s <- arbitrary
      oneof [return $ string s]


testAnyCharEmpty :: Bool
testAnyCharEmpty = case evalParse "" anyChar of
                     Right _ -> False
                     Left _ -> True


testAnyChar :: Char -> Bool
testAnyChar c = evalParse [c] anyChar == Right c


testCond :: Fun Char Bool -> Char -> Property
testCond f c = apply f c ==> evalParse [c] (cond (apply f)) == Right c


testCharFrom :: NonEmptyList Char -> Bool
testCharFrom (NonEmpty cs) = evalParse cs (charFrom cs) == Right (head cs)


tests :: [TestTree]
tests = [ testGroup "basics"
          [ testProperty "cond" testCond
          ]
        , testGroup "characters and strings"
          [ testProperty "char" testChar
          , testProperty "string" testString
          , testProperty "anyChar" testAnyChar
          , testProperty "anyCharEmpty" testAnyCharEmpty
          , testProperty "charFrom" testCharFrom
          ]
        ]

