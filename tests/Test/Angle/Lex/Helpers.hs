{-# LANGUAGE FlexibleInstances #-}
module Angle.Lex.Helpers.Tests 
    ( tests
    ) where

    
import Angle.Lex.Helpers

import Test.Framework.Providers.QuickCheck2 (testProperty)
import Test.Framework.Providers.HUnit
import Test.Framework
import Test.HUnit
import Test.QuickCheck
import Test.QuickCheck.Function
import Data.Char (isDigit, isAlpha, isAlphaNum, ord, chr)
    
import Control.Monad
    
newtype CharScan = CharScan {charScanChar :: Char}
    
charScan = char
             
instance Arbitrary CharScan where
    arbitrary = arbitrary >>= return . CharScan
             
instance Show CharScan where
    show (CharScan c) = "charScan " ++ show c
                        
instance CoArbitrary (Scanner CharScan) where
    
    
doesScan :: String -> Scanner a -> Bool
doesScan s sc = case evalScan s sc of
                   Right _ -> True
                   Left _ -> False
                   
doesNotScan :: String -> Scanner a -> Bool
doesNotScan s = not . doesScan s

testChar :: Char -> Bool
testChar x = evalScan [x] (char x) == Right x

testCharFrom :: Char -> String -> Property
testCharFrom c s = c `elem` s ==> evalScan [c] (charFrom s) == Right c

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

instance Show (Scanner Char) where
    
instance CoArbitrary (Scanner Char) where

testSomeTillChar :: Char -> String -> Property
testSomeTillChar c s = c `notElem` s ==> evalScan (s++[c]) (someTill (char c) anyChar) == Right s
                       
testAnyCharEmpty :: Bool
testAnyCharEmpty = case evalScan "" anyChar of
                     Right _ -> False
                     Left _ -> True

testAnyChar :: Char -> Bool
testAnyChar c = evalScan [c] anyChar == Right c
                
testNotScan :: String -> Scanner a -> Bool
testNotScan s sc = if doesScan s sc then doesNotScan s (notScan sc)
                   else doesScan s (notScan sc)
                        
testCond :: Fun Char Bool -> Char -> Property
testCond f c = apply f c ==> evalScan [c] (cond (apply f)) == Right c
                

            
tests = [ testGroup "basics" [
                         testProperty "someTill (char)" testSomeTillChar
                        , testProperty "cond" testCond
                        ] 
        , testGroup "characters and strings" [
                         testProperty "char" testChar
                        , testProperty "string" testString
                        , testProperty "anyChar" testAnyChar
                        , testProperty "anyCharEmpty" testAnyCharEmpty
                        ] 
        ]


-- instance (CoArbitrary a) => CoArbitrary (Scanner a) where
    
