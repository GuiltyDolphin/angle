{-# LANGUAGE FlexibleInstances #-}
module Test.Angle.Lex.Helpers
    ( tests
    ) where

import Test.QuickCheck.Function
    
import Angle.Lex.Helpers
import TestHelper
    

newtype CharScan = CharScan {charScanChar :: Char}
    

charScan :: Char -> Scanner Char
charScan = char
             

instance Arbitrary CharScan where
    arbitrary = arbitrary >>= return . CharScan
             

instance Show CharScan where
    show (CharScan c) = "charScan " ++ show c
    
    
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


testSomeTillChar :: Char -> String -> Property
testSomeTillChar c s = c `notElem` s && not (null s) ==> evalScan (s++[c]) (someTill (char c) anyChar) == Right s
                       

testAnyCharEmpty :: Bool
testAnyCharEmpty = case evalScan "" anyChar of
                     Right _ -> False
                     Left _ -> True


testAnyChar :: Char -> Bool
testAnyChar c = evalScan [c] anyChar == Right c
                

testNotScan :: (Show a) => String -> Scanner a -> Bool
testNotScan s sc = if doesScan s sc then doesNotScan s (notScan sc)
                   else doesScan s (notScan sc)
                        

testCond :: Fun Char Bool -> Char -> Property
testCond f c = apply f c ==> evalScan [c] (cond (apply f)) == Right c
                
            
tests :: [TestTree]
tests = [ testGroup "basics" 
          [ testProperty "someTill (char)" testSomeTillChar
          , testProperty "cond" testCond
          ] 
        , testGroup "characters and strings" 
          [ testProperty "char" testChar
          , testProperty "string" testString
          , testProperty "anyChar" testAnyChar
          , testProperty "anyCharEmpty" testAnyCharEmpty
          ] 
        ]
    
