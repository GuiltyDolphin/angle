module Test.Angle.Lex.Token
    ( tests
    ) where

import Control.Monad (liftM)
import Numeric (showFFloat)

import Test.QuickCheck
import Test.HUnit
    
import Angle.Lex.Token
import TestHelper

testTokInt :: Int -> Bool
testTokInt x = evalScan (show x) tokInt == Right x
               
testTokFloat :: Float -> Bool
testTokFloat x = evalScan (showFFloat Nothing x "") tokFloat == Right x
                 
testTokString :: NoQuoteString -> Bool
testTokString (NoQuoteString x) = evalScan ('"':x++"\"") tokString == Right x
                                  
newtype NoQuoteString = NoQuoteString 
    { getNoQuoteString :: String }
    deriving (Show)
    
instance Arbitrary NoQuoteString where
    arbitrary = liftM (NoQuoteString . filter (/='"')) arbitrary 

tests = [ testGroup "numbers"
          [ testProperty "integer" testTokInt
          , testProperty "float" testTokFloat
          ]
        , testProperty "string" testTokString
        ]










