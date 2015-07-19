module Angle.Lex.Lexer.Tests 
    ( tests
    ) where

import Test.Framework
import Test.Framework.Providers.HUnit
import Test.Framework.Providers.QuickCheck2
import Test.HUnit
import Test.QuickCheck
    
import Angle.Lex.Lexer
import Angle.Lex.Helpers (evalScan)
import Angle.Types.Lang
exampleAssign = "x = 1"
                

exprInt = ExprLit . LitInt
                
testEndOfStmt = evalScan exampleAssign stmt @?= evalScan (exampleAssign ++ ";") stmt

              
quoted :: String -> String
quoted xs = '"':xs ++ "\""

testLitStrEmpty :: Assertion
testLitStrEmpty = evalScan "" litStr @?= Right (LitStr "") 
testLitStr :: String -> Property
testLitStr x = '"' `notElem` x ==> evalScan (quoted x) litStr == Right (LitStr x)

testLitInt :: Int -> Bool
testLitInt x = evalScan (show x) litInt == Right (LitInt x)
               
--testLitFloat :: Small Float -> Bool
--testLitFloat x' = evalScan (show x) litFloat == Right (LitFloat x)
--                  where x = getSmall x'

testLitBool :: Bool
testLitBool = evalScan "true" litBool == Right (LitBool True)
              && evalScan "false" litBool == Right (LitBool False)

                 

testRangeNum :: Int -> Int -> Bool
testRangeNum x y = evalScan toTest litRange == Right expected
    where toTest = concat ["(", show x, "..", show y, ")"]
          expected = LitRange (exprInt x) (exprInt y)


-- FUNCTIONS
testEmptyCall :: Bool
testEmptyCall = evalScan "foo()" exprFunCall
 == Right (ExprFunCall "foo" [])


tests = [ testGroup "literals"
          [ testProperty "boolean" testLitBool
          , testProperty "range" testRangeNum
          , testProperty "integer" testLitInt
          , testProperty "string" testLitStr
--          , testProperty "float" testLitFloat
          ]
          , testGroup "functions"
                          [ testProperty "no args from empty call" testEmptyCall
                          ]
 ]









