module Test.Angle.Lex.Lexer
    ( tests
    ) where

import Control.Monad (liftM)

    
import Control.Applicative
import Test.Framework
import Test.Framework.Providers.HUnit
import Test.Framework.Providers.QuickCheck2
import Test.HUnit
import Test.QuickCheck
    
import Angle.Lex.Lexer
import Angle.Lex.Helpers (evalScan)
import Angle.Types.Lang
    

exampleAssign = "x = 1"
                
instance Arbitrary LangLit where
    arbitrary = oneof
                [ liftM LitInt arbitrary
--                , liftM LitFloat arbitrary -- Issue with e in float
                , liftM LitBool arbitrary
                , liftM LitStr (liftM (filter (/='"')) arbitrary)
                ]
                
checkFloatStr :: Float -> Bool
checkFloatStr x = not $ 'e' `elem` (show x) 

testEndOfStmt = evalScan exampleAssign stmt @?= evalScan (exampleAssign ++ ";") stmt

              
quoted :: String -> String
quoted xs = '"':xs ++ "\""

testLitStrEmpty :: Assertion
testLitStrEmpty = evalScan "" litStr @?= Right (LitStr "") 
testLitStr :: String -> Property
testLitStr x = '"' `notElem` x ==> evalScan (quoted x) litStr == Right (LitStr x)

testLitInt :: Int -> Bool
testLitInt x = evalScan (show x) litInt == Right (LitInt x)
               
testLitNull :: Bool
testLitNull = evalScan "()" litNull == Right LitNull
               
--testLitFloat :: Small Float -> Bool
--testLitFloat x' = evalScan (show x) litFloat == Right (LitFloat x)
--                  where x = getSmall x'

testLitBool :: Bool
testLitBool = evalScan "true" litBool == Right (LitBool True)
              && evalScan "false" litBool == Right (LitBool False)

                 

testRange :: LangLit -> LangLit -> Bool
testRange x y = evalScan toTest litRange == Right expected
    where toTest = concat ["(", show x, "..", show y, ")"]
          expected = LitRange (ExprLit x) (ExprLit y)
                     
testLangLitInt :: Int -> Bool
testLangLitInt x = evalScan (show x) langLit  
                   ==  Right (LitInt x)
                   
testLangLitBool :: Bool
testLangLitBool = evalScan "true" langLit == Right (LitBool True) && evalScan "false" langLit == Right (LitBool False)

-- FUNCTIONS
testEmptyCall :: Bool
testEmptyCall = evalScan "foo()" exprFunCall
 == Right (ExprFunCall "foo" [])
    
testOpAdd :: Bool
testOpAdd = evalScan "(+ 1 2)" langOp
 == Right (MultiOp OpAdd [ExprLit (LitInt 1), ExprLit (LitInt 2)])
    
testOpNeg :: Bool
testOpNeg = evalScan "-x" langOp ==  Right (SpecOp OpNeg (ExprIdent "x"))

newtype ValidIdent = ValidIdent { getValidIdent :: String }
    deriving (Show)
instance Arbitrary ValidIdent where
    arbitrary = (liftM ValidIdent) $ (:) <$> chooseAlpha <*> listOf chooseAlphaNum
                
chooseAlpha = oneof [choose ('a','z'), choose ('A','Z')]
chooseDigit = choose ('0','9')
chooseAlphaNum = oneof [chooseAlpha, chooseDigit]

tests = [ testGroup "literals"
          [ testProperty "boolean" testLitBool
          , testProperty "range" testRange
          , testProperty "integer" testLitInt
          , testProperty "string" testLitStr
--          , testProperty "float" testLitFloat
          ]
        , testGroup "functions"
          [ testProperty "no args from empty call" testEmptyCall
          ]
        , testGroup "operators"
          [ testProperty "addition operator" testOpAdd
          ]
        ]
