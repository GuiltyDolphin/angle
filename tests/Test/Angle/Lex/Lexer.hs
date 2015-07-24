module Test.Angle.Lex.Lexer
    ( tests
    ) where

import Control.Monad (liftM)

    
import Control.Applicative
    
import Angle.Lex.Lexer
import Angle.Lex.Helpers (evalScan)
import Angle.Types.Lang
    
import TestHelper
    

testShowSynStmt :: Stmt -> Bool
testShowSynStmt x = showSynTest x stmt
                    
testShowSynSingStmt :: SingStmt -> Bool
testShowSynSingStmt x = showSynTest x singStmt
                        
testShowSynLangStruct :: LangStruct -> Bool
testShowSynLangStruct x = showSynTest x langStruct
                          
testShowSynExpr :: Expr -> Bool
testShowSynExpr x = showSynTest x expr
                        
showSynTest :: (ShowSyn a, Eq a) => a -> Scanner a -> Bool
showSynTest x sc = evalScan (showSyn x) sc == Right x

    

exampleAssign = "x = 1"
                
                
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

testLitBool :: Bool
testLitBool = evalScan "true" litBool == Right (LitBool True)
              && evalScan "false" litBool == Right (LitBool False)

                 

testRange :: LangLit -> LangLit -> Bool
testRange x y = evalScan toTest litRange == Right expected
    where toTest = concat ["(", showSyn x, "..", showSyn y, ")"]
          expected = LitRange (ExprLit x) (ExprLit y)
                     
testLangLitInt :: Int -> Bool
testLangLitInt x = evalScan (show x) langLit  
                   ==  Right (LitInt x)
                   
testLangLitBool :: Bool
testLangLitBool = evalScan "true" langLit == Right (LitBool True) && evalScan "false" langLit == Right (LitBool False)

-- FUNCTIONS
testEmptyCall :: Bool
testEmptyCall = evalScan "foo()" exprFunCall
 == Right (ExprFunCall (LangIdent "foo") [])
    
testOpAdd :: Bool
testOpAdd = evalScan "(+ 1 2)" langOp
 == Right (MultiOp OpAdd [ExprLit (LitInt 1), ExprLit (LitInt 2)])
    
testOpNeg :: Bool
testOpNeg = evalScan "-x" langOp ==  Right (SpecOp OpNeg (ExprIdent (LangIdent "x")))


tests = [ testGroup "literals"
          [ testProperty "boolean" testLitBool
--          , testProperty "range" testRange
 --         , testProperty "integer" testLitInt
  --        , testProperty "string" testLitStr
--          , testProperty "float" testLitFloat
          ]
        , testGroup "functions"
          [ testProperty "no args from empty call" $ once testEmptyCall
          ]
        , testGroup "operators"
          [ testProperty "addition operator" $ once testOpAdd
          , testProperty "negation operator" $ once testOpNeg
          ]
        , localOption (QuickCheckMaxSize 10) $ testGroup "show syntax"
          [ testProperty "Stmt" $ testShowSynStmt
          , testProperty "SingStmt" $ testShowSynSingStmt
          , testProperty "LangStruct" $ testShowSynLangStruct
          ,  testProperty "Expr" $ testShowSynExpr
          ]
        ]
