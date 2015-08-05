module Test.Angle.Lex.Lexer
    ( tests
    ) where

import Angle.Lex.Lexer
import qualified Test.QuickCheck.Property as P
import Angle.Types.Lang
import TestHelper
    

testShowSynStmt :: Stmt -> Bool
testShowSynStmt x = showSynTest x stmt
                    

testShowSynSingStmt :: SingStmt -> Bool
testShowSynSingStmt x = showSynTest x singStmt
                        

testShowSynLangStruct :: LangStruct -> P.Result
testShowSynLangStruct x = liftShow x langStruct -- showSynTest x langStruct
                          

testShowSynExpr :: Expr -> P.Result
testShowSynExpr x = liftShow x expr -- showSynTest x expr
                    

testShowSynLambda :: Lambda -> P.Result
testShowSynLambda x = liftShow x lambda -- showSynTest x lambda
                        

showSynTest :: (ShowSyn a, Eq a) => a -> Scanner a -> Bool
showSynTest x sc = evalScan (showSyn x) sc == Right x
                   

liftShow :: (ShowSyn a, Eq a) => a -> Scanner a -> P.Result
liftShow x sc = P.result { P.ok = Just b
                         , P.reason = if b 
                                      then "" 
                                      else "expected:\n" ++ showSyn x ++ "\nbut got:\n" ++ reas}
    where b = showSynTest x sc
          reas = either (const "got an error!\n") showSyn (evalScan (showSyn x) sc)
                  
                   

-- withShowRes :: (ShowSyn a, Eq a) => a -> Scanner a -> Result
-- withShowRes x sc = MkResult (Just $ showSynTest x sc) True "bleh!" [] []
                   
withFail :: (ShowSyn a) => a -> P.Result
withFail x = P.failed { P.reason = showSyn x }
                
                
checkFloatStr :: Double -> Bool
checkFloatStr x = not $ 'e' `elem` (show x) 
              

escapedStr :: String -> String
escapedStr xs = "e\"" ++ xs ++ "\""


testLitStrEmpty :: Assertion
testLitStrEmpty = evalScan "\"\"" litStr @?= Right (LitStr "") 


testLitStr :: String -> Property
testLitStr x = '"' `notElem` x ==> evalScan (escapedStr x) litStr == Right (LitStr x)


testLitInt :: Int -> Bool
testLitInt x = evalScan (show x) litInt == Right (LitInt x)
               

testLitNull :: Bool
testLitNull = evalScan "()" litNull == Right LitNull


testLitBool :: Bool
testLitBool = evalScan "true" litBool == Right (LitBool True)
              && evalScan "false" litBool == Right (LitBool False)
                 

-- testRange :: LangLit -> LangLit -> LangLit -> Bool
-- testRange x y z = evalScan toTest litRange == Right expected
--     where toTest = concat ["(", showSyn x, "..", showSyn y, "..", showSyn z, ")"]
--           expected = LitRange x y (Just z)
                     

testLangLitInt :: Int -> Bool
testLangLitInt x = evalScan (show x) langLit  
                   ==  Right (LitInt x)
                   

testLangLitBool :: Bool
testLangLitBool = evalScan "true" langLit == Right (LitBool True) && evalScan "false" langLit == Right (LitBool False)


testEmptyCall :: Bool
testEmptyCall = evalScan "foo()" exprFunCall
 == Right (ExprFunCall (LangIdent "foo") [])
    

testOpAdd :: Bool
testOpAdd = evalScan "(+ 1 2)" langOp
 == Right (MultiOp OpAdd [ExprLit (LitInt 1), ExprLit (LitInt 2)])
    

testOpNeg :: Bool
testOpNeg = evalScan "-x" langOp ==  Right (SpecOp OpNeg (ExprIdent (LangIdent "x")))


tests :: [TestTree]
tests = [ testGroup "literals"
          [ testProperty "boolean" testLitBool
--          , testProperty "range" testRange
          , testProperty "integer" testLitInt
          , testProperty "string" testLitStr
          , testCase "empty string" testLitStrEmpty
--          , testProperty "float" testLitFloat
          ]
        , testGroup "functions"
          [ testProperty "no args from empty call" $ once testEmptyCall
          ]
        , testGroup "operators"
          [ testProperty "addition operator" $ once testOpAdd
          , testProperty "negation operator" $ once testOpNeg
          ]
        , localOption (QuickCheckMaxSize 10) $ 
          testGroup "show syntax"
          [ localOption (QuickCheckMaxSize 9) $ 
            testProperty "Stmt" $ testShowSynStmt
          , testProperty "SingStmt" $ testShowSynSingStmt
          , localOption (QuickCheckMaxSize 9) $ 
            testProperty "LangStruct" $ testShowSynLangStruct
          , testProperty "Expr" $ testShowSynExpr
          , testProperty "Lambda" testShowSynLambda
          ]
        ]
