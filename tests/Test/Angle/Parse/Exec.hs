module Test.Angle.Parse.Exec
    ( tests
    ) where

import TestHelper
    
import Angle.Parse.Exec
import Angle.Types.Lang

basicAddition = ("(+ 1 2 3)", LitInt 6)
                
testBasicAddition = testExecExprS (fst basicAddition) 

testWithString :: String -> Scanner a -> a
testWithString s sc = either (error "testWithString") id (evalScan s sc)
                      
testWithLitS :: String -> Scanner a -> (a -> ExecIO LangLit) -> (LangLit -> Bool) -> IO Bool
testWithLitS s sc ex p = liftM p $ ex (testWithString s sc)
                         
testWithLitSEq :: (Eq a) => String -> Scanner a -> (a -> ExecIO LangLit) -> LangLit -> IO Bool
testWithLitSEq s sc ex e = testWithLitS s sc ex (==e)

testExecStmtS :: String -> (LangLit -> Bool) -> IO Bool
testExecStmtS s p = testWithLit s stmt execStmt p
                   
testExecExprS :: String -> (LangLit -> Bool) -> IO Bool
testExecExprS s p = liftM p $ execExpr (testWithString s expr)
                   
testExecExpr :: Expr -> (LangLit -> Bool) -> IO Bool
testExecExpr e p = liftM p $ execExpr e
                   
tests = []








