module Test.Angle.Parse.Var
    ( tests
    ) where

import Angle.Parse.Var
import Angle.Types.Lang
import TestHelper


testVarLitDefSet :: VarVal -> LangLit -> Bool
testVarLitDefSet var val = varLitDef (setVarLit var val) == Just val
                           

testVarFunDefSet :: VarVal -> CallSig -> Bool
testVarFunDefSet var fd = varFunDef (setVarFun var fd) == Just fd

        
tests :: [TestTree]                  
tests = [ testGroup "setting"
          [ testProperty "lit def same" testVarLitDefSet
          , testProperty "fun def same" testVarFunDefSet
          ]
        ]
