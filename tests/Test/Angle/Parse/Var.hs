module Test.Angle.Parse.Var
    ( tests
    ) where

import Angle.Parse.Var
import Angle.Types.Lang
import TestHelper

        
tests :: [TestTree]                  
tests = [ testGroup "setting"
          [ --testProperty "lit def same" testVarLitDefSet
          --, testProperty "fun def same" testVarFunDefSet
          ]
        ]
