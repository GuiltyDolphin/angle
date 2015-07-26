module Test.Angle.Parse.Operations
    ( tests
    ) where

import Test.QuickCheck
    
import TestHelper
    
import Angle.Parse.Operations
import Angle.Types.Lang
import Control.Monad.Error
    
    
testAddList :: [LangLit] -> [LangLit] -> Property
testAddList xs ys = monadicEither $ do
                      res <- run $ addLit (LitList xs:ys)
                      assertEqualQC (LitList (xs++ys)) res

testAndLitBool :: [Bool] -> Property
testAndLitBool xs = monadicEither $ do
                      res <- run $ andLit (map LitBool xs)
                      assertEqualQC (LitBool $ and xs) res
                      
tests = [ testGroup "and"
          [ testProperty "andLit (with bools)" testAndLitBool
          ]
        ]



