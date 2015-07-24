module Test.Angle.Parse.Operations
    ( tests
    ) where

import Test.QuickCheck
    
import TestHelper
    
import Angle.Parse.Operations
import Angle.Types.Lang
import Control.Monad.Error
    
    
testAddList :: [Expr] -> [Expr] -> Bool
testAddList xs ys = (LitList (xs++ys)) == addList (LitList xs) (LitList ys)

testAndBool :: Bool -> Bool -> Bool
testAndBool x y = andBool (LitBool x) (LitBool y) == (LitBool $ x&&y)
                  
testAndLitBool :: [Bool] -> Property
testAndLitBool xs = monadicEither $ do
                      res <- run $ andLit (map LitBool xs)
                      assertEqualQC (LitBool $ and xs) res


tests = [ testGroup "and"
          [ testProperty "andBool" testAndBool
          , testProperty "andLit (with bools)" testAndLitBool
          ]
        ]



