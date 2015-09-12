module Test.Angle.Exec.Operations
    ( tests
    ) where

import Test.QuickCheck

import Angle.Exec.Operations
import Angle.Types.Lang
import TestHelper


-- testAddList :: TinyList LangLit -> TinyList LangLit -> Property
-- testAddList (TinyList xs) (TinyList ys) = monadicIO $ do
--                       res <- run $ runExec $ addLit (LitList xs:ys)
--                       assertEqual (LitList (xs++ys)) res


testAndLitBool :: [Bool] -> Property
testAndLitBool xs = monadicIO $ do
                      res <- run $ runExec $ andLit (map LitBool xs)
                      assertEqual (LitBool $ and xs) res


tests :: [TestTree]
tests = [ testGroup "add"
          [ -- testProperty "add list" testAddList
          ]
        , testGroup "and"
          [ testProperty "andLit (with bools)" testAndLitBool
          ]
        ]



