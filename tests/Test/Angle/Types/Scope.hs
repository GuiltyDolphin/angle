{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Test.Angle.Types.Scope
    ( tests
    ) where

import TestHelper

import Angle.Types.Scope

newtype SimpleName = SimpleName { getSimpleName :: String }
  deriving (Show, Eq, Ord)

instance Arbitrary SimpleName where
  arbitrary = SimpleName . ('v':) . show <$> choose (1, 10::Int)

newtype TestBind = TestBind { getTestBind :: BindEnv SimpleName Int }
  deriving (Show, Eq, Arbitrary)

newtype TestScope = TestScope { getTestScope :: GenScope SimpleName Int }
  deriving (Show, Eq, Arbitrary)

instance (Ord n, Arbitrary n, Arbitrary v) => Arbitrary (GenScope n v) where
  arbitrary = do
    outer <- frequency [ (7, return Nothing)
                      , (1, liftArby Just)
                      ]
    binds <- liftArby getTinyList
    return $ Scope outer (bindEnvFromList binds)
  shrink (Scope scs binds) = Scope <$> shrink scs <*> shrink binds


testMergeSameScope :: TestScope -> Bool
testMergeSameScope (TestScope { getTestScope = sc })
  = mergeScope sc sc == sc


testInsertIdempotent :: SimpleName -> VarVal Int -> TestScope -> Bool
testInsertIdempotent n v (TestScope { getTestScope = sc })
  = setVarInScope n v (setVarInScope n v sc) == setVarInScope n v sc


testInsertLast :: SimpleName -> VarVal Int -> VarVal Int -> TestScope -> Bool
testInsertLast n v1 v2 (TestScope { getTestScope = sc })
  = let sc' = setVarInScope n v2 (setVarInScope n v1 sc)
    in resolve n sc' == Just v2


testSetGet :: SimpleName -> VarVal Int -> TestScope -> Bool
testSetGet n v (TestScope { getTestScope = sc })
  = let sc' = setVarInScope n v sc
    in resolve n sc' == Just v


testDeleteSet :: SimpleName -> VarVal Int -> TestScope -> Bool
testDeleteSet n v (TestScope { getTestScope = sc })
  = let sc' = deleteFromScope n (setVarInScope n v sc)
    in sc' == deleteFromScope n sc


testGlobalScopeEquality :: TestScope -> Bool
testGlobalScopeEquality (TestScope { getTestScope = sc })
  | isOutermostScope sc = sc == globalScope sc
  | otherwise = sc /= globalScope sc


testResolveNotDefined :: SimpleName -> TestScope -> Property
testResolveNotDefined n (TestScope { getTestScope = sc })
  = let sc' = deleteFromScope n sc
    in case outerScope sc of
      Nothing -> label "global scope" $ isNothing $ resolve n sc'
      Just sc'' -> label "has outer" $ resolve n sc' == resolve n sc''


testDeleteGlobalDefinedLocal :: SimpleName -> VarVal Int -> TestScope -> Property
testDeleteGlobalDefinedLocal n v (TestScope { getTestScope = sc })
  = isOutermostScope sc /= True ==>
    let sc'  = setVarInScope n v sc
        sc'' = modifyGlobalScope sc' (deleteFromScope n)
    in resolve n sc'' == resolve n sc'


tests :: [TestTree]
tests = [ testGroup "merge"
          [ testProperty "merging a scope with itself has no effect" testMergeSameScope
          ]
        , testGroup "insert"
          [ testProperty "inserting is idempotent" testInsertIdempotent
          , testProperty "value after two inserts is value from last insert" testInsertLast
          , testProperty "get after insert is the inserted value" testSetGet
          , testProperty "deleting is the same as setting then deleting" testDeleteSet
          ]
        , testGroup "global scope"
          [ testProperty "global scope is current scope if current scope is outermost scope, otherwise not" testGlobalScopeEquality
          ]
        , testGroup "resolve"
          [ testProperty "resolving a deleted variable is the same as resolving in the outer scope" testResolveNotDefined
          , testProperty "global scope does not affect resolving if definition exists in current scope" testDeleteGlobalDefinedLocal
          ]
        ]
