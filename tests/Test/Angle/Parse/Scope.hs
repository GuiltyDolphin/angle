{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
module Test.Angle.Parse.Scope
    ( tests
    ) where

import Angle.Parse.Scope
import Angle.Types.Lang
import TestHelper

import Test.QuickCheck
    
import Control.Monad
import qualified Data.Map as M
import Data.Maybe (fromJust)

testOutermostScope :: Scope -> Property
testOutermostScope s = isOutermostScope s ==> outermostScope s == s
                       
testOutermostScopeParent :: Scope -> Property 
testOutermostScopeParent s = not (isOutermostScope s) ==> outermostScope s == outermostScope (fromJust $ outerScope s)
                             
testResolveDefinedInCurrent :: LangIdent -> VarVal -> Scope -> Bool
testResolveDefinedInCurrent n v s = let s' = setVarInScope n v s True in resolve n s' == Just v
                                             
testOnBindingsId :: Scope -> Bool
testOnBindingsId s = onBindings id s == s
                             
tests = [ testGroup "outermostScope"
          [ testProperty "outer same as current" testOutermostScope
          , testProperty "outermost of parent is outermost of current" testOutermostScopeParent
          ]
        , testGroup "resolve"
          [ testProperty "defined in current" testResolveDefinedInCurrent
          ]
        , testGroup "onBindings"
          [ testProperty "same with id" testOnBindingsId
          ]
        ]
