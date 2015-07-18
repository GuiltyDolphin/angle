{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
module Angle.Parse.Scope.Tests
    ( tests
    ) where

import Angle.Parse.Scope
import Angle.Types.Lang

import Test.Framework
import Test.Framework.Providers.HUnit
import Test.Framework.Providers.QuickCheck2
import Test.QuickCheck
    
import Control.Monad
import qualified Data.Map as M

instance Arbitrary LangLit where
    arbitrary = liftM LitInt arbitrary
                
instance Arbitrary CallSig where
    arbitrary = do
      idents <- arbitrary
      return $ CallSig idents (MultiStmt [])
             
instance Arbitrary BindEnv where
    arbitrary = do
      res <- arbitrary
      return $ M.fromList res
    
instance Arbitrary VarVal where
    arbitrary = do
      valDef <- arbitrary
      funDef <- arbitrary
      return $ emptyVar { varLitDef = valDef, varFunDef = funDef }
instance Arbitrary Scope where
    arbitrary = do
      outer <- arbitrary
      vars <- arbitrary
      return $ emptyScope { outerScope = outer, bindings = vars } 
tests = undefined
