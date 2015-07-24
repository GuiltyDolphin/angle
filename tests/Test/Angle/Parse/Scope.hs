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
