module Angle.Instances.Tests
    (
    ) where


import Angle.Exec.Parser
import Angle.Types.Lang

import Control.Monad

import Test.Framework
import Test.Framework.Providers.HUnit
import Test.Framework.Providers.QuickCheck2
import Test.QuickCheck
    
    
newtype LangInt = LInt { fromLangInt :: Int }
newtype LangBool = LBool { fromLangBool :: Bool }
newtype LangStr = LStr { fromLangStr :: String }

class ToLit a where
    toLangLit :: a -> LangLit

instance ToLit LangInt where
    toLangLit = LitInt . fromLangInt
instance ToLit LangBool where
    toLangLit = LitBool . fromLangBool
instance ToLit LangStr where
    toLangLit = LitStr . fromLangStr
                
instance Arbitrary LangInt where
    arbitrary = liftM LInt arbitrary
instance Arbitrary LangBool where
    arbitrary = liftM LBool arbitrary
instance Arbitrary LangStr where
    arbitrary = liftM LStr arbitrary
                
instance Arbitrary LangLit where
    arbitrary = oneof
                [ liftM LitInt arbitrary
                , liftM LitStr arbitrary
                , liftM LitBool arbitrary
                , liftM LitFloat arbitrary
                ]

tests = [ testGroup "scope tests"
          []
        ]
