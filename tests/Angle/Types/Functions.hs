module Angle.Types.Functions.Tests
    ( tests
    ) where

import Control.Monad
    
import Test.Framework
import Test.Framework.Providers.HUnit
import Test.Framework.Providers.QuickCheck2
import Test.QuickCheck
import Test.QuickCheck.Monadic
   
import Angle.Types.Lang 
import Angle.Types.Functions
import Angle.Parse.Error
    
instance Arbitrary LangType where
    arbitrary = elements [LTStr, LTInt, LTFloat, LTList
                         , LTBool, LTRange, LTNull ]
                
instance Arbitrary LangLit where
    arbitrary = oneof [liftM LitInt arbitrary
                      , liftM LitStr arbitrary
                      , liftM LitFloat arbitrary
                      , liftM LitBool arbitrary
                      ]

testShouldGeneralCast :: LangType -> LangType -> Property
testShouldGeneralCast x y = mostGeneral x == mostGeneral y ==> shareCast x y == True

--testCastSame :: (CanError m) => LangLit -> LangType -> PropertyM m Bool
--testCastSame l t = typeOf l == t ==> (monadic $ do
--                     res <- typeOf (cast l t) == t
--                     assert (res == t))

testOther :: PropertyM (Either LangError) ()
testOther = (do
  res <- run (cast (LitInt 1) LTInt)
  assert (typeOf res == LTInt))
             
testCastSame :: LangLit -> LangType -> Property
testCastSame l t = typeOf l == t ==> monadicEither $ do
                     res <- liftM typeOf $ run $ cast l t
                     assert (res == t)
             
monadicEither :: PropertyM (Either e) a -> Property
monadicEither = monadic (\x -> case x of
                                 Left _ -> property False
                                 Right x -> x)
                                      
testGeneral :: LangType -> LangType -> Property
testGeneral x y = canCast x y || canCast y x
                  ==> mostGeneral x == mostGeneral y

tests = undefined








