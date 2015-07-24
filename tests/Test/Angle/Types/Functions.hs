module Angle.Types.Functions.Tests
    ( tests
    ) where

import Control.Monad
    
import Test.QuickCheck
import Test.QuickCheck.Monadic
   
import Angle.Types.Lang 
import Angle.Types.Functions
import Angle.Parse.Error
import TestHelper
    
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
            
testGeneral :: LangType -> LangType -> Property
testGeneral x y = canCast x y || canCast y x
                  ==> mostGeneral x == mostGeneral y

tests = [ testGroup "casting"
          [ testProperty "casting to same type" testCastSame
          ]
        ]








