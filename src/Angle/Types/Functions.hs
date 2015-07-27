{-# LANGUAGE FlexibleContexts #-}
module Angle.Types.Functions
    ( cast
    , general
    , mostGeneral
    , shareCast
    , canCast
    , allType
    , castAll
    , withCast
    , withCast'
    ) where

import Control.Monad.Error

import Angle.Types.Lang
import Angle.Parse.Error
    

-- | True if the first type be casted to the second type.
--
-- All types can be casted to themselves.
canCast :: LangType -> LangType -> Bool
canCast LTInt LTFloat   = True
canCast x y | x == y    = True
            | otherwise = False
             

-- | Change the type of the given literal.
cast :: (CanError m) => LangLit -> LangType -> m LangLit
cast (LitInt x) LTFloat  = return $ LitFloat (fromIntegral x)
cast x y | typeOf x == y = return x
cast x y = langError $ typeCastErr (typeOf x) y

                          
general :: LangType -> LangType
general LTInt = LTFloat
general x = x
            

-- | @mostGeneral t@ is the least specific type that
-- @t@ can be casted to.
mostGeneral :: LangType -> LangType
mostGeneral x | general x == x = x
              | otherwise = mostGeneral (general x)
            

-- | True if the given types share a type that they
-- can be casted to.
shareCast :: LangType -> LangType -> Bool
shareCast x y = mostGeneral x == mostGeneral y
             

reqType :: (CanError m) => LangType -> LangType -> m LangType
reqType x y | x `canCast` y = return y
            | y `canCast` x = return x
            | otherwise = langError $ typeCastErr x y
                          

reqTypeList :: (CanError m) => [LangType] -> m LangType
reqTypeList (x:xs) = foldM reqType x xs
reqTypeList [] = undefined
             

-- | Cast all the values in a list to the same type.
castAll :: (CanError m) => [LangLit] -> m [LangLit]
castAll xs = do
  t <- reqTypeList (map typeOf xs)
  mapM (`cast` t) xs
                             

withCast :: (CanError m) => (LangLit -> LangLit -> LangLit) -> LangLit -> LangLit -> m LangLit
withCast f x y = do
  t <- reqType (typeOf x) (typeOf y)
  x' <- cast x t
  y' <- cast y t
  return $ f x' y'
         

withCast' :: (CanError m) => (LangLit -> LangLit -> m LangLit) -> LangLit -> LangLit -> m LangLit
withCast' f x y = do
  t <- reqType (typeOf x) (typeOf y)
  x' <- cast x t
  y' <- cast y t
  f x' y'
                    

allType :: LangType -> [LangLit] -> Bool
allType t = all ((==t) . typeOf)
