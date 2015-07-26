{-# LANGUAGE FlexibleContexts #-}
module Angle.Types.Functions
    ( cast
    , general
    , mostGeneral
    , shareCast
    , canCast
    , allType
    , allNumeric
    , castAll
    , withCast
    , withCast'
    , (.<)
    ) where

import Control.Monad.Error
import Data.List (nub)

import Angle.Types.Lang
import Angle.Parse.Error
    

-- Casting API
-- - cast
--   (cast ~:: LangType -> LangType -> LangType)

-- | Can the first type be casted to the second type?
-- All types can be casted to themselves.
canCast :: LangType -> LangType -> Bool
canCast LTInt LTFloat   = True
canCast x y | x == y    = True
            | otherwise = False
             
-- | Change the type of the given literal.
cast :: (CanError m) => LangLit -> LangType -> m LangLit
cast x y | not $ canCast (typeOf x) y = langError $ typeCastErr  (typeOf x) y
         | typeOf x == y = return x
cast (LitInt x) LTFloat  = return $ LitFloat (fromIntegral x)

                          
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
            
-- Need a more efficient version of this?
-- | True if all the values in the list have a common cast.
allCast :: [LangLit] -> Bool
--allCast xs = (<=1) . length . nub $ map (general . typeOf) xs
allCast (x:xs) = all (shareCast (typeOf x) . typeOf) xs
             
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


ltNumeric :: LangType -> Bool
ltNumeric LTInt = True
ltNumeric LTFloat = True
                    
-- | True if every element of the list has numeric properties.
allNumeric :: [LangLit] -> Bool
allNumeric = allTypeP ltNumeric
                    
-- | True if every element of the list satisfies the
-- type predicate.
-- 
-- See `allNumeric` for an example.
allTypeP :: (LangType -> Bool) -> [LangLit] -> Bool
allTypeP f = all (f . typeOf)
                    
allType :: LangType -> [LangLit] -> Bool
allType t = all ((==t) . typeOf)

-- | True if all of the elements are the same type.
allSameType :: [LangLit] -> Bool
allSameType xs = allType (typeOf $ head xs) xs
                 

-- | True if the type supports basic comparisons.
comparable :: LangType -> Bool
comparable LTList = True
comparable LTInt = True
comparable LTBool = True
comparable LTFloat = True
comparable _ = False
               
infix 4 .<
(.<) :: LangLit -> LangLit -> LangLit
(LitInt x) .< (LitInt y) = LitBool (x < y)
(LitFloat x) .< (LitFloat y) = LitBool (x < y)
(LitStr x) .< (LitStr y) = LitBool (x < y)
                           
infix 4 .>=
(.>=) :: LangLit -> LangLit -> LangLit
(LitInt x) .>= (LitInt y) = LitBool (x >= y)
(LitFloat x) .>= (LitFloat y) = LitBool (x >= y)
(LitStr x) .>= (LitStr y) = LitBool (x >= y)
 
infix 4 .<=
(.<=) :: LangLit -> LangLit -> LangLit
(LitInt x) .<= (LitInt y) = LitBool (x <= y)
(LitFloat x) .<= (LitFloat y) = LitBool (x <= y)
(LitStr x) .<= (LitStr y) = LitBool (x <= y)
                            
not' :: LangLit -> LangLit
not' (LitBool x) = LitBool (not x)

