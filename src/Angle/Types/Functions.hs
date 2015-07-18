{-# LANGUAGE FlexibleContexts #-}
module Angle.Types.Functions
    ( cast
    , general
    , mostGeneral
    , shareCast
    , canCast
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
canCast LTInt LTFloat = True
canCast x y | x == y = True
            | otherwise = False
             
-- | Change the type of the given literal.
cast :: (CanError m) => LangLit -> LangType -> m LangLit
cast x y | not $ canCast (typeOf x) y = throwError $ typeCastErr  (typeOf x) y
         | typeOf x == y = return x
cast (LitInt x) LTFloat = return $ LitFloat (fromIntegral x)

                          
general :: LangType -> LangType
general LTInt = LTFloat
general x = x
            
mostGeneral :: LangType -> LangType
mostGeneral x | general x == x = x
              | otherwise = mostGeneral (general x)
            
shareCast :: LangType -> LangType -> Bool
shareCast x y = mostGeneral x == mostGeneral y
             
reqType :: (CanError m) => LangType -> LangType -> m LangType
reqType x y | x == y = return x
            | x `canCast` y = return y
            | y `canCast` x = return x
            | otherwise = throwError $ typeCastErr x y
            
-- Need a more efficient version of this?
allCast :: [LangLit] -> Bool
--allCast xs = (<=1) . length . nub $ map (general . typeOf) xs
allCast (x:xs) = all (shareCast (typeOf x) . typeOf) xs
             
castAll :: (CanError m) => [LangLit] -> m [LangLit]
castAll xs = mapM (`cast` reqType) xs
             where reqType = head $ nub $ map (general . typeOf) xs
