module Angle.Parse.Operations
    ( addLit
    , negLit
    ) where

import qualified Data.Foldable as F
import Control.Monad

import Angle.Types.Lang
import Angle.Types.Functions
import Angle.Parse.Error

addList :: LangLit -> LangLit -> LangLit
addList (LitList xs) (LitList ys) = LitList (xs ++ ys)
                                    
addNum :: (CanError m) => LangLit -> LangLit -> m LangLit
addNum (LitInt x) (LitInt y) = return $ LitInt (x + y)
addNum (LitFloat x) (LitFloat y) = return $ LitFloat (x + y)
addNum (LitInt x) y@(LitFloat _) = addNum (LitFloat (fromIntegral x)) y
addNum x@(LitInt _) y = cast x (typeOf y) >>= (`addNum` y)
addNum x@(LitFloat _) y@(LitInt _) = addNum y x
addNum x y = langError $ typeMismatchErr (typeOf x) (typeOf y)
                                     

-- If using foldr1, cannot support errors properly,
-- might need recursion?
-- ^ Maybe fixed
addLit :: (CanError m) => [LangLit] -> m LangLit
addLit xs@(LitList _:_) = return $ foldr1 addList xs 
addLit (x@(LitInt _):xs) = foldM addNum x xs
addLit (x@(LitFloat _):xs) = foldM addNum x xs

notBool :: LangLit -> LangLit
notBool (LitBool x) = LitBool (not x)

                      
andBool :: LangLit -> LangLit -> LangLit
andBool (LitBool x) (LitBool y) = LitBool (x && y)
                                  
andLit :: [LangLit] -> LangLit
andLit xs@(LitBool _:_) = foldr1 andBool xs

                          
orBool :: LangLit -> LangLit -> LangLit
orBool (LitBool x) (LitBool y) = LitBool (x || y)
                                 
orLit :: [LangLit] -> LangLit
orLit xs@(LitBool _:_) = foldr1 orBool xs

                         
negNum :: LangLit -> LangLit
negNum (LitInt x) = LitInt (-x)
negNum (LitFloat x) = LitFloat (-x)

negLit :: (CanError m) => LangLit -> m LangLit
negLit x | isNumeric x = return $ negNum x
         | otherwise = langError $ typeNotValidErr (typeOf x)
         where isNumeric (LitInt _) = True
               isNumeric (LitFloat _) = True
               isNumeric _ = False
