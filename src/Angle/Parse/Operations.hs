module Angle.Parse.Operations
    (
    ) where

import Angle.Types.Lang
import Angle.Parse.Error

addList :: LangLit -> LangLit -> LangLit
addList (LitList xs) (LitList ys) = LitList (xs ++ ys)
                                    
addNum :: (CanError m) => LangLit -> LangLit -> m LangLit
addNum (LitInt x) (LitInt y) = return $ LitInt (x + y)
addNum (LitFloat x) (LitFloat y) = return $ LitFloat (x + y)
addNum (LitInt x) y@(LitFloat _) = addNum (LitFloat (fromIntegral x)) y
addNum x@(LitFloat _) y@(LitInt _) = addNum y x
                                     

-- If using foldr1, cannot support errors properly,
-- might need recursion?
-- ^ Maybe fixed
addLit :: (CanError m) => [LangLit] -> m LangLit
addLit xs@(LitList _:_) = return $ foldr1 addList xs 
addLit xs@(LitInt _:_) = foldM addNum (head xs) xs
addLit xs@(LitFloat _:_) = foldM addNum (head xs) xs

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
