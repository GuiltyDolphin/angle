module Angle.Parse.Operations
    (
    ) where

import Angle.Types.Lang

addList :: LangLit -> LangLit -> LangLit
addList (LitList xs) (LitList ys) = LitList (xs ++ ys)
                                    
addNum :: LangLit -> LangLit -> LangLit
addNum (LitInt x) (LitInt y) = LitInt (x + y)
addNum (LitFloat x) (LitFloat y) = LitFloat (x + y)
addNum (LitInt x) y@(LitFloat _) = addNum (LitFloat (fromIntegral x)) y
addNum x@(LitFloat _) y@(LitInt _) = addNum y x
                                     
addLit :: [LangLit] -> LangLit
addLit xs@(LitList _:_) = foldr1 addList xs 
addLit xs@(LitInt _:_) = foldr1 addNum xs
addLit xs@(LitFloat _:_) = foldr1 addNum xs

                           
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
