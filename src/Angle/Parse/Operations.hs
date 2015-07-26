{-# LANGUAGE RankNTypes #-}
module Angle.Parse.Operations
    ( addLit
    , andLit
    , divLit
    , eqLit
    , greaterEqLit
    , greaterLit
    , lessEqLit
    , lessLit
    , multLit
    , negLit
    , notLit
    , orLit
    , subLit
    ) where

import Control.Monad

import Angle.Types.Lang
import Angle.Types.Functions
import Angle.Parse.Error
                                    
-- If using foldr1, cannot support errors properly,
-- might need recursion?
-- ^ Maybe fixed
addLit :: MultiOperator
addLit (LitList x:xs) = return $ LitList (x++xs)
addLit xs             = onlyNumOp addLitNum xs
    where addLitNum = onNum (+) (+)


andLit :: MultiOperator
andLit []               = return $ LitBool True
andLit (x@(LitBool _):xs) = foldM andLitBool x xs
    where andLitBool = onLitBool (&&)


divLit :: MultiOperator
divLit = onlyNumOp divLitNum 
    where divLitNum = onNum div (/)


-- TODO: Note - currently floats /= integers, might
--  want to change this?

-- | Equality operator
-- On any types: true if all of the values are equal
eqLit :: MultiOperator
eqLit (x:xs) = return . LitBool . all (==x) $ xs


greaterEqLit :: MultiOperator
greaterEqLit = compOp (>=)


greaterLit :: MultiOperator
greaterLit = compOp (>)


lessEqLit :: MultiOperator
lessEqLit = compOp (<=)


-- | Less comparison operator
-- On numerics: true if each numeric is smaller than the preceding numeric.
-- On booleans: treating true as 1 and false as 0, the same as for numerics.
-- On strings: compares each element lexiographically.
lessLit :: MultiOperator
lessLit = compOp (<)


multLit :: MultiOperator
multLit = onlyNumOp multLitNum
    where multLitNum = onNum (*) (*)


-- | Negation operator:
-- On lists: returns the reversed list.
-- On numeric values: returns the negative value.
-- Other types: not valid.
negLit :: UnaryOperator
negLit (LitList xs) = return $ LitList (reverse xs)
negLit (LitInt x)   = return $ LitInt (-x)
negLit (LitFloat x) = return $ LitFloat (-x)
negLit x            = langError $ typeNotValidErrT x


notLit :: UnaryOperator
notLit (LitBool x) = return . LitBool $ not x
notLit x = langError $ typeNotValidErrT x 


orLit :: MultiOperator
orLit []                 = return $ LitBool False
orLit (x@(LitBool _):xs) = foldM orLitBool x xs
    where orLitBool = onLitBool (||)
orLit (x:_)              = langError $ typeNotValidErrT x


-- | Subtraction operator:
-- On List followed by Integers: treats the integers as indices to remove from the list.
-- On Numerics: subtracts all tailing numerics from the first numeric.
-- Other types: not valid. 
subLit :: MultiOperator
subLit (xs@(LitList _):ys) 
    | allType LTInt ys 
    = foldM (flip $ langListDrop . getLitInt) xs ys
      where langListDrop n (LitList zs) 
                | n >= length ys = langError $ indexOutOfBoundsErr n
                | otherwise = return . head . snd . splitAt n $ zs
subLit xs = onlyNumOp subLitNum xs
    where subLitNum = onNum (-) (-)


-- | Lift a binary operator across boolean values to 
-- work on `LangLit's.
onLitBool :: Binary Bool Bool -> BinaryOperator
onLitBool f (LitBool x) (LitBool y) = return . LitBool $ f x y
onLitBool _ x y = langError $ typeMismatchOpErrT x y


-- | Takes binary functions that act on Ints and Floats and
-- converts them to functions that act on `LangLit's.
onNum :: Binary Int Int -> Binary Float Float -> BinaryOperator
onNum i f = numOpLit i f LitInt LitFloat

         
-- | @numOp i f@ produces a function that will act upon
-- literal numerical values, using @i@ for integers, and
-- @f@ for floats. Integers will be casted to floats
-- if required.
numOp :: Binary Int LangLit -> Binary Float LangLit -> BinaryOperator
numOp i _ (LitInt x) (LitInt y)       = return $ i x y
numOp _ f (LitFloat x) (LitFloat y)   = return $ f x y
numOp _ f (LitInt x) (LitFloat y)     = return $ f (fromIntegral x) y
numOp i f x@(LitFloat _) y@(LitInt _) = numOp i f y x
numOp _ _ x y                         = langError $ typeMismatchOpErrT x y
                                        

type CompFunc       = forall a. (Ord a) => a -> a -> Bool          

type MultiOperator  = (CanError m) => [LangLit] -> m LangLit


type BinaryOperator = (CanError m) => LangLit -> LangLit -> m LangLit


type UnaryOperator  = (CanError m) => LangLit -> m LangLit


type Binary a b = a -> a -> b


-- | Given a comparison function, produces an operator
-- that produces a comparison between `LangLit's.
compOp :: CompFunc -> MultiOperator
compOp f (x@(LitStr _):xs) = foldM (compStr f) x xs
 where compStr :: CompFunc -> BinaryOperator
       compStr g (LitStr y) (LitStr z) 
           = return . LitBool $ g y z
       compStr _ y z 
           = langError $ typeMismatchOpErrT y z
compOp f xs = onlyNumOp (onNumBool f f) xs
              

-- | Convenience function for producing `LangLit' values from
-- functions to be supplied to `numOp'.
numOpLit :: Binary Int a -> Binary Float b -> (a -> LangLit) -> (b -> LangLit) -> BinaryOperator
numOpLit i f t1 t2 = numOp i' f'
    where i' x y = t1 $ i x y
          f' x y = t2 $ f x y
                
                
-- | Like `onNum', but given functions must result in boolean
-- values.
-- Used for implementing comparison operators.
onNumBool :: Binary Int Bool -> Binary Float Bool -> BinaryOperator
onNumBool i f = numOpLit i f LitBool LitBool






                  
               
-- | Operator (or remaining cases of a) that can only
-- act upon numeric types.
-- Throws a `TypeNotValidError' if invalid literals are passed.
onlyNumOp :: (CanError m) => (LangLit -> LangLit -> m LangLit) -> [LangLit] -> m LangLit
onlyNumOp f (x@(LitInt _):xs) = foldM f x xs
onlyNumOp f (x@(LitFloat _):xs) = foldM f x xs
onlyNumOp _ (x:_)              = langError $ typeNotValidErrT x




                 

       

