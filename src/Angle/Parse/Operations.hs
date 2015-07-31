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


-- | Addition operator.
--
-- On list followed by arbitrary types: appends the tail arguments to the list.
--
-- On numeric types: performs arithmetic addition.
addLit :: MultiOperator
addLit (LitList x:xs) = return $ LitList (x++xs)
addLit xs             = onlyNumOp addLitNum xs
    where addLitNum = onNum (+) (+)


-- | Logical and operator.
--
-- On booleans: performs logical and of the values.
andLit :: MultiOperator
andLit []               = return $ LitBool True
andLit (x@(LitBool _):xs) = foldM andLitBool x xs
    where andLitBool = onLitBool (&&)
andLit (x:_) = langError $ typeNotValidErrT x


-- | Division operator.
-- 
-- On numerics: performs arithmetic division.
divLit :: MultiOperator
divLit = onlyNumOp divLitNum 
    where divLitNum = onNum div (/)


-- TODO: Note - currently floats /= integers, might
--  want to change this?

-- | Equality operator
--
-- On any types: true if all of the values are equal
eqLit :: MultiOperator
eqLit [] = return $ LitBool True
eqLit (x:xs) = return . LitBool . all (==x) $ xs


-- | Greater than or equal to operator.
--
-- On strings: compares the strings lexiographically.
--
-- On numerics: compares the values numerically.
greaterEqLit :: MultiOperator
greaterEqLit = compOp (>=)


-- | Greater than operator.
--
-- On strings: compares the strings lexiographically.
--
-- On numerics: compares the values numerically.
greaterLit :: MultiOperator
greaterLit = compOp (>)


-- | Less than or equal to operator.
--
-- On strings: compares the strings lexiographically.
--
-- On numerics: compares the values numerically.
lessEqLit :: MultiOperator
lessEqLit = compOp (<=)


-- | Less than comparison operator.
--
-- On strings: compares the strings lexiographically.
--
-- On numerics: compares the values numerically.
lessLit :: MultiOperator
lessLit = compOp (<)


-- | Multiplication operator.
--
-- On numerics: performs arithmetical multiplication on the values.
multLit :: MultiOperator
multLit = onlyNumOp multLitNum
    where multLitNum = onNum (*) (*)


-- | Negation operator.
--
-- On lists: returns the reversed list.
--
-- On numerics: returns the negative value.
negLit :: UnaryOperator
negLit (LitList xs) = return $ LitList (reverse xs)
negLit (LitInt x)   = return $ LitInt (-x)
negLit (LitFloat x) = return $ LitFloat (-x)
negLit x            = langError $ typeNotValidErrT x


-- | Logical not operator.
--
-- On boolean: performs logical negation.
notLit :: UnaryOperator
notLit (LitBool x) = return . LitBool $ not x
notLit x = langError $ typeNotValidErrT x 


-- | Logical or operator.
-- 
-- On booleans: performs logical OR.
orLit :: MultiOperator
orLit []                 = return $ LitBool False
orLit (x@(LitBool _):xs) = foldM orLitBool x xs
    where orLitBool = onLitBool (||)
orLit (x:_)              = langError $ typeNotValidErrT x


-- | Subtraction operator.
--
-- On list followed by integers: treats the integers as indices to remove from the list.
--
-- On numerics: subtracts all tailing numerics from the first numeric.
subLit :: MultiOperator
subLit (x@(LitList _):ys) 
    | allType LTInt ys 
    = foldM (flip $ langListDrop . getLitInt) x ys
      where langListDrop n (LitList zs) 
                | n >= length zs = langError $ indexOutOfBoundsErr n
                | otherwise = return (LitList res)
              where res = f++s
                    (f,_:s) = splitAt n zs
            langListDrop _ _ = undefined
subLit xs = onlyNumOp subLitNum xs
    where subLitNum = onNum (-) (-)
                      

----------------------
-- END OF OPERATORS --
----------------------


-- | Lift a binary operator across boolean values to 
-- work on `LangLit's.
onLitBool :: Binary Bool Bool -> BinaryOperator
onLitBool f (LitBool x) (LitBool y) = return . LitBool $ f x y
onLitBool _ x y = langError $ typeMismatchOpErrT x y


-- | Takes binary functions that act on Ints and Floats and
-- converts them to functions that act on `LangLit's.
onNum :: Binary Int Int -> Binary Double Double -> BinaryOperator
onNum i f = numOpLit i f LitInt LitFloat

         
-- | @numOp i f@ produces a function that will act upon
-- literal numerical values, using @i@ for integers, and
-- @f@ for floats. Integers will be casted to floats
-- if required.
numOp :: Binary Int LangLit -> Binary Double LangLit -> BinaryOperator
numOp i _ (LitInt x) (LitInt y)       = return $ i x y
numOp _ f (LitFloat x) (LitFloat y)   = return $ f x y
numOp _ f (LitInt x) (LitFloat y)     = return $ f (fromIntegral x) y
numOp _ f (LitFloat x) (LitInt y) =     return $ f x (fromIntegral y)
numOp _ _ x y                         = langError $ typeMismatchOpErrT x y
                                        

-- | Synonym for a function that performs a comparison
-- between its arguments.
type CompFunc       = forall a. (Ord a) => a -> a -> Bool          

-- | Synonym for standard operators that act on a list of values.
type MultiOperator  = (CanError m) => [LangLit] -> m LangLit


-- | Synonym for an operator that acts upon only two values.
type BinaryOperator = (CanError m) => LangLit -> LangLit -> m LangLit


-- | Synonym for an operator that acts upon one value.
type UnaryOperator  = (CanError m) => LangLit -> m LangLit


-- | Synonym for a binary function.
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
numOpLit :: Binary Int a -> Binary Double b -> (a -> LangLit) -> (b -> LangLit) -> BinaryOperator
numOpLit i f t1 t2 = numOp i' f'
    where i' x y = t1 $ i x y
          f' x y = t2 $ f x y
                
                
-- | Like `onNum', but given functions must result in boolean
-- values.
--
-- Used for implementing comparison operators.
onNumBool :: Binary Int Bool -> Binary Double Bool -> BinaryOperator
onNumBool i f = numOpLit i f LitBool LitBool


-- | Operator (or remaining cases of a) that can only
-- act upon numeric types.
-- 
-- Throws a `TypeNotValidError' if invalid literals are passed.
onlyNumOp :: (CanError m) => (LangLit -> LangLit -> m LangLit) -> [LangLit] -> m LangLit
onlyNumOp f (x@(LitInt _):xs) = foldM f x xs
onlyNumOp f (x@(LitFloat _):xs) = foldM f x xs
onlyNumOp _ (x:_)              = langError $ typeNotValidErrT x
onlyNumOp _ [] = throwImplementationErr "onlyNumOp - got empty list"
