{-# LANGUAGE RankNTypes #-}
{-|
Module      : Angle.Exec.Operations
Description : Definitions for builtin operators.
Copyright   : Copyright (C) 2015 Ben Moon
License     : GNU GPL, version 3
Maintainer  : GuiltyDolphin@gmail.com
Stability   : alpha

Angle provides many builtin logical and arithmetical operators,
of which many are overloaded to work on multiple types. Definitions
for these operators can be found in this module.
-}
module Angle.Exec.Operations
    ( addLit
    , andLit
    , concatLit
    , divLit
    , eqLit
    , expLit
    , greaterEqLit
    , greaterLit
    , lessEqLit
    , lessLit
    , multLit
    , negLit
    , notLit
    , orLit
    , subLit
    , isBuiltinOp
    ) where

import Control.Monad

import Angle.Types.Lang
import Angle.Exec.Error

import Angle.Parse.Token (builtinOps)

isBuiltinOp :: LangIdent -> Bool
isBuiltinOp (LangIdent x) = x `elem` builtinOps

-- | Addition operator.
--
-- On list followed by arbitrary types: appends the tail arguments to the list.
--
-- On numeric types: performs arithmetic addition.
--
-- On strings: performs concatenation.
addLit :: MultiOperator
addLit (LitList x:xs@(_:_)) = return $ LitList (x++xs)
addLit (LitStr x:xs) | allType LTStr xs = return $ LitStr $ x ++ concatMap (\(LitStr y) -> y) xs
addLit xs             = onlyNumOp addLitNum xs
    where addLitNum = onNum (+) (+)


-- | Logical and operator.
--
-- On booleans: performs logical and of the values.
andLit :: MultiOperator
andLit []                 = return $ LitBool True
andLit [LitList xs]       = andLit xs
andLit (x@(LitBool _):xs) = foldM andLitBool x xs
    where andLitBool = onLitBool (&&)
andLit (x:_) = throwExecError $ typeNotValidErr x


-- | Concatenation operator
--
-- On lists: concatenates the lists.
--
-- On lists with other types: concatenates the lists
-- and appends the other elements in the appropriate places.
--
-- On other types: creates a list of the values.
concatLit :: MultiOperator
concatLit (z:zs) = foldM concatLit' z zs
  where concatLit' (LitList xs) (LitList ys) = return . LitList $ xs ++ ys
        concatLit' (LitList xs) y = return . LitList $ xs ++ [y]
        concatLit' x (LitList ys) = return . LitList $ x : ys
        concatLit' x y = return . LitList $ [x, y]
concatLit _ = throwExecError $ malformedSignatureErr "++"


-- | Division operator.
--
-- On numerics: performs arithmetic division.
divLit :: MultiOperator
divLit (_:xs) | LitInt 0 `elem` xs = throwExecError divideByZeroErr
              | LitFloat 0 `elem` xs = throwExecError divideByZeroErr
divLit xs = onlyNumOp divLitNum xs
    where divLitNum = onNum div (/)


-- | Equality operator
--
-- On any types: true if all of the values are equal
eqLit :: MultiOperator
eqLit [] = return $ LitBool True
eqLit [LitList xs] = eqLit xs
eqLit (x:xs) = return . LitBool . all (==x) $ xs


-- | Exponentation operator
--
-- On numerics: raises the first item to the power produced
-- by raising each successive element to the next power.
expLit :: MultiOperator
expLit = onlyNumOp expLitNum
  where
    expLitNum = onNum (^) (**)


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
negLit (LitList xs)  = return $ LitList (reverse xs)
negLit (LitInt x)    = return $ LitInt (-x)
negLit (LitFloat x)  = return $ LitFloat (-x)
negLit x@(LitBool _) = notLit [x]
negLit x             = throwExecError $ typeNotValidErr x


-- | Logical not operator.
--
-- On boolean: performs logical negation.
notLit :: MultiOperator
notLit [LitBool x] = return . LitBool $ not x
notLit [x] = throwExecError $ typeNotValidErr x
notLit xs = throwExecError $ wrongNumberOfArgumentsErr 1 (length xs)


-- | Logical or operator.
--
-- On booleans: performs logical OR.
orLit :: MultiOperator
orLit []                 = return $ LitBool False
orLit [LitList xs]       = orLit xs
orLit (x@(LitBool _):xs) = foldM orLitBool x xs
    where orLitBool = onLitBool (||)
orLit (x:_)              = throwExecError $ typeNotValidErr x


-- | Subtraction operator.
--
-- On list followed by integers: treats the integers as indices to remove from the list.
--
-- On numerics: subtracts all tailing numerics from the first numeric.
--
-- On single operand: see 'negLit'.
subLit :: MultiOperator
subLit [x] = negLit x
subLit (x@(LitList _):ys@(_:_))
    | allType LTInt ys
    = foldM (flip $ \(LitInt i) -> langListDrop i ) x ys
      where langListDrop n (LitList zs)
                | n >= length zs = throwExecError $ indexOutOfBoundsErr n
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
onLitBool _ x y = throwExecError $ typeMismatchOpErr x y


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
numOp _ _ x y                         = throwExecError $ typeMismatchOpErr x y


-- | Synonym for a function that performs a comparison
-- between its arguments.
type CompFunc       = forall a. (Ord a) => a -> a -> Bool

-- | Synonym for standard operators that act on a list of values.
type MultiOperator  = (CanErrorWithPos m) => [LangLit] -> m LangLit


-- | Synonym for an operator that acts upon only two values.
type BinaryOperator = (CanErrorWithPos m) => LangLit -> LangLit -> m LangLit


-- | Synonym for an operator that acts upon one value.
type UnaryOperator  = (CanErrorWithPos m) => LangLit -> m LangLit


-- | Synonym for a binary function.
type Binary a b = a -> a -> b


-- | Given a comparison function, produces an operator
-- that produces a comparison between `LangLit's.
--
-- When the operand is a single list, then the list will be
-- flattened until either a non-list value is found, or there
-- is more than one operand.
compOp :: CompFunc -> MultiOperator
compOp f [LitList xs]      = compOp f xs
compOp f xs = liftM (LitBool . and) $ mapM (uncurry $ compLit f) (group2 xs)


-- | Lift a comparison function to work on 'LangLit's.
compLit :: (CanErrorWithPos m) => (forall a. (Ord a) => a -> a -> Bool) -> LangLit -> LangLit -> m Bool
compLit f (LitInt x) (LitInt y) = return $ f (toRational x) (toRational y)
compLit f (LitInt x) (LitFloat y) = return $ f (toRational x) (toRational y)
compLit f x@(LitFloat _) y@(LitInt _) = compLit f y x
compLit f (LitFloat x) (LitFloat y) = return $ f (toRational x) (toRational y)
compLit f (LitStr x) (LitStr y) = return $ f x y
compLit _ x y = throwExecError $ typeMismatchOpErr x y


-- | Group the list into successive doubles.
--
-- e.g,
--
-- > group2 [1, 2, 3]
--
-- [(1, 2), (2, 3)]
group2 :: [a] -> [(a, a)]
group2 [] = []
group2 [x] = []
group2 xs = (fst', snd') : group2 (tail xs)
  where
    [fst', snd'] = take 2 xs


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
-- When the operand is a single list, then the list will be
-- flattened until either a non-list value is found, or there
-- is more than one operand.
--
-- Throws a `TypeNotValidError' if invalid literals are passed.
onlyNumOp :: (CanErrorWithPos m) => (LangLit -> LangLit -> m LangLit) -> [LangLit] -> m LangLit
onlyNumOp f [LitList xs]        = onlyNumOp f xs
onlyNumOp f (x@(LitInt _):xs)   = foldM f x xs
onlyNumOp f (x@(LitFloat _):xs) = foldM f x xs
onlyNumOp _ (x:_)               = throwExecError $ typeNotValidErr x
onlyNumOp _ [] = throwImplementationErr "onlyNumOp - got empty list"
