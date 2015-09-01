{-|
Module      : Angle.Parse.Builtins
Description : Builtin language functions and variables.
Copyright   : (c) Ben Moon, 2015
License     : GPL-3
Maintainer  : guiltydolphin@gmail.com
Stability   : experimental
Portability : POSIX

This module defines and exports functions and variables that can be used in Angle.

Minutae:

A function beginning with @builtin@ (e.g., @builtinPrint@) defines a builtin function accessible through the language by the section following @builtin@ (although the casing may differ). Thus @builtinPrint@ defines the builtin @print@ function.

The builtin @someFunction@ function, the @someFunction@ builtin, all refer to @someFunction@ that is callable from the language.


Typically, builtin functions can act upon either a set amount
of arguments or a list of arguments, wherein the behaviour
is similar to the standard call, but produces a list of
results rather than a singleton.

For example, the @isNull@ builtin produces a single boolean
value when passed one argument, but produces a list of boolean
values when passed multiple arguments - as is common with
predicate functions in Angle.
-}
module Angle.Parse.Builtins
    ( isBuiltin
    , builtinPrint
    , builtinStr
    , builtinIndex
    , builtinLength
    , builtinAsType
    , builtinGetArgs
    , builtinInput
    , builtinIsNull
    , startEnv
    , argsToString
    ) where


-- TODO:
-- - inClass(x,..cls) - builtin function for checking
--    if x satisfies the given classes.


import Control.Monad
import Control.Monad.State
import System.Environment

import Angle.Parse.Error
import Angle.Parse.Scope
import Angle.Parse.Types
import Angle.Types.Lang


emptyArgs :: ArgSig
emptyArgs = ArgSig { stdArgs=[], catchAllArg=Nothing }


builtinCallSig :: LangIdent -> Lambda
builtinCallSig name =
    Lambda
    { lambdaArgs = emptyArgs
                   { catchAllArg = Just (LangIdent "x") }
    , lambdaBody = body
    }
    where body = SingleStmt
                 ( StmtExpr
                   ( ExprFunCall name
                     [ ExprParamExpand
                       ( LangIdent "x" )])) startRef


builtinVar :: LangIdent -> (LangIdent, VarVal Lambda)
builtinVar name = (name, VarVal
                           { varDef = Just $ builtinCallSig name
                           , varBuiltin = True })


builtinsVars :: BindEnv Lambda
builtinsVars = bindEnvFromList $
               map (builtinVar . LangIdent) builtins


-- | Starting environment with builtin functions defined.
startEnv :: Env
startEnv = basicEnv
           { currentScope = emptyScope
                            { lambdaBindings = builtinsVars
                            } }


-- | True if the identifier represents a builtin function.
isBuiltin :: LangIdent -> Bool
isBuiltin = (`elem`builtins) . getIdent


-- | List of the builtin functions.
builtins :: [String]
builtins = [ "print", "str"
           , "index", "length"
           , "input", "eval"
           , "isNull"
           , "asType", "getArgs"]


-- | Builtin @print@ function.
--
-- @print(..x)@: converts each element of @..x@ to a string
-- before printing the concatenated result followed by a newline to STDOUT.
builtinPrint :: [LangLit] -> ExecIO LangLit
builtinPrint xs = liftIO (putStrLn res) >> returnVal (LitStr res)
    where res = argsToString xs


-- | Builtin @input@ function.
--
-- @input(..x)@: does the same as @builtinPrint@, but does
-- not automatically append a newline and returns the result
-- from STDIN.
builtinInput :: [LangLit] -> ExecIO LangLit
builtinInput xs = liftM LitStr (liftIO $ putStr res >> liftIO getLine) >>= returnVal
    where res = argsToString xs


-- | Convert arguments to a string.
argsToString :: [LangLit] -> String
argsToString = concatMap (\x -> case x of
                             (LitStr s) -> s
                             _          -> showSyn x)


-- | Builtin @asType@ function.
--
-- @asType(typ, x)@ deduces the type of @typ@ and attempts to
-- cast @x@ to that type.
--
-- @asType(typ, ..x)@ produces a list of casted values.
builtinAsType :: [LangLit] -> ExecIO LangLit
builtinAsType [x] = return x
builtinAsType [x,y] = asType x y
builtinAsType (x:xs) = liftM LitList $ mapM (asType x) xs
builtinAsType _ = throwParserError $ callBuiltinErr "asType: invalid call"


asType :: LangLit -> LangLit -> ExecIO LangLit
asType x y | typeOf x == typeOf y = return y
asType (LitStr _) (LitList xs)
    | all isLitChar xs = return . LitStr $ map (\(LitChar x) -> x) xs
    where isLitChar (LitChar _) = True
          isLitChar _ = False
asType (LitStr _) x = return . toLitStr $ x
asType (LitFloat _) (LitInt x) = return . LitFloat $ fromIntegral x
asType (LitFloat _) (LitStr y) = fromStr y LitFloat
asType (LitInt _) (LitStr y) = fromStr y LitInt
asType (LitList _) (LitStr xs) = return . LitList $ map LitChar xs
asType (LitBool _) (LitStr y) =
    case y of
      "true"  -> return $ LitBool True
      "false" -> return $ LitBool False
      _       -> return LitNull
asType (LitList _) x@(LitRange _ (Just _) _) = iterToLit x
asType _ _ = return LitNull


fromStr :: (Read a) => String -> (a -> LangLit) -> ExecIO LangLit
fromStr s f = case reads s of
              [(r,"")] -> return $ f r
              _        -> return LitNull


-- | Builtin @length@ function.
--
-- @length(x:\@list)@ returns the number of elements in the list.
--
-- @length(x:\@range)@ returns the number of elements that would be
-- produced by the range if the range is finite, otherwise the
-- keyword @:infinite@.
builtinLength :: [LangLit] -> ExecIO LangLit
builtinLength [LitList xs] = return . LitInt $ length xs
builtinLength [LitRange _ Nothing _] = return $ LitKeyword $ LangIdent "infinite"
builtinLength [LitRange x (Just y) Nothing] = return . LitInt $ (fromEnumL y + 1) - fromEnumL x
builtinLength _ = throwParserError $ callBuiltinErr "length: invalid call"


-- | Builtin @isNull@ function.
--
-- @isNull(x)@ returns @true@ if the passed value is the null literal.
--
-- @isNull(..x)@ returns a list of the above.
builtinIsNull :: [LangLit] -> ExecIO LangLit
builtinIsNull [x] = return . LitBool $ isNull x
builtinIsNull xs = return . LitList $ map (LitBool . isNull) xs


-- | Builtin @str@ function.
--
-- @str(x)@ is the same as @asType("", x)@
builtinStr :: [LangLit] -> ExecIO LangLit
builtinStr [] = return $ LitStr ""
builtinStr xs | length xs > 1 = throwParserError $ wrongNumberOfArgumentsErr 1 (length xs)
              | otherwise = return $ toLitStr (head xs)


-- TODO:
-- - Currently wraps back round with negatives
--   e.g. index(-5,-1,[1,2,3]); -> [2, 3]
-- - should probably work more like Pyhon's indexing system.

-- | Builtin @index@ function.
--
-- @index(x:\@int, xs:\@list)@: retrieve element at index @x@ from @xs@
--
-- @index(x:\@int, y:\@int, xs:\@list)@: return a list of elements that lie between index @x@ and index @y@ of @xs@.
--
-- Negative indices are treated as working backwards from the
-- end of the list. With @-1@ being the last element.
builtinIndex :: [LangLit] -> ExecIO LangLit
builtinIndex [LitInt x,LitList xs]
    | x >= length xs = throwParserError $ indexOutOfBoundsErr x
    | x < 0 = return $ xs !! (length xs + x)
    | otherwise = return $ xs !! x
builtinIndex [LitInt x,LitInt y,LitList xs]
    | x >= length xs || y > length xs
        = throwParserError $ indexOutOfBoundsErr x
    | x < 0 = builtinIndex
              [LitInt (length xs + x), LitInt y, LitList xs]
    | y < 0 = builtinIndex
              [LitInt x, LitInt (length xs + y), LitList xs]
    | otherwise
        = return . LitList $ splice x y xs
builtinIndex _ = throwParserError $ callBuiltinErr "index: invalid call signature"


splice :: Int -> Int -> [a] -> [a]
splice x y xs = take (1+y-x) $ drop x xs


toLitStr :: LangLit -> LangLit
toLitStr (LitInt x) = LitStr (show x)
toLitStr (LitFloat x) = LitStr (show x)
toLitStr (LitBool x) = LitStr (show x)
toLitStr x@(LitStr _) = x
toLitStr (LitList xs) = LitStr (show xs)
toLitStr x@(LitRange{}) = LitStr $ showSyn x
toLitStr LitNull = LitStr ""
toLitStr (LitChar x) = LitStr [x]
toLitStr _ = error "toLitStr: cannot display type"


-- | Builtin @getArgs@ function.
--
-- @getArgs()@ returns the arguments passed to the program.
builtinGetArgs :: [LangLit] -> ExecIO LangLit
builtinGetArgs _ = liftM (LitList . map LitStr) $ liftIO getArgs


fromEnumL :: LangLit -> Int
fromEnumL (LitInt x) = fromEnum x
fromEnumL (LitChar x) = fromEnum x
fromEnumL (LitFloat x) = fromEnum x
fromEnumL _ = error "fromEnumL: non-enumerable type"





