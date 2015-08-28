{-|
Module      : Angle.Parse.Builtins
Description : Builtin language functions, classes and variables.
Copyright   : (c) Ben Moon, 2015
License     : GPL-3
Maintainer  : guiltydolphin@gmail.com
Stability   : experimental
Portability : POSIX

This module defines and exports classes, functions and variables that can be used in Angle.

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
    ( builtins
    , isBuiltin
    , builtinPrint
    , builtinStr
    , builtinIndex
    , builtinLength
--    , builtinCompose
--    , builtinPartial
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
import Data.Maybe (fromJust)
import System.Environment

import Angle.Parse.Error
import Angle.Parse.Scope
import Angle.Parse.Types
import Angle.Types.Lang
import Angle.Lex.Lexer (litList, evalScan)


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
                            , classBindings = builtinClassBinds
                            } }


-- | True if the identifier represents a builtin function.
isBuiltin :: LangIdent -> Bool
isBuiltin = (`elem`builtins) . getIdent


-- | List of the builtin functions.
builtins :: [String]
builtins = [ "print", "str"
           , "index", "length"
           --, "compose", "partial"
           , "input", "eval"
           , "isNull"
           , "asType", "getArgs"]


builtinFuns :: [(String, [LangLit] -> ExecIO LangLit)]
builtinFuns = [ ("print", builtinPrint)
              , ("str", builtinStr)
              , ("index", builtinIndex)
              , ("length", builtinLength)
              , ("input", builtinInput)
--              , ("eval", builtinEval)
              , ("isNull", builtinIsNull)
              , ("asType", builtinAsType)
              , ("getArgs", builtinGetArgs)
              ]


getBuiltinFun :: LangIdent -> [LangLit] -> ExecIO LangLit
getBuiltinFun (LangIdent x) = fromJust $ lookup x builtinFuns


-- | Builtin print function.
--
-- @print(..x)@: converts each element of @..x@ to a string
-- before printing the concatenated result followed by a newline to STDOUT.
builtinPrint :: [LangLit] -> ExecIO LangLit
builtinPrint xs = liftIO (putStrLn res) >> returnVal (LitStr res)
    where res = argsToString xs


-- | Builtin input function.
--
-- @input(..x)@: does the same as @builtinPrint@, but does
-- not automatically append a newline and returns the result
-- from STDIN.
builtinInput :: [LangLit] -> ExecIO LangLit
builtinInput xs = liftM LitStr (liftIO $ putStr res >> liftIO getLine) >>= returnVal
    where res = argsToString xs


argsToString :: [LangLit] -> String
argsToString = concatMap
               (\x -> case x of
                        (LitStr s) -> s
                        _          -> showSyn x)


builtinAsType :: [LangLit] -> ExecIO LangLit
builtinAsType [x] = return x
builtinAsType [x,y] = asType x y
builtinAsType (x:xs) = liftM LitList $ mapM (asType x) xs
builtinAsType _ = throwParserError $ callBuiltinErr "asType: invalid call"


asType :: LangLit -> LangLit -> ExecIO LangLit
asType x y | typeOf x == typeOf y = return y
asType (LitStr _) (LitList xs)
    | all isLitChar xs = return . LitStr $ map (\(LitChar x) -> x) xs
    where isLitChar (LitChar x) = True
          isLitChar _ = False
asType (LitStr _) x = return . toLitStr $ x
asType (LitFloat _) (LitInt x) = return . LitFloat $ fromIntegral x
asType (LitFloat _) (LitStr y) = fromStr y LitFloat
asType (LitInt _) (LitStr y) = fromStr y LitInt
asType (LitList _) x@(LitStr xs) = return . LitList $ map LitChar xs
-- asType (LitList _) (LitStr y) = case evalScan y litList of
--                                    Left _ -> return LitNull
--                                    Right r -> return r
asType (LitBool _) (LitStr y) =
    case y of
      "true"  -> return $ LitBool True
      "false" -> return $ LitBool False
      _       -> return LitNull
asType (LitList _) x@(LitRange _ (Just _) _) = iterToLit x
asType _ _ = return LitNull


fromStr :: (Read a) => String -> (a -> LangLit) -> ExecIO LangLit
fromStr s f = case reads s of
              []       -> return LitNull
              [(r,"")] -> return $ f r


builtinLength :: [LangLit] -> ExecIO LangLit
builtinLength [LitList xs] = return . LitInt $ length xs
builtinLength [LitRange _ Nothing _] = return $ LitKeyword $ LangIdent "infinite"
builtinLength [LitRange x (Just y) Nothing] = return . LitInt $ (fromEnumL y + 1) - fromEnumL x
builtinLength _ = throwParserError $ callBuiltinErr "length: invalid call"


-- | @isNull(x)@ -> bool: true if @x@ is the null literal.
--
-- @isNull(..x)@ -> [bool]: list of applying @isNull@ to each argument.
builtinIsNull :: [LangLit] -> ExecIO LangLit
builtinIsNull [x] = return . LitBool $ isNull x
builtinIsNull xs = return . LitList $ map (LitBool . isNull) xs


-- | @str(x)@
builtinStr :: [LangLit] -> ExecIO LangLit
builtinStr [] = return $ LitStr ""
builtinStr xs | length xs > 1 = throwParserError $ wrongNumberOfArgumentsErr 1 (length xs)
              | otherwise = return $ toLitStr (head xs)


-- TODO:
-- - Currently wraps back round with negatives
--   e.g. index(-5,-1,[1,2,3]); -> [2, 3]
-- - should probably work more like Pyhon's indexing system.

-- | Builtin index function.
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

-- runCompose :: CallSig -> CallSig -> Expr -> ExecIO LangLit
-- runCompose c1 c2 e = do
--   intm <- callFunCallSig c2 [e]
--   callFunCallSig c1 [ExprLit intm]


-- partial :: CallSig -> [LangLit] -> ExecIO CallSig
-- partial x@(CallSig {callArgs=xArg@(ArgSig {stdArgs=xArgs})}) es
-- -- partial x@(CallSig {callArgs=ArgSig {stdArgs=xArgs, catchAllArg=Nothing}}) es
--     = do
--   p <- liftM envSourceRef get
--   let newStdArgs = drop (length es) xArgs
--       argList = map ExprLit es
--                 ++ map ExprIdent newStdArgs
--                 ++ map ExprParamExpand
--                        (maybeToList $ catchAllArg xArg)
--   return CallSig
--              { callArgs=xArg
--                { stdArgs=newStdArgs}
--              , callBody = SingleStmt
--                           (StmtExpr
--                            (ExprLambdaCall x argList)) p}
                            -- (map ExprLit es
                             -- ++ map ExprIdent
                             -- (drop (length es) xArgs)
                             -- ++ maybe [] ((:[]) . ExprParamExpand) (catchAllArg xArg)))) p}
  -- return CallSig { callArgs=ArgSig {stdArgs=drop (length es) xArgs, catchAllArg=Nothing }, callBody = SingleStmt (StmtExpr (ExprLambdaCall x (map ExprLit es ++ map ExprIdent (drop (length es) xArgs)))) p}


-- | Builtin partial application function.
--
-- @partial($f, ..x)@: Returns a lambda that is the partially applied function of @..x@ to @$f@.
--
-- @partial($f)@: Returns a lambda equivalent to the initial function.
-- builtinPartial :: [LangLit] -> ExecIO LangLit
-- builtinPartial [x@(LitLambda _)] = return x
-- builtinPartial (LitLambda xs:\@x) = liftM LitLambda $ partial x xs
-- -- builtinCompose (LitLambda x:[]) = return . LitLambda $ x
-- -- builtinCompose (LitLambda x:LitLambda y:[]) = liftM LitLambda $ compose x y
-- builtinPartial _ = throwImplementationErr "builtinPartial: better message please!"



-- compose :: CallSig -> CallSig -> ExecIO CallSig
-- compose x@(CallSig {callArgs=ArgSig {catchAllArg=Nothing}})
--         y@(CallSig {callArgs=ArgSig
--                     {stdArgs=[argY], catchAllArg=Nothing}})
--     = do
--   currRef <- liftM envSourceRef get
--   return y { callBody =
--                  SingleStmt
--                  (StmtExpr
--                   (ExprLambdaCall x
--                    [ExprLambdaCall y
--                     [ExprIdent argY]])) currRef }
-- compose _ _ = throwImplementationErr "compose: better message please!"


-- composeLambdas :: LangLit -> LangLit -> ExecIO LangLit
-- composeLambdas (LitLambda x) (LitLambda y) = liftM LitLambda $ compose x y
-- composeLambdas _ _ = throwImplementationErr "composeLambdas: better message please!"


-- | Builtin function composition function.
--
-- @compose($f, ..$g)@: composes @$f@ with @..$g@. That is, the result is a lambda that takes an argument and first applies @..$g@ to the argument, then @$f@ to the result produced by @..$g@.
--
-- @compose($f)@: returns a lambda equivalent to the original function.
-- builtinCompose :: [LangLit] -> ExecIO LangLit
-- builtinCompose [x@(LitLambda _)] = return x
-- builtinCompose (xs:\@x) = foldM composeLambdas x xs
-- -- builtinCompose (LitLambda x:[]) = return . LitLambda $ x
-- -- builtinCompose (LitLambda x:LitLambda y:[]) = liftM LitLambda $ compose x y
-- builtinCompose _ = throwImplementationErr "builtinCompose: better message please!"


toLitStr :: LangLit -> LangLit
toLitStr (LitInt x) = LitStr (show x)
toLitStr (LitFloat x) = LitStr (show x)
toLitStr (LitBool x) = LitStr (show x)
toLitStr x@(LitStr _) = x
toLitStr (LitList xs) = LitStr (show xs)
toLitStr x@(LitRange{}) = LitStr $ showSyn x
toLitStr LitNull = LitStr ""


builtinGetArgs :: [LangLit] -> ExecIO LangLit
builtinGetArgs _ = liftM (LitList . map LitStr) $ liftIO getArgs


fromEnumL :: LangLit -> Int
fromEnumL (LitInt x) = fromEnum x
fromEnumL (LitChar x) = fromEnum x
fromEnumL (LitFloat x) = fromEnum x


classToFun :: LangLit -> LangLit
classToFun (LitClassLambda l) = (LitLambda l)


typClass cls typ = undefined -- builtinAsType(typ,



builtinClass :: LangIdent -> Lambda -> (LangIdent, VarVal Lambda)
builtinClass name body = (name, VarVal { varDef = Just body
                                       , varBuiltin = True })


builtinClasses :: [(String, LangLit)]
builtinClasses = [ ("int", LitInt 1)
                 , ("float", LitFloat 1)
                 ]


builtinClassBinds :: BindEnv Lambda
builtinClassBinds = bindEnvFromList $ map toBuiltinClass builtinClasses
    where toBuiltinClass (name, val) = builtinClass (LangIdent name) (typClass name val)





