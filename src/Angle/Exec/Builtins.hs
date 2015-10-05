{-|
Module      : Angle.Exec.Builtins
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
module Angle.Exec.Builtins
    ( isBuiltin
    , builtinPrint
    , builtinStr
    , builtinIndex
    , builtinLength
    , builtinAsType
    , builtinGetArgs
    , builtinInput
    , builtinIsNull
    , builtinOpen
    , builtinRead
    , builtinWrite
    , builtinClose
    , builtinShell
    , argsToString

    -- ** Assignment handling
    , handleBuiltinAssignFun
    , handleBuiltinAssignLit

    -- ** Default environments
    , startEnv
    , initialEnvNotMain
    , initialEnvMain
    ) where


import Control.Monad
import Control.Monad.State
import System.Environment
import System.IO ( hFlush
                 , hGetLine
                 , hGetContents
                 , hGetChar
                 , hPutStr
                 , hClose
                 , stdout
                 , stdin
                 , stderr
                 , openFile
                 , IOMode(..))
import System.IO.Error ( tryIOError
                       , isAlreadyExistsError
                       , isDoesNotExistError
                       , isAlreadyInUseError
                       , isFullError
                       , isEOFError
                       , isIllegalOperation
                       , isPermissionError)
import System.Process (readProcess)

import Angle.Exec.Error
import Angle.Exec.Types
import Angle.Types.Lang
import Angle.Types.Scope


emptyArgs :: ArgSig
emptyArgs = ArgSig { stdArgs=[], catchAllArg=Nothing }


builtinCallSig :: LangIdent -> Lambda
builtinCallSig name =
    Lambda
    { lambdaArgs = emptyArgs
                   { catchAllArg = Just (CatchArg (LangIdent "x") Nothing) }
    , lambdaBody = body
    , lambdaScope = Nothing
    }
    where body = SingleStmt
                 ( StmtExpr
                   ( ExprFunCall name False
                     [ ExprParamExpand
                       ( LangIdent "x" )])) startRef


builtinVar :: LangIdent -> (LangIdent, VarVal Lambda)
builtinVar name = (name, VarVal
                           { varDef = Just $ builtinCallSig name
                           , varBuiltin = True })


builtinsVars :: BindEnv LangIdent Lambda
builtinsVars = bindEnvFromList $
               map (builtinVar . LangIdent) builtins


builtinValue :: LangIdent -> LangLit -> (LangIdent, VarVal LangLit)
builtinValue name val = (name, VarVal
                                 { varDef = Just val
                                 , varBuiltin = True })


builtinsValues :: BindEnv LangIdent LangLit
builtinsValues = bindEnvFromList $
                 map (\(x,y) -> builtinValue (LangIdent x) y) builtinValues


-- | Starting environment with builtin functions defined.
startEnv :: Env
startEnv = basicEnv { currentScope = startScope }


startScope :: Scope
startScope = emptyScope
    { lambdaBindings = builtinsVars
    , valueBindings = builtinsValues
    }


-- | Starting environment for programs not running as main.
initialEnvNotMain :: Env
initialEnvNotMain = startEnv { runAsMain = False }
    -- { currentScope = setVarLitInScope (LangIdent "main")
    --    (VarVal (Just (LitBool False)) True) (currentScope startEnv)
    -- }


-- | Starting environment for programs running as main.
initialEnvMain :: Env
initialEnvMain = startEnv { runAsMain = True }
    -- { currentScope = setVarLitInScope (LangIdent "main")
    --     (VarVal (Just (LitBool True)) True) (currentScope startEnv)
    -- }


-- | Builtin file handles
builtinHandles :: [(String, LangLit)]
builtinHandles = [ ("stdin", LitHandle stdin)
                 , ("stdout", LitHandle stdout)
                 , ("stderr", LitHandle stderr)
                 ]


builtinVariables :: [(String, LangLit)]
builtinVariables = [ ("as_class", LitBool False)
                   , ("main", LitBool False)
                   ]


-- | True if the identifier represents a builtin function.
isBuiltin :: LangIdent -> Bool
isBuiltin = (`elem`builtins) . getIdent


-- | List of the builtin functions.
builtins :: [String]
builtins = [ "print", "str"
           , "index", "length"
           , "input", "eval"
           , "isNull"
           , "asType", "getArgs"
           , "read", "write", "open", "close"
           , "include"
           , "shell"]


-- | List of builtin variables and their values.
builtinValues :: [(String, LangLit)]
builtinValues = builtinHandles ++ builtinVariables


-- | Builtin @print@ function.
--
-- @print(..x)@: converts each element of @..x@ to a string
-- before printing the concatenated result followed by a newline to STDOUT.
builtinPrint :: [LangLit] -> ExecIO LangLit
builtinPrint xs = withIOError (putStrLn res) >> returnVal (LitStr res)
    where res = argsToString xs


-- | Builtin @input@ function.
--
-- @input(..x)@: does the same as @builtinPrint@, but does
-- not automatically append a newline and returns the result
-- from STDIN.
builtinInput :: [LangLit] -> ExecIO LangLit
builtinInput xs = liftM LitStr (withIOError $ putStr res >> hFlush stdout >> getLine) >>= returnVal
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
builtinAsType _ = throwExecError $ callBuiltinErr "asType: invalid call"


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
asType (LitList _) (LitRange _ Nothing _) = throwExecError infiniteRangeErr
asType x y = throwExecError $ typeCastErr (typeOf y) (typeOf x)-- return LitNull


fromStr :: (Read a) => String -> (a -> LangLit) -> ExecIO LangLit
fromStr s f = case reads s of
              [(r,"")] -> return $ f r
              _        -> throwExecError . readErr $ s


-- | Builtin @length@ function.
--
-- @length(x:\@list)@ returns the number of elements in the list.
--
-- @length(x:\@range)@ returns the number of elements that would be
-- produced by the range if the range is finite, otherwise the
-- keyword @:infinite@.
builtinLength :: [LangLit] -> ExecIO LangLit
builtinLength [LitList xs] = return . LitInt $ length xs
builtinLength [x@(LitRange{})] | isInfiniteRange x = return $ LitKeyword $ LangIdent "infinite"
builtinLength [LitRange x (Just y) Nothing] = return . LitInt $ (fromEnumL y + 1) - fromEnumL x
builtinLength [LitRange x (Just y) (Just z)] = return .
  LitInt $ ceiling ((fromIntegral div1 / fromIntegral div2) :: Double)
  where
    div1 = (fromEnumL y + 1) - fromEnumL x
    div2 = fromEnumL z - fromEnumL x
builtinLength _ = throwExecError $ callBuiltinErr "length: invalid call"


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
builtinStr xs | length xs > 1 = throwExecError $ wrongNumberOfArgumentsErr 1 (length xs)
              | otherwise = return $ toLitStr (head xs)


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
    | x >= length xs = throwExecError $ indexOutOfBoundsErr x
    | x < 0 = return $ xs !! (length xs + x)
    | otherwise = return $ xs !! x
builtinIndex [x, LitStr s] = builtinIndex [x,LitList $ map LitChar s]  >>= joinList
  where
    joinList l = builtinAsType [LitStr "", l]
builtinIndex [LitInt x,LitInt y,LitList xs]
    | x > length xs || y > length xs
        = throwExecError $ indexOutOfBoundsErr x
    | x < 0 = builtinIndex
              [LitInt (length xs + x), LitInt y, LitList xs]
    | y < 0 = builtinIndex
              [LitInt x, LitInt (length xs + y + 1), LitList xs]
    | otherwise
        = return . LitList $ splice x y xs
builtinIndex [x,y,LitStr s] = builtinIndex [x, y, LitList $ map LitChar s] >>= joinList
  where
    joinList l = builtinAsType [LitStr "", l]
builtinIndex _ = throwExecError $ callBuiltinErr "index: invalid call signature"


splice :: Int -> Int -> [a] -> [a]
-- splice x y xs = take (1+y-x) $ drop x xs
splice x y = take (y-x) . drop x


toLitStr :: LangLit -> LangLit
toLitStr x@(LitStr _) = x
toLitStr (LitChar x) = LitStr [x]
toLitStr LitNull = LitStr ""
toLitStr x = LitStr $ showSyn x
-- toLitStr _ = error "toLitStr: cannot display type"


-- | Builtin @getArgs@ function.
--
-- @getArgs()@ returns the arguments passed to the program.
builtinGetArgs :: [LangLit] -> ExecIO LangLit
builtinGetArgs _ = liftM (LitList . map LitStr) $ withIOError getArgs


-- | Builtin @open@ function.
--
-- @open(file, mode)@ returns a file handle for @file@ in @mode@.
--
-- Modes are as follows:
--
-- [<] read only mode
--
-- [>] write mode (clobbers)
--
-- [>>] append mode
--
-- [<>] read-write mode
builtinOpen :: [LangLit] -> ExecIO LangLit
builtinOpen [LitStr fn, LitStr "<"] = liftM LitHandle $ withIOError $ openFile fn ReadMode
builtinOpen [LitStr fn, LitStr ">"] = liftM LitHandle $ withIOError $ openFile fn WriteMode
builtinOpen [LitStr fn, LitStr ">>"] = liftM LitHandle $ withIOError $ openFile fn AppendMode
builtinOpen [LitStr fn, LitStr "<>"] = liftM LitHandle $ withIOError $ openFile fn ReadWriteMode
builtinOpen _ = throwExecError $ callBuiltinErr "open: invalid call signature"


-- | Builtin @read@ function.
--
-- @read(handle)@ returns the unread part of the characters managed by @handle@.
--
-- @read(handle, integer)@ reads @integer@ lines from @handle@ and returns them as a new-line separated string.
--
-- @read(handle, integer, 'c')@ reads @integer@ characters from @handle@ and returns them in a string.
builtinRead :: [LangLit] -> ExecIO LangLit
builtinRead [LitHandle h] = liftM LitStr $ withIOError $ hGetContents h
builtinRead [LitHandle h, LitInt n] = liftM LitStr $ withIOError $ liftM unlines $ replicateM n $ hGetLine h
builtinRead [LitHandle h, LitInt n, LitStr "c"] = liftM LitStr $ withIOError $ replicateM n $ hGetChar h
builtinRead (s@(LitStr _):xs) = builtinOpen [s, LitStr "<"] >>= (builtinRead . (:xs))
builtinRead _ = throwExecError $ callBuiltinErr "read: invalid call signature"


-- | Builtin @write@ function.
--
-- @write(handle, string)@ writes @string@ to @handle@.
builtinWrite :: [LangLit] -> ExecIO LangLit
builtinWrite [LitHandle h, l@(LitStr s)] = withIOError (hPutStr h s) >> return l;
builtinWrite [h@(LitStr _), m@(LitStr _), l@(LitStr _)] = builtinOpen [h, m] >>= (builtinWrite . (:[l]))
builtinWrite _ = throwExecError $ callBuiltinErr "write: invalid call signature"


-- | Builtin @close@ function.
--
-- @close(handle)@ closes @handle@ for reading and writing.
builtinClose :: [LangLit] -> ExecIO LangLit
builtinClose xs = mapM_ bClose xs >> return LitNull
  where
    bClose (LitHandle x) = withIOError $ hClose x
    bClose x = throwExecError $ typeUnexpectedErr (typeOf x) LTHandle


-- | Builtin @shell@ function.
--
-- @shell(executable, arguments, stdin)@ runs the shell command
-- @executable@ with @args@ and @stdin@ and returns the result in a
-- string.
--
-- @shell(executable) = shell(executable, [], "")@
--
-- @shell(executable, arguments) = shell(executable, arguments, "")@
--
-- @shell(executable, stdin) = shell(executable, [], stdin)@
builtinShell :: [LangLit] -> ExecIO LangLit
builtinShell [p@(LitStr _)] = builtinShell [p, LitList [], LitStr ""]
builtinShell [p@(LitStr _), l@(LitList _)] = builtinShell [p, l, LitStr ""]
builtinShell [p@(LitStr _), sIn@(LitStr _)] = builtinShell [p, LitList [], sIn]
builtinShell [LitStr p, LitList args, LitStr sIn] = liftM LitStr $ withIOError $ readProcess p xs sIn
  where xs = map ((\(LitStr x) -> x) . toLitStr) args
builtinShell _ = throwExecError $ callBuiltinErr "shell: invalid call signature"


-- | Handler for assignments to builtin variables as literals.
handleBuiltinAssignLit :: LangIdent -> LangLit -> ExecIO a
handleBuiltinAssignLit n@(LangIdent "main") _ = throwExecError $ assignToBuiltinErr n (Just "assigned by execution program")
handleBuiltinAssignLit n _ = throwExecError $ assignToBuiltinErr n Nothing


-- | Handler for assignments to builtin variables as functions.
handleBuiltinAssignFun :: LangIdent -> Lambda -> ExecIO a
handleBuiltinAssignFun n _ = throwExecError $ assignToBuiltinErr n Nothing


handleIOError :: IOError -> ExecIO a
handleIOError e = throwExecError $ err e
  where
    err | isAlreadyExistsError e = alreadyExistsErr
        | isDoesNotExistError e = doesNotExistErr
        | isAlreadyInUseError e = alreadyInUseErr
        | isFullError e = deviceFullErr
        | isEOFError e = eofErr
        | isIllegalOperation e = illegalOperationErr
        | isPermissionError e = permissionErr
        | otherwise = error "Cannot handle user io exceptions"


-- | Executes code in IO but attempts to embed the exception into
-- Angle's error system.
withIOError :: IO a -> ExecIO a
withIOError x = do
    r <- liftIO $ tryIOError x
    case r of
        Right res -> return res
        Left e -> handleIOError e
