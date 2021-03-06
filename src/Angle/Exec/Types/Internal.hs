{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-|
Module      : Angle.Exec.Types.Internal
Description : Defines types used for executing Angle programs.
Copyright   : Copyright (C) 2015 Ben Moon
License     : GNU GPL, version 3
Maintainer  : GuiltyDolphin@gmail.com
Stability   : alpha

Defines the base types (ExecIO and Env) for executing Angle programs.

Also defines functions for working with iterable types in Angle.
-}
module Angle.Exec.Types.Internal
    ( ExecIO
    , runExecIOBasic
    , runExecIOEnv
    , fromIter
    , returnVal
    , iterToLit
    , fromEnumL
    , isInfiniteRange
    -- ** Excution environment
    , Env(..)
    , getEnv
    , updateEnv
    , basicEnv
    , getEnvValue
    , setEnvSynRep
    , updatePos

    , pushEnvCall
    , popEnvCall

    , updateStmt
    ) where


import Control.Applicative
import Control.Monad.Except
import Control.Monad.Trans.Except
import Control.Monad.State

import Angle.Exec.Error
import Angle.Types.Lang
import Angle.Types.Scope


-- | Angle program execution monad.
newtype ExecIO a = ExecIO
    { runExecIO :: ExceptT AngleError (StateT Env IO) a }
    deriving ( Functor, Applicative, Monad
             , MonadIO)


instance MonadError AngleError ExecIO where
    throwError = throwAE
    catchError = catchAE


instance CanErrorWithPos ExecIO where
    getErrorPos = liftM envSourceRef get
    getErrorSource = liftM sourceText get
    getErrorFile = liftM currentFile get
    getErrorCallStack = liftM (currentStack . callStack) get
    getErrorCall = liftM (((,) <$> currentName <*>  currentStmt) . callStack) get


instance CanError ExecIO where
    throwAE = ExecIO . throwE
    catchAE (ExecIO e) h
        = ExecIO (lift $ runExceptT e) >>= either h return


instance MonadState Env ExecIO where
    get = ExecIO $ lift get
    put x = ExecIO $ lift $ put x


-- | Environment in which Angle programs execute.
data Env = Env { currentScope :: Scope
               , sourceText :: String
               , envSourceRef :: SourceRef
               , envSynRep :: String
               , envValue :: LangLit
               , currentException :: Maybe AngleError
               , callStack :: CallStack
               -- ^ Calls and the statements that call them.
               , angleLibPath :: [FilePath]
               , currentFile :: Maybe FilePath
               , runAsMain :: Bool
               } deriving (Show, Eq)


-- | Track function calls and statement execution.
data CallStack = CallStack
    { currentStack :: [(LangIdent, Stmt)]
    , currentName :: LangIdent
    , currentStmt :: Stmt
    } deriving (Show, Eq)


startStack :: CallStack
startStack = CallStack { currentStack = []
                       , currentName = LangIdent "MODULE" }

-- | Retrieve the current execution environment.
getEnv :: ExecIO Env
getEnv = get


-- | Update the current execution environment
updateEnv :: Env -> ExecIO ()
updateEnv = put


-- | Run Angle program with a custom environment.
runExecIOEnv :: Env -> ExecIO a -> IO (Either AngleError a)
runExecIOEnv e x = evalStateT (runExceptT $ runExecIO x) e


-- | Run Angle program with a basic environment.
runExecIOBasic :: ExecIO a -> IO (Either AngleError a)
runExecIOBasic = runExecIOEnv basicEnv


-- | Update the current environment tracking position.
updatePos :: SourceRef -> ExecIO ()
updatePos pos = modify (\e -> e { envSourceRef = pos })


-- | Set the environment's current syntax representation.
setEnvSynRep :: String -> ExecIO ()
setEnvSynRep x = modify (\e -> e { envSynRep = x })


-- | Basic environment.
basicEnv :: Env
basicEnv = Env { currentScope = emptyScope
               , sourceText = ""
               , envSourceRef = startRef
               , envSynRep = ""
               , envValue = LitNull
               , currentException = Nothing
               , angleLibPath = []
               , currentFile = Nothing
               , callStack = startStack
               }


-- In a language like Java, could use try..catch
-- to exit early from function, maybe something
-- similar for this?

-- For this,
-- Throw the 'error' in the return statement,
-- then catch it in the calling expression.

-- | Set the current value in the execution environment.
returnVal :: LangLit -> ExecIO LangLit
returnVal v = putEnvValue v >> return v


-- | Retrieve the current environment value.
getEnvValue :: ExecIO LangLit
getEnvValue = liftM envValue get


putEnvValue :: LangLit -> ExecIO ()
putEnvValue v = modify (\e -> e {envValue=v})


popEnvCall :: ExecIO ()
popEnvCall = do
    currEnv <- getEnv
    let currStack = callStack currEnv
        ((newName,_):newStack) = currentStack currStack
        newCallStack = currStack { currentStack = newStack, currentName = newName }
    put currEnv { callStack = newCallStack }


pushEnvCall :: LangIdent -> ExecIO ()
pushEnvCall newCall = do
    currEnv <- getEnv
    let currStack = callStack currEnv
        oldName = currentName currStack
        oldStmt = currentStmt currStack
        oldStack = currentStack currStack
        newStack = (oldName, oldStmt) : oldStack
        newCallStack = currStack { currentName = newCall, currentStack = newStack }
    put currEnv { callStack = newCallStack }


updateStmt :: Stmt -> ExecIO ()
updateStmt s = do
    currEnv <- getEnv
    let oldStack = callStack currEnv
    put currEnv { callStack = oldStack { currentStmt = s } }


-- | Convert a list or range into a list of literals.
fromIter :: LangLit -> ExecIO [LangLit]
fromIter (LitList xs) = return xs
fromIter (LitStr xs) = return $ map LitChar xs
fromIter (LitRange x (Just y) Nothing) = iterFromTo x y
fromIter (LitRange x (Just y) (Just z)) = iterFromThenTo x y z
fromIter (LitRange x Nothing Nothing) = iterFrom x
fromIter (LitRange x Nothing (Just y)) = iterFromThen x y
fromIter x = throwExecError $ nonEnumErr $ typeOf x


-- | True if the range has infinite size.
isInfiniteRange :: LangLit -> Bool
isInfiniteRange (LitRange _ Nothing _) = True
isInfiniteRange (LitRange x (Just y) (Just z))
       = (y' > x' || y' == x') && zeroStep
  where
    [x',y',z'] = map fromEnumL [x,y,z]
    zeroStep = z' == x'
isInfiniteRange (LitRange x y@(Just _) Nothing) = isInfiniteRange (LitRange x y (Just (LitInt (fromEnumL x + 1))))
isInfiniteRange _ = error "isInfiniteRange: Passed a non-range"


-- | Retrieve enum value from literal, literal must be
-- enumerable.
fromEnumL :: LangLit -> Int
fromEnumL (LitInt x) = fromEnum x
fromEnumL (LitChar x) = fromEnum x
fromEnumL (LitFloat x) = fromEnum x
fromEnumL _ = error "fromEnumL: non-enumerable type"


-- | Convert a list or range into a literal list.
iterToLit :: LangLit -> ExecIO LangLit
iterToLit x@(LitRange{}) | isInfiniteRange x = throwExecError infiniteRangeErr
iterToLit x = liftM LitList $ fromIter x


iterFromThenTo :: LangLit -> LangLit -> LangLit -> ExecIO [LangLit]
iterFromThenTo (LitInt x) (LitInt y) (LitInt z) = return $ map LitInt $ enumFromThenTo x z y -- (succ z) y
iterFromThenTo (LitFloat x) (LitFloat y) (LitFloat z) = return $ map LitFloat $ enumFromThenTo x z y -- (succ z) y
iterFromThenTo (LitChar x) (LitChar y) (LitChar z) = return $ map LitChar $ enumFromThenTo x z y -- (succ z) y
iterFromThenTo _ _ _ = throwImplementationErr "iterFromThenTo: cannot handle non-enumerable types"


-- Potential: looping to infinity (and beyond)
iterFrom :: LangLit -> ExecIO [LangLit]
iterFrom (LitInt x) = return $ map LitInt $ enumFrom x
iterFrom (LitChar x) = return $ map LitChar $ enumFrom x
iterFrom (LitFloat x) = return $ map LitFloat $ enumFrom x
iterFrom _ = throwImplementationErr "iterFrom: define failure"


iterFromThen :: LangLit -> LangLit -> ExecIO [LangLit]
iterFromThen (LitInt x) (LitInt y) = return $ map LitInt $ enumFromThen x y
iterFromThen (LitChar x) (LitChar y) = return $ map LitChar $ enumFromThen x y
iterFromThen (LitFloat x) (LitFloat y) = return $ map LitFloat $ enumFromThen x y
iterFromThen _ _ = throwImplementationErr "iterFromThen: define failure"



iterFromTo :: LangLit -> LangLit -> ExecIO [LangLit]
iterFromTo (LitInt x) (LitInt y) = return $ map LitInt $ enumFromTo x y
iterFromTo (LitChar x) (LitChar y) = return $ map LitChar $ enumFromTo x y
iterFromTo (LitFloat x) (LitFloat y) = return $ map LitFloat $ enumFromTo x y
iterFromTo _ _ = throwImplementationErr "iterFromTo: cannot handle non-enumerable types"

