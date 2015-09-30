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
    , Env(..)
    , basicEnv
    , returnVal
    , getEnvValue
    , fromIter
    , iterToLit
    , fromEnumL
    , isInfiniteRange
    ) where


import Control.Applicative
import Control.Monad.Except
import Control.Monad.Trans.Except
import Control.Monad.State

import Angle.Exec.Error
import Angle.Exec.Scope
import Angle.Types.Lang


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


instance CanError ExecIO where
    throwAE = ExecIO . throwE
    catchAE (ExecIO e) h = ExecIO (lift $ runExceptT e) >>= either h return

instance MonadState Env ExecIO where
    get = ExecIO $ lift get
    put x = ExecIO $ lift $ put x


-- | Environment in which Angle programs execute.
data Env = Env { currentScope :: Scope
               , sourceText :: String
               , envSourceRef :: SourceRef
               , envSynRep :: String
               , envValue :: LangLit
               } deriving (Show, Eq)


-- | Run Angle program with a custom environment.
runExecIOEnv :: Env -> ExecIO a -> IO (Either AngleError a)
runExecIOEnv e x = evalStateT (runExceptT $ runExecIO x) e


-- | Run Angle program with a basic environment.
runExecIOBasic :: ExecIO a -> IO (Either AngleError a)
runExecIOBasic = runExecIOEnv basicEnv


-- | Basic environment.
basicEnv :: Env
basicEnv = Env { currentScope = emptyScope
               , sourceText = ""
               , envSourceRef = startRef
               , envSynRep = ""
               , envValue = LitNull
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


-- | Convert a list or range into a list of literals.
fromIter :: LangLit -> ExecIO [LangLit]
fromIter (LitList xs) = return xs
fromIter (LitStr xs) = return $ map LitChar xs
-- fromIter (LitRange x Nothing Nothing) =
fromIter (LitRange x (Just y) Nothing) = iterFromTo x y
fromIter (LitRange x (Just y) (Just z)) = iterFromThenTo x y z
fromIter _ = throwImplementationErr "fromIter: TODO: define this!"


-- | True if the range has infinite size.
isInfiniteRange :: LangLit -> Bool
isInfiniteRange (LitRange _ Nothing _) = True
isInfiniteRange (LitRange x (Just y) (Just z))
    = (y' > x' && z' - x' < 0) || (y' < x' && z' - x' > 0) || (z' - x') == 0
  where [x',y',z'] = map fromEnumL [x,y,z]
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
iterToLit x@(LitRange{}) | isInfiniteRange x = return LitNull
iterToLit x = liftM LitList $ fromIter x


iterFromThenTo :: LangLit -> LangLit -> LangLit -> ExecIO [LangLit]
iterFromThenTo (LitInt x) (LitInt y) (LitInt z) = return $ map LitInt $ enumFromThenTo x (succ z) y
iterFromThenTo (LitFloat x) (LitFloat y) (LitFloat z) = return $ map LitFloat $ enumFromThenTo x (succ z) y
iterFromThenTo (LitChar x) (LitChar y) (LitChar z) = return $ map LitChar $ enumFromThenTo x (succ z) y
iterFromThenTo _ _ _ = throwImplementationErr "iterFromThenTo: define failure"


-- Potential: looping to infinity (and beyond)
-- iterFrom :: LangLit -> ExecIO [LangLit]
-- iterFrom (LitInt x) = return $ map LitInt $ enumFrom x
-- iterFrom (LitChar x) = return $ map LitChar $ enumFrom x
-- iterFrom (LitFloat x) = return $ map LitFloat $ enumFrom x
-- iterFrom _ = throwImplementationErr "iterFrom: define failure"


iterFromTo :: LangLit -> LangLit -> ExecIO [LangLit]
iterFromTo (LitInt x) (LitInt y) = return $ map LitInt $ enumFromTo x y
iterFromTo (LitChar x) (LitChar y) = return $ map LitChar $ enumFromTo x y
iterFromTo (LitFloat x) (LitFloat y) = return $ map LitFloat $ enumFromTo x y
iterFromTo _ _ = throwImplementationErr "iterFromTo: define failure"

