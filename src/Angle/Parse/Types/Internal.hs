{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
module Angle.Parse.Types.Internal
    ( ExecIO
    , runExecIOBasic
    , runExecIOEnv
    , Env(..)
    , basicEnv
    , returnVal
    , getEnvValue
    , fromIter
    , iterToLit
    ) where


import Control.Applicative
import Control.Monad.Error
import Control.Monad.Trans.Except
import Control.Monad.State

import Angle.Parse.Error
import Angle.Parse.Scope
import Angle.Types.Lang


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


data Env = Env { currentScope :: Scope
               , sourceText :: String
               , envSourceRef :: SourceRef
               , envSynRep :: String
               , envValue :: LangLit
               } deriving (Show, Eq)


runExecIOEnv :: Env -> ExecIO a -> IO (Either AngleError a)
runExecIOEnv e x = evalStateT (runExceptT $ runExecIO x) e


runExecIOBasic :: ExecIO a -> IO (Either AngleError a)
runExecIOBasic = runExecIOEnv basicEnv


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

returnVal :: LangLit -> ExecIO LangLit
returnVal v = putEnvValue v >> return v


getEnvValue :: ExecIO LangLit
getEnvValue = liftM envValue get


putEnvValue :: LangLit -> ExecIO ()
putEnvValue v = modify (\e -> e {envValue=v})


fromIter :: LangLit -> ExecIO [LangLit]
fromIter (LitList xs) = return xs
fromIter (LitStr xs) = return $ map LitChar xs
fromIter (LitRange x Nothing Nothing) = iterFrom x
fromIter (LitRange x (Just y) Nothing) = iterFromTo x y
fromIter (LitRange x (Just y) (Just z)) = iterFromThenTo x y z
fromIter _ = throwImplementationErr "fromIter: TODO: define this!"


iterToLit :: LangLit -> ExecIO LangLit
iterToLit = liftM LitList . fromIter


iterFromThenTo :: LangLit -> LangLit -> LangLit -> ExecIO [LangLit]
iterFromThenTo (LitInt x) (LitInt y) (LitInt z) = return $ map LitInt $ enumFromThenTo x (succ z) y
iterFromThenTo (LitFloat x) (LitFloat y) (LitFloat z) = return $ map LitFloat $ enumFromThenTo x (succ z) y
iterFromThenTo (LitChar x) (LitChar y) (LitChar z) = return $ map LitChar $ enumFromThenTo x (succ z) y
iterFromThenTo _ _ _ = throwImplementationErr "iterFromThenTo: define failure"

iterFrom :: LangLit -> ExecIO [LangLit]
iterFrom (LitInt x) = return $ map LitInt $ enumFrom x
iterFrom (LitChar x) = return $ map LitChar $ enumFrom x
iterFrom (LitFloat x) = return $ map LitFloat $ enumFrom x
iterFrom _ = throwImplementationErr "iterFrom: define failure"


iterFromTo :: LangLit -> LangLit -> ExecIO [LangLit]
iterFromTo (LitInt x) (LitInt y) = return $ map LitInt $ enumFromTo x y
iterFromTo (LitChar x) (LitChar y) = return $ map LitChar $ enumFromTo x y
iterFromTo (LitFloat x) (LitFloat y) = return $ map LitFloat $ enumFromTo x y
iterFromTo _ _ = throwImplementationErr "iterFromTo: define failure"

