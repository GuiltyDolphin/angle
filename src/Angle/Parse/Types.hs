{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
module Angle.Parse.Types
    ( ExecIO
    , runExecIO
    , runExecIOBasic
    , runExecIOEnv
    , Env(..)
    , basicEnv
    , returnVal
    , getEnvValue
    , putEnvValue
    , fromIter
    , iterToLit
    , iterFrom
    , iterFromTo
    , iterFromThenTo
    ) where


import Control.Applicative
import Control.Monad.Error
import Control.Monad.Trans.Except
import Control.Monad.State

import Angle.Parse.Error
import Angle.Parse.Scope
import Angle.Types.Lang


-- newtype ExecIO a = ExecIO
    -- { runEIO :: ExceptT AngleError
                -- (StateT Env IO) a
    -- }
-- newtype ExecIO a = ExecIO
--     { runEIO :: ExceptT AngleError (StateT Env IO) a }
--     -- { runEIO :: StateT Env (ExceptT AngleError IO) a }
--     deriving ( Functor, Applicative, Monad
--              , MonadState Env
--              , MonadIO)

newtype ExecEnv a = ExecEnv { runExecEnv :: StateT Env IO a }
    deriving (Functor, Applicative, Monad
             , MonadState Env, MonadIO)

newtype ExecIO a = ExecIO
    { runExecIO :: ExceptT AngleError (StateT Env IO) a }
    deriving ( Functor, Applicative, Monad
             , MonadIO)


-- type ExecIO = ExceptT AngleError ExecEnv


-- runExecIO :: ExecIO a -> StateT Env IO (Either AngleError a)
-- runExecIO = runExecEnv . runExceptT


-- instance MonadError AngleError ExecIO where
--     throwError = throwE
--     catchError = catchE

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


-- instance MonadState Env ExecIO where
--     get = lift get
--     put x = lift (put x)


data Env = Env { currentScope :: Scope
               , sourceText :: String
               , envSourceRef :: SourceRef
               , envSynRep :: String
               , envValue :: LangLit
               } deriving (Show, Eq)

-- Stack API
-- * Stacks have types:
--   - Global, Function and Loop
--   - Only one Global stack, the rest are created
--     as needed.
--   - stackType :: Stack -> StackType
-- * Stacks know their parents
--   - parentStack :: Stack -> Maybe Stack
--   - only the Global stack should return Nothing.
-- * Stacks can have names
--   - stackName :: Stack -> Maybe String
--   - useful for keeping track of function calls.
-- * Stacks have levels (not sure about this one)
--   - stackLevel :: Stack -> Int
--   - for error reporting, allows stack depth to be
--     known.

-- data Stack = Stack
--     { stackParent :: Maybe Stack
--     , stackStmts :: ([Stmt], [Stmt])
--     , stackLevel :: Int
--     , stackType :: StackType
--     , stackName :: Maybe String
--     } deriving (Show, Eq)
--
--
-- data StackType = FunctionStack | LoopStack | GlobalStack
--                  deriving (Show, Eq)
--
--
-- newStack :: Stack -> StackType -> Maybe String -> Stack
-- newStack stack typ name =
--     let newStmts =
--             case nextStmt stack of
--               Nothing -> error "newStack: parent has no nextStmt"
--               Just x -> case x of
--                           s@(SingleStmt{}) -> [s]
--                           (MultiStmt xs) -> xs
--     in Stack
--            { stackParent = Just stack
--            , stackType = typ
--            , stackName = name
--            , stackLevel = stackLevel stack + 1
--            , stackStmts = ([], newStmts) }
--
--
-- -- | Update the current stack, retrieving the next
-- -- statement to be executed.
-- forwardStmt :: ExecIO (Maybe Stmt)
-- forwardStmt = do
--   stack <- liftM envStack get
--   let stackS = stackStmts stack
--       oldStmts = fst stackS
--       newStmts = snd stackS
--       nextStmt' = take 1 newStmts
--       nextS = case nextStmt' of
--                 [] -> Nothing
--                 [x] -> Just x
--       newStackStmts = (oldStmts ++ nextStmt', drop 1 newStmts)
--   modifyStack (\s -> s {stackStmts = newStackStmts})
--   return nextS
--
--
-- -- | Modify the current stack.
-- modifyStack :: (Stack -> Stack) -> ExecIO ()
-- modifyStack f = modify (\e -> e {envStack = f $ envStack e})
--
--
-- -- | Retrieve the next statement to be executed, if any.
-- nextStmt :: Stack -> Maybe Stmt
-- nextStmt (Stack { stackStmts = (_,ys) })
--     = case take 1 ys of
--         [] -> Nothing
--         [x] -> Just x
--
--
-- -- execStack :: ExecIO LangLit
-- -- execStack = do
-- --   stmt <- forwardStmt
-- --   case stmt of
-- --     Nothing -> error "no more statements!"
-- --     Just x -> execStmt x
--
--
-- localStackWithType :: Stack -> StackType -> Maybe Stack
-- localStackWithType s@(Stack { stackType = sTyp }) typ
--     = if sTyp == typ
--       then Just s
--       else case stackParent s of
--              Nothing -> Nothing
--              Just par -> localStackWithType par typ
--
--
-- getStack :: ExecIO Stack
-- getStack = liftM envStack get
--
--
-- popStack :: ExecIO ()
-- popStack = do
--   stack <- getStack
--   case stackParent stack of
--     Nothing -> throwImplementationErr "popStack: at top stack!"
--     Just x  -> modifyStack (const x)


-- runExecIOEnv :: Env -> ExecIO a -> IO (Either AngleError a)
-- runExecIOEnv e x = evalStateT (runExecIO x) e
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


data Stack = Stack
    { stackPointer :: StackFrame
    , stackTop :: StackFrame
    , stackStack :: [StackFrame]
    } deriving (Show, Eq)

data StackFrame = StackFrame
    { frameName :: String
    } deriving (Show, Eq)


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
iterFromThenTo (LitInt x) (LitInt y) (LitInt z) = return $ map LitInt $ enumFromThenTo x z y
iterFromThenTo (LitFloat x) (LitFloat y) (LitFloat z) = return $ map LitFloat $ enumFromThenTo x z y
iterFromThenTo (LitChar x) (LitChar y) (LitChar z) = return $ map LitChar $ enumFromThenTo x z y
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

