{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Angle.Parse.Exec
    ( runExecIOBasic
    , runExecIOEnv
    , basicEnv
    , execStmt
    , ExecIO
    , Env(..)
    ) where

import Control.Applicative
import Control.Monad
import Control.Monad.Error
import Control.Monad.State
import Data.Maybe (fromJust, fromMaybe)
    
import Debug.Trace (trace)

import Angle.Parse.Error
import Angle.Parse.Operations    
import Angle.Parse.Scope
import Angle.Parse.Var
import Angle.Types.Lang
    
-- TODO:
-- - errors:
--    add stack trace?
--    so, calling functions add to stack, maybe as
--    well as operators.


-- BUGS:
-- - variable not defined in function
--   e.g
--   defun foo(x) {
--     if (== [] x) then return x;
--     bar = print(x);
--   }
--   will result in error: x is not defined (in print)
--   * Appears to be due to leaving scope early.
--     e.g. defun foo(x) {print(x);print(x);}
--     prints inner x, then outer x.


-- Exec API
-- - Handling both IO and regular code
--    Have these separated? Could cause issues?
--    But might be unneccessary to have all in IO
-- - Environment in which to run (options etc?) - either
--    State, Reader or both
-- - ErrorT for error handling

-- assignVarVal :: Ident -> VarVal -> Exec LangLit
-- assignVarVal name val = do
-- TODO/NOTES
-- - Function assign (other than defun)


-- *****************************************
-- * Env - A temporary test of environment *
-- *****************************************
data Env = Env { currentScope :: Scope
               , envOptions :: OptionSet
               , sourceText :: String
               , envSourceRef :: SourceRef
               , envSynRep :: String
               , envStack :: Stack
               } deriving (Show, Eq)


-- TODO:
-- Stacks
-- * For keeping track of statements:
--  - Maybe have nested (i.e multi) statements just execute
--    in same stack, only having to push a new stack
--    when entering a function or loop.
-- * Moving up a stack should FORCE the execution to continue
--   in the new stack, there is no point in the stacks
--   if they cannot track statements.

-- * global (initial) stack is passed a multistatement to
--   start the execution process.
--   - the global stack will create new child stacks
--     as required.
--   - but as using a multistatement as a means of wrapping
--     many single statements, perhaps the stacks should
--     work with lists of statements instead?
-- * stacks could also be useful in reporting errors, as the
--   stacks could track their statement position and any
--   function calls / loops that they enter.


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


data Stack = Stack { parentStack :: Maybe Stack
                   , stackLevel :: Int
                   , stackName :: Maybe String
                   , stackType :: StackType
                   } deriving (Show, Eq)


data StackType = FunctionStack | LoopStack | GlobalStack
                 deriving (Show, Eq)


startStack :: Stack
startStack = newStack (Just "global stack") Nothing GlobalStack


newStack :: Maybe String -> Maybe Stack -> StackType -> Stack
newStack name stack t = 
    Stack
    { parentStack = stack
    , stackLevel = maybe 0 ((+1) . stackLevel) stack
    , stackName = name
    , stackType = t
    } 


parentStackWithType :: Stack -> StackType -> Maybe Stack
parentStackWithType stack typ = 
    case parentStack stack of
      Nothing -> Nothing
      Just par -> if stackType par == typ
                  then Just par
                  else parentStackWithType par typ


modifyStack :: (Stack -> Stack) -> ExecIO ()
modifyStack f = modify (\e -> e { envStack = f $ envStack e })
                

upStack :: ExecIO ()
upStack = do
  currStack <- liftM envStack get 
  case parentStack currStack of
    Nothing -> return ()
    Just x -> (return $! traceShowMsg "upStack - entering stack: " x) >> modifyStack (const x)
             

                                                     
updatePos :: SourceRef -> ExecIO ()
updatePos pos = modify (\e -> e { envSourceRef = pos })
        
execFunctionStack :: Stmt -> ExecIO LangLit
execFunctionStack s@(SingleStmt _ _) = withLeavingStack $ execStmt s
execFunctionStack (MultiStmt []) = upScope >> upStack >> return LitNull
execFunctionStack s@(MultiStmt _) = withLeavingStack $ execStmt s


withLeavingStack :: ExecIO a -> ExecIO a
withLeavingStack ex = do
  oldStack <- liftM envStack get
  r <- ex
  newStack' <- liftM envStack get
  if oldStack == newStack'
      then upStack >> return r
      else return r
           

-- | Execute in a new function stack, making sure
-- to preserve scope and stack when leaving the function.
withFunctionStack :: Maybe String -> ExecIO a -> ExecIO a
withFunctionStack name ex = do
  newFunctionStack name 
  oldStack <- liftM envStack get
  r <- ex
  newStack' <- liftM envStack get
  if oldStack == newStack'
      then upStack *> upScope *> return r
      else return r
  

withTemporaryStack 
    :: ExecIO a  -- ^ Wrapped execution
    -> StackType 
    -> Maybe String 
    -> ExecIO a
withTemporaryStack ex typ name = do
  downStack name typ
  withLeavingStack ex
                   

withLoopStack :: ExecIO a -> ExecIO a
withLoopStack ex = withTemporaryStack ex LoopStack Nothing
                   

upStackReturn :: ExecIO ()
upStackReturn = upStackType FunctionStack
              

upStackType :: StackType -> ExecIO ()
upStackType typ = do
  currStack <- liftM envStack get
  let outStack = innerStackWithType currStack typ
  case outStack of
    Nothing -> error $ "upStackType: no stacks of type " ++ show typ
    Just p -> modifyStack (const p) >> upStack
              

innerStackWithType :: Stack -> StackType -> Maybe Stack
innerStackWithType stack typ = if stackType stack == typ
                           then Just stack
                           else parentStackWithType stack typ


upStackLoop :: ExecIO ()
upStackLoop = upStackType LoopStack
              

-- | Execute the statement in a new stack.
downStack :: Maybe String -> StackType -> ExecIO ()
downStack name typ = do
  currStack <- liftM envStack get
  return $! traceShowMsg "downStack - entering new stack: " (newStack name (Just currStack) typ)
  modifyStack (const $ newStack name (Just currStack) typ)
              

newFunctionStack :: Maybe String -> ExecIO ()
newFunctionStack name = downStack name FunctionStack
                             

newLoopStack :: Maybe String -> ExecIO ()
newLoopStack name = downStack name LoopStack


setEnvSynRep :: String -> ExecIO ()
setEnvSynRep x = modify (\e -> e { envSynRep = x })
         

basicEnv :: Env
basicEnv = Env { currentScope = emptyScope
               , envOptions = defaultOptions
               , sourceText = ""
               , envSourceRef = startRef
               , envSynRep = ""
               , envStack = startStack
               }
         

data OptionSet = OS { printName :: Bool }
                 deriving (Show, Eq)
                          

defaultOptions :: OptionSet
defaultOptions = OS { printName = False }

               
-- | Create a new scope with the current scope as its
-- parent.
newScope :: ExecIO ()
newScope = do
  env <- get
  modifyScope (\s -> emptyScope {outerScope=Just s})
  let oldScope = currentScope env
      newScope' = emptyScope { outerScope = Just oldScope }
  put env { currentScope = newScope' }


lookupVar :: LangIdent -> ExecIO (Maybe VarVal)
lookupVar name = do
  env <- get
  let res = resolve name (currentScope env)
  return res
         

lookupVarLit :: LangIdent -> ExecIO (Maybe LangLit)
lookupVarLit name = do
  res <- lookupVar name
  case res of 
    Nothing -> return Nothing
    Just x -> return $ varLitDef x
  

lookupVarLitF :: LangIdent -> ExecIO LangLit
lookupVarLitF name = do
  res <- lookupVarLit name
  case res of
    Nothing -> throwLangError $ nameNotValueErr name
    Just x -> return x
              

lookupVarLambda :: LangIdent -> ExecIO (Maybe CallSig)
lookupVarLambda name = do
  res <- lookupVar name
  case res of 
    Nothing -> return Nothing
    Just x -> return $ varFunDef x


lookupVarLambdaF :: LangIdent -> ExecIO CallSig
lookupVarLambdaF name = do
  res <- lookupVarLambda name
  case res of
    Nothing -> throwLangError $ nameNotFunctionErr name
    Just x -> return x

         
lookupVarF :: LangIdent -> ExecIO VarVal
lookupVarF name = do
  res <- lookupVar name
  case res of
    Just x -> return x
    Nothing -> throwLangError $ nameNotDefinedErr name
               

lookupOp :: LangIdent -> ExecIO (Maybe CallSig)
lookupOp opName = do
  res <- lookupVar opName
  case res of
    Nothing -> return Nothing
    Just x -> return $ varFunDef x
              

lookupOpF :: LangIdent -> ExecIO CallSig
lookupOpF opName = do
  res <- lookupOp opName
  case res of
    Nothing -> throwLangError $ nameNotOpErr opName
    Just x -> return x
  

-- | Modify the current scope using the given function.
modifyScope :: (Scope -> Scope) -> ExecIO ()
modifyScope f = do
  env <- get
  let oldScope = currentScope env
      newScope' = f oldScope
  put env {currentScope=newScope'}


assignVarLit :: LangIdent -> LangLit -> ExecIO LangLit
assignVarLit name (LitLambda x) = assignVarLambda name x >> return LitNull
assignVarLit name val = do
  current <- lookupVarCurrentScope name
  modifyScope $ flip (setVarInScope name $ setVarLit
                      (fromMaybe emptyVar current)
                      val) True
  return val
         

lookupVarCurrentScope :: LangIdent -> ExecIO (Maybe VarVal)
lookupVarCurrentScope name = do
  currScope <- liftM currentScope get
  if name `isDefinedIn` currScope
    then return $ resolve name currScope
    else return Nothing
         

assignVar :: LangIdent -> VarVal -> ExecIO ()
assignVar name val = modifyScope $ flip (setVarInScope name val) True
                     


assignVarLambda :: LangIdent -> CallSig -> ExecIO ()
assignVarLambda name val = do
  current <- lookupVarCurrentScope name
  modifyScope $ flip (setVarInScope name $ setVarFun 
                      (fromMaybe emptyVar current) val) True


infix 4 |=
(|=) :: LangIdent -> LangLit -> ExecIO LangLit
(|=) = assignVarLit


-- TODO: Should this just stay in the current scope if there
-- is no parent scope?
-- | Changes the current scope to the parent scope.
upScope :: ExecIO ()
upScope = do
  currScope <- liftM currentScope get
  case outerScope currScope of
    Nothing -> return ()
    Just x -> modifyScope (const x)
  
          
argListBind :: [Expr] -> CallSig -> ExecIO ()
argListBind args cs = do
  let params = callArgs cs
      la = length args
      lp = length (stdArgs params)
  when (la > lp && not (hasCatchAllArg params) || la < lp)
           (throwLangError $ wrongNumberOfArgumentsErr lp la)
  vals <- mapM execExpr args
  let toBind = zip (stdArgs params) vals
      fullBind = toBind ++ [(fromJust $ catchAllArg params, LitList $ drop (length toBind) vals) | hasCatchAllArg params]
                 -- if length toBind /= la
                 -- then [(fromJust $ catchAllArg params, LitList $ drop (length toBind) vals)]
                 -- else [(fromJust $ catchAllArg params, LitList []) | hasCatchAllArg params] 
  newScope
  forM_ fullBind (uncurry assignVarLit)
                         

execExpr :: Expr -> ExecIO LangLit
execExpr (ExprLit x) = return x
execExpr (ExprIdent x) = lookupVarLitF x
execExpr (ExprFunIdent x) = liftM LitLambda $ lookupVarLambdaF x
execExpr (ExprOp x) = withErrHandle (execOp x)
execExpr (ExprFunCall name args) = execFunCall name args
execExpr (ExprList xs) = liftM LitList $ mapM execExpr xs
execExpr (ExprLambda x) = return (LitLambda x)
                                   

execFunCall :: LangIdent -> [Expr] -> ExecIO LangLit
execFunCall = callFun
                      

execOp :: LangOp -> ExecIO LangLit
execOp (SpecOp op expr) = execSpecOp op expr
execOp (MultiOp op exprs) = execMultiOp op exprs


execSpecOp :: Op -> Expr -> ExecIO LangLit
execSpecOp OpNeg x = execExpr x >>= notLit
execSpecOp x _ = error $ "execSpecOp - not a SpecOp: " ++ show x


execMultiOp :: Op -> [Expr] -> ExecIO LangLit
execMultiOp OpAdd xs       = withMultiOp xs addLit
execMultiOp OpAnd xs       = withMultiOp xs andLit
execMultiOp OpDiv xs       = withMultiOp xs divLit
execMultiOp OpEq  xs       = withMultiOp xs eqLit
execMultiOp OpGreater xs   = withMultiOp xs greaterLit
execMultiOp OpGreaterEq xs = withMultiOp xs greaterEqLit
execMultiOp OpLess xs      = withMultiOp xs lessLit
execMultiOp OpLessEq xs    = withMultiOp xs lessEqLit
execMultiOp OpMult xs      = withMultiOp xs multLit
execMultiOp OpOr xs        = withMultiOp xs orLit
execMultiOp OpSub xs       = withMultiOp xs subLit
execMultiOp (UserOp x) xs = do
  sig <- lookupOpF x
  callFunCallSig sig xs
execMultiOp x _ = error $ "execMultiOp - not a multiOp: " ++ show x
  
         
withMultiOp :: [Expr] -> ([LangLit] -> ExecIO LangLit) -> ExecIO LangLit
withMultiOp xs f = mapM execExpr xs >>= f 
  -- liftIO $ addLit (map execExpr xs)
                       

newtype ExecIO a = ExecIO 
    { runEIO :: ErrorT LError (StateT Env IO) a }
    deriving (Functor, Applicative, Monad
             , MonadState Env, MonadError LError
             , CanError, MonadIO)

              
runExecIOBasic :: ExecIO a -> IO (Either LError a)
runExecIOBasic = runExecIOEnv basicEnv


runExecIOEnv :: Env -> ExecIO a -> IO (Either LError a)
runExecIOEnv e x = evalStateT (runErrorT (runEIO x)) e
                   

toLitStr :: LangLit -> LangLit
toLitStr (LitInt x) = LitStr (show x)
toLitStr (LitFloat x) = LitStr (show x)
toLitStr (LitBool x) = LitStr (show x)
toLitStr x@(LitStr _) = x
toLitStr (LitList xs) = LitStr (show xs)
toLitStr x@(LitRange _ _) = LitStr $ showSyn x
toLitStr LitNull = LitStr ""
                   

callBuiltin :: LangIdent -> [Expr] -> ExecIO LangLit 
callBuiltin (LangIdent "print") xs = mapM execExpr xs >>= builtinPrint
callBuiltin (LangIdent "str")   xs = mapM execExpr xs >>= builtinStr
callBuiltin (LangIdent "index") xs = mapM execExpr xs >>= builtinIndex
callBuiltin (LangIdent x) _ = error $ "callBuiltin - not a builtin function: " ++ x
         

builtinPrint :: [LangLit] -> ExecIO LangLit
builtinPrint xs = liftIO $ putStrLn res >> return (LitStr res)
                  where res = concatMap showSyn xs
                              

-- | Implementation of the built-in str function.
builtinStr :: [LangLit] -> ExecIO LangLit
builtinStr [] = return $ LitStr ""
builtinStr xs | length xs > 1 = throwLangError $ wrongNumberOfArgumentsErr 1 (length xs)
              | otherwise = return $ toLitStr (head xs)
                            
                       
-- TODO:
-- - Currently wraps back round with negatives
--   e.g. index(-5,-1,[1,2,3]); -> [2, 3]     
-- - should probably work more like Pyhon's indexing system.

-- | Builtin index function.
--
-- index(int:x, list:xs): retrieve element at index @x@ from @xs@
--
-- index(int:x, int:y, list:xs): return a list of elements that lie between index @x@ and index@y@ of @xs@.
builtinIndex :: [LangLit] -> ExecIO LangLit
builtinIndex [LitInt x,LitList xs]
    | x >= length xs = throwLangError $ indexOutOfBoundsErr x
    | x < 0 = return $ xs !! (length xs + x)
    | otherwise = return $ xs !! x
builtinIndex [LitInt x,LitInt y,LitList xs] 
    | x >= length xs || y > length xs 
        = throwLangError $ indexOutOfBoundsErr x
    | x < 0 = builtinIndex 
              [LitInt (length xs + x), LitInt y, LitList xs]
    | y < 0 = builtinIndex 
              [LitInt x, LitInt (length xs + y), LitList xs]
    | otherwise 
        = return . LitList $ splice x y xs
builtinIndex _ = throwLangError $ callBuiltinErr "index: invalid call signature"


splice :: Int -> Int -> [a] -> [a]
splice x y xs = take (1+y-x) $ drop x xs


-- | True if the identifier represents a builtin function.
isBuiltin :: LangIdent -> Bool
isBuiltin = (`elem`builtins) . getIdent
    where builtins = ["print", "str", "index"]


callFun :: LangIdent -> [Expr] -> ExecIO LangLit
callFun x args | isBuiltin x = callBuiltin x args
               | otherwise = do
  callsig <- lookupVarLambdaF x
  withFunctionStack (Just . getIdent $ x) $ callFunCallSig callsig args

-- do
  --callsig <- lookupVarLambdaF x
  --argListBind args callsig
  --execStmt (callBody callsig)
           

callFunCallSig :: CallSig -> [Expr] -> ExecIO LangLit
callFunCallSig callsig args = do
  argListBind args callsig
  execStmt (callBody callsig)
           

-- | @withScope ex@ runs @ex@ but will ensure that
-- the initial scope is retained after @ex@ is executed.
withScope :: ExecIO a -> ExecIO a
withScope ex = do
  currScope <- liftM currentScope get
  res <- ex
  currScope' <- liftM currentScope get
  if currScope' == currScope
     then return res
     else modifyScope (const currScope) >> return res
                               

execStmt :: Stmt -> ExecIO LangLit
-- execStmt (SingleStmt x@(StmtReturn _) pos)
--     = modify (\s -> s { envSourceRef = pos })
--       >> execSingStmt x
execStmt (SingleStmt x pos) = updatePos pos >> execSingStmt x
execStmt (MultiStmt (x@(SingleStmt (StmtReturn _) _):_)) = execStmt x
execStmt (MultiStmt []) = return LitNull
execStmt (MultiStmt [x]) = execStmt x
execStmt (MultiStmt (x:xs)) = execStmt x >> execStmt (MultiStmt xs)
                              
-- (MultiStmt (x:xs)) -> execStmt x >> execStmt (MultiStmt xs)
--                       (a -> m b)    (a -> m b)
                          

-- { foo(x);
--   bar(y);
--   baz(z);
-- }
                          

execSingStmt :: SingStmt -> ExecIO LangLit
execSingStmt (StmtAssign name e) = execExpr e >>= assignVarLit name
execSingStmt (StmtStruct x) = execLangStruct x
execSingStmt (StmtExpr x) = setEnvSynRep (showSyn x) >> execExpr x
execSingStmt (StmtComment _) = return LitNull
execSingStmt (StmtReturn x) = do
  isGlob <- liftM (isOutermostScope . currentScope) get
  if isGlob
      then throwLangError returnFromGlobalErr
      else execExpr x <* upScope <* upStackReturn


traceShowMsg :: (Show a) => String -> a -> a
traceShowMsg msg x = trace (msg ++ show x) x
                    
-- TODO: Fix return statement
-- - options
-- - add break/return (boolean?) information to state? then
--   would need to break evaluation early if they are
--   True, then set them to false.

-- Possible:
-- entering loop construct
-- -> current loop, next statement
-- if break, then execute next statement
-- entering function/new scope:
-- -> current function, next statement
-- if return, then execute next statement
-- then reset (maybe Maybe values?)
-- would need to account for loops in loops
-- and functions in functions
                            

execLangStruct :: LangStruct -> ExecIO LangLit
execLangStruct (StructFor name e s) = execStructFor name e s
execLangStruct (StructWhile e s) = execStructWhile e s
execLangStruct (StructIf if' thn els) = execStructIf if' thn els
execLangStruct (StructDefun name cs) = assignVarLambda name cs *> return LitNull
                                        

execStructIf :: Expr -> Stmt -> Maybe Stmt -> ExecIO LangLit
execStructIf if' thn els = do
  p <- execExpr if'
  case p of
    (LitBool True) -> execStmt thn
    (LitBool False) -> case els of 
                         Nothing -> return LitNull
                         Just s  -> execStmt s
    x -> throwLangError $ typeUnexpectedErr (typeOf x) LTBool
                    


toIter :: LangLit -> ExecIO [LangLit]                        
toIter (LitList xs) = return xs
toIter _ = throwLangError $ defaultErr "toIter: TODO: define this!"


execStructFor :: LangIdent -> Expr -> Stmt -> ExecIO LangLit
execStructFor name e s = do
  iterable <- execExpr e >>= toIter
  outScope <- liftM currentScope get
  res <- forM iterable (\v -> do
                   assignVarLit name v
                   execStmt s)
  newS <- liftM currentScope get
  let newS' = deleteFromScope name newS
  modifyScope (const $ mergeScope newS' outScope)
  return (LitList res)
  -- forM iterable (\v -> assignVarLit name v $ do
  --                        execStmt s)

 

deleteVar :: LangIdent -> ExecIO () 
deleteVar name = modifyScope (deleteFromScope name)

execStructWhile :: Expr -> Stmt -> ExecIO LangLit
execStructWhile = undefined
                  

instance CanErrorWithPos ExecIO where
    getErrorPos = liftM envSourceRef get
    getErrorText = liftM envSynRep get
    getErrorSource = liftM sourceText get
                     
-- | Attempts to run the given ExecIO as usual,
-- but if an error occurs, will update the information
-- of the error and throw a new one.
withErrHandle :: ExecIO a -> ExecIO a
withErrHandle e = e `catchError` (\(LError {errorErr=lerr})
                                    -> throwLangError lerr)
    
                  

-- Builtins...                    

-- Exec Requirements
-- - Errors
--   - Custom datatype for Errors
--   - Use ErrorT to add error handling
-- - State
--   - Current scope
--   - Current settings



-- TODO:
--  eval and exec
--  - eval for results without side-effects?
--  - exec for results with side-effects?

-- execRun :: [SourceRef] -> ExecIO ()
-- execRun [] = return ()
-- execRun xs = do
--   forM_ xs 
--             (\(x,pos) -> 
--              execStmt x `catchError` (\e -> do
--                                        source <- liftM sourceText get
--                                        throwError e { errorPos=pos
--                                                     , errorSource=source
--                                                     }))
  
  
