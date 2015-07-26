{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Angle.Parse.Exec
    ( runExecIOBasic
    , runExecIOEnv
    , execStmt
    , ExecIO
    , Env(..)
    ) where

import Control.Applicative
import Control.Monad
import Control.Monad.Error
import Control.Monad.State
import Data.Maybe (fromJust)

import Angle.Parse.Error
import Angle.Parse.Operations    
import Angle.Parse.Scope
import Angle.Parse.Var
import Angle.Types.Lang

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
               } deriving (Show)
         
setEnvSynRep :: String -> ExecIO ()
setEnvSynRep x = modify (\e -> e { envSynRep = x })
         
basicEnv :: Env
basicEnv = Env { currentScope = emptyScope
               , envOptions = defaultOptions
               , sourceText = ""
               , envSourceRef = startRef
               , envSynRep = ""
               }
         
data OptionSet = OS { printName :: Bool }
                 deriving (Show, Eq)
                          

defaultOptions :: OptionSet
defaultOptions = OS { printName = True }

               
-- | Create a new scope with the current scope as its
-- parent.
newScope :: ExecIO ()
newScope = do
  env <- get
  modifyScope (\s -> emptyScope {outerScope=Just s})
  let oldScope = currentScope env
      newScope = emptyScope { outerScope = Just oldScope }
  put env { currentScope = newScope }

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
              
lookupVarFun :: LangIdent -> ExecIO (Maybe CallSig)
lookupVarFun name = do
  res <- lookupVar name
  case res of 
    Nothing -> return Nothing
    Just x -> return $ varFunDef x

lookupVarFunF :: LangIdent -> ExecIO CallSig
lookupVarFunF name = do
  res <- lookupVarFun name
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
    Nothing -> langError $ nameNotOpErr opName
    Just x -> return x
  

-- | Modify the current scope using the given function.
modifyScope :: (Scope -> Scope) -> ExecIO ()
modifyScope f = do
  env <- get
  let oldScope = currentScope env
      newScope = f oldScope
  put env {currentScope=newScope}

assignVarLit :: LangIdent -> LangLit -> ExecIO LangLit
assignVarLit name val = do
  modifyScope $ flip (setVarInScope name $ setVarLit emptyVar val) True
  return val
         
assignVar :: LangIdent -> VarVal -> ExecIO ()
assignVar name val = modifyScope $ flip (setVarInScope name val) True
                     
assignVarFun :: LangIdent -> CallSig -> ExecIO ()
assignVarFun name val = modifyScope $ flip (setVarInScope name $ setVarFun emptyVar val) True

infix 4 |=
(|=) = assignVarLit

-- Little test program until tests are instantiated
-- basicProg :: IO ()
-- basicProg = do
--   let prog1 = "x" |= LitInt 5          
--       prog2 = newScope >> "x" |= LitInt 6
--       prog3 = do
--               "y" |= LitStr "hello"
--               let cs = CallSig (ArgSig ["a","b"] Nothing) (MultiStmt [])
--               argListBind [ExprIdent "x", ExprIdent "y"] cs
--               liftM currentScope get
--   r1 <- runExecIOBasic $ prog1 *> prog2 *> lookupVar "x"
--   liftIO $ print r1
--   mapM_ (\x -> runExecIOBasic x >>= liftIO . print)
--            [ prog1 *> lookupVar "x"
--            , prog1 *> prog2 *> lookupVar "x"
--            , prog1 *> prog2 *> upScope *> lookupVar "x"
--            ]
  -- liftIO $ print $ runExecIOBasic $ prog1 *> prog2 *> upScope *> prog3
  
              
        
-- TODO: Should this just stay in the current scope if there
-- is no parent scope?
-- | Changes the current scope to the parent scope.
upScope :: ExecIO ()
upScope = modifyScope (fromJust . outerScope)
  
          
argListBind :: [Expr] -> CallSig -> ExecIO ()
argListBind args cs = do
  let params = callArgs cs
      la = length args
      lp = length (stdArgs params)
  when (la > lp && not (hasCatchAllArg params) || la < lp)
           (langError $ wrongNumberOfArgumentsErr lp la)
  vals <- mapM execExpr args
  let toBind = zip (stdArgs params) vals
      fullBind = toBind ++
                 if length toBind /= la
                 then [(fromJust $ catchAllArg params, LitList $ drop (length toBind) vals)]
                 else [(fromJust $ catchAllArg params, LitList []) | hasCatchAllArg params] 
  newScope
  forM_ fullBind (uncurry assignVarLit)
        

defun = assignVarFun
        
maybeToLit :: Maybe LangLit -> LangLit  
maybeToLit Nothing  = LitNull
maybeToLit (Just x) = x

litToMaybe :: LangLit -> Maybe LangLit
litToMaybe LitNull = Nothing
litToMaybe x = Just x

-- | Reduce a ``pure'' expression to a literal
evalExpr :: Expr -> ExecIO LangLit
evalExpr (ExprLit x) = return x
evalExpr (ExprIdent x) = lookupVarLitF x
                         
execExpr :: Expr -> ExecIO LangLit
execExpr (ExprLit x) = return x
execExpr (ExprIdent x) = lookupVarLitF x
execExpr (ExprOp x) = withErrHandle (execOp x)
execExpr (ExprFunCall name args) = execFunCall name args
                                   
execFunCall :: LangIdent -> [Expr] -> ExecIO LangLit
execFunCall = callFun
                      

execOp :: LangOp -> ExecIO LangLit
execOp (SpecOp op expr) = execSpecOp op expr
execOp (MultiOp op exprs) = execMultiOp op exprs

execSpecOp :: Op -> Expr -> ExecIO LangLit
execSpecOp OpNeg x = execExpr x >>= notLit

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
  
         
withMultiOp :: [Expr] -> ([LangLit] -> ExecIO LangLit) -> ExecIO LangLit
withMultiOp xs f = mapM execExpr xs >>= f 
  -- liftIO $ addLit (map execExpr xs)
         
basicExpr = ExprOp (MultiOp OpAdd [ExprLit (LitInt 1), ExprLit (LitFloat 8)])
                       
newtype ExecIO a = ExecIO 
    { runEIO :: ErrorT LError (StateT Env IO) a }
    deriving (Functor, Applicative, Monad
             , MonadState Env, MonadError LError
             , CanError, MonadIO)
        
withBasic :: Expr -> IO (Either LError LangLit)
withBasic e = evalStateT (runErrorT (runEIO (execExpr e))) basicEnv
              
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
                   
callBuiltin :: LangIdent -> [Expr] -> ExecIO LangLit 
callBuiltin (LangIdent "print") xs = mapM execExpr xs >>= builtinPrint
callBuiltin (LangIdent "str")   xs = mapM execExpr xs >>= builtinStr
         
builtinPrint :: [LangLit] -> ExecIO LangLit
builtinPrint xs = liftIO $ putStrLn res >> return (LitStr res)
                  where res = concatMap showSyn xs
                              
-- | Implementation of the built-in str function.
builtinStr :: [LangLit] -> ExecIO LangLit
builtinStr [] = return $ LitStr ""
builtinStr xs | length xs > 1 = throwLangError $ wrongNumberOfArgumentsErr 1 (length xs)
              | otherwise = return $ toLitStr (head xs)

-- | True if the identifier represents a builtin function.
isBuiltin :: LangIdent -> Bool
isBuiltin = ((`elem`builtins) . getIdent)
    where builtins = ["print", "str"]

callFun :: LangIdent -> [Expr] -> ExecIO LangLit
callFun x args | isBuiltin x = callBuiltin x args
               | otherwise = do
  callsig <- lookupVarFunF x
  callFunCallSig callsig args
-- do
  --callsig <- lookupVarFunF x
  --argListBind args callsig
  --execStmt (callBody callsig)
           
callFunCallSig :: CallSig -> [Expr] -> ExecIO LangLit
callFunCallSig callsig args = do
  argListBind args callsig
  execStmt (callBody callsig)
                               
execStmt :: Stmt -> ExecIO LangLit
execStmt (SingleStmt x pos) = modify (\s -> s {envSourceRef = pos}) >> execSingStmt x
execStmt (MultiStmt []) = return LitNull
execStmt (MultiStmt xs) = liftM last $ mapM execStmt xs
                          
execSingStmt :: SingStmt -> ExecIO LangLit
execSingStmt (StmtAssign name e) = execExpr e >>= assignVarLit name
execSingStmt (StmtStruct x) = execLangStruct x
execSingStmt (StmtExpr x) = setEnvSynRep (showSyn x) >> execExpr x
execSingStmt (StmtComment _) = return LitNull
                            
execLangStruct :: LangStruct -> ExecIO LangLit
execLangStruct (StructFor name e s) = execStructFor name e s
execLangStruct (StructWhile e s) = execStructWhile e s
execLangStruct (StructIf if' thn els) = execStructIf if' thn els
execLangStruct (StructDefun name cs) = assignVarFun name cs *> return LitNull
                                        
execStructIf if' thn els = do
  p <- execExpr if'
  case p of
    (LitBool True) -> execStmt thn
    (LitBool False) -> case els of 
                         Nothing -> return LitNull
                         Just s  -> execStmt s
    x -> throwLangError $ typeUnexpectedErr (typeOf x) LTBool
                    
execStructFor = undefined
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
  
  
