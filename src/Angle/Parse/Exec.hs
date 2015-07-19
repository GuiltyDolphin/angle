{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Angle.Parse.Exec
    ( runExecIOBasic
    , runExecIOEnv
    , execStmt
    , ExecIO
    , Env
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
               } deriving (Show)
         
basicEnv :: Env
basicEnv = Env { currentScope = emptyScope
               , envOptions = defaultOptions
               , sourceText = ""
               }
         
data OptionSet = OS { printName :: Bool }
                 deriving (Show, Eq)
                          

defaultOptions :: OptionSet
defaultOptions = OS { printName = True }

               
-- | Executes in environment `Env' with a result of @a@.
newtype Exec a = 
    Exec {runExec :: ErrorT LangError (State Env) a }
    deriving (Functor, Applicative
             , Monad, MonadState Env
             , MonadError LangError)
             
runExecBasic :: Exec a -> Either LangError a
runExecBasic e = evalState (runErrorT (runExec e)) basicEnv
    
-- | Create a new scope with the current scope as its
-- parent.
newScope :: ExecIO ()
newScope = do
  env <- get
  modifyScope (\s -> emptyScope {outerScope=Just s})
  let oldScope = currentScope env
      newScope = emptyScope { outerScope = Just oldScope }
  put env { currentScope = newScope }

lookupVar :: Ident -> ExecIO (Maybe VarVal)
lookupVar name = do
  env <- get
  let res = resolve name (currentScope env)
  return res
         
lookupVarLit :: Ident -> ExecIO (Maybe LangLit)
lookupVarLit name = do
  res <- lookupVar name
  case res of 
    Nothing -> return Nothing
    Just x -> return $ varLitDef x
  
lookupVarLitF :: Ident -> ExecIO LangLit
lookupVarLitF name = do
  res <- lookupVarLit name
  case res of
    Nothing -> langError $ nameNotValueErr name
    Just x -> return x
              
lookupVarFun :: Ident -> ExecIO (Maybe CallSig)
lookupVarFun name = do
  res <- lookupVar name
  case res of 
    Nothing -> return Nothing
    Just x -> return $ varFunDef x

lookupVarFunF :: Ident -> ExecIO CallSig
lookupVarFunF name = do
  res <- lookupVarFun name
  case res of
    Nothing -> langError $ nameNotFunctionErr name
    Just x -> return x

         
lookupVarF :: Ident -> ExecIO VarVal
lookupVarF name = do
  res <- lookupVar name
  case res of
    Just x -> return x
    Nothing -> langError $ nameNotDefinedErr name

-- | Modify the current scope using the given function.
modifyScope :: (Scope -> Scope) -> ExecIO ()
modifyScope f = do
  env <- get
  let oldScope = currentScope env
      newScope = f oldScope
  put env {currentScope=newScope}

assignVarLit :: Ident -> LangLit -> ExecIO LangLit
assignVarLit name val = do
  modifyScope $ flip (setVarInScope name $ setVarLit emptyVar val) True
  return val
         
assignVar :: Ident -> VarVal -> ExecIO ()
assignVar name val = modifyScope $ flip (setVarInScope name val) True
                     
assignVarFun :: Ident -> CallSig -> ExecIO ()
assignVarFun name val = modifyScope $ flip (setVarInScope name $ setVarFun emptyVar val) True

runWithEnv :: Env -> Exec a -> Either LangError a
runWithEnv env exec = evalState (runErrorT (runExec exec)) env

infix 4 |=
(|=) = assignVarLit

-- Little test program until tests are instantiated
basicProg :: IO ()
basicProg = do
  let prog1 = "x" |= LitInt 5          
      prog2 = newScope >> "x" |= LitInt 6
      prog3 = do
              "y" |= LitStr "hello"
              let cs = CallSig (ArgSig ["a","b"] Nothing) (MultiStmt [])
              argListBind [ExprIdent "x", ExprIdent "y"] cs
              liftM currentScope get
  r1 <- runExecIOBasic $ prog1 *> prog2 *> lookupVar "x"
  liftIO $ print r1
  mapM_ (\x -> runExecIOBasic x >>= liftIO . print)
           [ prog1 *> lookupVar "x"
           , prog1 *> prog2 *> lookupVar "x"
           , prog1 *> prog2 *> upScope *> lookupVar "x"
           ]
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
           (langError $ wrongNumberOfArgumentsErr la lp)
  vals <- mapM execExpr args
  let toBind = zip (stdArgs params) vals
      fullBind = toBind ++
                 if length toBind /= la
                 then [(fromJust $ catchAllArg params, LitList $ map ExprLit (drop (length toBind) vals))]
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
execExpr (ExprOp x) = execOp x
execExpr (ExprFunCall name args) = execFunCall name args
                                   
execFunCall :: Ident -> [Expr] -> ExecIO LangLit
execFunCall = callFun
                      

execOp :: LangOp -> ExecIO LangLit
execOp (SpecOp op expr) = execSpecOp op expr
execOp (MultiOp op exprs) = execMultiOp op exprs

execSpecOp :: Op -> Expr -> ExecIO LangLit
execSpecOp OpNeg x = do 
  res <- execExpr x
  negLit res
                       
execMultiOp :: Op -> [Expr] -> ExecIO LangLit
execMultiOp OpAdd xs = do
  lits <- mapM execExpr xs
  addLit lits
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
                   
callBuiltin :: Ident -> [Expr] -> ExecIO LangLit 
callBuiltin "print" xs = mapM execExpr xs >>= builtinPrint
callBuiltin "str"   xs = mapM execExpr xs >>= builtinStr
         
builtinPrint :: [LangLit] -> ExecIO LangLit
builtinPrint xs = liftIO $ putStrLn res >> return (LitStr res)
                  where res = concatMap show xs
                              
builtinStr :: [LangLit] -> ExecIO LangLit
builtinStr [] = return $ LitStr ""
builtinStr xs | length xs > 1 = langError $ wrongNumberOfArgumentsErr (length xs) 1
              | otherwise = return $ toLitStr (head xs)

isBuiltin :: Ident -> Bool
isBuiltin = (`elem`builtins)
    where builtins = ["print", "str"]

callFun :: Ident -> [Expr] -> ExecIO LangLit
callFun x args | isBuiltin x = callBuiltin x args
               | otherwise = do
  callsig <- lookupVarFunF x
  argListBind args callsig
  execStmt (callBody callsig)
                               
execStmt :: Stmt -> ExecIO LangLit
execStmt (SingleStmt x) = execSingStmt x
execStmt (MultiStmt xs) = liftM last $ mapM execStmt xs
                          
execSingStmt :: SingStmt -> ExecIO LangLit
execSingStmt (StmtAssign name e) = execExpr e >>= assignVarLit name
execSingStmt (StmtStruct x) = execLangStruct x
execSingStmt (StmtExpr x) = execExpr x
                            
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
    x -> langError $ typeUnexpectedErr (typeOf x) LTBool
                    
execStructFor = undefined
execStructWhile = undefined
                  

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

execRun :: [(Stmt, (SourcePos, SourcePos))] -> ExecIO ()
execRun [] = return ()
execRun xs = do
  forM_ xs 
            (\(x,pos) -> 
             execStmt x `catchError` (\e -> do
                                        source <- liftM sourceText get
                                        throwError e { errorPos=pos
                                                     , errorSource=source
                                                     }))
  
  
