{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Angle.Parse.Parser
    ( evalProg
    ) where

import Angle.Lex.Lexer
import Angle.Lex.Helpers
import Angle.Types.Lang
import Angle.Parse.Error

import Control.Monad.Reader
import Control.Monad.Error
import Data.Maybe (fromJust)
import Control.Applicative
import Control.Monad.State
import qualified Data.Map as M
    
langNot (LitBool x) = LitBool (not x)

langLitJoin :: LangLit -> LangLit -> Either LangError LangLit
langLitJoin (LitList xs) (LitList ys) = return $ LitList (xs++ys)
langLitJoin x (LitList _) = Left (typeUnexpectedErr (typeOf x) LTList)
langLitJoin l@(LitList _) x = langLitJoin x l

                            
langLitAdd :: LangLit -> LangLit -> Either LangError LangLit
langLitAdd l@(LitList _) r = langLitJoin l r
langLitAdd (LitInt x) (LitInt y) = return $ LitInt (x + y)
langLitAdd (LitFloat x) r 
    = case r of
        LitInt y -> return $ LitFloat (x + fromIntegral y)
        LitFloat y -> return $ LitFloat (x + y)
        _ -> Left $ typeUnexpectedErr (typeOf r) LTFloat
langLitAdd l r@(LitFloat _) = langLitAdd r l
langLitAdd l r 
    | typeOf l /= typeOf r 
        = Left $ typeMismatchErr ltype rtype
    | otherwise 
        = Left $ typeNotValidErr ltype
    where ltype = typeOf l
          rtype = typeOf r
                 
    

-- **************
-- Evaluating

-- * Need to be able to return values from language literals
-- * Need to be able to overload operator functions to act on different values of literals
-- * Need to be able to retrieve the value of a variable from scope
-- * Need to be able to assign values to variables

-- $setup
-- >>> let exprInt = ExprLit . LitInt
-- >>> let exprId = ExprIdent
-- >>> let exprAdd = ExprOp . MultiOp OpAdd
-- >>> let evalExpr = evalBasic . reduceExprToLit
-- >>> let evalStmt = evalBasic . reduceStmtToLit

-- type VarVal = (Maybe LangLit, Maybe CallSig)
data VarVal = VarVal { varLitDef :: Maybe LangLit
                     , varFunDef :: Maybe CallSig
                     } deriving (Show)
            
setVarLit :: VarVal -> LangLit -> VarVal
setVarLit var val = var { varLitDef = Just val }
setVarFun :: VarVal -> CallSig -> VarVal                    
setVarFun var fd = var { varFunDef = Just fd }

type BindEnv = M.Map Ident VarVal

-- | Environment in which programs are executed.
newtype ExecEnv a = ExecEnv
    { runExecEnv :: ErrorT LangError (State BindEnv) a }
    deriving ( Functor
             , Applicative
             , Monad
             , MonadError LangError
             , MonadState BindEnv)
    
eval :: BindEnv -> ExecEnv a -> Either LangError a
eval env = (`evalState` env) . runErrorT . runExecEnv
           
-- TODO: What is being assigned? Expression or literal?
--  If expression: should only literals be assigned?
--   i.e, the expression is reduced to a literal before assignment
--   this would potentially remove laziness
assignVal :: Ident -> Expr -> ExecEnv LangLit
assignVal name expr = do
  val <- reduceExprToLit expr
  modify $ M.alter (valAssign val) name
  return val
    where valAssign v Nothing = Just emptyVar { varLitDef = Just v }
          valAssign v (Just x) = Just x { varLitDef = Just v }

infixl 4 =:
(=:) = assignVal
                                   
assignFun :: Ident -> CallSig -> ExecEnv LangLit
assignFun name cs = do
  modify (M.alter funAssign name)
  return LitNull
    where funAssign Nothing = Just emptyVar { varFunDef = Just cs }
          funAssign (Just v) = Just v { varFunDef = Just cs }
  
getVar :: Ident -> ExecEnv VarVal
getVar name = do
  env <- get
  case M.lookup name env of
    Nothing -> throwError . nameNotDefinedErr $ name
    Just x -> return x
   
 
getVarVal :: Ident -> ExecEnv LangLit
getVarVal name = do
  e <- liftM varLitDef $ getVar name
  case e of
    Nothing -> throwError . nameNotValueErr $ name
    Just v -> return v

getFunVal :: Ident -> ExecEnv CallSig
getFunVal name = do
  f <- liftM varFunDef $ getVar name
  case f of
    Nothing -> throwError . nameNotFunctionErr $ name
    Just x -> return x

              
evalBasic = eval M.empty
            
opSub (ExprLit (LitInt x)) (ExprLit (LitInt y)) = ExprLit (LitInt (x - y))
opAdd (ExprLit (LitInt x)) (ExprLit (LitInt y)) = ExprLit (LitInt (x + y))

                                                  
subLit :: LangLit -> LangLit -> LangLit
subLit (LitInt x) (LitInt y) = LitInt (x - y)

opCall :: Op -> Expr -> Expr -> ExecEnv LangLit
opCall OpAdd exp1 exp2 = do
  l <- execExpr exp1
  r <- execExpr exp2
  addOp l r
         
-- |Add operation
-- >>> evalBasic $ addOp (exprInt 2) (exprInt 3)
-- Right (...5...)
addOp :: Expr -> Expr -> ExecEnv LangLit
addOp (ExprLit x) (ExprLit y) = return $ addLit x y
addOp x@(ExprIdent _) r = do
  l <- reduceExprToLit x
  addOp (ExprLit l) r
-- TODO: Make this into a proper type error
addOp x y = throwError noMsg
        
negOp :: Expr -> ExecEnv Expr
negOp (ExprLit x) = liftM ExprLit (negLit x)
negOp x = execExpr x >>= negOp
          
negLit :: LangLit -> ExecEnv LangLit
negLit (LitInt x) = return $ LitInt (-x)
negLit x = throwError . typeNotValidErr . typeOf $ x

                                
-- TODO: Version of this that can return errors
addLit :: LangLit -> LangLit -> LangLit
addLit (LitInt x) (LitInt y) = LitInt (x + y)

-- |Execute expression
execExpr :: Expr -> ExecEnv Expr
execExpr (ExprOp op) = liftM ExprLit (execOp op)
execExpr lit@(ExprLit _) = return lit
execExpr (ExprIdent x)   = liftM ExprLit (getVarVal x)
-- execExpr (ExprFunCall name args) = funCall name args
                                   
evalExpr :: Expr -> ExecEnv LangLit
evalExpr (ExprOp op) = evalOp op
evalExpr (ExprLit lit) = return lit
evalExpr (ExprIdent x) = getVarVal x
evalExpr (ExprFunCall name args) = funCall name args
                                   
evalOp (MultiOp op exprs) = evalMultOp op exprs
evalMultOp OpAdd exprs = liftM (foldr1 addLit) (mapM reduceExprToLit exprs)
                                   
execOp (MultiOp op exprs) = execMultOp op exprs
                            
-- TODO: Check if reducing to literals is required (may be able
--  to have expressions and allow for lazy)
execMultOp OpAdd exprs = liftM (foldr1 addLit) (mapM reduceExprToLit exprs)
                   
                                   
reduceExprToLit :: Expr -> ExecEnv LangLit
reduceExprToLit (ExprLit x) = return x
reduceExprToLit x = do
  res <- execExpr x
  reduceExprToLit res
                  
reduceStmtToLit :: Stmt -> ExecEnv LangLit
reduceStmtToLit (SingleStmt (StmtExpr x)) = reduceExprToLit x
reduceStmtToLit x = evalStmt x
                  
reduceToLit = reduceStmtToLit
-- evalExpr (ExprFunCall fc) = funCall fc
                            
-- funCall :: FunCall -> ExprC
-- funCall (FC name args) = do
                        
-- execSingStmt :: SingStmt -> ExecEnv Expr
-- execSingStmt (StmtAssign x y) = assignVal x y
-- execSingStmt (StmtExpr e) = execExpr e
-- execSingStmt (StmtStruct x) = execStruct x
                              
-- execStmt :: Stmt -> ExecEnv Expr
-- execStmt (SingleStmt s) = execSingStmt s
-- execStmt (MultiStmt [s]) = execStmt s
-- execStmt (MultiStmt stmts@(_:_)) = do
--   mapM_ execStmt (init stmts)
--   execStmt (last stmts)
           
evalStmt :: Stmt -> ExecEnv LangLit
evalStmt (SingleStmt s) = evalSingStmt s
evalStmt (MultiStmt [s]) = evalStmt s
evalStmt (MultiStmt stmts@(_:_)) = do
  mapM_ evalStmt (init stmts)
  evalStmt (last stmts)
           
execSingStmt (StmtStruct s) = execStruct s
execStmt (SingleStmt s) = execSingStmt s
           
evalSingStmt :: SingStmt -> ExecEnv LangLit
evalSingStmt (StmtAssign x y) = assignVal x y
evalSingStmt (StmtExpr e) = evalExpr e
-- evalSingStmt (StmtStruct x) = execStruct x
                          
execStruct (StructDefun name (CallSig args body)) = assignFun name (CallSig args body)
           
  
funCall :: Ident -> [Expr] -> ExecEnv LangLit
funCall name args = do
  (CallSig argList body) <- getFunVal name
  let zipped = zip args argList
  forM_ zipped (\(val,argName) -> assignVal argName val)
  evalStmt body
getProg :: String -> Stmt
getProg s = case evalScan s stmt of
              Left _ -> undefined
              Right x -> x
                         
addxy val   = getProg $ "y=(+ x " ++ show val ++ ");"
addx val    = getProg $ "x=(+ x " ++ show val ++ ");"
valx        = getProg   "x;"
assignx val = getProg $ "x=" ++ show val ++ ";"

basics = getProg "{(defun x(y) {y=(+ y 1)}); z=2; x(z)}"
defx = getProg "defun x(y) {y=(+ y 1)};"

       
evalProg = evalBasic . evalStmt

-- TODO:
--  eval and exec
--  - eval for results without side-effects?
--  - exec for results with side-effects?



-- *****************
-- ***** SCOPE *****
-- *****************

-- | Represents the current scope.
data Scope = Scope 
    { outerScope :: Maybe Scope -- ^Parent scope, if any
    , bindings   :: BindEnv
    } deriving (Show)


-- | True if the given scope has no parent scopes.           
isOutermostScope :: Scope -> Bool
isOutermostScope s = case outerScope s of
                    Nothing -> True
                    Just _ -> False


-- | @name `isDefinedIn` scope@ is True if @scope@
-- contains a definition for @name@.
isDefinedIn :: Ident -> Scope -> Bool
isDefinedIn name scope = case M.lookup name (bindings scope) of
                         Nothing -> False
                         Just _ -> True
                                   

-- | Runs a function in the outer scope of that provided.
--
-- Returns `Nothing' if no outer scope exists.
withOuterScope :: Scope -> (Scope -> a) -> Maybe a
withOuterScope sc f = liftM f (outerScope sc)
                      

-- | @withOutermostScope f scope@ runs @f@ in the parent-most
-- scope of @scope@.
withOutermostScope :: (Scope -> a) -> Scope -> a
withOutermostScope f scope = 
    if isOutermostScope scope
    then f scope
    else withOutermostScope f (fromJust $ outerScope scope)
                      

-- | Finds the local-most Scope that contains a definition
-- for the specified identifier.
innerScopeDefining :: Ident -> Scope -> Maybe Scope
innerScopeDefining name scope = 
    if name `isDefinedIn` scope
    then Just scope
    else join $ withOuterScope scope (innerScopeDefining name)
    

-- | @resolve name scope@ Retrieves the @name@'s value
-- from the local-most scope in which it is defined.
--
-- Returns Nothing if there is no definition for @name@.
resolve :: Ident -> Scope -> Maybe VarVal
resolve name scope = if name `isDefinedIn` scope
                     then fromCurrentScope name scope
                     else outerScope scope >>= resolve name
    where fromCurrentScope n s = M.lookup n (bindings s)
                                 
-- | A scope with no parent or bindings
emptyScope :: Scope
emptyScope = Scope { 
               outerScope = Nothing
             , bindings = M.empty
             }

-- | Run a function over the bindings of a scope.
onBindings :: (BindEnv -> BindEnv) -> Scope -> Scope
onBindings f scope = scope { bindings = f $ bindings scope }

-- | Boolean determines whether to overwrite ident if it
-- exists.
setVarInScope :: Ident -> VarVal -> Scope -> Bool -> Scope
setVarInScope name val scope@(Scope{bindings=binds}) overwrite
    = if name `isDefinedIn` scope 
      then if overwrite
           then scope {bindings=M.alter (\_ -> Just val) name binds}
           else scope
      else scope {bindings=M.alter (\_ -> Just val) name binds}
          


-- *****************************************
-- * Env - A temporary test of environment *
-- *****************************************
data Env = Env { currentScope :: Scope
               , envOptions :: OptionSet
               } deriving (Show)
         
basicEnv :: Env
basicEnv = Env { currentScope = emptyScope
               , envOptions = defaultOptions
               }
         
data OptionSet = OS { printName :: Bool }
                 deriving (Show, Eq)
                          

defaultOptions :: OptionSet
defaultOptions = OS { printName = True }

               
-- | Executes in environment `Env' with a result of @a@.
newtype Exec a = Exec {runExec :: State Env a }
    deriving (Functor, Applicative, Monad, MonadState Env)
             
runExecBasic :: Exec a -> a
runExecBasic e = evalState (runExec e) basicEnv
    
newScope :: Exec ()
newScope = do
  env <- get
  modifyScope (\s -> emptyScope {outerScope=Just s})
  let oldScope = currentScope env
      newScope = emptyScope { outerScope = Just oldScope }
  put env { currentScope = newScope }

lookupVar :: Ident -> Exec (Maybe VarVal)
lookupVar name = do
  env <- get
  let res = resolve name (currentScope env)
  return res

         
modifyScope :: (Scope -> Scope) -> Exec ()
modifyScope f = do
  env <- get
  let oldScope = currentScope env
      newScope = f oldScope
  put env {currentScope=newScope}

assignVar :: Ident -> LangLit -> Exec LangLit
assignVar name val = do
  env <- get
  let oldScope = currentScope env
      newScope = setVarInScope name (setVarLit emptyVar val) oldScope True
  put env { currentScope = newScope }
  return val

emptyVar :: VarVal
emptyVar = VarVal { varLitDef = Nothing, varFunDef = Nothing }
         
runWithEnv :: Env -> Exec a -> a
runWithEnv env exec = evalState (runExec exec) env

-- Little test program until tests are instantiated
basicProg :: IO ()
basicProg = do
  prog1 <- return $ do
    assignVar "x" (LitInt 5)
  print (runExecBasic (prog1 *> lookupVar "x"))
  prog2 <- return $ do
              newScope
              assignVar "x" (LitInt 6)
  print (runExecBasic (prog1 *> prog2 *> lookupVar "x"))
  print (runExecBasic (prog1 *> prog2 *> upScope *> lookupVar "x"))
        
upScope :: Exec ()
upScope = do
  modifyScope (\s -> fromJust $ outerScope s)
  
              
-- Scope API:
-- - changing scope
--   - new scope    
--     (newScope :: Maybe Scope -> Scope)
--   - global scope 
--     (globalScope :: Scope -> Scope)
--   - parent scope 
--     (parentScope :: Scope -> Maybe Scope)
-- - setting variables
--   - in current scope (maybe only applies to an Exec?)
--     - function value
--     - literal value
-- - executing in different scopes
--   - in a new scope
--   - in the global scope
--   - in the current scope
--   - in the parent scope
-- - resolving variables
--   - check current scope, then outer scopes 
--     (resolve :: Ident -> Scope -> Maybe VarVal)
--   - only check current scope 
--     (resolveCurrent :: Ident -> Scope -> Maybe VarVal)
--   - only check global scope 
--     (resolveGlobal :: Ident -> Scope -> Maybe VarVal)

-- Scope API when in Exec
-- - changing scope
--   - make new scope
--     (newScope :: Exec ())
--   - go to parent
--     (upScope :: Exec ())


-- Exec Requirements
-- - Errors
--   - Custom datatype for Errors
--   - Use ErrorT to add error handling
-- - State
--   - Current scope
--   - Current settings


-- VarVal API
-- - retrieving values
--   - function definition
--     (varFunDef :: VarVal -> CallSig)
--   - value definition
--     (varLitDef :: VarVal -> LangLit)
-- - setting values
--   - function definition
--     (varSetFunDef :: VarVal -> CallSig -> VarVal)
--   - value definition
--     (varSetLitDef :: VarVal -> LangLit -> VarVal)
--   - empty (basic) VarVal
--     (emptyVar :: VarVal)
-- - checking definitions
--   - function definition
--     (hasFunctionDefinition :: VarVal -> Bool)
--   - value definition
--     (hasLiteralDefinition :: VarVal -> Bool)
-- NOTES
-- - record for determining builtins?
--   (isBuiltin :: VarVal -> Bool)
