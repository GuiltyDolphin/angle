{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Angle.Parse.Parser
    () where

import Angle.Lex.Lexer
import Angle.Lex.Helpers
import Control.Monad.Reader
import Control.Monad.Error
import Control.Applicative
import Control.Monad.State
import qualified Data.Map as M
import Debug.Trace (trace)
    
import Data.IORef


data LangError = TypeError TypeError
               | SyntaxError String
               | UndefinedIdent String
               | DefaultError String

instance Show LangError where
    show (TypeError e) = "wrong type in expression: " ++ show e
    show (SyntaxError s) = "syntax error: " ++ s
    show (UndefinedIdent v) = "identifier not defined: " ++ v
    show (DefaultError s) = "defaultError: " ++ s

instance Error LangError where
    noMsg = DefaultError ""
    strMsg = DefaultError

type TestInt = IORef Int
  
type Env = IORef [(String, Expr)] 

type EnvE = ErrorT LangError IO    
newtype IOLangError a = ILE { runIOLangError :: ErrorT LangError IO a }
    deriving (Functor, Applicative, Monad, MonadError LangError)
           
type LE = ErrorT LangError (ReaderT Env IO)


opMap = [(OpNot, langNot)]

langNot (LitBool x) = LitBool (not x)

langLitJoin :: LangLit -> LangLit -> Either LangError LangLit
langLitJoin (LitList xs) (LitList ys) = return $ LitList (xs++ys)
langLitJoin x (LitList _) = Left (TypeError $ TypeUnexpected (typeOf x) LList)
langLitJoin l@(LitList _) x = langLitJoin x l

                            
langLitAdd :: LangLit -> LangLit -> Either LangError LangLit
langLitAdd l@(LitList _) r = langLitJoin l r
langLitAdd (LitInt x) (LitInt y) = return $ LitInt (x + y)
langLitAdd (LitFloat x) r 
    = case r of
        LitInt y -> return $ LitFloat (x + fromIntegral y)
        LitFloat y -> return $ LitFloat (x + y)
        _ -> Left . TypeError $ TypeUnexpected (typeOf r) LFloat
langLitAdd l r@(LitFloat _) = langLitAdd r l
langLitAdd l r 
    | typeOf l /= typeOf r 
        = Left . TypeError $ TypeMismatch ltype rtype
    | otherwise 
        = Left . TypeError $ TypeNotValid ltype
    where ltype = typeOf l
          rtype = typeOf r
                 
data LangType = LList | LBool | LStr | LInt | LFloat
                deriving (Eq)
              
typeOf :: LangLit -> LangType
typeOf (LitStr _) = LStr
typeOf (LitList _) = LList
typeOf (LitInt _) = LInt
typeOf (LitFloat _) = LFloat
typeOf (LitBool _) = LBool
              
instance Show LangType where
    show LList = "list"
    show LBool = "boolean"
    show LStr = "string"
    show LInt = "integer"
    show LFloat = "float"

data TypeError = TypeMismatch LangType LangType
               | TypeUnexpected LangType LangType
               | TypeNotValid LangType
               
instance Show TypeError where
    show (TypeMismatch l r) = "type mismatch: got (" ++ show l ++ ", " ++ show r ++ ") but both types should be the same"
    show (TypeUnexpected l r) = "unexpected type: " ++ show l ++ ", expecting: " ++ show r
    show (TypeNotValid l) = "type not valid for scenario: " ++ show l


data LError = LError { errorExpr :: Expr
                     , errorStmt :: Stmt
                     , errorErr :: LangError 
                     , errorSource :: String
                     , lerrorPos :: (Int, Int, Int)
                     }

instance Show LError where
    -- show (LError { errorExpr=ex, errorStmt=es, errorErr=ee })
    --     = unlines
    --       [ "error in statement: " ++ show es
    --       , "in expression: " ++ show ex
    --       , show ee ]
    show (LError { errorErr=ee, lerrorPos=ep@(_,_,pos), errorSource=es })
        = cEp ++ cEt ++ cEe
          where cEp = showPos ep ++ "\n"
                cEt = takeWhile (/='\n') (drop pos es)
                cEe = show ee
                showPos (ln,cn,_) =
                  concat ["line: ", show ln, ", column: ", show cn]

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

type BindEnv = M.Map Ident (Maybe Expr, Maybe CallSig)
type Ident = String
type Val = Int

data CallSig = CallSig [Ident] Stmt
               deriving (Show)
    
type EvalCxt = ErrorT String (State BindEnv)
    
type ExprC = EvalCxt Expr

eval :: BindEnv -> EvalCxt a -> Either String a
eval env = (`evalState` env) . runErrorT
           
-- TODO: What is being assigned? Expression or literal?
--  If expression: should only literals be assigned?
--   i.e, the expression is reduced to a literal before assignment
--   this would potentially remove laziness
assignVal :: Ident -> Expr -> EvalCxt Expr
assignVal name expr = do
  val <- execExpr expr
  modify $ M.alter (valAssign val) name
  return expr
    where valAssign v Nothing = Just (Just v, Nothing)
          valAssign v (Just (_,f)) = Just (Just v, f)
                                   
assignFun :: Ident -> CallSig -> EvalCxt Expr
assignFun name cs = do
  modify (M.alter funAssign name)
  return (ExprIdent name)
      where funAssign Nothing = Just (Nothing, Just cs)
            funAssign (Just (e,_)) = Just (e, Just cs)
  
getVar :: Ident -> EvalCxt (Maybe Expr, Maybe CallSig)
getVar name = do
  env <- get
  case M.lookup name env of
    Nothing -> throwError $ "not in scope: " ++ name
    Just x -> return x
    
getVarVal :: Ident -> EvalCxt Expr
getVarVal name = do
  (e,_) <- getVar name
  case e of
    Nothing -> throwError $ "no value definition for " ++ name
    Just v -> return v

getFunVal :: Ident -> EvalCxt CallSig
getFunVal name = do
  (_,f) <- getVar name
  case f of
    Nothing -> throwError $ "no function definition: " ++ name
    Just x -> return x

              
evalBasic = eval M.empty
            
opSub (ExprLit (LitInt x)) (ExprLit (LitInt y)) = ExprLit (LitInt (x - y))
opAdd (ExprLit (LitInt x)) (ExprLit (LitInt y)) = ExprLit (LitInt (x + y))

                                                  
subLit :: LangLit -> LangLit -> LangLit
subLit (LitInt x) (LitInt y) = LitInt (x - y)

opCall :: Op -> Expr -> Expr -> ExprC
opCall OpAdd exp1 exp2 = do
  l <- execExpr exp1
  r <- execExpr exp2
  addOp l r
         
-- |Add operation
-- >>> evalBasic $ addOp (exprInt 2) (exprInt 3)
-- Right (...5...)
addOp :: Expr -> Expr -> EvalCxt Expr
addOp (ExprLit x) (ExprLit y) = return . ExprLit $ addLit x y
addOp x@(ExprIdent _) r = do
  l <- execExpr x
  addOp l r
addOp x y = throwError "wrong types in addition"
        
negOp :: Expr -> EvalCxt Expr
negOp (ExprLit x) = liftM ExprLit (negLit x)
negOp x = execExpr x >>= negOp
          
negLit :: LangLit -> EvalCxt LangLit
negLit (LitInt x) = return $ LitInt (-x)
negLit _ = throwError "wrong type in negation!"

                                
-- TODO: Version of this that can return errors
addLit :: LangLit -> LangLit -> LangLit
addLit (LitInt x) (LitInt y) = LitInt (x + y)

-- |Execute expression
execExpr :: Expr -> EvalCxt Expr
execExpr (ExprOp op) = liftM ExprLit (execOp op)
execExpr lit@(ExprLit _) = return lit
execExpr (ExprIdent x)   = getVarVal x
execExpr (ExprFunCall name args) = funCall name args
                                   
execOp (MultiOp op exprs) = execMultOp op exprs
                            
-- TODO: Check if reducing to literals is required (may be able
--  to have expressions and allow for lazy)
execMultOp OpAdd exprs = liftM (foldr1 addLit) (mapM reduceExprToLit exprs)
                   
                                   
reduceExprToLit :: Expr -> EvalCxt LangLit
reduceExprToLit (ExprLit x) = return x
reduceExprToLit x = do
  res <- execExpr x
  reduceExprToLit res
                  
reduceStmtToLit :: Stmt -> EvalCxt LangLit
reduceStmtToLit (SingleStmt (StmtExpr x)) = reduceExprToLit x
reduceStmtToLit x = do
  res <- execStmt x
  reduceExprToLit res
                  
reduceToLit = reduceStmtToLit
-- evalExpr (ExprFunCall fc) = funCall fc
                            
-- funCall :: FunCall -> ExprC
-- funCall (FC name args) = do
                        
execSingStmt :: SingStmt -> EvalCxt Expr
execSingStmt (StmtAssign x y) = assignVal x y
execSingStmt (StmtExpr e) = execExpr e
execSingStmt (StmtStruct x) = execStruct x
                              
execStmt :: Stmt -> EvalCxt Expr
execStmt (SingleStmt s) = execSingStmt s
execStmt (MultiStmt [s]) = execStmt s
execStmt (MultiStmt stmts@(_:_)) = do
  mapM_ execStmt (init stmts)
  execStmt (last stmts)
                          
execStruct (StructDefun name args body) = assignFun name (CallSig args body)
           
  
funCall :: Ident -> [Expr] -> EvalCxt Expr
funCall name args = do
  (CallSig argList body) <- getFunVal name
  let zipped = zip args argList
  forM_ zipped (\(val,argName) -> assignVal argName val)
  execStmt body
getProg :: String -> Stmt
getProg s = case evalScan s stmt of
              Left _ -> undefined
              Right x -> x
                         
addxy val = getProg $ "y=(+ x " ++ show val ++ ");"
addx val = getProg $ "x=(+ x " ++ show val ++ ");"
valx = getProg $ "x;"
assignx val = getProg $ "x=" ++ show val ++ ";"
