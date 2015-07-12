{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Angle.Parse.Parser
    (
    ) where

import Angle.Lex.Lexer
import Control.Monad.Reader
import Control.Monad.Error
import Control.Applicative
    
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
           
