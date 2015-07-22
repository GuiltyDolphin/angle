{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Angle.Types.Lang
    ( Stmt(..)
    , SingStmt(..)
    , Expr(..)
    , LangOp(..)
    , Op(..)
    , LangStruct(..)
    , LangLit(..)
    , LangIdent
    , LangType(..)
    , typeOf
    , Ident
    , CallSig(..)
    , ArgSig(..)
    , hasCatchAllArg
    ) where

import Control.Monad.Error
import Control.Applicative
    
data Stmt = SingleStmt SingStmt 
          | MultiStmt [Stmt]
            deriving (Show, Eq)

-- | A single statement;
data SingStmt = StmtAssign LangIdent Expr
              | StmtComment String
              | StmtStruct LangStruct
              | StmtExpr Expr
                deriving (Show, Eq)

-- |Specialised language constructs
data LangStruct = StructFor LangIdent Expr Stmt
                | StructWhile Expr Stmt
                | StructIf Expr Stmt (Maybe Stmt)
                | StructDefun LangIdent CallSig
                | StructReturn Expr -- TODO: Probably don't 
                                    -- need this
                  deriving (Show, Eq)
                           
data CallSig = CallSig 
    { callArgs :: ArgSig
    , callBody :: Stmt
    } deriving (Show, Eq)
             
data ArgSig = ArgSig { stdArgs :: [LangIdent]
                     , catchAllArg :: Maybe LangIdent
                     } deriving (Show, Eq)

hasCatchAllArg :: ArgSig -> Bool
hasCatchAllArg x = case catchAllArg x of
                     Nothing -> False
                     Just _ -> True

data LangLit = LitStr String
             | LitInt Int
             | LitFloat Float
             | LitList [Expr]     -- See below
             | LitBool Bool
             | LitRange Expr Expr -- Might want Expr version of
                                  -- this, then have
                                  -- LitRange LangLit LangLit
             | LitNull
               deriving (Eq)
                        
instance Show LangLit where
    show (LitStr x) = show x
    show (LitInt x) = show x
    show (LitFloat x) = show x
    show (LitList xs) = show xs
    show (LitBool x) = if x then "true" else "false"
    show (LitRange x y) = "(" ++ show x ++ ".." ++ show y ++ ")"
    show LitNull = "null"

                        
data LangType = LTStr
              | LTInt
              | LTFloat
              | LTList
              | LTBool
              | LTRange
              | LTNull
                deriving (Eq)

typeOf :: LangLit -> LangType
typeOf (LitStr   _)   = LTStr
typeOf (LitInt   _)   = LTInt
typeOf (LitFloat _)   = LTFloat
typeOf (LitList  _)   = LTList
typeOf (LitBool  _)   = LTBool
typeOf (LitRange _ _) = LTRange
typeOf LitNull        = LTNull
              
instance Show LangType where
    show LTList = "list"
    show LTBool = "boolean"
    show LTStr = "string"
    show LTInt = "integer"
    show LTFloat = "float"
                   

data Expr = ExprIdent LangIdent
          | ExprLit LangLit
          | ExprFunCall LangIdent [Expr]
          | ExprOp LangOp
            deriving (Show, Eq)
                     
type LangIdent = String
type Ident = String
    
data LangOp = SpecOp Op Expr 
            | MultiOp Op [Expr]
              deriving (Show, Eq)

data Op = OpNeg
        | OpMult 
        | OpDiv 
        | OpAdd 
        | OpSub 
        | OpNot 
        | OpEq
          deriving (Show, Eq)
        
