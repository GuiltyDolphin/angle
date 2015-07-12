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
    ) where

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
                | StructDefun LangIdent [LangIdent] Stmt
                | StructReturn Expr -- TODO: Probably don't 
                                    -- need this
                  deriving (Show, Eq)

data LangLit = LitStr String
             | LitInt Int
             | LitFloat Float
             | LitList [Expr]
             | LitBool Bool
             | LitRange Expr Expr
             | LitNull
               deriving (Show, Eq)
                        
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
          | ExprB Expr
          | ExprOp LangOp
            deriving (Show, Eq)
                     
type LangIdent = String

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
        
