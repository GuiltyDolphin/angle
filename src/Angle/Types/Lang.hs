{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
module Angle.Types.Lang
    ( Stmt(..)
    , SingStmt(..)
    , Expr(..)
    , LangOp(..)
    , Op(..)
    , LangStruct(..)
    , LangLit(..)
    , LangIdent(..)
    , LangType(..)
    , typeOf
    , Ident
    , CallSig(..)
    , ArgSig(..)
    , hasCatchAllArg
    , ShowSyn(..)
    ) where

import Control.Monad.Error
import Control.Applicative
import Numeric (showFFloat)
    
data Stmt = SingleStmt SingStmt 
          | MultiStmt [Stmt]
            deriving (Show, Eq)

-- | Interface for types that can have a representation
-- in the language.
class ShowSyn a where
    -- | Convert the value to a string representation that
    -- would produce the exact same result if lexed.
    showSyn :: a -> String
               
instance ShowSyn Stmt where
    showSyn (SingleStmt x@(StmtComment _)) = showSyn x
    showSyn (SingleStmt x@(StmtStruct _)) = showSyn x
    showSyn (SingleStmt x) = showSyn x ++ ";"
    showSyn (MultiStmt xs) = "{" ++ concatMap showSyn xs ++ "}"
                             
instance ShowSyn SingStmt where
    showSyn (StmtAssign n e) = concat [showSyn n, " = ", showSyn e]
    showSyn (StmtStruct x) = showSyn x
    showSyn (StmtExpr e) = showSyn e
    showSyn (StmtComment x) = "#" ++ x ++ "-#"

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
                           
instance ShowSyn LangStruct where
    showSyn (StructFor n e s) = concat ["for ", showSyn n, " in ", showSyn e, " do ", showSyn s]
    showSyn (StructWhile e s) = concat ["while ", showSyn e, " do ", showSyn s]
    showSyn (StructIf e s els) 
        = concat ["if ", showSyn e, " then ", showSyn s] ++ 
          case els of
            Nothing -> ""
            Just x -> " else " ++ showSyn x
    showSyn (StructDefun n c) 
        = concat ["defun ", showSyn n, showSynSep "("
                              (case catchArg of
                                 Nothing -> ") "
                                 Just x -> concat [", ..", showSyn x, ") "]) ", " args]
          ++ showSyn body
        where args = stdArgs $ callArgs c
              body = callBody c
              catchArg = catchAllArg $ callArgs c
                    
    
showSynSep :: ShowSyn a => String -> String -> String -> [a] -> String
showSynSep start end _ [] = start ++ end
showSynSep start end sep xs = start ++ concatMap ((++sep) . showSyn) (init xs) ++ showSyn (last xs) ++ end

showSynArgs :: (ShowSyn a) => [a] -> String
showSynArgs = showSynSep "(" ")" ", "

showSynList :: (ShowSyn a) => [a] -> String
showSynList = showSynSep "[" "]" ", "
              
showSynOpList :: (ShowSyn a) => [a] -> String
showSynOpList = showSynSep " " ")" " "

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
               deriving (Show, Eq)
                   
instance ShowSyn LangLit where
    showSyn (LitStr x) = '\"' : x ++ "\""
    showSyn (LitInt x) = show x
    showSyn (LitFloat x) = showFFloat Nothing x ""
    showSyn (LitList xs) = showSynList xs
    showSyn (LitBool x) = if x then "true" else "false"
    showSyn (LitRange x y) = "(" ++ showSyn x ++ ".." ++ showSyn y ++ ")"
    showSyn LitNull = "null"
                   
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
                     
instance ShowSyn Expr where
    showSyn (ExprIdent x) = showSyn x
    showSyn (ExprLit x) = showSyn x
    showSyn (ExprFunCall n es) = showSyn n ++ showSynArgs es
    showSyn (ExprOp x) = showSyn x
                         
newtype LangIdent = LangIdent { getIdent :: String }
    deriving (Show, Eq, Ord)
    
instance ShowSyn LangIdent where
    showSyn = getIdent

type Ident = String
    
data LangOp = SpecOp Op Expr 
            | MultiOp Op [Expr]
              deriving (Show, Eq)
                       
instance ShowSyn LangOp where
    showSyn (SpecOp o e) = showSyn o ++ showSyn e
    showSyn (MultiOp o es) = concat ["(", showSyn o, showSynOpList es]

data Op = OpNeg
        | OpMult 
        | OpDiv 
        | OpAdd 
        | OpSub 
        | OpNot 
        | OpEq
          deriving (Show, Eq)
                   
instance ShowSyn Op where
    showSyn OpNeg = "-"
    showSyn OpMult = "*"
    showSyn OpDiv = "/"
    showSyn OpAdd = "+"
    showSyn OpSub = "-"
    showSyn OpNot = "^"
    showSyn OpEq = "=="
        
