{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE OverlappingInstances #-}
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
    , CallSig(..)
    , ArgSig(..)
    , hasCatchAllArg
    , ShowSyn(..)
    , SourceRef(..)
    , startRef
    ) where

import Numeric (showFFloat)
    
import Angle.Scanner (SourcePos, beginningOfFile)
    

-- | Most general construct in the language.
data Stmt = 
    SingleStmt 
    { stmtSingStmt :: SingStmt 
    , stmtSourcePos :: SourceRef
    } -- ^ Any language construct that
      --   performs some action or evaluation.
          | MultiStmt [Stmt] -- ^ Many statements, allowing
                             --   a series of statements to be
                             --   executed one after another,
                             --   discarding intermediate
                             --   results.
            deriving (Show)
                     

-- | Statements are equal if their contents are equal,
-- the position may differ.
instance Eq Stmt where
    (SingleStmt x _) == (SingleStmt y _) = x == y
    (MultiStmt xs) == (MultiStmt ys) = xs == ys
    _ == _ = False
                     

-- | Positional reference to some section of source code.
newtype SourceRef = SourceRef { getSourceRef :: (SourcePos, SourcePos) }
    deriving (Show, Eq)
             

-- | The initial `SourceRef' - starting and ending at
-- the beginning of the file.
startRef :: SourceRef
startRef = SourceRef (beginningOfFile, beginningOfFile)


-- | Interface for types that can have a string representation
-- in the language.
class ShowSyn a where
    -- | Convert the value to a string representation that
    -- would produce the exact same result if lexed.
    showSyn :: a -> String
               

instance ShowSyn Stmt where
    showSyn (SingleStmt x _) = showSyn x
    showSyn (MultiStmt xs) = "{\n" ++ showRest ++ "}\n" 
        where showRest = unlines 
                         $ map (" "++) $ lines 
                         $ concatMap showSyn xs
                             

instance ShowSyn SingStmt where
    showSyn (StmtAssign n e) = concat [showSyn n, " = ", showSyn e, ";\n"]
    showSyn (StmtStruct x) = showSyn x
    showSyn (StmtExpr e) = showSyn e ++ ";\n"
    showSyn (StmtComment x) = "#" ++ x ++ "\n"
    showSyn (StmtReturn x) = "return " ++ showSyn x ++ ";\n"


-- | A single statement.
data SingStmt = StmtAssign LangIdent Expr
              | StmtComment String -- ^ Comment which is - for all intents and purposes - ignored by the parser.
              | StmtStruct LangStruct
              | StmtExpr Expr -- ^ Expression. Evaluates to a literal.
              | StmtReturn Expr
                deriving (Show, Eq)


-- | Specialised language constructs.
data LangStruct = StructFor LangIdent Expr Stmt
                | StructWhile Expr Stmt
                | StructIf Expr Stmt (Maybe Stmt)
                | StructDefun LangIdent CallSig
                  deriving (Show, Eq)
                           

instance ShowSyn LangStruct where
    showSyn (StructFor n e s) = 
        concat [ "for ", showSyn n
               , " in ", showSyn e
               , " do ", showSyn s]
    showSyn (StructWhile e s) = 
        concat ["while ", showSyn e, " do ", showSyn s]
    showSyn (StructIf e s els) 
        = concat [ "if "   , showSyn e
                 , " then ", showSyn s] ++ 
          case els of
            Nothing -> ""
            Just x -> " else " ++ showSyn x
    showSyn (StructDefun n c)
        = concat ["defun ", showSyn n, showSyn c]
    -- showSyn (StructDefun n c) 
    --     = concat ["defun ", showSyn n, showSynSep "("
    --                           (case catchArg of
    --                              Nothing -> ") "
    --                              Just x -> concat [if not (null args) then ", .." else "..", showSyn x, ") "]) ", " args]
    --       ++ showSyn body
    --     where args = stdArgs $ callArgs c
    --           body = callBody c
    --           catchArg = catchAllArg $ callArgs c
                    
    
showSynSep :: ShowSyn a => String -> String -> String -> [a] -> String
showSynSep start end _ [] = start ++ end
showSynSep start end sep xs = start ++ concatMap ((++sep) . showSyn) (init xs) ++ showSyn (last xs) ++ end


showSynArgs :: (ShowSyn a) => [a] -> String
showSynArgs = showSynSep "(" ")" ", "


showSynList :: (ShowSyn a) => [a] -> String
showSynList = showSynSep "[" "]" ", "
              

showSynOpList :: (ShowSyn a) => [a] -> String
showSynOpList = showSynSep " " ")" " "


-- | A function.
data CallSig = CallSig 
    { callArgs :: ArgSig -- ^ The argument list that is accepted by the function.
    , callBody :: Stmt -- ^ The function body.
    } deriving (Show, Eq)
             

-- | An argument signature.
data ArgSig = ArgSig { stdArgs :: [LangIdent] -- ^ Standard positional arguments.
                     , catchAllArg :: Maybe LangIdent -- ^ Argument that catches any remaining arguments after the positional arguments have been filled.
                     } deriving (Show, Eq)


-- | @True@ if `catchAllArg` is @Just@ something.
hasCatchAllArg :: ArgSig -> Bool
hasCatchAllArg x = case catchAllArg x of
                     Nothing -> False
                     Just _ -> True


-- | Language literal values.
data LangLit = LitStr { getLitStr :: String } -- ^ Strings.
             | LitInt { getLitInt :: Int } -- ^ Integers, support at least the range -2^29 to 2^29-1.
             | LitFloat { getLitFloat :: Double } -- ^ Double-precision floating point value.
             | LitList { getLitList :: [LangLit] } -- ^ List of literal values. Values may be of different types.    
             | LitBool { getLitBool :: Bool } -- ^ Boolean value.
             | LitRange Expr Expr -- Might want Expr version of
                                  -- this, then have
                                  -- LitRange LangLit LangLit
             | LitNull -- ^ Null value. Implicit value 
                       -- returned from any expression 
                       -- that fails to return a value 
                       -- explicitly.
             | LitLambda { getLitLambda :: CallSig } -- ^ A function without a name.
               deriving (Show, Eq)
                   

instance ShowSyn LangLit where
    showSyn (LitStr x) = '\"' : x ++ "\""
    showSyn (LitInt x) = show x
    showSyn (LitFloat x) = showFFloat Nothing x ""
    showSyn (LitList xs) = showSynList xs
    showSyn (LitBool x) = if x then "true" else "false"
    showSyn (LitRange x y) = "(" ++ showSyn x ++ ".." ++ showSyn y ++ ")"
    showSyn LitNull = "null"
    showSyn (LitLambda x@(CallSig _ (SingleStmt _ _))) = init $ showSyn x
    showSyn (LitLambda x) = showSyn x
                   

data LangType = LTStr
              | LTInt
              | LTFloat
              | LTList
              | LTBool
              | LTRange
              | LTNull
              | LTLambda
                deriving (Eq)


typeOf :: LangLit -> LangType
typeOf (LitStr   _)   = LTStr
typeOf (LitInt   _)   = LTInt
typeOf (LitFloat _)   = LTFloat
typeOf (LitList  _)   = LTList
typeOf (LitBool  _)   = LTBool
typeOf (LitRange _ _) = LTRange
typeOf LitNull        = LTNull
typeOf (LitLambda _) = LTLambda
              

instance Show LangType where
    show LTList = "list"
    show LTBool = "boolean"
    show LTStr = "string"
    show LTInt = "integer"
    show LTFloat = "float"
    show LTNull = "null"
    show LTRange = "range"
    show LTLambda = "lambda"
                   

data Expr = ExprIdent LangIdent
          | ExprFunIdent LangIdent
          | ExprLambda CallSig
          | ExprLit LangLit
          | ExprFunCall LangIdent [Expr]
          | ExprOp LangOp
          | ExprList [Expr]
            deriving (Show, Eq)
                     
instance ShowSyn Expr where
    showSyn (ExprIdent x) = showSyn x
    showSyn (ExprLit x) = showSyn x
    showSyn (ExprFunCall n es) = showSyn n ++ showSynArgs es
    showSyn (ExprOp x) = showSyn x
    showSyn (ExprLambda x) = "(" ++ showSyn (LitLambda x) ++ ")"
    showSyn (ExprFunIdent x) = "$" ++ showSyn x
    showSyn (ExprList _) = error "showSyn - cannot show unevaluated list"
                         
                         
newtype LangIdent = LangIdent { getIdent :: String }
    deriving (Show, Eq, Ord)
                          
instance ShowSyn LangIdent where
    showSyn = getIdent
    
                         
instance ShowSyn CallSig where
    showSyn (CallSig args body) = showSyn args ++ " " ++ showSyn body
                                  

-- | TODO: Check this out... It looks a bit weird.
instance ShowSyn ArgSig where
    showSyn (ArgSig args catchArg) = 
        showSynSep "("
          (case catchArg of
             Nothing -> ")"
             Just x -> concat 
                       [ if not (null args) 
                         then ", .." 
                         else ".."
                       , showSyn x
                       , ")"]) ", " args
                                  

data LangOp = SpecOp Op Expr 
            | MultiOp Op [Expr]
              deriving (Show, Eq)
                       

instance ShowSyn LangOp where
    showSyn (SpecOp o e) = showSyn o ++ showSyn e
    showSyn (MultiOp o es) = concat ["(", showSyn o, showSynOpList es]

data Op = OpAdd 
        | OpAnd
        | OpDiv 
        | OpEq
        | OpGreater
        | OpGreaterEq
        | OpLess
        | OpLessEq
        | OpMult 
        | OpNeg
        | OpNot 
        | OpOr
        | OpSub 
        | UserOp LangIdent
          deriving (Show, Eq)
                   

instance ShowSyn Op where
    showSyn OpAdd = "+"
    showSyn OpAnd = "&"
    showSyn OpDiv = "/"
    showSyn OpEq = "=="
    showSyn OpGreater = ">"
    showSyn OpGreaterEq = ">="
    showSyn OpLess = "<"
    showSyn OpLessEq = "<="
    showSyn OpMult = "*"
    showSyn OpNeg = "-"
    showSyn OpNot = "^"
    showSyn OpOr = "|"
    showSyn OpSub = "-"
    showSyn (UserOp x) = showSyn x
