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
    , ArgSig(..)
    , hasCatchAllArg
    , ShowSyn(..)
    , SourceRef(..)
    , startRef
    , ClassRef(..)
    , ClassLambda(..)
    , AnnType(..)
    , ArgElt(..)
    , argNoAnn
    , Lambda(..)
    , typeAnnOf
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
    showSyn (StmtBreak x False) = "break" ++ maybe "" ((" "++) . showSyn) x
    showSyn (StmtBreak Nothing True) = "continue"
    showSyn (StmtBreak _ _) = error "showSyn: StmtBreak not a valid combination!"


-- | A single statement.
data SingStmt = StmtAssign LangIdent Expr
              | StmtComment String -- ^ Comment which is - for all intents and purposes - ignored by the parser.
              | StmtStruct LangStruct
              | StmtExpr Expr -- ^ Expression. Evaluates to a literal.
              | StmtReturn Expr
              | StmtBreak { breakValue :: Maybe Expr
                          , breakContinue :: Bool }
                deriving (Show, Eq)


-- | Specialised language constructs.
data LangStruct = StructFor LangIdent Expr Stmt
                | StructWhile Expr Stmt
                | StructIf Expr Stmt (Maybe Stmt)
                | StructDefun LangIdent Lambda
                | StructDefClass LangIdent Lambda
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
    showSyn (StructDefClass n c)
        = concat ["defclass ", showSyn n, showSyn c]
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


data ParamList = ParamList { parListParams :: [Expr] }
               deriving (Show, Eq)
             

data Lambda = Lambda { lambdaArgs :: ArgSig
                     , lambdaBody :: Stmt
                     } deriving (Show, Eq)
            
instance ShowSyn Lambda where
    showSyn (Lambda args body) = concat ["(", showSyn args, " ", showSyn body, ")"]

-- | An argument signature.
data ArgSig = ArgSig { stdArgs :: [ArgElt] -- ^ Standard positional arguments.
                     , catchAllArg :: Maybe LangIdent -- ^ Argument that catches any remaining arguments after the positional arguments have been filled.
                     } deriving (Show, Eq)


data ArgElt = ArgElt 
    { argEltType :: AnnType
    , argEltName :: LangIdent
    , argEltClass :: Maybe ClassRef
    } deriving (Show, Eq)
            
argNoAnn :: LangIdent -> ArgElt
argNoAnn name = ArgElt { argEltType = AnnLit
                       , argEltName = name
                       , argEltClass = Nothing }
            

instance ShowSyn ArgElt where
    showSyn (ArgElt {argEltType=typ
                    , argEltName=name
                    , argEltClass=cls })
        = case typ of
            AnnFun -> "$"
            AnnClass -> "@"
            AnnLit -> ""
          ++ showSyn name ++ case cls of 
                               Just c -> ':' : showSyn c
                               Nothing -> ""


data ClassLambda = ClassLambda LangIdent Lambda
              deriving (Show, Eq)
              

newtype ClassRef = ClassRef { getClassRef :: LangIdent }
    deriving (Show, Eq)
             
instance ShowSyn ClassRef where
    showSyn (ClassRef {getClassRef = name}) = '@' : showSyn name
                                     

data AnnType = AnnClass | AnnFun | AnnLit
               deriving (Eq)
                        
instance Show AnnType where
    show AnnClass = "class"
    show AnnFun = "function"
    show AnnLit = "literal"


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
             | LitChar { getLitChar :: Char }
             | LitRange LangLit (Maybe LangLit) (Maybe LangLit)
             | LitNull -- ^ Null value. Implicit value 
                       -- returned from any expression 
                       -- that fails to return a value 
                       -- explicitly.
             | LitLambda { getLitLambda :: Lambda } -- ^ A function without a name.
               deriving (Show, Eq)
                   

instance ShowSyn LangLit where
    showSyn (LitStr x) = '\"' : x ++ "\""
    showSyn (LitChar x) = show x
    showSyn (LitInt x) = show x
    showSyn (LitFloat x) = showFFloat Nothing x ""
    showSyn (LitList xs) = showSynList xs
    showSyn (LitBool x) = if x then "true" else "false"
    showSyn (LitRange x y z) = showRange x y z
    showSyn LitNull = "null"
    showSyn (LitLambda x@(Lambda _ (SingleStmt _ _))) = init $ showSyn x
    showSyn (LitLambda x) = showSyn x
                            

showRange :: (ShowSyn a) => a -> Maybe a -> Maybe a -> String
showRange x y z = "(" ++ showSyn x ++ ".." ++ maybe "" showSyn y ++ maybe "" ((".." ++) . showSyn) z ++ ")"
                   

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
typeOf (LitStr{})   = LTStr
typeOf (LitInt{})   = LTInt
typeOf (LitFloat{})   = LTFloat
typeOf (LitList{})   = LTList
typeOf (LitBool{})   = LTBool
typeOf (LitRange{}) = LTRange
typeOf LitNull        = LTNull
typeOf (LitLambda{}) = LTLambda
                       

-- TODO: Check this - can't identify classes
typeAnnOf (LitLambda{}) = AnnFun
typeAnnOf _ = AnnLit
              

instance Show LangType where
    show LTList = "list"
    show LTBool = "boolean"
    show LTStr = "string"
    show LTInt = "integer"
    show LTFloat = "float"
    show LTNull = "null"
    show LTRange = "range"
    show LTLambda = "function"
                   

data Expr = ExprIdent LangIdent
          | ExprFunIdent LangIdent
          | ExprLambda Lambda
          | ExprLit LangLit
          | ExprFunCall LangIdent [Expr]
          | ExprLambdaCall Lambda [Expr]
          | ExprOp LangOp
          | ExprList [Expr]
          | ExprRange Expr (Maybe Expr) (Maybe Expr)
          | ExprParamExpand LangIdent
            deriving (Show, Eq)
                     

instance ShowSyn Expr where
    showSyn (ExprIdent x) = showSyn x
    showSyn (ExprLit x) = showSyn x
    showSyn (ExprFunCall n es) = showSyn n ++ showSynArgs es
    showSyn (ExprOp x) = showSyn x
    showSyn (ExprLambda x) = "(" ++ showSyn (LitLambda x) ++ ")"
    showSyn (ExprFunIdent x) = "$" ++ showSyn x
    showSyn (ExprList _) = error "showSyn - cannot show unevaluated list"
    showSyn (ExprRange{}) = error "showSyn - cannot show unevaluated range"
    showSyn (ExprLambdaCall x xs) = showSyn (ExprLambda x) ++ " : (" ++ showSynArgs xs ++ ")"
    showSyn (ExprParamExpand _) = error "showSyn - ExprParamExpand made it to showSyn"
                         
                         
newtype LangIdent = LangIdent { getIdent :: String }
    deriving (Show, Eq, Ord)
                          
instance ShowSyn LangIdent where
    showSyn = getIdent
                                  

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
        | OpConcat
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
    showSyn OpConcat = "++"
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
