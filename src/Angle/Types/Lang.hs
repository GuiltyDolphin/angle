{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverlappingInstances #-}
{-|
Module      : Angle.Types.Lang
Description : Basic types that make up the Angle language.
Copyright   : Copyright (C) 2015 Ben Moon
License     : GNU GPL, version 3
Maintainer  : GuiltyDolphin@gmail.com
Stability   : experimental

* LangLit is a container for the various literal values.
* LangType represents the types of these values.
* LangIdent represents names (identifiers).

* Stmt wraps SingStmts.
* SingStmt represents the statements in Angle.


= Statements in Angle
In Angle, statements are split into two main types (see 'Stmt'),
singular statements and multi-statements.

Singular statements are represented by 'SingStmt' and are what the
programmer uses to achieve tasks; multi-statements are multiple
singular statements grouped together such that the effective value
of the statements is the same as the last statement in the group.


== Singular statements
Statements are separated by semi-colons and allow the programmer to
perform actions.

[@assignment@] this allows the programmer to give a name to values and
functions, so that he may refer to them later on in the code.

[@comments@] comments have no functional value, but serve as a means
to document or explain parts of code.

[@structures@] see "Angle.Types.Lang#structures"

[@expressions@] see 'Expr'

[@return@] allows the programmer to exit a function early and use
the provided value as the function's value.

[@break and continue@] for use during loops: break exits the loop
immediately, whereas continue starts the next iteration of the loop,
skipping the rest of the loop body.

[@raise@] allows the user to throw exceptions.


== Language structures #structures#
In Angle, there exist language structures for performing certain tasks.

[@if statement@ : 'StructIf'] categorized as a structure for
convenience, consists of three parts: a condition, a body of code
that will execute if the condition holds, and an optional body that
will execute if the condition does not hold.

[@function definitions@ : 'StructDefun'] allows the assignment of previously
non-existant lambda bodies to a name.

[@try catch@ : 'StructTryCatch'] allows basic handling of exceptions.
-}
module Angle.Types.Lang
    ( Expr(..)

    -- * Fundamental types
    , LangLit(..)
    , isNull
    , LangIdent(..)
    , isSymbolIdent

    -- * Advanced types
    , Stmt(..)
    , SingStmt(..)
    , LangStruct(..)

    -- ** Angle types
    , LangType(..)
    , typeOf

    , ArgSig(..)
    , CatchArg(..)
    , ShowSyn(..)
    , SourceRef(..)
    , startRef
    , ConstrRef(..)
    , ArgElt(..)
    , Lambda(..)
    , Scope
    , enumType
    , allType
    ) where

import Control.Applicative ((<$>))
import Numeric (showFFloat)
import System.IO (Handle)

-- import Angle.Scanner (SourcePos, beginningOfFile)
import Angle.Types.Scope (GenScope)

import Text.Parsec.Pos

import Angle.Parse.Token (validSymbolIdentChars)

type Scope = GenScope LangIdent LangLit


-- | Wraps statements to allow for positional tracking as well
-- as multiple statements grouped together.
data Stmt =
    SingleStmt
    { stmtSingStmt :: SingStmt
    , stmtSourcePos :: SourceRef
    } -- ^ Any language construct that
      --   performs some action or evaluation.
    | MultiStmt [Stmt]
      -- ^ Many statements, allowing
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


-- | Positional reference to a section of source code.
newtype SourceRef = SourceRef
    { getSourceRef :: (SourcePos, SourcePos)
    } deriving (Show, Eq)


-- | The initial `SourceRef' - starting and ending at
-- the beginning of the file.
startRef :: SourceRef
startRef = SourceRef (initialPos "" , initialPos "")


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
    showSyn (StmtAssignNonLocal n e) = concat [showSyn n, " |= ", showSyn e, ";\n"]
    showSyn (StmtAssignGlobal n e) = concat [showSyn n, " ||= ", showSyn e, ";\n"]
    showSyn (StmtStruct x) = showSyn x
    showSyn (StmtExpr e) = showSyn e ++ ";\n"
    showSyn (StmtComment x) = "#" ++ x ++ "\n"
    showSyn (StmtReturn x) = "return " ++ showSyn x ++ ";\n"
    showSyn (StmtBreak x False) = "break" ++ retVal ++ ";\n"
      where
        retVal = maybe "" ((" "++) . showSyn) x
    showSyn (StmtBreak Nothing True) = "continue;\n"
    showSyn (StmtRaise e) = "raise " ++ showSyn e ++ ";\n"
    showSyn (StmtBreak _ _) = error "showSyn: StmtBreak not a valid combination!"


-- | A single statement.
data SingStmt = StmtAssign LangIdent Expr
              | StmtAssignNonLocal LangIdent Expr
              | StmtAssignGlobal LangIdent Expr
              | StmtComment String -- ^ Comment which is - for all intents and purposes - ignored by the parser.
              | StmtStruct LangStruct
              | StmtExpr Expr -- ^ Expression. Evaluates to a literal.
              | StmtReturn Expr
              | StmtBreak { breakValue :: Maybe Expr
                          , breakContinue :: Bool }
              | StmtRaise LangLit
                deriving (Show, Eq)


-- | Specialised language constructs.
data LangStruct = StructIf Expr Stmt (Maybe Stmt)
                | StructDefun LangIdent Lambda
                | StructTryCatch Stmt [([LangLit], Stmt)]
                  deriving (Show, Eq)


instance ShowSyn LangStruct where
    showSyn (StructIf e s els)
        = concat [ "if "   , showSyn e
                 , " then ", showSyn s] ++
          case els of
            Nothing -> ""
            Just x  -> " else " ++ showSyn x
    showSyn (StructDefun n c)
        = concat ["defun ", showSyn n, showLambdaFun c]
    showSyn (StructTryCatch s es) = "try " ++ showSyn s ++ concatMap showCatch es
      where
        showCatch (toCatch, b) = "\ncatch " ++ es' toCatch ++ showSyn b
        es' toCatch = if length toCatch == 1
                then showSyn . head $ toCatch
                else showSyn . LitList $ toCatch



showSynSep :: ShowSyn a => String -> String -> String -> [a] -> String
showSynSep start end _ [] = start ++ end
showSynSep start end sep xs = start ++ concatMap ((++sep) . showSyn) (init xs) ++ showSyn (last xs) ++ end


showSynArgs :: (ShowSyn a) => [a] -> String
showSynArgs = showSynSep "(" ")" ", "


-- | Lambdas consist of two parts: the parameter list and the body.
--
-- The parameter list describes the possible forms with which the
-- lambda can be invoked.
--
-- The body is the code that is executed upon successful invokation.
data Lambda = Lambda { lambdaArgs :: ArgSig
                     , lambdaBody :: Stmt
                     , lambdaScope :: Maybe Scope
                     } deriving (Show, Eq)


instance ShowSyn Lambda where
    showSyn (Lambda args body@(SingleStmt _ _) _)
         = concat ["(", showSyn args, " ", init $ showSyn body, ")"]
    showSyn (Lambda args body _) = concat ["(", showSyn args, " ", showSyn body, ")"]


-- | An argument signature.
data ArgSig = ArgSig { stdArgs :: [ArgElt] -- ^ Standard positional arguments.
                     , catchAllArg :: Maybe CatchArg -- ^ Argument that catches any remaining arguments after the positional arguments have been filled.
                     } deriving (Show, Eq)


data CatchArg = CatchArg
    { catchArgName :: LangIdent
    , catchArgConstr :: Maybe ConstrRef
    } deriving (Show, Eq)


instance ShowSyn CatchArg where
    showSyn (CatchArg { catchArgName = n, catchArgConstr = c })
        = showSyn n ++ case c of
                            Just constr  -> ':' : showSyn constr
                            Nothing -> ""


-- | A single element of a parameter list, allows enforcing of parameter
-- constraints.
data ArgElt = ArgElt
    { argEltName :: LangIdent
    , argEltConstr :: Maybe ConstrRef
    } deriving (Show, Eq)


instance ShowSyn ArgElt where
    showSyn (ArgElt { argEltName=name
                    , argEltConstr=constr })
        = showSyn name ++ case constr of
                          Just c  -> ':' : showSyn c
                          Nothing -> ""


-- | Name referencing a function to be used as a parameter constraint.
--
-- Parameter-constraints perform run-time checks on arguments
-- passed to a function.
--
-- For example, if a function of the form @foo(x:\@largeInt)@
-- is called with some value @y@, the value of @y@ will be
-- evaluated and then passed to the function @largeInt@.
-- @largeInt@ should then return a boolean value stating whether
-- the value passed satisfies it's criteria. Failure to return
-- a boolean value will result in a fatal error.
-- If @largeInt@ returns @true@, then the function proceeds as normal,
-- otherwise an error is thrown.
--
-- Functions to be used as parameter-constraints must satisfy the following:
--
-- * Must be able to take 1 value on its own.
--
-- * Must return a true or false value when used as a constraint.
data ConstrRef = ConstrRef
    { getConstrRef :: LangIdent
    , constrRefArgs :: Maybe [Expr]
    }
    deriving (Show, Eq)


instance ShowSyn ConstrRef where
    showSyn (ConstrRef {getConstrRef = name, constrRefArgs=a})
        = '@' : showSyn name ++ showRefArgs
      where
        showRefArgs = maybe "" showSynArgs a


-- | Represents the basic types that can be used in Angle.
--
-- The reason for the types being contained within one datatype
-- is to allow untyped expressions to exist in Angle, including
-- multi-type lists.
--
-- See 'ConstrRef' for a means of providing run-time parameter
-- constraints in Angle.
data LangLit = LitStr String -- ^ Strings.
             | LitInt Int -- ^ Integers, support at least the range -2^29 to 2^29-1.
             | LitFloat Double -- ^ Double-precision floating point value.
             | LitList [LangLit] -- ^ List of literal values. Values may be of different types.
             | LitBool Bool -- ^ Boolean value.
             | LitChar Char -- ^ Character literals, these cannot be specified by the programmer and are used internally
                                             -- by Angle.
             | LitRange LangLit (Maybe LangLit) (Maybe LangLit)
             | LitNull -- ^ Implicit value
                       -- returned from any expression
                       -- that fails to return a value
                       -- explicitly.
             | LitLambda Lambda -- ^ A function without a name.
             | LitClosure Lambda
             | LitHandle Handle
               deriving (Show, Eq)


instance ShowSyn LangLit where
    showSyn (LitStr x) = show x
    showSyn (LitChar x) = show x
    showSyn (LitInt x) = show x
    showSyn (LitFloat x) = showFFloat Nothing x ""
    showSyn (LitList xs) = showSynList xs
        where showSynList = showSynSep "[" "]" ", "
    showSyn (LitBool x) = if x then "true" else "false"
    showSyn (LitRange x y z) = showRange
        where showRange
                  = concat [ "["
                           , showSyn x
                           , ".."
                           , maybe "" showSyn y
                           , maybe "" ((".." ++) . showSyn) z
                           , "]" ]

    showSyn LitNull = "null"
    showSyn (LitLambda x) = showSyn x
    showSyn (LitClosure x) = showSyn x
    showSyn (LitHandle h) = show h


-- | The types of the values that can be used in Angle.
--
-- The separation of the value containers ('LangLit') and
-- type representations allows untyped expressions to be
-- attained more easily.
data LangType = LTStr
              | LTChar
              | LTInt
              | LTFloat
              | LTList
              | LTBool
              | LTRange
              | LTNull
              | LTLambda
              | LTHandle
                deriving (Eq)


-- | Function for determining the type of a literal.
typeOf :: LangLit -> LangType
typeOf (LitStr _)    = LTStr
typeOf (LitChar _)   = LTChar
typeOf (LitInt _)    = LTInt
typeOf (LitFloat _)  = LTFloat
typeOf (LitList _)   = LTList
typeOf (LitBool _)   = LTBool
typeOf (LitRange{})  = LTRange
typeOf LitNull       = LTNull
typeOf (LitLambda{}) = LTLambda
typeOf (LitClosure{}) = LTLambda
typeOf (LitHandle _) = LTHandle


instance Show LangType where
    show LTList = "list"
    show LTBool = "boolean"
    show LTStr = "string"
    show LTInt = "integer"
    show LTFloat = "float"
    show LTNull = "null"
    show LTRange = "range"
    show LTLambda = "function"
    show LTChar = "char"
    show LTHandle = "handle"


-- | Expressions must be evaluable to some literal, although
-- in some cases they may evaluate to the null literal.
data Expr = ExprIdent LangIdent
            -- ^ 'LangIdent' when representing a variable.
          | ExprLit LangLit -- ^ Expression wrapping a literal value.
          | ExprFunCall LangIdent Bool [Expr]
          | ExprLambdaCall Lambda [Expr]
          | ExprList [Expr]
            -- ^ An unevaluated list (see 'LitList').
          | ExprRange Expr (Maybe Expr) (Maybe Expr)
            -- ^ An unevaluated range (see 'LitRange').
          | ExprParamExpand LangIdent
            -- ^ Special form of expression that represents a
            -- catch parameter.
            deriving (Show, Eq)


instance ShowSyn Expr where
    showSyn (ExprIdent x) = showSyn x
    showSyn (ExprLit x) = showSyn x
    showSyn (ExprFunCall n asClass es) = (if asClass then "@" else "") ++ showSyn n ++ showSynArgs es
    showSyn (ExprList xs) = showSynList xs
        where showSynList = showSynSep "[" "]" ", "
    showSyn (ExprRange{}) = error "showSyn - cannot show unevaluated range"
    showSyn (ExprLambdaCall x xs) = showSyn (LitLambda x) ++ " : (" ++ showSynArgs xs ++ ")"
    showSyn (ExprParamExpand x) = ".." ++ showSyn x


-- | Represents names that can be assigned values.
--
-- Each name can contain one literal value and one lambda (function).
-- When a name is being resolved, context and constraints expressed
-- by the programmer determine whether a name will resolve to the
-- contained value or lambda.
newtype LangIdent = LangIdent { getIdent :: String }
    deriving (Show, Eq, Ord)

-- | Return 'True' if the identifier consists entirely of valid identifier
-- symbol characters (see 'validSymbolIdentChars').
isSymbolIdent :: LangIdent -> Bool
isSymbolIdent = all (`elem` validSymbolIdentChars) . getIdent

instance ShowSyn LangIdent where
    showSyn = getIdent


instance ShowSyn ArgSig where
    showSyn (ArgSig args catchArg) =
        showSynSep "("
          (case catchArg of
             Nothing -> ")"
             Just x  -> concat
                        [ if not (null args)
                          then ", .."
                          else ".."
                        , showSyn x
                        , ")"]) ", " args


showLambdaFun :: Lambda -> String
showLambdaFun (Lambda {lambdaArgs=args, lambdaBody=body})
    = showSyn args ++ " " ++ showSyn body


-- | True when passed the nullary literal.
isNull :: LangLit -> Bool
isNull LitNull = True
isNull _ = False


-- | True if the type can be enumerated in Angle.
enumType :: LangLit -> Bool
enumType (LitInt _) = True
enumType (LitChar _) = True
enumType (LitFloat _) = True
enumType _ = False


-- | True if all values in list have the specified type.
allType :: LangType -> [LangLit] -> Bool
allType t = all ((==t) . typeOf)
