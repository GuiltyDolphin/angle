{-# LANGUAGE MultiParamTypeClasses #-}
{-|
Module      : Angle.Parse.Parser.Internal
Description : Module defining functions for parsing Angle code.
Copyright   : Copyright (C) 2015 Ben Moon
License     : GNU GPL, version 3
Maintainer  : GuiltyDolphin@gmail.com
Stability   : alpha

Uses parsers from "Angle.Parse.Token" and "Angle.Parse.Helpers", as well
as custom lexing functions to define all the necessary functions for
converting Angle source code into Haskell types.
-}
module Angle.Parse.Parser.Internal
    ( program
    , evalParse

    -- ** Literals
    , lambda
    , langLit
    , litBool
    , litInt
    , litList
    , litNull
    , litRange
    , litStr

    -- ** Statements and structures
    , langStruct
    , singStmt
    , stmt

    -- ** Expressions
    , expr
    , exprFunCall
    ) where


import Control.Applicative ((<*), (<*>), (*>), (<$>), liftA)
import Control.Monad.State

import Text.Parsec

import Angle.Parse.Helpers
import Angle.Parse.Token
import Angle.Types.Lang


-- | Single statement or a multi-statement.
--
-- See 'singleStmt' for the syntax of singular statments.
--
-- Multi-statements are declared by surrounding a series
-- of statements with braces.
--
-- For example: a series of statements that swaps the values
-- of two variables might be:
--
-- @
-- {
--   c = x;
--   x = y;
--   y = c;
-- }
-- @
--
--  Which would swap he values of @x@ and @y@, and the value produced by
--  the series of statements would be the value of the last (value of
--  @c@).
stmt :: Parser st Stmt
stmt = (multiStmt <|> singleStmt) <?> "statement"


singleStmt :: Parser st Stmt
singleStmt = do
  initPos <- getPosition -- liftM sourcePos get
  res <- singStmt
  endPos <- getPosition -- liftM sourcePos get
  return SingleStmt
             { stmtSingStmt  = res
             , stmtSourcePos = SourceRef (initPos, endPos)
             }


-- | Statement consisting of zero or more statements.
multiStmt :: Parser st Stmt
multiStmt = MultiStmt <$> between tokMultiStmtStart tokMultiStmtEnd (many (try stmt))


-- | Singular statement.
--
-- Takes the following forms:
--
-- [@return expr@] exits early from a function and produces the
-- value that @expr@ evaluates to.
--
-- [@break {expr}@] exits early from a loop, and produces the value
-- @expr@ if supplied, or the last value present in the loop.
--
-- [@ident = expr@] assigns the value of @expr@ to the variable
-- @ident@.
--
-- [@expr@] executes an expression and produces the value evaluated.
--
-- Or a structure may be used, see 'langStruct'.
singStmt :: Parser st SingStmt
singStmt = try stmtStruct
           <|> try (stmtExpr   <* singStmtEnd)
           <|> try (stmtAssignGlobal <* singStmtEnd)
           <|> try (stmtAssignNonLocal <* singStmtEnd)
           <|> try (stmtAssign <* singStmtEnd)
           <?> "statement"


singStmtEnd :: Parser st ()
singStmtEnd = surrounded spaces (void (char ';')) <?> "end of statement"


-- | Variable assignment.
stmtAssign :: Parser st SingStmt
stmtAssign = StmtAssign
             <$> try (langIdent <* tokAssign)
             <*> expr


stmtAssignNonLocal :: Parser st SingStmt
stmtAssignNonLocal = StmtAssignNonLocal
                     <$> try (langIdent <* tokAssignNonLocal)
                     <*> expr


stmtAssignGlobal :: Parser st SingStmt
stmtAssignGlobal = StmtAssignGlobal
                     <$> try (langIdent <* tokAssignGlobal)
                     <*> expr


-- Check this (manyTill...)
stmtComment :: Parser st SingStmt
stmtComment = StmtComment
              <$> (char '#' *> manyTill anyChar tokEndComment <* tokEndComment)
    where
      tokEndComment = void (char '\n')
                      <|> void tokEOF


stmtStruct :: Parser st SingStmt
stmtStruct = liftM StmtStruct langStruct


stmtExpr :: Parser st SingStmt
stmtExpr = liftM StmtExpr expr


-- | Language structure.
--
-- Possible forms are:
--
-- [@for ident in expr do stmt@] loops over values produced by
-- @expr@, allowing them to be referenced in @stmt@ by the name
-- @ident@.
--
-- [@if expr then stmt1 {else stmt2}@] if @expr@ evaluates to @true@,
-- executes @stmt1@, otherwise will execute @stmt2@ if it exists, or
-- produce a null value.
--
-- [@unless expr stmt@] if @expr@ evaluates to @false@ then execute
-- @stmt@, otherwise produce a null value.
langStruct :: Parser st LangStruct
langStruct =     try structIf
             <|> try structUnless
             <|> structTryCatch
             <?> "language construct"


-- | Conditional if statement.
--
-- of the form:
--
-- if EXPR then STMT {else STMT}
structIf :: Parser st LangStruct
structIf = StructIf
           <$> (string "if " *> expr)
           <*> (surrounded spaces (string "then") *> stmt)
           <*> optionMaybe (try (string "else " *> stmt))


-- | unless EXPR STMT
--
-- is equivalent to
--
-- if (NOT)EXPR then STMT
structUnless :: Parser st LangStruct
structUnless = do
  e <- string "unless " *> expr
  tokNSpaced
  s <- stmt
  els <- optionMaybe $ string "else " *> stmt
  return $ StructIf (ExprFunCall (LangIdent "^") False [e]) s els


-- | Exception handling.
structTryCatch :: Parser st LangStruct
structTryCatch = do
    string "try "
    tryCode <- stmt
    catchers <- many catchSec
    return $ StructTryCatch tryCode catchers
  where exceptionList = tokList $ sepEndBy langLit tokEltSep
        singleE = liftM (:[]) langLit
        catchSec = do
          string "catch "
          toCatch <- singleE <|> exceptionList
          exceptCode <- stmt
          return (toCatch, exceptCode)


-- | A program consists of a series of statements.
program :: Parser st Stmt
program = liftM MultiStmt $ followed tokEOF (many stmt)


exprLit :: Parser st Expr
exprLit = liftM ExprLit langLit


-- | Language literals.
langLit :: Parser st LangLit
langLit = try litStr
          <|> try litLambda
          <|> try litNull
          <|> try litChar
          <|> try litClosure
          <|> try litRange
          <|> try litBool
          <|> try litList
          <|> try litFloat
          <|> try litInt
          <?> "literal"


-- | A literal string.
litStr :: Parser st LangLit
litStr = liftM LitStr tokString


-- | Denary integer.
litInt :: Parser st LangLit
litInt = liftA LitInt tokInt <?> "integer literal"


-- | Floating-point literal.
litFloat :: Parser st LangLit
litFloat = liftM LitFloat tokFloat
           <?> "floating-point literal"


-- | Multi-type list.
litList :: Parser st LangLit
litList = liftM LitList (tokList $ sepEndBy langLit tokEltSep)
          <?> "list literal"


litLambda :: Parser st LangLit
litLambda = liftM LitLambda lambda


litClosure :: Parser st LangLit
litClosure = char '$' >> liftM LitClosure lambda


exprList :: Parser st Expr
exprList = liftM ExprList (tokList $ sepEndBy expr tokEltSep)


exprRange :: Parser st Expr
exprRange = brackets $ do
              from <- expr
              string ".."
              to <- optionMaybe expr
              step <- optionMaybe . try $ do
                        string ".."
                        expr
              return $ ExprRange from to step


-- | Boolean literal.
--
-- litBool = true | false ;
litBool :: Parser st LangLit
litBool = liftM LitBool (litTrue <|> litFalse)
          <?> "boolean literal"
    where litTrue  = string "true" >> return True
          litFalse = string "false" >> return False


litChar :: Parser st LangLit
litChar = liftM LitChar tokChar


-- | Dotted range of values.
--
-- Ranges have a single mandatory part and two optional parts, in the
-- form @(start..[stop][..step])@.
--
-- Start is the value with which the range will begin. @(1..)@
-- represents a range starting with the integer 1 and enumerating
-- to infinity.
--
-- Stop is the value at which the range will stop enumerating when
-- it is equal to or higher than the specified value.
-- @(1..50)@ represents a range that can produce the numerical
-- values 1 through 50.
--
-- Step is the optional increment of the range, for non-integers, such
-- as characters, the range step is the positional difference between
-- the characters.
-- @(1..10..2)@ represents a range that will enumerate 1 through
-- 10, but incrementing by 2 on each enumeration.
--
-- The following are all valid forms of ranges:
--
-- [@(start..)@] range from start to maximum bound of type.
--
-- [@(start..stop)@] range from start to stop.
--
-- [@(start..stop..step)@] range from start to stop, incrementing by
-- step.
--
-- [@(start....step)@] range from start to maxmimum bound of type,
-- incrementing by step.
litRange :: Parser st LangLit
litRange = try $ brackets $ do
             from <- langLit
             string ".."
             to <- optionMaybe langLit
             step <- optionMaybe . try $ do
                                string ".."
                                langLit
             return $ LitRange from to step


-- | Non-valued literal.
--
-- litNull = 'null' | '()' ;
litNull :: Parser st LangLit
litNull = (string "()" <|> string "null") >> return LitNull


-- | Expression.
expr :: Parser st Expr
expr = (   try exprLit
       <|> exprList
       <|> try exprRange
       <|> exprFunCall
       <|> exprIdent)
       <?> "expression"


exprIdent :: Parser st Expr
exprIdent = liftM ExprIdent langIdent <?> "identifier"


identName :: Parser st LangIdent
identName = liftM LangIdent (try $ ident False) <?> "identifier"


identKeyword :: Parser st LangIdent
identKeyword = liftM LangIdent (try $ ident True)


langIdent :: Parser st LangIdent
langIdent = identName


-- | Lambdas are unnamed functions which can be assigned to
-- identifiers or passed to functions when used in their literal
-- form.
--
-- @((x y) (+ x y);)@
--
-- References a lambda that performs basic addition.
lambda :: Parser st Lambda
lambda = do
    args <- callList <* spaces
    symbol "->"
    body <- stmt
    return $ Lambda args body Nothing


-- | Set of arguments for a function
arglist :: Parser st [Expr]
arglist = parens (sepEndBy (expr <|> exprParamExpand) tokEltSep)


exprParamExpand :: Parser st Expr
exprParamExpand = liftM ExprParamExpand $ string ".." >> langIdent


-- | Function call.
--
-- Function calls consist of an identifier followed
-- by an argument list surrounded by parentheses (see 'arglist').
exprFunCall :: Parser st Expr
exprFunCall = (do
  asClass <- (char '@' >> return True) <|> return False
  name <- try (langIdent <* lookAhead (char '('))
  args <- arglist
  return $ ExprFunCall name asClass args) <?> "function call"


constrRef :: Parser st ConstrRef
constrRef = char '@' >> liftM2 ConstrRef langIdent (optionMaybe arglist)


constrRefArgSig :: Parser st ConstrRef
constrRefArgSig = try (char ':' >> constrRef)


callList :: Parser st ArgSig
callList = parens $ do
    params  <- sepEndBy argElt tokEltSep
    catcher <- optionMaybe catchArg
    return $ ArgSig params catcher


catchArg :: Parser st CatchArg
catchArg = do
    string ".."
    n <- identName
    constr <- optionMaybe constrRefArgSig
    return $ CatchArg n constr


argElt :: Parser st ArgElt
argElt = do
  name <- identName
  cls <- optionMaybe constrRefArgSig
  return $ ArgElt name cls


