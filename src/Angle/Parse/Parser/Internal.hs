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
    , langOp
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
-- [@continue@] skips the rest of the body of the current loop and
-- begins the next iteration.
--
-- [@ident = expr@] assigns the value of @expr@ to the variable
-- @ident@.
--
-- [@expr@] executes an expression and produces the value evaluated.
--
-- Or a structure may be used, see 'langStruct'.
singStmt :: Parser st SingStmt
singStmt = try stmtStruct
           <|> try (stmtReturn <* singStmtEnd)
           <|> try (stmtRaise <* singStmtEnd)
           <|> try (stmtBreak <* singStmtEnd)
           <|> try (stmtAssignGlobal <* singStmtEnd)
           <|> try (stmtAssignNonLocal <* singStmtEnd)
           <|> try (stmtAssign <* singStmtEnd)
           <|> stmtExpr   <* singStmtEnd
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


stmtBreak :: Parser st SingStmt
stmtBreak = sBreak <|> sContinue
  where sContinue = string "continue" >> return StmtBreak { breakValue=Nothing, breakContinue=True}
        sBreak = string "break" >> (do
              retV <- optionMaybe (try (tokNSpaced *> expr))
              return StmtBreak { breakValue=retV, breakContinue=False})


-- Check this (manyTill...)
stmtComment :: Parser st SingStmt
stmtComment = StmtComment
              <$> (char '#' *> manyTill anyChar tokEndComment <* tokEndComment)
    where
      tokEndComment = void (char '\n')
                      <|> void tokEOF


stmtStruct :: Parser st SingStmt
stmtStruct = liftM StmtStruct langStruct


stmtReturn :: Parser st SingStmt
stmtReturn = liftM StmtReturn (string "return " *> expr)
               <?> "return construct"


stmtExpr :: Parser st SingStmt
stmtExpr = liftM StmtExpr expr


stmtRaise :: Parser st SingStmt
stmtRaise = string "raise " >> liftM (StmtRaise . getLitKeyword) litKeyword


-- | Language structure.
--
-- Possible forms are:
--
-- [@for ident in expr do stmt@] loops over values produced by
-- @expr@, allowing them to be referenced in @stmt@ by the name
-- @ident@.
--
-- [@while expr do stmt@] while @expr@ evaluates to @true@, will
-- execute @stmt@ then repeat.
--
-- [@if expr then stmt1 {else stmt2}@] if @expr@ evaluates to @true@,
-- executes @stmt1@, otherwise will execute @stmt2@ if it exists, or
-- produce a null value.
--
-- [@unless expr stmt@] if @expr@ evaluates to @false@ then execute
-- @stmt@, otherwise produce a null value.
--
-- [@defun ident(args) stmt@] defines a function @ident@ that has
-- a parameter list @args@ and body @stmt@.
langStruct :: Parser st LangStruct
langStruct =     try structFor
             <|> try structWhile
             <|> try structIf
             <|> try structUnless
             <|> try structDefun
             <|> structTryCatch
             <?> "language construct"


-- | For loop.
structFor :: Parser st LangStruct
structFor = do
  name <- string "for " *> langIdent
  iter <- string " in " *> expr
  body <- string " do " *> stmt
  return $ StructFor name iter body


-- | While loop.
structWhile :: Parser st LangStruct
structWhile = StructWhile
              <$> (string "while " *> expr)
              <*> (string " do "   *> stmt)


-- | Conditional if statement.
--
-- of the form:
--
-- if EXPR then STMT {else STMT}
structIf :: Parser st LangStruct
structIf = StructIf
           <$> (string "if " *> expr)
           <*> (string " then " *> stmt)
           <*> optionMaybe (string "else " *> stmt)


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
  return $ StructIf (ExprOp (MultiOp OpSub [e])) s els


-- | Function definition.
structDefun :: Parser st LangStruct
structDefun = StructDefun
              <$> (string "defun " *> identName)
              <*> (Lambda
                   <$> callList <* tokStmtBetween
                   <*> stmt <*> return Nothing)


-- | Exception handling.
structTryCatch :: Parser st LangStruct
structTryCatch = do
    string "try "
    tryCode <- stmt
    catchers <- many catchSec
    return $ StructTryCatch tryCode catchers
  where exceptionList = tokList $ sepEndBy (liftM getLitKeyword litKeyword) tokEltSep
        singleE = liftM ((:[]) . getLitKeyword) litKeyword
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
          <|> try litNull
          <|> try litChar
          <|> try litClosure
          <|> try litLambda
          <|> try litRange
          <|> try litBool
          <|> try litList
          <|> try litFloat
          <|> try litInt
          <|> litKeyword
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


litKeyword :: Parser st LangLit
litKeyword = liftM LitKeyword (char ':' >> identKeyword)


litLambda :: Parser st LangLit
litLambda = liftM LitLambda lambda


litClosure :: Parser st LangLit
litClosure = char '$' >> liftM LitClosure lambda


exprList :: Parser st Expr
exprList = liftM ExprList (tokList $ sepEndBy expr tokEltSep)


exprRange :: Parser st Expr
exprRange = parens $ do
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
litRange = try $ parens $ do
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
       <|> exprFunIdent
       <|> try exprOp
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


funIdent :: Parser st LangIdent
funIdent = char '$' *> identName


exprFunIdent :: Parser st Expr
exprFunIdent = liftM ExprFunIdent funIdent


-- | Lambdas are unnamed functions which can be assigned to
-- identifiers or passed to functions when used in their literal
-- form.
--
-- @((x y) (+ x y);)@
--
-- References a lambda that performs basic addition.
lambda :: Parser st Lambda
lambda = parens $ do
    args <- callList <* tokNSpaced
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


exprOp :: Parser st Expr
exprOp = liftM ExprOp langOp


-- | Either a special operator or a multi-operator.
--
-- Special operators are prefix and have one operand, i.e., prefix
-- unary operators.
--
-- Multi-operators can take an arbitrary number of arguments (usually
-- at least one) but must be surrounded by parentheses.
langOp :: Parser st LangOp
langOp = multiOp


makeOp :: Parser st a -> Op -> Parser st Op
makeOp sc op = sc >> return op


opAdd, opAnd, opConcat, opDiv, opEq,
  opGreater, opGreaterEq, opLess, opLessEq,
  opMult, opNeg, opNot, opOr, opSub, opExp
  :: Parser st Op
opAdd  = makeOp (char '+')    OpAdd
opAnd  = makeOp (char '&')    OpAnd
opConcat = makeOp (string "++") OpConcat
opDiv  = makeOp (char '/')    OpDiv
opEq   = makeOp (string "==") OpEq
opGreater = makeOp (char '>') OpGreater
opGreaterEq = makeOp (string ">=") OpGreaterEq
opLess = makeOp (char '<')    OpLess
opLessEq = makeOp (string "<=") OpLessEq
opMult = makeOp (char '*')    OpMult
opNeg  = makeOp (char '-')    OpNeg
opNot  = makeOp (char '^')    OpNot
opOr   = makeOp (char '|')    OpOr
opSub  = makeOp (char '-')    OpSub
opExp  = makeOp (string "**") OpExp


userOp :: Parser st Op
userOp = liftM (UserOp . LangIdent) (many1 tokOpChar) <?> "operator"


specOps :: [Parser st Op]
specOps = [opNeg, opNot]


-- |Operators called within parentheses that may have
-- multiple operands
multiOp :: Parser st LangOp
multiOp = (choice (map (try . multOp) multiOps) <|> multOp userOp)
  <?> "operator expression"


-- | List of all the MultiOp parsers.
multiOps :: [Parser st Op]
multiOps = [ opAdd, opAnd, opConcat
           , opExp
           , opDiv, opEq
           , opGreater, opGreaterEq
           , opLess, opLessEq
           , opMult, opOr
           , opNot
           , opSub ]


multOp :: Parser st Op -> Parser st LangOp
multOp sc = MultiOp
            <$> sc
            <*> parens (sepEndBy expr comma)


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
  typ <- argSigType
  name <- identName
  cls <- optionMaybe constrRefArgSig
  return $ ArgElt typ name cls


argSigType :: Parser st AnnType
argSigType = char '$' *> return AnnFun
             <|> char '!'*> return AnnLit
             <|> return AnnAny
