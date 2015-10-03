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


import Control.Applicative
import Control.Monad.State

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
stmt :: Parser Stmt
stmt = (multiStmt <|> singleStmt) <?> "statement"


singleStmt :: Parser Stmt
singleStmt = do
  many $ surrounded whitespace stmtComment
  initPos <- liftM sourcePos get
  res <- singStmt
  endPos <- liftM sourcePos get
  return SingleStmt
             { stmtSingStmt  = res
             , stmtSourcePos = SourceRef (initPos, endPos)
             }


-- | Statement consisting of zero or more statements.
multiStmt :: Parser Stmt
multiStmt = MultiStmt <$> within tokMultiStmtStart tokMultiStmtEnd (many (tryParse stmt))


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
singStmt :: Parser SingStmt
singStmt = stmtStruct
           <|> stmtReturn <* singStmtEnd
           <|> stmtRaise <* singStmtEnd
           <|> stmtBreak <* singStmtEnd
           <|> stmtAssign <* singStmtEnd
           <|> stmtExpr   <* singStmtEnd
           <?> "statement"


singStmtEnd :: Parser ()
singStmtEnd = surrounded whitespace (void (char ';')) <?> "end of statement"


-- | Variable assignment.
stmtAssign :: Parser SingStmt
stmtAssign = StmtAssign
             <$> tryParse (langIdent <* tokAssign)
             <*> expr


stmtBreak :: Parser SingStmt
stmtBreak = sBreak <|> sContinue
  where sContinue = string "continue" >> return StmtBreak { breakValue=Nothing, breakContinue=True}
        sBreak = string "break" >> (do
              retV <- optional (tryParse (tokNSpaced *> expr))
              return StmtBreak { breakValue=retV, breakContinue=False})


stmtComment :: Parser SingStmt
stmtComment = StmtComment
              <$> (char '#' *> manyTill tokEndComment anyChar <* tokEndComment)
    where
      tokEndComment = void (char '\n')
                      <|> void tokEOF


stmtStruct :: Parser SingStmt
stmtStruct = liftM StmtStruct langStruct


stmtReturn :: Parser SingStmt
stmtReturn = liftM StmtReturn (string "return " *> expr)
               <?> "return construct"


stmtExpr :: Parser SingStmt
stmtExpr = liftM StmtExpr expr


stmtRaise :: Parser SingStmt
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
langStruct :: Parser LangStruct
langStruct =     structFor
             <|> structWhile
             <|> structIf
             <|> structUnless
             <|> structDefun
             <|> structTryCatch
             <?> "language construct"


-- | For loop.
structFor :: Parser LangStruct
structFor = do
  name <- string "for " *> langIdent
  iter <- string " in " *> expr
  body <- string " do " *> stmt
  return $ StructFor name iter body


-- | While loop.
structWhile :: Parser LangStruct
structWhile = StructWhile
              <$> (string "while " *> expr)
              <*> (string " do "   *> stmt)


-- | Conditional if statement.
--
-- of the form:
--
-- if EXPR then STMT {else STMT}
structIf :: Parser LangStruct
structIf = StructIf
           <$> (string "if " *> expr)
           <*> (string " then " *> stmt)
           <*> optional (string "else " *> stmt)


-- | unless EXPR STMT
--
-- is equivalent to
--
-- if (NOT)EXPR then STMT
structUnless :: Parser LangStruct
structUnless = do
  e <- string "unless " *> expr
  tokNSpaced
  s <- stmt
  els <- optional $ string "else " *> stmt
  return $ StructIf (ExprOp (SpecOp OpNot e)) s els


-- | Function definition.
structDefun :: Parser LangStruct
structDefun = StructDefun
              <$> (string "defun " *> identName)
              <*> (Lambda
                   <$> callList <* tokStmtBetween
                   <*> stmt)


-- | Exception handling.
structTryCatch :: Parser LangStruct
structTryCatch = do
    string "try "
    tryCode <- stmt
    catchers <- many catchSec
    return $ StructTryCatch tryCode catchers
  where exceptionList = tokList $ sepWith tokEltSep (liftM getLitKeyword litKeyword)
        singleE = liftM ((:[]) . getLitKeyword) litKeyword
        catchSec = do
          string "catch "
          toCatch <- singleE <|> exceptionList
          exceptCode <- stmt
          return (toCatch, exceptCode)


-- | A program consists of a series of statements.
program :: Parser Stmt
program = liftM MultiStmt $ followed tokEOF (many stmt)


exprLit :: Parser Expr
exprLit = liftM ExprLit langLit


-- | Language literals.
langLit :: Parser LangLit
langLit = litStr
          <|> litNull
          <|> litChar
          <|> tryParse litLambda
          <|> litRange
          <|> litBool
          <|> litList
          <|> tryParse litFloat
          <|> litInt
          <|> litKeyword
          <?> "literal"


-- | A literal string.
litStr :: Parser LangLit
litStr = liftM LitStr tokString


-- | Denary integer.
litInt :: Parser LangLit
litInt = liftA LitInt tokInt <?> "integer literal"


-- | Floating-point literal.
litFloat :: Parser LangLit
litFloat = liftM LitFloat tokFloat
           <?> "floating-point literal"


-- | Multi-type list.
litList :: Parser LangLit
litList = liftM LitList (tokList $ sepWith tokEltSep langLit)
          <?> "list literal"


litKeyword :: Parser LangLit
litKeyword = liftM LitKeyword (char ':' >> identName)


litLambda :: Parser LangLit
litLambda = liftM LitLambda lambda


exprList :: Parser Expr
exprList = liftM ExprList (tokList $ sepWith tokEltSep expr)


exprRange :: Parser Expr
exprRange = parens $ do
              from <- expr
              string ".."
              to <- optional expr
              step <- optional . tryParse $ do
                        string ".."
                        expr
              return $ ExprRange from to step


-- | Boolean literal.
--
-- litBool = true | false ;
litBool :: Parser LangLit
litBool = liftM LitBool (litTrue <|> litFalse)
          <?> "boolean literal"
    where litTrue  = string "true" >> return True
          litFalse = string "false" >> return False


litChar :: Parser LangLit
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
litRange :: Parser LangLit
litRange = tryParse $ parens $ do
             from <- langLit
             string ".."
             to <- optional langLit
             step <- optional . tryParse $ do
                                string ".."
                                langLit
             return $ LitRange from to step


-- | Non-valued literal.
--
-- litNull = 'null' | '()' ;
litNull :: Parser LangLit
litNull = string "()" <|> string "null" >> return LitNull


-- | Expression.
expr :: Parser Expr
expr = (   tryParse exprLit
       <|> exprList
       <|> exprFunIdent
       <|> tryParse exprOp
       <|> tryParse exprRange
       <|> exprFunCall
       <|> exprIdent)
       <?> "expression"


exprIdent :: Parser Expr
exprIdent = liftM ExprIdent langIdent <?> "identifier"


identName :: Parser LangIdent
identName = liftM LangIdent (tryParse ident) <?> "identifier"


langIdent :: Parser LangIdent
langIdent = identName


funIdent :: Parser LangIdent
funIdent = char '$' *> identName


exprFunIdent :: Parser Expr
exprFunIdent = liftM ExprFunIdent funIdent


-- | Lambdas are unnamed functions which can be assigned to
-- identifiers or passed to functions when used in their literal
-- form.
--
-- @((x y) (+ x y);)@
--
-- References a lambda that performs basic addition.
lambda :: Parser Lambda
lambda = parens $ do
    args <- callList <* tokNSpaced
    body <- stmt
    return $ Lambda args body


-- | Set of arguments for a function
arglist :: Parser [Expr]
arglist = parens (sepWith tokEltSep (expr <|> exprParamExpand))


exprParamExpand :: Parser Expr
exprParamExpand = liftM ExprParamExpand $ string ".." >> langIdent


-- | Function call.
--
-- Function calls consist of an identifier followed
-- by an argument list surrounded by parentheses (see 'arglist').
exprFunCall :: Parser Expr
exprFunCall = (do
  asClass <- (char '@' >> return True) <|> return False
  name <- tryParse (langIdent <* lookAhead (char '('))
  args <- arglist
  return $ ExprFunCall name asClass args) <?> "function call"


exprOp :: Parser Expr
exprOp = liftM ExprOp langOp


-- | Either a special operator or a multi-operator.
--
-- Special operators are prefix and have one operand, i.e., prefix
-- unary operators.
--
-- Multi-operators can take an arbitrary number of arguments (usually
-- at least one) but must be surrounded by parentheses.
langOp :: Parser LangOp
langOp = specOp <|> multiOp


makeOp :: Parser a -> Op -> Parser Op
makeOp sc op = sc >> return op


opAdd, opAnd, opConcat, opDiv, opEq,
  opGreater, opGreaterEq, opLess, opLessEq,
  opMult, opNeg, opNot, opOr, opSub, opExp
  :: Parser Op
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


userOp :: Parser Op
userOp = liftM (UserOp . LangIdent) (some tokOpChar) <?> "operator"


-- | Special operators that can be used outside of
-- parentheses in a prefix form.
specOp :: Parser LangOp
specOp = choice (map preOp specOps) <?> "special operator"


specOps :: [Parser Op]
specOps = [opNeg, opNot]


preOp :: Parser Op -> Parser LangOp
preOp sc = do
  op  <- sc
  opr <- expr
  return $ SpecOp op opr


-- |Operators called within parentheses that may have
-- multiple operands
multiOp :: Parser LangOp
multiOp = parens (choice (map (tryParse . multOp) multiOps)
                  <|> multOp userOp) <?> "operator expression"


-- | List of all the MultiOp parsers.
multiOps :: [Parser Op]
multiOps = [ opAdd, opAnd, opConcat
           , opExp
           , opDiv, opEq
           , opGreater, opGreaterEq
           , opLess, opLessEq
           , opMult, opOr
           , opSub ]


multOp :: Parser Op -> Parser LangOp
multOp sc = MultiOp
            <$> (sc <* tokSpace)
            <*> sepWith (some tokWhitespace) expr


constrRef :: Parser ConstrRef
constrRef = liftM ConstrRef $ char '@' >> langIdent


constrRefArgSig :: Parser ConstrRef
constrRefArgSig = tryParse (char ':' >> constrRef)


callList :: Parser ArgSig
callList = parens $ do
    params  <- sepWith tokEltSep argElt
    catcher <- optional catchArg -- optional (string ".." *> identName)
    return $ ArgSig params catcher


catchArg :: Parser CatchArg
catchArg = do
    string ".."
    n <- identName
    constr <- optional constrRefArgSig
    return $ CatchArg n constr


argElt :: Parser ArgElt
argElt = do
  typ <- argSigType
  name <- identName
  cls <- optional constrRefArgSig
  return $ ArgElt typ name cls


argSigType :: Parser AnnType
argSigType = char '$' *> return AnnFun
             <|> char '!'*> return AnnLit
             <|> return AnnAny
