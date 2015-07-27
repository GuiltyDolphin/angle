{-# LANGUAGE MultiParamTypeClasses #-}
module Angle.Lex.Lexer 
    ( program
    , stmt
      -- TODO: Exporting these for tests, check this
    , litStr
    , litInt
    , litFloat
    , litBool
    , litRange
    , litNull
    , langOp
    , exprIdent
    , exprLit
    , exprFunCall
    , exprOp
    , langLit
    , langStruct
    , singStmt
    , expr
    , structDefun
    ) where


import Control.Applicative
import Control.Monad.State

import Angle.Lex.Helpers
import Angle.Lex.Token
import Angle.Types.Lang


stmt :: Scanner Stmt
stmt = (multiStmt <|> singleStmt) <?> "statement"
       

singleStmt :: Scanner Stmt
singleStmt = do
  initPos <- liftM sourcePos get
  res <- singStmt
  endPos <- liftM sourcePos get
  return SingleStmt 
             { stmtSingStmt  = res
             , stmtSourcePos = SourceRef (initPos, endPos)
             }


-- TODO: Is it wise to allow empty multi-statements?
-- | Statement consisting of zero or more statements.
multiStmt :: Scanner Stmt
multiStmt = MultiStmt <$> within tokMultiStmtStart tokMultiStmtEnd (many stmt)


-- TODO: Last statement in a multi-statement block, or at
-- end of file, shouldn't need to have a newline or semi-colon
singStmt :: Scanner SingStmt
singStmt = stmtComment
           <|> stmtStruct 
           <|> stmtReturn <* singStmtEnd
           <|> stmtAssign <* singStmtEnd
           <|> stmtExpr   <* singStmtEnd
           <?> "statement"


singStmtEnd :: Scanner ()
singStmtEnd = surrounded whitespace (void (char ';')) <?> "end of statement"
              -- <|> void (char '\n')
              -- <|> void (lookAhead tokMultiStmtEnd)
              -- <|> lookAhead tokEOF
               

-- | Variable assignment.
stmtAssign :: Scanner SingStmt
stmtAssign = StmtAssign 
             <$> tryScan (langIdent <* tokAssign) 
             <*> expr


stmtComment :: Scanner SingStmt
stmtComment = StmtComment 
              <$> (char '#' *> manyTill' tokEndComment anyChar)
    where 
      tokEndComment = void (char '\n') 
                      <|> void tokEOF 
                -- <|> void (string "-#")
                -- <?> "end of comment"
                  

stmtStruct :: Scanner SingStmt 
stmtStruct = liftM StmtStruct langStruct


stmtReturn :: Scanner SingStmt
stmtReturn = liftM StmtReturn (string "return " *> expr) 
               <?> "return construct"


stmtExpr :: Scanner SingStmt
stmtExpr = liftM StmtExpr expr

                  
langStruct :: Scanner LangStruct
langStruct =     structFor 
             <|> structWhile 
             <|> structIf 
             <|> structDefun 
             <?> "language construct"
             
             
-- | For loop.
structFor :: Scanner LangStruct
structFor = do
  string "for "
  name <- langIdent
  string " in "
  iter <- expr
  string " do "
  body <- stmt
  return $ StructFor name iter body


-- | While loop.
structWhile :: Scanner LangStruct
structWhile = StructWhile 
              <$> (string "while " *> expr)
              <*> (string " do "   *> stmt)
         

-- | Conditional if statement.
structIf :: Scanner LangStruct
structIf = StructIf
           <$> (string "if " *> expr)
           <*> (string " then " *> stmt)
           <*> optional (string "else " *> stmt)

        
-- | Function definition.
structDefun :: Scanner LangStruct
structDefun = StructDefun
              <$> (string "defun " *> identName)
              <*> (CallSig 
                   <$> callList <* tokStmtBetween
                   <*> stmt)


program :: Scanner Stmt
program = liftM MultiStmt $ followed tokEOF (many stmt)
          

exprLit :: Scanner Expr
exprLit = liftM ExprLit langLit
               

-- | Language literals.
langLit :: Scanner LangLit
langLit = litStr 
          <|> litNull
          <|> litRange 
          <|> litBool 
          <|> litList 
          <|> tryScan litFloat 
          <|> litInt 
          <?> "literal"


-- | A literal string.
litStr :: Scanner LangLit
litStr = liftM LitStr tokString <?> "string literal"


-- | Denary integer.
litInt :: Scanner LangLit
litInt = liftA LitInt tokInt <?> "integer literal"
         

-- | Floating-point literal.
litFloat :: Scanner LangLit
litFloat = liftM LitFloat tokFloat 
           <?> "floating-point literal"
             

-- | Multi-type list.
litList :: Scanner LangLit
litList = liftM LitList (tokList $ sepWith tokEltSep langLit) 
          <?> "list literal"
              

exprList :: Scanner Expr
exprList = liftM ExprList (tokList $ sepWith tokEltSep expr)


-- | Boolean literal.
litBool :: Scanner LangLit
litBool = liftM LitBool (litTrue <|> litFalse) 
          <?> "boolean literal"
    where litTrue  = tokTrue  >> return True
          litFalse = tokFalse >> return False
                   

-- TODO: Add additional `step' to ranges (1..7..3)
-- | Dotted range of values.
litRange = parens (LitRange 
                   <$> (expr <* tokRangeSep) 
                   <*> expr)
           <?> "range literal"
               

-- | Non-valued literal.
litNull :: Scanner LangLit
litNull = string "()" <|> string "null" >> return LitNull


expr :: Scanner Expr
expr = (   tryScan exprLit
       <|> exprList
       <|> exprFunIdent 
       <|> tryScan exprOp
       <|> exprLambda
       <|> exprFunCall 
       <|> exprIdent)
       <?> "expression"
       

exprIdent :: Scanner Expr
exprIdent = liftM ExprIdent langIdent <?> "identifier"
           

identName :: Scanner LangIdent
identName = liftM LangIdent (tryScan ident) <?> "identifier"
            

langIdent :: Scanner LangIdent
langIdent = identName
            

funIdent :: Scanner LangIdent
funIdent = char '$' *> identName
           

exprFunIdent :: Scanner Expr
exprFunIdent = liftM ExprFunIdent funIdent
         

exprLambda :: Scanner Expr
exprLambda = liftM ExprLambda $ do
  char '('
  args <- callList
  tokSpace
  body <- stmt
  char ')'
  return $ CallSig args body


-- | Set of arguments for a function 
arglist :: Scanner [Expr]
arglist = within tokTupleStart tokTupleEnd (sepWith tokEltSep expr)


callList :: Scanner ArgSig
callList = do
  tokTupleStart
  params <- sepWith tokEltSep identName
  catcher <- optional (string ".." *> identName)
  tokTupleEnd
  return $ ArgSig params catcher
  

exprFunCall :: Scanner Expr
exprFunCall = ExprFunCall 
              <$> tryScan (langIdent <* lookAhead (char '(')) 
              <*> arglist 
              <?> "function call"
              

exprOp :: Scanner Expr
exprOp = liftM ExprOp langOp -- <?> "operation"
                       

langOp :: Scanner LangOp
langOp = specOp <|> multiOp -- <?> "operation"


makeOp :: Scanner a -> Op -> Scanner Op
makeOp sc op = sc >> return op


-- TODO: This conflicts with literal -ve for integers,
-- either make it so that literals use the operator (probably
-- slower) or resolve the conflict!
-- Probably fixed.

opAdd, opAnd, opDiv, opEq,
  opGreater, opGreaterEq, opLess, opLessEq,
  opMult, opNeg, opNot, opOr, opSub
  :: Scanner Op
opAdd  = makeOp (char '+')    OpAdd
opAnd  = makeOp (char '&')    OpAnd
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


userOp :: Scanner Op
userOp = liftM (UserOp . LangIdent) (some tokOpChar) <?> "operator"

       
-- | Special operators that can be used outside of
-- parentheses in a prefix form.
specOp :: Scanner LangOp
specOp = choice (map preOp specOps) <?> "special operator"
                       

specOps :: [Scanner Op]
specOps = [opNeg, opNot]
          

preOp :: Scanner Op -> Scanner LangOp
preOp sc = do
  op  <- sc
  opr <- expr
  return $ SpecOp op opr
         

-- |Operators called within parentheses that may have
-- multiple operands
multiOp :: Scanner LangOp
multiOp = parens (tryScan (choice (map multOp multiOps))
                  <|> multOp userOp) <?> "operator expression"
          

-- | List of all the MultiOp scanners.
multiOps :: [Scanner Op]
multiOps = [ opAdd, opAnd
           , opDiv, opEq
           , opGreater, opGreaterEq
           , opLess, opLessEq
           , opMult, opOr
           , opSub ]
           

multOp :: Scanner Op -> Scanner LangOp
multOp sc = MultiOp
            <$> (sc <* tokSpace)
            <*> sepWith (some tokWhitespace) expr


-- TODO:
-- Things to add
--  Passing functions as arguments:
--   foo($x, y, $z) -> x and z passed as functions
--  Lambdas
--   ((x,y,z) (+ x y z)) -> x y z arguments, then body
--   Can pass as arguments:
--   foo($((x) (+ x 1))) -> lambda: $((x) (+ x 1)) passed as arg.
