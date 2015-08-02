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
    , litList
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
    , evalScan
    ) where


import Control.Applicative
import Control.Monad.State
import Data.Maybe (isJust)

import Angle.Lex.Helpers
import Angle.Lex.Token
import Angle.Types.Lang

    

-- TODO:
--
-- * Fix missing semi-colon at end of multi-stmt
--   - e.g. {1;2} -> 1, but should error.
-- * Fix if stmt issue
--   - e.g. defun foo(x) {if (> x 1) print("yup"); else print("nope!";}
--    lexes, and it shouldn't!
--    also, it always prints "yup"
-- * Spaces will not be parsed after comments


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
singStmt = many (surrounded whitespace stmtComment) >>
           stmtStruct 
           <|> stmtReturn <* singStmtEnd
           <|> stmtBreak <* singStmtEnd
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
                 

stmtBreak :: Scanner SingStmt
stmtBreak = do
  n <- string "break" <|> string "continue"
  case n of
    "continue" -> return StmtBreak { breakValue=Nothing, breakContinue=True}
    "break" -> do
              retV <- optional (tryScan (tokNSpaced *> expr))
              return StmtBreak { breakValue=retV, breakContinue=False}


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
             <|> structUnless
             <|> structDefun 
             <|> structDefClass
             <?> "language construct"
             
             
-- | For loop.
structFor :: Scanner LangStruct
structFor = do
  name <- string "for " *> langIdent
  iter <- string " in " *> expr
  body <- string " do " *> stmt
  return $ StructFor name iter body


-- | While loop.
structWhile :: Scanner LangStruct
structWhile = StructWhile 
              <$> (string "while " *> expr)
              <*> (string " do "   *> stmt)
         

-- | Conditional if statement.
--
-- of the form:
--
-- if EXPR then STMT {else STMT}
structIf :: Scanner LangStruct
structIf = StructIf
           <$> (string "if " *> expr)
           <*> (string " then " *> stmt)
           <*> optional (string "else " *> stmt)


-- | unless EXPR STMT
--
-- is equivalent to 
-- 
-- if (NOT)EXPR then STMT
structUnless :: Scanner LangStruct
structUnless = do
  e <- string "unless " *> expr
  char ' ' <|> tokNewLine
  s <- stmt
  return $ StructIf (ExprOp (SpecOp OpNot e)) s Nothing
         

structWhen :: Scanner LangStruct
structWhen = do
  e <- string "when " *> expr
  char ' ' <|> tokNewLine
  s <- stmt
  return $ StructIf e s Nothing

        
-- | Function definition.
structDefun :: Scanner LangStruct
structDefun = StructDefun
              <$> (string "defun " *> identName)
              <*> (Lambda
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
          <|> litChar
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
           

exprRange :: Scanner Expr
exprRange = parens $ do
              from <- expr
              string ".."
              to <- expr
              step <- optional . tryScan $ do
                        string ".."
                        expr
              return $ ExprRange from to step


-- | Boolean literal.
litBool :: Scanner LangLit
litBool = liftM LitBool (litTrue <|> litFalse) 
          <?> "boolean literal"
    where litTrue  = string "true" >> return True
          litFalse = string "false" >> return False
                     

litChar :: Scanner LangLit
litChar = liftM LitChar $ surrounded (char '\'') (notChar '\'')
                   

-- TODO: Add additional `step' to ranges (1..7..3)
-- | Dotted range of values.
litRange :: Scanner LangLit
litRange = parens $ do
             from <- langLit
             string ".."
             to <- langLit
             step <- optional . tryScan $ do
                       string ".."
                       langLit
             return $ LitRange from to step
               

-- | Non-valued literal.
litNull :: Scanner LangLit
litNull = string "()" <|> string "null" >> return LitNull


expr :: Scanner Expr
expr = (   tryScan exprLit
       <|> exprList
       <|> exprFunIdent 
       <|> tryScan exprOp
       <|> tryScan exprRange
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
exprLambda = liftM ExprLambda . parens $ do
    args <- callList <* tokSpace
    body <- stmt
    return $ Lambda args body


-- | Set of arguments for a function 
arglist :: Scanner [Expr]
arglist = within tokTupleStart tokTupleEnd (sepWith tokEltSep (expr <|> exprParamExpand))
          

exprParamExpand :: Scanner Expr
exprParamExpand = liftM ExprParamExpand $ string ".." >> langIdent


-- callList :: Scanner ArgSig
-- callList = parens $ do
--     params  <- sepWith tokEltSep identName
--     catcher <- optional (string ".." *> identName)
--     return $ ArgSig params catcher
  

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

opAdd, opAnd, opConcat, opDiv, opEq,
  opGreater, opGreaterEq, opLess, opLessEq,
  opMult, opNeg, opNot, opOr, opSub
  :: Scanner Op
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
multiOp = parens (choice (map (tryScan . multOp) multiOps)
                  <|> multOp userOp) <?> "operator expression"
          

-- | List of all the MultiOp scanners.
multiOps :: [Scanner Op]
multiOps = [ opAdd, opAnd, opConcat
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


classRef :: Scanner ClassRef
classRef = liftM ClassRef $ char '@' >> langIdent
           

structDefClass :: Scanner LangStruct
structDefClass = do
  string "defclass "
  name <- langIdent
  arg <- callList
  body <- stmt
  return $ StructDefClass name (Lambda arg body)


classRefArgSig :: Scanner ClassRef
classRefArgSig = tryScan (char ':' >> classRef)

callList :: Scanner ArgSig
callList = parens $ do
    params  <- sepWith tokEltSep argElt
    catcher <- optional (string ".." *> identName)
    return $ ArgSig params catcher


argElt :: Scanner ArgElt
argElt = do
  typ <- argSigType
  name <- identName
  cls <- optional classRefArgSig
  return $ ArgElt typ name cls


argSigType :: Scanner AnnType
argSigType = char '@' *> return AnnClass
             <|> char '$' *> return AnnFun
             <|> return AnnLit
