{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
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

-- Write this!
import Angle.Lex.Helpers
import Angle.Types.Lang
import Angle.Lex.Token
import Control.Monad.State
import Control.Applicative


stmt :: Scanner Stmt
stmt = (multiStmt <|> singleStmt) <?> "statement"
       
-- singleStmt :: Scanner Stmt
-- singleStmt = liftM SingleStmt singStmt
             
singleStmt :: Scanner Stmt
singleStmt = do
  initPos <- liftM sourcePos get
  res <- singStmt
  endPos <- liftM sourcePos get
  return SingleStmt { stmtSingStmt = res
                    , stmtSourcePos = SourceRef (initPos, endPos)
                    }

-- |Statement consisting of zero or more statements
-- >>> evalScan "{}" multiStmt
-- Right (...[])
--
-- >>> evalScan "{1;3}" multiStmt
-- Right (...[...1...3...])
--
-- TODO: Is it wise to allow empty multi-statements?
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

singStmtEnd = surrounded whitespace (void (char ';')) <?> "end of statement"
              -- <|> void (char '\n')
              -- <|> void (lookAhead tokMultiStmtEnd)
              -- <|> lookAhead tokEOF
               
-- |Variable assignment
-- >>> evalScan "x=5" stmtAssign
-- Right (..."x"...5...)
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
             
             
-- |For loop
-- >>> evalScan "for x in (2..8) do {}" structFor
-- Right (..."x"...2...8...[]...)
structFor :: Scanner LangStruct
structFor = do
  string "for "
  name <- langIdent
  string " in "
  iter <- expr
  string " do "
  body <- stmt
  return $ StructFor name iter body

-- |While loop
-- >>> evalScan "while true do {}" structWhile
-- Right (...True...[]...)         
structWhile :: Scanner LangStruct
structWhile = StructWhile 
              <$> (string "while " *> expr)
              <*> (string " do "   *> stmt)
         
-- |Conditional if statement
-- >>> evalScan "if true then {}" structIf
-- Right (...True...[]...Nothing)
--
-- >>> evalScan "if false then {} else {}" structIf
-- Right (...False...[]...Just...[]...)
structIf :: Scanner LangStruct
structIf = StructIf
           <$> (string "if " *> expr)
           <*> (string " then " *> stmt)
           <*> optional (string "else " *> stmt)

        
-- |Function definition
-- >>> evalScan "defun foo(x) { print(x) }" structDefun
-- Right (..."foo"..."x"...)
structDefun :: Scanner LangStruct
structDefun = StructDefun
              <$> (string "defun " *> identName)
              <*> (CallSig 
                   <$> callList' <* tokStmtBetween
                   <*> stmt)

program :: Scanner Stmt
program = liftM MultiStmt $ followed tokEOF (many stmt)
          
program' = liftM MultiStmt $ followed tokEOF (some stmt)
  
  -- sepWith (some tokNewLine) stmt

exprLit = liftM ExprLit langLit

               

-- |Language literals
langLit :: Scanner LangLit
langLit = litStr 
          <|> litNull
          <|> litRange 
          <|> litBool 
          <|> litList 
          <|> tryScan litFloat 
          <|> litInt 
          <?> "literal"

-- |A literal string
-- >>> evalScan "\"test\"" litStr
-- Right (... "test")
--
-- >>> evalScan "test" litStr
-- Left ...
-- ...
litStr :: Scanner LangLit
litStr = liftM LitStr tokString <?> "string literal"

-- |Denary integer
-- >>> evalScan "123" litInt
-- Right (... 123)
--
-- >>> evalScan "test" litInt
-- Left ...
-- ...
litInt :: Scanner LangLit
litInt = liftA LitInt tokInt <?> "integer literal"
-- litInt = liftM (LitInt . read) (some tokDenaryDigit) <?> "integer literal"
         

-- |Floating-point literal
-- >>> evalScan "12.3" litFloat
-- Right (...12.3)
litFloat :: Scanner LangLit
litFloat = liftM LitFloat tokFloat 
           <?> "floating-point literal"
             

-- |Multi-type list
-- >>> evalScan "[1,\"hello\",true]" litList
-- Right (... [... 1...,... "hello"...,... True...])
--
-- >>> evalScan "1,\"hello\",true" litList
-- Left ...
-- ...
litList :: Scanner LangLit
litList = liftM LitList (tokList $ sepWith tokEltSep langLit) 
          <?> "list literal"
              
exprList :: Scanner Expr
exprList = liftM ExprList (tokList $ sepWith tokEltSep expr)


-- |Boolean literal
-- >>> evalScan "true" litBool
-- Right (... True)
--
-- >>> evalScan "false" litBool
-- Right (... False)
litBool :: Scanner LangLit
litBool = liftM LitBool (litTrue <|> litFalse) 
          <?> "boolean literal"
    where litTrue  = tokTrue  >> return True
          litFalse = tokFalse >> return False
                   
-- |Dotted range of values
-- >>> evalScan "(1..7)" litRange
-- Right (...1...7...)
--
-- TODO: Add additional `step' to ranges (1..7..3)
litRange = parens (LitRange 
                   <$> (expr <* tokRangeSep) 
                   <*> expr)
           <?> "range literal"
               
-- | Non-valued literal
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
       
exprIdent = liftM ExprIdent langIdent <?> "identifier"
           

identName :: Scanner LangIdent
identName = liftM LangIdent (tryScan ident) <?> "identifier"
            
langIdent = identName
            
funIdent :: Scanner LangIdent
funIdent = char '$' *> identName
           
exprFunIdent :: Scanner Expr
exprFunIdent = liftM ExprFunIdent funIdent
         
exprLambda = liftM ExprLambda $ do
  char '('
  args <- callList'
  tokSpace
  body <- stmt
  char ')'
  return $ CallSig args body
  


-- langIdent :: Scanner LangIdent
-- langIdent = tryScan (liftM LangIdent ident) <?> "identifier"

-- data LangFunCall = FC LangIdent [Expr]
--                    deriving (Show)
                 

-- | Set of arguments for a function 
arglist = within tokTupleStart tokTupleEnd (sepWith tokEltSep expr)

callList' = do
  tokTupleStart
  params <- sepWith tokEltSep identName
  catcher <- optional (string ".." *> identName)
  tokTupleEnd
  return $ ArgSig params catcher
  
  

-- exprFunCall = liftM ExprFunCall langFunCall
exprFunCall = ExprFunCall <$> tryScan (langIdent <* lookAhead (char '(')) <*> arglist <?> "function call"
              
exprOp = liftM ExprOp langOp -- <?> "operation"
                       

-- langOp = unOp <|> binOp <?> "operation"
langOp = specOp <|> multiOp -- <?> "operation"

opMult, opDiv, opAdd, opSub, opNot :: Scanner Op
makeOp :: Scanner a -> Op -> Scanner Op
makeOp sc op = sc >> return op

-- TODO: This conflicts with literal -ve for integers,
-- either make it so that literals use the operator (probably
-- slower) or resolve the conflict!
opNeg  = makeOp (char '-')    OpNeg  <?> "operator (-)"
opMult = makeOp (char '*')    OpMult <?> "operator (*)"
opDiv  = makeOp (char '/')    OpDiv  <?> "operator (/)"
opAdd  = makeOp (char '+')    OpAdd  <?> "operator (+)"
opSub  = makeOp (char '-')    OpSub  <?> "operator (-)"
opNot  = makeOp (char '^')    OpNot  <?> "operator (^)"
opGreater = makeOp (char '>') OpGreater <?> "operator (>)"
opLess = makeOp (char '<')    OpLess <?> "operator (<)"
opGreaterEq = makeOp (string ">=") OpGreaterEq <?> "operator (>=)"
opLessEq = makeOp (string "<=") OpLessEq <?> "operator (<=)"
opEq   = makeOp (string "==") OpEq   <?> "operator (==)"
opOr   = makeOp (char '|')    OpOr   <?> "operator (|)"
opAnd  = makeOp (char '&')    OpAnd <?> "operator (&)"

userOp = liftM (UserOp . LangIdent) (some tokOpChar) <?> "operator"
       
-- |Operators that can be used outside parentheses
-- >>> evalScan "^true" specOp
-- Right (...Not...True...)
specOp :: Scanner LangOp
specOp = choice (map preOp specOps) <?> "special operator"
                       
specOps :: [Scanner Op]
specOps = [opNot, opNeg]
          
preOp sc = do
  op  <- sc
  opr <- expr
  return $ SpecOp op opr
         
-- |Operators called within parentheses that may have
-- multiple operands
-- >>> evalScan "(+ 1 3)" multiOp
-- Right (...Add...1...3...)
multiOp :: Scanner LangOp
multiOp = parens (choice (map multOp multiOps) 
                  <|> multOp userOp) <?> "operator expression"
          
multiOps :: [Scanner Op]
multiOps = [opMult, opDiv, opAdd, opSub, opEq, opOr]
           
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
