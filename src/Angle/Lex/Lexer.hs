{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
module Angle.Lex.Lexer 
    ( program
    , stmt
    ) where

-- Write this!
import Angle.Lex.Helpers
import Angle.Types.Lang
import Angle.Lex.Token
import Control.Monad.State
import Control.Applicative


stmt :: Scanner Stmt
stmt = (multiStmt <|> singleStmt) <?> "statement"
       
singleStmt :: Scanner Stmt
singleStmt = liftM SingleStmt singStmt

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
singStmt = surrounded tokStmtBetween
           (   tryScan stmtAssign <* (char ';' <|> char '\n')
           <|> stmtStruct 
           <|> stmtExpr           <* checkStmtEnd
           <|> stmtComment)
               
-- |Variable assignment
-- >>> evalScan "x=5" stmtAssign
-- Right (..."x"...5...)
stmtAssign :: Scanner SingStmt
stmtAssign = StmtAssign 
             <$> (langIdent <* tokAssign) 
             <*> expr

stmtComment :: Scanner SingStmt
stmtComment = StmtComment <$> (char '#' *> many (notChar '\n'))
              <?> "comment"
           
stmtStruct :: Scanner SingStmt 
stmtStruct = liftM StmtStruct langStruct

stmtExpr :: Scanner SingStmt
stmtExpr = liftM StmtExpr expr

                  
langStruct :: Scanner LangStruct
langStruct =     structFor 
             <|> structWhile 
             <|> structIf 
             <|> structDefun 
             <|> structReturn 
             <?> "language construct"
             
             
-- |For loop
-- >>> evalScan "for x in (2..8) do {}" structFor
-- Right (..."x"...2...8...[]...)
structFor :: Scanner LangStruct
structFor = do
  keyword "for"
  name <- langIdent
  keyword " in"
  iter <- expr
  string " do"
  optional tokNewLine
  body <- stmt
  return $ StructFor name iter body

-- |While loop
-- >>> evalScan "while true do {}" structWhile
-- Right (...True...[]...)         
structWhile :: Scanner LangStruct
structWhile = StructWhile 
              <$> (keyword "while" *> expr)
              <*> (keyword " do"   *> stmt)
         
-- |Conditional if statement
-- >>> evalScan "if true then {}" structIf
-- Right (...True...[]...Nothing)
--
-- >>> evalScan "if false then {} else {}" structIf
-- Right (...False...[]...Just...[]...)
structIf :: Scanner LangStruct
structIf = StructIf
           <$> (keyword "if" *> expr)
           <*> (keyword " then" *> stmt)
           <*> optional (keyword "else" *> stmt)

        
-- |Function definition
-- >>> evalScan "defun foo(x) { print(x) }" structDefun
-- Right (..."foo"..."x"...)
structDefun :: Scanner LangStruct
structDefun = StructDefun
              <$> (string "defun " *> langIdent)
              <*> parens (sepWith (char ',') langIdent)
              <*> stmt
         
structReturn = liftM StructReturn (keyword "return" *> expr) 
               <?> "return construct"

         

program :: Scanner Stmt
program = liftM MultiStmt $ followed tokEOF (many stmt)
  
  -- sepWith (some tokNewLine) stmt

exprLit = liftM ExprLit langLit

               

-- |Language literals
langLit :: Scanner LangLit
langLit = litStr 
          <|> tryScan litFloat 
          <|> litInt 
          <|> litList 
          <|> litBool 
          <|> litRange 
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
litList = liftM LitList (tokList $ sepWith tokEltSep expr) 
          <?> "list literal"


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

expr = tryScan exprB 
       <|> exprOp 
       <|> tryScan exprFunCall 
       <|> exprLit 
       <|> exprIdent 
       <?> "expression"
       
exprB = liftM ExprB (within tokParenL tokParenR expr) <?> "bracketed expression"

exprIdent = liftM ExprIdent langIdent <?> "identifier"
            
langIdent :: Scanner LangIdent
langIdent = ident <?> "identifier"

-- data LangFunCall = FC LangIdent [Expr]
--                    deriving (Show)
                 

 
arglist = within tokTupleStart tokTupleEnd (sepWith tokEltSep expr)

-- exprFunCall = liftM ExprFunCall langFunCall
exprFunCall = ExprFunCall <$> langIdent <*> arglist <?> "function call"
              
-- -- |Standard function call
-- -- >>> evalScan "fun(1,2)" langFunCall
-- -- Right (..."fun" [...1...,...2...])
-- langFunCall = FC <$> langIdent <*> arglist <?> "function call"
  
exprOp = liftM ExprOp langOp -- <?> "operation"
                       

-- langOp = unOp <|> binOp <?> "operation"
langOp = specOp <|> multiOp -- <?> "operation"

opMult, opDiv, opAdd, opSub, opNot :: Scanner Op
makeOp :: Scanner a -> Op -> Scanner Op
makeOp sc op = sc >> return op

opNeg  = makeOp (char '-')    OpNeg  <?> "operator (-)"
opMult = makeOp (char '*')    OpMult <?> "operator (*)"
opDiv  = makeOp (char '/')    OpDiv  <?> "operator (/)"
opAdd  = makeOp (char '+')    OpAdd  <?> "operator (+)"
opSub  = makeOp (char '-')    OpSub  <?> "operator (-)"
opNot  = makeOp (char '^')    OpNot  <?> "operator (^)"
opEq   = makeOp (string "==") OpEq   <?> "operator (==)"
       
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
multiOp = choice (map multOp multiOps) <?> "operator expression"
          
multiOps :: [Scanner Op]
multiOps = [opMult, opDiv, opAdd, opSub, opEq]
           
multOp :: Scanner Op -> Scanner LangOp
multOp sc = tryScan . parens $ MultiOp
            <$> (sc <* tokSpace)
            <*> sepWith (some tokWhitespace) expr
                     
  