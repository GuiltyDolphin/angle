{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
module Angle.Lex.Lexer 
    ( Expr(..)
    ) where

-- Write this!
import Angle.Lex.Helpers
import Angle.Lex.Token
import Control.Monad (liftM2)
import Control.Monad.State
import Control.Applicative
import Control.Monad.Error
import Data.Monoid
import Control.Monad.Reader
import Data.Maybe (fromJust)
import Data.List (span, break)
import Data.Char (isAlphaNum, isAlpha, isNumber, digitToInt, isDigit, isAscii)
import qualified Data.Map as M


exprLit = liftM ExprLit langLit

data LangLit = LitStr String
             | LitInt Int
             | LitFloat Float
             | LitList [LangLit]
             | LitBool Bool
             | LitRange Expr Expr
               deriving (Show)

langLit :: Scanner LangLit
langLit = litStr <|> tryScan litFloat <|> litInt <|> litList <|> litBool <|> litRange <?> "literal"

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
litInt = liftM (LitInt . read) (some tokDenaryDigit) <?> "integer literal"
         

-- |Floating-point literal
-- >>> evalScan "12.3" litFloat
-- Right (...12.3)
litFloat :: Scanner LangLit
litFloat = liftM (LitFloat . read) (do
             first <- some tokDenaryDigit
             char '.'
             rest <- some tokDenaryDigit
             return (first ++ "." ++ rest)) <?> "floating-point literal"
             

-- |Multi-type list
-- >>> evalScan "[1,\"hello\",true]" litList
-- Right (... [... 1,... "hello",... True])
--
-- >>> evalScan "1,\"hello\",true" litList
-- Left ...
-- ...
litList :: Scanner LangLit
litList = liftM LitList (within tokListStart tokListEnd (sepWith tokEltSep langLit)) <?> "list literal"


-- |Boolean literal
-- >>> evalScan "true" litBool
-- Right (... True)
--
-- >>> evalScan "false" litBool
-- Right (... False)
litBool :: Scanner LangLit
litBool = liftM LitBool (litTrue <|> litFalse) <?> "boolean literal"
  where litTrue = tokTrue >> return True
        litFalse = tokFalse >> return False
                   
-- |Dotted range of values
-- >>> evalScan "(1..7)" litRange
-- Right (...1...7...)
--
-- TODO: Add additional `step' to ranges (1..7..3)
litRange = parens (do
  start <- expr
  tokRangeSep
  end <- expr
  return $ LitRange start end) <?> "range literal"



data Expr = ExprIdent LangIdent
          | ExprLit LangLit
          | ExprFunCall LangFunCall
          | ExprB Expr
          | ExprOp LangOp
            deriving (Show)
            
expr = tryScan exprB <|> tryScan exprOp <|> tryScan exprFunCall <|> exprLit <|> exprIdent <?> "expression"
       
exprB = liftM ExprB (within tokParenL tokParenR expr) <?> "bracketed expression"

exprIdent = liftM ExprIdent langIdent
            
type LangIdent = String
langIdent :: Scanner LangIdent
langIdent = ident <?> "identifier"

data LangFunCall = FC { funName :: LangIdent, funArgs :: [Expr] }
  deriving (Show)
 
arglist = within tokTupleStart tokTupleEnd (sepWith tokEltSep expr)

exprFunCall = liftM ExprFunCall langFunCall
              
-- |Standard function call
-- >>> evalScan "fun(1,2)" langFunCall
-- Right (...funName =..."fun", funArgs = [...1...,...2...]...)
langFunCall = do
  name <- langIdent
  args <- arglist
  return FC { funName = name
            , funArgs = args }
  
-- TODO: Issue with recursion when using binary operators
-- Fix this? Or just keep the only parse operator solution.
exprOp = liftM ExprOp langOp <?> "operation"
data LangOp = UnOp Op Expr | BinOp Op Expr Expr
              deriving (Show)

langOp = unOp <|> binOp <?> "operation"

data Op = Mult | Div | Add | Sub | Not
          deriving (Show)

spacedOp :: Scanner Op -> Scanner Op
spacedOp = surrounded spaces
opMult, opDiv, opAdd, opSub, opNot :: Scanner Op
opMult = spacedOp $ char '*' >> return Mult
opDiv = spacedOp $ char '/' >> return Div
opAdd = spacedOp $ char '+' >> return Add
opSub = spacedOp $ char '-' >> return Sub
opNot = spacedOp $ char '^' >> return Not
        
-- |Unary operators
-- >>> evalScan "^" unOpC
-- Right (...Not)
unOpC :: Scanner Op -> Scanner LangOp
unOpC op = do 
  p <- op
  r <- expr
  return $ UnOp p r
      
unOp = choice $ map unOpC [opNot]

-- |Binary operators
-- >>> evalScan "+" binOp
-- Right (...Add)
--
-- >>> evalScan "-" binOp
-- Right (...Sub)
binOp :: Scanner LangOp
binOp = choice (map binOpC [opMult, opDiv, opAdd, opSub]) <?> "binary operator"
        
binOpC :: Scanner Op -> Scanner LangOp
binOpC op = do
  s <- someTill op anyChar
  l <- expr
  p <- op
  r <- expr
  return $ BinOp p l r
  
        
checkOp op = do
  lookAhead (notScan op)
  l <- expr
  p <- op
  r <- expr
  return $ BOp p l r
         
         
data BOp = BOp Op Expr Expr
           deriving (Show)
opAdd' = checkOp 
         
testOps = [checkOp opMult, checkOp opAdd]
          
-- 
-- 
-- unOp = do
--   op <- choice [opNot]
--   operand <- expr
--   return $ UnOp op operand
--          
-- binOp = do
--   l <- expr
--   op <- choice [opAdd, opSub]
--   r <- expr
--   return $ BinOp op l r
  
data Stmt = SingleStmt SingStmt | MultiStmt [Stmt]
            deriving (Show)

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
multiStmt = do
  tokMultiStmtStart
  body <- many stmt
  tokMultiStmtEnd
  return $ MultiStmt body

-- | A single statement;
data SingStmt = StmtAssign LangIdent Expr
              | StmtStruct LangStruct
              | StmtExpr Expr
                deriving (Show)
                
-- TODO: Last statement in a multi-statement block, or at
-- end of file, shouldn't need to have a newline or semi-colon
singStmt :: Scanner SingStmt
singStmt = tryScan stmtAssign <* checkStmtEnd
           <|> stmtStruct 
           <|> stmtExpr <* checkStmtEnd
           
stmtExpr :: Scanner SingStmt
stmtExpr = liftM StmtExpr expr
           
-- |Variable assignment
-- >>> evalScan "x=5" stmtAssign
-- Right (..."x"...5...)
stmtAssign :: Scanner SingStmt
stmtAssign = do
  name <- langIdent
  tokAssign
  val <- expr
  return $ StmtAssign name val
                
stmtStruct :: Scanner SingStmt 
stmtStruct = liftM StmtStruct langStruct

-- |Specialised language constructs
data LangStruct = StructFor LangIdent Expr Stmt
                | StructWhile Expr Stmt
                | StructIf Expr Stmt (Maybe Stmt)
                | StructDefun LangIdent [LangIdent] Stmt
                | StructReturn Expr -- TODO: Probably don't 
                                    -- need this
                  deriving (Show)
                  
langStruct = structFor <|> structWhile <|> structIf <|> structDefun <|> structReturn <?> "language construct"
             
             
-- |For loop
-- >>> evalScan "for x in (2..8) do {}" structFor
-- Right (..."x"...2...8...[]...)
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
structWhile = do
  keyword "while"
  p <- expr
  keyword " do"
  body <- stmt
  return $ StructWhile p body
         
-- |Conditional if statement
-- >>> evalScan "if true then {}" structIf
-- Right (...True...[]...Nothing)
--
-- >>> evalScan "if false then {} else {}" structIf
-- Right (...False...[]...Just...[]...)
structIf = do
  keyword "if"
  p <- expr
  keyword " then"
  thenBody <- stmt
  elseBody <- optional (do
                         keyword "else"
                         stmt)
  return $ StructIf p thenBody elseBody

        
data LangFun = LangFun { funDeclName :: LangIdent
                       , funDeclArgs :: [LangIdent]
                       , funDeclBody :: Stmt } 
               deriving (Show)
             
-- |Function definition
-- >>> evalScan "defun foo(x) { print(x) }" structDefun
-- Right (..."foo"..."x"...)
structDefun :: Scanner LangStruct
structDefun = do
  keyword "defun"
  name <- langIdent
  argNames <- parens (sepWith (char ',') langIdent)
  optional tokStmtEnd
  body <- stmt
  return $ StructDefun name argNames body
         
structReturn = liftM StructReturn (keyword "return" *> expr) <?> "return construct"

         

program :: Scanner [Stmt]
program = do
  followed tokEOF (some stmt)
  
  -- sepWith (some tokNewLine) stmt
  
