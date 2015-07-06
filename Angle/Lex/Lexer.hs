{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
module Angle.Lex.Lexer () where

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



-- <2|>+<|3>
-- <2+3>
-- data Stmt = SingStmt SingStmt | MultiStmt [Stmt]
--   deriving (Show)
-- data SingStmt = FunDef {funName :: String, funBody :: [Stmt] }
--           | FunRetrieve FunRetrieve
--           | FunRetrieveMany [FunRetrieve]
--           | FunCall String [Stmt] [Stmt]
--           | StmtExpr Expr
--           | Assign Ident Stmt
--   deriving (Show)
-- 
-- stmt = singStmt <|> multiStmt
-- 
-- singStmt = liftM SingStmt (followed tokStmtEnd (funDef <|> retrieve <|> assign) <?> "statement")
--        
-- multiStmt = liftM MultiStmt (within tokMultiStmtStart tokMultiStmtEnd (many stmt)) <?> "multi-line statement"
-- 
-- funDef = angles $ do
--            name <- ident
--            char ':'
--            body <- many stmt
--            return $ FunDef name body
-- 
-- data FunRetrieve = FR { frLevel :: Int
--                       , frIdent :: Ident
--                       , frDirection :: Direction }
--   deriving (Show, Eq)
-- 
-- data Direction = DirLeft | DirRight
--   deriving (Show, Eq)
-- 
-- type Ident = String
-- retrieve = liftM FunRetrieve $ retrieveLeft <|> retrieveRight
-- 
-- -- TODO: Make a more general retriever 
-- retrieveLeft = do
--   level <- some (char '|')
--   string "->"
--   store <- ident
--   return FR { frLevel=length level
--             , frIdent=store
--             , frDirection=DirLeft }
-- 
-- -- retrieveDir :: Direction -> Scanner FunRetrieve 
-- -- retrieveDir dir = do
-- --   let (first,second) = case dir of
-- --                          DirLeft -> (some (char '|'), ident)
-- --                          DirRight -> (ident, some (char '|'))
-- --   if (dir == DirLeft) then
-- --       string "->"
-- --   else string "<-"
-- --   let (level,store) = case dir of
-- --   return FR { frLevel=length level
-- --             , frIdent=store
-- --             , frDirection=dir }
-- 
-- retrieveRight = do
--   store <- ident
--   string "<-"
--   level <- some (char '|')
--   return FR { frLevel=length level
--             , frIdent=store
--             , frDirection=DirRight }
-- 
-- retrieveMany = retrieveManyLeft <|> retrieveManyRight
-- 
-- retrieveManyLeft = do
--   startLevel <- some (char '|')
--   string "->"
--   char '('
--   names <- sepWith (char ',') ident
--   char ')'
--   let withLevels = zip names [length startLevel..]
--   mapM (return . makeFr) withLevels
--       where makeFr (name,level) = FR { frLevel=level
--                                      , frIdent=name
--                                      , frDirection=DirLeft}
--   -- return . FunRetrieveMany $ map makeFr withLevels
--   --     where makeFr (name,level) = FR { frLevel=level
--   --                                    , frIdent=name
--   --                                    , frDirection=DirLeft}
-- retrieveManyRight = do
--   names <- parens (sepWith (char ',') ident)
--   string "<-"
--   startLevel <- some (char '|')
--   let withLevels = zip names [length startLevel..]
--   mapM (return . makeFr) withLevels
--       where makeFr (name,level) = FR { frLevel=level
--                                      , frIdent=name
--                                      , frDirection=DirLeft}
--   -- return . FunRetrieveMany $ map makeFr withLevels
--   --     where makeFr (name,level) = FR { frLevel=level
--   --                                    , frIdent=name
--   --                                    , frDirection=DirLeft}
-- 
-- assign = do
--   name <- ident
--   char '='
--   val <- stmt
--   return $ Assign name val
-- 
-- type VarMap = M.Map (String, Scope) Expr
--     
-- data Scope = Local | Outer | Global
--   deriving (Show, Eq, Ord)
-- 
-- data Expr = ExprIdent String
--           | ExprOp LangOp
--           | ExprLit LangLit
--   deriving (Show)
-- 
-- -- |A variable/function identifier
-- -- >>> evalScan "test" exprIdent
-- -- Right (ExprIdent "test")
-- --
-- -- >>> evalScan "{test}" exprIdent
-- -- Left ...
-- -- ...
-- exprIdent = liftM ExprIdent ident
--             
--                
-- data LangOp = BinOp Op Expr Expr | UnOp Op Expr
--   deriving (Show)
-- data Op = Add | Sub
--   deriving (Show, Eq)
           
-- funRet = do
--   val <- stmt
--   string "->|"
--   return $ Expr val


-- ***************************

-- stmt = funDef | construct ;
-- funDef = '<', {funChar}+, ':', {stmt}, '>' ;



-- construct = retrieve | return

-- literal = string | number | bool | 
-- return = 

-- loopFor = '<', 'for', ',' ident, ',', expr, ',', stmt, '>' ;


-- Examples
-- <for,x,[1..3],<print,"test">>
-- <ntimes,count,stmt:<for,_,[0..count],stmt>>
-- <ntimes:(count,stmt)<-|;<for,_,[0..count],stmt>>


-- <fun:(x,y,z)<-|;body>
-- == <fun(x,y,z):body>


-- <for,x,[1..3],
-- <print,<str,x>>
-- >

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
data LangOp = UnOp Op | BinOp Op
              deriving (Show)

langOp = unOp <|> binOp <?> "operation"

data Op = Add | Sub | Not
          deriving (Show)

spacedOp :: Scanner Op -> Scanner Op
spacedOp = surrounded spaces
opAdd, opSub, opNot :: Scanner Op
opAdd = spacedOp $ char '+' >> return Add
opSub = spacedOp $ char '-' >> return Sub
opNot = spacedOp $ char '^' >> return Not
        
-- |Unary operators
-- >>> evalScan "^" unOp
-- Right (...Not)
unOp :: Scanner LangOp
unOp = liftM UnOp (choice [opNot])
       
-- |Binary operators
-- >>> evalScan "+" binOp
-- Right (...Add)
--
-- >>> evalScan "-" binOp
-- Right (...Sub)
binOp :: Scanner LangOp
binOp = liftM BinOp (choice [opAdd, opSub])
        
checkOp op = do
  lookAhead (notScan op)
  l <- expr
  p <- op
  r <- expr
  return $ BOp p l r
         
binOp' sc :: Scanner Op
         
         
data BOp = BOp Op Expr Expr
           deriving (Show)
opAdd' = checkOp 
  
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
  
