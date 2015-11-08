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
litFloat = liftM (LitFloat . read) $ do
             first <- some tokDenaryDigit
             char '.'
             rest <- some tokDenaryDigit
             return (first ++ "." ++ rest)
             

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
litRange = parens (do
  start <- expr
  tokRangeSep
  end <- expr
  return $ LitRange start end)



data Expr = ExprIdent LangIdent
          | ExprLit LangLit
          | ExprFunCall LangFunCall
            deriving (Show)
            
expr = exprFunCall <|> exprLit <|> exprIdent <?> "expression"

exprIdent = liftM ExprIdent langIdent
            
type LangIdent = String
langIdent = ident




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
  
  
data Stmt = SingleStmt SingStmt | MultiStmt [Stmt]
            deriving (Show)

data SingStmt = StmtAssign LangIdent Expr
              | StmtStruct LangStruct
              | StmtExpr Expr
                deriving (Show)
                

data LangStruct = StructFor LangIdent Expr Stmt
                | StructWhile Expr Stmt
                | StructIf Expr Stmt (Maybe Stmt)
                  deriving (Show)
                  

         
