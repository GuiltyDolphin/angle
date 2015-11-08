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
             | LitList [LangLit]
             | LitBool Bool
               deriving (Show)

langLit :: Scanner LangLit
langLit = litStr <|> litInt <|> litList <|> litBool <?> "literal"

-- |A literal string
-- >>> evalScan "\"test\"" litStr
-- Right (... "test")
--
-- >>> evalScan "test" litStr
-- Left ...
-- ...
litStr :: Scanner LangLit
litStr = liftM LitStr (within tokStringStart tokStringEnd (many tokStringBodyChar)) <?> "string literal"

-- |Denary integer
-- >>> evalScan "123" litInt
-- Right (... 123)
--
-- >>> evalScan "test" litInt
-- Left ...
-- ...
litInt :: Scanner LangLit
litInt = liftM (LitInt . read) (some tokDenaryDigit) <?> "integer literal"

-- |Multi-type list
-- >>> evalScan "[1,\"hello\",$t]" litList
-- Right (... [... 1,... "hello",... True])
--
-- >>> evalScan "1,\"hello\",$t" litList
-- Left ...
-- ...
litList :: Scanner LangLit
litList = liftM LitList (within tokListStart tokListEnd (sepWith tokEltSep langLit)) <?> "list literal"


-- |Boolean literal
-- >>> evalScan "$t" litBool
-- Right (... True)
--
-- >>> evalScan "$f" litBool
-- Right (... False)
--
-- >>> evalScan "true" litBool
-- Left ...
-- ...
litBool :: Scanner LangLit
litBool = liftM LitBool (litTrue <|> litFalse) <?> "boolean literal"
  where litTrue = string "$t" >> return True
        litFalse = string "$f" >> return False



data Expr = ExprIdent LangIdent
          | ExprLit LangLit
          | ExprFunCall LangFunCall
            deriving (Show)
            
expr = exprFunCall <|> exprLit <|> exprIdent <?> "expression"

exprIdent = liftM ExprIdent langIdent
            
type LangIdent = String
langIdent = ident




data LangFunCall = FC { funName :: LangIdent, funArgs :: [Arg] }
  deriving (Show)
data Arg = Arg { argValue :: Expr
               , argDir :: Direction
               , argLevel :: Int }
  deriving (Show)
data Direction = DirLeft | DirRight
  deriving (Show)
                 
               
exprFunCall = liftM ExprFunCall langFunCall
-- |Function call (with arguments)
-- >>> evalScan "<+,6,7>" langFunCall
-- Right (...funName =..."+", funArgs = [...6...,...7...]...)
--
-- >>> evalScan "<add>" langFunCall
-- Right (...funName =..."add", funArgs = []...)
--
-- >>> evalScan "<7,2,->" langFunCall
-- Right (...funName =..."-", funArgs = [...7...,...2...]...)
langFunCall :: Scanner LangFunCall
langFunCall = angles (    tryScan procCall 
                      <|> tryScan prefixFunCall 
                      <|> tryScan postfixFunCall
                      <|> infixFunCall) <?> "function call"
    where procCall = do
            name <- langIdent
            lookAhead (char '>')
            return FC { funName = name
                      , funArgs = [] }
          prefixFunCall = do
            name <- langIdent
            tokEltSep
            args <- sepWith tokEltSep expr
            lookAhead (char '>')
            return FC { funName = name
                      , funArgs = mkArgList DirRight args }
          postfixFunCall = do
            (ExprIdent name:args) <- liftM reverse $ sepWith tokEltSep expr
            lookAhead (char '>')
            return FC { funName = name
                      , funArgs = mkArgList DirLeft args }
          infixFunCall = do
            args <- sepWith tokEltSep expr
            let (argsSt, (ExprIdent name):argsRst) = break isDecl args
                argsRight = mkArgList DirRight argsRst
                argsLeft = mkArgList DirLeft argsSt
            lookAhead (char '>')
            return FC { funName = name
                      , funArgs = argsLeft ++ argsRight }
          isDecl (ExprIdent (IdentDecl _)) = True
          isDecl _ = False
          mkArgList dir args = 
              map (\(x,level) -> Arg { argValue = x
                                     , argDir = dir
                                     , argLevel = level })
                   (zip args [1..])
                          
            
-- <1,@+@,3>
-- <+,1,3>
-- <1,3,+>
