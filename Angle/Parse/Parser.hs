{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Angle.Parse.Parser
    (
    ) where

import Angle.Lex.Lexer
import Control.Monad.Reader
import Control.Monad.Error
import Control.Applicative
    
import Data.IORef


-- Mutable state
-- Various alternative exist for emulating or modifying state
--   in both pure and side-effecting scenarios.
-- The State Monad\footnote{https://hackage.haskell.org/package/mtl-1.1.0.2/docs/Control-Monad-State-Lazy.html} - Provides an effect of State without
--   side-effects by performing computations based on an initial
--   state.
-- The ST Monad\footnote{https://hackage.haskell.org/package/base-4.7.0.0/docs/Control-Monad-ST.html} - Provides mutable state
--   via memory on the machine - allows reading and writing to this memory.
---  Prevents users from accessing external threads by forcing code that uses it to be fully general in the current thread type.\footnote{https://wiki.haskell.org/Monad/ST}
-- IORefs\footnote{https://hackage.haskell.org/package/base-4.7.0.2/docs/Data-IORef.html} - Allow for mutable variable references through the IO Monad.
-- MVars\footnote{https://hackage.haskell.org/package/base-4.8.0.0/docs/Control-Concurrent-MVar.html} - Mutable locations that can either be empty of contain a value of some type.
--  Provides a simple API of putMVar and takeMVar. These will store or retrieve a value from an MVar or block if the MVar is full or empty, respectively.
-- TVars\footnote{https://hackage.haskell.org/package/stm-2.1.1.2/docs/Control-Concurrent-STM-TVar.html} - Transactional Variables that have shared memory locations and support atomic memory transactions.
-- To represent the 
data LangError = TypeError TypeError
               | SyntaxError String
               | UndefinedIdent String
               | DefaultError String

instance Show LangError where
    show (TypeError e) = "wrong type in expression: " ++ show e
    show (SyntaxError s) = "syntax error: " ++ s
    show (UndefinedIdent v) = "identifier not defined: " ++ v
    show (DefaultError s) = "defaultError: " ++ s

instance Error LangError where
    noMsg = DefaultError ""
    strMsg = DefaultError

type TestInt = IORef Int
  
type Env = IORef [(String, Expr)] 

type EnvE = ErrorT LangError IO    
newtype IOLangError a = ILE { runIOLangError :: ErrorT LangError IO a }
    deriving (Functor, Applicative, Monad, MonadError LangError)
           
type LE = ErrorT LangError (ReaderT Env IO)

-- isBound :: String -> LE Bool
-- isBound s = do
--   env <- ask
--   case lookup env s of
--     Nothing -> return False
--     Just _ -> return True

int1 :: IO TestInt 
int1 = newIORef 1
       
-- add1 :: TestInt -> IO TestInt
-- add1 = return . (+1)

opMap = [(OpNot, langNot)]

langNot (LitBool x) = LitBool (not x)

evalExpr :: Expr -> LangLit
evalExpr (ExprLit x) = x
                       
langLitJoin :: LangLit -> LangLit -> Either LangError LangLit
langLitJoin (LitList xs) (LitList ys) = return $ LitList (xs++ys)
langLitJoin x (LitList _) = Left (TypeError $ TypeUnexpected (typeOf x) LList)
langLitJoin l@(LitList _) x = langLitJoin x l

                            
langLitAdd :: LangLit -> LangLit -> Either LangError LangLit
langLitAdd l@(LitList _) r = langLitJoin l r
langLitAdd (LitInt x) (LitInt y) = return $ LitInt (x + y)
langLitAdd (LitFloat x) r 
    = case r of
        LitInt y -> return $ LitFloat (x + fromIntegral y)
        LitFloat y -> return $ LitFloat (x + y)
        _ -> Left . TypeError $ TypeUnexpected (typeOf r) LFloat
langLitAdd l r@(LitFloat _) = langLitAdd r l
langLitAdd l r 
    | typeOf l /= typeOf r 
        = Left . TypeError $ TypeMismatch ltype rtype
    | otherwise 
        = Left . TypeError $ TypeNotValid ltype
    where ltype = typeOf l
          rtype = typeOf r
                 
data LangType = LList | LBool | LStr | LInt | LFloat
                deriving (Eq)
              
typeOf :: LangLit -> LangType
typeOf (LitStr _) = LStr
typeOf (LitList _) = LList
typeOf (LitInt _) = LInt
typeOf (LitFloat _) = LFloat
typeOf (LitBool _) = LBool
              
instance Show LangType where
    show LList = "list"
    show LBool = "boolean"
    show LStr = "string"
    show LInt = "integer"
    show LFloat = "float"

data TypeError = TypeMismatch LangType LangType
               | TypeUnexpected LangType LangType
               | TypeNotValid LangType
               
instance Show TypeError where
    show (TypeMismatch l r) = "type mismatch: got (" ++ show l ++ ", " ++ show r ++ ") but both types should be the same"
    show (TypeUnexpected l r) = "unexpected type: " ++ show l ++ ", expecting: " ++ show r
    show (TypeNotValid l) = "type not valid for scenario: " ++ show l


data LError = LError { errorExpr :: Expr
                     , errorStmt :: Stmt
                     , errorErr :: LangError 
                     , errorSource :: String
                     , lerrorPos :: (Int, Int, Int)
                     }

instance Show LError where
    -- show (LError { errorExpr=ex, errorStmt=es, errorErr=ee })
    --     = unlines
    --       [ "error in statement: " ++ show es
    --       , "in expression: " ++ show ex
    --       , show ee ]
    show (LError { errorErr=ee, lerrorPos=ep@(_,_,pos), errorSource=es })
        = cEp ++ cEt ++ cEe
          where cEp = showPos ep ++ "\n"
                cEt = takeWhile (/='\n') (drop pos es)
                cEe = show ee
                showPos (ln,cn,_) =
                  concat ["line: ", show ln, ", column: ", show cn]
