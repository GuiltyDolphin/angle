{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE KindSignatures #-}
module Angle.Parse.Error
    ( typeMismatchErr
    , typeUnexpectedErr
    , typeNotValidErr
    , typeCastErr
    , nameNotDefinedErr
    , nameNotFunctionErr
    , nameNotValueErr
    , nameNotOpErr
    , wrongNumberOfArgumentsErr
    , LangError
    , CanError
    , CanErrorWithPos(..)
    , throwError
    , langError
    , SourcePos
    , LError(..)
    , throwLangError
    , indexOutOfBoundsErr
--    , getLangError
    ) where


import Control.Monad.Error
   
import Angle.Types.Lang
import Angle.Scanner
-- Errors
-- Need to be able to throw errors from `pure' code,
-- like in Operations.
-- Adding a List and Int don't make sense, so need to
-- throw an error when this occurs.
-- Might make the code less pretty? Will need maybe more
-- boilerplate and adding monads to this `pure' code.

 
-- LangError API

class (MonadError LError m) => CanError (m :: * -> *) where
    --getLangError :: m a -> m LError

instance CanError (Either LError)
    --getLangError (Left e) = return e
    --getLangError (Right _) = error "getLangError: No error"

instance (Monad m) => CanError (ErrorT LError m)
   
data LangError = TypeError TypeError
               | SyntaxError String
               | NameError NameError
               | CallError CallError
               | DefaultError String
               | LitError LitError
               | UserError String -- TODO: Add keyword and
                                  -- structures for allowing
                                  -- the user to throw errors
                 
typeErr    = TypeError
syntaxErr  = SyntaxError
nameErr    = NameError
callErr    = CallError
defaultErr = DefaultError
litErr     = LitError

instance Show LangError where
    show (TypeError e)    = "wrong type in expression: " ++ show e
    show (SyntaxError s)  = "syntax error: " ++ s
    show (NameError v)    = "name error: " ++ show v
    show (CallError x)    = "call error: " ++ show x
    show (DefaultError s) = "defaultError: " ++ s
    show (LitError x) = "literal error: " ++ show x

instance Error LangError where
    noMsg = DefaultError ""
    strMsg = DefaultError

data TypeError = TypeMismatch   LangType LangType
               | TypeUnexpected LangType LangType
               | TypeNotValid   LangType
               | TypeCast LangType LangType
                 
typeMismatchErr   t1 = typeErr . TypeMismatch   t1
typeUnexpectedErr t1 = typeErr . TypeUnexpected t1
typeNotValidErr      = typeErr . TypeNotValid
typeCastErr       t1 = typeErr . TypeCast       t1
               
instance Show TypeError where
    show (TypeMismatch l r)   = "type mismatch: got (" ++ show l ++ ", " ++ show r ++ ") but both types should be the same"
    show (TypeUnexpected l r) = "unexpected type: " ++ show l ++ ", expecting: " ++ show r
    show (TypeNotValid l)     = "type not valid for scenario: " ++ show l
    show (TypeCast l r) = "cannot convert " ++ show l ++ " to " ++ show r
                            
data NameError = NameNotDefined LangIdent 
               | NameNotFunction LangIdent
               | NameNotValue LangIdent
               | NameNotOp LangIdent

nameNotDefinedErr, nameNotFunctionErr, nameNotValueErr :: LangIdent -> LangError
nameNotDefinedErr  = nameErr . NameNotDefined
nameNotFunctionErr = nameErr . NameNotFunction
nameNotValueErr    = nameErr . NameNotValue
nameNotOpErr       = nameErr . NameNotOp

instance Show NameError where
    show (NameNotDefined  (LangIdent name)) = "not in scope: "         ++ name
    show (NameNotFunction (LangIdent name)) = "not a valid function: " ++ name
    show (NameNotValue    (LangIdent name)) = "no value assigned: "    ++ name
    show (NameNotOp       (LangIdent name)) = "non-existant operator: " ++ name
                                  
data CallError = 
    WrongNumberOfArguments Int Int
    deriving (Eq)
             
wrongNumberOfArgumentsErr expect = callErr . WrongNumberOfArguments expect 
             
             
instance Show CallError where
    show (WrongNumberOfArguments x y) = "wrong number of arguments: expected " ++ show x ++ " but got " ++ show y
                               


data LError = LError { errorErr    :: LangError  -- The actual error
                     , errorSource :: String
                     , errorPos    :: SourceRef -- Position at which the error occurred
                     , errorText :: String -- Additonal text representing the error
                     }

instance Show LError where
    -- show (LError { errorExpr=ex, errorStmt=es, errorErr=ee })
    --     = unlines
    --       [ "error in statement: " ++ show es
    --       , "in expression: " ++ show ex
    --       , show ee ]
    show (LError { errorErr=ee
                 , errorPos=SourceRef (start,end)
                 , errorText=et
                 , errorSource=es
                 })
        = cEp ++ cEt ++ cEe
          where cEp = concat ["[", showPos start, "-", showPos end, "]"] ++ "\n"
                cEt = "in " ++ et ++ "\n"
                cEe = show ee
                showPos (SourcePos (cn,ln,_)) 
                    = concat ["(", show ln, ",", show cn, ")"]
                      
getSourceLine :: String -> SourcePos -> String
getSourceLine s pos = lines s !! lineNo pos
                         
instance Error LError where
    noMsg = LError {errorErr=noMsg, errorPos=SourceRef (beginningOfFile, beginningOfFile), errorSource="", errorText=""}
    strMsg m = noMsg {errorErr=strMsg m}
               
langError :: (CanError m) => LangError -> m a
langError e = throwError noMsg { errorErr = e }
              
throwLangError :: (CanErrorWithPos m) => LangError -> m a
throwLangError e = do
  errPosRef <- getErrorPos
  errText <- getErrorText
  errSource <- getErrorSource
  throwError LError { errorErr = e, errorPos = errPosRef
                    , errorText = errText
                    , errorSource = errSource
                    }


class (MonadError LError m) => CanErrorWithPos m where
    getErrorPos :: m SourceRef
    -- getErrorLError :: m LError
    getErrorText :: m String
    getErrorSource :: m String


data LitError = IndexOutOfBoundsError Int
              deriving (Eq)
                       
indexOutOfBoundsErr = litErr . IndexOutOfBoundsError

instance Show LitError where
    show (IndexOutOfBoundsError x) = "index out of bounds: " ++ show x






