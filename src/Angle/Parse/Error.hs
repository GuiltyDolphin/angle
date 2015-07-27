{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE KindSignatures #-}
module Angle.Parse.Error
    ( typeMismatchErr
    , typeUnexpectedErr
    , typeNotValidErr
    , typeNotValidErrT
    , typeCastErr
    , typeMismatchOpErr
    , typeMismatchOpErrT
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
    , defaultErr
    , syntaxErr
    , userErr
    ) where


import Control.Monad.Error
import Data.Function (on)
   
import Angle.Scanner
import Angle.Types.Lang


class (MonadError LError m) => CanError (m :: * -> *)


instance CanError (Either LError)


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
                 

typeErr :: TypeError -> LangError
typeErr    = TypeError


syntaxErr :: String -> LangError
syntaxErr  = SyntaxError


nameErr :: NameError -> LangError
nameErr    = NameError


callErr :: CallError -> LangError
callErr    = CallError


defaultErr :: String -> LangError
defaultErr = DefaultError


litErr :: LitError -> LangError
litErr     = LitError
             

userErr :: String -> LangError
userErr = UserError


instance Show LangError where
    show (TypeError e)    = "wrong type in expression: " ++ show e
    show (SyntaxError s)  = "syntax error: " ++ s
    show (NameError v)    = "name error: " ++ show v
    show (CallError x)    = "call error: " ++ show x
    show (DefaultError s) = "defaultError: " ++ s
    show (LitError x) = "literal error: " ++ show x
    show (UserError x) = "user error: " ++ x


instance Error LangError where
    noMsg = DefaultError ""
    strMsg = DefaultError


data TypeError = TypeMismatch   LangType LangType
               | TypeUnexpected LangType LangType
               | TypeNotValid   LangType
               | TypeCast LangType LangType
               | TypeMismatchOp LangType LangType

                 
typeMismatchErr :: LangType -> LangType -> LangError
typeMismatchErr   t1 = typeErr . TypeMismatch   t1


typeUnexpectedErr :: LangType -> LangType -> LangError
typeUnexpectedErr t1 = typeErr . TypeUnexpected t1


typeNotValidErr :: LangType -> LangError
typeNotValidErr      = typeErr . TypeNotValid


typeNotValidErrT :: LangLit -> LangError
typeNotValidErrT     = typeNotValidErr . typeOf


typeCastErr :: LangType -> LangType -> LangError
typeCastErr       t1 = typeErr . TypeCast       t1


typeMismatchOpErr :: LangType -> LangType -> LangError
typeMismatchOpErr t1 = typeErr . TypeMismatchOp t1


typeMismatchOpErrT :: LangLit -> LangLit -> LangError
typeMismatchOpErrT = typeMismatchOpErr `on` typeOf

               
instance Show TypeError where
    show (TypeMismatch l r)   = "type mismatch: got (" ++ show l ++ ", " ++ show r ++ ") but both types should be the same"
    show (TypeUnexpected l r) = "unexpected type: " ++ show l ++ ", expecting: " ++ show r
    show (TypeNotValid l)     = "type not valid for scenario: " ++ show l
    show (TypeCast l r) = "cannot convert " ++ show l ++ " to " ++ show r
    show (TypeMismatchOp l r) = "cannot perform operation on types " ++ show l ++ " and " ++ show r
    -- show (TypeMismatchOp op l r) = "cannot perform operation (" ++ showSyn op ++ ") on types " ++ show l ++ " and " ++ show r
                            

data NameError = NameNotDefined LangIdent 
               | NameNotFunction LangIdent
               | NameNotValue LangIdent
               | NameNotOp LangIdent


nameNotDefinedErr :: LangIdent -> LangError
nameNotDefinedErr  = nameErr . NameNotDefined


nameNotFunctionErr :: LangIdent -> LangError
nameNotFunctionErr = nameErr . NameNotFunction


nameNotValueErr :: LangIdent -> LangError
nameNotValueErr    = nameErr . NameNotValue


nameNotOpErr :: LangIdent -> LangError
nameNotOpErr       = nameErr . NameNotOp


instance Show NameError where
    show (NameNotDefined  (LangIdent name)) = "not in scope: "         ++ name
    show (NameNotFunction (LangIdent name)) = "not a valid function: " ++ name
    show (NameNotValue    (LangIdent name)) = "no value assigned: "    ++ name
    show (NameNotOp       (LangIdent name)) = "non-existant operator: " ++ name
                                  

data CallError = 
    WrongNumberOfArguments Int Int
    deriving (Eq)
             

wrongNumberOfArgumentsErr :: Int -> Int -> LangError
wrongNumberOfArgumentsErr expect = callErr . WrongNumberOfArguments expect 
             
             
instance Show CallError where
    show (WrongNumberOfArguments x y) = "wrong number of arguments: expected " ++ show x ++ " but got " ++ show y


data LError = LError { errorErr    :: LangError  -- The actual error
                     , errorSource :: String
                     , errorPos    :: SourceRef -- Position at which the error occurred
                     , errorText :: String -- Additonal text representing the error
                     }


instance Show LError where
    show (LError { errorErr=ee
                 , errorPos=SourceRef (start,end)
                 , errorText=et
                 })
        = cEp ++ cEt ++ cEe
          where cEp = concat ["[", showPos start, "-", showPos end, "]"] ++ "\n"
                cEt = "in " ++ et ++ "\n"
                cEe = show ee
                showPos (SourcePos (cn,ln,_)) 
                    = concat ["(", show ln, ",", show cn, ")"]
                      

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
                       

indexOutOfBoundsErr :: Int -> LangError
indexOutOfBoundsErr = litErr . IndexOutOfBoundsError


instance Show LitError where
    show (IndexOutOfBoundsError x) = "index out of bounds: " ++ show x






