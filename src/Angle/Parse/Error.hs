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
    , ParserError
    , CanError(..)
    , CanErrorWithPos(..)
    , throwError
    , langError
    , SourcePos
    , LError(..)
    , AngleError(..)
    , throwParserError
    , indexOutOfBoundsErr
    , defaultErr
    , userErr
    , returnFromGlobalErr
    , callBuiltinErr
    , implementationErr
    , throwImplementationErr
    , throwReturn
    , catchReturn
    ) where


import Control.Monad.Error
import Control.Monad.Trans.Except
import Data.Function (on)
import Data.Monoid
   
import Angle.Scanner
import Angle.Types.Lang


-- class (MonadError AngleError m) => CanError (m :: * -> *)
   
-- instance CanError (Either AngleError)
    
-- class (MonadError AngleError (ExceptT AngleError m)) => CanError m

-- type LangError = ExceptT AngleError
-- class (MonadError AngleError m) => CanError m
   
-- type CanError = ExceptT AngleError
class (Monad m) => CanError (m :: * -> *) where
    throwAE :: AngleError -> m a
    catchAE :: m a -> (AngleError -> m a) -> m a
               

-- | General error structure.
data AngleError = ParserError 
    { parserErrSourceRef :: SourceRef 
    , parserErrErr :: ParserError
    , parserErrSourceText :: String
    }
                | ImplementationError String
                | ControlException ControlException


parserErr :: SourceRef -> ParserError -> String -> AngleError
parserErr = ParserError


implementationErr :: String -> AngleError
implementationErr = ImplementationError


controlException :: ControlException -> AngleError
controlException = ControlException                  


instance Show AngleError where
    show (ImplementationError x) = "Implementation error: " ++ x
    show (ParserError { parserErrErr=ee
                      , parserErrSourceRef=SourceRef (start,end)
                      , parserErrSourceText=es
                      })
        = cEp ++ cEt ++ cEe
          where cEp = concat ["[", showPos start, "-", showPos end, "]"] ++ "\n"
                cEt = let lns = lines es in
                      if null lns
                      then "no source\n"
                      else replicate (colNo start) ' ' ++ "v\n" ++ lns !! lineNo start ++ "\n"
                cEe = show ee
                showPos sp 
                    = let ln = show $ lineNo sp
                          cn = show $ colNo sp
                      in concat ["(", ln, ",", cn, ")"]


data ParserError = TypeError TypeError
                 | NameError NameError
                 | CallError CallError
                 | DefaultError String
                 | LitError LitError
                 | KeywordError KeywordError
                 | UserError String -- TODO: Add keyword and
                                  -- structures for allowing
                                  -- the user to throw errors
                 

typeErr :: TypeError -> ParserError
typeErr    = TypeError


nameErr :: NameError -> ParserError
nameErr    = NameError


callErr :: CallError -> ParserError
callErr    = CallError


defaultErr :: String -> ParserError
defaultErr = DefaultError


litErr :: LitError -> ParserError
litErr     = LitError
             

userErr :: String -> ParserError
userErr = UserError
          

keywordErr :: KeywordError -> ParserError
keywordErr = KeywordError


instance Show ParserError where
    show (TypeError e)    = "wrong type in expression: " ++ show e
    show (NameError v)    = "name error: " ++ show v
    show (CallError x)    = "call error: " ++ show x
    show (DefaultError s) = "defaultError: " ++ s
    show (LitError x) = "literal error: " ++ show x
    show (UserError x) = "user error: " ++ x
    show (KeywordError x) = "keyword error: " ++ show x


instance Error ParserError where
    noMsg = DefaultError ""
    strMsg = DefaultError


data TypeError = TypeMismatch   LangType LangType
               | TypeUnexpected LangType LangType
               | TypeNotValid   LangType
               | TypeCast LangType LangType
               | TypeMismatchOp LangType LangType

                 
typeMismatchErr :: LangType -> LangType -> ParserError
typeMismatchErr   t1 = typeErr . TypeMismatch   t1


typeUnexpectedErr :: LangType -> LangType -> ParserError
typeUnexpectedErr t1 = typeErr . TypeUnexpected t1


typeNotValidErr :: LangType -> ParserError
typeNotValidErr      = typeErr . TypeNotValid


typeNotValidErrT :: LangLit -> ParserError
typeNotValidErrT     = typeNotValidErr . typeOf


typeCastErr :: LangType -> LangType -> ParserError
typeCastErr       t1 = typeErr . TypeCast       t1


typeMismatchOpErr :: LangType -> LangType -> ParserError
typeMismatchOpErr t1 = typeErr . TypeMismatchOp t1


typeMismatchOpErrT :: LangLit -> LangLit -> ParserError
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


nameNotDefinedErr :: LangIdent -> ParserError
nameNotDefinedErr  = nameErr . NameNotDefined


nameNotFunctionErr :: LangIdent -> ParserError
nameNotFunctionErr = nameErr . NameNotFunction


nameNotValueErr :: LangIdent -> ParserError
nameNotValueErr    = nameErr . NameNotValue


nameNotOpErr :: LangIdent -> ParserError
nameNotOpErr       = nameErr . NameNotOp


instance Show NameError where
    show (NameNotDefined  (LangIdent name)) = "not in scope: "         ++ name
    show (NameNotFunction (LangIdent name)) = "not a valid function: " ++ name
    show (NameNotValue    (LangIdent name)) = "no value assigned: "    ++ name
    show (NameNotOp       (LangIdent name)) = "non-existant operator: " ++ name
                                  

data CallError = WrongNumberOfArguments Int Int
               | BuiltIn String
    deriving (Eq)
             

wrongNumberOfArgumentsErr :: Int -> Int -> ParserError
wrongNumberOfArgumentsErr expect = callErr . WrongNumberOfArguments expect 
                                   

callBuiltinErr :: String -> ParserError
callBuiltinErr = callErr . BuiltIn
             
             
instance Show CallError where
    show (WrongNumberOfArguments x y) = "wrong number of arguments: expected " ++ show x ++ " but got " ++ show y
    show (BuiltIn x) = "builtin: " ++ x


data LError = LError { errorErr    :: ParserError  -- The actual error
                     , errorSource :: String
                     , errorPos    :: SourceRef -- Position at which the error occurred
                     , errorText :: String -- Additonal text representing the error
                     }
            

data KeywordError = ReturnFromGlobal
                    deriving (Eq)
        

returnFromGlobalErr :: ParserError
returnFromGlobalErr = keywordErr ReturnFromGlobal
                      

instance Show KeywordError where
    show ReturnFromGlobal = "return from outermost scope"


instance Error LError where
    noMsg = LError {errorErr=noMsg, errorPos=SourceRef (beginningOfFile, beginningOfFile), errorSource="", errorText=""}
    strMsg m = noMsg {errorErr=strMsg m}
               

-- langError :: (CanError m) => ParserError -> m a
-- langError e
-- langError e = throwError noMsg { errorErr = e }
-- langError :: (CanError m) => ParserError -> m a
-- langError e = throwError ParserError { parserErrErr = e }
langError ::  (CanErrorWithPos m) => ParserError -> m a
langError = throwParserError -- throwAE ParserError { parserErrErr = e }


throwParserError :: (CanErrorWithPos m, Monad m) => ParserError -> m a
throwParserError e = do
  errPosRef <- getErrorPos
  errSource <- getErrorSource
  throwAE ParserError { parserErrSourceRef = errPosRef
                         , parserErrSourceText = errSource
                         , parserErrErr = e
                         }


-- throwImplementationErr :: (CanError m) => String -> m a
-- throwImplementationErr = throwError . implementationErr
throwImplementationErr :: (CanError m) => String -> m a
throwImplementationErr = throwAE . implementationErr


class (CanError m) => CanErrorWithPos m where
    getErrorPos :: m SourceRef
    getErrorSource :: m String


data LitError = IndexOutOfBoundsError Int
              deriving (Eq)
                       

indexOutOfBoundsErr :: Int -> ParserError
indexOutOfBoundsErr = litErr . IndexOutOfBoundsError


instance Show LitError where
    show (IndexOutOfBoundsError x) = "index out of bounds: " ++ show x



data ControlException = ControlReturn LangLit
                      deriving (Show, Eq)
                               

controlReturn :: LangLit -> AngleError
controlReturn = controlException . ControlReturn


throwReturn :: (CanError m) => LangLit -> m a
throwReturn = throwAE . controlReturn


catchReturn :: (CanError m) => m a -> (LangLit -> m a) -> m a
catchReturn ex h = ex `catchAE` 
                   (\e -> case e of
                            ControlException (ControlReturn v) -> h v
                            err -> throwAE err)
