module Angle.Parse.Error
    ( typeMismatchErr
    , typeUnexpectedErr
    , typeNotValidErr
    , nameNotDefinedErr
    , nameNotFunctionErr
    , nameNotValueErr
    , LangError
    ) where

import Angle.Types.Lang

import Control.Monad.Error

data LangError = TypeError TypeError
               | SyntaxError String
               | NameError NameError
               | DefaultError String
               | UserError String -- TODO: Add keyword and
                                  -- structures for allowing
                                  -- the user to throw errors
                 
typeErr    = TypeError
syntaxErr  = SyntaxError
nameErr    = NameError
defaultErr = DefaultError

instance Show LangError where
    show (TypeError e)    = "wrong type in expression: " ++ show e
    show (SyntaxError s)  = "syntax error: " ++ s
    show (NameError v)    = "name error: " ++ show v
    show (DefaultError s) = "defaultError: " ++ s

instance Error LangError where
    noMsg = DefaultError ""
    strMsg = DefaultError

data TypeError = TypeMismatch   LangType LangType
               | TypeUnexpected LangType LangType
               | TypeNotValid   LangType
                 
typeMismatchErr   t1 = typeErr . TypeMismatch   t1
typeUnexpectedErr t1 = typeErr . TypeUnexpected t1
typeNotValidErr      = typeErr . TypeNotValid
               
instance Show TypeError where
    show (TypeMismatch l r)   = "type mismatch: got (" ++ show l ++ ", " ++ show r ++ ") but both types should be the same"
    show (TypeUnexpected l r) = "unexpected type: " ++ show l ++ ", expecting: " ++ show r
    show (TypeNotValid l)     = "type not valid for scenario: " ++ show l
                            
data NameError = NameNotDefined LangIdent 
               | NameNotFunction LangIdent
               | NameNotValue LangIdent

nameNotDefinedErr, nameNotFunctionErr, nameNotValueErr :: LangIdent -> LangError
nameNotDefinedErr  = nameErr . NameNotDefined
nameNotFunctionErr = nameErr . NameNotFunction
nameNotValueErr    = nameErr . NameNotValue

instance Show NameError where
    show (NameNotDefined  name) = "not in scope: "         ++ name
    show (NameNotFunction name) = "not a valid function: " ++ name
    show (NameNotValue    name) = "no value assigned: "    ++ name
                               


data LError = LError { errorErr    :: LangError  -- The actual error
                     , errorSource :: String
                     , lerrorPos   :: (Int, Int, Int) -- Position at which the error occurred
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
                         
instance Error LError where
    noMsg = LError {errorErr=noMsg, lerrorPos=(0,0,0), errorSource=""}
    strMsg m = noMsg {errorErr=strMsg m}
