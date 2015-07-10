{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Angle.Parse.Parser
    (
    ) where

import Angle.Lex.Lexer
import Control.Monad.Reader
import Control.Monad.Error

data LangError = TypeError Expr
               | SyntaxError String
               | UndefinedIdent String
               | DefaultError String

                       
                 
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
