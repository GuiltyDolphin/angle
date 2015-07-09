{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE FlexibleInstances #-}
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
