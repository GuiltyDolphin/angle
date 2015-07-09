{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE FlexibleInstances #-}
module Angle.Parse.Parser
    (
    ) where

import Angle.Lex.Lexer
import Control.Monad.Reader
import Control.Monad.Error
    

newtype LangEnv = Env [(Expr, Expr)]
    
class LangAdd a b c | a b -> c where
    langAdd :: a -> b -> c

instance (Num a) => LangAdd a a a where
    langAdd = (+)
