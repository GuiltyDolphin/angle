module Angle.Parse.Prep
    (
    ) where

import Angle.Types.Lang
    
isRedundantMultiStmt :: Stmt -> Bool
isRedundantMultiStmt (MultiStmt xs) = length xs == 1
isRedundantMultiStmt _ = False
                         

-- TODO: This won't work recursively.
redundantMultisToSingles :: Stmt -> Stmt
redundantMultisToSingles x@(SingleStmt _) = x
redundantMultisToSingles x@(MultiStmt xs) 
    | isRedundantMultiStmt x = head xs
    | otherwise = x

-- | Attempt to reduce an operation to a single boolean.
reduceBoolLiterals :: LangOp -> Expr
reduceBoolLiterals (SpecOp OpNot x)
    = case x of
        ExprLit x' -> case x' of
                        LitBool x'' -> ExprLit $ LitBool $ not x''
                        y' -> ExprLit y'
        y -> y
reduceBoolLiterals x = ExprOp x
