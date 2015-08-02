module Angle.Parse.Var
    ( VarVal(..)
    , emptyVar
    ) where

import Angle.Types.Lang


-- VarVal API
-- - retrieving values
--   - function definition
--     (varFunDef :: VarVal -> CallSig)
--   - value definition
--     (varLitDef :: VarVal -> LangLit)
-- - setting values
--   - function definition
--     (varSetFunDef :: VarVal -> CallSig -> VarVal)
--   - value definition
--     (varSetLitDef :: VarVal -> LangLit -> VarVal)
--   - empty (basic) VarVal
--     (emptyVar :: VarVal)
-- - checking definitions
--   - function definition
--     (hasFunctionDefinition :: VarVal -> Bool)
--   - value definition
--     (hasLiteralDefinition :: VarVal -> Bool)
-- TODO/NOTES
-- - record for determining builtins?
--   (isBuiltin :: VarVal -> Bool)
--   would need to enforce builtins in all scopes
-- - record for constants
--   (isConst :: VarVal -> Bool)
--   cannot assign to constants


-- | Represents a variable definition.
data VarVal a = VarVal 
    { varDef :: Maybe a
    , varBuiltin :: Bool
    } deriving (Show, Eq)
            

-- | Variable with no definitions.
emptyVar :: VarVal a
emptyVar = VarVal { varDef = Nothing
                  , varBuiltin = False }

           
setVarDef :: VarVal a -> a -> VarVal a
setVarDef var val = var { varDef = Just val }
