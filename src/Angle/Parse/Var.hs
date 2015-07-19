module Angle.Parse.Var
    (
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
data VarVal = VarVal 
    { varLitDef :: Maybe LangLit
    , varFunDef :: Maybe CallSig
    } deriving (Show)
            

-- | Variable with no definitions.
emptyVar = VarVal { varLitDef = Nothing, varFunDef = Nothing }
           
 
-- | Set the literal definition of a VarVal.
setVarLit :: VarVal -> LangLit -> VarVal
setVarLit var val = var { varLitDef = Just val }


-- | Set the function definition of a VarVal.
setVarFun :: VarVal -> CallSig -> VarVal                    
setVarFun var fd = var { varFunDef = Just fd }
