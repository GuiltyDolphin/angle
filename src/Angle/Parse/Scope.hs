module Angle.Parse.Scope
    ( Scope(..)
    , emptyScope
    , VarVal(..)
    , emptyVar
    , BindEnv(..)
    , bindEnvFromList
    , resolve
    , outermostScope
    , isOutermostScope
    , isDefinedIn
    , mergeScope
    , setVarFunInScope
    , setVarLitInScope
    , setVarClassInScope
    , deleteLitFromScope
    , resolveLit
    ) where

import Control.Applicative
import Control.Monad
import qualified Data.Map as M
import Data.Maybe (fromJust, isJust)
import Data.Function (on)

import Angle.Types.Lang


bindEnvFromList :: [(LangIdent, VarVal a)] -> BindEnv a
bindEnvFromList = BindEnv . M.fromList


emptyBindEnv :: BindEnv a
emptyBindEnv = BindEnv M.empty


newtype BindEnv a = BindEnv
    { unBindEnv :: M.Map LangIdent (VarVal a) }
    deriving (Show, Eq)


type BindMap a = M.Map LangIdent (VarVal a)

-- Scope API:
-- - changing scope
--   - new scope
--     (newScope :: Maybe Scope -> Scope)
--   - global scope
--     (globalScope :: Scope -> Scope)
--   - parent scope
--     (parentScope :: Scope -> Maybe Scope)
-- - setting variables
--   - in current scope (maybe only applies to an Exec?)
--     - function value
--     - literal value
-- - executing in different scopes
--   - in a new scope
--   - in the global scope
--   - in the current scope
--   - in the parent scope
-- - resolving variables
--   - check current scope, then outer scopes
--     (resolve :: Ident -> Scope -> Maybe VarVal)
--   - only check current scope
--     (resolveCurrent :: Ident -> Scope -> Maybe VarVal)
--   - only check global scope
--     (resolveGlobal :: Ident -> Scope -> Maybe VarVal)
-- TODO/NOTES
-- - Resolving literals & functions rather than just name


-- | Contains variable-value bindings, along with a reference
-- to a parent scope.
data Scope = Scope
    { outerScope :: Maybe Scope -- ^ Parent scope, if any.
--    , bindings   :: BindEnv
    , valueBindings :: BindEnv LangLit
    , lambdaBindings :: BindEnv Lambda
    , classBindings :: BindEnv Lambda -- ClassLambda
    } deriving (Show, Eq)


-- | True if the given scope has no parent scopes.
isOutermostScope :: Scope -> Bool
isOutermostScope s = case outerScope s of
                    Nothing -> True
                    Just _  -> False


-- | True if the scope contains a defition for the given
-- identifier.
isDefinedIn :: (Scope -> BindEnv a) -> LangIdent -> Scope -> Bool
isDefinedIn binds name scope = isJust $ lookupBind name $ binds scope


onBind :: (BindMap a -> b) -> BindEnv a -> b
onBind f = f . unBindEnv


withBind :: (BindMap a -> BindMap a) -> BindEnv a -> BindEnv a
withBind f = toBind . onBind f


toBind :: BindMap a -> BindEnv a
toBind = BindEnv


onBinds :: (BindMap a -> BindMap a -> BindMap a) -> BindEnv a -> BindEnv a -> BindEnv a
onBinds f x = toBind . (f `on` unBindEnv) x


-- | Runs a function in the outer scope of that provided.
--
-- Returns `Nothing' if no outer scope exists.
withOuterScope :: Scope -> (Scope -> a) -> Maybe a
withOuterScope sc f = liftM f (outerScope sc)


-- | Get the parent-most scope of the given scope.
outermostScope :: Scope -> Scope
outermostScope scope =
    if isOutermostScope scope
    then scope
    else outermostScope (fromJust $ outerScope scope)


-- | Finds the local-most Scope that contains a definition
-- for the specified identifier.
innerScopeDefining binds name scope
    = if isDefinedIn binds name scope
      then Just scope
      else join $ withOuterScope scope (innerScopeDefining binds name)


-- | Retrieves the variable's value from the local-most
-- scope in which it is defined.
--
-- Returns Nothing if no definition is found.
resolveLit :: LangIdent -> Scope -> Maybe (VarVal LangLit)
resolveLit = resolve valueBindings


resolve :: (Scope -> BindEnv a) -> LangIdent -> Scope -> Maybe (VarVal a)
resolve binds name scope = case innerScopeDefining binds name scope of
                             Nothing     -> Nothing
                             Just scope' -> fromCurrentScope binds scope'
                                                                    where fromCurrentScope b = lookupBind name . b


-- | A scope with no parent or bindings.
emptyScope :: Scope
emptyScope = Scope {
               outerScope = Nothing
             , valueBindings = emptyBindEnv
             , lambdaBindings = emptyBindEnv
             , classBindings = emptyBindEnv
             }


-- | Run a function over the bindings of a scope.

onLitBindings
  :: (BindEnv LangLit -> BindEnv LangLit) -> Scope -> Scope
onLitBindings f scope = scope { valueBindings = f $ valueBindings scope }


onFunBindings
  :: (BindEnv Lambda -> BindEnv Lambda) -> Scope -> Scope
onFunBindings f scope = scope { lambdaBindings = f $ lambdaBindings scope }


onClassBindings
  :: (BindEnv Lambda -> BindEnv Lambda) -> Scope -> Scope
onClassBindings f scope = scope { classBindings = f $ classBindings scope }


insertVar :: LangIdent -> VarVal a -> BindEnv a -> BindEnv a
insertVar = insertBind


setVarLitInScope :: LangIdent -> VarVal LangLit -> Scope -> Scope
setVarLitInScope name val = onLitBindings (insertVar name val)


setVarFunInScope :: LangIdent -> VarVal Lambda -> Scope -> Scope
setVarFunInScope name val = onFunBindings (insertVar name val)


setVarClassInScope :: LangIdent -> VarVal Lambda -> Scope -> Scope
setVarClassInScope name val = onClassBindings (insertVar name val)



-- | Merge the binding values of the scopes,
-- favouring the first when a definition exists
-- in both, but always favouring a definition
-- over no definition.
mergeScope :: Scope -> Scope -> Scope
mergeScope sc1 sc2
    = let nLits = mergeBinds `on` valueBindings
          nFuns = mergeBinds `on` lambdaBindings
          nClss = mergeBinds `on` classBindings
      in sc1 { valueBindings = nLits sc1 sc2
             , lambdaBindings = nFuns sc1 sc2
             , classBindings = nClss sc1 sc2
             }


deleteLitFromScope :: LangIdent -> Scope -> Scope
deleteLitFromScope =  onLitBindings . deleteBind



deleteBind :: LangIdent -> BindEnv a -> BindEnv a
deleteBind = withBind . M.delete


mergeBinds :: BindEnv a -> BindEnv a -> BindEnv a
mergeBinds = onBinds M.union


lookupBind :: LangIdent -> BindEnv a -> Maybe (VarVal a)
lookupBind = onBind . M.lookup


insertBind :: LangIdent -> VarVal a -> BindEnv a -> BindEnv a
insertBind n = withBind . M.insert n


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
