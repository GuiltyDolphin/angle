module Angle.Parse.Scope
    ( Scope(..)
    , emptyScope
    , VarVal(..)
    , emptyVar
    , BindEnv
    , setVarInScope
    , resolve
    , outermostScope
    , isOutermostScope
    , onBindings
    , isDefinedIn
    , mergeScope
    , deleteFromScope
    ) where

import Control.Monad
import qualified Data.Map as M    
import Data.Maybe (fromJust)

import Angle.Parse.Var
import Angle.Types.Lang


type BindEnv = M.Map LangIdent VarVal

    
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


-- | Represents the current scope.
data Scope = Scope 
    { outerScope :: Maybe Scope -- ^Parent scope, if any
    , bindings   :: BindEnv
    } deriving (Show, Eq)


-- | True if the given scope has no parent scopes.           
isOutermostScope :: Scope -> Bool
isOutermostScope s = case outerScope s of
                    Nothing -> True
                    Just _ -> False


-- | @name `isDefinedIn` scope@ is True if @scope@
-- contains a definition for @name@.
isDefinedIn :: LangIdent -> Scope -> Bool
isDefinedIn name scope = case M.lookup name (bindings scope) of
                         Nothing -> False
                         Just _ -> True
                                   

-- | Runs a function in the outer scope of that provided.
--
-- Returns `Nothing' if no outer scope exists.
withOuterScope :: Scope -> (Scope -> a) -> Maybe a
withOuterScope sc f = liftM f (outerScope sc)
                      

-- | @withOutermostScope f scope@ runs @f@ in the parent-most
-- scope of @scope@.
withOutermostScope :: (Scope -> a) -> Scope -> a
withOutermostScope f = f . outermostScope
         

-- | Get the parent-most scope of the given scope.
outermostScope :: Scope -> Scope
outermostScope scope =
    if isOutermostScope scope
    then scope
    else outermostScope (fromJust $ outerScope scope)
                      

-- | Finds the local-most Scope that contains a definition
-- for the specified identifier.
innerScopeDefining :: LangIdent -> Scope -> Maybe Scope
innerScopeDefining name scope = 
    if name `isDefinedIn` scope
    then Just scope
    else join $ withOuterScope scope (innerScopeDefining name)
    

-- | @resolve name scope@ Retrieves the @name@'s value
-- from the local-most scope in which it is defined.
--
-- Returns Nothing if there is no definition for @name@.
resolve :: LangIdent -> Scope -> Maybe VarVal
resolve name scope = case innerScopeDefining name scope of
                       Nothing -> Nothing
                       Just scope' -> fromCurrentScope scope'
    where fromCurrentScope s = M.lookup name (bindings s)
                                 

-- | A scope with no parent or bindings
emptyScope :: Scope
emptyScope = Scope { 
               outerScope = Nothing
             , bindings = M.empty
             }


-- | Run a function over the bindings of a scope.
onBindings :: (BindEnv -> BindEnv) -> Scope -> Scope
onBindings f scope = scope { bindings = f $ bindings scope }


setVarInScope 
    :: LangIdent 
    -> VarVal 
    -> Scope 
    -> Bool  -- ^ Overwrite Var if it already exists.
    -> Scope
setVarInScope name val scope@(Scope{bindings=binds}) overwrite
    = if name `isDefinedIn` scope 
      then if overwrite
           then scope {bindings=M.alter (\_ -> Just val) name binds}
           else scope
      else scope {bindings=M.alter (\_ -> Just val) name binds}


-- | Merge the binding values of the scopes,
-- favouring the first when a definition exists
-- in both, but always favouring a definition
-- over no definition.
mergeScope :: Scope -> Scope -> Scope
mergeScope x@(Scope {bindings=xs}) (Scope {bindings=ys})
    = x { bindings=M.union xs ys }
      

-- | Remove the identifier's definition from the given scope.
deleteFromScope :: LangIdent -> Scope -> Scope
deleteFromScope name scope@(Scope {bindings=binds})
    = scope { bindings = M.delete name binds } 









