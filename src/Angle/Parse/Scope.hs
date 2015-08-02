module Angle.Parse.Scope
    ( Scope(..)
    , emptyScope
    , VarVal(..)
    , emptyVar
    , BindEnv
    , resolve
    , outermostScope
    , isOutermostScope
    , isDefinedIn
    , mergeScope
    , setVarFunInScope
    , setVarLitInScope
    , setVarClassInScope
    , deleteLitFromScope
    , deleteFunFromScope
    , resolveLit
    , resolveFun
    ) where

import Control.Monad
import qualified Data.Map as M    
import Data.Maybe (fromJust, isJust)
import Data.Function (on)

import Angle.Parse.Var
import Angle.Types.Lang


-- | Mapping from variables to values.
-- type BindEnv = M.Map LangIdent VarVal
type BindEnv a = M.Map LangIdent (VarVal a)

    
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
                    Just _ -> False


-- | True if the scope contains a defition for the given
-- identifier.

isDefinedIn binds name scope = isJust $ M.lookup name $ binds scope 

isLitIn :: LangIdent -> Scope -> Bool
isLitIn = isDefinedIn valueBindings
          
isFunIn = isDefinedIn lambdaBindings
          
isClassIn = isDefinedIn classBindings
                                   

-- | Runs a function in the outer scope of that provided.
--
-- Returns `Nothing' if no outer scope exists.
withOuterScope :: Scope -> (Scope -> a) -> Maybe a
withOuterScope sc f = liftM f (outerScope sc)
                      

-- | @withOutermostScope f scope@ runs @f@ with the parent-most
-- scope of @scope@ as it's argument.
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
innerScopeDefining binds name scope
    = if isDefinedIn binds name scope
      then Just scope
      else join $ withOuterScope scope (innerScopeDefining binds name)
           
innerScopeDefiningLit = innerScopeDefining valueBindings
innerScopeDefiningFun = innerScopeDefining lambdaBindings
innerScopeDefiningClass = innerScopeDefining classBindings
    

-- | Retrieves the variable's value from the local-most
-- scope in which it is defined.
-- 
-- Returns Nothing if no definition is found.
resolveLit = resolve valueBindings
             
resolveFun = resolve lambdaBindings
                               
resolve binds name scope = case innerScopeDefining binds name scope of
                             Nothing -> Nothing
                             Just scope' -> fromCurrentScope binds scope'
                                                                    where fromCurrentScope b s = M.lookup name (b s)
                                 

-- | A scope with no parent or bindings.
emptyScope :: Scope
emptyScope = Scope { 
               outerScope = Nothing
             , valueBindings = M.empty
             , lambdaBindings = M.empty
             , classBindings = M.empty
             }


-- | Run a function over the bindings of a scope.

onLitBindings f scope = scope { valueBindings = f $ valueBindings scope }
                        
onFunBindings f scope = scope { lambdaBindings = f $ lambdaBindings scope }
                        
onClassBindings f scope = scope { classBindings = f $ classBindings scope }


setVarLitInScope name val = onLitBindings (M.insert name val)
      
setVarFunInScope name val = onFunBindings (M.insert name val)
                            
setVarClassInScope name val = onClassBindings (M.insert name val)
                                         


-- | Merge the binding values of the scopes,
-- favouring the first when a definition exists
-- in both, but always favouring a definition
-- over no definition.
mergeScope :: Scope -> Scope -> Scope
mergeScope sc1 sc2
    = let nLits = M.union `on` valueBindings
          nFuns = M.union `on` lambdaBindings
          nClss = M.union `on` classBindings
      in sc1 { valueBindings = nLits sc1 sc2
             , lambdaBindings = nFuns sc1 sc2
             , classBindings = nClss sc1 sc2
             } 


deleteLitFromScope :: LangIdent -> Scope -> Scope
deleteLitFromScope name =  onLitBindings (M.delete name)

deleteFunFromScope name = onFunBindings (M.delete name)






