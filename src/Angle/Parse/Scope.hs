module Angle.Parse.Scope
    ( Scope(..)
    , emptyScope
    , VarVal(..)
    , emptyVar
    , CallSig(..)
    , BindEnv
    ) where

import Control.Monad
import qualified Data.Map as M    
import Data.Maybe (fromJust)

import Angle.Parse.Var
import Angle.Types.Lang



type BindEnv = M.Map Ident VarVal

    
type Ident = String


data CallSig = CallSig [Ident] Stmt
               deriving (Show)


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

-- Scope API when in Exec
-- - changing scope
--   - make new scope
--     (newScope :: Exec ())
--   - go to parent
--     (upScope :: Exec ())

-- *****************
-- ***** SCOPE *****
-- *****************

-- | Represents the current scope.
data Scope = Scope 
    { outerScope :: Maybe Scope -- ^Parent scope, if any
    , bindings   :: BindEnv
    } deriving (Show)


-- | True if the given scope has no parent scopes.           
isOutermostScope :: Scope -> Bool
isOutermostScope s = case outerScope s of
                    Nothing -> True
                    Just _ -> False


-- | @name `isDefinedIn` scope@ is True if @scope@
-- contains a definition for @name@.
isDefinedIn :: Ident -> Scope -> Bool
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
withOutermostScope f scope = 
    if isOutermostScope scope
    then f scope
    else withOutermostScope f (fromJust $ outerScope scope)
                      

-- | Finds the local-most Scope that contains a definition
-- for the specified identifier.
innerScopeDefining :: Ident -> Scope -> Maybe Scope
innerScopeDefining name scope = 
    if name `isDefinedIn` scope
    then Just scope
    else join $ withOuterScope scope (innerScopeDefining name)
    

-- | @resolve name scope@ Retrieves the @name@'s value
-- from the local-most scope in which it is defined.
--
-- Returns Nothing if there is no definition for @name@.
resolve :: Ident -> Scope -> Maybe VarVal
resolve name scope = if name `isDefinedIn` scope
                     then fromCurrentScope name scope
                     else outerScope scope >>= resolve name
    where fromCurrentScope n s = M.lookup n (bindings s)
                                 
-- | A scope with no parent or bindings
emptyScope :: Scope
emptyScope = Scope { 
               outerScope = Nothing
             , bindings = M.empty
             }

-- | Run a function over the bindings of a scope.
onBindings :: (BindEnv -> BindEnv) -> Scope -> Scope
onBindings f scope = scope { bindings = f $ bindings scope }

-- | Boolean determines whether to overwrite ident if it
-- exists.
setVarInScope :: Ident -> VarVal -> Scope -> Bool -> Scope
setVarInScope name val scope@(Scope{bindings=binds}) overwrite
    = if name `isDefinedIn` scope 
      then if overwrite
           then scope {bindings=M.alter (\_ -> Just val) name binds}
           else scope
      else scope {bindings=M.alter (\_ -> Just val) name binds}