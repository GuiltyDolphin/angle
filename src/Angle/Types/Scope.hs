{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ImpredicativeTypes #-}
{-|
Module      : Angle.Types.Scope
Description : Defines functions for working with GenScopes.
Copyright   : Copyright (C) 2015 Ben Moon
License     : GNU GPL, version 3
Maintainer  : GuiltyDolphin@gmail.com
Stability   : alpha

Each variable in Angle belongs to a scope.

Each scope contains a reference to its parent scope and the binding
environment that maps variables to their respective values.

Each variable can have a function definition and a value definition,
which are resolved independently and at distinguishable times.

A variable is considered in-scope if it exists in the current scope
or any of the parent scopes.

The outer-most scope is called the global scope and is accessible
from all other parts of a program.
-}
module Angle.Types.Scope
    ( GenScope(..)
    , BindEnv(..)
    , VarVal(..)
    , bindEnvFromList
    , deleteFromScope
    , emptyScope
    , emptyVar
    , isDefinedIn
    , isOutermostScope
    , mergeScope
    , resolve
    , setVarInScope
    , modifyOuterScope
    , modifyGlobalScope
    , globalScope
    ) where


import Control.Monad
import qualified Data.Map as M
import Data.Maybe (isJust)
import Data.Function (on)


-- | Convert a list to a binding environment.
bindEnvFromList :: (Ord n) => [(n, VarVal a)] -> BindEnv n a
bindEnvFromList = BindEnv . M.fromList


emptyBindEnv :: BindEnv n a
emptyBindEnv = BindEnv M.empty


-- | Binding environment.
newtype BindEnv n a = BindEnv
    { unBindEnv :: M.Map n (VarVal a) }
    deriving (Show, Eq)


type BindMap n a = M.Map n (VarVal a)


-- | Contains variable-value bindings, along with a reference
-- to a parent scope.
data GenScope n v = Scope
    { outerScope :: Maybe (GenScope n v) -- ^ Parent scope, if any.
    , bindings   :: BindEnv n v
    } deriving (Show, Eq)




-- | True if the given scope has no parent scopes.
isOutermostScope :: GenScope n v -> Bool
isOutermostScope s = case outerScope s of
                    Nothing -> True
                    Just _  -> False


-- | The outermost scope of the given scope stack.
globalScope :: GenScope n v -> GenScope n v
globalScope sc@(Scope { outerScope = Nothing }) = sc
globalScope (Scope { outerScope = Just sc }) = globalScope sc


-- | True if the scope contains a defition for the given identifier.
isDefinedIn :: (Ord n) => n -> GenScope n v -> Bool
isDefinedIn name scope = isJust $ lookupBind name $ bindings scope


onBind :: (BindMap n a -> b) -> BindEnv n a -> b
onBind f = f . unBindEnv


withBind :: (BindMap n a -> BindMap n a) -> BindEnv n a -> BindEnv n a
withBind f = toBind . onBind f


toBind :: BindMap n a -> BindEnv n a
toBind = BindEnv


onBinds :: (BindMap n a -> BindMap n a -> BindMap n a) -> BindEnv n a -> BindEnv n a -> BindEnv n a
onBinds f x = toBind . (f `on` unBindEnv) x


-- | Runs a function in the outer scope of that provided.
--
-- Returns `Nothing' if no outer scope exists.
withOuterScope :: GenScope n v -> (GenScope n v -> a) -> Maybe a
withOuterScope sc f = liftM f (outerScope sc)


-- | Allows the modification of a parent scope without modifying
-- the current scope (bar the parent changing).
modifyOuterScope :: GenScope n v -> (GenScope n v -> GenScope n v) -> GenScope n v
modifyOuterScope sc parF =
    case outerScope sc of
        Nothing -> sc
        Just parS -> let newPar = parF parS in sc { outerScope = Just newPar }


-- | Modifies the global scope while not affecting non-globals.
modifyGlobalScope :: GenScope n v -> (GenScope n v -> GenScope n v) -> GenScope n v
modifyGlobalScope sc globF =
    if isOutermostScope sc
    then globF sc
    else let (Just outer) = outerScope sc
         in sc { outerScope = Just $ modifyGlobalScope outer globF }


-- | Finds the local-most GenScope that contains a definition
-- for the specified identifier.
innerScopeDefining :: (Ord n) => n -> GenScope n v -> Maybe (GenScope n v)
innerScopeDefining name scope
    = if isDefinedIn name scope
      then Just scope
      else join $ withOuterScope scope (innerScopeDefining name)


-- | Retrieves the variable's definition from the local-most
-- scope in which it is defined.
--
-- Returns Nothing if no definition is found.
resolve :: (Ord n) => n -> GenScope n v -> Maybe (VarVal v)
resolve name scope =
    case innerScopeDefining name scope of
             Nothing     -> Nothing
             Just scope' -> fromCurrentScope scope'
  where
    fromCurrentScope = lookupBind name . bindings


-- | A scope with no parent or bindings.
emptyScope :: GenScope n v
emptyScope = Scope {
               outerScope = Nothing
             , bindings = emptyBindEnv
             }


-- | Run a function over the bindings of a scope.
onBindings
  :: (BindEnv n v -> BindEnv n v) -> GenScope n v -> GenScope n v
onBindings f scope = scope { bindings = f $ bindings scope }


insertVar :: (Ord n) => n -> VarVal a -> BindEnv n a -> BindEnv n a
insertVar = insertBind


-- | Set the value definition for the given variable in the given
-- scope.
setVarInScope :: (Ord n) => n -> VarVal v -> GenScope n v -> GenScope n v
setVarInScope name val = onBindings (insertVar name val)


-- | Merge the binding values of the scopes, favouring the first
-- when a definition exists in both, but always favouring a
-- definition over no definition.
mergeScope :: (Ord n) => GenScope n v -> GenScope n v -> GenScope n v
mergeScope sc1 sc2
    = let nLits = mergeBinds `on` bindings
      in sc1 { bindings = nLits sc1 sc2 }


-- | Remove the value binding of a variable from the given scope.
deleteFromScope :: (Ord n) => n -> GenScope n v -> GenScope n v
deleteFromScope =  onBindings . deleteBind



deleteBind :: (Ord n) => n -> BindEnv n a -> BindEnv n a
deleteBind = withBind . M.delete


mergeBinds :: (Ord n) => BindEnv n a -> BindEnv n a -> BindEnv n a
mergeBinds = onBinds M.union


lookupBind :: (Ord n) => n -> BindEnv n a -> Maybe (VarVal a)
lookupBind = onBind . M.lookup


insertBind :: (Ord n) => n -> VarVal a -> BindEnv n a -> BindEnv n a
insertBind n = withBind . M.insert n


-- | Represents a variable definition.
data VarVal a = VarVal
    { varDef :: Maybe a
    , varBuiltin :: Bool
    } deriving (Show, Eq, Ord)


-- | Variable with no definitions.
emptyVar :: VarVal a
emptyVar = VarVal { varDef = Nothing
                  , varBuiltin = False }
