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
    , deleteLitFromScope
    , emptyScope
    , emptyVar
    , isDefinedIn
    , isOutermostScope
    , mergeScope
    , resolve
    , setVarFunInScope
    , setVarLitInScope
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
data GenScope n v f = Scope
    { outerScope :: Maybe (GenScope n v f) -- ^ Parent scope, if any.
    , valueBindings :: BindEnv n v
    , lambdaBindings :: BindEnv n f
    } deriving (Show, Eq)


-- type GenScope n v f = (Ord n) => GScope n v f


-- | True if the given scope has no parent scopes.
isOutermostScope :: GenScope n v f -> Bool
isOutermostScope s = case outerScope s of
                    Nothing -> True
                    Just _  -> False


-- | True if the scope contains a defition for the given
-- identifier.
isDefinedIn :: (Ord n) => (GenScope n v f -> BindEnv n a) -> n -> GenScope n v f -> Bool
isDefinedIn binds name scope = isJust $ lookupBind name $ binds scope


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
withOuterScope :: GenScope n v f -> (GenScope n v f -> a) -> Maybe a
withOuterScope sc f = liftM f (outerScope sc)


-- | Finds the local-most GenScope that contains a definition
-- for the specified identifier.
innerScopeDefining :: (Ord n) => (GenScope n v f -> BindEnv n a) -> n -> GenScope n v f -> Maybe (GenScope n v f)
innerScopeDefining binds name scope
    = if isDefinedIn binds name scope
      then Just scope
      else join $ withOuterScope scope (innerScopeDefining binds name)


-- | Retrieves the variable's definition from the local-most
-- scope in which it is defined.
--
-- Returns Nothing if no definition is found.
resolve :: (Ord n) => (GenScope n v f -> BindEnv n a) -> n -> GenScope n v f -> Maybe (VarVal a)
resolve binds name scope =
    case innerScopeDefining binds name scope of
             Nothing     -> Nothing
             Just scope' -> fromCurrentScope binds scope'
  where
    fromCurrentScope b = lookupBind name . b


-- | A scope with no parent or bindings.
emptyScope :: GenScope n v f
emptyScope = Scope {
               outerScope = Nothing
             , valueBindings = emptyBindEnv
             , lambdaBindings = emptyBindEnv
             }


-- | Run a function over the bindings of a scope.
onLitBindings
  :: (BindEnv n v -> BindEnv n v) -> GenScope n v f -> GenScope n v f
onLitBindings f scope = scope { valueBindings = f $ valueBindings scope }


onFunBindings
  :: (BindEnv n f -> BindEnv n f) -> GenScope n v f -> GenScope n v f
onFunBindings f scope = scope { lambdaBindings = f $ lambdaBindings scope }


insertVar :: (Ord n) => n -> VarVal a -> BindEnv n a -> BindEnv n a
insertVar = insertBind


-- | Set the value definition for the given variable in the given
-- scope.
setVarLitInScope :: (Ord n) => n -> VarVal v -> GenScope n v f -> GenScope n v f
setVarLitInScope name val = onLitBindings (insertVar name val)


-- | Set the lambda definition for the given variable in the given
-- scope.
setVarFunInScope :: (Ord n) => n -> VarVal f -> GenScope n v f -> GenScope n v f
setVarFunInScope name val = onFunBindings (insertVar name val)



-- | Merge the binding values of the scopes, favouring the first
-- when a definition exists in both, but always favouring a
-- definition over no definition.
mergeScope :: (Ord n) => GenScope n v f -> GenScope n v f -> GenScope n v f
mergeScope sc1 sc2
    = let nLits = mergeBinds `on` valueBindings
          nFuns = mergeBinds `on` lambdaBindings
      in sc1 { valueBindings = nLits sc1 sc2
             , lambdaBindings = nFuns sc1 sc2
             }


-- | Remove the value binding of a variable from the given scope.
deleteLitFromScope :: (Ord n) => n -> GenScope n v f -> GenScope n v f
deleteLitFromScope =  onLitBindings . deleteBind



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
