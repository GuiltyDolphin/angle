{-# LANGUAGE RankNTypes #-}
{-|
Module      : Angle.Exec.Scope
Description : Functions for working with the runtime scope.
Copyright   : Copyright (C) 2015 Ben Moon
License     : GNU GPL, version 3
Maintainer  : GuiltyDolphin@gmail.com
Stability   : alpha

Builds upon "Angle.Types.Scope" by providing functions for
working with scopes during run-time.
-}
module Angle.Exec.Scope
    ( -- ** Traversing the scope
      newScope
    , upScope
    , getScope

    -- ** Outer scope
    , lookupVarOuter
    , assignVarOuter

    -- ** Current scope
    , lookupVarLocal
    , modifyScope
    , assignVarLocal
    , assignVarBuiltinLit

    -- ** Global scope
    , lookupVarGlobal
    , assignVarGlobal
    ) where


import Control.Monad

import Angle.Exec.Builtins ( handleBuiltinAssignLit )
import Angle.Exec.Error
import Angle.Exec.Types
import Angle.Types.Lang
import Angle.Types.Scope


type VarMaybeLookup = LangIdent -> ExecIO (Maybe LangLit)


type VarValLookup = LangIdent -> ExecIO (Maybe (VarVal LangLit))


-- | Determine which scope to lookup the variable in, then perform
-- the lookup.
lookupVarWithScope :: (Scope -> Maybe Scope) -> VarValLookup
lookupVarWithScope scopeF name = do
    actionScope <- liftM scopeF getScope
    return $ case actionScope of
               Nothing -> Nothing
               Just sc -> resolve name sc


lookupVarGlobalScope :: VarValLookup
lookupVarGlobalScope = lookupVarWithScope (Just . globalScope)


lookupVarScope :: (Scope -> Maybe Scope) -> VarMaybeLookup
lookupVarScope scopeF name = do
    actionScope <- liftM scopeF getScope
    case actionScope of
        Nothing -> return Nothing
        Just sc -> case resolve name sc of
                      Nothing -> return Nothing
                      Just x -> return $ varDef x



-- | Retrieve the current scope from the environment.
getScope :: ExecIO Scope
getScope = liftM currentScope getEnv


lookupVarLocal :: LangIdent -> ExecIO LangLit
lookupVarLocal (LangIdent "_it") = getEnvValue
lookupVarLocal (LangIdent "main") = liftM (LitBool . runAsMain) getEnv
lookupVarLocal n = lookupVarErr Just n


lookupVarOuter :: LangIdent -> ExecIO LangLit
lookupVarOuter = lookupVarWithScopeAndError outerScope nameNotDefinedLitErr


lookupVarErr :: (Scope -> Maybe Scope) -> LangIdent -> ExecIO LangLit
lookupVarErr scF n = lookupVarWithScopeAndError scF nameNotDefinedLitErr n >>= returnVal


lookupVarGlobal :: LangIdent -> ExecIO LangLit
lookupVarGlobal = lookupVarErr (Just . globalScope)


-- | Lookup the value for the given variable with the specified set
-- of bindings; throw the specified exception if no value can be found.
lookupVarWithScopeAndError
  :: (Scope -> Maybe Scope) ->  (LangIdent -> ExecError)
  -> LangIdent -> ExecIO LangLit
lookupVarWithScopeAndError scF err name
    = do
    res <- lookupVarScope scF name
    case res of
        Nothing -> throwExecError $ err name
        Just v -> return v


-- | Modify the current scope using the given function.
modifyScope :: (Scope -> Scope) -> ExecIO ()
modifyScope f = do
  env <- getEnv
  let oldScope = currentScope env
      newScope' = f oldScope
  updateEnv env {currentScope=newScope'}


lookupVarCurrentScope :: VarValLookup
lookupVarCurrentScope name = do
  currScope <- liftM currentScope getEnv
  if isDefinedIn name currScope
     then return $ resolve name currScope
     else return Nothing


assignVarOuter :: LangIdent -> LangLit -> ExecIO ()
assignVarOuter n v = do
    currScope <- getScope
    case outerScope currScope of
        Nothing -> assignVarLocal n v
        Just sc -> do
            modifyScope (const sc)
            assignVarLocal n v
            newParent <- getScope
            modifyScope (const currScope { outerScope = Just newParent })


assignVarLocal :: LangIdent -> LangLit -> ExecIO ()
assignVarLocal = assignVar handleBuiltinAssignLit setVarInScope


assignVar :: (LangIdent -> LangLit -> ExecIO LangLit) -> (LangIdent -> VarVal LangLit -> Scope -> Scope) -> LangIdent -> LangLit -> ExecIO ()
assignVar handleBuiltin setf name val = do
  current <- lookupVarCurrentScope name
  val' <- if maybe False varBuiltin current
         then handleBuiltin name val
         else return val
  modifyScope $ setf name emptyVar {varDef=Just val'}


-- | Changes the current scope to the parent scope.
upScope :: ExecIO ()
upScope = do
  currScope <- liftM currentScope getEnv
  case outerScope currScope of
    Nothing -> return ()
    Just x  -> modifyScope (const x)


-- | Create a new scope with the current scope as its
-- parent.
newScope :: ExecIO ()
newScope = do
  env <- getEnv
  modifyScope (\s -> emptyScope {outerScope=Just s})
  let oldScope = currentScope env
      newScope' = emptyScope { outerScope = Just oldScope }
  updateEnv env { currentScope = newScope' }



assignVarBuiltinLit :: LangIdent -> LangLit -> ExecIO LangLit
assignVarBuiltinLit n v = assignVarBuiltin setVarInScope n v
    >> returnVal v


assignVarBuiltin :: (LangIdent -> VarVal b -> Scope -> Scope) -> LangIdent -> b -> ExecIO ()
assignVarBuiltin setf name val
    = modifyScope $ setf name emptyVar { varDef=Just val, varBuiltin=True }


type ScopeAssign =
  (LangIdent -> VarVal LangLit -> Scope -> Scope)
  -> (LangIdent -> LangLit -> ExecIO LangLit)
  -> LangIdent -> LangLit -> ExecIO ()

assignVarWithScope
  :: (Scope -> (Scope -> Scope) -> Scope)
  -> VarValLookup
  -> ScopeAssign
assignVarWithScope scopeMod lookupF assignF handleBuiltin name val = do
    currScope <- getScope
    current <- lookupF name
    if maybe False varBuiltin current
    then handleBuiltin name val
    else return val
    let newScope' = scopeMod currScope (assignF name VarVal { varDef = Just val, varBuiltin = False })
    modifyScope (const newScope')


assignVarGlobal :: LangIdent -> LangLit -> ExecIO ()
assignVarGlobal
  = assignVarWithScope modifyGlobalScope lookupVarGlobalScope setVarInScope handleBuiltinAssignLit
