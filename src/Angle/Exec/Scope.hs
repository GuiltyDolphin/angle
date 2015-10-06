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
    , lookupVarLitOuter
    , lookupVarFunOuter
    , assignVarLitOuter
    , assignVarFunOuter

    -- ** Current scope
    , lookupVarLitLocal
    , lookupVarFunLocal
    , modifyScope
    , assignVarLitLocal
    , assignVarFunLocal
    , assignVarBuiltinLit

    -- ** Global scope
    , lookupVarLitGlobal
    , lookupVarFunGlobal
    , assignVarLitGlobal
    , assignVarFunGlobal
    ) where


import Control.Monad

import Angle.Exec.Builtins ( handleBuiltinAssignLit
                           , handleBuiltinAssignFun)
import Angle.Exec.Error
import Angle.Exec.Types
import Angle.Types.Lang
import Angle.Types.Scope


type VarMaybeLookup a = (Scope -> BindEnv LangIdent a) -> LangIdent -> ExecIO (Maybe a)


type VarValLookup a = (Scope -> BindEnv LangIdent a) -> LangIdent -> ExecIO (Maybe (VarVal a))


-- | Determine which scope to lookup the variable in, then perform
-- the lookup.
lookupVarWithScope :: (Scope -> Maybe Scope) -> VarValLookup a
lookupVarWithScope scopeF binds name = do
    actionScope <- liftM scopeF getScope
    return $ case actionScope of
               Nothing -> Nothing
               Just sc -> resolve binds name sc


lookupVarGlobalScope :: VarValLookup a
lookupVarGlobalScope = lookupVarWithScope (Just . globalScope)


lookupVarScope :: (Scope -> Maybe Scope) -> VarMaybeLookup a
lookupVarScope scopeF binds name = do
    actionScope <- liftM scopeF getScope
    case actionScope of
        Nothing -> return Nothing
        Just sc -> case resolve binds name sc of
                      Nothing -> return Nothing
                      Just x -> return $ varDef x



-- | Retrieve the current scope from the environment.
getScope :: ExecIO Scope
getScope = liftM currentScope getEnv


lookupVarLitLocal :: LangIdent -> ExecIO LangLit
lookupVarLitLocal (LangIdent "_it") = getEnvValue
lookupVarLitLocal (LangIdent "main") = liftM (LitBool . runAsMain) getEnv
lookupVarLitLocal n = lookupVarLitErr Just n


lookupVarLitOuter :: LangIdent -> ExecIO LangLit
lookupVarLitOuter = lookupVarOuterErr nameNotDefinedLitErr valueBindings


lookupVarLitErr :: (Scope -> Maybe Scope) -> LangIdent -> ExecIO LangLit
lookupVarLitErr scF n = lookupVarWithScopeAndError scF nameNotDefinedLitErr valueBindings n >>= returnVal


lookupVarFunErr :: (Scope -> Maybe Scope) -> LangIdent -> ExecIO Lambda
lookupVarFunErr scF = lookupVarWithScopeAndError scF nameNotDefinedFunErr lambdaBindings


lookupVarFunLocal :: LangIdent -> ExecIO Lambda
lookupVarFunLocal = lookupVarFunErr Just


lookupVarLitGlobal :: LangIdent -> ExecIO LangLit
lookupVarLitGlobal = lookupVarLitErr (Just . globalScope)


lookupVarFunGlobal:: LangIdent -> ExecIO Lambda
lookupVarFunGlobal = lookupVarFunErr (Just . globalScope)


lookupVarFunOuter :: LangIdent -> ExecIO Lambda
lookupVarFunOuter = lookupVarOuterErr nameNotDefinedFunErr lambdaBindings

lookupVarOuterErr :: (LangIdent -> ExecError) -> VarLookup a
lookupVarOuterErr = lookupVarWithScopeAndError outerScope


type VarLookup a = (Scope -> BindEnv LangIdent a) -> LangIdent -> ExecIO a


-- | Lookup the value for the given variable with the specified set
-- of bindings; throw the specified exception if no value can be found.
lookupVarWithScopeAndError :: (Scope -> Maybe Scope) ->  (LangIdent -> ExecError) -> VarLookup a
lookupVarWithScopeAndError scF err binds name
    = do
    res <- lookupVarScope scF binds name
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


lookupVarCurrentScope :: VarValLookup a
lookupVarCurrentScope binds name = do
  currScope <- liftM currentScope getEnv
  if isDefinedIn binds name currScope
     then return $ resolve binds name currScope
     else return Nothing


assignVarFunLocal :: LangIdent -> Lambda -> ExecIO ()
assignVarFunLocal =
    assignVar lambdaBindings handleBuiltinAssignFun setVarFunInScope


assignVarOuter :: (LangIdent -> a -> ExecIO ()) -> LangIdent -> a -> ExecIO ()
assignVarOuter f n v = do
    currScope <- getScope
    case outerScope currScope of
        Nothing -> f n v
        Just sc -> do
            modifyScope (const sc)
            f n v
            newParent <- getScope
            modifyScope (const currScope { outerScope = Just newParent })


assignVarFunOuter :: LangIdent -> Lambda -> ExecIO ()
assignVarFunOuter = assignVarOuter assignVarFunLocal


assignVarLitOuter :: LangIdent -> LangLit -> ExecIO ()
assignVarLitOuter = assignVarOuter assignVarLitLocal


assignVarLitLocal :: LangIdent -> LangLit -> ExecIO ()
assignVarLitLocal
    = assignVar valueBindings handleBuiltinAssignLit setVarLitInScope


assignVar :: (Scope -> BindEnv LangIdent a) -> (LangIdent -> b -> ExecIO b) -> (LangIdent -> VarVal b -> Scope -> Scope) -> LangIdent -> b -> ExecIO ()
assignVar binds handleBuiltin setf name val = do
  current <- lookupVarCurrentScope binds name
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
assignVarBuiltinLit n v = assignVarBuiltin setVarLitInScope n v
    >> returnVal v


assignVarBuiltin :: (LangIdent -> VarVal b -> Scope -> Scope) -> LangIdent -> b -> ExecIO ()
assignVarBuiltin setf name val
    = modifyScope $ setf name emptyVar { varDef=Just val, varBuiltin=True }


type ScopeAssign a = forall b. (Scope -> BindEnv LangIdent a) -> (LangIdent -> VarVal b -> Scope -> Scope) -> (LangIdent -> b -> ExecIO b) -> LangIdent -> b -> ExecIO ()

assignVarWithScope :: (Scope -> (Scope -> Scope) -> Scope) -> VarValLookup a -> ScopeAssign a
assignVarWithScope scopeMod lookupF binds assignF handleBuiltin name val = do
    currScope <- getScope
    current <- lookupF binds name
    if maybe False varBuiltin current
    then handleBuiltin name val
    else return val
    let newScope' = scopeMod currScope (assignF name VarVal { varDef = Just val, varBuiltin = False })
    modifyScope (const newScope')


assignVarGlobal :: ScopeAssign a
assignVarGlobal = assignVarWithScope modifyGlobalScope lookupVarGlobalScope


assignVarFunGlobal :: LangIdent -> Lambda -> ExecIO ()
assignVarFunGlobal = assignVarGlobal lambdaBindings setVarFunInScope handleBuiltinAssignFun


assignVarLitGlobal :: LangIdent -> LangLit -> ExecIO ()
assignVarLitGlobal = assignVarGlobal valueBindings setVarLitInScope handleBuiltinAssignLit
