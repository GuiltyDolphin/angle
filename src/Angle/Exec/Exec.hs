{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-|
Module      : Angle.Exec.Exec
Description : Controls execution of an Angle program.
Copyright   : Copyright (C) 2015 Ben Moon
License     : GNU GPL, version 3
Maintainer  : GuiltyDolphin@gmail.com
Stability   : alpha

All programs in Angle can be expressed as a single statement
(usually a multi-statement) and as such all programs are executed
via this one statement.
-}
module Angle.Exec.Exec
    ( execStmt
    ) where

import Control.Applicative
import Control.Monad
import Control.Monad.State
import Data.Maybe (isNothing, isJust, fromMaybe)

import Angle.Parse.Parser (program, evalParse)
import Angle.Exec.Builtins
import Angle.Exec.Error
import Angle.Exec.Operations
import Angle.Exec.Scope
import Angle.Exec.Types
import Angle.Types.Lang


updatePos :: SourceRef -> ExecIO ()
updatePos pos = modify (\e -> e { envSourceRef = pos })


setEnvSynRep :: String -> ExecIO ()
setEnvSynRep x = modify (\e -> e { envSynRep = x })


-- | Create a new scope with the current scope as its
-- parent.
newScope :: ExecIO ()
newScope = do
  env <- get
  modifyScope (\s -> emptyScope {outerScope=Just s})
  let oldScope = currentScope env
      newScope' = emptyScope { outerScope = Just oldScope }
  put env { currentScope = newScope' }


lookupVar :: (Scope -> BindEnv a) -> LangIdent -> ExecIO (Maybe a)
lookupVar binds name = do
  currScope <- getScope
  case resolve binds name currScope of
    Nothing -> return Nothing
    Just x  -> return $ varDef x


lookupVarF :: (Scope -> BindEnv a) -> (LangIdent -> ExecError) -> LangIdent -> ExecIO a
lookupVarF binds err name = lookupVar binds name
                        >>= maybe (throwExecError $ err name)
                            return


getScope :: ExecIO Scope
getScope = liftM currentScope get


lookupVarLitF :: LangIdent -> ExecIO LangLit
lookupVarLitF = (returnVal =<<) . lookupVarF valueBindings nameNotDefinedLitErr


lookupVarLambdaF :: LangIdent -> ExecIO Lambda
lookupVarLambdaF = lookupVarF lambdaBindings nameNotDefinedFunErr


-- | Modify the current scope using the given function.
modifyScope :: (Scope -> Scope) -> ExecIO ()
modifyScope f = do
  env <- get
  let oldScope = currentScope env
      newScope' = f oldScope
  put env {currentScope=newScope'}


lookupVarCurrentScope :: (Scope -> BindEnv a) -> LangIdent -> ExecIO (Maybe (VarVal a))
lookupVarCurrentScope binds name = do
  currScope <- liftM currentScope get
  if isDefinedIn binds name currScope
     then return $ resolve binds name currScope
     else return Nothing


assignVarLambda :: LangIdent -> Lambda -> ExecIO ()
assignVarLambda =
    assignVar lambdaBindings handleBuiltinAssignFun setVarFunInScope


assignVarLit :: LangIdent -> LangLit -> ExecIO LangLit
assignVarLit n v = assignVar valueBindings handleBuiltinAssignLit
    setVarLitInScope n v >> returnVal v


assignVar
  :: (Scope -> BindEnv a)
     -> (LangIdent -> b -> ExecIO b) -- ^ Builtin handler function.
     -> (LangIdent -> VarVal b -> Scope -> Scope)
     -> LangIdent
     -> b -- ^ Value to assign.
     -> ExecIO ()
assignVar binds handleBuiltin setf name val = do
  current <- lookupVarCurrentScope binds name
  -- when (maybe False varBuiltin current) $ handleBuiltinAssign name val
  val' <- if maybe False varBuiltin current
         then handleBuiltin name val
         else return val
  modifyScope $ setf name emptyVar {varDef=Just val'}


-- | Changes the current scope to the parent scope.
upScope :: ExecIO ()
upScope = do
  currScope <- liftM currentScope get
  case outerScope currScope of
    Nothing -> return ()
    Just x  -> modifyScope (const x)


bindArgs :: [Expr] -> ArgSig -> ExecIO ()
bindArgs args (ArgSig
                    { stdArgs=params
                    , catchAllArg=catchParam})
    = do
  let toCheck = zip args params
      la = length args
      lp = length params
  when ((la > lp && isNothing catchParam) || la < lp)
           (throwExecError $ wrongNumberOfArgumentsErr lp la)
  catchBind <- case catchParam of
                 Nothing -> return []
                 Just cp -> do
                   let toCatch = drop lp args
                   res <- mapM execExpr toCatch
                   return [(catchArgName cp, LitList res)]
  vals <- mapM (uncurry checkArg) toCheck
  case catchParam of
      Nothing -> return ()
      Just catcher ->
          case catchArgConstr catcher of
               Nothing -> return ()
               Just (ConstrRef cstr constrArgs) ->
                   case catchBind of
                        [] -> return ()
                        [(_, r)] -> checkSatConstr r (Just cstr) (fromMaybe [] constrArgs)
  let toBindFuns = map fst $ filter (isAnnFun . snd)  vals
      toBindLits = map fst $ filter (isAnnLit . snd) vals
      toBindAny = map fst (filter (isAnnAny . snd) vals) ++ catchBind
      isAnnFun AnnFun = True
      isAnnFun _ = False
      isAnnLit AnnLit = True
      isAnnLit _ = False
      isAnnAny AnnAny = True
      isAnnAny _ = False
  newScope
  forM_ toBindFuns (\(x, LitLambda l) -> assignVarLambda x l)
  forM_ toBindLits (uncurry assignVarLit)
  forM_ toBindAny (uncurry assignVarLit)
  return ()


-- | Make sure the argument satisfies any
-- class or type restrictions placed upon it.
checkArg :: Expr -> ArgElt -> ExecIO ((LangIdent, LangLit), AnnType)
checkArg ex (ArgElt {argEltConstr=constr, argEltType=typ
                    , argEltName=name}) = do
  v <- execExpr ex
  let cstr = fmap getConstrRef constr
  checkSatConstr v cstr (fromMaybe [] $ maybe Nothing constrRefArgs constr)
  checkSatType v typ
  return ((name, v), typ)


checkSatConstr :: LangLit -> Maybe LangIdent -> [Expr] -> ExecIO ()
checkSatConstr _ Nothing _ = return ()
checkSatConstr v (Just clsName) constrArgs = do
  res <- satConstr v clsName constrArgs
  unless res (throwExecError $ typeExpectConstrErr v clsName)


execConstr :: LangLit -> LangIdent -> [Expr] -> ExecIO LangLit
execConstr val clsName args = do
  cls <- lookupVarLambdaF clsName
  res <- callLambda cls True $ ExprLit val : args
  case res of
    x@(LitBool _) -> return x
    y             -> throwExecError
                     $ typeConstrWrongReturnErr clsName (typeOf y)


-- | Sets the 'as_class' variable to true if the function is
-- being called as a class, and cleans up afterwords.
withClass :: ExecIO a -> ExecIO a
withClass s = do
  assignVarBuiltinLit (LangIdent "as_class") (LitBool True)
  res <- s `catchAE` (\e -> setClassFalse >> throwAE e)
  setClassFalse
  return res
  where
    setClassFalse = assignVarBuiltinLit (LangIdent "as_class") (LitBool False)


-- | True if the literal is of the specified annotation type.
satType :: LangLit -> AnnType -> Bool
satType _ AnnAny = True
satType (LitLambda _) AnnFun = True
satType (LitLambda _) AnnLit = False
satType _ AnnLit = True
satType _ _ = False


-- | Fails if @satType@ returns false with the same arguments.
checkSatType :: CanErrorWithPos m => LangLit -> AnnType -> m ()
checkSatType val typ = do
  let res = satType val typ
  unless res $ throwExecError $ typeAnnWrongErr typ $ typeAnnOf val


-- | True if the given constraint returns true when
-- passed the literal.
satConstr :: LangLit -> LangIdent -> [Expr] -> ExecIO Bool
satConstr val clsName constrArgs = do
  (LitBool res) <- execConstr val clsName constrArgs
  return res


-- | Expand any ExprParamExpand expressions in an argument list.
expandParams :: [Expr] -> ExecIO [Expr]
expandParams [] = return []
expandParams (x:xs)
  = case x of
        ExprParamExpand n -> do
            val <- lookupVarLitF n
            case val of
              LitList vs -> liftM (map ExprLit vs ++) $ expandParams xs
              _          -> liftM (x:) $ expandParams xs
        _                -> liftM (x:) $ expandParams xs


execExpr :: Expr -> ExecIO LangLit
execExpr (ExprLit x@(LitRange{})) = checkLitRange x >> returnVal x
execExpr (ExprLit x) = returnVal x
execExpr (ExprIdent x) = lookupVarLitF x
execExpr (ExprFunIdent x) = liftM LitLambda $ lookupVarLambdaF x
execExpr (ExprOp x) = execOp x
execExpr (ExprFunCall name asClass args) = execFunCall name asClass args
execExpr (ExprList xs) = liftM LitList $ mapM execExpr xs
execExpr (ExprLambdaCall f xs) = callLambda f False xs
execExpr x@(ExprRange{}) = do
  r <- execExprRange x
  checkLitRange r
  returnVal r
execExpr _ = undefined


execExprRange :: Expr -> ExecIO LangLit
execExprRange (ExprRange x Nothing Nothing)
    = liftM3 LitRange (execExpr x) (return Nothing) (return Nothing)
execExprRange (ExprRange x (Just y) Nothing)
    = liftM3 LitRange (execExpr x) (liftM Just $ execExpr y) (return Nothing)
execExprRange (ExprRange x Nothing (Just z))
    = liftM3 LitRange (execExpr x) (return Nothing) (liftM Just $ execExpr z)
execExprRange (ExprRange x (Just y) (Just z))
    = liftM3 LitRange (execExpr x) (liftM Just $ execExpr y) (liftM Just $ execExpr z)
execExprRange _ = error "excExprRange: passed non-range expression"


execFunCall :: LangIdent -> Bool -> [Expr] -> ExecIO LangLit
execFunCall = callFun


execOp :: LangOp -> ExecIO LangLit
execOp (SpecOp op expr) = execSpecOp op expr
execOp (MultiOp op exprs) = execMultiOp op exprs


execSpecOp :: Op -> Expr -> ExecIO LangLit
execSpecOp OpNeg x = execExpr x >>= negLit
execSpecOp OpNot x = execExpr x >>= notLit
execSpecOp x _ = throwImplementationErr $ "execSpecOp - not a SpecOp: " ++ show x


execMultiOp :: Op -> [Expr] -> ExecIO LangLit
execMultiOp OpAdd xs       = withMultiOp xs addLit
execMultiOp OpAnd xs       = withMultiOp xs andLit
execMultiOp OpConcat xs    = withMultiOp xs concatLit
execMultiOp OpDiv xs       = withMultiOp xs divLit
execMultiOp OpEq  xs       = withMultiOp xs eqLit
execMultiOp OpExp xs       = withMultiOp xs expLit
execMultiOp OpGreater xs   = withMultiOp xs greaterLit
execMultiOp OpGreaterEq xs = withMultiOp xs greaterEqLit
execMultiOp OpLess xs      = withMultiOp xs lessLit
execMultiOp OpLessEq xs    = withMultiOp xs lessEqLit
execMultiOp OpMult xs      = withMultiOp xs multLit
execMultiOp OpOr xs        = withMultiOp xs orLit
execMultiOp OpSub xs       = withMultiOp xs subLit
execMultiOp (UserOp _) _ = throwImplementationErr "execMultiOp: implement user operators"
execMultiOp x _ = throwImplementationErr $ "execMultiOp - not a multiOp: " ++ show x


withMultiOp :: [Expr] -> ([LangLit] -> ExecIO LangLit) -> ExecIO LangLit
withMultiOp xs f = mapM execExpr xs >>= f


callFun :: LangIdent -> Bool -> [Expr] -> ExecIO LangLit
callFun x asClass args | isBuiltin x = callBuiltin x args
                       | otherwise = do
                           l <- lookupVarLambdaF x
                           callLambda l asClass args


callLambda :: Lambda -> Bool -> [Expr] -> ExecIO LangLit
callLambda (Lambda
            { lambdaArgs=params
            , lambdaBody=body}) asClass args
    = do
  fullArgs <- expandParams args
  -- bindArgs args params
  bindArgs fullArgs params
  let f = if asClass then withClass else id
  res <- f (execStmt body `catchReturn` return)
  upScope
  return res


-- | Executes a single statement.
execStmt :: Stmt -> ExecIO LangLit
execStmt (SingleStmt x pos) = updatePos pos >> execSingStmt x
execStmt (MultiStmt (x@(SingleStmt (StmtReturn _) _):_)) = execStmt x
execStmt (MultiStmt []) = return LitNull
execStmt (MultiStmt [x]) = execStmt x
execStmt (MultiStmt (x:xs)) = execStmt x >> execStmt (MultiStmt xs)


execSingStmt :: SingStmt -> ExecIO LangLit
execSingStmt (StmtAssign name e) = execExpr e
    >>= (\x -> case x of
        x'@(LitLambda l) -> assignVarLambda name l >> returnVal x'
        x' -> assignVarLit name x')
execSingStmt (StmtStruct x) = execLangStruct x
execSingStmt (StmtExpr x) = setEnvSynRep (showSyn x) >> execExpr x
execSingStmt (StmtComment _) = return LitNull
execSingStmt (StmtReturn x) = do
  isGlob <- liftM (isOutermostScope . currentScope) get
  if isGlob
      then throwExecError returnFromGlobalErr
      else execExpr x >>= throwReturn
execSingStmt (StmtBreak x False)
    = case x of
        Nothing -> throwBreak Nothing
        Just v  -> execExpr v
                   >>= returnVal >>= (throwBreak . Just)
execSingStmt (StmtBreak _ True) = throwContinue
execSingStmt (StmtRaise e) = raiseException e


raiseException :: LangIdent -> ExecIO LangLit
raiseException e = do
  env <- get
  let currE = currentException env
  return (LitKeyword e)
  case currE of
        Nothing -> throwExecError $ userErr e
        Just err -> if similarErr err
                   then throwAE err
                   else throwExecError $ userErr e
  where
    similarErr err = errToKeyword err == e
                   || genErrKeyword err == e
                   || LangIdent "error" == e


-- Possible:
-- entering loop construct
-- -> current loop, next statement
-- if break, then execute next statement
-- entering function/new scope:
-- -> current function, next statement
-- if return, then execute next statement
-- then reset (maybe Maybe values?)
-- would need to account for loops in loops
-- and functions in functions


execLangStruct :: LangStruct -> ExecIO LangLit
execLangStruct (StructFor name e s)
    = execStructFor name e s `catchBreak` maybe getEnvValue returnVal
execLangStruct (StructWhile e s)
    = execStructWhile e s `catchBreak` maybe getEnvValue returnVal
execLangStruct (StructIf if' thn els)
    = execStructIf if' thn els
execLangStruct (StructDefun name cs)
    = assignVarLambda name cs *> return LitNull
execLangStruct (StructTryCatch b cs)
    = execStructTryCatch b cs


execStructIf :: Expr -> Stmt -> Maybe Stmt -> ExecIO LangLit
execStructIf if' thn els = do
  p <- execExpr if'
  case p of
    (LitBool True)  -> execStmt thn
    (LitBool False) -> case els of
                         Nothing -> return LitNull
                         Just s  -> execStmt s
    x               -> throwExecError $ typeUnexpectedErr (typeOf x) LTBool


execStructFor :: LangIdent -> Expr -> Stmt -> ExecIO LangLit
execStructFor name e s = do
  iterable <- execExpr e >>= fromIter
  outScope <- liftM currentScope get
  res <- forM iterable (\v -> do
                   assignVarLit name v
                   execStmt s `catchContinue` getEnvValue)
  newS <- liftM currentScope get
  let newS' = deleteLitFromScope name newS
  modifyScope (const $ mergeScope newS' outScope)
  return (LitList res)


execStructWhile :: Expr -> Stmt -> ExecIO LangLit
execStructWhile ex s = do
  pos <- liftM envSourceRef get
  execStructFor (LangIdent "_")
                    (ExprLit
                     (LitRange (LitInt 1) Nothing Nothing))
                     (SingleStmt
                     (StmtStruct
                      (StructIf ex s (Just (SingleStmt (StmtBreak Nothing False) pos)))) pos)


execStructTryCatch :: Stmt -> [([LangIdent], Stmt)] -> ExecIO LangLit
execStructTryCatch b catchers = execStmt b `catchAE` genHandle
  where
    genHandle e = do
      env <- get
      put env { currentException = Just e }
      res <- checkCatch e catchers
      put env { currentException = Nothing }
      return res
    checkCatch e [] = throwError e
    checkCatch e ((toCatch, ex):es) = if catches
                                      then execStmt ex
                                      else checkCatch e es
      where catches = errToKeyword e `elem` toCatch || genErrKeyword e `elem` toCatch
                    || LangIdent "error" `elem` toCatch



builtinArgs :: [Expr] -> ExecIO [LangLit]
builtinArgs xs = expandParams xs >>= mapM execExpr


callBuiltin :: LangIdent -> [Expr] -> ExecIO LangLit
callBuiltin (LangIdent "print") xs = builtinArgs xs >>= builtinPrint
callBuiltin (LangIdent "str")   xs = builtinArgs xs >>= builtinStr
callBuiltin (LangIdent "index") xs = builtinArgs xs >>= builtinIndex
callBuiltin (LangIdent "length") xs = builtinArgs xs >>= builtinLength
callBuiltin (LangIdent "input") xs = builtinArgs xs >>= builtinInput
callBuiltin (LangIdent "eval") xs = builtinArgs xs >>= builtinEval
callBuiltin (LangIdent "asType") xs = builtinArgs xs >>= builtinAsType
callBuiltin (LangIdent "getArgs") xs = builtinArgs xs >>= builtinGetArgs
callBuiltin (LangIdent "isNull") xs = builtinArgs xs >>= builtinIsNull
callBuiltin (LangIdent "open") xs = builtinArgs xs >>= builtinOpen
callBuiltin (LangIdent "read") xs = builtinArgs xs >>= builtinRead
callBuiltin (LangIdent "write") xs = builtinArgs xs >>= builtinWrite
callBuiltin (LangIdent "close") xs = builtinArgs xs >>= builtinClose
callBuiltin (LangIdent "shell") xs = builtinArgs xs >>= builtinShell
callBuiltin (LangIdent "include") xs = builtinArgs xs >>= builtinInclude
callBuiltin (LangIdent x) _ = throwImplementationErr $ "callBuiltin - not a builtin function: " ++ x


builtinEval :: [LangLit] -> ExecIO LangLit
builtinEval xs = do
  let r = evalParse st program
  case r of
    Left e    -> throwExecError . syntaxErr $ e -- throwExecError . callBuiltinErr $ "eval: no parse"
    Right res -> execStmt res
  where st = argsToString xs


-- | Builtin @include@ function.
--
-- @include(file_names)@ will attempt to open, read and evaluate
-- each file in @file_names@ inplace. This is effectively the
-- same as having written the code contained within the files
-- at the point of call.
--
-- If any of @file_names@ are handles, then they will be read
-- fully and evaluated.
--
-- Filepaths are assumed to be relative, unless an absolute
-- path is provided.
builtinInclude :: [LangLit] -> ExecIO LangLit
builtinInclude xs = mapM_ includeFile xs >> return LitNull
  where
    includeFile h@(LitHandle _) = builtinRead [h] >>= builtinEval . (:[])
    includeFile f@(LitStr _) = builtinRead [f] >>= builtinEval . (:[])
    includeFile x = throwExecError $ typeNotValidErr x


assignVarBuiltinLit :: LangIdent -> LangLit -> ExecIO LangLit
assignVarBuiltinLit n v = assignVarBuiltin valueBindings
    setVarLitInScope n v >> returnVal v


assignVarBuiltinLambda :: LangIdent -> Lambda -> ExecIO ()
assignVarBuiltinLambda =
  assignVarBuiltin lambdaBindings setVarFunInScope


assignVarBuiltin
  :: (Scope -> BindEnv a)
     -> (LangIdent -> VarVal b -> Scope -> Scope)
     -> LangIdent
     -> b -- ^ Value to assign.
     -> ExecIO ()
assignVarBuiltin binds setf name val = do
    current <- lookupVarCurrentScope binds name
    modifyScope $ setf name emptyVar { varDef=Just val, varBuiltin=True }


validRangeLit :: LangLit -> Bool
validRangeLit (LitRange x Nothing Nothing)
    = enumType x
validRangeLit (LitRange x (Just y) Nothing)
    = enumType x && typeOf x == typeOf y
validRangeLit (LitRange x (Just y) (Just z))
    = enumType x && all ((== typeOf x) . typeOf) [y, z]
validRangeLit (LitRange x Nothing (Just z))
    = enumType x && typeOf x == typeOf z
validRangeLit _ = error "validRangeLit: Attempting to verify a non-range"


checkLitRange :: LangLit -> ExecIO ()
checkLitRange r@(LitRange x y z)
    = unless (validRangeLit r)
      (if enumType x
       then throwExecError $ badRangeErr (typeOf x) (fmap typeOf y) (fmap typeOf z)
       else throwExecError $ typeExpectConstrErr x (LangIdent "enum"))
checkLitRange _ = throwImplementationErr "checkLitRange: Attempting to check a non-range"
