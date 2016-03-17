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
import Control.Monad.IO.Class (liftIO)
import Data.Maybe (isNothing, fromMaybe)
import System.Directory (findFile, canonicalizePath)
import System.FilePath (takeDirectory)

import Angle.Parse.Parser (program, evalParse)
import Angle.Exec.Builtins
import Angle.Exec.Error
import Angle.Exec.Operations
import Angle.Exec.Scope
import Angle.Exec.Types
import Angle.Types.Lang
import Angle.Types.Scope


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
                        _ -> undefined
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
  forM_ toBindFuns (\(x, LitLambda l) -> assignVarFunLocal x l)
  forM_ toBindLits (uncurry assignVarLitLocal)
  forM_ toBindAny (uncurry assignVarLitLocal)
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
  cls <- lookupVarFunLocal clsName
  res <- callLambda cls True $ ExprLit val : args
  case res of
    x@(LitBool _) -> return x
    y             -> throwExecError
                     $ typeConstrWrongReturnErr clsName (typeOf y)


-- | Sets the 'as_constr' variable to true if the function is
-- being called as a class, and cleans up afterwords.
withClass :: ExecIO a -> ExecIO a
withClass s = do
  assignVarBuiltinLit (LangIdent "as_constr") (LitBool True)
  res <- s `catchAE` (\e -> setClassFalse >> throwAE e)
  setClassFalse
  return res
  where
    setClassFalse = assignVarBuiltinLit (LangIdent "as_constr") (LitBool False)


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
            val <- lookupVarLitLocal n
            case val of
              LitList vs -> liftM (map ExprLit vs ++) $ expandParams xs
              _          -> liftM (x:) $ expandParams xs
        _                -> liftM (x:) $ expandParams xs


execExpr :: Expr -> ExecIO LangLit
execExpr (ExprLit x@(LitRange{})) = checkLitRange x >> returnVal x
execExpr (ExprLit (LitClosure lam)) = do
    currScope <- getScope
    returnVal . LitLambda $ lam { lambdaScope = Just currScope }
execExpr (ExprLit x) = returnVal x
execExpr (ExprIdent x) = lookupVarLitLocal x
execExpr (ExprFunIdent x) = liftM LitLambda $ lookupVarFunLocal x
execExpr (ExprOp x) = execOp x
execExpr (ExprFunCall name asClass args) = execFunCall name asClass args
execExpr (ExprList xs) = liftM LitList $ mapM execExpr xs
execExpr (ExprLambdaCall f xs) = callPureLambda f False xs
execExpr x@(ExprRange{}) = do
  r <- execExprRange x
  checkLitRange r
  returnVal r
execExpr _ = undefined



callPureLambda :: Lambda -> Bool -> [Expr] -> ExecIO LangLit
callPureLambda lam asClass exprs = do
    pushEnvCall (LangIdent "LAMBDA")
    res <- callLambda lam asClass exprs
    popEnvCall
    return res


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
execOp (MultiOp op exprs) = execMultiOp op exprs


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
execMultiOp OpNot xs       = withMultiOp xs notLit
execMultiOp OpOr xs        = withMultiOp xs orLit
execMultiOp OpSub xs       = withMultiOp xs subLit
execMultiOp (UserOp _) _ = throwImplementationErr "execMultiOp: implement user operators"
execMultiOp x _ = throwImplementationErr $ "execMultiOp - not a multiOp: " ++ show x


withMultiOp :: [Expr] -> ([LangLit] -> ExecIO LangLit) -> ExecIO LangLit
withMultiOp xs f = mapM execExpr xs >>= f


callFun :: LangIdent -> Bool -> [Expr] -> ExecIO LangLit
callFun x asClass args
    | isBuiltin x = callBuiltin x args
    | otherwise = do
        l <- lookupVarFunLocal x
        pushEnvCall x
        res <- callLambda l asClass args
        popEnvCall
        return res


callLambda :: Lambda -> Bool -> [Expr] -> ExecIO LangLit
callLambda l@(Lambda { lambdaScope = (Just s) }) asClass args
    = do
    cScope <- getScope
    modifyScope (const s)
    res <- callLambda l { lambdaScope = Nothing } asClass args
        `catchAE` (\e -> modifyScope (const cScope) >> throwError e)
    modifyScope (const cScope)
    return res
callLambda (Lambda
            { lambdaArgs=params
            , lambdaBody=body}) asClass args
    = do
  fullArgs <- expandParams args
  bindArgs fullArgs params
  let f = if asClass then withClass else id
  res <- f (execStmt body `catchReturn` return)
  upScope
  return res


-- | Executes a single statement.
execStmt :: Stmt -> ExecIO LangLit
execStmt s@(SingleStmt x pos) = updateStmt s >> updatePos pos >> execSingStmt x
execStmt (MultiStmt (x@(SingleStmt (StmtReturn _) _):_)) = execStmt x
execStmt (MultiStmt []) = return LitNull
execStmt (MultiStmt [x]) = execStmt x
execStmt (MultiStmt (x:xs)) = execStmt x >> execStmt (MultiStmt xs)


assignment
    :: (LangIdent -> Lambda -> ExecIO ())
    -> (LangIdent -> LangLit -> ExecIO ())
    -> LangIdent -> Expr -> ExecIO LangLit
assignment aFun aLit name e = execExpr e
    >>= (\x -> case x of
        x'@(LitLambda l) -> aFun name l >> returnVal x'
        x' -> aLit name x' >> returnVal x')


execSingStmt :: SingStmt -> ExecIO LangLit
execSingStmt (StmtAssign name e) = assignment assignVarFunLocal assignVarLitLocal name e
execSingStmt (StmtAssignNonLocal name e) = assignment assignVarFunOuter assignVarLitOuter name e
execSingStmt (StmtAssignGlobal name e) = assignment assignVarFunGlobal assignVarLitGlobal name e
execSingStmt (StmtStruct x) = execLangStruct x
execSingStmt (StmtExpr x) = do
  res <- execExpr x
  setEnvSynRep (showSyn res)
  returnVal res
execSingStmt (StmtComment _) = return LitNull
execSingStmt (StmtReturn x) = execReturn x
execSingStmt (StmtBreak x False) = execBreak x
execSingStmt (StmtBreak _ True) = throwContinue
execSingStmt (StmtRaise e) = raiseException e


execReturn :: Expr -> ExecIO LangLit
execReturn x = do
  isGlob <- liftM (isOutermostScope . currentScope) getEnv
  if isGlob
      then throwExecError returnFromGlobalErr
      else do
          res <- execExpr x
          case res of
              LitLambda lam -> do
                  sc <- getScope
                  let newS = case lambdaScope lam of
                                Nothing -> sc
                                Just s -> modifyGlobalScope s (\sc' -> sc' { outerScope = Just sc })
                  throwReturn $ LitLambda lam { lambdaScope = Just newS }
              y -> throwReturn y


execBreak :: Maybe Expr -> ExecIO LangLit
execBreak x = case x of
                Nothing -> throwBreak Nothing
                Just v  -> execExpr v
                          >>= returnVal >>= (throwBreak . Just)


raiseException :: LangIdent -> ExecIO LangLit
raiseException e = do
  env <- getEnv
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


execLangStruct :: LangStruct -> ExecIO LangLit
execLangStruct (StructFor name e s)
    = execStructFor name e s `catchBreak` maybe (getEnvValue >>= returnVal) returnVal
execLangStruct (StructWhile e s)
    = execStructWhile e s `catchBreak` maybe (getEnvValue >>= returnVal) returnVal
execLangStruct (StructIf if' thn els)
    = execStructIf if' thn els
execLangStruct (StructDefun name cs)
    = assignVarFunLocal name cs *> return LitNull
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
  outScope <- liftM currentScope getEnv
  res <- forM iterable (\v -> do
                   assignVarLitLocal name v
                   execStmt s `catchContinue` getEnvValue)
  newS <- liftM currentScope getEnv
  let newS' = deleteLitFromScope name newS
  modifyScope (const $ mergeScope newS' outScope)
  returnVal (LitList res)


execStructWhile :: Expr -> Stmt -> ExecIO LangLit
execStructWhile ex s = do
  pos <- liftM envSourceRef getEnv
  execStructFor (LangIdent "__WHILEVAL__")
                    (ExprLit
                     (LitRange (LitInt 1) Nothing Nothing))
                     (SingleStmt
                     (StmtStruct
                      (StructIf ex s (Just (SingleStmt (StmtBreak Nothing False) pos)))) pos)


execStructTryCatch :: Stmt -> [([LangIdent], Stmt)] -> ExecIO LangLit
execStructTryCatch b catchers = do
    currEnv <- getEnv
    execStmt b `catchAE` genHandle currEnv
  where
    genHandle env e = do
      cscope <- getScope -- Preserve the scope!
      updateEnv env { currentException = Just e, currentScope = cscope }
      res <- checkCatch e catchers `catchBreak` breakTry
      newEnv <- getEnv
      updateEnv newEnv { currentException = Nothing }
      return res
    checkCatch e [] = throwError e
    checkCatch e ((toCatch, ex):es) = if catches
                                      then execStmt ex
                                      else checkCatch e es
      where catches = errToKeyword e `elem` toCatch || genErrKeyword e `elem` toCatch
                    || LangIdent "error" `elem` toCatch
    breakTry (Just (LitKeyword (LangIdent "try")))
        = execStructTryCatch b catchers
    breakTry e = throwBreak e


builtinArgs :: [Expr] -> ExecIO [LangLit]
builtinArgs xs = expandParams xs >>= mapM execExpr


callBuiltin :: LangIdent -> [Expr] -> ExecIO LangLit
callBuiltin (LangIdent "print")    xs = builtinArgs xs >>= builtinPrint
callBuiltin (LangIdent "str")      xs = builtinArgs xs >>= builtinStr
callBuiltin (LangIdent "index")    xs = builtinArgs xs >>= builtinIndex
callBuiltin (LangIdent "length")   xs = builtinArgs xs >>= builtinLength
callBuiltin (LangIdent "input")    xs = builtinArgs xs >>= builtinInput
callBuiltin (LangIdent "eval")     xs = builtinArgs xs >>= builtinEval
callBuiltin (LangIdent "asType")   xs = builtinArgs xs >>= builtinAsType
callBuiltin (LangIdent "getArgs")  xs = builtinArgs xs >>= builtinGetArgs
callBuiltin (LangIdent "isNull")   xs = builtinArgs xs >>= builtinIsNull
callBuiltin (LangIdent "open")     xs = builtinArgs xs >>= builtinOpen
callBuiltin (LangIdent "read")     xs = builtinArgs xs >>= builtinRead
callBuiltin (LangIdent "write")    xs = builtinArgs xs >>= builtinWrite
callBuiltin (LangIdent "close")    xs = builtinArgs xs >>= builtinClose
callBuiltin (LangIdent "shell")    xs = builtinArgs xs >>= builtinShell
callBuiltin (LangIdent "include")  xs = builtinArgs xs >>= builtinInclude
callBuiltin (LangIdent "nonlocal") xs = builtinArgs xs >>= builtinNonLocal
callBuiltin (LangIdent "global")   xs = builtinArgs xs >>= builtinGlobal
callBuiltin (LangIdent "local")    xs = builtinArgs xs >>= builtinLocal
callBuiltin (LangIdent "round")    xs = builtinArgs xs >>= builtinRound
callBuiltin (LangIdent x) _ = throwImplementationErr $ "callBuiltin - not a builtin function: " ++ x


builtinEval :: [LangLit] -> ExecIO LangLit
builtinEval xs = do
  let r = evalParse st program
  case r of
    Left e    -> throwExecError . syntaxErr $ e
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
    includeFile (LitStr f) = includeAngleFile f
    includeFile x = throwExecError $ typeNotValidErr x
    includeAngleFile f = do
        fn <- getAngleFile f
        case fn of
            Nothing -> throwExecError . noSuchFileErr $ f
            Just pth -> do
              txt <- builtinRead [LitStr pth]
              env <- getEnv
              currFile <- liftM currentFile getEnv
              currMain <- liftM runAsMain getEnv
              currScope <- liftM currentScope getEnv
              updateEnv env { currentFile = Just pth, runAsMain = False }
              builtinEval [txt]
                  `catchAE` (\e -> do
                      newEnv <- getEnv
                      updateEnv env
                      throwAE e)
              -- newEnv <- getEnv
              recentScope <- liftM currentScope getEnv
              updateEnv env { currentScope = mergeScope recentScope currScope }
              -- updateEnv newEnv { currentFile = currFile, runAsMain = currMain }
              return LitNull
-- builtinRead [f] >>= builtinEval . (:[])


-- | Builtin @nonlocal@ function.
--
-- @nonlocal(var)@ attempts to lookup the value of @var@ in any
-- of the parent scopes of the current scope. Fails with a
-- @nameError@ if @var@ is not defined.
--
-- @nonlocal(:fun, var)@ attempts to lookup the lambda value
-- of @var@ in a parent scope.
builtinNonLocal :: [LangLit] -> ExecIO LangLit
builtinNonLocal [LitKeyword (LangIdent "fun"), LitKeyword n]
    = liftM LitLambda $ lookupVarFunOuter n
builtinNonLocal [LitKeyword n]
    = lookupVarLitOuter n
builtinNonLocal _ = throwExecError . callBuiltinErr $ "nonlocal: invalid call signature"


-- | Builtin @global@ function.
--
-- @global(var)@ attempts to lookup the value of @var@ in the global
-- scope. Fails with a @nameError@ if @var@ is not defined.
--
-- @global(:fun, var)@ attempts to lookup the lambda value
-- of @var@ in the global scope.
builtinGlobal :: [LangLit] -> ExecIO LangLit
builtinGlobal [LitKeyword (LangIdent "fun"), LitKeyword n]
    = liftM LitLambda $ lookupVarFunGlobal n
builtinGlobal [LitKeyword n]
    = lookupVarLitGlobal n
builtinGlobal _ = throwExecError . callBuiltinErr $ "global: invalid call signature"


-- | Built-in @local@ function.
--
-- @local(var)@ attempts to resolve the variable represented by the keyword
-- @var@.
--
-- @local(:fun, var)@ attempts to resove the lambda value of @var@@
builtinLocal :: [LangLit] -> ExecIO LangLit
builtinLocal [LitKeyword (LangIdent "fun"), LitKeyword n]
    = liftM LitLambda $ lookupVarFunLocal n
builtinLocal [LitKeyword n]
    = lookupVarLitLocal n
builtinLocal _ = throwExecError . callBuiltinErr $ "local: invalid call signature"


getAngleFile :: FilePath -> ExecIO (Maybe FilePath)
getAngleFile fp = do
    searchPath <- liftM angleLibPath getEnv
    currFile <- liftM currentFile getEnv
    maybeFound <- liftIO $ findFile (searchPath ++ [maybe "" takeDirectory currFile, ""]) fp
    case maybeFound of
        Nothing -> return Nothing
        Just p -> liftM Just $ liftIO $ canonicalizePath p


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
