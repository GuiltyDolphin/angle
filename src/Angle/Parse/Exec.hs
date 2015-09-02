{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-|
Module      : Angle.Parse.Exec
Description : Controls execution of an Angle program.
Copyright   : Copyright (C) 2015 Ben Moon
License     : GNU GPL, version 3
Maintainer  : GuiltyDolphin@gmail.com
Stability   : alpha

All programs in Angle can be expressed as a single statement
(usually a multi-statement) and as such all programs are executed
via this one statement.
-}
module Angle.Parse.Exec
    ( execStmt
    ) where


-- TODO:
-- - For loops produce values, are they not expressions?
--   - Should be able to have for loops etc in places
--     where expressions can be placed.

import Control.Applicative
import Control.Monad
import Control.Monad.State
import Data.Maybe (isNothing)

import Angle.Lex.Lexer (program, evalScan)
import Angle.Parse.Builtins
import Angle.Parse.Error
import Angle.Parse.Operations
import Angle.Parse.Scope
import Angle.Parse.Types
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


lookupVarF :: (Scope -> BindEnv a) -> (LangIdent -> ParserError) -> LangIdent -> ExecIO a
lookupVarF binds err name = lookupVar binds name
                        >>= maybe (throwParserError $ err name)
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


assignVarLit :: LangIdent -> LangLit -> ExecIO LangLit
assignVarLit n v = assignVar valueBindings setVarLitInScope n v >> returnVal v


lookupVarCurrentScope :: (Scope -> BindEnv a) -> LangIdent -> ExecIO (Maybe (VarVal a))
lookupVarCurrentScope binds name = do
  currScope <- liftM currentScope get
  if isDefinedIn binds name currScope
     then return $ resolve binds name currScope
     else return Nothing


assignVarLambda :: LangIdent -> Lambda -> ExecIO ()
assignVarLambda = assignVar lambdaBindings setVarFunInScope


assignVar
  :: (Scope -> BindEnv a)
     -> (LangIdent -> VarVal b -> Scope -> Scope)
     -> LangIdent
     -> b -- ^ Value to assign.
     -> ExecIO ()
assignVar binds setf name val = do
  current <- lookupVarCurrentScope binds name
  when (maybe False varBuiltin current) $ throwParserError . assignToBuiltinErr $ name
  modifyScope $ setf name emptyVar {varDef=Just val}


-- TODO: Should this just stay in the current scope if there
-- is no parent scope?
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
  when (la > lp && isNothing catchParam || la < lp)
           (throwParserError $ wrongNumberOfArgumentsErr lp la)
  catchBind <- case catchParam of
                 Nothing -> return []
                 Just cp -> do
                   let toCatch = drop lp args
                   res <- mapM execExpr toCatch
                   return [(cp, LitList res)]
  vals <- mapM (uncurry checkArg) toCheck
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
  checkSatConstr v $ fmap getConstrRef constr
  checkSatType v typ
  return ((name, v), typ)


checkSatConstr :: LangLit -> Maybe LangIdent -> ExecIO ()
checkSatConstr _ Nothing = return ()
checkSatConstr v (Just clsName) = do
  res <- satConstr v clsName
  unless res (throwParserError $ typeExpectConstrErr v clsName)


execConstr :: LangLit -> LangIdent -> ExecIO LangLit
execConstr val clsName = do
  cls <- lookupVarLambdaF clsName
  res <- callLambda cls [ExprLit val]
  case res of
    x@(LitBool _) -> return x
    y             -> throwParserError
                     $ typeConstrWrongReturnErr clsName (typeOf y)


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
  unless res $ throwParserError $ typeAnnWrongErr typ $ typeAnnOf val


-- | True if the given class returns true when
-- passed the literal.
satConstr :: LangLit -> LangIdent -> ExecIO Bool
satConstr val clsName = do
  (LitBool res) <- execConstr val clsName
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
execExpr (ExprFunCall name args) = execFunCall name args
execExpr (ExprList xs) = liftM LitList $ mapM execExpr xs
execExpr (ExprLambdaCall f xs) = callLambda f xs
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


execFunCall :: LangIdent -> [Expr] -> ExecIO LangLit
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


callFun :: LangIdent -> [Expr] -> ExecIO LangLit
callFun x args | isBuiltin x = callBuiltin x args
               | otherwise = do
  l <- lookupVarLambdaF x
  callLambda l args


callLambda :: Lambda -> [Expr] -> ExecIO LangLit
callLambda (Lambda
            { lambdaArgs=params
            , lambdaBody=body}) args
    = do
  bindArgs args params
  res <- execStmt body `catchReturn` return
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
      then throwParserError returnFromGlobalErr
      else execExpr x >>= throwReturn
execSingStmt (StmtBreak x False)
    = case x of
        Nothing -> throwBreak Nothing
        Just v  -> execExpr v
                   >>= returnVal >>= (throwBreak . Just)
execSingStmt (StmtBreak _ True) = throwContinue


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


execStructIf :: Expr -> Stmt -> Maybe Stmt -> ExecIO LangLit
execStructIf if' thn els = do
  p <- execExpr if'
  case p of
    (LitBool True)  -> execStmt thn
    (LitBool False) -> case els of
                         Nothing -> return LitNull
                         Just s  -> execStmt s
    x               -> throwParserError $ typeUnexpectedErr (typeOf x) LTBool


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


-- TODO:
-- * This is a bit... Verbose
--   Might be better way?
-- * For loop will always loop once with empty list,
--   meaning that the while loop will always excute one too
--   many times.
execStructWhile :: Expr -> Stmt -> ExecIO LangLit
execStructWhile ex s = do
  pos <- liftM envSourceRef get
  execStructFor (LangIdent "_")
                    (ExprLit
                     (LitRange (LitInt 1) Nothing Nothing))
                     (SingleStmt
                     (StmtStruct
                      (StructIf ex s (Just (SingleStmt (StmtBreak Nothing False) pos)))) pos)


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
callBuiltin (LangIdent x) _ = throwImplementationErr $ "callBuiltin - not a builtin function: " ++ x


builtinEval :: [LangLit] -> ExecIO LangLit
builtinEval xs = do
  let r = evalScan st program
  case r of
    Left _    -> throwParserError . callBuiltinErr $ "eval: no parse"
    Right res -> execStmt res
    where st = argsToString xs


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
       then throwParserError $ badRangeErr (typeOf x) (fmap typeOf y) (fmap typeOf z)
       else throwParserError $ typeExpectConstrErr x (LangIdent "enum"))
checkLitRange _ = throwImplementationErr "checkLitRange: Attempting to check a non-range"
