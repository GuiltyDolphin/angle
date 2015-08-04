{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
module Angle.Parse.Exec
    ( execStmt
    , Env(..)
    ) where

import Control.Applicative
import Control.Monad
import Control.Monad.State
import Data.Maybe (fromJust, fromMaybe, isJust, isNothing)
    
import Debug.Trace (trace)

import Angle.Parse.Builtins
import Angle.Parse.Error
import Angle.Parse.Operations    
import Angle.Parse.Scope
import Angle.Parse.Types
import Angle.Parse.Var
import Angle.Types.Lang
import Angle.Lex.Lexer (program, evalScan)
    
-- TODO:
-- - errors:
--    add stack trace?
--    so, calling functions add to stack, maybe as
--    well as operators.


-- BUGS:
-- - variable not defined in function
--   e.g
--   defun foo(x) {
--     if (== [] x) then return x;
--     bar = print(x);
--   }
--   will result in error: x is not defined (in print)
--   * Appears to be due to leaving scope early.
--     e.g. defun foo(x) {print(x);print(x);}
--     prints inner x, then outer x.
-- * Will not return properly from last statement in function.
--   - Forcing an upScope doesn't work - as multi-statements
--     aren't restricted to functions.
--   - Maybe keep track of whether it has returned?
--     - Would mean more state to keep track of...



-- Exec API
-- - Handling both IO and regular code
--    Have these separated? Could cause issues?
--    But might be unneccessary to have all in IO
-- - Environment in which to run (options etc?) - either
--    State, Reader or both
-- - ErrorT for error handling

-- assignVarVal :: Ident -> VarVal -> Exec LangLit
-- assignVarVal name val = do
-- TODO/NOTES
-- - Function assign (other than defun)


-- TODO:
-- Stacks
-- * For keeping track of statements:
--  - Maybe have nested (i.e multi) statements just execute
--    in same stack, only having to push a new stack
--    when entering a function or loop.
-- * Moving up a stack should FORCE the execution to continue
--   in the new stack, there is no point in the stacks
--   if they cannot track statements.

-- * global (initial) stack is passed a multistatement to
--   start the execution process.
--   - the global stack will create new child stacks
--     as required.
--   - but as using a multistatement as a means of wrapping
--     many single statements, perhaps the stacks should
--     work with lists of statements instead?
-- * stacks could also be useful in reporting errors, as the
--   stacks could track their statement position and any
--   function calls / loops that they enter.


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
              

lookupVarLit :: LangIdent -> ExecIO (Maybe LangLit)
lookupVarLit = lookupVar valueBindings
              

lookupVar :: (Scope -> BindEnv a) -> LangIdent -> ExecIO (Maybe a)
lookupVar binds name = do
  currScope <- getScope
  case resolve binds name currScope of
    Nothing -> return Nothing
    Just x -> return $ varDef x
              

lookupVarF :: (Scope -> BindEnv a) -> (LangIdent -> ParserError) -> LangIdent -> ExecIO a
lookupVarF binds err name = lookupVar binds name
                        >>= maybe (throwParserError $ err name)
                            return
                            

lookupClassF :: LangIdent -> ExecIO Lambda
lookupClassF = lookupVarF classBindings nameNotDefinedClassErr
 

getScope :: ExecIO Scope
getScope = liftM currentScope get 
           

lookupVarLitF :: LangIdent -> ExecIO LangLit
lookupVarLitF = (returnVal =<<) . lookupVarF valueBindings nameNotDefinedLitErr
              

lookupVarLambda :: LangIdent -> ExecIO (Maybe Lambda)
lookupVarLambda = lookupVar lambdaBindings
              
  
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

          
lookupVarLitCurrentScope :: LangIdent -> ExecIO (Maybe (VarVal LangLit))
lookupVarLitCurrentScope = lookupVarCurrentScope valueBindings

                                
lookupVarFunCurrentScope :: LangIdent -> ExecIO (Maybe (VarVal Lambda))
lookupVarFunCurrentScope = lookupVarCurrentScope lambdaBindings
                     

assignVarLambda :: LangIdent -> Lambda -> ExecIO ()
assignVarLambda = assignVar lambdaBindings setVarFunInScope
              

assignVarClass :: LangIdent -> Lambda -> ExecIO ()
assignVarClass = assignVar classBindings setVarClassInScope
              

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
  
  


infix 4 |=
(|=) :: LangIdent -> LangLit -> ExecIO LangLit
(|=) = assignVarLit


-- TODO: Should this just stay in the current scope if there
-- is no parent scope?
-- | Changes the current scope to the parent scope.
upScope :: ExecIO ()
upScope = do
  currScope <- liftM currentScope get
  case outerScope currScope of
    Nothing -> return ()
    Just x -> modifyScope (const x)
  
  
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
      toBindClass = map fst $ filter (isAnnClass . snd) vals
      toBindLits = (map fst $ filter (isAnnLit . snd) vals) ++ catchBind
      isAnnFun AnnFun = True
      isAnnFun _ = False
      isAnnLit AnnLit = True
      isAnnLit _ = False
      isAnnClass AnnClass = True
      isAnnClass _ = False
  newScope
  forM_ toBindFuns (\(x, (LitLambda l)) -> assignVarLambda x l)
  forM_ toBindClass (\(x, (LitLambda l)) -> assignVarClass x l)
  forM_ toBindLits (uncurry assignVarLit)
  return ()
-- bindArgs :: [Expr] -> CallSig -> ExecIO ()
-- bindArgs args' cs = do
--   args <- expandParams args'
--   let params = callArgs cs
--       la = length args
--       lp = length (stdArgs params)
--   when (la > lp && not (hasCatchAllArg params) || la < lp)
--            (throwParserError $ wrongNumberOfArgumentsErr lp la)
--   vals <- mapM execExpr args
--   let toBind = zip (stdArgs params) vals
--       fullBind = toBind ++ [(fromJust $ catchAllArg params, LitList $ drop (length toBind) vals) | hasCatchAllArg params]
--                  -- if length toBind /= la
--                  -- then [(fromJust $ catchAllArg params, LitList $ drop (length toBind) vals)]
--                  -- else [(fromJust $ catchAllArg params, LitList []) | hasCatchAllArg params] 
--   newScope
--   forM_ fullBind (uncurry assignVarLit)
        

-- | Make sure the argument satisfies any
-- class or type restrictions placed upon it.
checkArg :: Expr -> ArgElt -> ExecIO ((LangIdent, LangLit), AnnType)
checkArg ex (ArgElt {argEltClass=cls, argEltType=typ
                    , argEltName=name}) = do
  v <- execExpr ex
  checkSatClass v $ fmap getClassRef cls
  checkSatType v typ
  return ((name, v), typ)
         

checkSatClass :: LangLit -> Maybe LangIdent -> ExecIO ()
checkSatClass _ Nothing = return ()
checkSatClass v (Just clsName) = do
  res <- satClass v clsName
  case res of
    True -> return ()
    False -> throwParserError $ typeExpectClassErr v clsName
                   
lookupClass = lookupVar classBindings
                   
               
-- execClass val cls = do
--   res <- callLambda cls [val]
--   case res of
--     LitBool x -> return x
--     y -> throwParserError $ typeClassWrongReturnErr (typeOf y)


execClass val clsName = do
  cls <- lookupClassF clsName
  res <- callLambda cls [ExprLit val]
  case res of
    x@(LitBool _) -> return x
    y -> throwParserError 
         $ typeClassWrongReturnErr clsName (typeOf y)


satType (LitLambda _) AnnFun = True
satType (LitLambda _) AnnClass = True -- TODO: Check this!
satType (LitLambda _) AnnLit = False
satType _ AnnLit = True
satType _ _ = False

              
checkSatType val typ = do
  let res = satType val typ
  unless res $ throwParserError $ typeAnnWrongErr typ $ typeAnnOf val


satClass val clsName = do
  (LitBool res) <- execClass val clsName
  return res

  -- cls <- lookupClassF clsName
  -- execClass val cls
  
  
  

-- | Expand any ExprParamExpand expressions in an argument list.
expandParams :: [Expr] -> ExecIO [Expr]
expandParams [] = return []
expandParams (x:xs) 
  = case x of
      ExprParamExpand n -> do
             val <- lookupVarLitF n
             case val of 
               LitList vs -> liftM (map ExprLit vs ++) $ expandParams xs
               _ -> liftM (x:) $ expandParams xs
      _ -> liftM (x:) $ expandParams xs
                         

execExpr :: Expr -> ExecIO LangLit
execExpr (ExprLit x) = returnVal x
execExpr (ExprIdent x) = lookupVarLitF x
execExpr (ExprFunIdent x) = liftM LitLambda $ lookupVarLambdaF x
execExpr (ExprOp x) = execOp x
execExpr (ExprFunCall name args) = execFunCall name args
execExpr (ExprList xs) = liftM LitList $ mapM execExpr xs
execExpr (ExprLambda x) = returnVal (LitLambda x)
execExpr (ExprLambdaCall f xs) = callLambda f xs
                                   

execFunCall :: LangIdent -> [Expr] -> ExecIO LangLit
execFunCall = callFun
                      

returnVal :: LangLit -> ExecIO LangLit
returnVal v = putEnvValue v >> return v
    

getEnvValue :: ExecIO LangLit
getEnvValue = liftM envValue get


putEnvValue :: LangLit -> ExecIO ()
putEnvValue v = modify (\e -> e {envValue=v})


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
-- execMultiOp (UserOp x) xs = do
--   sig <- lookupOpF x
--   callFunCallSig sig xs
execMultiOp x _ = throwImplementationErr $ "execMultiOp - not a multiOp: " ++ show x
  
         
withMultiOp :: [Expr] -> ([LangLit] -> ExecIO LangLit) -> ExecIO LangLit
withMultiOp xs f = mapM execExpr xs >>= f 
  -- liftIO $ addLit (map execExpr xs)
                   

callFun :: LangIdent -> [Expr] -> ExecIO LangLit
callFun x args | isBuiltin x = callBuiltin x args
               | otherwise = do
  l <- lookupVarLambdaF x
  callLambda l args
  -- callFunCallSig callsig args
                 

callLambda :: Lambda -> [Expr] -> ExecIO LangLit
callLambda (Lambda 
            { lambdaArgs=params
            , lambdaBody=body}) args
    = do
  bindArgs args params
  res <- execStmt body `catchReturn` return
  upScope
  return res
           

callFunCallSig :: Lambda -> [Expr] -> ExecIO LangLit
callFunCallSig (Lambda {lambdaArgs=params, lambdaBody=body}) args = do
  bindArgs args params
  res <- execStmt body `catchReturn` return
  upScope
  return res
           

-- | @withScope ex@ runs @ex@ but will ensure that
-- the initial scope is retained after @ex@ is executed.
withScope :: ExecIO a -> ExecIO a
withScope ex = do
  currScope <- liftM currentScope get
  res <- ex
  currScope' <- liftM currentScope get
  if currScope' == currScope
     then return res
     else modifyScope (const currScope) >> return res
                               

execStmt :: Stmt -> ExecIO LangLit
-- execStmt (SingleStmt x@(StmtReturn _) pos)
--     = modify (\s -> s { envSourceRef = pos })
--       >> execSingStmt x
execStmt (SingleStmt x pos) = updatePos pos >> execSingStmt x
execStmt (MultiStmt (x@(SingleStmt (StmtReturn _) _):_)) = execStmt x
execStmt (MultiStmt []) = return LitNull
execStmt (MultiStmt [x]) = execStmt x
execStmt (MultiStmt (x:xs)) = execStmt x >> execStmt (MultiStmt xs)
                              
-- (MultiStmt (x:xs)) -> execStmt x >> execStmt (MultiStmt xs)
--                       (a -> m b)    (a -> m b)
                          

-- { foo(x);
--   bar(y);
--   baz(z);
-- }
                          

execSingStmt :: SingStmt -> ExecIO LangLit
execSingStmt (StmtAssign name e) = execExpr e >>= assignVarLit name
execSingStmt (StmtStruct x) = execLangStruct x
execSingStmt (StmtExpr x) = setEnvSynRep (showSyn x) >> execExpr x
execSingStmt (StmtComment _) = return LitNull
execSingStmt (StmtReturn x) = do
  isGlob <- liftM (isOutermostScope . currentScope) get
  if isGlob
      then throwParserError returnFromGlobalErr
      else execExpr x >>= throwReturn
execSingStmt (StmtBreak x False) = do
  case x of
    Nothing -> throwBreak Nothing
    Just v -> execExpr v >>= returnVal >>= (throwBreak . Just)
execSingStmt (StmtBreak _ True) = throwContinue


traceShowMsg :: (Show a) => String -> a -> a
traceShowMsg msg x = trace (msg ++ show x) x
                    
-- TODO: Fix return statement
-- - options
-- - add break/return (boolean?) information to state? then
--   would need to break evaluation early if they are
--   True, then set them to false.

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
execLangStruct (StructFor name e s) = execStructFor name e s `catchBreak` maybe getEnvValue returnVal
execLangStruct (StructWhile e s) = execStructWhile e s `catchBreak` maybe getEnvValue returnVal
execLangStruct (StructIf if' thn els) = execStructIf if' thn els
execLangStruct (StructDefun name cs) = assignVarLambda name cs *> return LitNull
execLangStruct (StructDefClass name cs) = assignVarClass name cs *> return LitNull
                                        

execStructIf :: Expr -> Stmt -> Maybe Stmt -> ExecIO LangLit
execStructIf if' thn els = do
  p <- execExpr if'
  case p of
    (LitBool True) -> execStmt thn
    (LitBool False) -> case els of 
                         Nothing -> return LitNull
                         Just s  -> execStmt s
    x -> throwParserError $ typeUnexpectedErr (typeOf x) LTBool
                    


fromIter :: LangLit -> ExecIO [LangLit]                        
fromIter (LitList xs) = return xs
fromIter (LitRange x Nothing Nothing) = iterFrom x
fromIter (LitRange x (Just y) Nothing) = iterFromTo x y
fromIter (LitRange x (Just y) (Just z)) = iterFromThenTo x y z
fromIter _ = throwImplementationErr "fromIter: TODO: define this!"


iterToLit :: LangLit -> ExecIO LangLit
iterToLit = liftM LitList . fromIter


iterFromThenTo :: LangLit -> LangLit -> LangLit -> ExecIO [LangLit]
iterFromThenTo (LitInt x) (LitInt y) (LitInt z) = return $ map LitInt $ enumFromThenTo x z y
iterFromThenTo (LitFloat x) (LitFloat y) (LitFloat z) = return $ map LitFloat $ enumFromThenTo x z y
iterFromThenTo (LitChar x) (LitChar y) (LitChar z) = return $ map LitChar $ enumFromThenTo x z y
iterFromThenTo _ _ _ = throwImplementationErr "iterFromThenTo: define failure"
                                                     
iterFrom :: LangLit -> ExecIO [LangLit]
iterFrom (LitInt x) = return $ map LitInt $ enumFrom x
iterFrom (LitChar x) = return $ map LitChar $ enumFrom x
iterFrom (LitFloat x) = return $ map LitFloat $ enumFrom x
iterFrom _ = throwImplementationErr "iterFrom: define failure"
             

iterFromTo :: LangLit -> LangLit -> ExecIO [LangLit]
iterFromTo (LitInt x) (LitInt y) = return $ map LitInt $ enumFromTo x y
iterFromTo (LitChar x) (LitChar y) = return $ map LitChar $ enumFromTo x y
iterFromTo (LitFloat x) (LitFloat y) = return $ map LitFloat $ enumFromTo x y
iterFromTo _ _ = throwImplementationErr "iterFromTo: define failure"


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
  -- forM iterable (\v -> assignVarLit name v $ do
  --                        execStmt s)

    
-- SCOPE TODO:
-- * Protected variable?
-- * Copying scope, with ability to merge.
 

-- deleteVar :: LangIdent -> ExecIO () 
-- deleteVar name = modifyScope (deleteFromScope name)


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
--   return $! trace "in while" ()
--   b <- execExpr ex
--   cond <- case b of
--             LitBool b' -> return b'
--             x -> throwParserError $ typeUnexpectedErr LTBool (typeOf x)
--   if cond
--   then execStmt s `seq` execStructWhile ex s
--   else getEnvValue
--   whileM (do 
--            cond <- execExpr ex 
--            case cond of
--              LitBool b -> return b
--              x -> throwParserError $ typeUnexpectedErr LTBool (typeOf x)) LitNull (\_ -> execStmt s)
       
-- instance CanErrorWithPos ExecIO where
--     getErrorPos = liftM envSourceRef get
--     getErrorText = liftM envSynRep get
--     getErrorSource = liftM sourceText get
    
                  

-- Builtins...                    

-- Exec Requirements
-- - Errors
--   - Custom datatype for Errors
--   - Use ErrorT to add error handling
-- - State
--   - Current scope
--   - Current settings



-- TODO:
--  eval and exec
--  - eval for results without side-effects?
--  - exec for results with side-effects?

-- execRun :: [SourceRef] -> ExecIO ()
-- execRun [] = return ()
-- execRun xs = do
--   forM_ xs 
--             (\(x,pos) -> 
--              execStmt x `catchError` (\e -> do
--                                        source <- liftM sourceText get
--                                        throwError e { errorPos=pos
--                                                     , errorSource=source
--                                                     }))
  
  

builtinArgs :: [Expr] -> ExecIO [LangLit]
builtinArgs xs = expandParams xs >>= mapM execExpr


callBuiltin :: LangIdent -> [Expr] -> ExecIO LangLit 
callBuiltin (LangIdent "print") xs = builtinArgs xs >>= builtinPrint
callBuiltin (LangIdent "str")   xs = builtinArgs xs >>= builtinStr
callBuiltin (LangIdent "index") xs = builtinArgs xs >>= builtinIndex
callBuiltin (LangIdent "length") xs = builtinArgs xs >>= builtinLength
--callBuiltin (LangIdent "compose") xs = builtinArgs xs >>= builtinCompose
--callBuiltin (LangIdent "partial") xs = builtinArgs xs >>= builtinPartial
callBuiltin (LangIdent "input") xs = builtinArgs xs >>= builtinInput
callBuiltin (LangIdent "eval") xs = builtinArgs xs >>= builtinEval
callBuiltin (LangIdent "asType") xs = builtinArgs xs >>= builtinAsType
callBuiltin (LangIdent "getArgs") xs = builtinArgs xs >>= builtinGetArgs 
callBuiltin (LangIdent x) _ = throwImplementationErr $ "callBuiltin - not a builtin function: " ++ x


builtinEval :: [LangLit] -> ExecIO LangLit
builtinEval xs = do
  let r = evalScan st program
  case r of
    Left _ -> throwParserError . callBuiltinErr $ "eval: no parse"
    Right res -> execStmt res
    where st = argsToString xs
