module Angle.Parse.Builtins
    ( builtins
    , isBuiltin
    , builtinPrint
    , builtinStr
    , builtinIndex
    , builtinLength
    , builtinCompose
    , builtinPartial
    , builtinAsType
    , builtinGetArgs
    , builtinInput
    , startEnv
    , argsToString
    ) where


import Control.Monad
import Control.Monad.State
import qualified Data.Map as M
import Data.Maybe (maybeToList)
import System.Environment

import Angle.Parse.Error
import Angle.Parse.Scope
import Angle.Parse.Types
import Angle.Types.Lang
import Angle.Lex.Lexer (litList, evalScan)

           
builtinCallSig :: LangIdent -> CallSig
builtinCallSig name = 
    CallSig 
    { callArgs = ArgSig 
                 { stdArgs=[]
                 , catchAllArg=Just (LangIdent "x")
                 }
    , callBody = SingleStmt 
                 (StmtExpr 
                  (ExprFunCall name 
                   [ExprParamExpand 
                    (LangIdent "x")])) startRef 
    }


builtinVar :: LangIdent -> (LangIdent, VarVal)
builtinVar name = (name, VarVal
                   { varLitDef = Nothing
                   , varFunDef = Just (builtinCallSig name)
                   , varBuiltin = True})


builtinsVars = M.fromList $ 
               map (builtinVar . LangIdent) builtins


-- | Starting environment with builtin functions defined.
startEnv = basicEnv 
           { currentScope = emptyScope 
                            { bindings = builtinsVars } }


-- | True if the identifier represents a builtin function.
isBuiltin :: LangIdent -> Bool
isBuiltin = (`elem`builtins) . getIdent

           
-- | List of the builtin functions. 
builtins :: [String]
builtins = [ "print", "str"
           , "index", "length"
           , "compose", "partial"
           , "input", "eval"
           , "asType", "getArgs"]
         

builtinPrint :: [LangLit] -> ExecIO LangLit
builtinPrint xs = liftIO $ putStrLn res >> return (LitStr res)
    where res = argsToString xs
                                            

builtinInput :: [LangLit] -> ExecIO LangLit
builtinInput xs = liftM LitStr (liftIO $ putStr res >> liftIO getLine) 
    where res = argsToString xs
                              

argsToString :: [LangLit] -> String
argsToString = concatMap 
               (\x -> case x of
                        (LitStr s) -> s
                        _ -> showSyn x)
                              

builtinAsType :: [LangLit] -> ExecIO LangLit
builtinAsType [x] = return x
builtinAsType [x,y] = asType x y
builtinAsType (x:xs) = liftM LitList $ mapM (asType x) xs
builtinAsType _ = throwParserError $ callBuiltinErr "asType: invalid call"
                         

asType :: LangLit -> LangLit -> ExecIO LangLit
asType (LitStr _) x = return . toLitStr $ x
asType (LitFloat _) (LitInt x) = return . LitFloat $ fromIntegral x
asType (LitFloat _) (LitStr y) = fromStr y LitFloat
asType (LitInt _) (LitStr y) = fromStr y LitInt
asType (LitList _) (LitStr y) = case evalScan y litList of
                                  Left _ -> throwParserError . callBuiltinErr $ "asType: could not convert string literal to list"
                                  Right r -> return r
asType (LitBool _) (LitStr y) = 
    case y of
      "true" -> return $ LitBool True
      "false" -> return $ LitBool False
      _ -> throwParserError . callBuiltinErr $ "asType: could not convert string literal to boolean"
                                    
fromStr :: (Read a) => String -> (a -> LangLit) -> ExecIO LangLit
fromStr s f = case reads s of
              [] -> throwParserError . callBuiltinErr $ "asType: could not convert string literal to " ++ show (typeOf $ f undefined)
              [(r,"")] -> return $ f r
                              

builtinLength :: [LangLit] -> ExecIO LangLit
builtinLength [LitList xs] = return . LitInt $ length xs
builtinLength _ = throwParserError $ callBuiltinErr "length: invalid call"

-- | Implementation of the built-in str function.
builtinStr :: [LangLit] -> ExecIO LangLit
builtinStr [] = return $ LitStr ""
builtinStr xs | length xs > 1 = throwParserError $ wrongNumberOfArgumentsErr 1 (length xs)
              | otherwise = return $ toLitStr (head xs)
                            
                       
-- TODO:
-- - Currently wraps back round with negatives
--   e.g. index(-5,-1,[1,2,3]); -> [2, 3]     
-- - should probably work more like Pyhon's indexing system.

-- | Builtin index function.
--
-- @index(int:x, list:xs)@: retrieve element at index @x@ from @xs@
--
-- @index(int:x, int:y, list:xs)@: return a list of elements that lie between index @x@ and index @y@ of @xs@.
--
-- Negative indices are treated as working backwards from the
-- end of the list. With @-1@ being the last element.
builtinIndex :: [LangLit] -> ExecIO LangLit
builtinIndex [LitInt x,LitList xs]
    | x >= length xs = throwParserError $ indexOutOfBoundsErr x
    | x < 0 = return $ xs !! (length xs + x)
    | otherwise = return $ xs !! x
builtinIndex [LitInt x,LitInt y,LitList xs] 
    | x >= length xs || y > length xs 
        = throwParserError $ indexOutOfBoundsErr x
    | x < 0 = builtinIndex 
              [LitInt (length xs + x), LitInt y, LitList xs]
    | y < 0 = builtinIndex 
              [LitInt x, LitInt (length xs + y), LitList xs]
    | otherwise 
        = return . LitList $ splice x y xs
builtinIndex _ = throwParserError $ callBuiltinErr "index: invalid call signature"


splice :: Int -> Int -> [a] -> [a]
splice x y xs = take (1+y-x) $ drop x xs


-- runCompose :: CallSig -> CallSig -> Expr -> ExecIO LangLit
-- runCompose c1 c2 e = do
--   intm <- callFunCallSig c2 [e]
--   callFunCallSig c1 [ExprLit intm]
                 

partial :: CallSig -> [LangLit] -> ExecIO CallSig
partial x@(CallSig {callArgs=xArg@(ArgSig {stdArgs=xArgs})}) es
-- partial x@(CallSig {callArgs=ArgSig {stdArgs=xArgs, catchAllArg=Nothing}}) es
    = do
  p <- liftM envSourceRef get
  let newStdArgs = drop (length es) xArgs
      argList = map ExprLit es
                ++ map ExprIdent newStdArgs
                ++ map ExprParamExpand 
                       (maybeToList $ catchAllArg xArg)
  return CallSig 
             { callArgs=xArg 
               { stdArgs=newStdArgs}
             , callBody = SingleStmt 
                          (StmtExpr 
                           (ExprLambdaCall x argList)) p}
                            -- (map ExprLit es 
                             -- ++ map ExprIdent 
                             -- (drop (length es) xArgs)
                             -- ++ maybe [] ((:[]) . ExprParamExpand) (catchAllArg xArg)))) p}
  -- return CallSig { callArgs=ArgSig {stdArgs=drop (length es) xArgs, catchAllArg=Nothing }, callBody = SingleStmt (StmtExpr (ExprLambdaCall x (map ExprLit es ++ map ExprIdent (drop (length es) xArgs)))) p}


-- | Builtin partial application function.
--
-- @partial($f, ..x)@: Returns a lambda that is the partially applied function of @..x@ to @$f@.
--
-- @partial($f)@: Returns a lambda equivalent to the initial function.
builtinPartial :: [LangLit] -> ExecIO LangLit
builtinPartial [x@(LitLambda _)] = return x
builtinPartial (LitLambda x:xs) = liftM LitLambda $ partial x xs
-- builtinCompose (LitLambda x:[]) = return . LitLambda $ x
-- builtinCompose (LitLambda x:LitLambda y:[]) = liftM LitLambda $ compose x y
builtinPartial _ = throwImplementationErr "builtinPartial: better message please!"

                 

compose :: CallSig -> CallSig -> ExecIO CallSig
compose x@(CallSig {callArgs=ArgSig {catchAllArg=Nothing}})
        y@(CallSig {callArgs=ArgSig 
                    {stdArgs=[argY], catchAllArg=Nothing}})
    = do 
  currRef <- liftM envSourceRef get
  return y { callBody = 
                 SingleStmt 
                 (StmtExpr 
                  (ExprLambdaCall x 
                   [ExprLambdaCall y 
                    [ExprIdent argY]])) currRef }
compose _ _ = throwImplementationErr "compose: better message please!"
              

composeLambdas :: LangLit -> LangLit -> ExecIO LangLit
composeLambdas (LitLambda x) (LitLambda y) = liftM LitLambda $ compose x y
composeLambdas _ _ = throwImplementationErr "composeLambdas: better message please!"


-- | Builtin function composition function.
--
-- @compose($f, ..$g)@: composes @$f@ with @..$g@. That is, the result is a lambda that takes an argument and first applies @..$g@ to the argument, then @$f@ to the result produced by @..$g@.
--
-- @compose($f)@: returns a lambda equivalent to the original function.
builtinCompose :: [LangLit] -> ExecIO LangLit
builtinCompose [x@(LitLambda _)] = return x
builtinCompose (x:xs) = foldM composeLambdas x xs
-- builtinCompose (LitLambda x:[]) = return . LitLambda $ x
-- builtinCompose (LitLambda x:LitLambda y:[]) = liftM LitLambda $ compose x y
builtinCompose _ = throwImplementationErr "builtinCompose: better message please!"


toLitStr :: LangLit -> LangLit
toLitStr (LitInt x) = LitStr (show x)
toLitStr (LitFloat x) = LitStr (show x)
toLitStr (LitBool x) = LitStr (show x)
toLitStr x@(LitStr _) = x
toLitStr (LitList xs) = LitStr (show xs)
toLitStr x@(LitRange{}) = LitStr $ showSyn x
toLitStr LitNull = LitStr ""


builtinGetArgs :: [LangLit] -> ExecIO LangLit
builtinGetArgs _ = liftM (LitList . map LitStr) $ liftIO getArgs
