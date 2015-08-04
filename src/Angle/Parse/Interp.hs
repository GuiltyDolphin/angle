module Angle.Parse.Interp
    ( main
    , interpWithArgs
    ) where

-- import System.IO
import Control.Monad
import Control.Monad.State
import qualified Data.Map as M
import System.Environment
import System.IO
import System.Exit

import Angle.Lex.Lexer
import Angle.Parse.Exec
import Angle.Lex.Helpers (evalScan)
import Angle.Scanner (ScanError)
import Angle.Types.Lang
import Angle.Parse.Error
import Angle.Parse.Types
import Angle.Parse.Var
import Angle.Types.Lang
import Angle.Parse.Builtins
import Angle.Parse.Scope
    

stringToStmt :: String -> Either ScanError Stmt
stringToStmt s = evalScan s program
                 

runStmt :: String -> Stmt -> [String] -> IO (Either AngleError LangLit)
runStmt source s args = do 
  --let env' = initializeEnv args
  --    env = env' { sourceText=source }
  let env = basicEnv { sourceText = source }
  runExecIOEnv env (execStmt s)

            
runFile :: Bool -> FilePath -> [String] -> IO ()
runFile showSyntx f args = do
  s <- readFile f
  case stringToStmt s of
    Left err -> putStrLn "Syntax error" >> print err >> exitWith (ExitFailure 100)
    Right r -> do
              when showSyntx (do
                  putStrLn "Here's what I think..."
                  putStrLn (showSyn r))
              res <- runStmt s r args
              case res of
                Left err -> putStrLn "Runtime error" >> print err >> exitWith (ExitFailure 101)
                Right _ -> return ()
                             
interp :: Options -> IO ()
interp (Options { showRep=sr, fileName=f, progArgs=args }) = runFile sr f args
               

data Options = Options { showRep :: Bool
                       , fileName :: FilePath 
                       , progArgs :: [String]}
               deriving (Show, Eq)
                        

basicOptions :: Options
basicOptions = Options { showRep = False
                       , fileName = ""
                       , progArgs = [] }


argsToOpts :: [String] -> Options
argsToOpts xs | "--show-rep" `elem` xs = (argsToOpts xs') { showRep = True }
              where xs' = filter (/="--show-rep") xs
argsToOpts args@(f:_) = basicOptions { fileName = f, progArgs = args }
-- argsToOpts [f] = basicOptions { fileName = f }

main :: IO ()
main = liftM argsToOpts getArgs >>= interp
       

interpWithArgs :: [String] -> IO ()
interpWithArgs = interp . argsToOpts


-- initializeEnv :: [String] -> Env
-- initializeEnv args =
--     startEnv {currentScope = startScope { valueBindings = M.insert 
--                                           (LangIdent "args") 
--                                           (emptyVar {varDef = Just (LitList (map LitStr args))}) startBinds}}
--         where startBinds = valueBindings startScope
--               startScope = currentScope startEnv



