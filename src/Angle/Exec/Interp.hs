module Angle.Exec.Interp
    ( main
    , interpWithArgs
    ) where

-- import System.IO
import Control.Monad.State
import System.Environment
import System.Exit

import Angle.Parse.Parser
import Angle.Exec.Exec
import Angle.Scanner (ScanError)
import Angle.Exec.Error
import Angle.Exec.Types
import Angle.Types.Lang


stringToStmt :: String -> Either ScanError Stmt
stringToStmt s = evalScan s program


runStmt :: String -> Stmt -> [String] -> IO (Either AngleError LangLit)
runStmt source s _ = do
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
argsToOpts _ = error "argsToOpts: invalid args"
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



